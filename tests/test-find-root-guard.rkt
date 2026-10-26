#lang racket

;; @speed fast
;; @suite default
;; @boundary integration

;; BOUNDARY: integration

;; test-find-root-guard.rkt — BUG-2 regression tests
;; Validates that find rejects scans from filesystem root and system directories.
;;
;; R97 (BUG-0097) additions: symlink confinement around the search root —
;; in-root symlink targets are OK, targets escaping the search root are
;; denied (structured records in details 'denied), dangling links are
;; reported, and directory symlinks (including cycles) are NEVER recursed
;; into.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         "../tools/builtins/find.rkt"
         (only-in "../tools/tool.rkt"
                  tool-result-content
                  tool-result-details
                  tool-result-is-error?)
         "helpers/fixtures.rkt")

(define (result-is-error? r)
  (tool-result-is-error? r))

(define (result-details r)
  (tool-result-details r))

(define (result-strings r)
  (for/list ([part (in-list (tool-result-content r))])
    (cond
      [(string? part) part]
      [(hash? part) (hash-ref part 'text "")]
      [else (~a part)])))

(define (denied-reasons r)
  (define d (result-details r))
  (if (and d (hash? d) (hash-has-key? d 'denied))
      (map (lambda (rec) (hash-ref rec 'reason)) (hash-ref d 'denied))
      '()))

(define (denied-paths r)
  (define d (result-details r))
  (if (and d (hash? d) (hash-has-key? d 'denied))
      (map (lambda (rec) (hash-ref rec 'path)) (hash-ref d 'denied))
      '()))

(define (result-error-msg r)
  (string-join (for/list ([part (in-list (tool-result-content r))])
                 (cond
                   [(string? part) part]
                   [(hash? part) (hash-ref part 'text "")]
                   [else (~a part)]))
               ""))

(define (create-file dir name [content ""])
  (define f (build-path dir name))
  (make-parent-directory* f)
  (call-with-output-file f (lambda (out) (display content out)) #:exists 'replace)
  f)

(define (create-symlink dir link-name target)
  (define l (build-path dir link-name))
  (make-file-or-directory-link target l)
  l)

(define root-guard-tests
  (test-suite "find-root-guard"

    ;; BUG-2: find must reject path "/"
    (test-case "find rejects path / (filesystem root)"
      (define r (tool-find (hasheq 'path "/")))
      (check-true (result-is-error? r) (format "Expected error for /, got: ~a" r))
      (define msg (result-error-msg r))
      (check-true (or (regexp-match? #rx"[Rr]oot" msg)
                      (regexp-match? #rx"[Ff]ilesystem" msg)
                      (regexp-match? #rx"[Ss]ystem" msg)
                      (regexp-match? #rx"[Pp]rotected" msg))
                  (format "Error message should mention root/filesystem/system: ~a" msg)))

    ;; BUG-2: find must reject system directories
    (test-case "find rejects system path /proc"
      (define r (tool-find (hasheq 'path "/proc")))
      (check-true (result-is-error? r)))

    (test-case "find rejects system path /sys"
      (define r (tool-find (hasheq 'path "/sys")))
      (check-true (result-is-error? r)))

    (test-case "find rejects system path /dev"
      (define r (tool-find (hasheq 'path "/dev")))
      (check-true (result-is-error? r)))

    (test-case "find rejects system path /run"
      (define r (tool-find (hasheq 'path "/run")))
      (check-true (result-is-error? r)))

    (test-case "find rejects system path /snap"
      (define r (tool-find (hasheq 'path "/snap")))
      (check-true (result-is-error? r)))

    (test-case "find rejects symlink to system path /proc/self/cwd"
      ;; /proc/self/cwd is a symlink — find should still reject it
      (define r (tool-find (hasheq 'path "/proc/self/cwd")))
      (check-true (result-is-error? r)))

    ;; Positive cases: non-system paths should work
    (test-case "find allows /tmp (non-system path)"
      (define r (tool-find (hasheq 'path "/tmp" 'max-results 1 'max-depth 0)))
      ;; May return empty results, but must NOT error
      (check-false (result-is-error? r)))

    (test-case "find allows relative paths"
      (with-temp-dir (λ (dir)
                       (create-file dir "test.txt")
                       (define orig-cd (current-directory))
                       (dynamic-wind (lambda () (current-directory dir))
                                     (lambda ()
                                       (define r (tool-find (hasheq 'path ".")))
                                       (check-false (result-is-error? r)))
                                     (lambda () (current-directory orig-cd))))))

    (test-case "find allows project-style absolute paths"
      (with-temp-dir (λ (dir)
                       (create-file dir "test.txt")
                       (define r (tool-find (hasheq 'path (path->string dir))))
                       (check-false (result-is-error? r)))))

    ;; R97: a root written with '..' that normalizes to the filesystem root
    ;; must stay rejected (guard runs on the normalized root).
    (test-case "find rejects path normalizing to / via '..'"
      (define r (tool-find (hasheq 'path "/tmp/..")))
      (check-true (result-is-error? r)
                  (format "Expected error for /tmp/.., got: ~a" r)))

    ;; ============================================================
    ;; R97: symlink confinement and cycle defense
    ;; ============================================================

    ;; Symlink whose resolved target stays inside the search root is OK.
    (test-case "R97: symlink with in-root target is emitted and openable"
      (with-temp-dir (λ (dir)
                       (create-file dir "real.txt" "in-root-symlink-sentinel")
                       (create-symlink dir "link.txt" (build-path dir "real.txt"))
                       (define r (tool-find (hasheq 'path (path->string dir))))
                       (check-false (result-is-error? r))
                       (define c (result-strings r))
                       ;; emitted as the traversal path (absolute)
                       (check-not-false (member (path->string (build-path dir "link.txt")) c))
                       ;; resolved target inside the root: never denied
                       (check-false (member "symlink-target-outside-root" (denied-reasons r)))
                       (check-false (member "dangling-symlink" (denied-reasons r))))))

    ;; Symlink whose resolved target escapes the search root is DENIED.
    (test-case "R97: symlink target outside search root is denied"
      (with-temp-dir (λ (dir)
                       (create-file dir "keep.txt")
                       (create-symlink dir "escape-link" "/etc/passwd")
                       (define r (tool-find (hasheq 'path (path->string dir))))
                       (check-false (result-is-error? r))
                       (define c (result-strings r))
                       (check-not-false (member (path->string (build-path dir "keep.txt")) c))
                       (check-false (member (path->string (build-path dir "escape-link")) c)
                                    "escaping symlink must not be emitted")
                       (check-not-false (member "symlink-target-outside-root" (denied-reasons r))
                                        (format "expected escape denial record, got: ~a"
                                                (denied-reasons r)))
                       (check-not-false (member (path->string (build-path dir "escape-link"))
                                                (denied-paths r))))))

    ;; Dangling symlink: structured denial record, never emitted, search
    ;; still succeeds (a stale link must not fail the whole scan).
    (test-case "R97: dangling symlink yields structured denial, not a crash"
      (with-temp-dir (λ (dir)
                       (create-file dir "solid.txt")
                       (create-symlink dir "dangle-link"
                                       (build-path dir "no-such-target-xyz"))
                       (define r (tool-find (hasheq 'path (path->string dir))))
                       (check-false (result-is-error? r)
                                    (format "dangling link must not fail the search: ~a" r))
                       (define c (result-strings r))
                       (check-not-false (member (path->string (build-path dir "solid.txt")) c))
                       (check-false (member (path->string (build-path dir "dangle-link")) c))
                       (check-not-false (member "dangling-symlink" (denied-reasons r)))
                       (check-not-false (member (path->string (build-path dir "dangle-link"))
                                                (denied-paths r))))))

    ;; Directory symlink cycle: the cycle entry is never recursed into — no
    ;; results under the cycle link, bounded scan, search completes.
    (test-case "R97: directory symlink cycle is not traversed"
      (with-temp-dir (λ (dir)
                       (create-file dir "d1/inner.txt")
                       (create-file dir "d1/sub/deep.txt")
                       ;; loop back to the search root itself
                       (create-symlink dir "d1/loop" dir)
                       (define r (tool-find (hasheq 'path (path->string dir))))
                       (check-false (result-is-error? r))
                       (define c (result-strings r))
                       ;; nothing from INSIDE the cycle is emitted (the
                       ;; marker is the real fixture's cycle directory,
                       ;; built from path pieces — no hardcoded prefix)
                       (define cycle-marker
                         (path->string (path->directory-path (build-path dir "d1" "loop"))))
                       (check-false (findf (lambda (s) (string-contains? s cycle-marker)) c)
                                    "cycle directory must not be traversed")
                       ;; real subtree still found
                       (check-not-false (member (path->string (build-path dir "d1/inner.txt")) c))
                       (check-not-false (member (path->string (build-path dir "d1/sub/deep.txt")) c)))))

    ;; Directory symlink to a sibling tree: recursion must stay confined to
    ;; the declared root even though the target is a real directory.
    (test-case "R97: directory symlink is never recursed into (escape defense)"
      (with-temp-dir (λ (dir)
                       (create-file dir "root-file.txt")
                       (define sibling (make-temporary-file "q97-sibling-~a" 'directory))
                       (dynamic-wind void
                                     (lambda ()
                                       (call-with-output-file
                                        (build-path sibling "sibling-secret.txt")
                                        (lambda (out) (display "sibling" out))
                                        #:exists 'replace)
                                       (create-symlink dir "sibling-link" sibling)
                                       (define r (tool-find (hasheq 'path (path->string dir))))
                                       (check-false (result-is-error? r))
                                       (define c (result-strings r))
                                       (check-false (findf (lambda (s) (string-contains? s
                                                                "sibling-secret")) c)
                                                    "must not descend through dir symlink")
                                       (check-not-false
                                        (member (path->string (build-path dir "root-file.txt")) c)))
                                     (lambda ()
                                       (delete-directory/files sibling #:must-exist? #f))))))))

(run-tests root-guard-tests)
