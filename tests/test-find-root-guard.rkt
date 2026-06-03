#lang racket

;; BOUNDARY: integration

;; test-find-root-guard.rkt — BUG-2 regression tests
;; Validates that find rejects scans from filesystem root and system directories.

(require rackunit
         rackunit/text-ui
         racket/file
         "../tools/builtins/find.rkt"
         (only-in "../tools/tool.rkt" tool-result-content tool-result-is-error?)
         "helpers/fixtures.rkt")

(define (result-is-error? r) (tool-result-is-error? r))

(define (result-error-msg r)
  (string-join
   (for/list ([part (in-list (tool-result-content r))])
     (cond [(string? part) part]
           [(hash? part) (hash-ref part 'text "")]
           [else (~a part)]))
   ""))

(define (create-file dir name [content ""])
  (define f (build-path dir name))
  (make-parent-directory* f)
  (call-with-output-file f (lambda (out) (display content out)) #:exists 'replace)
  f)

(define root-guard-tests
  (test-suite "find-root-guard"

    ;; BUG-2: find must reject path "/"
    (test-case "find rejects path / (filesystem root)"
      (define r (tool-find (hasheq 'path "/")))
      (check-true (result-is-error? r)
                  (format "Expected error for /, got: ~a" r))
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

    ;; Positive cases: non-system paths should work
    (test-case "find allows /tmp (non-system path)"
      (define r (tool-find (hasheq 'path "/tmp" 'max-results 1 'max-depth 0)))
      ;; May return empty results, but must NOT error
      (check-false (result-is-error? r)))

    (test-case "find allows relative paths"
      (with-temp-dir
       (λ (dir)
         (create-file dir "test.txt")
         (define orig-cd (current-directory))
         (dynamic-wind
           (lambda () (current-directory dir))
           (lambda ()
             (define r (tool-find (hasheq 'path ".")))
             (check-false (result-is-error? r)))
           (lambda () (current-directory orig-cd))))))

    (test-case "find allows project-style absolute paths"
      (with-temp-dir
       (λ (dir)
         (create-file dir "test.txt")
         (define r (tool-find (hasheq 'path (path->string dir))))
         (check-false (result-is-error? r)))))))

(run-tests root-guard-tests)
