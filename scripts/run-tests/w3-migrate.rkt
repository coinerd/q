#lang racket/base
;; W3 ONE-OFF: area-by-area declarative metadata migration.
;; NOT a deliverable — deleted after the wave.
;;
;; Invariant: every suite membership (except unit_fast, which intentionally
;; grows) is byte-identical before/after; verified via w3-snapshot.rkt.
;;
;; Derived-tag policy (documented in docs/TEST_CONVENTIONS.md):
;;   @speed    slow-file? ? slow : fast
;;   @suite    area tokens mirroring today's heuristic predicates
;;             (tui first when tui-file?; else security/arch/runtime/
;;             extensions/workflows/platform/mutating as applicable;
;;             "default" when no area predicate hits)
;;   @boundary /workflows/ or *integration*  -> integration
;;             *tmux* / *e2e*                 -> e2e
;;             audit-unsafe body scan         -> integration
;;             otherwise                      -> unit
;;   @isolation  only for mutating-file? tests (process)
;;   @mutates    only for mutating-file? tests (env/fs/repo token scan)
;;   @requires   tmux -> terminal; network markers -> network

(require racket/string
         racket/list
         racket/file
         racket/path
         (only-in "classify.rkt"
                  base-dir
                  collect-test-files
                  get-file-metadata
                  slow-file?
                  tui-file?
                  mutating-file?
                  security-file?
                  arch-file?
                  runtime-file?
                  extensions-file?
                  workflows-file?
                  platform-file?))

(define alias-header-files
  ;; Files whose OWN header carries the deprecated @isolation subprocess
  ;; alias (fixture strings in other files are intentionally left alone).
  '("tests/test-run-tests-in-process-mode.rkt" "tests/test-run-tests-json-classification.rkt"
                                               "tests/test-run-tests-ledger.rkt"
                                               "tests/test-run-tests-overhead-diagnostics.rkt"
                                               "tests/test-run-tests-profiles.rkt"
                                               "tests/test-run-tests-script.rkt"))

;; ------------------------------------------------------------
;; Body scans
;; ------------------------------------------------------------

(define env-unsafe-rx #rx"(?i:putenv|setenv)")
(define network-unsafe-rx #rx"(?i:tcp-connect|http-connector|open-input-url|url-request)")
(define subprocess-unsafe-rx
  (regexp (string-append "[(]subprocess[[:space:]]|[(]system[[:space:]]|[(]process[[:space:]]|"
                         "[(]process*[[:space:]]|shell-complete")))
(define fs-write-unsafe-rx
  (regexp (string-append "call-with-output-file|with-output-to-file|write-to-file|display-to-file|"
                         "delete-file|delete-directory|copy-file|copy-directory|"
                         "rename-file-or-directory|make-directory")))
(define exit-unsafe-rx #rx"[(]exit[[:space:]]")
(define tmux-rx #rx"(?i:tmux)")
(define integration-name-rx #rx"integration|workflow-")
(define e2e-name-rx #rx"(?i:e2e|tmux)")

(define (body-flags content)
  (append (if (regexp-match? env-unsafe-rx content)
              '(env)
              '())
          (if (regexp-match? network-unsafe-rx content)
              '(network)
              '())
          (if (regexp-match? subprocess-unsafe-rx content)
              '(subprocess)
              '())
          (if (regexp-match? fs-write-unsafe-rx content)
              '(fs-write)
              '())
          (if (regexp-match? exit-unsafe-rx content)
              '(exit)
              '())))

(define (mutates-scan content f)
  (define env? (regexp-match? #rx"(?i:putenv)" content))
  (define fs? (regexp-match? fs-write-unsafe-rx content))
  (define repo?
    (regexp-match?
     #rx"sync-version|bump-version|metrics-readme|sync-readme|pre-commit|ci-local|check-deps|self-hosting"
     f))
  (define tokens
    (append (if env?
                '("env")
                '())
            (if fs?
                '("fs")
                '())
            (if (and repo? (not env?) (not fs?))
                '("repo")
                '())))
  (if (null? tokens)
      "fs"
      (string-join tokens " ")))

;; ------------------------------------------------------------
;; Derived tags
;; ------------------------------------------------------------

(define (derive-suite-tokens f)
  (cond
    [(tui-file? f)
     (define rest
       (append (if (mutating-file? f)
                   '("mutating")
                   '())
               (if (security-file? f)
                   '("security")
                   '())
               (if (arch-file? f)
                   '("arch")
                   '())
               (if (runtime-file? f)
                   '("runtime")
                   '())))
     (string-join (cons "tui" rest) " ")]
    [else
     (define tokens
       (append (if (security-file? f)
                   '("security")
                   '())
               (if (arch-file? f)
                   '("arch")
                   '())
               (if (runtime-file? f)
                   '("runtime")
                   '())
               (if (extensions-file? f)
                   '("extensions")
                   '())
               (if (workflows-file? f)
                   '("workflows")
                   '())
               (if (platform-file? f)
                   '("platform")
                   '())
               (if (mutating-file? f)
                   '("mutating")
                   '())))
     (if (null? tokens)
         "default"
         (string-join tokens " "))]))

(define (derive-boundary f content)
  (cond
    [(or (workflows-file? f) (regexp-match? integration-name-rx f)) "integration"]
    [(regexp-match? e2e-name-rx f) "e2e"]
    [(pair? (body-flags content)) "integration"]
    [else "unit"]))

(define (derive-requires f content)
  (cond
    [(regexp-match? tmux-rx f) "terminal"]
    [(regexp-match? network-unsafe-rx content) "network"]
    [else #f]))

;; ------------------------------------------------------------
;; Header rewriting
;; ------------------------------------------------------------

(define tag-line-rx
  #px";;.*(\\bsuite\\b|\\bspeed\\b|\\bboundary\\b|\\bmutates\\b|\\bisolation\\b|\\btimeout\\b|\\brequires\\b|\\bcovers\\b|\\bnot-test\\b)")

(define (full-path f)
  (if (absolute-path? f)
      f
      (build-path base-dir f)))

(define (migrate-file f)
  (define p (full-path f))
  (define m (get-file-metadata f))
  (define lines (file->lines p))
  (define content (string-join lines "\n"))

  ;; Normalize deprecated @isolation subprocess alias in real headers.
  (when (and (member f alias-header-files) (equal? (hash-ref m 'isolation #f) "subprocess"))
    (set! lines
          (for/list ([line (in-list lines)]
                     [i (in-naturals)])
            (if (< i 50)
                (regexp-replace #rx"@isolation[ \t]+subprocess" line "@isolation process")
                line))))

  ;; Existing tag presence comes from the parser itself.
  (define has-suite (hash-ref m 'suite #f))
  (define has-speed (hash-ref m 'speed #f))
  (define has-boundary (hash-ref m 'boundary #f))
  (define has-isolation (hash-ref m 'isolation #f))
  (define has-mutates (hash-ref m 'mutates #f))
  (define has-requires (pair? (hash-ref m 'requires '())))

  (define mutating? (mutating-file? f))

  ;; Never make a non-mutating file mutating: @isolation/@mutates only
  ;; for files that already classify as mutating today.
  (define line1-segments
    (append (if (not has-speed)
                (list (format ";; @speed ~a" (if (slow-file? f) "slow" "fast")))
                '())
            (if (not has-suite)
                (list (format ";; @suite ~a" (derive-suite-tokens f)))
                '())))
  (define line2-segments
    (append (if (not has-boundary)
                (list (format ";; @boundary ~a" (derive-boundary f content)))
                '())
            (if (and mutating? (not has-isolation))
                (list ";; @isolation process")
                '())
            (if (and mutating? (not has-mutates))
                (list (format ";; @mutates ~a" (mutates-scan content f)))
                '())
            (if (not has-requires)
                (let ([r (derive-requires f content)])
                  (if r
                      (list (format ";; @requires ~a" r))
                      '()))
                '())))

  (cond
    [(and (null? line1-segments) (null? line2-segments) (not (member f alias-header-files))) #f]
    [else
     (define line-1
       (if (null? line1-segments)
           #f
           (string-join line1-segments "  ")))
     (define line-2
       (if (null? line2-segments)
           #f
           (string-join line2-segments "  ")))
     (define insert-lines (filter values (list line-1 line-2)))
     ;; Find insertion index: after last tag line within first 50 lines.
     (define tag-idx
       (for/fold ([acc #f])
                 ([line (in-list lines)]
                  [i (in-naturals)]
                  #:break (>= i 50))
         (if (regexp-match? tag-line-rx line) i acc)))
     (define lang-idx
       (for/or ([line (in-list lines)]
                [i (in-naturals)]
                #:break (>= i 5))
         (and (regexp-match? #rx"^#lang " line) i)))
     (define-values (idx extra-blank)
       (cond
         [tag-idx (values (add1 tag-idx) '())]
         [(and lang-idx
               (< (add1 lang-idx) (length lines))
               (string=? (list-ref lines (add1 lang-idx)) ""))
          (values (+ lang-idx 2) '())]
         [lang-idx (values (add1 lang-idx) '(""))]
         [else (values 0 '())]))
     (define new-lines (append (take lines idx) extra-blank insert-lines (drop lines idx)))
     (call-with-output-file p
                            #:exists 'replace
                            (lambda (out)
                              (for ([line (in-list new-lines)])
                                (displayln line out))))
     #t]))

;; ------------------------------------------------------------
;; Main
;; ------------------------------------------------------------

(define all-files (collect-test-files 'all))
(define changed 0)
(for ([f (in-list all-files)]
      [i (in-naturals)])
  (when (migrate-file f)
    (set! changed (add1 changed)))
  (when (zero? (modulo (add1 i) 200))
    (printf "processed ~a/~a~n" (add1 i) (length all-files))))
(printf "MIGRATION DONE: ~a/~a files rewritten~n" changed (length all-files))
