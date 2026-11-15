#lang racket
;; @speed fast
;; @suite testing
;; @boundary integration

(require rackunit
         racket/runtime-path)

(define-runtime-path qt "../qt")

;; Read both pipes while the child runs; neither pipe may block the other.
(define (invoke-command timeout cwd executable . args)
  (define cust (make-custodian))
  (define proc #f)
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-custodian cust]
                                 [current-directory cwd]
                                 [subprocess-group-enabled #t])
                    (define-values (child out in err) (apply subprocess #f #f #f executable args))
                    (set! proc child)
                    (close-output-port in)
                    (define stdout #f)
                    (define stderr #f)
                    (define out-thread (thread (lambda () (set! stdout (port->string out)))))
                    (define err-thread (thread (lambda () (set! stderr (port->string err)))))
                    (define done
                      (thread (lambda ()
                                (subprocess-wait child)
                                (thread-wait out-thread)
                                (thread-wait err-thread))))
                    (unless (sync/timeout timeout done)
                      (error 'invoke-command "subprocess timed out after ~a seconds" timeout))
                    (values (subprocess-status child) stdout stderr)))
                (lambda ()
                  (when proc
                    (when (eq? (subprocess-status proc) 'running)
                      (subprocess-kill proc #t))
                    (subprocess-wait proc))
                  (custodian-shutdown-all cust))))

(define (invoke . args)
  (apply invoke-command 300 (current-directory) (find-executable-path "racket") qt args))

;; Run the qt CLI from an arbitrary working directory (launch-tree resolution
;; depends on it).
(define (invoke-in cwd . args)
  (apply invoke-command 300 cwd (find-executable-path "racket") qt args))

(module+ test
  (test-case "subprocess helper drains both pipes and times out"
    (define racket-exe (find-executable-path "racket"))
    (define-values (status out err)
      (invoke-command
       10
       (current-directory)
       racket-exe
       "-e"
       "(begin (display (make-string 200000 #\\e) (current-error-port)) (display (make-string 200000 #\\o)))"))
    (check-equal? status 0)
    (check-equal? (string-length out) 200000)
    (check-equal? (string-length err) 200000)
    (check-exn #rx"timed out"
               (lambda () (invoke-command 0.2 (current-directory) racket-exe "-e" "(sleep 30)"))))
  (test-case "quick explain selects audited tests without execution"
    (define-values (status out err) (invoke "quick" "--explain"))
    (check-equal? status 0)
    (check-regexp-match #rx"test-vdom-html-render.rkt" out)
    (check-regexp-match #rx"selection only" out))
  ;; `full`, `default` and `release` are supported lanes now, so an unsupported
  ;; COMMAND no longer belongs in the same list as an unsupported OPTION; both
  ;; still fail closed before any selection or execution. Successful plan and
  ;; execution paths live in tests/test-qt-full.rkt and tests/test-qt-lanes.rkt,
  ;; which use fixture roots (repo-wide discovery costs ~25s per fresh process).
  (test-case "unadmitted files, unsupported claims and bad full args fail closed"
    ;; Every entry is a spelling that MUST fail closed (the positive cases live
    ;; in tests/test-qt-interface.rkt, test-qt-full.rkt and test-qt-lanes.rkt):
    ;; an unadmitted quick file, --changed without a range, an unknown command,
    ;; a non-SHA release candidate, an unsupported option, and a bare `./qt`
    ;; with no command at all.
    (for ([args '(#("quick" "tests/test-verifier-risk-threshold.rkt")
                  #("quick" "--changed")
                  #("bogus-command")
                  #("release" "--candidate" "HEAD")
                  #("full" "--bogus")
                  #())])
      (define-values (status out err) (apply invoke (vector->list args)))
      (check-not-equal? status 0)))
  (test-case "full explain from a tree without tests/ fails through qt:"
    ;; No tests/ directory at all: selection used to escape as a raw Racket
    ;; error. The CLI must report it as a `qt:` failure with exit 1.
    (define dir (make-temporary-file "qt-empty-~a" 'directory))
    (dynamic-wind void
                  (lambda ()
                    (define-values (status out err) (invoke-in dir "full" "--explain"))
                    (check-not-equal? status 0)
                    (check-regexp-match #rx"^qt: " err)
                    (check-false (regexp-match? #rx"NOT VERIFIED" out)
                                 "a failed explain must not print a plan"))
                  (lambda () (delete-directory/files dir))))
  (test-case "focused quick executes real assertions"
    (if (and (eq? (system-type 'os*) 'linux)
             (directory-exists? "/proc/self")
             (find-executable-path "setsid"))
        (let-values ([(status out err) (invoke "quick" "tests/test-vdom-html-render.rkt")])
          (check-equal? status 0 (string-append out err))
          (check-regexp-match #rx"quick" out))
        (displayln "SKIP focused quick: Linux /proc or setsid unavailable"))))
