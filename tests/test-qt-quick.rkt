#lang racket
;; @speed fast
;; @suite testing
;; @boundary integration
(require rackunit
         racket/runtime-path)
(define-runtime-path qt "../qt")

(define (run-command timeout executable . args)
  (define cust (make-custodian))
  (define proc #f)
  (dynamic-wind void
                (lambda ()
                  (parameterize ([current-custodian cust]
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
                      (error 'run-command "subprocess timed out after ~a seconds" timeout))
                    (values (subprocess-status child) (string-append stdout stderr))))
                (lambda ()
                  (when proc
                    (when (eq? (subprocess-status proc) 'running)
                      (subprocess-kill proc #t))
                    (subprocess-wait proc))
                  (custodian-shutdown-all cust))))

(define (run . args)
  (apply run-command 300 (find-executable-path "racket") qt args))
(define (quick-supported?)
  (and (eq? (system-type 'os*) 'linux)
       (directory-exists? "/proc/self")
       (find-executable-path "setsid")))
(module+ test
  (test-case "subprocess helper drains both pipes and times out"
    (define racket-exe (find-executable-path "racket"))
    (define-values (status text)
      (run-command
       10
       racket-exe
       "-e"
       "(begin (display (make-string 200000 #\\e) (current-error-port)) (display (make-string 200000 #\\o)))"))
    (check-equal? status 0)
    (check-equal? (string-length text) 400000)
    (check-exn #rx"timed out" (lambda () (run-command 0.2 racket-exe "-e" "(sleep 30)"))))
  (test-case "entire quick suite runs through bounded queue"
    (if (quick-supported?)
        (let-values ([(status text) (run "quick" "--jobs" "2")])
          (check-equal? status 0 text)
          ;; §3 expansion grew the reviewed owner table from 4 to 28 owners;
          ;; W5 added the quick child-run classifier and R3c added the pure
          ;; delivery-stop decision; R3d retired the recursive arch source-scan
          ;; and v09945 archival audit owners (23 owners,
          ;; capability-denylist-scanned, generated admission record); the pin
          ;; moves with that contract.
          (check-regexp-match #rx"quick passed: 23 files" text)
          (check-regexp-match #rx"not PR/full verification" text))
        (displayln "SKIP quick suite: Linux /proc or setsid unavailable")))
  (test-case "campaign decisions execute and count six test cases"
    (if (quick-supported?)
        (let-values ([(status text) (run "quick" "tests/test-gsd-campaign-decision-pure.rkt")])
          (check-equal? status 0 text)
          (check-regexp-match #rx"quick passed: 1 files, 6 tests" text))
        (displayln "SKIP campaign decisions: Linux /proc or setsid unavailable")))
  ;; The former "delivery verifier executes all ten decisions once" case was
  ;; removed (W4b): it asserted the SAME single-file CLI contract as the
  ;; campaign case above (named admitted owner runs, counted pass line) with a
  ;; different payload, and that payload (tests/test-gsd-delivery-verifier-
  ;; decision.rkt) is an audited quick owner, so the whole-suite case below
  ;; already executes its ten decisions on every run of this file.
  (test-case "invalid jobs and paths fail"
    (for ([args '(("quick" "--jobs" "0") ("quick" "--jobs")
                                         ("quick" "../tests/test-vdom-html-render.rkt")
                                         ("quick" "--bogus"))])
      (define-values (status text) (apply run args))
      (check-not-equal? status 0 text))))
