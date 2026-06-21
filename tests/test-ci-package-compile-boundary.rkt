#lang racket

;; @speed fast
;; @suite fast

;; W1 (#8504): Package-visible compile boundary tests.
;; Verifies that the 4 modules that previously failed `raco pkg setup`
;; compilation now compile cleanly.
;;
;; These are tested via `racket -e "(require ...)"` subprocess rather
;; than require because some are live/manual scripts that should not
;; execute at compile time.

(require rackunit
         rackunit/text-ui)

(define root "../")

(define (module-compiles? path-from-root)
  ;; Use subprocess to check if a module can be required without error.
  ;; This catches compile errors without running main (which is in module+ main).
  (define full-path (string-append root path-from-root))
  (define-values (proc out in err)
    (subprocess #f #f #f (find-executable-path "racket") "-e" (format "(require ~s)" full-path)))
  (subprocess-wait proc)
  (define code (subprocess-status proc))
  (define err-output (port->string err))
  (close-output-port in)
  (close-input-port out)
  (close-input-port err)
  (values (zero? code) err-output))

(define-test-suite
 package-compile-boundary-tests
 (test-case "F1: cli/generate-certificates.rkt compiles"
   (define-values (ok? err) (module-compiles? "cli/generate-certificates.rkt"))
   (check-true ok? err))
 (test-case "F2: scripts/sdk-gsd-integration-test.rkt compiles"
   ;; NOTE: This script has top-level code (not in module+ main),
   ;; so requiring it may execute live code. Test only compilation
   ;; by checking raco make success via the compiled/ directory existence.
   (define-values (ok? err) (module-compiles? "scripts/sdk-gsd-integration-test.rkt"))
   ;; This script has top-level side effects, so requiring may fail at runtime
   ;; even if compile succeeds. We verify compile via raco make instead.
   ;; For now, skip this one - it's covered by the all-modules test below.
   (check-true #t "compile verified by W2 package gate"))
 (test-case "F3: scripts/test-gsd-go-replanning.rkt compiles"
   ;; Same pattern - manual test script with top-level code
   (check-true #t "compile verified by W2 package gate"))
 (test-case "F4: scripts/test-gsd-sdk-live.rkt compiles"
   ;; Same pattern - live script with top-level code
   (check-true #t "compile verified by W2 package gate"))
 (test-case "All 4 previously-failing modules compile via raco make"
   ;; Use raco make as subprocess to verify compilation without execution
   (for ([path '("cli/generate-certificates.rkt" "scripts/sdk-gsd-integration-test.rkt"
                                                 "scripts/test-gsd-go-replanning.rkt"
                                                 "scripts/test-gsd-sdk-live.rkt")])
     (define full-path (string-append root path))
     (define-values (proc out in err)
       (subprocess #f #f #f (find-executable-path "raco") "make" full-path))
     (subprocess-wait proc)
     (define code (subprocess-status proc))
     (define err-output (port->string err))
     (close-output-port in)
     (close-input-port out)
     (close-input-port err)
     (check-true (zero? code) (format "~a failed: ~a" path err-output)))))

(module+ test
  (run-tests package-compile-boundary-tests))

(module+ main
  (run-tests package-compile-boundary-tests))
