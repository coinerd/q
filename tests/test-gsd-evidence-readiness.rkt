#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @timeout 120
;; Fail-closed public readiness boundary. These tests deliberately do not
;; claim an authenticated GREEN/probe integration fixture.
(require rackunit
         racket/file
         racket/runtime-path
         racket/system
         json
         "../extensions/gsd/evidence-readiness.rkt"
         "../extensions/gsd/review-proof.rkt")
(define-runtime-path cli "../scripts/gsd-binding-data.rkt")
(module+ test
  (test-case "missing source evidence stays blocked through API and CLI"
    (define base (make-temporary-file "readiness-empty-~a" 'directory))
    (define proof-root (build-path base "owner"))
    (define plan (make-string 64 #\a))
    (dynamic-wind
     void
     (lambda ()
       (parameterize ([current-owned-proof-root proof-root])
         (define result (evidence-readiness base plan 1))
         (check-equal? (hash-ref result 'status) "blocked")
         (check-equal? (hash-ref result 'code) "invalid-current-evidence")
         (check-equal? (hash-ref result 'context) 'null)
         (check-equal? (hash-ref result 'spec) 'null)
         (check-regexp-match #rx"missing-owned-proof" (hash-ref result 'message))
         (check-exn #rx"missing-owned-proof" (lambda () (current-evidence-context base plan 1)))
         ;; A registration cannot bypass the SOURCE receipt boundary.
         (make-directory* (build-path base "evidence-repairs" plan "W1"))
         (call-with-output-file
          (build-path base "evidence-repairs" plan "W1" "ledger.json")
          (lambda (out) (write-json (hasheq 'registrations (list (hasheq 'revision 1))) out)))
         (check-equal? (hash-ref (evidence-readiness base plan 1) 'code) "invalid-current-evidence")
         ;; CLI must preserve the typed refusal, not manufacture a context.
         (define out (open-output-string))
         (define err (open-output-string))
         (define exit
           (parameterize ([current-output-port out]
                          [current-error-port err])
             (system*/exit-code (find-executable-path "racket")
                                cli
                                "readiness"
                                (path->string base)
                                plan
                                "1")))
         (check-equal? exit 0 (get-output-string err))
         (define response (string->jsexpr (get-output-string out)))
         (check-equal? (hash-ref response 'status) "blocked")
         (check-equal? (hash-ref response 'context) 'null)
         (check-equal? (hash-ref response 'spec) 'null)))
     (lambda () (delete-directory/files base)))))
