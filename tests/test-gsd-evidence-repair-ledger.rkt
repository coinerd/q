#lang racket
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @timeout 120
(require rackunit
         racket/file
         json
         "../extensions/gsd/review-proof.rkt"
         "../extensions/gsd/evidence-repair-ledger.rkt")
(module+ test
  (test-case "durable bounded reservation, cross-process exclusion and registration integrity"
    (define root (make-temporary-file "ledger~a" 'directory))
    (define plan (make-string 64 #\a))
    (define c
      (hasheq 'plan-id
              plan
              'wave
              0
              'attempt-id
              "a"
              'attempt-fence
              1
              'receipt-generation
              1
              'receipt-sha256
              (make-string 64 #\b)
              'evidence-revision
              0
              'repo
              "repo"
              'branch
              "b"
              'origin
              "o"
              'head
              (make-string 40 #\c)
              'tree
              (make-string 40 #\d)
              'snapshot-sha256
              (make-string 64 #\e)
              'green-proof-sha256
              (make-string 64 #\f)))
    (parameterize ([current-owned-proof-root (build-path root "proofs")])
      (check-exn exn:fail?
                 (lambda () (reserve-evidence-repair! root (hash-set c 'head "bad") "spec")))
      (check-equal? (hash-ref (reserve-evidence-repair! root c "spec") 'status) "reserved")
      (define lock-path (build-path root "evidence-repairs" plan "W0.g1" "ledger.lock"))
      (define-values (child from-child to-child child-err)
        (subprocess
         #f
         #f
         #f
         (find-executable-path "racket")
         "-e"
         (format (string-append
                  "(require racket/file) "
                  "(call-with-file-lock/timeout #f 'exclusive "
                  "(lambda () (displayln 'locked) (flush-output) (read-line)) void #:lock-file ~s)")
                 (path->string lock-path))))
      (check-equal? (read-line from-child) "locked")
      (check-exn #rx"busy" (lambda () (load-evidence-repair root plan 0 1)))
      (displayln "release" to-child)
      (flush-output to-child)
      (subprocess-wait child)
      (check-equal? (subprocess-status child) 0)
      (close-input-port from-child)
      (close-input-port child-err)
      (close-output-port to-child)
      (check-exn #rx"resource identity"
                 (lambda () (load-evidence-repair (build-path root "other") plan 0 1)))
      (check-equal? (hash-ref (reserve-evidence-repair! root c "spec") 'status) "existing")
      (check-equal? (hash-ref (reserve-evidence-repair! root c "different") 'status) "stale")
      (check-equal? (hash-ref (reserve-evidence-repair! root (hash-set c 'attempt-id "b") "spec")
                              'status)
                    "exhausted")
      (check-exn exn:fail?
                 (lambda ()
                   (advance-evidence-repair! root c "repair-1" "start" (hasheq 'state "captured"))))
      (advance-evidence-repair! root c "repair-1" "reserved" (hasheq 'state "intent"))
      (advance-evidence-repair! root c "repair-1" "intent" (hasheq 'state "start"))
      (check-equal? (hash-ref (load-evidence-repair root plan 0 1) 'resume) "paused")
      (advance-evidence-repair! root c "repair-1" "start" (hasheq 'state "captured" 'probe "proof"))
      (define secret (gensym))
      (define consumed? #f)
      (define register!
        (make-evidence-registrar (lambda (auth context probe id)
                                   (begin0 (and (eq? auth secret) (not consumed?))
                                     (set! consumed? #t)))))
      (define reg
        (register! root
                   plan
                   0
                   #:context c
                   #:probe "proof"
                   #:authorization secret
                   #:operation-id "repair-1"))
      (check-equal? reg
                    (register! root
                               plan
                               0
                               #:context c
                               #:probe "proof"
                               #:authorization secret
                               #:operation-id "repair-1"))
      (check-exn exn:fail?
                 (lambda ()
                   (register! root
                              plan
                              0
                              #:context c
                              #:probe "other"
                              #:authorization secret
                              #:operation-id "repair-1")))
      (check-exn exn:fail?
                 (lambda ()
                   (register! root
                              plan
                              0
                              #:context c
                              #:probe "proof"
                              #:authorization #t
                              #:operation-id "repair-1")))
      (check-equal? (hash-ref reg 'revision) 1)
      (check-equal? (hash-ref reg 'prior-registration-sha256) 'null)
      (define ledger-path (build-path root "evidence-repairs" plan "W0.g1" "ledger.json"))
      (define saved (call-with-input-file ledger-path read-json))
      (call-with-output-file ledger-path
                             #:exists 'truncate
                             (lambda (out) (write-json (hash-set saved 'operation 'null) out)))
      (check-exn #rx"integrity" (lambda () (load-evidence-repair root plan 0 1)))
      ;; A distinct receipt generation has its OWN bounded slot at its own
      ;; path: the exhausted g1 budget never blocks an authorized g2
      ;; reservation, while the same generation stays exhausted for foreign
      ;; contexts. Generation 0 keeps the legacy un-suffixed path.
      (check-equal?
       (hash-ref (reserve-evidence-repair!
                  root
                  (hash-set* c 'receipt-generation 2 'receipt-sha256 (make-string 64 #\b))
                  "spec")
                 'status)
       "reserved")
      (check-equal?
       (hash-ref (reserve-evidence-repair!
                  root
                  (hash-set* c 'receipt-generation 2 'receipt-sha256 (make-string 64 #\b))
                  "other-spec")
                 'status)
       "stale")
      (check-true (file-exists? (build-path root "evidence-repairs" plan "W0.g2" "ledger.json")))
      (check-false (file-exists? (build-path root "evidence-repairs" plan "W0" "ledger.json"))))
    (delete-directory/files root)))
