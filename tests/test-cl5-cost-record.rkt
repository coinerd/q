#lang racket

;; Tests for the CL5 cost-record summariser/verifier (#9982). The verifier is
;; fail-closed: every test below either proves a re-derivation or proves a refusal.

(require json
         rackunit
         racket/string
         "../scripts/cl5/cost-record.rkt")

(define rev "0123456789abcdef0123456789abcdef01234567")
(define tree "89abcdef0123456789abcdef0123456789abcdef")
(define host (hash 'nproc 4))

(define (files entries) ; entries: (list path elapsed-ms status)
  (for/list ([e (in-list entries)])
    (match-define (list path ms status) e)
    (hash 'path path 'elapsed_ms ms 'status status)))

(define (producer #:name [name "fast-0"]
                  #:wall [wall 10.0]
                  #:elapsed
                  [elapsed (list (list "tests/a.rkt" 1000 "pass") (list "tests/b.rkt" 3000 "pass"))]
                  #:verdict [verdict "pass"]
                  #:file-count [file-count #f]
                  #:timeout [timeout 0]
                  #:serial [serial #f])
  (define fs (files elapsed))
  (record-producer name
                   'runner
                   "fast"
                   "0/3"
                   4
                   "./qt --lane fast"
                   fs
                   wall
                   1000
                   (or file-count (length fs))
                   0
                   0
                   timeout
                   0
                   verdict))

(define (record #:producers ps #:cache [cache "warm"])
  (build-record #:revision rev #:tree tree #:host host #:cache cache #:producers ps))

(define (update-producer j key value)
  (hash-set j 'producers (list (hash-set (car (hash-ref j 'producers)) key value))))

(define (rec->json-hash rec)
  (with-input-from-string (jsexpr->string (record->jsexpr rec)) read-json))

(test-case "a consistent record verifies and re-derives its totals"
  (define rec (record #:producers (list (producer) (producer #:name "fast-1" #:wall 20.0))))
  (define s (verify-record (rec->json-hash rec)))
  (check-equal? (hash-ref s 'producers) 2)
  (check-equal? (hash-ref s 'comprehensive-wall-s) 30.0)
  (check-equal? (hash-ref s 'files) 4)
  (check-equal? (hash-ref s 'serial-ms) 8000))

(test-case "a producer whose declared serial_ms is wrong is refused"
  (define j
    (update-producer (rec->json-hash (record #:producers (list (producer)))) 'serial_ms 999999))
  (check-exn #rx"declared serial_ms" (lambda () (verify-record j))))

(test-case "a producer whose declared max_ms is wrong is refused"
  (define j (update-producer (rec->json-hash (record #:producers (list (producer)))) 'max_ms 1))
  (check-exn #rx"declared max_ms" (lambda () (verify-record j))))

(test-case "a comprehensive total that is not the producer sum is refused"
  (define rec (record #:producers (list (producer))))
  (define j
    (hash-set (rec->json-hash rec)
              'comprehensive
              (hash-set (hash-ref (rec->json-hash rec) 'comprehensive) 'wall_s 999.0)))
  (check-exn #rx"comprehensive wall" (lambda () (verify-record j))))

(test-case "a producer claiming a pass while recording a timeout is refused"
  (define p
    (producer #:elapsed (list (list "tests/a.rkt" 1000 "pass") (list "tests/b.rkt" 120000 "timeout"))
              #:timeout 1
              #:verdict "pass"))
  (check-exn #rx"claims a pass"
             (lambda () (verify-record (rec->json-hash (record #:producers (list p)))))))

(test-case "duplicate file entries inside one producer are refused"
  (define p
    (producer #:elapsed (list (list "tests/a.rkt" 1000 "pass") (list "tests/a.rkt" 2000 "pass"))))
  (check-exn #rx"duplicate file entry"
             (lambda () (verify-record (rec->json-hash (record #:producers (list p)))))))

(test-case "a file_count that disagrees with the recorded files is refused"
  (define p (struct-copy record-producer (producer) [file-count 5]))
  (check-exn #rx"declares file_count"
             (lambda () (verify-record (rec->json-hash (record #:producers (list p)))))))

(test-case "an unknown schema is refused"
  (define j (hash-set (rec->json-hash (record #:producers (list (producer)))) 'schema "cl5-cost/0"))
  (check-exn #rx"unknown schema" (lambda () (verify-record j))))

(test-case "a non-sha revision or tree is refused"
  (check-exn #rx"revision"
             (lambda ()
               (build-record #:revision "HEAD"
                             #:tree tree
                             #:host host
                             #:cache "warm"
                             #:producers (list (producer)))))
  (check-exn #rx"tree"
             (lambda ()
               (build-record #:revision rev
                             #:tree "main"
                             #:host host
                             #:cache "warm"
                             #:producers (list (producer))))))

(test-case "an unlabelled cache state is refused"
  (check-exn #rx"cache must be"
             (lambda ()
               (build-record #:revision rev
                             #:tree tree
                             #:host host
                             #:cache "lukewarm"
                             #:producers (list (producer))))))

(test-case "an empty producer list or duplicate producer names are refused"
  (check-exn #rx"at least one producer" (lambda () (record #:producers '())))
  (check-exn #rx"duplicate producer names"
             (lambda () (record #:producers (list (producer) (producer))))))

(test-case "a record with a missing host descriptor is refused"
  (define j (hash-set (rec->json-hash (record #:producers (list (producer)))) 'host (hash)))
  (check-exn #rx"host descriptor" (lambda () (verify-record j))))
