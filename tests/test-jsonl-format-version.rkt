#lang racket/base
;; BOUNDARY: serialization

;; tests/test-jsonl-format-version.rkt — JSONL format header tests (F8)

(require rackunit
         racket/file
         "../util/json/jsonl.rkt")

(define (make-temp-jsonl)
  (make-temporary-file "q-jsonl-fmt-~a.jsonl"))

(test-case "jsonl-format-version is positive integer"
  (check-true (exact-positive-integer? jsonl-format-version)))

(test-case "format header is written when #:format-header? #t on new file"
  (define path (make-temp-jsonl))
  (dynamic-wind (lambda () (void))
                (lambda ()
                  (delete-file path)
                  (jsonl-append! path (hasheq 'test #t) #:format-header? #t)
                  (define entries (jsonl-read-all path))
                  (check-equal? (length entries) 2)
                  (check-equal? (hash-ref (car entries) '__format #f) "jsonl")
                  (check-equal? (hash-ref (car entries) 'version #f) jsonl-format-version))
                (lambda ()
                  (when (file-exists? path)
                    (delete-file path)))))

(test-case "format header not written when #:format-header? omitted"
  (define path (make-temp-jsonl))
  (dynamic-wind (lambda () (void))
                (lambda ()
                  (delete-file path)
                  (jsonl-append! path (hasheq 'test #t))
                  (define entries (jsonl-read-all path))
                  (check-equal? (length entries) 1)
                  (check-false (hash-ref (car entries) '__format #f)))
                (lambda ()
                  (when (file-exists? path)
                    (delete-file path)))))

(test-case "format header not duplicated on subsequent appends"
  (define path (make-temp-jsonl))
  (dynamic-wind (lambda () (void))
                (lambda ()
                  (delete-file path)
                  (jsonl-append! path (hasheq 'a 1) #:format-header? #t)
                  (jsonl-append! path (hasheq 'b 2) #:format-header? #t)
                  (define entries (jsonl-read-all path))
                  ;; Only 1 header + 2 data entries
                  (check-equal? (length entries) 3)
                  (define headers
                    (filter (lambda (e) (equal? (hash-ref e '__format #f) "jsonl")) entries))
                  (check-equal? (length headers) 1))
                (lambda ()
                  (when (file-exists? path)
                    (delete-file path)))))

(test-case "strip-format-header removes header when present"
  (define entries (list (hasheq '__format "jsonl" 'version 1) (hasheq 'data #t)))
  (define stripped (strip-format-header entries))
  (check-equal? (length stripped) 1)
  (check-equal? (hash-ref (car stripped) 'data) #t))

(test-case "strip-format-header passes through when no header"
  (define entries (list (hasheq 'data #t) (hasheq 'more #t)))
  (define stripped (strip-format-header entries))
  (check-equal? (length stripped) 2))

(test-case "strip-format-header handles empty list"
  (check-equal? (strip-format-header '()) '()))
