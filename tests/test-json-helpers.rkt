#lang racket/base

(require rackunit
         racket/file
         json
         "../util/json-helpers.rkt")

(define tmp-dir (make-temporary-file "json-helpers-test-~a" 'directory))

(define (tmp-path name)
  (build-path tmp-dir name))

;; Basic read/write round-trip
(test-case "read-json-file / write-json-file round-trip"
  (define p (tmp-path "basic.json"))
  (define data (hasheq 'name "test" 'value 42 'items '(1 2 3)))
  (write-json-file p data)
  (check-equal? (read-json-file p) data)
  (delete-file p))

;; write with #:exists 'replace
(test-case "write-json-file #:exists 'replace"
  (define p (tmp-path "replace.json"))
  (write-json-file p (hasheq 'v 1))
  (write-json-file p (hasheq 'v 2) #:exists 'replace)
  (check-equal? (read-json-file p) (hasheq 'v 2))
  (delete-file p))

;; read non-existent file raises exception
(test-case "read-json-file missing file"
  (define p (tmp-path "nonexistent.json"))
  (check-exn exn:fail:filesystem? (lambda () (read-json-file p))))

;; write nested structures
(test-case "write-json-file nested structures"
  (define p (tmp-path "nested.json"))
  (define data (hasheq 'outer (hasheq 'inner (list (hasheq 'x 1) (hasheq 'y 2)))))
  (write-json-file p data)
  (check-equal? (read-json-file p) data)
  (delete-file p))

;; Cleanup
(delete-directory/files tmp-dir)
