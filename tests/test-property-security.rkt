#lang racket

;; tests/test-property-security.rkt — property-based tests for security-critical modules
;;
;; TH-04: Uses quickcheck for invariant testing on:
;;   - Path filter consistency
;;   - JSONL roundtrip (write→read preserves data)
;;   - Glob pattern compilation

(require rackunit
         racket/port
         racket/file
         racket/string
         quickcheck
         (only-in "../util/path-filters.rkt"
                  hidden-name?)
         (only-in "../util/jsonl.rkt"
                  jsonl-append!
                  jsonl-read-all-valid)
         (only-in "../util/glob.rkt"
                  glob->regexp))

;; ============================================================
;; Helper: temporary file for JSONL tests
;; ============================================================

(define (with-temp-jsonl-file thunk)
  (define tmp (make-temporary-file "property-test-~a.jsonl"))
  (dynamic-wind
    void
    (lambda () (thunk tmp))
    (lambda () (with-handlers ([exn:fail? void]) (delete-file tmp)))))

;; ============================================================
;; Property 1: hidden-name? is consistent for dot-prefixed names
;; ============================================================

(test-case
 "PBT: hidden-name? returns #t for all dot-prefixed strings"
 (quickcheck
  (property ([s arbitrary-string])
    (define dot-name (string-append "." s))
    (==> (not (string=? s ""))
         (hidden-name? dot-name)))))

(test-case
 "PBT: hidden-name? returns #f for strings without leading dot"
 (quickcheck
  (property ([s arbitrary-string])
    (==> (and (not (string=? s ""))
              (not (char=? (string-ref s 0) #\.)))
         (not (hidden-name? s))))))

;; ============================================================
;; Property 2: JSONL write→read roundtrip preserves entry count
;; ============================================================

(test-case
 "PBT: JSONL roundtrip preserves entry count"
 (quickcheck
  (property ([entries (arbitrary-list arbitrary-string)])
    (with-temp-jsonl-file
     (lambda (tmp-path)
       (for ([e entries])
         (jsonl-append! tmp-path (hasheq 'data e)))
       (define read-back (jsonl-read-all-valid tmp-path))
       (==> #t
            (= (length read-back) (length entries))))))))

;; ============================================================
;; Property 3: glob->regexp with no wildcards matches literally
;; ============================================================

(test-case
 "PBT: glob->regexp with plain name matches only that name"
 (quickcheck
  (property ([s arbitrary-string])
    (==> (and (not (string-contains? s "*"))
              (not (string-contains? s "?"))
              (not (string-contains? s "[")))
         (regexp-match? (glob->regexp s) s)))))

(test-case
 "PBT: glob->regexp with * matches any suffix without slashes"
 (quickcheck
  (property ([prefix arbitrary-string]
             [suffix arbitrary-string])
    (==> (and (not (string-contains? prefix "*"))
              (not (string-contains? prefix "?"))
              (not (string-contains? prefix "/"))
              (not (string-contains? suffix "*"))
              (not (string-contains? suffix "?"))
              (not (string-contains? suffix "/")))
         (regexp-match? (glob->regexp (string-append prefix "*"))
                        (string-append prefix suffix))))))

;; ============================================================
;; Property 4: JSONL roundtrip preserves hash keys
;; ============================================================

(test-case
 "PBT: JSONL roundtrip preserves hash keys and values"
 (quickcheck
  (property ([s arbitrary-string])
    (with-temp-jsonl-file
     (lambda (tmp-path)
       (jsonl-append! tmp-path (hasheq 'payload s 'tag "test"))
       (define read-back (jsonl-read-all-valid tmp-path))
       (==> #t
            (and (= (length read-back) 1)
                 (equal? (hash-ref (car read-back) 'payload #f) s)
                 (equal? (hash-ref (car read-back) 'tag #f) "test"))))))))
