#lang racket/base
(require json
         racket/list
         racket/string
         "../../util/json/checksum.rkt")
(provide canonical-evidence-json
         canonical-evidence-digest
         valid-evidence-context?
         evidence-context-digest
         valid-probe-spec?)
;; Deterministic JSON subset: symbol object keys, exact integers, no floats.
(define (canonical-evidence-json value)
  (define (encode v)
    (cond
      [(hash? v)
       (unless (andmap symbol? (hash-keys v))
         (error 'canonical-evidence-json "object keys must be symbols"))
       (string-append "{"
                      (string-join (for/list ([k (in-list (sort (hash-keys v) symbol<?))])
                                     (string-append (jsexpr->string (symbol->string k))
                                                    ":"
                                                    (encode (hash-ref v k))))
                                   ",")
                      "}")]
      [(list? v) (string-append "[" (string-join (map encode v) ",") "]")]
      [(or (string? v) (boolean? v) (eq? v 'null) (exact-integer? v)) (jsexpr->string v)]
      [else (error 'canonical-evidence-json "unsupported JSON value: ~e" v)]))
  (encode value))
(define (canonical-evidence-digest value)
  (sha256-string (canonical-evidence-json (if (hash? value)
                                              (hash-remove value 'sha256)
                                              value))))
(define context-fields
  '(plan-id wave
            attempt-id
            attempt-fence
            receipt-generation
            receipt-sha256
            evidence-revision
            repo
            branch
            origin
            head
            tree
            snapshot-sha256
            green-proof-sha256))
(define (hex? x n)
  (and (string? x) (= (string-length x) n) (regexp-match? #px"^[0-9a-f]+$" x)))
(define (valid-evidence-context? c)
  (and (hash? c)
       (for/and ([k (in-list context-fields)])
         (hash-has-key? c k))
       (for/and ([k '(wave attempt-fence receipt-generation evidence-revision)])
         (exact-nonnegative-integer? (hash-ref c k)))
       (for/and ([k '(plan-id receipt-sha256 snapshot-sha256 green-proof-sha256)])
         (hex? (hash-ref c k) 64))
       (for/and ([k '(head tree)])
         (hex? (hash-ref c k) 40))
       (for/and ([k '(attempt-id repo branch origin)])
         (define v (hash-ref c k))
         (and (string? v) (positive? (string-length v))))))
(define (valid-probe-spec? s)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define files (append (hash-ref s 'test-files) (hash-ref s 'harness-files)))
    (define paths (map (lambda (e) (hash-ref e 'path)) files))
    (and (equal? (hash-ref s 'kind) "probe-spec/v1")
         (valid-evidence-context? (hash-ref s 'context))
         (hex? (hash-ref s 'baseline-head) 40)
         (hex? (hash-ref s 'baseline-tree) 40)
         (equal? (hash-ref s 'suite-export) "evidence-suite")
         (equal? (hash-ref s 'test-paths)
                 (map (lambda (e) (hash-ref e 'path)) (hash-ref s 'test-files)))
         (= (length (hash-ref s 'test-paths)) 1)
         (= (length paths) (length (remove-duplicates paths)))
         (andmap (lambda (e)
                   (and (hex? (hash-ref e 'sha256) 64)
                        (regexp-match? #px"^tests/[A-Za-z0-9_./-]+[.]rkt$" (hash-ref e 'path))
                        (not (member ".." (string-split (hash-ref e 'path) "/")))))
                 files)
         (string? (hash-ref s 'focused-normalized-command))
         (positive? (string-length (hash-ref s 'focused-normalized-command)))
         (exact-positive-integer? (hash-ref s 'timeout-sec))
         (<= (hash-ref s 'timeout-sec) 600))))
(define (evidence-context-digest c)
  (unless (valid-evidence-context? c)
    (error 'evidence-context-digest "invalid context"))
  (canonical-evidence-digest c))
