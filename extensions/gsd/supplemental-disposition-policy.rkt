#lang racket/base
;; Immutable identity only: never a broker grant or an authorization callback.
(require racket/file
         racket/path
         "supplemental-evidence-format.rkt"
         "supplemental-evidence-authority.rkt")
(provide disposition-amendment
         disposition-amendment-1
         disposition-amendment-2)
(define production-home (simplify-path (find-system-path 'home-dir) #f))
(define production-root (build-path production-home ".q" "agent" "supplemental-evidence"))
(define (ref kind digest)
  (hasheq 'kind kind 'sha256 digest))
(define disposition-amendment-1
  (seal-supplemental-record
   (hasheq 'kind
           "supplemental-disposition-amendment/v1"
           'plan-id
           "79a69b427b40fb6c6b68b4fd1484a74551db182afa9b96f065353324af16b19e"
           'wave
           1
           'root-proposal
           (ref "task-proposal" "30be1b8ecd87ee21246d19473a338a571f3e0ef13dea7fe214824163280ae31c")
           'root-approval
           (ref "task-approval" "87838ebf39cea992c9064590df7c35078f2d6ff64c52c37ddb9c62bef4cc5b78")
           'parent-proposal
           (ref "task-proposal" "77538def70739c9260a0920705a7174baf2f326dbbffb1e560d4b57c2fccc629")
           'parent-approval
           (ref "task-approval" "8b34d850c38078e0263946da930b84c76fd3b8d829428d20b37f4c00c09afd27")
           'terminal-body-sha256
           "209accd0ca0f80a7f6d3fc562baeedafcb96396d98019a1ae861f160dfa5ca17"
           'terminal-record-sha256
           "7412b190a534cd291eb676a10bcbee75196f3f5fd3b42f2c04d5924cec94ae40"
           'allocation
           (hasheq 'attempt-id "attempt-4" 'attempt-fence 7 'coordinator-fence 7)
           'snapshot-sha256
           "5d10ae297263d52f60e2305365b299f6060c232c9459168fc8b1fdba32071077"
           'model-dispatches
           1
           'successor-permitted
           #f)))
(define disposition-amendment-2
  (seal-supplemental-record
   (hash-set* disposition-amendment-1
              'v2-proposal
              (hash-ref disposition-amendment-1 'parent-proposal)
              'v2-approval
              (hash-ref disposition-amendment-1 'parent-approval)
              'parent-proposal
              (ref "task-proposal" "f4238535e291e23062926f698c922da75fd3caadcd2de160e8b3ca68b33ba764")
              'parent-approval
              (ref "task-approval" "f8ea96b875e8b05fc2f5800674270c6dddf93b4e256fa6b8fc1ed650fcd5b816")
              'terminal-body-sha256
              "b60c5f76528c76ff66ce98943633c93fed570e80b60a99192150d178c86d8259"
              'terminal-record-sha256
              "0c9e45d27cfb3934c29dc9bdc44686b62a6011ddf21c08d8c507beaf88e6e5c0"
              'allocation
              (hasheq 'attempt-id "attempt-5" 'attempt-fence 8 'coordinator-fence 8))))
(define default-amendment disposition-amendment-2)
(define fixture-amendment (make-parameter #f))
(define fixture-predecessor (make-parameter #f))
(module+ ancestry-support
  (provide disposition-predecessor)
  (define (disposition-predecessor)
    (if (fixture-amendment)
        (fixture-predecessor)
        disposition-amendment-1)))
(define (disposition-amendment)
  (define fixture (fixture-amendment))
  (cond
    [fixture
     (unless (equal? (car fixture) (simplify-path (current-supplemental-authority-root) #f))
       (error 'disposition-amendment "fixture authority root changed"))
     (cdr fixture)]
    [else default-amendment]))
(module+ test-support
  (provide call-with-test-disposition-amendment)
  (define (call-with-test-disposition-amendment amendment thunk #:predecessor [predecessor #f])
    (define raw (current-supplemental-authority-root))
    (unless (and (path-string? raw) (complete-path? raw))
      (error 'call-with-test-disposition-amendment "isolated temporary authority root required"))
    (define root (simplify-path raw #f))
    (define temp (simplify-path (find-system-path 'temp-dir) #f))
    (define (within? p base)
      (define parts (explode-path p))
      (define prefix (explode-path base))
      (and (> (length parts) (length prefix)) (equal? prefix (take-prefix parts (length prefix)))))
    (define (take-prefix xs n)
      (if (zero? n)
          '()
          (cons (car xs) (take-prefix (cdr xs) (sub1 n)))))
    (unless (and (not (equal? root production-root))
                 (not (within? root production-home))
                 (within? root temp)
                 (directory-exists? root)
                 (equal? root (resolve-path root))
                 (= #o700 (bitwise-and #o7777 (file-or-directory-permissions root 'bits))))
      (error 'call-with-test-disposition-amendment "refusing real/default or nonisolated root"))
    (let check ([p root])
      (when (link-exists? p)
        (error 'call-with-test-disposition-amendment "symlink root component"))
      (define-values (base _name _dir?) (split-path p))
      (when (path? base)
        (check base)))
    (unless (and
             (hash? amendment)
             (immutable? amendment)
             (equal?
              (sort (hash-keys amendment) symbol<?)
              (sort (hash-keys (if predecessor disposition-amendment-2 disposition-amendment-1))
                    symbol<?))
             (equal? amendment (seal-supplemental-record amendment))
             (equal? (hash-ref amendment 'kind) "supplemental-disposition-amendment/v1")
             (for/and ([key '(plan-id terminal-body-sha256 terminal-record-sha256 snapshot-sha256)])
               (define v (hash-ref amendment key))
               (and (string? v) (regexp-match? #px"^[0-9a-f]{64}$" v)))
             (exact-nonnegative-integer? (hash-ref amendment 'wave))
             (for/and ([key '(root-proposal root-approval parent-proposal parent-approval)]
                       [kind '("task-proposal" "task-approval" "task-proposal" "task-approval")])
               (define r (hash-ref amendment key))
               (and (supplemental-reference? r) (equal? (hash-ref r 'kind) kind)))
             (let ([a (hash-ref amendment 'allocation)])
               (and (hash? a)
                    (= (hash-count a) 3)
                    (string? (hash-ref a 'attempt-id #f))
                    (positive? (string-length (hash-ref a 'attempt-id)))
                    (exact-positive-integer? (hash-ref a 'attempt-fence #f))
                    (exact-positive-integer? (hash-ref a 'coordinator-fence #f))))
             (equal? (hash-ref amendment 'model-dispatches) 1)
             (eq? (hash-ref amendment 'successor-permitted) #f))
      (error 'call-with-test-disposition-amendment "invalid immutable fixture amendment"))
    (when predecessor
      (call-with-test-disposition-amendment predecessor void)
      (unless (and (equal? (hash-ref amendment 'v2-proposal) (hash-ref predecessor 'parent-proposal))
                   (equal? (hash-ref amendment 'v2-approval) (hash-ref predecessor 'parent-approval)))
        (error 'call-with-test-disposition-amendment "fixture predecessor mismatch")))
    (parameterize ([fixture-amendment (cons root (seal-supplemental-record amendment))]
                   [fixture-predecessor predecessor])
      (thunk))))
