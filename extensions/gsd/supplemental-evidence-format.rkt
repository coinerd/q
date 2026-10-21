#lang racket/base
;; Pure wire descriptors ONLY. Digest integrity is never operator authentication.
;; Integration API:
;; - task/v1 has exactly task-fields below; context retains the legacy validator.
;; - other v1 kinds require kind/context/task-sha256/sha256 and may carry typed
;;   canonical JSON payload fields. Their semantic links MUST be checked by the
;;   owner/controller, not inferred here. No descriptor grants any capability.
;; - valid-supplemental-record? dispatches only explicit supplemental kinds;
;;   legacy records remain the responsibility of their existing validators.
(require racket/list
         racket/string
         "evidence-repair-format.rkt"
         "effective-verify-binding.rkt")
(provide supplemental-record-digest
         seal-supplemental-record
         valid-supplemental-task?
         valid-supplemental-record?)

(define supplemental-record-digest canonical-evidence-digest)
;; Copy strings before sealing so caller mutation cannot alter retained bytes.
;; Mutable hashes are refused, including nested context/policy objects.
(define (freeze value)
  (cond
    [(hash? value)
     (unless (immutable? value)
       (raise-argument-error 'seal-supplemental-record "immutable hash" value))
     (for/hasheq ([(key item) (in-hash value)])
       (values key (freeze item)))]
    [(list? value) (map freeze value)]
    [(string? value) (string->immutable-string value)]
    [else value]))
(define (seal-supplemental-record record)
  (define frozen (freeze record))
  (unless (and (hash? frozen) (json-value? frozen))
    (raise-argument-error 'seal-supplemental-record "immutable canonical JSON hash" record))
  (hash-set frozen 'sha256 (string->immutable-string (supplemental-record-digest frozen))))
(define (hex? v n)
  (and (string? v) (= (string-length v) n) (regexp-match? #px"^[0-9a-f]+$" v)))
(define (json-value? v)
  (cond
    [(hash? v)
     (and (immutable? v)
          (for/and ([(k value) (in-hash v)])
            (and (symbol? k) (json-value? value))))]
    [(list? v) (andmap json-value? v)]
    [else (or (and (string? v) (immutable? v)) (boolean? v) (eq? v 'null) (exact-integer? v))]))
(define (sealed? r)
  (and (hash? r)
       (immutable? r)
       (json-value? r)
       (hex? (hash-ref r 'sha256 #f) 64)
       (equal? (hash-ref r 'sha256) (supplemental-record-digest r))))
(define (exact-keys? h keys)
  (and (hash? h)
       (immutable? h)
       (= (hash-count h) (length keys))
       (andmap (lambda (k) (hash-has-key? h k)) keys)))
(define (relative-path? p)
  (and (string? p)
       (regexp-match? #px"^[A-Za-z0-9_][A-Za-z0-9_./-]*$" p)
       (for/and ([part (in-list (string-split p "/" #:trim? #f))])
         (and (not (member part '("" "." ".."))) (not (string-prefix? part "-"))))))
(define (racket-path? p prefix)
  (and (relative-path? p) (string-prefix? p prefix) (string-suffix? p ".rkt")))
(define task-fields
  '(kind context
         base-command
         effective-command
         test-path
         harness-path
         owner-paths
         baseline-head
         baseline-tree
         control-plane-head
         bounds
         sha256))
;; Aggregate policy ceiling, not an inferred execution timeout. Task approval
;; must pin its own positive deadline at or below one day; the owner must enforce
;; that deadline independently of these structural checks.
(define (valid-bounds? b)
  (and (exact-keys? b '(model-dispatches verify-timeout-sec probe-timeout-sec deadline-sec))
       (equal? (hash-ref b 'model-dispatches) 1)
       (for/and ([key '(verify-timeout-sec probe-timeout-sec deadline-sec)]
                 [ceiling '(14400 600 86400)])
         (define value (hash-ref b key))
         (and (exact-positive-integer? value) (<= value ceiling)))))
;; Recovery lineage is identity, never authority. Admission authenticates these
;; exact references against the permanent private claim and active pointer.
(define (valid-recovery? r)
  (and (exact-keys? r '(parent-proposal parent-approval terminal-sha256 pointer-sha256 allocation))
       (for/and ([key '(parent-proposal parent-approval)] [kind '("task-proposal" "task-approval")])
         (define ref (hash-ref r key))
         (and (exact-keys? ref '(kind sha256))
              (equal? (hash-ref ref 'kind) kind) (hex? (hash-ref ref 'sha256) 64)))
       (hex? (hash-ref r 'terminal-sha256) 64)
       (hex? (hash-ref r 'pointer-sha256) 64)
       (let ([a (hash-ref r 'allocation)])
         (and (exact-keys? a '(attempt-id attempt-fence coordinator-fence))
              (string? (hash-ref a 'attempt-id))
              (positive? (string-length (hash-ref a 'attempt-id)))
              (exact-nonnegative-integer? (hash-ref a 'attempt-fence))
              (exact-nonnegative-integer? (hash-ref a 'coordinator-fence))))))
(define (valid-supplemental-task? task)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and (if (equal? (hash-ref task 'kind #f) "supplemental-evidence-task/v2")
             (and (exact-keys? task (cons 'recovery task-fields))
                  (valid-recovery? (hash-ref task 'recovery)))
             (and (exact-keys? task task-fields)
                  (equal? (hash-ref task 'kind) "supplemental-evidence-task/v1")))
         (sealed? task)
         (valid-evidence-context? (hash-ref task 'context))
         (for/and ([key '(baseline-head baseline-tree control-plane-head)])
           (hex? (hash-ref task key) 40))
         (racket-path? (hash-ref task 'test-path) "tests/")
         (racket-path? (hash-ref task 'harness-path) "tests/helpers/")
         (not (equal? (hash-ref task 'test-path) (hash-ref task 'harness-path)))
         (let ([paths (hash-ref task 'owner-paths)])
           (and (list? paths)
                (andmap relative-path? paths)
                (= (length paths) (length (remove-duplicates paths)))
                (not (member (hash-ref task 'test-path) paths))
                (not (member (hash-ref task 'harness-path) paths))))
         (valid-bounds? (hash-ref task 'bounds))
         (effective-verify-superset? (hash-ref task 'base-command)
                                     (hash-ref task 'effective-command)
                                     (hash-ref task 'test-path)))))
(define descriptor-kinds
  '("supplemental-evidence-candidate/v1" "supplemental-evidence-grant/v1"
                                         "supplemental-evidence-approval/v1"
                                         "supplemental-evidence-supersession/v1"
                                         "supplemental-evidence-promotion/v1"))
(define (valid-supplemental-record? record)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and (sealed? record)
         (if (member (hash-ref record 'kind #f)
                     '("supplemental-evidence-task/v1" "supplemental-evidence-task/v2"))
             (valid-supplemental-task? record)
             (and (if (member (hash-ref record 'kind #f) descriptor-kinds) #t #f)
                  (valid-evidence-context? (hash-ref record 'context #f))
                  (hex? (hash-ref record 'task-sha256 #f) 64))))))
