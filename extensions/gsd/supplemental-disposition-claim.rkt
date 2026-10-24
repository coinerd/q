#lang racket/base
;; Known-source private helper. No caller-supplied authentication procedure.
(require racket/file
         racket/path
         "supplemental-evidence-store.rkt"
         "supplemental-evidence-format.rkt"
         "supplemental-evidence-authority.rkt"
         "supplemental-disposition-policy.rkt"
         (submod "supplemental-disposition-policy.rkt" ancestry-support)
         "../../util/json/checksum.rkt"
         (only-in "evidence-repair-format.rkt" canonical-evidence-json)
         (submod "supplemental-evidence-journal.rkt" disposition-store-support))
(provide inspect-supplemental-disposition-parent
         install-supplemental-disposition-claim!
         validate-disposition-claim!
         read-disposition-claim!)
(define (refuse message)
  (error 'supplemental-disposition-claim "~a" message))
(define (claim-path id)
  (build-path (root!) (string-append "supplemental-disposition-" id ".json")))
(define (pointer p g)
  (hasheq 'schema "supplemental-pointer/v1" 'task-proposal p 'task-approval g))
(define (inherited? parent task)
  (for/and ([(key value) (in-hash parent)]
            #:unless (memq key '(kind sha256 control-plane-head disposition)))
    (equal? value (hash-ref task key #f))))
(define (spent? r)
  (define facts (hash-ref r 'facts))
  (and (equal? (hash-ref r 'state) "blocked")
       (= (hash-ref r 'revision) 3)
       (hash-ref r 'launch-consumed)
       (exact-keys? facts '(allocation dispatch-id block-reason))
       (for/and ([key '(dispatch-id block-reason)])
         (define v (hash-ref facts key))
         (and (string? v) (positive? (string-length v))))))
(define (inspect-supplemental-disposition-parent grant)
  (inspect-parent grant (disposition-amendment) (disposition-predecessor)))
(define (inspect-parent grant a predecessor)
  (call-with-supplemental-authority-lock
   (lambda ()
     (define-values (proposal task expiry) (authenticated-task! grant #t))
     (unless (and (equal? grant (hash-ref a 'parent-approval))
                  (equal? proposal (hash-ref a 'parent-proposal))
                  (equal?
                   (hash-ref task 'kind)
                   (if predecessor "supplemental-evidence-task/v3" "supplemental-evidence-task/v2")))
       (refuse "amendment requires its exact parent generation"))
     (when predecessor
       (define-values (pp pt po)
         (inspect-parent (hash-ref predecessor 'parent-approval) predecessor #f))
       (define previous (read-disposition-claim! (claim-path (hash-ref predecessor 'sha256))))
       (unless (and (equal? (hash-ref a 'v2-proposal) pp)
                    (equal? (hash-ref a 'v2-approval) (hash-ref predecessor 'parent-approval))
                    (equal? (hash-ref previous 'amendment) predecessor)
                    (equal? (hash-ref previous 'target) (pointer proposal grant))
                    (equal? (hash-ref previous 'lineage) (hash-ref task 'disposition))
                    (equal? (hash-ref task 'disposition)
                            (descriptor predecessor
                                        (pointer pp (hash-ref predecessor 'parent-approval))))
                    (inherited? pt task))
         (refuse "historical disposition claim or scope mismatch")))
     (define r (hash-ref task 'recovery))
     (unless (and (equal? (hash-ref r 'parent-proposal) (hash-ref a 'root-proposal))
                  (equal? (hash-ref r 'parent-approval) (hash-ref a 'root-approval)))
       (refuse "amendment root mismatch"))
     (define old-claim (read-recovery-claim! (recovery-claim-path (hash-ref a 'root-approval))))
     (unless (and (equal? (hash-ref old-claim 'lineage) r)
                  (equal? (hash-ref old-claim 'target)
                          (if predecessor
                              (pointer (hash-ref a 'v2-proposal) (hash-ref a 'v2-approval))
                              (pointer proposal grant)))
                  (or (not predecessor)
                      (equal? (hash-ref old-claim 'pointer-path)
                              (hash-ref (read-disposition-claim! (claim-path (hash-ref predecessor
                                                                                       'sha256)))
                                        'pointer-path))))
       (refuse "historical private recovery claim mismatch"))
     ;; This v1 inspector authenticates revocation, but ignores historical expiry.
     (define-values (rp root rt) (inspect-supplemental-recovery-parent (hash-ref a 'root-approval)))
     (unless (and (equal? rp (hash-ref a 'root-proposal))
                  (equal? (hash-ref rt 'sha256) (hash-ref r 'terminal-sha256))
                  (equal? (hash-ref (hash-ref rt 'facts) 'allocation) (hash-ref r 'allocation))
                  (inherited? root task)
                  (for/and ([key '(plan-id wave snapshot-sha256)])
                    (equal? (hash-ref (hash-ref task 'context) key) (hash-ref a key))))
       (refuse "immutable root ancestry mismatch"))
     (define-values (_dir snapshot intent _lock) (paths (root!) grant #f))
     (when (present? intent)
       (refuse "disposition parent has pending intent"))
     (define record (validate-record! (read-sealed snapshot) grant proposal task expiry))
     (unless (and (spent? record)
                  (equal? (hash-ref record 'sha256) (hash-ref a 'terminal-body-sha256))
                  (equal? (sha256-string (canonical-evidence-json record))
                          (hash-ref a 'terminal-record-sha256))
                  (equal? (hash-ref (hash-ref record 'facts) 'allocation) (hash-ref a 'allocation)))
       (refuse "exact spent terminal amendment facts required"))
     (values proposal task record))))
(define (descriptor a old)
  (hasheq 'amendment-id
          (hash-ref a 'sha256)
          'root-proposal
          (hash-ref a 'root-proposal)
          'root-approval
          (hash-ref a 'root-approval)
          'parent-proposal
          (hash-ref a 'parent-proposal)
          'parent-approval
          (hash-ref a 'parent-approval)
          'terminal-body-sha256
          (hash-ref a 'terminal-body-sha256)
          'terminal-record-sha256
          (hash-ref a 'terminal-record-sha256)
          'pointer-sha256
          (supplemental-record-digest old)
          'allocation
          (hash-ref a 'allocation)))
(define (check-lineage! task)
  (define a (disposition-amendment))
  (define-values (p parent terminal)
    (inspect-supplemental-disposition-parent (hash-ref a 'parent-approval)))
  (define old (pointer p (hash-ref a 'parent-approval)))
  (unless (and (equal? (hash-ref task 'kind) "supplemental-evidence-task/v3")
               (equal? (hash-ref task 'disposition) (descriptor a old))
               (inherited? parent task))
    (refuse "disposition changed fixed amendment or inherited scope"))
  (define old-claim (read-recovery-claim! (recovery-claim-path (hash-ref a 'root-approval))))
  (values a old (hash-ref old-claim 'pointer-path)))
(define (digest? v)
  (and (string? v) (regexp-match? #px"^[0-9a-f]{64}$" v)))
(define (ref? r kind)
  (and (supplemental-reference? r) (equal? (hash-ref r 'kind) kind)))
(define (read-disposition-claim! path)
  (define c (read-sealed path))
  (define a (hash-ref c 'amendment #f))
  (define d (hash-ref c 'lineage #f))
  (define t (hash-ref c 'target #f))
  (define bound (hash-ref c 'pointer-path #f))
  (unless (and (exact-keys? c '(schema amendment lineage pointer-path target sha256))
               (equal? (hash-ref c 'schema) "supplemental-disposition-claim/v1")
               (exact-keys? (hash-remove (hash-remove a 'v2-proposal) 'v2-approval)
                            '(kind plan-id
                                   wave
                                   root-proposal
                                   root-approval
                                   parent-proposal
                                   parent-approval
                                   terminal-body-sha256
                                   terminal-record-sha256
                                   allocation
                                   snapshot-sha256
                                   model-dispatches
                                   successor-permitted
                                   sha256))
               (or (and (not (hash-has-key? a 'v2-proposal)) (not (hash-has-key? a 'v2-approval)))
                   (and (ref? (hash-ref a 'v2-proposal #f) "task-proposal")
                        (ref? (hash-ref a 'v2-approval #f) "task-approval")))
               (equal? a (seal-supplemental-record a))
               (equal? (hash-ref a 'kind) "supplemental-disposition-amendment/v1")
               (digest? (hash-ref a 'plan-id))
               (exact-nonnegative-integer? (hash-ref a 'wave))
               (digest? (hash-ref a 'snapshot-sha256))
               (equal? (hash-ref a 'model-dispatches) 1)
               (eq? (hash-ref a 'successor-permitted) #f)
               (exact-keys? d
                            '(amendment-id root-proposal
                                           root-approval
                                           parent-proposal
                                           parent-approval
                                           terminal-body-sha256
                                           terminal-record-sha256
                                           pointer-sha256
                                           allocation))
               (equal?
                d
                (descriptor a (pointer (hash-ref a 'parent-proposal) (hash-ref a 'parent-approval))))
               (for/and ([key '(root-proposal root-approval parent-proposal parent-approval)]
                         [kind '("task-proposal" "task-approval" "task-proposal" "task-approval")])
                 (ref? (hash-ref a key) kind))
               (for/and ([key '(terminal-body-sha256 terminal-record-sha256)])
                 (digest? (hash-ref a key)))
               (let ([allocation (hash-ref a 'allocation)])
                 (and (exact-keys? allocation '(attempt-id attempt-fence coordinator-fence))
                      (string? (hash-ref allocation 'attempt-id))
                      (positive? (string-length (hash-ref allocation 'attempt-id)))
                      (exact-positive-integer? (hash-ref allocation 'attempt-fence))
                      (exact-positive-integer? (hash-ref allocation 'coordinator-fence))))
               (string? bound)
               (complete-path? bound)
               (equal? bound (canonical-pointer-path bound))
               (exact-keys? t '(schema task-proposal task-approval))
               (equal? (hash-ref t 'schema) "supplemental-pointer/v1")
               (ref? (hash-ref t 'task-proposal) "task-proposal")
               (ref? (hash-ref t 'task-approval) "task-approval")
               (equal? (file-name-from-path path)
                       (string->path
                        (string-append "supplemental-disposition-" (hash-ref a 'sha256) ".json"))))
    (refuse "invalid permanent disposition claim"))
  c)
(define (validate-disposition-claim! grant proposal task)
  (define-values (a old bound) (check-lineage! task))
  (define claim (read-disposition-claim! (claim-path (hash-ref a 'sha256))))
  (unless (and (equal? (hash-ref claim 'amendment) a)
               (equal? (hash-ref claim 'lineage) (hash-ref task 'disposition))
               (equal? (hash-ref claim 'pointer-path) bound)
               (equal? (hash-ref claim 'target) (pointer proposal grant))
               (equal? (read-pointer! bound) (pointer proposal grant)))
    (refuse "disposition requires exact active pointer and permanent claim")))
(define (install-supplemental-disposition-claim! path old target proposal grant)
  (call-with-supplemental-authority-lock
   (lambda ()
     (define-values (p task expiry) (authenticated-task! grant #t))
     (unless (and (equal? p proposal)
                  (< (current-seconds) expiry)
                  (<= expiry (+ (current-seconds) (hash-ref (hash-ref task 'bounds) 'deadline-sec))))
       (refuse "fresh exact bounded disposition approval required"))
     (define-values (a expected bound) (check-lineage! task))
     (unless (and (equal? old expected)
                  (equal? bound (canonical-pointer-path path))
                  (equal? target (pointer proposal grant)))
       (refuse "disposition predecessor/path/target mismatch"))
     (define file (claim-path (hash-ref a 'sha256)))
     (define claim
       (seal (hasheq 'schema
                     "supplemental-disposition-claim/v1"
                     'amendment
                     a
                     'lineage
                     (hash-ref task 'disposition)
                     'pointer-path
                     bound
                     'target
                     target)))
     (define existed? (present? file))
     (when (and existed? (not (equal? (read-disposition-claim! file) claim)))
       (refuse "amendment already permanently claimed"))
     (define current (read-pointer! path))
     (unless (or (equal? current old) (and existed? (equal? current target)))
       (refuse "disposition active pointer CAS mismatch"))
     (unless existed?
       (atomic-write! file claim))
     (unless (equal? claim (read-disposition-claim! file))
       (refuse "claim readback mismatch"))
     (cut! 'disposition-claim-written)
     (unless (equal? current target)
       (atomic-write! path target))
     (cut! 'disposition-pointer-written)
     (validate-disposition-claim! grant proposal task)
     target)))
