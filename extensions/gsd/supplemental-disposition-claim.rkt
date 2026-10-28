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
         ;; Acyclic storage/validation core (BUG-0090): the immutable journal
         ;; primitives this claim needs are provided by a shared leaf helper,
         ;; never by the journal itself, so first use cannot re-enter a
         ;; partially instantiated module through a lazy back-edge.
         (only-in "supplemental-evidence-store-core.rkt"
                  authenticate-supplemental-task!
                  paths
                  validate-record!
                  inspect-supplemental-recovery-parent
                  read-recovery-claim!
                  read-pointer!
                  canonical-pointer-path
                  cut!
                  [claim-path recovery-claim-path]))
(provide inspect-supplemental-disposition-parent
         install-supplemental-disposition-claim!
         validate-disposition-claim!
         read-disposition-claim!
         disposition-late-terminal?)
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
;; A terminal shape is fixed per amendment generation and is never inferred
;; from an arbitrary blocked record. The exhausted ancestor generations keep
;; the original launched blocked/revision-3 launch-spent shape. The fixed
;; third amendment is the ONLY generation permitted the late
;; blocked/revision-6 verifying-prefix shape (the shape the expired attempt-6
;; operation genuinely sealed), and only with exactly its six typed facts.
;; This is not a generic late-terminal recovery: any other revision, fact set,
;; generation or value refuses, so ancestors can never admit a late terminal.
(define (nonempty-string? v)
  (and (string? v) (positive? (string-length v))))
(define (reference-kind? v kind)
  (and (supplemental-reference? v) (equal? (hash-ref v 'kind) kind)))
(define (allocation-shape? a)
  (and (exact-keys? a '(attempt-id attempt-fence coordinator-fence))
       (nonempty-string? (hash-ref a 'attempt-id #f))
       (exact-nonnegative-integer? (hash-ref a 'attempt-fence #f))
       (exact-nonnegative-integer? (hash-ref a 'coordinator-fence #f))))
(define (launched-terminal? r)
  (define facts (hash-ref r 'facts))
  (and (equal? (hash-ref r 'state) "blocked")
       (= (hash-ref r 'revision) 3)
       (hash-ref r 'launch-consumed)
       (exact-keys? facts '(allocation dispatch-id block-reason))
       (allocation-shape? (hash-ref facts 'allocation))
       (nonempty-string? (hash-ref facts 'dispatch-id))
       (nonempty-string? (hash-ref facts 'block-reason))))
(define (late-terminal? r)
  (define facts (hash-ref r 'facts))
  (and (equal? (hash-ref r 'state) "blocked")
       (= (hash-ref r 'revision) 6)
       (hash-ref r 'launch-consumed)
       (exact-keys?
        facts
        '(allocation dispatch-id candidate-ref candidate-approval-ref verify-job block-reason))
       (allocation-shape? (hash-ref facts 'allocation))
       (nonempty-string? (hash-ref facts 'dispatch-id))
       (reference-kind? (hash-ref facts 'candidate-ref) "candidate-proposal")
       (reference-kind? (hash-ref facts 'candidate-approval-ref) "candidate-approval")
       (let ([job (hash-ref facts 'verify-job)]) (and (hash? job) (positive? (hash-count job))))
       (nonempty-string? (hash-ref facts 'block-reason))))
;; The amendment's own `prior-parent-*` keys identify the fixed third
;; generation (production has exactly one: disposition-amendment-3); the
;; isolated test seam can only present the same fixed shape.
(define (third-generation? amendment)
  (hash-has-key? amendment 'prior-parent-proposal))
(define (terminal-shape? amendment record)
  (if (third-generation? amendment)
      (late-terminal? record)
      (launched-terminal? record)))
(define (disposition-late-terminal? amendment record)
  (and (third-generation? amendment) (late-terminal? record)))
(define (inspect-supplemental-disposition-parent grant)
  (inspect-parent grant (disposition-amendment) (disposition-ancestors)))
(define (inspect-parent grant a ancestors)
  ;; The active amendment binds its own nearest predecessor; each earlier
  ;; generation is inspected recursively through the remaining immutable
  ;; ancestor chain, so every frame re-checks its own parent generation,
  ;; spent terminal and permanent claim. No generic chain construction: the
  ;; chain is exactly the fixed amendment list from the policy module.
  (define predecessor (and (pair? ancestors) (car ancestors)))
  (define rest
    (if (pair? ancestors)
        (cdr ancestors)
        '()))
  (define prior-keys
    (if (hash-has-key? a 'prior-parent-proposal)
        '(prior-parent-proposal prior-parent-approval)
        '(v2-proposal v2-approval)))
  (call-with-supplemental-authority-lock
   (lambda ()
     (define-values (proposal task expiry) (authenticate-supplemental-task! grant #t))
     (unless (and (equal? grant (hash-ref a 'parent-approval))
                  (equal? proposal (hash-ref a 'parent-proposal))
                  (equal?
                   (hash-ref task 'kind)
                   (if predecessor "supplemental-evidence-task/v3" "supplemental-evidence-task/v2")))
       (refuse "amendment requires its exact parent generation"))
     (when predecessor
       (define-values (pp pt po)
         (inspect-parent (hash-ref predecessor 'parent-approval) predecessor rest))
       (define previous (read-disposition-claim! (claim-path (hash-ref predecessor 'sha256))))
       (unless (and (equal? (hash-ref a (car prior-keys)) pp)
                    (equal? (hash-ref a (cadr prior-keys)) (hash-ref predecessor 'parent-approval))
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
     (unless (and (terminal-shape? a record)
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
               (exact-keys? (hash-remove (hash-remove (hash-remove (hash-remove a 'v2-proposal)
                                                                   'v2-approval)
                                                      'prior-parent-proposal)
                                         'prior-parent-approval)
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
               (or (and (not (hash-has-key? a 'prior-parent-proposal))
                        (not (hash-has-key? a 'prior-parent-approval)))
                   (and (ref? (hash-ref a 'prior-parent-proposal #f) "task-proposal")
                        (ref? (hash-ref a 'prior-parent-approval #f) "task-approval")))
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
     (define-values (p task expiry) (authenticate-supplemental-task! grant #t))
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
