#lang racket
;; @speed fast
;; @suite extensions
;; @boundary unit
;; @timeout 600
;; Real Git/authority fixtures take ~81s warm and exceeded 120s under the fast gate's cold parallel load.
(require rackunit
         racket/file
         racket/runtime-path
         (only-in "../runtime/approval/broker.rkt" approval-grant?)
         (only-in "test-gsd-supplemental-evidence-journal.rkt"
                  task
                  with-fixture
                  fixture-grant
                  fixture-root
                  fixture-workspace
                  base-binding
                  approve!
                  revoke-approval!)
         "../extensions/gsd/supplemental-evidence-format.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         "../extensions/gsd/supplemental-disposition-policy.rkt"
         (only-in "../extensions/gsd/supplemental-disposition-claim.rkt" disposition-late-terminal?)
         (submod "../extensions/gsd/supplemental-disposition-policy.rkt" test-support)
         (submod "../extensions/gsd/supplemental-disposition-policy.rkt" ancestry-support)
         "../extensions/gsd/evidence-repair-format.rkt"
         "../util/json/checksum.rkt")
(define (ref kind digit)
  (hasheq 'kind kind 'sha256 (make-string 64 digit)))
(define allocation (hasheq 'attempt-id "attempt-4" 'attempt-fence 7 'coordinator-fence 7))
(define recovery
  (hasheq 'parent-proposal
          (ref "task-proposal" #\a)
          'parent-approval
          (ref "task-approval" #\b)
          'terminal-sha256
          (make-string 64 #\c)
          'pointer-sha256
          (make-string 64 #\d)
          'allocation
          allocation))
(define disposition
  (hasheq 'amendment-id
          (make-string 64 #\e)
          'root-proposal
          (ref "task-proposal" #\a)
          'root-approval
          (ref "task-approval" #\b)
          'parent-proposal
          (ref "task-proposal" #\c)
          'parent-approval
          (ref "task-approval" #\d)
          'terminal-body-sha256
          (make-string 64 #\a)
          'terminal-record-sha256
          (make-string 64 #\b)
          'pointer-sha256
          (make-string 64 #\c)
          'allocation
          allocation))
(define v3
  (seal-supplemental-record (hash-set* task
                                       'kind
                                       "supplemental-evidence-task/v3"
                                       'recovery
                                       recovery
                                       'disposition
                                       disposition)))
(define (pointer p g)
  (hasheq 'schema "supplemental-pointer/v1" 'task-proposal p 'task-approval g))
(define (put path value)
  (display-to-file (canonical-evidence-json value) path #:exists 'truncate)
  (file-or-directory-permissions path #o600))
(define (spend! grant [owned allocation])
  (start-supplemental-operation! grant base-binding)
  (transition-supplemental-operation! grant
                                      0
                                      "proposal-approved"
                                      "reserved"
                                      (hasheq 'allocation owned))
  (transition-supplemental-operation! grant
                                      1
                                      "reserved"
                                      "launched"
                                      (hasheq 'dispatch-id "failed-provider"))
  (transition-supplemental-operation! grant 2 "launched" "blocked" (hasheq 'block-reason "spent")))
;; Independent storage fixture: no controller/worker/source writes.
(define (with-spent proc #:short-parent? [short? #f])
  (with-fixture
   (lambda (f)
     (define root-grant (fixture-grant f))
     (spend! root-grant)
     (define-values (rp rt ro) (inspect-supplemental-recovery-parent root-grant))
     (define old (pointer rp root-grant))
     (define path (build-path (fixture-workspace f) "pointer.json"))
     (put path old)
     (define r
       (hasheq 'parent-proposal
               rp
               'parent-approval
               root-grant
               'terminal-sha256
               (hash-ref ro 'sha256)
               'pointer-sha256
               (supplemental-record-digest old)
               'allocation
               allocation))
     (define parent
       (seal-supplemental-record (hash-set* rt 'kind "supplemental-evidence-task/v2" 'recovery r)))
     (define pp
       (propose-retain! "task-proposal"
                        (hasheq 'task parent 'expires-at (+ (current-seconds) (if short? 2 1700)))))
     (define pg (approve! pp))
     (define parent-pointer (pointer pp pg))
     (install-supplemental-recovery-claim! path old parent-pointer pp pg)
     (define po (spend! pg))
     (define a
       (seal-supplemental-record (hash-set* disposition-amendment-1
                                            'plan-id
                                            (hash-ref (hash-ref rt 'context) 'plan-id)
                                            'wave
                                            1
                                            'root-proposal
                                            rp
                                            'root-approval
                                            root-grant
                                            'parent-proposal
                                            pp
                                            'parent-approval
                                            pg
                                            'terminal-body-sha256
                                            (hash-ref po 'sha256)
                                            'terminal-record-sha256
                                            (sha256-string (canonical-evidence-json po))
                                            'snapshot-sha256
                                            (hash-ref (hash-ref rt 'context) 'snapshot-sha256)
                                            'allocation
                                            allocation)))
     (call-with-test-disposition-amendment
      a
      (lambda ()
        (define-values (p g t target) (child! parent parent-pointer a (if short? 2 1700)))
        (install-supplemental-disposition-claim! path parent-pointer target p g)
        (define owned (hasheq 'attempt-id "attempt-5" 'attempt-fence 8 'coordinator-fence 8))
        (define terminal (spend! g owned))
        (define second
          (seal-supplemental-record (hash-set* a
                                               'v2-proposal
                                               pp
                                               'v2-approval
                                               pg
                                               'parent-proposal
                                               p
                                               'parent-approval
                                               g
                                               'terminal-body-sha256
                                               (hash-ref terminal 'sha256)
                                               'terminal-record-sha256
                                               (sha256-string (canonical-evidence-json terminal))
                                               'allocation
                                               owned)))
        (call-with-test-disposition-amendment
         second
         (lambda ()
           ;; The old amendment-1 proposal (production f4238535) cannot
           ;; install through amendment-2. Use its genuinely approved fixture
           ;; analogue, not fabricated production records or live authority.
           ;; Both active-policy mismatch and the predecessor's permanent claim
           ;; independently refuse the old amendment proposal.
           (check-exn exn:fail?
                      (lambda ()
                        (install-supplemental-disposition-claim! path parent-pointer target p g)))
           (call-with-test-disposition-amendment
            a
            (lambda ()
              (define-values (cp cg ct competitor) (child! parent parent-pointer a 1690))
              (check-exn
               #rx"already permanently claimed"
               (lambda ()
                 (install-supplemental-disposition-claim! path parent-pointer competitor cp cg)))))
           (proc f path target t terminal second))
         #:predecessor a))))))
(define (child! parent old a [offset 1600])
  (define d
    (hash-set* disposition
               'amendment-id
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
  (define t
    (seal-supplemental-record
     (hash-set* parent 'kind "supplemental-evidence-task/v3" 'disposition d)))
  (define p
    (propose-retain! "task-proposal" (hasheq 'task t 'expires-at (+ (current-seconds) offset))))
  (define g (approve! p))
  (values p g t (pointer p g)))
(define (spend-late! grant owned)
  ;; The fixed third-generation parent terminal: the same production journal
  ;; transitions the live attempt-6 operation traversed, through the real
  ;; candidate proposal/approval records, ending in the blocked/revision-6
  ;; verifying-prefix shape. Never a hand-built or rewritten record.
  (define (step! from to facts)
    (define op (load-supplemental-operation grant))
    (transition-supplemental-operation! grant (hash-ref op 'revision) from to facts))
  (start-supplemental-operation! grant base-binding)
  (step! "proposal-approved" "reserved" (hasheq 'allocation owned))
  (step! "reserved" "launched" (hasheq 'dispatch-id "supplemental-author-attempt-6"))
  (define retained (load-supplemental-operation grant))
  (define candidate-ref
    (propose-retain! "candidate-proposal"
                     (hasheq 'task (hash-ref retained 'proposal-ref))
                     (list (hash-ref retained 'proposal-ref))))
  (step! "launched" "candidate-sealed" (hasheq 'candidate-ref candidate-ref))
  (step! "candidate-sealed"
         "candidate-approved"
         (hasheq 'candidate-approval-ref (approve! candidate-ref)))
  (step! "candidate-approved"
         "verifying"
         (hasheq 'verify-job (hasheq 'id "fixture-verify-job" 'owner "fixture-controller")))
  (step! "verifying" "blocked" (hasheq 'block-reason "operator deadline expired"))
  (load-supplemental-operation grant))
;; Third bounded generation: one more genuinely spent child derived from
;; sealed records only. The amendment-2 permanent claim installs inside its
;; own frame (moving the pointer), then the immutable amendment-3 fixture
;; binds its exact pair, both terminal digest namespaces and failed attempt
;; allocation. By default that terminal is the genuine late
;; blocked/revision-6 shape; `#:late? #f` keeps the exhausted revision-3
;; shape as a negative (wrong-revision) control. No fourth successor is ever
;; bound.
(define (with-spent3 proc #:late? [late? #t])
  (with-spent
   (lambda (f path old parent po a)
     (define-values (p2 g2 t2 target2) (child! parent old a))
     ;; The second amendment's claim/pointer install stays in its own frame
     ;; and MUST precede spending the new child: v3 operation admission
     ;; validates the permanent claim and active pointer.
     (install-supplemental-disposition-claim! path old target2 p2 g2)
     (define owned3 (hasheq 'attempt-id "attempt-6" 'attempt-fence 9 'coordinator-fence 9))
     (define terminal3
       (if late?
           (spend-late! g2 owned3)
           (spend! g2 owned3)))
     (define third
       (seal-supplemental-record (hash-set* a
                                            'prior-parent-proposal
                                            (hash-ref a 'parent-proposal)
                                            'prior-parent-approval
                                            (hash-ref a 'parent-approval)
                                            'parent-proposal
                                            p2
                                            'parent-approval
                                            g2
                                            'terminal-body-sha256
                                            (hash-ref terminal3 'sha256)
                                            'terminal-record-sha256
                                            (sha256-string (canonical-evidence-json terminal3))
                                            'allocation
                                            owned3)))
     (call-with-test-disposition-amendment third
                                           (lambda () (proc f path target2 t2 terminal3 third p2 g2))
                                           #:predecessor a))))
(define (claim-refused-with? path old target p g)
  (check-exn #rx"exact spent terminal amendment facts required"
             (lambda () (install-supplemental-disposition-claim! path old target p g))))
(define (call-with-fixture-chain first second amendment thunk)
  (call-with-test-disposition-amendment
   first
   (lambda ()
     (call-with-test-disposition-amendment
      second
      (lambda () (call-with-test-disposition-amendment amendment thunk #:predecessor second))
      #:predecessor first))))
(define-runtime-path self "test-gsd-supplemental-disposition-store.rkt")
(define (worker root a path old target p g)
  (define-values (process out in err)
    (subprocess #f #f #f (find-executable-path "racket") self "--claim" (path->string root)))
  (write (list a (disposition-predecessor) (path->string path) old target p g) in)
  (newline in)
  (flush-output in)
  (unless (equal? (read-line out) "ready")
    (error 'worker "not ready"))
  (list process out in err))
(module+ main
  (define args (current-command-line-arguments))
  (when (and (= (vector-length args) 2) (equal? (vector-ref args 0) "--claim"))
    (define (freeze v)
      (cond
        [(string? v) (string->immutable-string v)]
        [(list? v) (map freeze v)]
        [(hash? v)
         (for/hasheq ([(k x) (in-hash v)])
           (values k (freeze x)))]
        [else v]))
    (define request (freeze (read)))
    (displayln "ready")
    (flush-output)
    (read-line)
    (read-line)
    (with-handlers ([exn:fail? (lambda (e)
                                 (displayln (exn-message e))
                                 (exit 3))])
      (parameterize ([current-supplemental-authority-root (string->path (vector-ref args 1))])
        (call-with-test-disposition-amendment
         (car request)
         (lambda () (apply install-supplemental-disposition-claim! (cddr request)))
         #:predecessor (cadr request)))
      (exit 0))))
(module+ test
  (test-case "explicit v3 wire descriptor is structurally supported (identity only)"
    (define accepted? (valid-supplemental-task? v3))
    (check-true accepted?)
    (check-true (valid-supplemental-record? v3))
    (for ([key (hash-keys disposition)])
      (check-false (valid-supplemental-task?
                    (seal-supplemental-record
                     (hash-set v3 'disposition (hash-remove disposition key))))))
    (check-false (valid-supplemental-task?
                  (seal-supplemental-record
                   (hash-set v3 'disposition (hash-set disposition 'extra #t))))))
  (test-case "active production amendment-3 ID is derived from its public binding fields"
    (check-equal? (hash-ref (disposition-amendment) 'sha256)
                  (hash-ref disposition-amendment-3 'sha256))
    ;; Drop the published ID and use the policy's own canonical sealing path.
    (define derived (seal-supplemental-record (hash-remove (disposition-amendment) 'sha256)))
    (check-equal? (hash-ref derived 'sha256) (hash-ref disposition-amendment-3 'sha256))
    ;; Exhausted ancestors keep their published identities and remain reachable
    ;; only as immutable provenance through the fixed ancestor chain.
    (check-equal? (hash-ref disposition-amendment-1 'sha256)
                  "e608629026bfd787c7c34a7dc163ebdc15f8c7ba82fff46277ba428037f82313")
    (check-equal? (hash-ref disposition-amendment-2 'sha256)
                  "645a6a88de529fa239edbda468b8c12e64df1c0c1fbff859db4a9f284e242505")
    (check-not-equal? (hash-ref disposition-amendment-2 'sha256)
                      (hash-ref disposition-amendment-3 'sha256))
    (check-equal? (disposition-predecessor) disposition-amendment-2)
    (check-equal? (disposition-ancestors) (list disposition-amendment-2 disposition-amendment-1)))
  (test-case "production policy is frozen; real/default root cannot use fixture seam"
    (define a (disposition-amendment))
    (check-equal? (hash-ref a 'plan-id)
                  "79a69b427b40fb6c6b68b4fd1484a74551db182afa9b96f065353324af16b19e")
    (check-equal? (hash-ref a 'wave) 1)
    ;; The expired attempt-6 operation was genuinely sealed blocked/revision 6
    ;; by the supported terminal writer; the fixed amendment-3 binds that exact
    ;; sealed record in both digest namespaces (the record's own sha256 field
    ;; and sha256 over its complete canonical JSON bytes).
    (check-equal? (hash-ref a 'terminal-body-sha256)
                  "758d653547879fe5204785af73acf1f7850810ad61e72bc9664532fd3d7068b1")
    (check-equal? (hash-ref a 'terminal-record-sha256)
                  "837710077fec71c156ac65ee86075d2c1d847270617056cbf34a1a7e1a7160c4")
    (check-not-equal? (hash-ref a 'terminal-body-sha256) (hash-ref a 'terminal-record-sha256))
    ;; The exhausted amendment-2 predecessor keeps its real terminal history.
    (check-equal? (hash-ref disposition-amendment-2 'terminal-body-sha256)
                  "b60c5f76528c76ff66ce98943633c93fed570e80b60a99192150d178c86d8259")
    (check-equal? (hash-ref disposition-amendment-2 'terminal-record-sha256)
                  "0c9e45d27cfb3934c29dc9bdc44686b62a6011ddf21c08d8c507beaf88e6e5c0")
    (check-equal? (hash-ref a 'snapshot-sha256)
                  "5d10ae297263d52f60e2305365b299f6060c232c9459168fc8b1fdba32071077")
    (check-equal? (hash-ref a 'root-proposal)
                  (hasheq 'kind
                          "task-proposal"
                          'sha256
                          "30be1b8ecd87ee21246d19473a338a571f3e0ef13dea7fe214824163280ae31c"))
    (check-equal? (hash-ref a 'root-approval)
                  (hasheq 'kind
                          "task-approval"
                          'sha256
                          "87838ebf39cea992c9064590df7c35078f2d6ff64c52c37ddb9c62bef4cc5b78"))
    ;; The active parent is the exact amendment-2 disposition child, whose
    ;; launch permission attempt-6 consumed.
    (check-equal? (hash-ref a 'parent-proposal)
                  (hasheq 'kind
                          "task-proposal"
                          'sha256
                          "15e68c8b1ddd822a969b18ebfb7e22c425a0f74e0a68ec48a2b712d1cd05e49d"))
    (check-equal? (hash-ref a 'parent-approval)
                  (hasheq 'kind
                          "task-approval"
                          'sha256
                          "ccf20bc98f7688b4bb5fa69226c54a876038c88fd28a8fe476dcad9ae7e1f9ac"))
    ;; The spent amendment-2 parent pair is pinned as immutable prior ancestry
    ;; and the inherited v2 link still names amendment-1's parent pair.
    (check-equal? (hash-ref a 'prior-parent-proposal)
                  (hasheq 'kind
                          "task-proposal"
                          'sha256
                          "f4238535e291e23062926f698c922da75fd3caadcd2de160e8b3ca68b33ba764"))
    (check-equal? (hash-ref a 'prior-parent-approval)
                  (hasheq 'kind
                          "task-approval"
                          'sha256
                          "f8ea96b875e8b05fc2f5800674270c6dddf93b4e256fa6b8fc1ed650fcd5b816"))
    (check-equal? (hash-ref a 'prior-parent-proposal)
                  (hash-ref disposition-amendment-2 'parent-proposal))
    (check-equal? (hash-ref a 'prior-parent-approval)
                  (hash-ref disposition-amendment-2 'parent-approval))
    (check-equal? a disposition-amendment-3)
    (check-equal? (hash-ref disposition-amendment-1 'parent-proposal) (hash-ref a 'v2-proposal))
    (check-equal? (hash-ref disposition-amendment-1 'parent-approval) (hash-ref a 'v2-approval))
    (check-not-equal? (hash-ref a 'prior-parent-proposal) (hash-ref a 'parent-proposal))
    (check-equal? (hash-ref a 'allocation)
                  (hasheq 'attempt-id "attempt-6" 'attempt-fence 9 'coordinator-fence 9))
    (check-equal? (hash-ref a 'model-dispatches) 1)
    (check-false (hash-ref a 'successor-permitted))
    (check-true (immutable? a))
    (check-true (immutable? (hash-ref a 'allocation)))
    (for ([key '(root-proposal root-approval
                               prior-parent-proposal
                               prior-parent-approval
                               parent-proposal
                               parent-approval)]
          [kind '("task-proposal" "task-approval"
                                  "task-proposal"
                                  "task-approval"
                                  "task-proposal"
                                  "task-approval")])
      (check-true (immutable? (hash-ref a key)))
      (check-true (supplemental-reference? (hash-ref a key)))
      (check-equal? (hash-ref (hash-ref a key) 'kind) kind))
    (check-equal? (hash-ref a 'kind) "supplemental-disposition-amendment/v1")
    (check-equal? a (seal-supplemental-record a))
    (check-false (approval-grant? a))
    (check-exn exn:fail? (lambda () (call-with-test-disposition-amendment a void))))
  (test-case "configured default and other home roots refuse before filesystem access"
    (define home (find-system-path 'home-dir))
    (define a (disposition-amendment))
    ;; No existence checks, directory traversal, record loads, or writes at home.
    ;; The guard also catches accidental metadata/content probes by the seam.
    (define guard
      (make-security-guard
       (current-security-guard)
       (lambda (who path modes)
         ;; The seam asks for the system temporary-directory pathname only.
         (unless (eq? who 'find-system-path)
           (error 'private-filesystem-probe "unexpected filesystem access: ~a" who)))
       (lambda args (error 'private-network-probe "unexpected network access"))))
    (for ([root (list (build-path home ".q" "agent" "supplemental-evidence")
                      (build-path home "disposition-test-forbidden-root"))])
      (parameterize ([current-supplemental-authority-root root]
                     [current-security-guard guard])
        (check-exn #rx"refusing real/default or nonisolated root"
                   (lambda () (call-with-test-disposition-amendment a void))))))
  (test-case "symlink temporary authority roots cannot bind a fixture amendment"
    (with-fixture (lambda (f)
                    (define alias (build-path (fixture-workspace f) "authority-link"))
                    (make-file-or-directory-link (fixture-root f) alias)
                    (parameterize ([current-supplemental-authority-root alias])
                      (check-exn #rx"refusing real/default or nonisolated root|symlink root component"
                                 (lambda ()
                                   (call-with-test-disposition-amendment (disposition-amendment)
                                                                         void)))))))
  (test-case "fixture getter rejects a changed root and restores its original binding"
    (with-fixture
     (lambda (f)
       (define a disposition-amendment-1)
       (define other (build-path (fixture-workspace f) "other-private"))
       (make-directory other #o700)
       (call-with-test-disposition-amendment
        a
        (lambda ()
          (check-equal? (disposition-amendment) a)
          (parameterize ([current-supplemental-authority-root other])
            (check-exn #rx"fixture authority root changed" disposition-amendment))
          (check-equal? (disposition-amendment) a)))
       ;; Correctly resealed public identity still is neither a broker grant nor
       ;; permission to relax the fixture schema/reference/scoped-field bounds.
       (for ([bad (list (hash-set a 'extra #t)
                        (hash-set a 'kind "supplemental-disposition-amendment/v2")
                        (hash-set a 'root-approval (hash-ref a 'root-proposal))
                        (hash-set a 'model-dispatches 2)
                        (hash-set a 'successor-permitted #t))])
         (check-exn #rx"invalid immutable fixture amendment"
                    (lambda ()
                      (call-with-test-disposition-amendment (seal-supplemental-record bad) void)))))))
  (test-case "permanent winner survives both durable cuts; historical v2 admission is not reused"
    (for ([cut '(disposition-claim-written disposition-pointer-written)])
      (with-spent
       (lambda (f path old parent po a)
         (define before
           (for/list ([p (in-directory (fixture-root f))]
                      #:when (regexp-match? #rx"[.]json$" (path->string p)))
             (cons p (file->bytes p))))
         (define-values (p g t target) (child! parent old a))
         (define-values (p2 g2 t2 target2) (child! parent old a 1650))
         (check-exn
          exn:fail?
          (lambda ()
            (parameterize ([current-supplemental-journal-crash-cut (lambda (event)
                                                                     (when (eq? event cut)
                                                                       (error 'cut "durable cut")))])
              (install-supplemental-disposition-claim! path old target p g))))
         (check-true (supplemental-recovery-pointer-claimed? path))
         (check-exn exn:fail?
                    (lambda () (install-supplemental-disposition-claim! path old target2 p2 g2)))
         (check-equal? (install-supplemental-disposition-claim! path old target p g) target)
         (check-exn exn:fail? (lambda () (load-supplemental-operation (hash-ref old 'task-approval))))
         (define-values (pp pt terminal)
           (inspect-supplemental-disposition-parent (hash-ref old 'task-approval)))
         (check-equal? terminal po)
         (check-equal? (hash-ref (start-supplemental-operation! g base-binding) 'revision) 0)
         ;; Ongoing admission follows the NEW owned allocation, not immutable parent4/7.
         (define fresh (hasheq 'attempt-id "attempt-6" 'attempt-fence 9 'coordinator-fence 9))
         (transition-supplemental-operation! g
                                             0
                                             "proposal-approved"
                                             "reserved"
                                             (hasheq 'allocation fresh))
         (check-equal? (hash-ref (hash-ref (load-supplemental-operation g) 'facts) 'allocation) fresh)
         (for ([entry before])
           (check-equal? (file->bytes (car entry)) (cdr entry)))
         (delete-file path)
         (check-true (supplemental-recovery-pointer-claimed? path))
         (check-exn exn:fail? (lambda () (load-supplemental-operation g)))))))
  (test-case "independent processes contend for exactly one permanent amendment winner"
    (with-spent (lambda (f path old parent po a)
                  (define-values (p g t target) (child! parent old a))
                  (define-values (p2 g2 t2 target2) (child! parent old a 1650))
                  (define workers
                    (list (worker (fixture-root f) a path old target p g)
                          (worker (fixture-root f) a path old target2 p2 g2)))
                  (for ([w workers])
                    (displayln "go" (caddr w))
                    (close-output-port (caddr w)))
                  (define statuses
                    (for/list ([w workers])
                      (define output (port->string (cadr w)))
                      (define errors (port->string (cadddr w)))
                      (subprocess-wait (car w))
                      (check-equal? errors "")
                      (close-input-port (cadr w))
                      (close-input-port (cadddr w))
                      (subprocess-status (car w))))
                  (check-equal? (sort statuses <) '(0 3))
                  (define winner (if (zero? (car statuses)) target target2))
                  (check-equal?
                   (install-supplemental-disposition-claim! path
                                                            old
                                                            winner
                                                            (hash-ref winner 'task-proposal)
                                                            (hash-ref winner 'task-approval))
                   winner))))
  (test-case "new grant revocation and alternate source paths never reopen the amendment"
    (with-spent
     (lambda (f path old parent po a)
       (define-values (p g t target) (child! parent old a))
       (define alternate (build-path (fixture-workspace f) "alias.json"))
       (put alternate old)
       (check-exn exn:fail?
                  (lambda () (install-supplemental-disposition-claim! alternate old target p g)))
       (install-supplemental-disposition-claim! path old target p g)
       (revoke-approval! g)
       (check-exn exn:fail? (lambda () (load-supplemental-operation g)))
       (check-exn exn:fail? (lambda () (install-supplemental-disposition-claim! path old target p g)))
       (delete-file path)
       (check-true (supplemental-recovery-pointer-claimed? path)))))
  (test-case "root, v2 and v3 parent revocation always deny passive ancestry and child admission"
    (for ([key '(root-approval v2-approval parent-approval)])
      (with-spent
       (lambda (f path old parent po a)
         (define-values (p g t target) (child! parent old a))
         (install-supplemental-disposition-claim! path old target p g)
         (revoke-approval! (hash-ref a key))
         (check-exn exn:fail?
                    (lambda ()
                      (inspect-supplemental-disposition-parent (hash-ref a 'parent-approval))))
         (check-exn exn:fail? (lambda () (start-supplemental-operation! g base-binding)))
         (delete-file path)
         (check-true (supplemental-recovery-pointer-claimed? path))))))
  (test-case "all disposition bindings are identity constraints, not substitutes for exact lineage"
    (with-spent
     (lambda (f path old parent po a)
       (define-values (p g t target) (child! parent old a))
       (for ([key (hash-keys (hash-ref t 'disposition))])
         (define d (hash-ref t 'disposition))
         (define replacement
           (cond
             [(memq key '(root-proposal parent-proposal)) (ref "task-proposal" #\f)]
             [(memq key '(root-approval parent-approval)) (ref "task-approval" #\f)]
             [(eq? key 'allocation) (hash-set (hash-ref a 'allocation) 'attempt-fence 99)]
             [else (make-string 64 #\f)]))
         (define changed
           (seal-supplemental-record (hash-set t 'disposition (hash-set d key replacement))))
         (define cp
           (propose-retain! "task-proposal"
                            (hasheq 'task changed 'expires-at (+ (current-seconds) 1600))))
         (define cg (approve! cp))
         (check-exn exn:fail?
                    (lambda ()
                      (install-supplemental-disposition-claim! path old (pointer cp cg) cp cg))))
       ;; Swapping body/full digest namespaces must also refuse independently.
       (define swapped
         (seal-supplemental-record (hash-set t
                                             'disposition
                                             (hash-set* (hash-ref t 'disposition)
                                                        'terminal-body-sha256
                                                        (hash-ref a 'terminal-record-sha256)
                                                        'terminal-record-sha256
                                                        (hash-ref a 'terminal-body-sha256)))))
       (define sp
         (propose-retain! "task-proposal"
                          (hasheq 'task swapped 'expires-at (+ (current-seconds) 1600))))
       (define sg (approve! sp))
       (check-exn exn:fail?
                  (lambda ()
                    (install-supplemental-disposition-claim! path old (pointer sp sg) sp sg)))
       (install-supplemental-disposition-claim! path old target p g)
       (check-exn exn:fail? (lambda () (inspect-supplemental-disposition-parent g))))))
  (test-case "pending historical WAL is refused without reconciliation or mutation"
    (with-spent
     (lambda (f path old parent po a)
       (define intent
         (build-path (fixture-root f)
                     "supplemental-operations"
                     (string-append (hash-ref (hash-ref old 'task-approval) 'sha256) ".intent.json")))
       (put intent po)
       (define bytes (file->bytes intent))
       (define-values (p g t target) (child! parent old a))
       (check-exn exn:fail?
                  (lambda () (inspect-supplemental-disposition-parent (hash-ref old 'task-approval))))
       (check-exn exn:fail? (lambda () (install-supplemental-disposition-claim! path old target p g)))
       (check-equal? (file->bytes intent) bytes)
       (delete-file intent)
       (check-equal? (install-supplemental-disposition-claim! path old target p g) target))))
  (test-case "deny-only discovery is inert on absent roots and fails closed on malformed disposition claims"
    (define absent (make-temporary-file "disposition-absent-~a" 'directory))
    (delete-directory absent)
    (parameterize ([current-supplemental-authority-root absent])
      (check-false (supplemental-recovery-pointer-claimed? "/tmp/unused-disposition-pointer"))
      (check-false (directory-exists? absent)))
    (with-fixture
     (lambda (f)
       (define bad (build-path (fixture-root f) "supplemental-disposition-malformed.json"))
       (put bad (seal-supplemental-record (hasheq 'schema "bad")))
       (check-exn exn:fail?
                  (lambda ()
                    (supplemental-recovery-pointer-claimed? "/tmp/unused-disposition-pointer"))))))
  (test-case "expired new approval cannot claim amendment-2; both permanent claims deny discovery independently"
    (with-spent
     (lambda (f path old parent po a)
       (define-values (ep eg et expired) (child! parent old a 1))
       (sleep 2)
       (check-exn exn:fail?
                  (lambda () (install-supplemental-disposition-claim! path old expired ep eg)))
       (define-values (p g t target) (child! parent old a))
       (install-supplemental-disposition-claim! path old target p g)
       (define claims
         (find-files (lambda (p)
                       (regexp-match? #rx"/supplemental-disposition-.*[.]json$" (path->string p)))
                     (fixture-root f)))
       (check-equal? (length claims) 2)
       (delete-file path)
       ;; Leave each disposition as the SOLE denial, hiding the recovery
       ;; claim too. Discovery must not open or repair the absent pointer.
       (define all-claims
         (find-files (lambda (p)
                       (regexp-match? #rx"/supplemental-(recovery|disposition)-.*[.]json$"
                                      (path->string p)))
                     (fixture-root f)))
       (for ([claim claims])
         (define bytes (file->bytes claim))
         (define hidden
           (for/list ([other all-claims]
                      #:unless (equal? other claim))
             (cons other (build-path (fixture-workspace f) (file-name-from-path other)))))
         (dynamic-wind (lambda ()
                         (for ([entry hidden])
                           (rename-file-or-directory (car entry) (cdr entry))))
                       (lambda ()
                         (check-true (supplemental-recovery-pointer-claimed? path))
                         (check-false (file-exists? path)))
                       (lambda ()
                         (for ([entry hidden])
                           (rename-file-or-directory (cdr entry) (car entry)))))
         (check-equal? (file->bytes claim) bytes)))))
  (test-case "historical expiry is ignored only by passive inspection"
    (with-spent
     (lambda (f path old parent po a)
       (sleep 3)
       (check-exn exn:fail? (lambda () (load-supplemental-operation (hash-ref old 'task-approval))))
       (define-values (_p _t terminal)
         (inspect-supplemental-disposition-parent (hash-ref old 'task-approval)))
       (check-equal? terminal po)
       (define-values (p g t target) (child! parent old a))
       (install-supplemental-disposition-claim! path old target p g)
       (check-equal? (hash-ref (start-supplemental-operation! g base-binding) 'revision) 0))
     #:short-parent? #t))
  (test-case "third-generation admission requires the exact late revision-6 terminal shape"
    ;; The exhausted revision-3 launch-spent shape is an ancestor generation's
    ;; terminal, never the fixed third generation's: binding it refuses.
    (with-spent3 #:late? #f
                 (lambda (f path old t2 terminal3 third p2 g2)
                   (check-equal? (hash-ref terminal3 'revision) 3)
                   ;; The generation gate, not merely the revision, decides the late shape:
                   ;; the fixed third amendment refuses this ancestor revision-3 terminal.
                   (check-false (disposition-late-terminal? third terminal3))
                   (define-values (p3 g3 _t3 target3) (child! t2 old third))
                   (claim-refused-with? path old target3 p3 g3)
                   (check-false (file-exists? (build-path (fixture-root f)
                                                          (string-append "supplemental-disposition-"
                                                                         (hash-ref third 'sha256)
                                                                         ".json")))))))
  (test-case "a pending all-zero terminal binding refuses through the isolated test policy"
    (with-spent3 (lambda (f path old t2 terminal3 third p2 g2)
                   ;; The genuine late record exists, but the presented amendment still
                   ;; carries the documented pending marker. Shape passes; both digest
                   ;; namespaces must independently refuse, and nothing is claimed.
                   (define pending
                     (seal-supplemental-record (hash-set* third
                                                          'terminal-body-sha256
                                                          (make-string 64 #\0)
                                                          'terminal-record-sha256
                                                          (make-string 64 #\0))))
                   (define second-fixture (car (disposition-ancestors)))
                   (define first-fixture (cadr (disposition-ancestors)))
                   (define-values (bp bg _bt _target) (child! t2 old pending))
                   (call-with-fixture-chain first-fixture
                                            second-fixture
                                            pending
                                            (lambda ()
                                              (claim-refused-with? path old (pointer bp bg) bp bg)))
                   (check-false (file-exists? (build-path (fixture-root f)
                                                          (string-append "supplemental-disposition-"
                                                                         (hash-ref pending 'sha256)
                                                                         ".json")))))))
  (test-case "amendment-3 admits its exact consumed lineage through both permanent predecessor claims"
    (with-spent3
     (lambda (f path old t2 terminal3 third p2 g2)
       ;; The amendment-3 parent is not yet claimed: its install binds the
       ;; third permanent claim and moves the pointer in one transition.
       (define-values (p3 g3 t3 target3) (child! t2 old third))
       (install-supplemental-disposition-claim! path old target3 p3 g3)
       (define claims
         (find-files (lambda (p)
                       (regexp-match? #rx"/supplemental-disposition-.*[.]json$" (path->string p)))
                     (fixture-root f)))
       (check-equal? (length claims) 3)
       ;; Passive three-link ancestry resolves through both permanent claims;
       ;; the active frame returns its own exact spent terminal.
       (check-true (disposition-late-terminal? third terminal3))
       ;; No ancestor generation can ever admit the late shape, even over the
       ;; same genuine record: the allowance is fixed to the third amendment.
       (check-false (disposition-late-terminal? (car (disposition-ancestors)) terminal3))
       (define-values (pp pt terminal) (inspect-supplemental-disposition-parent g2))
       (check-equal? pp p2)
       (check-equal? terminal terminal3)
       (check-exn exn:fail? (lambda () (load-supplemental-operation g2)))
       ;; New admission follows the NEW grant, never any immutable failed
       ;; ancestor approval, and it starts unspent.
       (check-equal? (hash-ref (start-supplemental-operation! g3 base-binding) 'revision) 0)
       (check-exn exn:fail? (lambda () (start-supplemental-operation! g2 base-binding)))))))
(test-case "no fourth successor: successor-permitted #f refuses a v3 child of the new grant permanently"
  (with-spent3
   (lambda (f path old t2 terminal3 third p2 g2)
     (define-values (p3 g3 t3 target3) (child! t2 old third))
     (install-supplemental-disposition-claim! path old target3 p3 g3)
     ;; A genuinely approved competitor of the amendment-3 parent can never
     ;; claim: the amendment is a single permanent winner. The distinct
     ;; expiry offset makes this a genuinely distinct approved competitor,
     ;; never a same-identity replay of the installed target.
     (define-values (p4 g4 t4 target4) (child! t2 old third 1650))
     (check-exn #rx"already permanently claimed"
                (lambda () (install-supplemental-disposition-claim! path old target4 p4 g4)))
     ;; A v3 successor OF the NEW grant (fourth successor generation) can
     ;; never claim or move the pointer, however it is presented.
     (define-values (p5 g5 t5 target5) (child! t3 target3 third 1650))
     (check-exn exn:fail?
                (lambda () (install-supplemental-disposition-claim! path target3 target5 p5 g5)))
     (check-exn exn:fail?
                (lambda () (install-supplemental-disposition-claim! path old target5 p5 g5)))
     (check-true (supplemental-recovery-pointer-claimed? path))
     (check-equal? (length (find-files (lambda (p)
                                         (regexp-match? #rx"/supplemental-disposition-.*[.]json$"
                                                        (path->string p)))
                                       (fixture-root f)))
                   3))))
(test-case "terminal facts mismatch refuses amendment-3 before any claim or pointer write"
  (with-spent3
   (lambda (f path old t2 terminal3 third p2 g2)
     (define (bad-variant changed)
       (seal-supplemental-record changed))
     (define bads
       (list
        (bad-variant (hash-set third 'terminal-body-sha256 (make-string 64 #\f)))
        (bad-variant (hash-set third 'terminal-record-sha256 (hash-ref third 'terminal-body-sha256)))
        (bad-variant
         (hash-set third 'allocation (hash-set (hash-ref third 'allocation) 'coordinator-fence 99)))))
     (define second-fixture (car (disposition-ancestors)))
     (define first-fixture (cadr (disposition-ancestors)))
     (for ([bad bads])
       ;; (i) Under the live amendment, a child whose descriptor carries the
       ;; mismatched terminal facts never claims.
       (define-values (bp bg _bt _btarget) (child! t2 old bad))
       (check-exn exn:fail?
                  (lambda ()
                    (install-supplemental-disposition-claim! path old (pointer bp bg) bp bg)))
       ;; (ii) Under the mismatched amendment binding itself, the parent's
       ;; real spent terminal no longer matches: admission refuses.
       (call-with-test-disposition-amendment
        first-fixture
        (lambda ()
          (call-with-test-disposition-amendment
           second-fixture
           (lambda ()
             (call-with-test-disposition-amendment
              bad
              (lambda ()
                (check-exn
                 #rx"exact spent terminal amendment facts required"
                 (lambda ()
                   (install-supplemental-disposition-claim! path old (pointer bp bg) bp bg))))
              #:predecessor second-fixture))
           #:predecessor first-fixture))))
     (check-false (file-exists? (build-path (fixture-root f)
                                            (string-append "supplemental-disposition-"
                                                           (hash-ref third 'sha256)
                                                           ".json"))))
     ;; The real binding still admits after every negative probe.
     (define-values (p3 g3 _t3 target3) (child! t2 old third))
     (install-supplemental-disposition-claim! path old target3 p3 g3)
     (check-true (supplemental-recovery-pointer-claimed? path)))))
(test-case "revocation at any of the five live links denies passive ancestry and child admission"
  (for ([key '(root-approval v2-approval prior-parent-approval parent-approval)])
    (with-spent3
     (lambda (f path old t2 terminal3 third p2 g2)
       (revoke-approval! (hash-ref third key))
       (check-exn exn:fail?
                  (lambda ()
                    (inspect-supplemental-disposition-parent (hash-ref third 'parent-approval))))
       (check-exn exn:fail? (lambda () (start-supplemental-operation! g2 base-binding)))
       (check-false (file-exists? (build-path (fixture-root f)
                                              (string-append "supplemental-disposition-"
                                                             (hash-ref third 'sha256)
                                                             ".json"))))))))
