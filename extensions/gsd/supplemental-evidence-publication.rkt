#lang racket/base
;; STABILITY: internal
;; BUG-0088 sections 7-8 (publication/supersession wave): shared owner
;; transition helpers, the staged supplemental evidence wrapper (T/A/K/G1 +
;; exact predicted context E for the EXACT proposed receipt r*), the sealed
;; supersession proposal S, and the durable extraction home for two blocks
;; moved out of delivery-journal.rkt (module-size law: extract instead of
;; growing past the ceiling). The extraction blocks keep their historical
;; contracts byte-for-byte; delivery-journal re-provides them.
;;
;; TRUST MODEL: nothing here grants authority. Live operator approvals are
;; revalidated through supplemental-evidence-authority at every boundary;
;; the journal's byte facts are re-derived inside the exclusive authority
;; lock ("late cut"), never trusted from caller state. This module does NOT
;; statically require delivery-journal/delivery-publication (delivery-journal
;; re-provides the extraction blocks); it loads them dynamically at call
;; time, exactly like supplemental-evidence-proof.rkt.
(require json
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         (only-in "../../util/json/checksum.rkt" sha256-string)
         (only-in "review-proof-format.rkt"
                  proof-hex?
                  valid-owned-proof-reference?
                  supplemental-owned-proof?)
         (only-in "evidence-repair-format.rkt"
                  canonical-evidence-digest
                  valid-evidence-context?
                  evidence-context-digest)
         (only-in "supplemental-evidence-format.rkt"
                  seal-supplemental-record
                  supplemental-record-digest
                  valid-supplemental-record?)
         (only-in "supplemental-evidence-authority.rkt"
                  current-supplemental-authority-root
                  call-with-supplemental-authority-lock
                  call-with-supplemental-admission
                  supplemental-reference?
                  propose-retain!
                  load-supplemental-record
                  validate-supplemental-approval)
         (only-in "supplemental-evidence-journal.rkt"
                  load-supplemental-operation
                  transition-supplemental-operation!)
         (only-in "review-proof.rkt" current-owned-proof-root frozen-verify-binding))
(provide (struct-out supplemental-staged-evidence)
         predict-supplemental-context
         stage-supplemental-evidence!
         load-supplemental-staged-evidence
         stage-supplemental-red!
         stage-supplemental-supersession-authorization!
         supplemental-contract-refs
         supplemental-staged-probe-transition!
         seal-supplemental-supersession-proposal!
         load-supplemental-supersession-proposal!
         replay-supplemental-supersession!
         ;; extraction blocks moved from delivery-journal.rkt (re-provided there)
         remote-pending-path
         load-remote-pending
         record-remote-pending!
         clear-remote-pending!
         remote-pending-blocker
         literal-files?
         context-receipt?
         valid-author-marker?
         author-marker-matches-journal?
         journal-evidence-author-repair
         evidence-author-repair-pending?)

(define-runtime-path journal-module "delivery-journal.rkt")
(define-runtime-path publication-module "delivery-publication.rkt")
(define (dynamic-journal symbol)
  (dynamic-require journal-module symbol))
(define (dynamic-publication symbol)
  (dynamic-require publication-module symbol))
(define (refuse message)
  (error 'supplemental-evidence-publication "~a" message))
(define (hex? s n)
  (and (string? s) (= (string-length s) n) (regexp-match? #px"^[0-9a-f]+$" s)))
(define (text? s)
  (and (string? s) (positive? (string-length s))))

;; ============================================================
;; Extraction block 1 (moved verbatim from delivery-journal.rkt):
;; remote-backing marker (v1.00.31 W3, register F5). The journal's load
;; validation REQUIRES a verified receipt, so an unpublished branch can
;; never park its state in the journal itself. The typed remote-pending
;; marker is a SIBLING record: schema-1, exact campaign/wave identity,
;; the unpushed branch/head and the reason. Its presence is a typed
;; ladder-entry refusal naming branch and head; it is cleared
;; automatically the moment the receipt is recorded (or already durable),
;; so "push, re-verify, deliver" is the only way forward.

(define (remote-pending-path root plan wave)
  (unless (and (hex? plan 64) (exact-nonnegative-integer? wave))
    (error 'delivery-journal "invalid campaign/wave identity"))
  (build-path (path->complete-path root)
              ".planning"
              "campaigns"
              plan
              (format "coordinator-w~a.remote-pending.json" wave)))

(define (load-remote-pending root plan wave)
  (define path (remote-pending-path root plan wave))
  (and (file-exists? path)
       (with-handlers ([exn:fail? (lambda (_) #f)])
         (define data (call-with-input-file path read-json))
         (and (hash? data)
              (equal? (hash-ref data 'schema-version #f) 1)
              (equal? (hash-ref data 'plan-id #f) plan)
              (equal? (hash-ref data 'wave #f) wave)
              (text? (hash-ref data 'branch #f))
              (hex? (hash-ref data 'head #f) 40)
              (string? (hash-ref data 'reason #f))
              data))))

(define (record-remote-pending! root plan wave branch head reason)
  (unless (and (text? branch) (hex? head 40) (string? reason) (positive? (string-length reason)))
    (error 'delivery-journal "invalid remote-pending marker"))
  (define path (remote-pending-path root plan wave))
  (when (link-exists? path)
    (error 'delivery-journal "symlinked remote-pending marker refused"))
  (make-parent-directory* path)
  (call-with-atomic-output-file path
                                (lambda (out _)
                                  (write-json (hasheq 'schema-version
                                                      1
                                                      'plan-id
                                                      plan
                                                      'wave
                                                      wave
                                                      'branch
                                                      branch
                                                      'head
                                                      head
                                                      'at
                                                      (current-seconds)
                                                      'reason
                                                      reason)
                                              out)
                                  (newline out))))

(define (clear-remote-pending! root plan wave)
  (define path (remote-pending-path root plan wave))
  (when (file-exists? path)
    (delete-file path)))

;; The typed ladder-entry gate: (remote-pending-blocker root plan wave)
;; returns 'branch-not-published exactly when the marker exists, else #f.
;; Callers that carry the marker hash can name branch and head in the
;; operator-facing refusal (see delivery-coordinator).
(define (remote-pending-blocker root plan wave)
  (and (load-remote-pending root plan wave) 'branch-not-published))

;; ============================================================
;; Extraction block 2 (moved verbatim from delivery-journal.rkt):
;; the permanent one-per-wave evidence-author marker. The supplemental
;; supersession MUST preserve this spent marker byte-for-byte, so its
;; load-time validation lives beside the transition that preserves it.

(define (literal-files? fs)
  (and (list? fs)
       (pair? fs)
       (= (length fs) (length (remove-duplicates fs)))
       (andmap (lambda (f)
                 (and (text? f)
                      (regexp-match? #px"^[A-Za-z0-9_.-]+(?:/[A-Za-z0-9_.-]+)*$" f)
                      (not (regexp-match? #px"(?:^|/)[.][.]?(?:/|$)" f))))
               fs)))
(define (context-receipt? c r plan wave generation)
  (and (valid-evidence-context? c)
       (valid-delivery-receipt?/checked r)
       (equal? (hash-ref c 'plan-id) plan)
       (equal? (hash-ref c 'wave) wave)
       (equal? (hash-ref c 'receipt-generation) generation)
       (equal? (hash-ref c 'receipt-sha256) (canonical-evidence-digest r))
       (valid-owned-proof-reference? (hash-ref r 'owned-proof #f))
       (for/and ([k '(attempt-id attempt-fence branch origin head tree)])
         (equal? (hash-ref c k) (hash-ref r k #f)))
       (equal? (hash-ref c 'green-proof-sha256)
               (hash-ref (hash-ref r 'owned-proof (hasheq)) 'sha256 #f))))
(define (valid-delivery-receipt?/checked r)
  ;; Loaded dynamically to keep this module outside the journal's static
  ;; import graph; the journal remains the sole validator of receipt shape.
  ((dynamic-journal 'valid-delivery-receipt?) r))
(define (valid-author-marker? m plan wave)
  (and (hash? m)
       (equal? (hash-ref m 'schema-version #f) 1)
       ;; BUG-0084: "retained" is the honest no-edit terminal — the author
       ;; examined the wave and returned without edits; the old receipt stays
       ;; current and the loop routes to a fresh review.
       (member (hash-ref m 'state #f) '("pending" "running" "completed" "retained"))
       (literal-files? (hash-ref m 'frozen-files #f))
       (let ([c (hash-ref m 'context #f)]
             [r (hash-ref m 'old-receipt #f)])
         (and (valid-evidence-context? c)
              (context-receipt? c r plan wave (hash-ref c 'receipt-generation))
              (or (equal? (hash-ref m 'state) "pending")
                  (and (text? (hash-ref m 'successor-attempt #f))
                       (not (equal? (hash-ref m 'successor-attempt) (hash-ref c 'attempt-id)))
                       (exact-nonnegative-integer? (hash-ref m 'successor-fence #f))
                       (>= (hash-ref m 'successor-fence) (hash-ref c 'attempt-fence))))))
       (or (not (equal? (hash-ref m 'state) "completed"))
           (hex? (hash-ref m 'successor-receipt-sha256 #f) 64))
       (or (not (equal? (hash-ref m 'state) "retained"))
           (and (member (hash-ref m 'disposition #f) '("no-edit" "session-lost"))
                (hex? (hash-ref m 'retained-tree-sha256 #f) 40)))))
(define (author-marker-matches-journal? m j)
  (and (valid-author-marker? m (hash-ref j 'plan-id #f) (hash-ref j 'wave #f))
       (let* ([history (hash-ref j 'receipt-history '())]
              [receipts (append history (list (hash-ref j 'receipt)))]
              [generation (hash-ref (hash-ref m 'context) 'receipt-generation)])
         (and (< generation (length receipts))
              (equal? (list-ref receipts generation) (hash-ref m 'old-receipt))
              (if (equal? (hash-ref m 'state) "completed")
                  (and (< (add1 generation) (length receipts))
                       (let ([successor (list-ref receipts (add1 generation))])
                         (and (equal? (hash-ref successor 'attempt-id #f)
                                      (hash-ref m 'successor-attempt))
                              (equal? (hash-ref successor 'attempt-fence #f)
                                      (hash-ref m 'successor-fence))
                              (equal? (canonical-evidence-digest successor)
                                      (hash-ref m 'successor-receipt-sha256)))))
                  (= generation (length history)))))))
(define (journal-evidence-author-repair j)
  (define m (and (hash? j) (hash-ref j 'evidence-author-repair #f)))
  (and m (author-marker-matches-journal? m j) m))
(define (evidence-author-repair-pending? j)
  (define m (journal-evidence-author-repair j))
  (and m
       (member (hash-ref m 'state) '("pending" "running"))
       (equal? (hash-ref m 'old-receipt) (hash-ref j 'receipt #f))
       #t))

;; ============================================================
;; Concurrency/authority admission lives in supplemental-evidence-
;; authority.rkt (coordinated shared implementation):
;; call-with-supplemental-authority-lock is the exclusive, same-root
;; reentrant owner lock, and call-with-supplemental-admission validates a
;; LIST of live approval references inside that critical section. The
;; helpers below use them; they never re-implement them.

;; ============================================================
;; The exact predicted evidence context E for the EXACT proposed receipt.

;; Derived ONLY from the exact r* bytes, the authenticated journal's current
;; generation (read late, inside the caller's lock), and the frozen snapshot
;; identity. E's canonical digest never depends on R1. Certification must
;; preserve r* exactly; E is recomputed from the same inputs at every use and
;; refuses to silently drift.
(define (predict-supplemental-context journal receipt frozen plan wave)
  (unless (and (hash? journal)
               ((dynamic-journal 'valid-delivery-receipt?) receipt)
               (hash? frozen)
               (hex? (hash-ref frozen 'snapshot-sha256 #f) 64))
    (refuse "cannot predict supplemental context without receipt and frozen snapshot"))
  (define old-receipt (hash-ref journal 'receipt #f))
  (unless (and (hash? old-receipt)
               (equal? (hash-ref old-receipt 'branch #f) (hash-ref receipt 'branch #f))
               (equal? (hash-ref old-receipt 'origin #f) (hash-ref receipt 'origin #f))
               (equal? (hash-ref old-receipt 'repo #f) (hash-ref receipt 'repo #f))
               (not (equal? (hash-ref old-receipt 'head #f) (hash-ref receipt 'head #f))))
    (refuse "superseded receipt must keep the campaign repository/branch identity"))
  (define history (hash-ref journal 'receipt-history '()))
  (unless ((dynamic-journal 'valid-receipt-history?) history)
    (refuse "journal receipt history is malformed"))
  (define ref (hash-ref receipt 'owned-proof #f))
  (unless (and (supplemental-owned-proof? ref) (valid-owned-proof-reference? ref))
    (refuse "predicted context requires an authenticated supplemental GREEN reference"))
  (define context
    (hasheq 'plan-id
            plan
            'wave
            wave
            'attempt-id
            (hash-ref receipt 'attempt-id)
            'attempt-fence
            (hash-ref receipt 'attempt-fence)
            'receipt-generation
            (add1 (length history))
            'receipt-sha256
            (canonical-evidence-digest receipt)
            'evidence-revision
            0
            'repo
            ;; The evidence context's repo is the exact execution checkout
            ;; identity (no trailing separator), matching the readiness and
            ;; probe canonicalization of the authenticated execution cwd.
            (let ([r (hash-ref receipt 'repo)])
              (if (regexp-match? #px"/+$" r)
                  (string-append
                   (regexp-replace #px"/+$" r "")
                   (if (equal? (regexp-replace #px"/+$" r "") "/") "/" ""))
                  r))
            'branch
            (hash-ref receipt 'branch)
            'origin
            (hash-ref receipt 'origin)
            'head
            (hash-ref receipt 'head)
            'tree
            (hash-ref receipt 'tree)
            'snapshot-sha256
            (hash-ref frozen 'snapshot-sha256)
            'green-proof-sha256
            (hash-ref ref 'sha256)))
  (unless (valid-evidence-context? context)
    (refuse "predicted supplemental context is invalid"))
  context)

;; ============================================================
;; Staged wrapper: DISTINCT from current readiness, persisted under the
;; owner-managed proof store. Pinning is monotonic: the exact r*, E, G1 and
;; contract references are immutable once staged; later stages may only fill
;; in R1 and the S authorization.

(struct supplemental-staged-evidence (record path) #:transparent)
(define staged-kind "supplemental-staged-evidence/v1")
(define staged-pinned-keys
  '(kind plan-id wave contract task-sha256 candidate-sha256 attempt-id attempt-fence
                                                         receipt old-receipt-sha256 old-generation context
                                                         green-proof-ref baseline-head baseline-tree test-path
                                                         harness-path harness-files operation-id snapshot-sha256))
(define (staging-path plan)
  (build-path (current-owned-proof-root) plan "supplemental-staging" "staged.json"))
(define (valid-staged-record? s)
  (and (hash? s)
       (equal? (hash-ref s 'kind #f) staged-kind)
       (hex? (hash-ref s 'plan-id #f) 64)
       (exact-nonnegative-integer? (hash-ref s 'wave #f))
       (let ([refs (hash-ref s 'contract #f)])
         (and (hash? refs)
              (andmap (lambda (k) (supplemental-reference? (hash-ref refs k #f)))
                      '(task-proposal task-approval candidate-proposal candidate-approval))))
       (hex? (hash-ref s 'task-sha256 #f) 64)
       (hex? (hash-ref s 'candidate-sha256 #f) 64)
       (text? (hash-ref s 'attempt-id #f))
       (exact-nonnegative-integer? (hash-ref s 'attempt-fence #f))
       ((dynamic-journal 'valid-delivery-receipt?) (hash-ref s 'receipt #f))
       (hex? (hash-ref s 'old-receipt-sha256 #f) 64)
       (exact-nonnegative-integer? (hash-ref s 'old-generation #f))
       (valid-evidence-context? (hash-ref s 'context #f))
       (supplemental-owned-proof? (hash-ref s 'green-proof-ref #f))
       (hex? (hash-ref s 'baseline-head #f) 40)
       (hex? (hash-ref s 'baseline-tree #f) 40)
       (text? (hash-ref s 'test-path #f))
       (text? (hash-ref s 'harness-path #f))
       (let ([h (hash-ref s 'harness-files #f)])
         (and (list? h) (pair? h) (andmap (lambda (e) (and (hash? e) (text? (hash-ref e 'path #f)) (hex? (hash-ref e 'sha256 #f) 64))) h)))
       (equal? (hash-ref s 'operation-id #f) "repair-1")
       (hex? (hash-ref s 'snapshot-sha256 #f) 64)
       (let ([red (hash-ref s 'red-proof-ref #f)])
         (or (not red) (and (hash? red) (hex? (hash-ref red 'sha256 #f) 64) (text? (hash-ref red 'path #f)))))
       (let ([sref (hash-ref s 'supersession-proposal #f)]
             [sapref (hash-ref s 'supersession-approval #f)])
         (or (and (not sref) (not sapref))
             (and (supplemental-reference? sref)
                  (equal? (hash-ref sref 'kind) "supersession-proposal")
                  (supplemental-reference? sapref)
                  (equal? (hash-ref sapref 'kind) "supersession-approval"))))))
;; Persisted JSON loads with mutable hashes; the staged validators (and every
;; stored reference predicate) require immutable data, so normalize on read.
(define (immutable-json v)
  (cond
    [(hash? v)
     (for/hasheq ([(k x) (in-hash v)]) (values k (immutable-json x)))]
    [(list? v) (map immutable-json v)]
    [(string? v) (string->immutable-string v)]
    [else v]))
(define (read-staged path)
  (and (file-exists? path)
       (let ([data
              (begin
                (when (link-exists? path) (refuse "symlinked staged wrapper"))
                (immutable-json (call-with-input-file path read-json)))])
         (unless (and (valid-staged-record? data)
                      (equal? (hash-ref data 'sha256) (canonical-evidence-digest data)))
           (refuse "staged wrapper is malformed or altered"))
         data)))
(define (write-staged! path record)
  (make-parent-directory* path)
  (define temp (make-temporary-file ".staged-~a.tmp" #f (path-only path)))
  (dynamic-wind
   void
   (lambda ()
     (call-with-output-file temp
                            (lambda (out)
                              (write-json (hash-set record 'sha256 (canonical-evidence-digest record)) out)
                              (newline out))
                            #:exists 'truncate)
     (rename-file-or-directory temp path #t))
   (lambda ()
     (when (file-exists? temp)
       (delete-file temp)))))
(define (seal-staged record) (hash-set record 'sha256 (canonical-evidence-digest record)))
;; UPDATE supplies the fields being added; every pinned field already staged
;; must repeat byte-identically (monotonic fill-in of R1/S only).
(define (merge-staged! path update)
  (define prior (read-staged path))
  (define next
    (if prior
        (for/fold ([rec prior]) ([(k v) (in-hash update)])
          (when (and (member k staged-pinned-keys)
                     (not (equal? (hash-ref prior k) v)))
            (refuse (format "staged pinned field ~a may not change" k)))
          (hash-set rec k v))
        update))
  (define sealed (seal-staged next))
  (unless (valid-staged-record? sealed)
    (refuse "staged wrapper update is invalid"))
  (write-staged! path sealed)
  sealed)
(define (load-supplemental-staged-evidence base plan wave)
  (define path (staging-path plan))
  (define s (read-staged path))
  (and s
       (let ([s (supplemental-staged-evidence s path)])
         (unless (and (equal? (hash-ref (supplemental-staged-evidence-record s) 'plan-id) plan)
                      (equal? (hash-ref (supplemental-staged-evidence-record s) 'wave) wave))
           (refuse "staged wrapper identity mismatch"))
         s)))
(define (stage-supplemental-evidence! base plan wave attempt-id fence contract receipt green-ref task candidate-record)
  (unless (and (hash? contract)
               (andmap (lambda (k) (supplemental-reference? (hash-ref contract k #f)))
                       '(task-proposal task-approval candidate-proposal candidate-approval))
               (hash? task)
               (hash? candidate-record)
               (equal? (hash-ref task 'sha256) (hash-ref candidate-record 'task-sha256 #f)))
    (refuse "invalid staged task/candidate contract"))
  (define path (staging-path plan))
  (define frozen (frozen-verify-binding base plan wave))
  ;; Late cut: E is predicted from the authenticated journal's CURRENT
  ;; generation under the exclusive authority lock, before any probe runs.
  (define staged
    (call-with-supplemental-authority-lock
     (lambda ()
       (define journal ((dynamic-journal 'load-delivery-journal) base plan wave))
       (unless journal (refuse "missing authenticated journal"))
       (define context
         (predict-supplemental-context journal receipt frozen plan wave))
       (unless (and (equal? (hash-ref context 'attempt-id) attempt-id)
                    (equal? (hash-ref context 'attempt-fence) fence))
         (refuse "staged context does not carry the live allocation"))
       (unless (and (equal? (hash-ref task 'baseline-head) (hash-ref task 'baseline-head #f))
                    (hex? (hash-ref task 'baseline-head #f) 40))
         (refuse "task baseline identity is malformed"))
       (define harness-files
         (for/list ([e (in-list (hash-ref candidate-record 'manifest '()))]
                    #:when (equal? (hash-ref e 'path #f) (hash-ref task 'harness-path)))
           (hasheq 'path (hash-ref e 'path) 'sha256 (hash-ref e 'sha256))))
       (unless (pair? harness-files)
         (refuse "candidate manifest lacks the approved harness entry"))
       (seal-staged
        (hasheq 'kind
                staged-kind
                'plan-id
                plan
                'wave
                wave
                'contract
                contract
                'task-sha256
                (hash-ref task 'sha256)
                'candidate-sha256
                (hash-ref (hash-ref contract 'candidate-proposal) 'sha256)
                'attempt-id
                attempt-id
                'attempt-fence
                fence
                'receipt
                receipt
                'old-receipt-sha256
                (canonical-evidence-digest (hash-ref journal 'receipt))
                'old-generation
                (length (hash-ref journal 'receipt-history '()))
                'context
                context
                'green-proof-ref
                green-ref
                'baseline-head
                (hash-ref task 'baseline-head)
                'baseline-tree
                (hash-ref task 'baseline-tree)
                'test-path
                (hash-ref task 'test-path)
                'harness-path
                (hash-ref task 'harness-path)
                'harness-files
                harness-files
                'operation-id
                "repair-1"
                'snapshot-sha256
                (hash-ref frozen 'snapshot-sha256))))))
  (merge-staged! path staged)
  (load-supplemental-staged-evidence base plan wave))
(define (stage-supplemental-red! base plan wave red-ref)
  (unless (and (hash? red-ref) (hex? (hash-ref red-ref 'sha256 #f) 64) (text? (hash-ref red-ref 'path #f)))
    (refuse "invalid red proof reference"))
  (define path (staging-path plan))
  (merge-staged! path (hasheq 'kind staged-kind 'red-proof-ref red-ref))
  (load-supplemental-staged-evidence base plan wave))
(define (stage-supplemental-supersession-authorization! base plan wave sref sapref)
  (define path (staging-path plan))
  (merge-staged!
   path
   (hasheq 'kind staged-kind 'supersession-proposal sref 'supersession-approval sapref))
  (load-supplemental-staged-evidence base plan wave))
(define (supplemental-contract-refs staged)
  (hash-ref (supplemental-staged-evidence-record staged) 'contract))

;; ============================================================
;; Protected operation plumbing shared by the staged probe and the
;; publication/promotion transitions. Idempotent at target: a replay
;; observing the target state returns instead of double-writing.

(define (supplemental-staged-probe-transition! grant-ref from to facts)
  (define operation (load-supplemental-operation grant-ref))
  (cond
    [(equal? (hash-ref operation 'state) to) operation]
    [(equal? (hash-ref operation 'state) from)
     (transition-supplemental-operation! grant-ref
                                         (hash-ref operation 'revision)
                                         from
                                         to
                                         facts)]
    [else
     (refuse
      (format "protected operation is ~a, expected ~a to advance to ~a"
              (hash-ref operation 'state)
              from
              to))]))

;; ============================================================
;; Sealed supersession proposal S.

(define supersession-payload-keys
  '(kind context task-sha256 receipt old-receipt-sha256 old-generation
                                                         attempt-id attempt-fence green-proof-ref red-proof-ref
                                                         task-proposal task-approval candidate-proposal
                                                         candidate-approval snapshot-sha256 sha256))
(define (build-supplemental-supersession-record staged)
  (define record (supplemental-staged-evidence-record staged))
  (unless (hash-ref record 'red-proof-ref #f)
    (refuse "cannot seal S before the staged RED/GREEN probe is captured"))
  (unless (and (valid-evidence-context? (hash-ref record 'context #f))
               ((dynamic-journal 'valid-delivery-receipt?) (hash-ref record 'receipt #f)))
    (refuse "staged wrapper lacks the exact proposed receipt/context"))
  (seal-supplemental-record
   (hasheq 'kind
           "supplemental-evidence-supersession/v1"
           'context
           (hash-ref record 'context)
           'task-sha256
           (hash-ref record 'task-sha256)
           'receipt
           (hash-ref record 'receipt)
           'old-receipt-sha256
           (hash-ref record 'old-receipt-sha256)
           'old-generation
           (hash-ref record 'old-generation)
           'attempt-id
           (hash-ref record 'attempt-id)
           'attempt-fence
           (hash-ref record 'attempt-fence)
           'green-proof-ref
           (hash-ref record 'green-proof-ref)
           'red-proof-ref
           (hash-ref record 'red-proof-ref)
           'task-proposal
           (hash-ref (hash-ref record 'contract) 'task-proposal)
           'task-approval
           (hash-ref (hash-ref record 'contract) 'task-approval)
           'candidate-proposal
           (hash-ref (hash-ref record 'contract) 'candidate-proposal)
           'candidate-approval
           (hash-ref (hash-ref record 'contract) 'candidate-approval)
           'snapshot-sha256
           (hash-ref record 'snapshot-sha256))))
(define (retain-supplemental-supersession! record)
  (unless (valid-supplemental-record? record)
    (refuse "sealed supersession record is invalid"))
  (propose-retain! "supersession-proposal" record))
(define (seal-supplemental-supersession-proposal! base plan wave)
  (define staged (load-supplemental-staged-evidence base plan wave))
  (unless staged (refuse "no staged supplemental evidence"))
  (define record (build-supplemental-supersession-record staged))
  (retain-supplemental-supersession! record))
(define (load-supplemental-supersession-proposal! sref)
  (unless (and (supplemental-reference? sref) (equal? (hash-ref sref 'kind) "supersession-proposal"))
    (refuse "invalid supersession proposal reference"))
  (define proposal (load-supplemental-record sref))
  (define payload (hash-ref proposal 'payload #f))
  (unless (and (hash? payload)
                (equal? (hash-ref payload 'kind #f) "supplemental-evidence-supersession/v1")
                (valid-supplemental-record? payload)
                ;; sref digests the wrapped proposal; the payload's own seal
                ;; is what must be internally consistent.
                (equal? (hash-ref payload 'sha256 #f) (supplemental-record-digest payload))
                (andmap (lambda (k) (hash-has-key? payload k)) supersession-payload-keys))
    (refuse "sealed supersession payload is malformed"))
  payload)

;; ============================================================
;; Guarded publication/supersession replay. The heavy transition lives in
;; delivery-publication.rkt (it owns the durable publication record); this
;; wrapper is the crash-recovery entry: it refuses without an exact S
;; authorization and a durable write-ahead intent, and is idempotent at the
;; already-published/already-superseded identities. It NEVER restarts a
;; probe, a model dispatch, or an unconstrained publication.
(define (replay-supplemental-supersession! base plan wave read-current sapref live-guard)
  (unless (and (supplemental-reference? sapref)
               (equal? (hash-ref sapref 'kind) "supersession-approval"))
    (refuse "invalid supersession approval reference"))
  ;; The approval record names the exact S proposal; its own digest is not
  ;; the proposal's digest.
  (define approval-record (validate-supplemental-approval sapref))
  (unless (and approval-record
               (supplemental-reference? (hash-ref approval-record 'proposal #f))
               (equal? (hash-ref (hash-ref approval-record 'proposal #f) 'kind)
                       "supersession-proposal"))
    (refuse "supersession approval is missing, revoked or foreign"))
  (define sproposal-ref (hash-ref approval-record 'proposal))
  (define payload (load-supplemental-supersession-proposal! sproposal-ref))
  (unless (validate-supplemental-approval sapref sproposal-ref)
    (refuse "supersession approval is missing, revoked or foreign"))
  (define staged (load-supplemental-staged-evidence base plan wave))
  (unless (and staged
               (equal? (hash-ref (supplemental-staged-evidence-record staged) 'supersession-approval #f)
                       sapref))
    (refuse "staged wrapper does not carry this S authorization"))
  ((dynamic-publication 'publish-supplemental-supersession!)
   base plan wave read-current sapref payload live-guard))

;; Idempotent protected-operation advance through the publication tail
;; (supersession-approved -> published -> receipt-certified). A replay that
;; observes a later state is a no-op; a different state refuses.
(define (complete-supplemental-supersession! grant-ref payload)
  (define receipt (hash-ref payload 'receipt))
  (define (current-state)
    (hash-ref (load-supplemental-operation grant-ref) 'state #f))
  (when (equal? (current-state) "supersession-approved")
    (supplemental-staged-probe-transition!
     grant-ref
     "supersession-approved"
     "published"
     (hasheq 'publication
             (hasheq 'status
                     "confirmed"
                     'branch
                     (hash-ref receipt 'branch)
                     'head
                     (hash-ref receipt 'head)))))
  (when (equal? (current-state) "published")
    (supplemental-staged-probe-transition!
     grant-ref
     "published"
     "receipt-certified"
     (hasheq 'receipt (canonical-evidence-digest receipt)))))
