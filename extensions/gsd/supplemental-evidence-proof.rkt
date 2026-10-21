#lang racket/base
;; BUG-0088: authenticated C1 resolution, NOT a replacement frozen declaration.
;; CONTRACT is an inert hash of four private kind+sha256 references. Every use
;; reloads T, A, K, CAp and the protected operation; supplied descriptors cannot
;; authorize execution. No parameter accepting an effective command/hash exists.
;;
;; Controller preconditions (not granted by this module): hold the exclusive
;; campaign lease/operation ownership; allocate the real fresh attempt/fence;
;; consume dispatch before authoring; seal scope/commit-chain policy before K;
;; persist that campaign in verifying; transition the protected operation with
;; verify-job={execution-cwd,command-sha256,operational-jobs (#f or 2)}; admit
;; exactly one bounded owned Verify and retain admission until group quiescence.
;; Allocation facts must carry attempt-id, attempt-fence, coordinator-fence.
;; This resolver does not certify/supersede a receipt or authorize publication.
(require racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         "review-proof-format.rkt"
         "supplemental-evidence-format.rkt"
         "supplemental-evidence-authority.rkt"
         "supplemental-evidence-journal.rkt"
         "campaign-state.rkt"
         (only-in "campaign-repository.rkt" load-campaign-record)
         "evidence-repair-format.rkt"
         "../../sandbox/subprocess.rkt"
         (only-in "../../util/json/checksum.rkt" sha256-file sha256-string))
(provide resolve-supplemental-verify-binding
         validate-supplemental-proof-authority!
         supplemental-proof-offline-facts
         (struct-out supplemental-proof-facts))
;; Offline facts are NOT permission. Bundle admission must independently anchor
;; the four refs (and later promotion/registration) in the authenticated request.
(struct supplemental-proof-facts
        (identity frozen-command effective-command source attempt-id attempt-fence)
  #:transparent)
(define-runtime-path journal-module "delivery-journal.rkt")
(define-runtime-path receipt-module "delivery-receipt.rkt")
(define-runtime-path proof-module "review-proof.rkt")
(define (refuse message)
  (error 'supplemental-evidence-proof "~a" message))
(define (git-fact cwd . args)
  (define r
    (run-subprocess "git"
                    #:args (append (list "-C" cwd) args)
                    #:directory cwd
                    #:timeout 15
                    #:process-group? #t
                    #:encoding 'utf-8/strict))
  (and (not (subprocess-result-timed-out? r))
       (not (subprocess-result-truncated? r))
       (equal? (subprocess-result-exit-code r) 0)
       (string-trim (subprocess-result-stdout r))))
(define ref-keys '(task-proposal task-approval candidate-proposal candidate-approval))
;; JSON readers return mutable strings. Copy inert wire data before asking the
;; authority for its immutable reference type; this never creates an approval.
(define (freeze-wire value)
  (cond
    [(hash? value)
     (for/hasheq ([(k v) (in-hash value)])
       (values k (freeze-wire v)))]
    [(list? value) (map freeze-wire value)]
    [(string? value) (string->immutable-string value)]
    [else value]))
(define (refs-only contract)
  (unless (hash? contract)
    (refuse "supplemental references required"))
  (for/hasheq ([key (in-list ref-keys)])
    (define ref (freeze-wire (hash-ref contract key #f)))
    (unless (and (supplemental-reference? ref) (equal? (hash-ref ref 'kind) (symbol->string key)))
      (refuse (format "incorrect supplemental reference kind: ~a = ~a" key ref)))
    (values key ref)))
(define (payload-task record)
  (define payload (hash-ref record 'payload))
  (unless (and (hash? payload)
               (= (hash-count payload) 2)
               (valid-supplemental-task? (hash-ref payload 'task #f))
               (exact-positive-integer? (hash-ref payload 'expires-at #f)))
    (refuse "invalid bounded private task payload"))
  (hash-ref payload 'task))
(define (descriptor! descriptor frozen before attempt-id fence cwd)
  (define refs (refs-only descriptor))
  (define tr (freeze-wire (hash-ref descriptor 'task-record)))
  (define kr (freeze-wire (hash-ref descriptor 'candidate-record)))
  (for ([record (in-list (list tr kr))]
        [key '(task-proposal candidate-proposal)])
    (unless (and (hash? record)
                 (equal? (hash-ref record 'schema #f) "supplemental-proposal-store/v1")
                 (equal? (hash-ref record 'kind #f) (symbol->string key))
                 (equal? (canonical-evidence-digest record) (hash-ref (hash-ref refs key) 'sha256)))
      (refuse "public descriptor differs from retained proposal identity")))
  (define task (payload-task tr))
  (define candidate (hash-ref kr 'payload))
  (define context (hash-ref task 'context))
  (unless (and (valid-supplemental-record? candidate)
               (equal? (hash-ref candidate 'kind) "supplemental-evidence-candidate/v1")
               (equal? (hash-ref candidate 'task-sha256) (hash-ref task 'sha256))
               (equal? (hash-ref candidate 'task-proposal #f) (hash-ref refs 'task-proposal))
               (equal? (hash-ref candidate 'task-approval #f) (hash-ref refs 'task-approval))
               (equal? (hash-ref candidate 'context) context)
               (equal? (hash-ref candidate 'source #f) before)
               (equal? (hash-ref candidate 'attempt-id #f) attempt-id)
               (equal? (hash-ref candidate 'attempt-fence #f) fence)
               (equal? (hash-ref candidate 'execution-cwd #f) cwd)
               (not (equal? attempt-id (hash-ref context 'attempt-id)))
               (> fence (hash-ref context 'attempt-fence))
               (equal? (hash-ref frozen 'snapshot-sha256) (hash-ref context 'snapshot-sha256))
               (equal? (hash-ref frozen 'normalized-command) (hash-ref task 'base-command))
               (null? (hash-ref tr 'refs))
               (equal? (hash-ref context 'repo) cwd)
               (equal? (hash-ref kr 'refs)
                       (list (hash-ref refs 'task-proposal) (hash-ref refs 'task-approval))))
    (refuse "candidate/task/frozen/source/attempt binding mismatch"))
  (for ([key '(branch origin)])
    (unless (equal? (hash-ref context key) (hash-ref before key))
      (refuse "candidate changed campaign identity")))
  (values refs task candidate))
(define (check-path! cwd relative)
  (unless (and (string? relative) (regexp-match? #px"^[A-Za-z0-9_][A-Za-z0-9_./-]*$" relative))
    (refuse "unsafe manifest path"))
  (for/fold ([path cwd]) ([part (in-list (string-split relative "/" #:trim? #f))])
    (when (member part '("" "." ".."))
      (refuse "escaping manifest path"))
    (define next (build-path path part))
    (when (link-exists? next)
      (refuse "symlinked candidate path"))
    next))
(define (actual-source! candidate task cwd before)
  (define snapshot (dynamic-require receipt-module 'committed-delivery-snapshot))
  (unless (and (equal? cwd (path->string (simplify-path (path->complete-path cwd))))
               (equal? before (snapshot cwd)))
    (refuse "candidate source is not the actual clean execution checkout"))
  (for/fold ([part (build-path "/")]) ([piece (in-list (cdr (explode-path (string->path cwd))))])
    (define next (build-path part piece))
    (when (link-exists? next)
      (refuse "symlinked execution checkout"))
    next)
  (define manifest (hash-ref candidate 'manifest #f))
  (define expected-paths
    (append (list (hash-ref task 'test-path) (hash-ref task 'harness-path))
            (hash-ref task 'owner-paths)))
  (unless (and (list? manifest)
               (= (length manifest) (length expected-paths))
               (equal? (sort (map (lambda (e) (hash-ref e 'path)) manifest) string<?)
                       (sort expected-paths string<?)))
    (refuse "candidate manifest must enumerate exact task paths"))
  (for ([entry (in-list manifest)])
    (define path (check-path! cwd (hash-ref entry 'path)))
    (define mode (hash-ref entry 'mode #f))
    (define blob (hash-ref entry 'blob #f))
    (unless (and (member mode '("100644" "100755"))
                 (proof-hex? blob 40)
                 (file-exists? path)
                 (equal? (zero? (bitwise-and (file-or-directory-permissions path 'bits) #o111))
                         (equal? mode "100644"))
                 (equal? (sha256-file path) (hash-ref entry 'sha256 #f))
                 (equal? (git-fact cwd "ls-tree" "HEAD" "--" (hash-ref entry 'path))
                         (format "~a blob ~a\t~a" mode blob (hash-ref entry 'path))))
      (refuse "candidate blob/mode/bytes mismatch")))
  (unless (equal? before (snapshot cwd))
    (refuse "candidate source changed during manifest validation")))
;; frozen is the unchanged result of frozen-verify-binding, never a caller's C1.
;; #:capture? #t requires the verifying phase. Readback accepts subsequent phases
;; but still refuses revoked/blocked/expired operations and source drift.
(define (resolve-supplemental-verify-binding contract
                                             frozen
                                             base
                                             plan
                                             wave
                                             before
                                             attempt-id
                                             fence
                                             cwd
                                             #:capture? [capture? #t])
  (unless (equal? frozen ((dynamic-require proof-module 'frozen-verify-binding) base plan wave))
    (refuse "caller frozen binding differs from immutable snapshot"))
  (define current (load-campaign-record base plan))
  (define current-wave
    (and current
         (for/first ([w (in-list (campaign-record-waves current))]
                     #:when (= wave (campaign-wave-index w)))
           w)))
  (define current-attempt (and current-wave (campaign-wave-current-attempt current-wave)))
  (unless (and current
               current-attempt
               (not (campaign-record-cancellation current))
               (equal? (campaign-fence-token current) fence)
               (equal? (campaign-attempt-fence-token current-attempt) fence)
               (equal? (campaign-attempt-id current-attempt) attempt-id)
               (or (not capture?) (eq? (campaign-wave-status current-wave) 'verifying)))
    (refuse "supplemental attempt is not the actual current uncancelled campaign attempt"))
  (define refs (refs-only contract))
  (for ([approval-key '(task-approval candidate-approval)]
        [proposal-key '(task-proposal candidate-proposal)])
    (unless (validate-supplemental-approval (hash-ref refs approval-key) (hash-ref refs proposal-key))
      (refuse "missing, forged or revoked private operator approval")))
  (define descriptor
    (hash-set* refs
               'task-record
               (load-supplemental-record (hash-ref refs 'task-proposal))
               'candidate-record
               (load-supplemental-record (hash-ref refs 'candidate-proposal))))
  (define-values (_ task candidate) (descriptor! descriptor frozen before attempt-id fence cwd))
  (define context (hash-ref task 'context))
  (unless (and (equal? plan (hash-ref context 'plan-id)) (= wave (hash-ref context 'wave)))
    (refuse "task belongs to another campaign/wave"))
  ;; Proof readback can run under authority (staging/probe admission). Never
  ;; acquire the operation lock or reconcile WAL here: journal writers take
  ;; operation -> authority. This pure read authenticates the same snapshot
  ;; and refuses pending intent; the controller must reconcile it first.
  (define operation (inspect-supplemental-entry-operation (hash-ref refs 'task-approval)))
  (unless operation
    (refuse "no protected current operation"))
  (define facts (hash-ref operation 'facts))
  (define allocation (hash-ref facts 'allocation #f))
  (define job (hash-ref facts 'verify-job #f))
  (define command (hash-ref task 'effective-command))
  (unless (hash? job)
    (refuse "protected verification intent missing"))
  (define jobs (hash-ref job 'operational-jobs #f))
  (define command-sha
    (sha256-string (string-join (list "/bin/sh" "-c" (apply-verify-jobs command jobs)) "\u0")))
  (unless (and
           (member (hash-ref operation 'state)
                   (if capture?
                       '("verifying")
                       '("verifying" "green-captured"
                                     "probing"
                                     "probe-captured"
                                     "supersession-approved"
                                     "published"
                                     "receipt-certified"
                                     "registered"
                                     "completed")))
           (< (current-seconds) (hash-ref operation 'deadline-at))
           (equal? (hash-ref operation 'proposal-ref) (hash-ref refs 'task-proposal))
           (equal?
            (hash-ref (hash-ref operation 'base-binding) 'context (hash-ref operation 'base-binding))
            context)
           (equal? (hash-ref facts 'candidate-ref #f) (hash-ref refs 'candidate-proposal))
           (equal? (hash-ref facts 'candidate-approval-ref #f) (hash-ref refs 'candidate-approval))
           (hash? allocation)
           (equal? (hash-ref allocation 'attempt-id #f) attempt-id)
           (equal? (hash-ref allocation 'attempt-fence #f) fence)
           (equal? (hash-ref allocation 'coordinator-fence #f) fence)
           (hash? job)
           (equal? (hash-ref job 'execution-cwd #f) cwd)
           (equal? (hash-ref job 'command-sha256 #f) command-sha))
    (refuse "protected operation/allocation/verification intent mismatch"))
  ;; Authenticate old E against the retained receipt, not merely T's supplied E.
  ;; Dynamic loading avoids a static cycle through delivery-journal/review-proof.
  (define journal ((dynamic-require journal-module 'load-delivery-journal) base plan wave))
  (unless journal
    (refuse "missing old receipt journal"))
  (define history (hash-ref journal 'receipt-history '()))
  (define receipts (append history (list (hash-ref journal 'receipt))))
  (define generation (hash-ref context 'receipt-generation))
  (unless (< generation (length receipts))
    (refuse "old receipt generation absent"))
  (define old (list-ref receipts generation))
  (unless (and (equal? (canonical-evidence-digest old) (hash-ref context 'receipt-sha256))
               (for/and ([key '(branch origin head tree attempt-id attempt-fence)])
                 (equal? (hash-ref old key) (hash-ref context key)))
               (equal? (hash-ref (hash-ref old 'owned-proof) 'sha256)
                       (hash-ref context 'green-proof-sha256))
               (valid-legacy-owned-proof-reference? (hash-ref old 'owned-proof))
               (or (not capture?) (= generation (length history))))
    (refuse "old receipt context drift"))
  (define old-manifest ((dynamic-require proof-module 'load-receipt-owned-proof) old base plan wave))
  (unless (equal? (hash-ref old-manifest 'execution-cwd) (hash-ref context 'repo))
    (refuse "old context execution checkout mismatch"))
  (actual-source! candidate task cwd before)
  ;; Git/proof reads can wait. Recheck protected admission and operator authority
  ;; after those waits, immediately before handing C1 back to the owned runner.
  (for ([approval-key '(task-approval candidate-approval)]
        [proposal-key '(task-proposal candidate-proposal)])
    (unless (validate-supplemental-approval (hash-ref refs approval-key) (hash-ref refs proposal-key))
      (refuse "supplemental authority changed during validation")))
  (unless (and (equal? operation (inspect-supplemental-entry-operation (hash-ref refs 'task-approval)))
               (< (current-seconds) (hash-ref operation 'deadline-at))
               (equal? current (load-campaign-record base plan)))
    (refuse "supplemental operation/campaign changed during validation"))
  (hash-set* frozen
             'effective-normalized-command
             command
             'supplemental-contract
             descriptor
             'operational-jobs
             jobs))
(define (supplemental-proof-offline-facts manifest ref frozen)
  (unless (and (supplemental-owned-proof? manifest)
               (valid-owned-proof-manifest? manifest)
               (valid-owned-proof-reference? ref)
               (equal? (hash-ref ref 'binding) (hash-ref manifest 'binding))
               (equal? (hash-ref ref 'supplemental-contract)
                       (hash-ref manifest 'supplemental-contract)))
    (refuse "incomplete supplemental envelope"))
  (define binding (hash-ref manifest 'binding))
  (define-values (refs task candidate)
    (descriptor! (hash-ref manifest 'supplemental-contract)
                 frozen
                 (hash-ref manifest 'before)
                 (hash-ref binding 'attempt-id)
                 (hash-ref binding 'attempt-fence)
                 (hash-ref manifest 'execution-cwd)))
  (unless (and (equal? (hash-ref manifest 'frozen-normalized-command) (hash-ref task 'base-command))
               (equal? (hash-ref manifest 'effective-normalized-command)
                       (hash-ref task 'effective-command))
               (equal? (hash-ref binding 'snapshot-sha256) (hash-ref frozen 'snapshot-sha256))
               (equal? (hash-ref binding 'plan-id) (hash-ref (hash-ref task 'context) 'plan-id))
               (equal? (hash-ref binding 'wave) (hash-ref (hash-ref task 'context) 'wave)))
    (refuse "effective proof contract mismatch"))
  (supplemental-proof-facts refs
                            (hash-ref task 'base-command)
                            (hash-ref task 'effective-command)
                            (hash-ref candidate 'source)
                            (hash-ref binding 'attempt-id)
                            (hash-ref binding 'attempt-fence)))
(define (validate-supplemental-proof-authority! manifest ref frozen base plan wave)
  (supplemental-proof-offline-facts manifest ref frozen)
  (define b (hash-ref manifest 'binding))
  (define resolved
    (resolve-supplemental-verify-binding (hash-ref manifest 'supplemental-contract)
                                         frozen
                                         base
                                         plan
                                         wave
                                         (hash-ref manifest 'before)
                                         (hash-ref b 'attempt-id)
                                         (hash-ref b 'attempt-fence)
                                         (hash-ref manifest 'execution-cwd)
                                         #:capture? #f))
  (unless (and (equal? (hash-ref resolved 'supplemental-contract)
                       (hash-ref manifest 'supplemental-contract))
               (equal? (hash-ref resolved 'operational-jobs) (hash-ref manifest 'operational-jobs #f))
               (equal? (hash-ref manifest 'execution-base)
                       (path->string (simplify-path (path->complete-path base)))))
    (refuse "private/public supplemental identity/operation mismatch"))
  (define operation
    (inspect-supplemental-entry-operation (hash-ref (hash-ref resolved 'supplemental-contract)
                                                  'task-approval)))
  (unless (and (equal? (hash-ref manifest 'command-sha256)
                       (hash-ref (hash-ref (hash-ref operation 'facts) 'verify-job) 'command-sha256))
               (or (equal? (hash-ref operation 'state) "verifying")
                   (equal? (hash-ref (hash-ref operation 'facts) 'green-proof-ref #f) ref)))
    (refuse "operation retained a different GREEN proof"))
  (void))
