#lang racket/base
;; Coordinator-owned publication of an approved implementation head.
;;
;; This module owns the durable approval/publication intent and exact remote
;; readback confirmation.  It never treats legacy remote-pending markers or a
;; cached confirmed record as authority: every replay performs an authenticated
;; current readback through the injected/default publisher before a receipt can
;; be certified.
(require json
         racket/file
         racket/list
         racket/path
         racket/string
         racket/runtime-path
         (only-in "../../util/json/checksum.rkt" sha256-string)
         (only-in "delivery-handoff.rkt" controller-environment redact-delivery-text)
         (only-in "campaign-state.rkt"
                  campaign-record?
                  campaign-plan-id
                  campaign-record-waves
                  campaign-record-cancellation
                  campaign-fence-token
                  campaign-record-plan-snapshot-digest
                  campaign-wave-index
                  campaign-wave-current-attempt
                  campaign-wave-delivery-branch
                  campaign-wave-delivery-head-sha
                  campaign-attempt-id
                  campaign-attempt-fence-token)
         (only-in "delivery-receipt.rkt" certify-publication-receipt!)
         (only-in "attempt-artifacts.rkt" record-delivered-branches)
         (only-in "../../sandbox/subprocess.rkt"
                  run-subprocess
                  subprocess-result-stdout
                  subprocess-result-stderr
                  subprocess-result-exit-code
                  subprocess-result-timed-out?
                  subprocess-result-truncated?))

(provide approved-publication-path
         approved-publication-branches
         durable-spare-branches
         load-approved-publication
         replay-approved-publication!
         current-gsd-approved-head-publisher
         default-approved-head-publisher
         publish-approved-head!)

(define-runtime-path default-delivery-controller-script "../../scripts/gsd-delivery.py")

(define (hex? value n)
  (and (string? value) (= (string-length value) n) (regexp-match? #px"^[0-9a-f]+$" value)))
(define (text? value)
  (and (string? value) (positive? (string-length value))))
(define (non-empty-string? value)
  (text? value))

(define (approved-publication-path root plan wave)
  (unless (and (hex? plan 64) (exact-nonnegative-integer? wave))
    (error 'delivery-publication "invalid campaign/wave identity"))
  (build-path (path->complete-path root)
              ".planning"
              "campaigns"
              plan
              (format "coordinator-w~a.publication.json" wave)))

(define (bounded-redacted-text value)
  (define text (redact-delivery-text (format "~a" value)))
  (substring text 0 (min 8192 (string-length text))))

(define (valid-receipt-candidate? data)
  (and (hash? data)
       (andmap (lambda (k) (text? (hash-ref data k #f))) '(repo branch origin evidence attempt-id))
       (hex? (hash-ref data 'head #f) 40)
       (hex? (hash-ref data 'tree #f) 40)
       (exact-nonnegative-integer? (hash-ref data 'attempt-fence #f))
       (exact-nonnegative-integer? (hash-ref data 'verified-at #f))))

;; BUG-0079 REVIEW-2 item 3: the durable receipt-candidate must be
;; cross-validated against the top-level publication identity — repo,
;; origin, branch, head, tree, attempt binding and the evidence digest —
;; never merely type-checked. Any inconsistency refuses the record.
(define (consistent-receipt-candidate? data)
  (define candidate (hash-ref data 'receipt-candidate #f))
  (and (valid-receipt-candidate? candidate)
       (for/and ([key (in-list '(repo origin branch head tree attempt-id attempt-fence))])
         (equal? (hash-ref candidate key #f) (hash-ref data key #f)))
       (equal? (sha256-string (hash-ref candidate 'evidence #f))
               (hash-ref data 'evidence-digest #f))))

(define (valid-publication? data)
  (and (hash? data)
       (equal? (hash-ref data 'schema-version #f) 1)
       (hex? (hash-ref data 'plan-id #f) 64)
       (exact-nonnegative-integer? (hash-ref data 'wave #f))
       (member (hash-ref data 'status #f) '("intent" "confirmed"))
       (text? (hash-ref data 'repo #f))
       (text? (hash-ref data 'origin #f))
       (text? (hash-ref data 'branch #f))
       (not (member (hash-ref data 'branch #f) '("main" "master")))
       (hex? (hash-ref data 'head #f) 40)
       (hex? (hash-ref data 'tree #f) 40)
       (text? (hash-ref data 'attempt-id #f))
       (exact-nonnegative-integer? (hash-ref data 'attempt-fence #f))
       (or (not (hash-ref data 'coordinator-fence #f))
           (exact-nonnegative-integer? (hash-ref data 'coordinator-fence #f)))
       (hex? (hash-ref data 'snapshot-digest #f) 64)
       (hex? (hash-ref data 'evidence-digest #f) 64)
       (consistent-receipt-candidate? data)
       (let ([history (hash-ref data 'publication-history '())])
         (and (list? history) (andmap hash? history)))
       (exact-nonnegative-integer? (hash-ref data 'recorded-at #f))
       (or (not (hash-ref data 'confirmed-at #f))
           (exact-nonnegative-integer? (hash-ref data 'confirmed-at #f)))))

;; BUG-0079 REVIEW-2 item 3: an EXISTING publication file is never treated as
;; absent. Symlinked reads are refused, and a corrupt/inconsistent record
;; RAISES instead of returning #f so callers can never overwrite it with a
;; fresh intent. Only a genuinely missing file loads as #f.
(define (load-approved-publication root plan wave)
  (define path (approved-publication-path root plan wave))
  (when (link-exists? path)
    (error 'delivery-publication "symlinked publication record refused"))
  (and (file-exists? path)
       (let* ([data (with-handlers ([exn:fail? (lambda (_) #f)])
                      (call-with-input-file path
                                            (lambda (in)
                                              (define v (read-json in))
                                              (and (not (eof-object? v)) v))))]
              [mismatch? (and (hash? data)
                              (or (not (equal? (hash-ref data 'plan-id #f) plan))
                                  (not (equal? (hash-ref data 'wave #f) wave))))])
         (unless (valid-publication? data)
           (error 'delivery-publication
                  "existing publication record is corrupt or invalid; refusing to load or overwrite"))
         (when mismatch?
           (error 'delivery-publication
                  "existing publication record identity is inconsistent; refusing"))
         data)))

(define (save! root plan wave data)
  (unless (valid-publication? data)
    (error 'delivery-publication "invalid publication record"))
  (define path (approved-publication-path root plan wave))
  (when (link-exists? path)
    (error 'delivery-publication "symlinked publication record refused"))
  (make-parent-directory* path)
  (call-with-atomic-output-file path
                                (lambda (out _)
                                  (write-json data out)
                                  (newline out)))
  data)

(define (find-wave rec wave)
  (and (campaign-record? rec)
       (for/first ([w (in-list (campaign-record-waves rec))]
                   #:when (= (campaign-wave-index w) wave))
         w)))

(define (capture-active-authorization! plan
                                       wave
                                       snapshot
                                       attempt-id
                                       attempt-fence
                                       read-current
                                       phase
                                       [expected-active-fence #f]
                                       [expected-digest #f])
  (unless (and (procedure? read-current) (procedure-arity-includes? read-current 0))
    (error 'delivery-publication "authoritative current-record callback is required"))
  (define rec (read-current))
  (define w (find-wave rec wave))
  (define attempt (and w (campaign-wave-current-attempt w)))
  (define active-fence (and (campaign-record? rec) (campaign-fence-token rec)))
  (define digest (and (campaign-record? rec) (campaign-record-plan-snapshot-digest rec)))
  ;; BUG-0079 REVIEW-2 item 1: EVERY boundary re-verifies the SAME captured
  ;; active coordinator fence AND frozen plan-snapshot digest — drift during
  ;; the effect window refuses instead of authorizing a takeover.
  (unless (and (campaign-record? rec)
               (equal? (campaign-plan-id rec) plan)
               (not (campaign-record-cancellation rec))
               (exact-nonnegative-integer? active-fence)
               (or (not expected-active-fence) (equal? active-fence expected-active-fence))
               (or (not expected-digest) (equal? digest expected-digest))
               w
               attempt
               (equal? (campaign-attempt-id attempt) attempt-id)
               (equal? (campaign-attempt-fence-token attempt) attempt-fence)
               (hex? digest 64))
    (error 'delivery-publication
           (format "publication authorization refused at ~a: stale, cancelled, or unbound attempt"
                   phase)))
  (define bound-branch (campaign-wave-delivery-branch w))
  (define bound-head (campaign-wave-delivery-head-sha w))
  (when (and (non-empty-string? bound-branch)
             (not (equal? bound-branch (hash-ref snapshot 'branch #f))))
    (error 'delivery-publication "publication authorization refused: branch binding drifted"))
  (when (and (non-empty-string? bound-head) (not (equal? bound-head (hash-ref snapshot 'head #f))))
    ;; BUG-0079 REVIEW-2 item 5: a fresh approval under the SAME attempt and
    ;; branch whose head is a PROVEN strict descendant of the durably bound
    ;; head is the legitimate same-attempt repair-tail transition (the
    ;; durable record still binds the older receipt head); anything else is
    ;; drift. Ancestry is proven with real git in the snapshot's repository
    ;; and fails closed.
    (unless (and (non-empty-string? bound-branch)
                 (equal? bound-branch (hash-ref snapshot 'branch #f))
                 (strict-descendant? (hash-ref snapshot 'repo #f)
                                     bound-head
                                     (hash-ref snapshot 'head #f)))
      (error 'delivery-publication "publication authorization refused: head binding drifted")))
  (values active-fence digest))

(define (receipt-candidate snapshot attempt-id attempt-fence evidence)
  (hash-set* snapshot
             'attempt-id
             attempt-id
             'attempt-fence
             attempt-fence
             'verified-at
             (current-seconds)
             'evidence
             (bounded-redacted-text evidence)))

(define (intent-record plan
                       wave
                       snapshot
                       attempt-id
                       attempt-fence
                       active-fence
                       snapshot-digest
                       evidence)
  (define safe-evidence (bounded-redacted-text evidence))
  (hasheq 'schema-version
          1
          'plan-id
          plan
          'wave
          wave
          'status
          "intent"
          'repo
          (hash-ref snapshot 'repo)
          'origin
          (hash-ref snapshot 'origin)
          'branch
          (hash-ref snapshot 'branch)
          'head
          (hash-ref snapshot 'head)
          'tree
          (hash-ref snapshot 'tree)
          'attempt-id
          attempt-id
          'attempt-fence
          attempt-fence
          'coordinator-fence
          active-fence
          'snapshot-digest
          snapshot-digest
          'evidence-digest
          (sha256-string safe-evidence)
          'receipt-candidate
          (receipt-candidate snapshot attempt-id attempt-fence safe-evidence)
          'publication-history
          '()
          'recorded-at
          (current-seconds)))

(define exact-identity-keys
  '(plan-id wave
            repo
            origin
            branch
            head
            tree
            attempt-id
            attempt-fence
            snapshot-digest
            evidence-digest))
(define repair-generation-keys
  '(plan-id wave repo origin branch attempt-id attempt-fence snapshot-digest))

(define (same-keys? a b keys)
  (and (hash? a)
       (hash? b)
       (for/and ([key (in-list keys)])
         (equal? (hash-ref a key #f) (hash-ref b key #f)))))

(define (same-publication-identity? record intent)
  (same-keys? record intent exact-identity-keys))

(define (git root . args)
  (define r
    (run-subprocess "git"
                    #:args (append (list "-C" (path->string (path->complete-path root))) args)
                    #:directory root
                    #:timeout 15))
  (and (not (subprocess-result-timed-out? r))
       (not (subprocess-result-truncated? r))
       (equal? (subprocess-result-exit-code r) 0)
       (string-trim (subprocess-result-stdout r))))

(define (strict-descendant? repo old-head new-head)
  (and (text? repo)
       (hex? old-head 40)
       (hex? new-head 40)
       (not (string=? old-head new-head))
       (with-handlers ([exn:fail? (lambda (_) #f)])
         (and (git repo "merge-base" "--is-ancestor" old-head new-head) #t))))

(define (repair-tail-publication? prior intent)
  (and (same-keys? prior intent repair-generation-keys)
       (strict-descendant? (hash-ref intent 'repo #f)
                           (hash-ref prior 'head #f)
                           (hash-ref intent 'head #f))))

(define (with-publication-history intent prior)
  (hash-set
   intent
   'publication-history
   (append
    (hash-ref prior 'publication-history '())
    (list
     (for/hasheq ([key (in-list '(status branch head tree evidence-digest recorded-at confirmed-at))]
                  #:when (hash-has-key? prior key))
       (values key (hash-ref prior key)))))))

(define (parse-controller-result stdout)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define data (string->jsexpr stdout))
    (and (hash? data) data)))

(define (default-approved-head-publisher base
                                         plan
                                         wave
                                         snapshot
                                         attempt-id
                                         attempt-fence
                                         coordinator-fence
                                         evidence)
  (define result
    (run-subprocess "python3"
                    #:args (list (path->string default-delivery-controller-script)
                                 "publish-head"
                                 "--repo"
                                 (hash-ref snapshot 'repo)
                                 "--plan"
                                 plan
                                 "--wave"
                                 (number->string wave)
                                 "--expected-branch"
                                 (hash-ref snapshot 'branch)
                                 "--expected-head"
                                 (hash-ref snapshot 'head)
                                 "--expected-origin"
                                 (hash-ref snapshot 'origin)
                                 "--expected-tree"
                                 (hash-ref snapshot 'tree))
                    #:directory base
                    #:environment (controller-environment)
                    #:timeout 240
                    #:process-group? #t))
  (cond
    [(or (subprocess-result-timed-out? result)
         (subprocess-result-truncated? result)
         (not (zero? (subprocess-result-exit-code result))))
     (define parsed (parse-controller-result (subprocess-result-stdout result)))
     (hasheq 'status
             "blocked"
             'reason
             (redact-delivery-text (format "~a"
                                           (or (and parsed (hash-ref parsed 'reason #f))
                                               (subprocess-result-stderr result)
                                               "publication controller refused"))))]
    [else
     (or (parse-controller-result (subprocess-result-stdout result))
         (hasheq 'status "blocked" 'reason "publication controller returned malformed JSON"))]))

(define current-gsd-approved-head-publisher (make-parameter default-approved-head-publisher))

(define (confirmed-record intent)
  (hash-set* intent 'status "confirmed" 'confirmed-at (current-seconds)))

(define (save-intent-for-transition! base plan wave prior intent)
  (cond
    [(not prior) (save! base plan wave intent)]
    [(same-publication-identity? prior intent) prior]
    [(repair-tail-publication? prior intent)
     (save! base plan wave (with-publication-history intent prior))]
    [else (error 'delivery-publication "publication identity changed; refusing replay")]))

(define (publish-approved-head! base
                                plan
                                wave
                                snapshot
                                attempt-id
                                attempt-fence
                                coordinator-fence
                                evidence
                                [read-current #f])
  (unless (and (hash? snapshot)
               (text? (hash-ref snapshot 'repo #f))
               (text? (hash-ref snapshot 'origin #f))
               (text? (hash-ref snapshot 'branch #f))
               (not (member (hash-ref snapshot 'branch #f) '("main" "master")))
               (hex? (hash-ref snapshot 'head #f) 40)
               (hex? (hash-ref snapshot 'tree #f) 40)
               (text? attempt-id)
               (exact-nonnegative-integer? attempt-fence)
               ;; BUG-0079 REVIEW-2 item 8: an empty/bool-only approval text
               ;; is never genuine successful Verify evidence.
               (text? evidence))
    (error 'delivery-publication "approved publication identity is incomplete"))
  (define-values (active-fence snapshot-digest)
    (capture-active-authorization! plan
                                   wave
                                   snapshot
                                   attempt-id
                                   attempt-fence
                                   read-current
                                   'before-intent))
  ;; BUG-0079 REVIEW-2 item 1: the caller's passed coordinator fence must
  ;; match the fresh authoritative capture.
  (when (and coordinator-fence (not (equal? coordinator-fence active-fence)))
    (error
     'delivery-publication
     "publication authorization refused: caller coordinator fence does not match the active capture"))
  ;; BUG-0079 REVIEW-2 items 1+2: ONE transition guard closing over the
  ;; active fence AND the frozen plan digest captured at before-intent,
  ;; re-verified at every boundary (before-push, after-effect, and
  ;; immediately before the durable certification write).
  (define (guard! phase)
    (capture-active-authorization! plan
                                   wave
                                   snapshot
                                   attempt-id
                                   attempt-fence
                                   read-current
                                   phase
                                   active-fence
                                   snapshot-digest))
  (define intent
    (intent-record plan wave snapshot attempt-id attempt-fence active-fence snapshot-digest evidence))
  (define prior (load-approved-publication base plan wave))
  (define active-intent (save-intent-for-transition! base plan wave prior intent))
  (guard! 'before-push)
  (define outcome
    ((current-gsd-approved-head-publisher) base
                                           plan
                                           wave
                                           snapshot
                                           attempt-id
                                           attempt-fence
                                           active-fence
                                           evidence))
  (guard! 'after-effect)
  (define status (and (hash? outcome) (hash-ref outcome 'status #f)))
  (cond
    [(and (member status '("published" "already-published" "confirmed"))
          (equal? (hash-ref outcome 'branch #f) (hash-ref snapshot 'branch))
          (equal? (hash-ref outcome 'head #f) (hash-ref snapshot 'head)))
     (guard! 'before-certification)
     (save! base
            plan
            wave
            (confirmed-record
             (hash-set (if (same-publication-identity? active-intent intent) active-intent intent)
                       'coordinator-fence
                       active-fence)))]
    [else
     (define reason (and (hash? outcome) (hash-ref outcome 'reason #f)))
     (error 'delivery-publication
            (format "publication refused: ~a" (or reason "exact remote readback missing")))]))

;; BUG-0079 REVIEW-2 item 2: the publication-only replay is ONE transition
;; guarded by a single stable active-fence+digest capture. The initial
;; capture binds to the durable intent's frozen coordinator fence and
;; snapshot digest (a takeover since the intent was recorded refuses), and
;; the SAME guard is re-invoked immediately before the final receipt
;; certification writes — the final certification can never re-capture an
;; unconstrained fresh fence after the publisher's external effect. The
;; replay targets the exact expected approved head recorded in the intent.
(define (replay-approved-publication! base plan wave read-current)
  (define prior (load-approved-publication base plan wave))
  (and prior
       (let* ([candidate (hash-ref prior 'receipt-candidate #f)]
              [snapshot (for/hasheq ([key (in-list '(repo origin branch head tree))])
                          (values key (hash-ref prior key)))]
              [attempt-id (hash-ref prior 'attempt-id)]
              [attempt-fence (hash-ref prior 'attempt-fence)]
              [evidence (hash-ref candidate 'evidence)]
              [prior-fence (hash-ref prior 'coordinator-fence #f)]
              [prior-digest (hash-ref prior 'snapshot-digest #f)])
         (define-values (active-fence digest)
           (capture-active-authorization! plan
                                          wave
                                          snapshot
                                          attempt-id
                                          attempt-fence
                                          read-current
                                          'replay-intent
                                          prior-fence
                                          prior-digest))
         (define (guard! phase)
           (capture-active-authorization! plan
                                          wave
                                          snapshot
                                          attempt-id
                                          attempt-fence
                                          read-current
                                          phase
                                          active-fence
                                          digest))
         (publish-approved-head! base
                                 plan
                                 wave
                                 snapshot
                                 attempt-id
                                 attempt-fence
                                 active-fence
                                 evidence
                                 read-current)
         (guard! 'before-receipt-certification)
         (certify-publication-receipt! base
                                       plan
                                       wave
                                       candidate
                                       #:expected-attempt-id attempt-id
                                       #:expected-fence attempt-fence
                                       #:guard (lambda () (guard! 'receipt-certification-write))))))

;; BUG-0079 REVIEW-2 item 9 (fail-closed branch protection): enumerating the
;; approved publication branches must never raise and never silently drop an
;; unloadable record. A corrupt/inconsistent/symlinked publication file still
;; names a branch whose retained checkout must survive campaign-start
;; reclaim — reading its raw top-level branch protects it instead of letting
;; a broken file turn durable approved evidence into reclaimable garbage.
(define (raw-publication-branch root plan wave)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define path (approved-publication-path root plan wave))
    (and (file-exists? path)
         (not (link-exists? path))
         (let ([data (call-with-input-file path
                                           (lambda (in)
                                             (define v (read-json in))
                                             (and (not (eof-object? v)) v)))])
           (and (hash? data)
                (equal? (hash-ref data 'plan-id #f) plan)
                (equal? (hash-ref data 'wave #f) wave)
                (text? (hash-ref data 'branch #f))
                (hash-ref data 'branch))))))

(define (approved-publication-branches root plan)
  (define dir (build-path (path->complete-path root) ".planning" "campaigns" plan))
  (if (not (directory-exists? dir))
      '()
      (remove-duplicates
       (for/list ([entry (in-list (directory-list dir))]
                  #:when (regexp-match? #rx"^coordinator-w[0-9]+\\.publication\\.json$"
                                        (path->string entry))
                  #:do [(define m
                          (regexp-match #rx"^coordinator-w([0-9]+)\\.publication\\.json$"
                                        (path->string entry)))
                        (define wave (and m (string->number (cadr m))))
                        (define rec
                          (and wave
                               (with-handlers ([exn:fail? (lambda (_)
                                                            (raw-publication-branch root plan wave))])
                                 (define loaded (load-approved-publication root plan wave))
                                 (and loaded (hash-ref loaded 'branch #f)))))
                        (define branch (or rec (and wave (raw-publication-branch root plan wave))))]
                  #:when (text? branch))
         branch)
       string=?)))

;; BUG-0079 REVIEW-2 item 9: the full crash-recovery spare list for per-wave
;; and campaign-start reclaim — delivered record branches PLUS durably
;; approved publication branches — so reclaim goes AROUND retained approved
;; checkouts, never through them.
(define (durable-spare-branches root rec)
  (remove-duplicates (append (record-delivered-branches rec)
                             (approved-publication-branches root (campaign-plan-id rec)))
                     string=?))
