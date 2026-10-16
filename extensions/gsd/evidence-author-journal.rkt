#lang racket/base
;; evidence-author-journal.rkt — the mutation API of the independent
;; evidence-author allowance's permanent one-per-wave journal slot.
;;
;; Extracted from delivery-journal.rkt (module-size law: extract instead
;; of growing past the ceiling). Marker LOADING/validation stays with the
;; journal (load-time integrity needs it); this module owns ONLY the
;; guarded transitions:
;;   - reserve-evidence-author-repair!  (default-one durable reservation)
;;   - start-evidence-author-repair!    (at-most-once durable launch)
;;   - reconcile-evidence-author-receipt! (proof-bearing supersession,
;;     requires strict git ancestry at this shared boundary)
;; No rejection counter is spent, no REQUEST_CHANGES is manufactured, and
;; the completed marker is retained forever (one allowance per wave).
(require racket/list
         racket/path
         (only-in "delivery-journal.rkt"
                  load-delivery-journal
                  save!
                  journal-evidence-author-repair
                  evidence-author-repair-pending?
                  review-rejection-allowed-stages
                  journal-review-rejection
                  review-rejection-matches-receipt?
                  context-receipt?
                  literal-files?
                  valid-delivery-receipt?
                  text?
                  hex?)
         "review-proof.rkt"
         "author-scope.rkt"
         "evidence-repair-format.rkt"
         "composition-root.rkt")
(provide reserve-evidence-author-repair!
         start-evidence-author-repair!
         reconcile-evidence-author-receipt!)

(define (validate-author-context! root plan wave context receipt files)
  (define manifest (load-receipt-owned-proof receipt root plan wave))
  (unless (and manifest
               (equal? files (frozen-author-files root plan wave))
               (equal? (simplify-path (path->directory-path (hash-ref context 'repo)))
                       (simplify-path (path->directory-path (hash-ref manifest 'execution-cwd))))
               (equal? (hash-ref context 'snapshot-sha256)
                       (hash-ref (frozen-verify-binding root plan wave) 'snapshot-sha256)))
    (error 'delivery-journal "evidence author context differs from owned execution or frozen scope")))
(define (author-guard! guard)
  (unless (and (procedure? guard) (procedure-arity-includes? guard 0) (guard))
    (error 'delivery-journal "evidence author live guard refused")))
(define (author-save! root plan wave old next guard)
  (author-guard! guard)
  (unless (equal? old (load-delivery-journal root plan wave))
    (error 'delivery-journal "evidence author journal raced"))
  (save! root plan wave next))
(define (reserve-evidence-author-repair! root plan wave context frozen-files #:guard guard)
  (author-guard! guard)
  (define j (load-delivery-journal root plan wave))
  (unless (and j
               (member (hash-ref j 'stage) review-rejection-allowed-stages)
               (literal-files? frozen-files)
               (not (let ([rejected (journal-review-rejection j)])
                      (and rejected
                           (review-rejection-matches-receipt? rejected (hash-ref j 'receipt))
                           (not (hash-ref rejected 'superseded-by-attempt #f)))))
               (context-receipt? context
                                 (hash-ref j 'receipt)
                                 plan
                                 wave
                                 (length (hash-ref j 'receipt-history '()))))
    (error 'delivery-journal "invalid evidence author reservation"))
  (validate-author-context! root plan wave context (hash-ref j 'receipt) frozen-files)
  (define m
    (hasheq 'schema-version
            1
            'state
            "pending"
            'context
            context
            'frozen-files
            frozen-files
            'old-receipt
            (hash-ref j 'receipt)))
  (define prior (journal-evidence-author-repair j))
  (cond
    [(equal? prior m) j]
    [prior (error 'delivery-journal "evidence author allowance exhausted")]
    [else (author-save! root plan wave j (hash-set j 'evidence-author-repair m) guard)]))
(define (start-evidence-author-repair! root plan wave attempt fence #:guard guard)
  (author-guard! guard)
  (define j (load-delivery-journal root plan wave))
  (define m (journal-evidence-author-repair j))
  (unless (and (evidence-author-repair-pending? j)
               (member (hash-ref j 'stage) review-rejection-allowed-stages)
               (text? attempt)
               (exact-nonnegative-integer? fence)
               (not (equal? attempt (hash-ref (hash-ref m 'context) 'attempt-id)))
               (>= fence (hash-ref (hash-ref m 'context) 'attempt-fence))
               (or (equal? (hash-ref m 'state) "pending")
                   (and (equal? attempt (hash-ref m 'successor-attempt))
                        (equal? fence (hash-ref m 'successor-fence)))))
    (error 'delivery-journal "evidence author start refused"))
  (validate-author-context! root
                            plan
                            wave
                            (hash-ref m 'context)
                            (hash-ref m 'old-receipt)
                            (hash-ref m 'frozen-files))
  (author-save!
   root
   plan
   wave
   j
   (hash-set j
             'evidence-author-repair
             (hash-set* m 'state "running" 'successor-attempt attempt 'successor-fence fence))
   guard))
(define (reconcile-evidence-author-receipt! root
                                            plan
                                            wave
                                            receipt
                                            #:guard guard
                                            #:head-ancestor? head-ancestor?)
  (author-guard! guard)
  (define j (load-delivery-journal root plan wave))
  (define m (journal-evidence-author-repair j))
  (unless (and (evidence-author-repair-pending? j)
               (equal? (hash-ref m 'state) "running")
               (valid-delivery-receipt? receipt)
               (member (hash-ref j 'stage) review-rejection-allowed-stages)
               (equal? (hash-ref receipt 'attempt-id #f) (hash-ref m 'successor-attempt))
               (equal? (hash-ref receipt 'attempt-fence #f) (hash-ref m 'successor-fence))
               (for/and ([k '(repo branch origin)])
                 (equal? (hash-ref receipt k) (hash-ref (hash-ref m 'old-receipt) k)))
               (for/and ([k '(head tree)])
                 (not (equal? (hash-ref receipt k) (hash-ref (hash-ref m 'old-receipt) k))))
               ;; Real-git ancestry at the SHARED supersession boundary: the
               ;; successor must strictly descend from the exact old receipt
               ;; head. Publication's own check does not cover the direct
               ;; delivery-receipt path, so the predicate is mandatory here
               ;; (never a trust-me default).
               (head-ancestor? (hash-ref (hash-ref m 'old-receipt) 'head) (hash-ref receipt 'head)))
    (error 'delivery-journal "evidence author supersession refused"))
  (validate-author-context! root
                            plan
                            wave
                            (hash-ref m 'context)
                            (hash-ref m 'old-receipt)
                            (hash-ref m 'frozen-files))
  (unless (load-receipt-owned-proof receipt root plan wave)
    (error 'delivery-journal "evidence author successor lacks full owned Verify"))
  ;; Whitelist generation-independent accounting; no ACK/review/usage bindings survive.
  (define fresh
    (for/fold ([h (hasheq 'schema-version
                          1
                          'plan-id
                          plan
                          'wave
                          wave
                          'stage
                          "context-ready"
                          'receipt
                          receipt
                          'receipt-history
                          (append (hash-ref j 'receipt-history '()) (list (hash-ref j 'receipt)))
                          'model-calls
                          0
                          'tokens
                          0
                          'cost
                          0
                          'usage-missing
                          #f
                          'evidence-author-repair
                          (hash-set* m
                                     'state
                                     "completed"
                                     'successor-receipt-sha256
                                     (canonical-evidence-digest receipt)))])
              ([k '(review-rejection review-rejection-history review-rejection-count)]
               #:when (hash-has-key? j k))
      (hash-set h k (hash-ref j k))))
  (author-save! root plan wave j fresh guard))
