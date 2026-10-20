#lang racket/base
;; STABILITY: internal
;; BUG-0088 owner frontend. Called ONLY from keyboard-user message dispatch,
;; never the shared extension/model command dispatcher. Transport is not authority:
;; authorization still requires a real broker decision and owner-side consumption.
(require racket/string
         "../context.rkt"
         "../state.rkt"
         "../../util/event/event.rkt"
         "../../util/event/event-bus.rkt"
         (only-in "../../extensions/gsd/evidence-repair-format.rkt" canonical-evidence-json)
         "../../extensions/gsd/supplemental-evidence-authority.rkt"
         (submod "../../extensions/gsd/supplemental-evidence-authority.rkt" owner-frontend)
         "../../extensions/gsd/supplemental-evidence-operator.rkt"
         (only-in "../../extensions/gsd/supplemental-evidence-controller.rkt"
                  load-supplemental-pointer
                  write-supplemental-pointer!))
(provide handle-owner-evidence-command!
         call-with-owner-approval-publisher)

(define (report! ctx text)
  (atomic-state-update! (tui-ctx-ui-state-box ctx)
                        (lambda (state) (add-transcript-entry state (make-system-entry text))))
  (mark-dirty! ctx))

;; Capture the actual UI bus, not a model/session's command payload or publisher.
;; Threads spawned by the user /go inherit this parameter for controller K/S waits.
(define (call-with-owner-approval-publisher ctx thunk)
  (define bus (tui-ctx-event-bus ctx))
  (define publisher
    (and bus
         (lambda (type payload)
           (define sid (ui-state-session-id (unbox (tui-ctx-ui-state-box ctx))))
           (publish! bus (make-event type (current-inexact-milliseconds) (or sid "") #f payload)))))
  (parameterize ([current-supplemental-operator-publisher publisher])
    (thunk)))

(define usage
  (string-append
   "Owner commands (campaign root = q's current project directory):\n"
   "/evidence-replan inspect KIND SHA256\n"
   "/evidence-replan authorize-task PLAN WAVE TASK-SHA256\n"
   "/evidence-replan revoke KIND APPROVAL-SHA256\n"
   "Propose using the installed CLI first. Inspect exact retained bytes; authorization does not run /go.\n"
   "After task approval use /go; the governed controller requests exact candidate and supersession approval in this UI."))

(define (digest? s)
  (and (string? s) (regexp-match? #px"^[0-9a-f]{64}$" s)))
(define (handle-owner-evidence-command! ctx raw)
  (define words (string-split (string-trim raw)))
  (cond
    [(or (null? words) (not (equal? (car words) "/evidence-replan"))) #f]
    [else
     (with-handlers ([exn:fail?
                      (lambda (e)
                        (report! ctx (format "Owner evidence command refused: ~a" (exn-message e))))])
       (define args (cdr words))
       (cond
         [(and (= (length args) 3) (equal? (car args) "inspect") (digest? (caddr args)))
          (define ref
            (hasheq 'kind
                    (string->immutable-string (cadr args))
                    'sha256
                    (string->immutable-string (caddr args))))
          (report! ctx
                   (format "Retained ~a SHA256 ~a\n~a"
                           (cadr args)
                           (caddr args)
                           (canonical-evidence-json (load-supplemental-record ref))))]
         [(and (= (length args) 3)
               (equal? (car args) "revoke")
               (member (cadr args) '("task-approval" "candidate-approval" "supersession-approval"))
               (digest? (caddr args)))
          (when (ui-state-busy? (unbox (tui-ctx-ui-state-box ctx)))
            (error 'owner-evidence "session busy; wait for quiescence"))
          (unless (and (supplemental-operator-channel-available?) (tui-ctx-event-bus ctx))
            (error 'owner-evidence "~a" (supplemental-operator-refusal)))
          (define target
            (hasheq 'kind
                    (string->immutable-string (cadr args))
                    'sha256
                    (string->immutable-string (caddr args))))
          (unless (validate-supplemental-approval target)
            (error 'owner-evidence "revocation requires a live private approval"))
          ;; Retention is inert. Only the exact revocation proposal's genuine
          ;; interactive grant can cross the protected revoke! transition.
          (define ref
            (propose-retain! "revocation-proposal" (hasheq 'reason "owner revocation") (list target)))
          (call-with-owner-approval-publisher
           ctx
           (lambda ()
             (thread
              (lambda ()
                (with-handlers ([exn:fail? (lambda (e)
                                             (report! ctx
                                                      (format "Owner revocation refused: ~a"
                                                              (exn-message e))))])
                  (define grant (request-supplemental-approval! ref))
                  (cond
                    [(not grant)
                     (report! ctx "Owner denied/cancelled revocation; approval unchanged.")]
                    [else
                     (define revoked (revoke! ref grant))
                     (unless revoked
                       (error 'owner-evidence "exact broker grant consumption refused"))
                     (report!
                      ctx
                      (format
                       "Approval durably revoked: ~a ~a. Revocation record SHA256 ~a. No work launched or allowance reset."
                       (hash-ref target 'kind)
                       (hash-ref target 'sha256)
                       (hash-ref revoked 'sha256)))]))))))]
         [(and (= (length args) 4)
               (equal? (car args) "authorize-task")
               (digest? (cadr args))
               (digest? (cadddr args)))
          (define plan (cadr args))
          (define wave (string->number (caddr args)))
          (define expected (cadddr args))
          (define base (current-directory))
          (unless (exact-nonnegative-integer? wave)
            (error 'owner-evidence "invalid wave"))
          (when (ui-state-busy? (unbox (tui-ctx-ui-state-box ctx)))
            (error 'owner-evidence "session busy; wait for quiescence"))
          (unless (and (supplemental-operator-channel-available?) (tui-ctx-event-bus ctx))
            (error 'owner-evidence "~a" (supplemental-operator-refusal)))
          (define pointer (load-supplemental-pointer base plan wave))
          (unless (and pointer (equal? expected (hash-ref (hash-ref pointer 'task-proposal) 'sha256)))
            (error 'owner-evidence "task digest does not match this campaign's retained pointer"))
          (define ref (hash-ref pointer 'task-proposal))
          ;; Never block the keyboard loop while waiting for its human decision.
          (call-with-owner-approval-publisher
           ctx
           (lambda ()
             (thread
              (lambda ()
                (with-handlers ([exn:fail? (lambda (e)
                                             (report! ctx
                                                      (format "Owner authorization refused: ~a"
                                                              (exn-message e))))])
                  (define grant (request-supplemental-approval! ref))
                  (cond
                    [(not grant)
                     (report! ctx
                              "Owner denied/cancelled authorization; no task authority installed.")]
                    [else
                     (define approval (approve-supplemental-proposal! ref grant))
                     (unless approval
                       (error 'owner-evidence "exact broker grant consumption refused"))
                     ;; The pointer writer revalidates private authority and refuses replacement/drift.
                     (write-supplemental-pointer! base plan wave ref approval)
                     (report!
                      ctx
                      "Exact task approved. Use /go to resume the governed controller; no work launched by this command.")]))))))]
         [else (report! ctx usage)]))
     #t]))
