#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @isolation temp-dir
;; @timeout 120
(require rackunit
         racket/file
         (prefix-in journal: "test-gsd-supplemental-evidence-journal.rkt")
         "../extensions/gsd/supplemental-evidence-journal.rkt"
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         "../extensions/gsd/supplemental-evidence-operator.rkt"
         "../runtime/approval/broker.rkt"
         "../tui/state-types.rkt"
         "../tui/state-events/approval-events.rkt"
         "../tui/state-events/registry.rkt"
         (only-in "../tui/tui-keybindings.rkt"
                  make-tui-ctx
                  handle-approval-overlay-key
                  tui-ctx-ui-state-box)
         "../util/event/event.rkt"
         "../util/event/event-bus.rkt"
         "../tui/commands/evidence-owner.rkt"
         (only-in "../tui/context.rkt" tui-ctx-input-state-box)
         (only-in "../tui/input.rkt" input-insert-string)
         (only-in "../tui/message-dispatch.rkt" process-tui-message!)
         (prefix-in shared: "../tui/tui-keybindings.rkt")
         (only-in "../extensions/gsd/supplemental-evidence-controller.rkt"
                  write-supplemental-pointer!
                  load-supplemental-pointer))

(define (with-fixture proc)
  (define home (make-temporary-file "supplemental-frontend-~a" 'directory))
  (define root (build-path home "private"))
  (dynamic-wind (lambda ()
                  (make-directory root #o700)
                  (set-approval-channel! (make-approval-channel #:timeout-ms 1000)))
                (lambda ()
                  (parameterize ([current-supplemental-authority-root root])
                    (proc)))
                (lambda ()
                  (clear-approval-channel!)
                  (delete-directory/files home))))

(module+ test
  (test-case "retained T/K/S reaches real UI selection and returns only exact opaque broker grant"
    (with-fixture
     (lambda ()
       (for ([kind '("task-proposal" "candidate-proposal" "supersession-proposal")])
         (define ref (propose-retain! kind (hasheq 'objective "frontend fixture only")))
         (define bus (make-event-bus))
         (define ctx (make-tui-ctx #:session-runner void #:event-bus bus #:session-queue #f))
         (define shown? #f)
         (subscribe! bus
                     (lambda (evt)
                       (define type (event-ev evt))
                       (define payload (event-payload evt))
                       (define state-box (tui-ctx-ui-state-box ctx))
                       (set-box! state-box
                                 (apply-event-to-state (unbox state-box)
                                                       (make-event type 0 "fixture" #f payload)))
                       (when (regexp-match? #rx"requested$" type)
                         (define overlay (ui-state-active-overlay (unbox state-box)))
                         (check-not-false overlay
                                          "production reducer must present the retained record")
                         (when overlay
                           (set! shown? #t)
                           (define extra (overlay-state-extra overlay))
                           (check-equal? (hash-ref extra 'commitment-digest) (hash-ref ref 'sha256))
                           (define view (hash-ref extra 'approval-view))
                           (check-equal? (hash-ref view 'record-canonical-json)
                                         (hash-ref (supplemental-approval-presentation ref)
                                                   'record-canonical-json))
                           (check-equal? (handle-approval-overlay-key ctx #\y) 'handled)))))
         (define grant
           (call-with-owner-approval-publisher
            ctx
            (lambda ()
              ;; Campaign execution runs in a child thread; its controller waits
              ;; must inherit this exact production publisher without rebinding.
              (define result (make-channel))
              (thread (lambda () (channel-put result (request-supplemental-approval! ref))))
              (sync/timeout 2 result))))
         (check-true shown?)
         (check-true (approval-grant? grant))
         (when grant
           (check-true (call-with-approval-grant grant (hash-ref ref 'sha256) (lambda () #t)))
           (check-false (call-with-approval-grant grant (hash-ref ref 'sha256) (lambda () #t))))))))
  (test-case "deny through the genuine UI returns no grant"
    (with-fixture
     (lambda ()
       (define ref (propose-retain! "candidate-proposal" (hasheq 'objective "deny fixture")))
       (define bus (make-event-bus))
       (define ctx (make-tui-ctx #:session-runner void #:event-bus bus #:session-queue #f))
       (subscribe! bus
                   (lambda (evt)
                     (define sb (tui-ctx-ui-state-box ctx))
                     (set-box! sb (apply-event-to-state (unbox sb) evt))
                     (when (ui-state-active-overlay (unbox sb))
                       (process-tui-message! ctx '(key #\n)))))
       (check-false
        (call-with-owner-approval-publisher ctx (lambda () (request-supplemental-approval! ref))))
       (check-equal? (pending-approval-count) 0))))
  (test-case "keyboard task command installs exact private approval, shared dispatch cannot invoke it"
    (with-fixture
     (lambda ()
       (define project (build-path (current-supplemental-authority-root) 'up "project"))
       (make-directory project)
       (parameterize ([current-directory project])
         (define plan (make-string 64 #\a))
         (define ref (propose-retain! "task-proposal" (hasheq 'objective "keyboard fixture")))
         (write-supplemental-pointer! (current-directory) plan 1 ref)
         (define bus (make-event-bus))
         (define ctx (make-tui-ctx #:session-runner void #:event-bus bus #:session-queue #f))
         (define raw (format "/evidence-replan authorize-task ~a 1 ~a" plan (hash-ref ref 'sha256)))
         (define shown (make-semaphore 0))
         (subscribe! bus
                     (lambda (evt)
                       (define sb (tui-ctx-ui-state-box ctx))
                       (set-box! sb (apply-event-to-state (unbox sb) evt))
                       (when (ui-state-active-overlay (unbox sb))
                         (semaphore-post shown))))
         (handle-owner-evidence-command! ctx
                                         (format "/evidence-replan inspect task-proposal ~a"
                                                 (hash-ref ref 'sha256)))
         (check-true
          (for/or ([entry (in-list (ui-state-transcript (unbox (tui-ctx-ui-state-box ctx))))])
            (regexp-match? (regexp-quote (hash-ref (supplemental-approval-presentation ref)
                                                   'record-canonical-json))
                           (transcript-entry-text entry)))
          (format "transcript: ~s" (ui-state-transcript (unbox (tui-ctx-ui-state-box ctx)))))
         ;; The shared dispatcher (also used for programmatic commands) has no owner provenance.
         (shared:process-slash-command ctx 'unknown raw)
         (check-equal? (pending-approval-count) 0)
         (define ib (tui-ctx-input-state-box ctx))
         (set-box! ib (input-insert-string (unbox ib) raw))
         (process-tui-message! ctx '(key enter))
         (check-not-false (sync/timeout 2 shown))
         (check-equal?
          (hash-ref (overlay-state-extra (ui-state-active-overlay (unbox (tui-ctx-ui-state-box ctx))))
                    'commitment-digest)
          (hash-ref ref 'sha256))
         (process-tui-message! ctx '(key #\y))
         (let wait ([tries 100])
           (define pointer (load-supplemental-pointer (current-directory) plan 1))
           (define approved? (validate-supplemental-approval (hash-ref pointer 'task-approval) ref))
           (cond
             [approved? (check-not-false approved?)]
             [(positive? tries)
              (sleep 0.01)
              (wait (sub1 tries))]
             [else
              (fail
               "keyboard-selected opaque grant was not consumed into the exact task pointer")]))))))
  (test-case "keyboard revocation denies wrong digests, then durably fences admission and recovery"
    (journal:with-fixture
     (lambda (f)
       (define target (journal:fixture-grant f))
       (define op (journal:advance-to! f 'verifying))
       ;; Leave a genuine late write-ahead cut for recovery to refuse after revoke.
       (parameterize ([current-supplemental-journal-crash-cut (lambda (cut)
                                                                (when (eq? cut 'intent-written)
                                                                  (error 'fixture "cut")))])
         (check-exn #rx"cut" (lambda () (journal:step! f 'green-captured))))
       (define pending (load-supplemental-operation target))
       (check-equal? (hash-ref pending 'state) "recover-pending-verified")
       (define snapshot (file->bytes (journal:operation-path f)))
       (define bus (make-event-bus))
       (define ctx (make-tui-ctx #:session-runner void #:event-bus bus #:session-queue #f))
       (define shown (make-semaphore 0))
       (subscribe! bus
                   (lambda (evt)
                     (define sb (tui-ctx-ui-state-box ctx))
                     (set-box! sb (apply-event-to-state (unbox sb) evt))
                     (when (equal? (event-ev evt) "supplemental-evidence.approval-requested")
                       (semaphore-post shown))))
       (define raw
         (format "/evidence-replan revoke ~a ~a" (hash-ref target 'kind) (hash-ref target 'sha256)))
       (define (keyboard! text)
         (define ib (tui-ctx-input-state-box ctx))
         (set-box! ib (input-insert-string (unbox ib) text))
         (process-tui-message! ctx '(key enter)))
       (shared:process-slash-command ctx 'unknown raw)
       (check-equal? (pending-approval-count) 0)
       (keyboard! raw)
       (check-not-false (sync/timeout 2 shown) "keyboard revoke must request a real approval")
       (define overlay (ui-state-active-overlay (unbox (tui-ctx-ui-state-box ctx))))
       (check-not-false overlay)
       (when overlay
         (define extra (overlay-state-extra overlay))
         (define view (hash-ref extra 'approval-view))
         (define ref (hasheq 'kind "revocation-proposal" 'sha256 (hash-ref extra 'commitment-digest)))
         (check-equal? (hash-ref (load-supplemental-record ref) 'refs) (list target))
         (check-equal? (hash-ref view 'record-canonical-json)
                       (hash-ref (supplemental-approval-presentation ref) 'record-canonical-json))
         ;; A decision for any other digest cannot authorize this revocation.
         (check-false (approval-decide! (hash-ref extra 'request-id) (make-string 64 #\0) #t))
         (check-not-false (validate-supplemental-approval target))
         (process-tui-message! ctx '(key #\n))
         (sleep 0.05)
         (check-not-false (validate-supplemental-approval target))
         (check-equal? (file->bytes (journal:operation-path f)) snapshot)
         (keyboard! raw)
         (check-not-false (sync/timeout 2 shown))
         (process-tui-message! ctx '(key #\y))
         (let wait ([tries 200])
           (cond
             [(not (validate-supplemental-approval target)) (void)]
             [(positive? tries)
              (sleep 0.01)
              (wait (sub1 tries))]
             [else (fail "keyboard approval did not durably revoke")]))
         (check-false (validate-supplemental-approval target))
         ;; No approval channel or frontend state is needed to observe durable refusal.
         (clear-approval-channel!)
         (check-false (validate-supplemental-approval target))
         (check-exn exn:fail?
                    (lambda () (start-supplemental-operation! target (journal:fixture-base f))))
         (check-exn exn:fail? (lambda () (load-supplemental-operation target)))
         (check-exn exn:fail?
                    (lambda ()
                      (reconcile-supplemental-operation! target
                                                         (hash-ref pending 'revision)
                                                         (hash-ref pending 'state))))
         (check-exn exn:fail?
                    (lambda ()
                      (transition-supplemental-operation! target
                                                          (hash-ref op 'revision)
                                                          "verifying"
                                                          'launched
                                                          (hasheq 'dispatch-id
                                                                  "forbidden-redispatch"))))
         (define-values (code output errors)
           (journal:finish-worker (journal:worker f "--cas" "forbidden")))
         (check-equal? code 3 errors)
         (check-regexp-match #rx"live authenticated task approval required" output)
         (check-equal? (file->bytes (journal:operation-path f)) snapshot)))))
  (test-case "owner revoke refuses unavailable channel, bus, busy state and invalid target"
    (journal:with-fixture
     (lambda (f)
       (define target (journal:fixture-grant f))
       (define bus (make-event-bus))
       (define ctx (make-tui-ctx #:session-runner void #:event-bus bus #:session-queue #f))
       (define requests 0)
       (subscribe! bus
                   (lambda (evt)
                     (when (equal? (event-ev evt) "supplemental-evidence.approval-requested")
                       (set! requests (add1 requests)))))
       (define raw
         (format "/evidence-replan revoke ~a ~a" (hash-ref target 'kind) (hash-ref target 'sha256)))
       (handle-owner-evidence-command! ctx
                                       (format "/evidence-replan revoke task-approval ~a"
                                               (make-string 64 #\0)))
       (define sb (tui-ctx-ui-state-box ctx))
       (set-box! sb (set-busy (unbox sb) #t))
       (handle-owner-evidence-command! ctx raw)
       (set-box! sb (set-busy (unbox sb) #f))
       (handle-owner-evidence-command! (make-tui-ctx #:session-runner void #:session-queue #f) raw)
       (clear-approval-channel!)
       (handle-owner-evidence-command! ctx raw)
       (check-equal? requests 0)
       (check-equal? (pending-approval-count) 0)
       (check-not-false (validate-supplemental-approval target)))))
  (test-case "missing UI publisher fails closed even with an active broker"
    (with-fixture
     (lambda ()
       (define ref (propose-retain! "task-proposal" (hasheq 'objective "missing publisher")))
       (check-exn #rx"UI publisher unavailable" (lambda () (request-supplemental-approval! ref #f)))
       (check-equal? (pending-approval-count) 0))))
  (test-case "no channel fails closed before registering anything"
    (with-fixture (lambda ()
                    (define ref (propose-retain! "task-proposal" (hasheq 'objective "no channel")))
                    (clear-approval-channel!)
                    (check-exn #rx"no active interactive approval channel"
                               (lambda () (request-supplemental-approval! ref)))
                    (check-equal? (pending-approval-count) 0)))))
