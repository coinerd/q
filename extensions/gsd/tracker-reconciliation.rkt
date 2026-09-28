#lang racket/base
;; STABILITY: internal

;; Idempotent GitHub tracker reconciliation after receipt-authoritative
;; delivery. This module does not discover issues, invent receipts, or repair
;; historical campaign status. The parent/orchestrator must call it only after
;; durable completion wiring; this boundary independently re-checks the durable
;; DONE record, verified receipt identity, authenticated delivery readback, and
;; delivered handoff merge SHA before issuing tracker commands.

(require racket/file
         racket/list
         "campaign-state.rkt"
         "campaign-repository.rkt"
         "delivery-handoff.rkt"
         "delivery-journal.rkt"
         "effect-ports.rkt")

(provide reconcile-tracker-after-delivery!
         tracker-reconciliation-result
         tracker-reconciliation-result?
         tracker-reconciliation-result-status
         tracker-reconciliation-result-reason
         tracker-reconciliation-result-merge-sha
         tracker-reconciliation-result-actions)

(struct tracker-reconciliation-result (status reason merge-sha actions) #:transparent)

(define FULL-SHA-RX #px"^[0-9a-f]{40}$")

(define (full-sha? v)
  (and (string? v) (regexp-match? FULL-SHA-RX v)))

(define (nonempty-string? v)
  (and (string? v) (positive? (string-length v))))

(define (blocked reason [merge-sha #f])
  (tracker-reconciliation-result 'blocked reason merge-sha '()))

(define (reconciled merge-sha actions)
  (tracker-reconciliation-result 'reconciled #f merge-sha actions))

(define (find-wave rec wave-index)
  (for/first ([w (in-list (campaign-record-waves rec))]
              #:when (= (campaign-wave-index w) wave-index))
    w))

(define (safe-load-campaign base-dir plan-id)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (load-campaign-record base-dir plan-id)))

(define (safe-load-journal base-dir plan-id wave-index)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (load-delivery-journal base-dir plan-id wave-index)))

(define (read-single-datum path)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and (file-exists? path)
         (call-with-input-file path
                               (lambda (in)
                                 (parameterize ([read-accept-reader #f]
                                                [read-accept-lang #f]
                                                [read-accept-graph #f]
                                                [read-accept-compiled #f])
                                   (define datum (read in))
                                   (and (hash? datum) (eof-object? (read in)) datum)))))))

(define (safe-load-handoff base-dir plan-id wave-index)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (read-single-datum (delivery-handoff-path base-dir plan-id wave-index))))

(define (safe-delivery-proof delivery-reader base-dir plan-id wave-index)
  (with-handlers ([exn:fail? (lambda (e)
                               (hasheq 'status "delivery-pending" 'reason (exn-message e)))])
    (delivery-reader base-dir plan-id wave-index)))

(define (delivered-merge-sha proof plan-id wave-index)
  (define sha (and (hash? proof) (hash-ref proof 'merge-sha #f)))
  (and (hash? proof)
       (equal? (hash-ref proof 'status #f) "delivered")
       (equal? (hash-ref proof 'plan-id #f) plan-id)
       (equal? (hash-ref proof 'wave #f) wave-index)
       (full-sha? sha)
       sha))

(define (tracker-issue-number binding plan-id wave-index)
  (and (hash? binding)
       (equal? (hash-ref binding 'plan-id #f) plan-id)
       (equal? (hash-ref binding 'wave #f) wave-index)
       (let ([n (hash-ref binding 'issue-number #f)]) (and (exact-positive-integer? n) n))))

(define (tracker-board-field binding)
  (and (hash? binding)
       (let ([field (hash-ref binding 'board-field #f)]
             [value (hash-ref binding 'board-value #f)])
         (and (nonempty-string? field) (nonempty-string? value) (cons field value)))))

(define (receipt-matches-wave? receipt wave)
  (and (valid-delivery-receipt? receipt)
       (equal? (hash-ref receipt 'branch #f) (campaign-wave-delivery-branch wave))
       (equal? (hash-ref receipt 'head #f) (campaign-wave-delivery-head-sha wave))))

(define (handoff-delivered-with-merge? handoff plan-id wave-index merge-sha)
  (and (hash? handoff)
       (equal? (hash-ref handoff 'plan-id #f) plan-id)
       (equal? (hash-ref handoff 'wave #f) wave-index)
       (eq? (hash-ref handoff 'status #f) 'delivered)
       (equal? (hash-ref handoff 'merge-sha #f) merge-sha)))

(define (corr plan-id wave-index merge-sha suffix issue-number)
  (format "tracker:~a:w~a:~a:~a:~a" plan-id wave-index merge-sha suffix issue-number))

(define (execute-tracker-commands! github-port
                                   plan-id
                                   wave-index
                                   wave
                                   receipt
                                   merge-sha
                                   binding
                                   issue-number)
  (define board (tracker-board-field binding))
  (define common
    (hasheq 'issue-number
            issue-number
            'plan-id
            plan-id
            'wave
            wave-index
            'delivery-branch
            (campaign-wave-delivery-branch wave)
            'delivery-head-sha
            (campaign-wave-delivery-head-sha wave)
            'receipt-branch
            (hash-ref receipt 'branch)
            'receipt-head
            (hash-ref receipt 'head)
            'merge-sha
            merge-sha))
  (define actions '())
  (when board
    (define field (car board))
    (define value (cdr board))
    (set! actions
          (append actions
                  (list ((gsd-github-port-execute github-port)
                         (gsd-github-command 'board-set-field
                                             (corr plan-id wave-index merge-sha "board" issue-number)
                                             (hash-set (hash-set common 'field field) 'value value)
                                             #f))))))
  (set! actions
        (append actions
                (list ((gsd-github-port-execute github-port)
                       (gsd-github-command 'issue-close
                                           (corr plan-id wave-index merge-sha "close" issue-number)
                                           common
                                           #f)))))
  actions)

(define (reconcile-tracker-after-delivery! base-dir
                                           plan-id
                                           wave-index
                                           tracker-binding
                                           github-port
                                           #:delivery-reader [delivery-reader delivery-readback])
  (unless (and (string? plan-id) (regexp-match? #px"^[0-9a-f]{64}$" plan-id))
    (raise-argument-error 'reconcile-tracker-after-delivery! "64-hex plan-id" plan-id))
  (unless (exact-nonnegative-integer? wave-index)
    (raise-argument-error 'reconcile-tracker-after-delivery! "exact-nonnegative-integer?" wave-index))
  (unless (gsd-github-port? github-port)
    (raise-argument-error 'reconcile-tracker-after-delivery! "gsd-github-port?" github-port))
  (let/ec return
    (when ((gsd-github-port-dry-run? github-port))
      (return (blocked "github port is dry-run; tracker reconciliation requires a live port")))
    (define issue-number (tracker-issue-number tracker-binding plan-id wave-index))
    (unless issue-number
      (return (blocked "missing safe tracker issue binding")))
    (define rec (safe-load-campaign base-dir plan-id))
    (define wave (and rec (find-wave rec wave-index)))
    (unless wave
      (return (blocked "missing durable campaign wave")))
    (unless (eq? (campaign-wave-status wave) 'done)
      (return (blocked "durable campaign wave is not DONE")))
    (define journal (safe-load-journal base-dir plan-id wave-index))
    (define receipt (and (hash? journal) (hash-ref journal 'receipt #f)))
    (unless (receipt-matches-wave? receipt wave)
      (return (blocked "delivery journal receipt branch/head do not match durable wave")))
    (define proof (safe-delivery-proof delivery-reader base-dir plan-id wave-index))
    (define merge-sha (delivered-merge-sha proof plan-id wave-index))
    (unless merge-sha
      (return (blocked "authenticated delivery readback is not delivered")))
    (define handoff (safe-load-handoff base-dir plan-id wave-index))
    (unless (handoff-delivered-with-merge? handoff plan-id wave-index merge-sha)
      (return (blocked "delivered handoff missing or merge SHA mismatch" merge-sha)))
    (reconciled merge-sha
                (execute-tracker-commands! github-port
                                           plan-id
                                           wave-index
                                           wave
                                           receipt
                                           merge-sha
                                           tracker-binding
                                           issue-number))))
