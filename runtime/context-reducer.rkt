#lang racket/base

;; runtime/context-reducer.rkt — pair-aware context reduction for retry (#1329)
;;
;; Provides trim-context-pair-aware which reduces message context for retry
;; while keeping tool_call/result pairs atomic and ensuring the first
;; non-system message is always 'user'.
;;
;; Strategy:
;;   1. Separate system and non-system messages
;;   2. Walk backwards through non-system msgs, collecting "atomic units"
;;   3. An atomic unit is: one standalone message, OR assistant+tool_calls + all matching tool results
;;   4. Take atomic units from the newest end until we reach keep-count
;;   5. Ensure first non-system message is always 'user (add synthetic if needed)

(require "../util/protocol-types.rkt"
         "../util/message-helpers.rkt"
         racket/list)

(provide trim-context-pair-aware)

(define SYNTHETIC-USER-TEXT "[context was compacted for retry]")

(define (make-synthetic-user-msg)
  (make-message (format "synthetic-~a" (current-inexact-milliseconds))
                #f
                'user
                'message
                (list (make-text-part SYNTHETIC-USER-TEXT))
                0
                (hasheq)))

;; has-tool-calls? and get-tool-call-ids imported from util/message-helpers.rkt

(define (tool-result-msg? msg)
  (eq? (message-role msg) 'tool))

(define (get-result-call-ids msg)
  (for/list ([cp (in-list (message-content msg))]
             #:when (tool-result-part? cp))
    (tool-result-part-tool-call-id cp)))

;; Main entry point
(define (trim-context-pair-aware ctx keep-count)
  (cond
    [(null? ctx) '()]
    [else
     (define system-msgs (filter (lambda (m) (eq? (message-role m) 'system)) ctx))
     (define non-system (filter (lambda (m) (not (eq? (message-role m) 'system))) ctx))
     (cond
       [(null? non-system) system-msgs]
       [(<= (length non-system) keep-count) (append system-msgs (ensure-user-first non-system))]
       [else
        ;; Walk backwards to build atomic units
        (define units (build-atomic-units non-system))
        ;; Take units from the end until we reach keep-count
        (define kept (take-units units keep-count))
        (append system-msgs (ensure-user-first kept))])]))

;; Build atomic units from a message list (oldest-first order).
;; Returns a list of units, each unit is a list of messages that must
;; stay together. Units are in oldest-first order.
(define (build-atomic-units msgs)
  ;; Process from newest to oldest, building units
  (define rev-msgs (reverse msgs))
  (define rev-units (build-units-from-newest rev-msgs))
  (reverse rev-units))

;; Walk through messages in newest-first order, producing units.
(define (build-units-from-newest rev-msgs)
  (let loop ([msgs rev-msgs]
             [units '()])
    (cond
      [(null? msgs) units]
      [else
       (define msg (car msgs))
       (cond
         ;; Tool result: find all consecutive tool results, then find the
         ;; matching tool_call message. Group them together as one unit.
         [(tool-result-msg? msg)
          (define-values (tool-results rest-after-results) (splitf-at msgs tool-result-msg?))
          (cond
            [(and (pair? rest-after-results) (has-tool-calls? (car rest-after-results)))
             ;; The next msg is a tool_call. Check if it covers all results.
             (define call-msg (car rest-after-results))
             (define call-ids (get-tool-call-ids call-msg))
             (define result-ids (apply append (map get-result-call-ids tool-results)))
             (cond
               [(and (pair? call-ids)
                     (for/and ([rid (in-list result-ids)])
                       (member rid call-ids)))
                ;; Complete pair: [tool_call, result, result, ...]
                ;; In oldest-first order: tool_call first, then results
                (define unit (cons call-msg tool-results))
                (loop (cdr rest-after-results) (cons unit units))]
               ;; Mismatch — treat results as orphans, skip them
               [else (loop rest-after-results units)])]
            ;; Orphan results — no matching tool_call — skip
            [else (loop rest-after-results units)])]
         ;; Regular message: one-message unit
         [else (loop (cdr msgs) (cons (list msg) units))])])))

;; Take units from the newest end (last in list) until count >= target.
;; Returns messages in oldest-first order.
(define (take-units units target)
  ;; units is oldest-first. Take from the end.
  (define rev-units (reverse units))
  (let loop ([ru rev-units]
             [count 0]
             [acc '()])
    (cond
      [(null? ru) (apply append (reverse acc))]
      [(>= count target) (apply append (reverse acc))]
      [else
       (define u (car ru))
       (loop (cdr ru) (+ count (length u)) (cons u acc))])))

;; Ensure first non-system message is 'user.
(define (ensure-user-first msgs)
  (cond
    [(null? msgs) '()]
    [(eq? (message-role (car msgs)) 'user) msgs]
    [else (cons (make-synthetic-user-msg) msgs)]))
