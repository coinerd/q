#lang racket/base

;; q/gui/components/rich-transcript-view.rkt — Rich transcript using text% + editor-canvas%
;;
;; Replaces the canvas% + draw-text approach with a text% editor that
;; supports native text selection, auto-wrap, and rich formatting.
;; All racket/gui classes loaded via dynamic-require for headless safety.

(require racket/contract
         racket/string
         racket/class
         racket/list
         "../../ui-core/theme-protocol.rkt")

(provide role->label
         role->color
         hex->color-object
         (contract-out [make-role-label-delta (-> string? (or/c string? #f) hash?)]
                       [make-content-delta (-> (or/c string? #f) hash?)]
                       [render-message-descriptor (-> hash? ui-theme? hash?)]
                       [messages->render-plan (-> any/c ui-theme? (listof hash?))]
                       [compute-transcript-diff (-> list? list? (listof hash?))]
                       [apply-diff-to-plan
                        (-> (listof hash?) (listof hash?) ui-theme? (listof hash?))]
                       [update-last-message (-> list? string? list?)]))

;; ──────────────────────────────
;; Pure helpers (headless-testable)
;; ──────────────────────────────

(define (role->label role)
  (case (string->symbol (or role ""))
    [(user) "You"]
    [(assistant) "Assistant"]
    [(system) "System"]
    [(tool) "Tool"]
    [else (string-titlecase (or role "Unknown"))]))

(define (role->color role theme)
  (case (string->symbol (or role ""))
    [(user) (theme-ref theme 'accent)]
    [(assistant) (theme-ref theme 'foreground)]
    [(system) (theme-ref theme 'muted)]
    [(tool) (theme-ref theme 'warning)]
    [else (theme-ref theme 'muted)]))

(define (hex->color-object hex-str)
  (define cleaned (string-trim (or hex-str "#000000") "#" #:left? #t))
  (define r (string->number (substring cleaned 0 2) 16))
  (define g (string->number (substring cleaned 2 4) 16))
  (define b (string->number (substring cleaned 4 6) 16))
  (hash 'r r 'g g 'b b 'hex (or hex-str "#000000")))

(define (make-role-label-delta label color-hex)
  (hash 'type 'role-label 'label label 'color color-hex 'bold #t 'family 'modern 'size 12))

(define (make-content-delta color-hex)
  (hash 'type 'content 'color color-hex 'bold #f 'family 'modern 'size 12))

;; ──────────────────────────────
;; Render plan: message → styled segments
;; ──────────────────────────────

(define (render-message-descriptor msg theme)
  (define role (hash-ref msg 'role "system"))
  (define text (hash-ref msg 'text ""))
  (define label (role->label role))
  (define role-color (role->color role theme))
  (define content-color (theme-ref theme 'foreground))
  (hash 'role
        role
        'text
        text
        'segments
        (list (hash 'type
                    'role-label
                    'text
                    (string-append label ": ")
                    'style
                    (make-role-label-delta label role-color))
              (hash 'type
                    'content
                    'text
                    (string-append text "\n\n")
                    'style
                    (make-content-delta content-color)))))

(define (messages->render-plan msgs theme)
  (for/list ([m (in-list (if (list? msgs)
                             msgs
                             '()))])
    (render-message-descriptor m theme)))

;; ──────────────────────────────
;; Diff-based update logic
;; ──────────────────────────────

;; (update-last-message msgs new-text) → updated message list
;; Replaces the text of the last message with new-text.
;; Used for streaming delta accumulation.
(define (update-last-message msgs new-text)
  (cond
    [(null? msgs) msgs]
    [else
     (define last (car (reverse msgs)))
     (define updated (hash-set last 'text new-text))
     (append (take-msgs msgs (- (length msgs) 1)) (list updated))]))

;; Helper: safe take
(define (take-msgs lst n)
  (if (>= n (length lst))
      lst
      (take lst n)))

;; (compute-transcript-diff old-msgs new-msgs) → list of ops
;; Returns a list of operation hashes describing what changed.
;; Ops: (hash 'op 'append 'msg <message-hash>)
;;      (hash 'op 'update-last 'msg <message-hash>)
;;      (hash 'op 'reset 'msgs <list-of-messages>)
(define (compute-transcript-diff old-msgs new-msgs)
  (define old-len (length old-msgs))
  (define new-len (length new-msgs))
  (cond
    ;; No change
    [(= old-len new-len)
     (cond
       [(and (> old-len 0)
             (not (equal? (hash-ref (list-ref old-msgs (- old-len 1)) 'text #f)
                          (hash-ref (list-ref new-msgs (- new-len 1)) 'text #f))))
        ;; Last message text changed (streaming update)
        (list (hash 'op 'update-last 'msg (list-ref new-msgs (- new-len 1))))]
       [else '()])]
    ;; New messages appended
    [(> new-len old-len)
     (for/list ([i (in-range old-len new-len)])
       (hash 'op 'append 'msg (list-ref new-msgs i)))]
    ;; Messages removed (clear/reset)
    [(< new-len old-len) (list (hash 'op 'reset 'msgs new-msgs))]))

;; (apply-diff-to-plan current-plan diff-ops theme) → updated plan
;; Applies diff operations to an existing render plan, producing a new plan.
(define (apply-diff-to-plan current-plan diff-ops theme)
  (for/fold ([plan current-plan]) ([op (in-list diff-ops)])
    (case (hash-ref op 'op #f)
      [(append)
       (define msg (hash-ref op 'msg))
       (append plan (list (render-message-descriptor msg theme)))]
      [(update-last)
       (define msg (hash-ref op 'msg))
       (define new-desc (render-message-descriptor msg theme))
       (if (null? plan)
           (list new-desc)
           (append (take-msgs plan (- (length plan) 1)) (list new-desc)))]
      [(reset)
       (define msgs (hash-ref op 'msgs '()))
       (messages->render-plan msgs theme)]
      [else plan])))
