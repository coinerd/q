#lang racket/base

;;; tui/builtins.rkt — Built-in interactive components for q TUI
;;;
;;; Provides SelectList, BorderedLoader, and SettingsList components
;;; built on the q-component model from component.rkt.

(require racket/list
         racket/string
         "component.rkt"
         "state.rkt")

;; ═══════════════════════════════════════════════════════════════════
;; Provides
;; ═══════════════════════════════════════════════════════════════════

;; SelectList
(provide make-select-list
         select-list-state
         (struct-out select-list-state)

         ;; BorderedLoader
         make-bordered-loader
         spinner-frames

         ;; SettingsList
         make-settings-list
         settings-entry
         (struct-out settings-entry)

         ;; Helpers
         filter-options
         truncate-str)

;; ═══════════════════════════════════════════════════════════════════
;; Helpers
;; ═══════════════════════════════════════════════════════════════════

(define (filter-options options filter-text)
  (if (string=? filter-text)
      options
      (filter (lambda (o) (string-contains? (string-downcase o) (string-downcase filter-text)))
              options)))

(define (truncate-str s max-len)
  (if (> (string-length s) max-len)
      (string-append (substring s 0 (max 0 (- max-len 3))) "...")
      s))

(define (take-at-most lst n)
  (take lst (min n (length lst))))
(define (drop-at-most lst n)
  (drop lst (min n (length lst))))

;; ═══════════════════════════════════════════════════════════════════
;; SelectList component
;; ═══════════════════════════════════════════════════════════════════

;; A scrollable, filterable selection list
(struct select-list-state (options selected-idx filter-text scroll-offset) #:transparent)

(define (make-select-list options
                          #:on-select [on-select void]
                          #:on-cancel [on-cancel void]
                          #:id [id 'select-list])
  (define state-box (box (select-list-state options 0 "" 0)))
  (make-q-component
   (lambda (ui-state width)
     (define sls (unbox state-box))
     (define filter-line (format "Filter: ~a" (select-list-state-filter-text sls)))
     (define filtered
       (filter-options (select-list-state-options sls) (select-list-state-filter-text sls)))
     (define visible-count (max 1 (- (quotient (max width 20) 2) 2)))
     (define offset (select-list-state-scroll-offset sls))
     (define idx (select-list-state-selected-idx sls))
     (define visible (take-at-most (drop-at-most filtered offset) visible-count))
     (append (list (list (cons 'text filter-line)))
             (for/list ([opt (in-list visible)]
                        [i (in-naturals)])
               (list (cons 'text (format "~a ~a" (if (= (+ i offset) idx) "▸" " ") opt))))))
   #:id id
   #:handle-input
   (lambda (data ui-state)
     (define sls (unbox state-box))
     (cond
       [(eq? data 'up)
        (define new-idx (max 0 (sub1 (select-list-state-selected-idx sls))))
        (set-box! state-box (struct-copy select-list-state sls [selected-idx new-idx]))
        (values ui-state (input-consumed))]
       [(eq? data 'down)
        (define filtered
          (filter-options (select-list-state-options sls) (select-list-state-filter-text sls)))
        (define new-idx
          (min (max 0 (sub1 (length filtered))) (add1 (select-list-state-selected-idx sls))))
        (set-box! state-box (struct-copy select-list-state sls [selected-idx new-idx]))
        (values ui-state (input-consumed))]
       [(eq? data 'enter)
        (define filtered
          (filter-options (select-list-state-options sls) (select-list-state-filter-text sls)))
        (when (and (>= (select-list-state-selected-idx sls) 0)
                   (< (select-list-state-selected-idx sls) (length filtered)))
          (on-select (list-ref filtered (select-list-state-selected-idx sls))))
        (values ui-state (input-action (list 'select (select-list-state-selected-idx sls))))]
       [(eq? data 'escape)
        (on-cancel)
        (values ui-state (input-action 'cancel))]
       [(string? data) ;; filter text input
        (set-box! state-box
                  (struct-copy select-list-state
                               sls
                               [filter-text
                                (string-append (select-list-state-filter-text sls) data)]))
        (values ui-state (input-consumed))]
       [else (values ui-state (input-bubble))]))
   #:wants-focus? #t))

;; ═══════════════════════════════════════════════════════════════════
;; BorderedLoader component
;; ═══════════════════════════════════════════════════════════════════

(define spinner-frames '("⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"))

(define (make-bordered-loader status-text #:id [id 'loader])
  (define frame-box (box 0))
  (make-q-component
   (lambda (ui-state width)
     (define frame-idx (unbox frame-box))
     (define spinner (list-ref spinner-frames (modulo frame-idx (length spinner-frames))))
     (define inner-width (max 10 (- width 4)))
     (define top (format "╭─~a─╮" (make-string (max 0 (- inner-width 2)) #\─)))
     (define bottom (format "╰─~a─╯" (make-string (max 0 (- inner-width 2)) #\─)))
     (define text (format "│ ~a ~a │" spinner (truncate-str status-text (max 1 (- inner-width 4)))))
     (list (list (cons 'text top)) (list (cons 'text text)) (list (cons 'text bottom))))
   #:id id
   #:handle-input (lambda (data ui-state)
                    ;; Advance spinner on any input (tick)
                    (set-box! frame-box (add1 (unbox frame-box)))
                    (values ui-state (input-bubble)))))

;; ═══════════════════════════════════════════════════════════════════
;; SettingsList component
;; ═══════════════════════════════════════════════════════════════════

(struct settings-entry (key label value type) #:transparent)

(define (make-settings-list entries #:on-change [on-change void] #:id [id 'settings])
  (define idx-box (box 0))
  (make-q-component
   (lambda (ui-state width)
     (define selected (unbox idx-box))
     (for/list ([e (in-list entries)]
                [i (in-naturals)])
       (define marker (if (= i selected) "▸ " "  "))
       (define val-str
         (cond
           [(eq? (settings-entry-type e) 'boolean) (if (settings-entry-value e) "[●]" "[○]")]
           [else (format "[~a]" (settings-entry-value e))]))
       (list (cons 'text (format "~a~a: ~a" marker (settings-entry-label e) val-str)))))
   #:id id
   #:handle-input (lambda (data ui-state)
                    (cond
                      [(eq? data 'up)
                       (set-box! idx-box (max 0 (sub1 (unbox idx-box))))
                       (values ui-state (input-consumed))]
                      [(eq? data 'down)
                       (set-box! idx-box (min (sub1 (length entries)) (add1 (unbox idx-box))))
                       (values ui-state (input-consumed))]
                      [(eq? data 'enter)
                       (define idx (unbox idx-box))
                       (define e (list-ref entries idx))
                       (when (eq? (settings-entry-type e) 'boolean)
                         (define new-val (not (settings-entry-value e)))
                         (on-change (settings-entry-key e) new-val))
                       (values ui-state (input-action 'toggle))]
                      [else (values ui-state (input-bubble))]))
   #:wants-focus? #t))
