#lang racket/base

;; tui/state-ui.rkt — UI state helpers: scroll, selection, overlay, widgets, queries
;;
;; Pure functions. No terminal I/O. No side effects.
;; Split from state.rkt (v0.22.6 W2) to keep each module under 400 lines.

(require racket/list
         racket/string
         "state-types.rkt")

;; Transcript helpers
(provide transcript-entries
         add-transcript-entry
         visible-entries
         scroll-up
         scroll-down
         scroll-to-bottom
         scroll-to-top

         ;; State queries
         ui-busy?
         ui-session-label
         ui-model-label
         ui-status-text

         ;; Branch management
         set-current-branch
         set-visible-branches
         clear-visible-branches

         ;; Overlay management (#643)
         show-overlay
         update-overlay-input
         dismiss-overlay
         overlay-active?
         ANCHOR-TOP-LEFT
         ANCHOR-CENTER
         ANCHOR-BOTTOM-RIGHT
         anchor?

         ;; Overlay config (#1145)
         (struct-out overlay-config)
         overlay-config?
         make-overlay-config
         show-overlay-with-config
         overlay-compute-bounds

         ;; Selection
         set-selection-anchor
         set-selection-end
         clear-selection
         has-selection?

         ;; Widget management (#714)
         set-extension-widget
         remove-extension-widget
         remove-all-extension-widgets
         get-widget-lines-above
         get-widget-lines-below

         ;; Custom header/footer (#717)
         set-custom-header
         set-custom-footer
         clear-custom-header
         clear-custom-footer

         ;; Focus management
         set-focused-component
         clear-focused-component

         ;; Custom editor component (#1150)
         set-editor-component
         clear-editor-component

         ;; Retry enrichment (v0.14.2)
         get-last-turn-tool-summary)

;; ============================================================
;; Transcript helpers
;; ============================================================

(define (add-transcript-entry state entry)
  (define-values (id-entry state1)
    (if (transcript-entry-id entry)
        (values entry state)
        (assign-entry-id entry state)))
  (struct-copy ui-state
               state1
               [transcript (cons id-entry (ui-state-transcript state1))]
               [scroll-offset 0]))

(define (transcript-entries state)
  (reverse (ui-state-transcript state)))

(define (visible-entries state transcript-height)
  (reverse (ui-state-transcript state)))

(define (scroll-up state [amount 1])
  (define actual (+ (ui-state-scroll-offset state) amount))
  (define next (struct-copy ui-state state [scroll-offset actual]))
  (if (has-selection? next)
      (clear-selection next)
      next))

(define (scroll-down state [amount 1])
  (define new-offset (max 0 (- (ui-state-scroll-offset state) amount)))
  (define next (struct-copy ui-state state [scroll-offset new-offset]))
  (if (has-selection? next)
      (clear-selection next)
      next))

(define (scroll-to-bottom state)
  (struct-copy ui-state state [scroll-offset 0]))

(define (scroll-to-top state)
  (struct-copy ui-state state [scroll-offset 999999]))

;; ============================================================
;; Selection
;; ============================================================

(define (set-selection-anchor state col row)
  (struct-copy ui-state state [sel-anchor (cons col row)] [sel-end (cons col row)]))

(define (set-selection-end state col row)
  (struct-copy ui-state state [sel-end (cons col row)]))

(define (clear-selection state)
  (struct-copy ui-state state [sel-anchor #f] [sel-end #f]))

(define (has-selection? state)
  (and (ui-state-sel-anchor state) (ui-state-sel-end state)))

;; ============================================================
;; Queries
;; ============================================================

(define (ui-busy? state)
  (ui-state-busy? state))

(define (ui-session-label state)
  (or (ui-state-session-id state) "no session"))

(define (ui-model-label state)
  (or (ui-state-model-name state) "no model"))

(define (ui-status-text state)
  (or (ui-state-status-message state)
      (if (ui-state-busy? state)
          (format "busy~a...~a"
                  (if (ui-state-pending-tool-name state)
                      (format " (~a)" (ui-state-pending-tool-name state))
                      "")
                  (queue-status-suffix (ui-state-queue-counts state)))
          (format "idle~a" (queue-status-suffix (ui-state-queue-counts state))))))

(define (queue-status-suffix counts)
  (if (not counts)
      ""
      (let ([steering (hash-ref counts 'steering 0)]
            [followup (hash-ref counts 'followup 0)])
        (cond
          [(and (> steering 0) (> followup 0))
           (format " | \u2691~a steer, ~a follow" steering followup)]
          [(> steering 0) (format " | \u2691~a steer" steering)]
          [(> followup 0) (format " | \u2691~a follow" followup)]
          [else ""]))))

;; ============================================================
;; Branch management helpers
;; ============================================================

(define (set-current-branch state branch-id)
  (struct-copy ui-state state [current-branch branch-id]))

(define (set-visible-branches state branches)
  (struct-copy ui-state state [visible-branches branches]))

(define (clear-visible-branches state)
  (struct-copy ui-state state [visible-branches '()]))

;; ============================================================
;; Overlay anchors (#725)
;; ============================================================

(define ANCHOR-TOP-LEFT 'top-left)
(define ANCHOR-CENTER 'center)
(define ANCHOR-BOTTOM-RIGHT 'bottom-right)

(define (anchor? v)
  (and (symbol? v) (or (eq? v ANCHOR-TOP-LEFT) (eq? v ANCHOR-CENTER) (eq? v ANCHOR-BOTTOM-RIGHT))))

;; ============================================================
;; Overlay config (#1145)
;; ============================================================

(struct overlay-config (anchor width-spec height-spec margin) #:transparent)

(define make-overlay-config overlay-config)

(define (show-overlay-with-config state config type content [input ""])
  (struct-copy ui-state
               state
               [active-overlay
                (overlay-state type
                               content
                               input
                               (overlay-config-anchor config)
                               (overlay-config-width-spec config)
                               (overlay-config-height-spec config)
                               (overlay-config-margin config)
                               #f)]))

(define (overlay-compute-bounds ov term-width term-height)
  (define w-spec (overlay-state-width ov))
  (define h-spec (overlay-state-height ov))
  (define margin (overlay-state-margin ov))
  (define w
    (if (pair? w-spec)
        (inexact->exact (floor (* (cdr w-spec) term-width)))
        w-spec))
  (define h
    (if (pair? h-spec)
        (inexact->exact (floor (* (cdr h-spec) term-height)))
        h-spec))
  (define anchor (overlay-state-anchor ov))
  (define-values (x y)
    (case anchor
      [(top-left) (values margin margin)]
      [(top-center) (values (quotient (- term-width w) 2) margin)]
      [(top-right) (values (max 0 (- term-width w margin)) margin)]
      [(mid-left) (values margin (quotient (- term-height h) 2))]
      [(mid-center center) (values (quotient (- term-width w) 2) (quotient (- term-height h) 2))]
      [(mid-right) (values (max 0 (- term-width w margin)) (quotient (- term-height h) 2))]
      [(bottom-left) (values margin (max 0 (- term-height h margin)))]
      [(bottom-center) (values (quotient (- term-width w) 2) (max 0 (- term-height h margin)))]
      [(bottom-right) (values (max 0 (- term-width w margin)) (max 0 (- term-height h margin)))]
      [else (values margin margin)]))
  (values x y w h))

;; ============================================================
;; Overlay helpers (#643)
;; ============================================================

(define (show-overlay state
                      type
                      content
                      [input ""]
                      #:anchor [anchor ANCHOR-TOP-LEFT]
                      #:width [width #f]
                      #:height [height #f]
                      #:margin [margin 0])
  (struct-copy ui-state
               state
               [active-overlay (overlay-state type content input anchor width height margin #f)]))

(define (update-overlay-input state input)
  (define ov (ui-state-active-overlay state))
  (if ov
      (struct-copy ui-state state [active-overlay (struct-copy overlay-state ov [input input])])
      state))

(define (dismiss-overlay state)
  (struct-copy ui-state state [active-overlay #f]))

(define (overlay-active? state)
  (and (ui-state-active-overlay state) #t))

;; ============================================================
;; Extension widget helpers (#714)
;; ============================================================

(define (set-extension-widget state ext-name key lines)
  (define widgets (ui-state-extension-widgets state))
  (define widget-key (cons ext-name key))
  (struct-copy ui-state state [extension-widgets (hash-set widgets widget-key lines)]))

(define (remove-extension-widget state ext-name key)
  (define widgets (ui-state-extension-widgets state))
  (define widget-key (cons ext-name key))
  (struct-copy ui-state state [extension-widgets (hash-remove widgets widget-key)]))

(define (remove-all-extension-widgets state ext-name)
  (define widgets (ui-state-extension-widgets state))
  (define filtered
    (for/hash ([(k v) (in-hash widgets)]
               #:unless (equal? (car k) ext-name))
      (values k v)))
  (struct-copy ui-state state [extension-widgets filtered]))

(define (get-widget-lines-above state)
  (define widgets (ui-state-extension-widgets state))
  (apply append (hash-values widgets)))

(define (get-widget-lines-below state)
  '())

;; ============================================================
;; Custom header/footer (#717)
;; ============================================================

(define (set-custom-header state lines)
  (struct-copy ui-state state [custom-header lines]))

(define (set-custom-footer state lines)
  (struct-copy ui-state state [custom-footer lines]))

(define (clear-custom-header state)
  (struct-copy ui-state state [custom-header #f]))

(define (clear-custom-footer state)
  (struct-copy ui-state state [custom-footer #f]))

;; ============================================================
;; Focus management
;; ============================================================

(define (set-focused-component state component-id)
  (struct-copy ui-state state [focused-component component-id]))

(define (clear-focused-component state)
  (struct-copy ui-state state [focused-component #f]))

;; ============================================================
;; Custom editor component (#1150)
;; ============================================================

(define (set-editor-component state comp)
  (struct-copy ui-state state [editor-component comp]))

(define (clear-editor-component state)
  (struct-copy ui-state state [editor-component #f]))

;; ============================================================
;; Retry enrichment (v0.14.2)
;; ============================================================

(define (get-last-turn-tool-summary state)
  (define entries (transcript-entries state))
  (define user-idx
    (for/last ([e (in-list entries)]
               [i (in-naturals)]
               #:when (eq? (transcript-entry-kind e) 'user))
      i))
  (cond
    [(not user-idx) #f]
    [else
     (define after-user (drop entries (add1 user-idx)))
     (define tool-entries
       (for/list ([e (in-list after-user)]
                  #:when (eq? (transcript-entry-kind e) 'tool-end))
         e))
     (cond
       [(null? tool-entries) #f]
       [else
        (define parts
          (for/list ([e (in-list tool-entries)])
            (define meta (transcript-entry-meta e))
            (define name (hash-ref meta 'name "unknown"))
            (define result (hash-ref meta 'result ""))
            (define result-str
              (truncate-string (if (string? result)
                                   result
                                   (format "~a" result))
                               100))
            (format "~a: ~a" name result-str)))
        (string-join parts "; ")])]))
