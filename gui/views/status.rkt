#lang racket/base

;; q/gui/views/status.rkt — Status bar view for GUI
;;
;; Renders a status bar showing model name, session status,
;; token count, and turn number. Produces view descriptors
;; consumed by the concrete renderer.

(require racket/contract
         racket/format
         racket/string
         "../../ui-core/theme-protocol.rkt")

(provide (contract-out [render-status-bar
                        (->* (ui-theme?)
                             (#:model (or/c string? #f)
                                      #:status (or/c symbol? string? #f)
                                      #:turn exact-nonnegative-integer?
                                      #:tokens (or/c exact-nonnegative-integer? #f)
                                      #:context-percent (or/c exact-nonnegative-integer? #f)
                                      #:cost (or/c string? #f)
                                      #:active-goal (or/c string? #f)
                                      #:width exact-nonnegative-integer?)
                             hash?)]
                       [status-text (-> (or/c symbol? string?) string?)]
                       [format-status-string
                        (->* (any/c any/c)
                             (#:model (or/c string? #f)
                                      #:context-percent (or/c exact-nonnegative-integer? #f)
                                      #:cost (or/c string? #f)
                                      #:active-goal (or/c string? #f))
                             string?)]))

;; ──────────────────────────────
;; Status symbol → display text
;; ──────────────────────────────
(define (status-text status)
  (case status
    [(idle ready) "Ready"]
    [(processing thinking streaming) "Processing..."]
    [(waiting input) "Waiting for input"]
    [(error) "Error"]
    [(cancelled) "Cancelled"]
    [else (if (string? status) status "Ready")]))

;; ──────────────────────────────
;; Render status bar
;; ──────────────────────────────
(define (format-status-string model
                              status-sym
                              #:model [model-name #f]
                              #:context-percent [ctx-pct #f]
                              #:cost [cost-str #f]
                              #:active-goal [goal #f])
  (define parts
    (filter string?
            (list (or model-name (if (string? model) model "q"))
                  (status-text status-sym)
                  (and ctx-pct (format "ctx:~a%" ctx-pct))
                  cost-str
                  (and goal
                       (if (> (string-length goal) 30)
                           (format "Goal: ~a..." (substring goal 0 27))
                           (format "Goal: ~a" goal))))))
  (string-join parts " | "))

(define (render-status-bar theme
                           #:model [model #f]
                           #:status [status 'idle]
                           #:turn [turn 0]
                           #:tokens [tokens #f]
                           #:context-percent [ctx-pct #f]
                           #:cost [cost-str #f]
                           #:active-goal [goal #f]
                           #:width [width 120])
  (define status-sym (if (symbol? status) status 'idle))
  (define left-section
    (format-status-string model
                          status-sym
                          #:model model
                          #:context-percent ctx-pct
                          #:cost cost-str
                          #:active-goal goal))
  (define right-section
    (format "Turn: ~a~a"
            turn
            (if tokens
                (format " | Tokens: ~a" tokens)
                "")))
  (hash 'view
        'status-bar
        'left
        left-section
        'right
        right-section
        'status
        status-sym
        'model
        model
        'width
        width
        'bg
        (theme-ref theme 'muted)
        'fg
        (theme-ref theme 'foreground)
        'accent
        (theme-ref theme 'accent)))
