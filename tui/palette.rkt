#lang racket/base

;; q/tui/palette.rkt — Slash-command registry + interactive command palette for the TUI

(require racket/string
         racket/list
         racket/function
         "render.rkt")

(provide
 (struct-out cmd-entry)
 make-command-registry
 register-command!
 lookup-command
 all-commands
 commands-by-category
 filter-commands
 render-palette-overlay
 complete-command)

;; ---------------------------------------------------------------------------
;; Struct: command entry in the registry
;; ---------------------------------------------------------------------------

(struct cmd-entry
  (name         ; string — e.g. "/help"
   summary      ; string — one-line description
   category     ; symbol — 'general | 'session | 'model | 'debug
   args-spec    ; (listof string) — arg names for display, e.g. '("<id>")
   )
  #:transparent)

;; ---------------------------------------------------------------------------
;; Registry operations
;; ---------------------------------------------------------------------------

;; Returns a hash of all built-in commands: command-name-string → cmd-entry
(define (make-command-registry)
  (define built-ins
    (list
     (cmd-entry "/help"      "Show help"                     'general '())
     (cmd-entry "/quit"      "Exit session"                  'general '())
     (cmd-entry "/clear"     "Clear transcript"              'general '())
     (cmd-entry "/compact"   "Trigger compaction"            'session '())
     (cmd-entry "/interrupt" "Interrupt current turn"        'session '())
     (cmd-entry "/branches"  "List session branches"         'session '())
     (cmd-entry "/leaves"    "List leaf nodes"               'session '())
     (cmd-entry "/switch"    "Switch to branch"              'session '("<id>"))
     (cmd-entry "/children"  "Show children of node"         'session '("<id>"))
     (cmd-entry "/model"     "Switch or show model"          'model   '("<name>"))
     (cmd-entry "/history"   "Show session history"          'session '())
     (cmd-entry "/fork"      "Fork session at point"         'session '("<id>"))))
  (for/fold ([h (hash)])
            ([e (in-list built-ins)])
    (hash-set h (cmd-entry-name e) e)))

;; Adds or replaces a command in the registry
(define (register-command! reg entry)
  (hash-set reg (cmd-entry-name entry) entry))

;; Returns cmd-entry or #f
(define (lookup-command reg name)
  (hash-ref reg name #f))

;; Returns (listof cmd-entry), sorted by name
(define (all-commands reg)
  (sort (hash-values reg)
        string<? #:key cmd-entry-name))

;; Filter by category
(define (commands-by-category reg cat)
  (sort (filter (lambda (e) (eq? (cmd-entry-category e) cat))
                (hash-values reg))
        string<? #:key cmd-entry-name))

;; ---------------------------------------------------------------------------
;; Palette filtering
;; ---------------------------------------------------------------------------

;; Filter commands matching a prefix string.
;; Returns (listof cmd-entry) where name starts with prefix, sorted by name.
(define (filter-commands reg prefix)
  (sort (filter (lambda (e) (string-prefix? (cmd-entry-name e) prefix))
                (hash-values reg))
        string<? #:key cmd-entry-name))

;; ---------------------------------------------------------------------------
;; TUI Palette Overlay
;; ---------------------------------------------------------------------------

;; Render a palette overlay for the TUI.
;; partial-input: the current partial slash command typed by the user
;; commands: (listof cmd-entry) — the filtered matches
;; cols: terminal width in columns
;; Returns (listof styled-line?) for rendering.
;; Each line shows: "/command  <summary>"
;; The matching prefix portion is rendered in bold.
(define (render-palette-overlay partial-input commands cols)
  (define prefix-len (string-length partial-input))
  (for/list ([cmd (in-list commands)])
    (let* ([name (cmd-entry-name cmd)]
           [summary (cmd-entry-summary cmd)]
           [args (cmd-entry-args-spec cmd)]
           [args-str (if (null? args) "" (string-append " " (string-join args " ")))]
           ;; Build the display: "/command <args>  — summary"
           [name-part (if (> (string-length name) prefix-len)
                          (substring name prefix-len)
                          "")]
           [args-display args-str]
           [full-name (string-append partial-input name-part)]
           [dash "  "]
           [line-text (string-append full-name args-display dash summary)])
      ;; Truncate to cols
      (define display-text
        (if (> (string-length line-text) cols)
            (substring line-text 0 cols)
            line-text))
      ;; Build segments: bold prefix + plain rest
      ;; The prefix portion (matching what user typed) is bold
      (define actual-prefix-len (min prefix-len (string-length name)))
      (if (and (> actual-prefix-len 0)
               (> (string-length name) actual-prefix-len))
          ;; Split into bold prefix + plain rest
          (let* ([bold-part (substring name 0 actual-prefix-len)]
                 [rest-part (substring name actual-prefix-len)]
                 [rest-text (string-append rest-part args-display dash summary)]
                 [truncated-rest
                  (if (> (+ actual-prefix-len (string-length rest-text)) cols)
                      (substring rest-text 0 (max 0 (- cols actual-prefix-len)))
                      rest-text)])
            (styled-line
             (list (styled-segment bold-part '(bold))
                   (styled-segment truncated-rest '()))))
          ;; Either no prefix or prefix covers the whole name
          (styled-line
           (list (styled-segment display-text
                                 (if (> actual-prefix-len 0) '(bold) '()))))))))

;; ---------------------------------------------------------------------------
;; CLI Autocompletion
;; ---------------------------------------------------------------------------

;; Complete a partial slash command.
;; Returns (listof string) of matching command names.
(define (complete-command reg partial)
  (define matches (filter-commands reg partial))
  (map cmd-entry-name matches))
