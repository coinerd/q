#lang racket/base

;; q/tui/palette.rkt — Slash-command registry + interactive command palette for the TUI

(require racket/string
         racket/list
         racket/function
         racket/set
         "command-parse.rkt"
         "render.rkt")

(provide
 (struct-out cmd-entry)
 make-command-registry
 register-command!
 lookup-command
 resolve-command
 all-commands
 commands-by-category
 filter-commands
 render-palette-overlay
 complete-command
 commands-from-hashes
 merge-extension-commands)

;; ---------------------------------------------------------------------------
;; Struct: command entry in the registry
;; ---------------------------------------------------------------------------

(struct cmd-entry
  (name         ; string — e.g. "/help"
   summary      ; string — one-line description
   category     ; symbol — 'general | 'session | 'model | 'debug
   args-spec    ; (listof string) — arg names for display, e.g. '("<id>")
   aliases      ; (listof string) — short forms, e.g. '("h" "?")
   )
  #:transparent)

;; ---------------------------------------------------------------------------
;; Registry operations
;; ---------------------------------------------------------------------------

;; Returns a hash of all built-in commands: command-name-string → cmd-entry
(define (make-command-registry)
  (define built-ins
    (list
     (cmd-entry "/help"      "Show help"                     'general '() '("h" "?"))
     (cmd-entry "/quit"      "Exit session"                  'general '() '("q" "exit"))
     (cmd-entry "/clear"     "Clear transcript"              'general '() '("cls"))
     (cmd-entry "/compact"   "Trigger compaction"            'session '() '())
     (cmd-entry "/interrupt" "Interrupt current turn"        'session '() '("stop" "cancel"))
     (cmd-entry "/branches"  "List session branches"         'session '() '())
     (cmd-entry "/leaves"    "List leaf nodes"               'session '() '())
     (cmd-entry "/switch"    "Switch to branch"              'session '("<id>") '())
     (cmd-entry "/children"  "Show children of node"         'session '("<id>") '())
     (cmd-entry "/model"     "Switch or show model"          'model   '("<name>") '("m"))
     (cmd-entry "/history"   "Show session history"          'session '() '())
     (cmd-entry "/fork"      "Fork session at point"         'session '("<id>") '())
     (cmd-entry "/tree"      "Show session tree"             'session '() '("t"))
     (cmd-entry "/name"      "Set session name"              'session '("<title>") '())
     (cmd-entry "/sessions"  "List and manage sessions"      'session '("list|info|delete") '())))
  ;; Build main hash by name
  (define by-name
    (for/fold ([h (hash)])
              ([e (in-list built-ins)])
      (hash-set h (cmd-entry-name e) e)))
  ;; Add alias entries: "/h" → same entry as "/help"
  (for/fold ([h by-name])
            ([e (in-list built-ins)])
    (for/fold ([h2 h])
              ([a (in-list (cmd-entry-aliases e))])
      (hash-set h2 (string-append "/" a) e))))

;; Adds or replaces a command in the registry
(define (register-command! reg entry)
  (hash-set reg (cmd-entry-name entry) entry))

;; Returns cmd-entry or #f
(define (lookup-command reg name)
  (hash-ref reg name #f))

;; Resolve a slash command string to (values cmd-entry args) or (values #f #f).
;; Uses command-parse.rkt for name→symbol mapping, then looks up in registry.
(define (resolve-command reg text)
  (define trimmed (string-trim text))
  (cond
    [(string=? trimmed "") (values #f #f)]
    [(not (char=? (string-ref trimmed 0) #\/)) (values #f #f)]
    [else
     (define parts (string-split trimmed))
     (define cmd-name (car parts))
     (define args (cdr parts))
     (define entry (lookup-command reg cmd-name))
     (if entry
         (values entry args)
         (values #f #f))]))

;; Returns (listof cmd-entry), sorted by name, deduplicated by canonical name
(define (all-commands reg)
  (define seen (mutable-set))
  (define unique '())
  (for ([e (in-list (sort (hash-values reg) string<? #:key cmd-entry-name))])
    (define canonical (cmd-entry-name e))
    (unless (set-member? seen canonical)
      (set-add! seen canonical)
      (set! unique (cons e unique))))
  (sort unique string<? #:key cmd-entry-name))

;; Filter by category, deduplicated
(define (commands-by-category reg cat)
  (define seen (mutable-set))
  (define results '())
  (for ([e (in-list (hash-values reg))])
    (define name (cmd-entry-name e))
    (when (and (eq? (cmd-entry-category e) cat)
               (not (set-member? seen name)))
      (set-add! seen name)
      (set! results (cons e results))))
  (sort results string<? #:key cmd-entry-name))

;; ---------------------------------------------------------------------------
;; Palette filtering
;; ---------------------------------------------------------------------------

;; Filter commands matching a prefix string.
;; Returns (listof cmd-entry) where name starts with prefix, deduplicated.
(define (filter-commands reg prefix)
  (define seen (mutable-set))
  (define results '())
  (for ([e (in-list (hash-values reg))])
    (define name (cmd-entry-name e))
    (when (and (string-prefix? name prefix)
               (not (set-member? seen name)))
      (set-add! seen name)
      (set! results (cons e results))))
  (sort results string<? #:key cmd-entry-name))

;; ---------------------------------------------------------------------------
;; Extension command integration (#677)
;; ---------------------------------------------------------------------------

;; Convert a list of command hash descriptors to cmd-entry structs.
;; Each hash should have keys: name, summary, category, args-spec, aliases.
;; Used by the runtime to convert extension-provided command definitions.
(define (commands-from-hashes cmd-hashes)
  (for/list ([h (in-list cmd-hashes)])
    (cmd-entry (hash-ref h 'name "")
               (hash-ref h 'summary "")
               (hash-ref h 'category 'general)
               (hash-ref h 'args-spec '())
               (hash-ref h 'aliases '()))))

;; Merge extension commands into a base registry hash.
;; Extension commands override built-in commands with the same name.
(define (merge-extension-commands reg ext-cmds)
  (for/fold ([r reg])
            ([e (in-list ext-cmds)])
    (hash-set r (cmd-entry-name e) e)))

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
;; Returns (listof string) of matching command names, deduplicated.
(define (complete-command reg partial)
  (define matches (filter-commands reg partial))
  (map cmd-entry-name matches))
