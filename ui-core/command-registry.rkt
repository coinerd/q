#lang racket

;; q/ui-core/command-registry.rkt — Unified command metadata registry
;;
;; Central source of truth for slash command metadata shared between TUI and GUI.
;; Each command has: name, summary, category, args-spec, aliases, and a
;; frontend-availability flag (tui?, gui?).
;;
;; W4.1 (v0.94.4): Extract from tui/palette.rkt to make command metadata
;; available to both frontends without TUI dependency.

(require racket/contract
         (only-in "../util/command-types.rkt"
                  cmd-entry
                  cmd-entry?
                  cmd-entry-name
                  cmd-entry-summary
                  cmd-entry-category
                  cmd-entry-args-spec
                  cmd-entry-aliases))

;; Extended command entry with frontend availability
(provide ui-command
         ui-command?
         ui-command-name
         ui-command-summary
         ui-command-category
         ui-command-args-spec
         ui-command-aliases
         ui-command-tui?
         ui-command-gui?

         ;; Registry
         (contract-out [make-ui-command-registry (-> (listof ui-command?) hash?)]
                       [ui-registry-lookup (-> hash? string? (or/c ui-command? #f))]
                       [ui-registry-all (-> hash? (listof ui-command?))]
                       [ui-registry-by-category (-> hash? symbol? (listof ui-command?))]
                       [ui-registry-filter (-> hash? string? (listof ui-command?))]
                       [ui-registry-names (-> hash? (listof string?))]))

;; ── Extended command entry ─────────────────────────────────

(struct ui-command
        (name ; string — e.g. "/help"
         summary ; string — one-line description
         category ; symbol — 'general | 'session | 'model | 'debug
         args-spec ; (listof string) — arg names for display
         aliases ; (listof string) — short forms
         tui? ; boolean — available in TUI
         gui?) ; boolean — available in GUI
  #:transparent)

;; ── Registry operations ────────────────────────────────────

(define (make-ui-command-registry commands)
  (for/hash ([cmd (in-list commands)])
    (values (ui-command-name cmd) cmd)))

(define (ui-registry-lookup registry name)
  (hash-ref registry name #f))

(define (ui-registry-all registry)
  (sort (hash-values registry) string<? #:key ui-command-name))

(define (ui-registry-by-category registry cat)
  (filter (lambda (c) (eq? (ui-command-category c) cat)) (hash-values registry)))

(define (ui-registry-filter registry query)
  (define q (string-downcase query))
  (if (string=? q "/")
      (ui-registry-all registry)
      (filter (lambda (c)
                (or (string-contains? (string-downcase (ui-command-name c)) q)
                    (string-contains? (string-downcase (ui-command-summary c)) q)
                    (and (member q (map string-downcase (ui-command-aliases c))) #t)))
              (hash-values registry))))

(define (ui-registry-names registry)
  (sort (hash-keys registry) string<?))

;; ── Canonical command list ─────────────────────────────────

(define canonical-commands
  (list (ui-command "/help" "Show help" 'general '() '("h" "?") #t #t)
        (ui-command "/quit" "Exit session" 'general '() '("q" "exit") #t #t)
        (ui-command "/clear" "Clear transcript" 'general '() '("cls") #t #t)
        (ui-command "/compact" "Trigger compaction" 'session '() '() #t #t)
        (ui-command "/interrupt" "Interrupt current turn" 'session '() '("stop" "cancel") #t #t)
        (ui-command "/branches" "List session branches" 'session '() '() #t #t)
        (ui-command "/leaves" "List leaf nodes" 'session '() '() #t #t)
        (ui-command "/switch" "Switch to branch" 'session '("<id>") '() #t #t)
        (ui-command "/children" "Show children of node" 'session '("<id>") '() #t #t)
        (ui-command "/model" "Switch or show model" 'model '("<name>") '("m") #t #t)
        (ui-command "/history" "Show session history" 'session '() '() #t #t)
        (ui-command "/fork" "Fork session at point" 'session '("<id>") '() #t #t)
        (ui-command "/tree" "Show session tree" 'session '() '("t") #t #t)
        (ui-command "/name" "Set session name" 'session '("<title>") '() #t #t)
        (ui-command "/sessions" "List and manage sessions" 'session '("list|info|delete") '() #t #t)
        (ui-command "/status" "Show session and provider status" 'general '() '("st" "info") #t #t)
        (ui-command "/activate" "Activate extension" 'general '("<name>") '("a") #t #t)
        (ui-command "/deactivate" "Deactivate extension" 'general '("<name>") '() #t #t)
        (ui-command "/reload" "Hot-reload all extensions" 'general '() '() #t #t)))

(provide canonical-commands)
