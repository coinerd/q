#lang racket/base

;; q/cli/args/print.rkt — Usage and version display
;;
;; Sub-module of cli/args.rkt (split for modularity).
;; Provides print-usage, print-version.

(require racket/format
         racket/string
         "flags.rkt" ; FLAG-DEFINITIONS, flag-def accessors
         "../../util/version.rkt" ; q-version
         )

(provide print-usage
         print-version)

;; ============================================================
;; I/O: print-usage (generated from flag table)
;; ============================================================

(define (print-usage [port (current-output-port)])
  (displayln "Usage: q [options] [prompt]" port)
  (newline port)
  (displayln "Commands:" port)
  (displayln "  q                          Start interactive chat session" port)
  (displayln "  q \"<prompt>\"               Run a single-shot prompt" port)
  (displayln "  q --session <id>           Resume an existing session" port)
  (displayln "  q --tui                    Terminal UI mode (TUI)" port)
  (displayln "  q --json                   JSON mode (machine-readable output)" port)
  (displayln "  q --rpc                    RPC mode (stdin/stdout JSONL protocol)" port)
  (displayln "  q doctor                   Run setup and provider diagnostics" port)
  (displayln "  q init                     Guided setup wizard" port)
  (displayln "  q sessions list            List sessions" port)
  (displayln "  q sessions info <id>       Show session details" port)
  (displayln "  q sessions delete <id>     Delete a session" port)
  (newline port)
  (displayln "Options:" port)
  ;; Generate option lines from FLAG-DEFINITIONS
  (for ([fd (in-list FLAG-DEFINITIONS)])
    (define short-part
      (if (flag-def-short fd)
          (format "-~a, " (flag-def-short fd))
          "    "))
    (define long-part (format "--~a" (flag-def-long fd)))
    (define type-placeholder
      (cond
        [(eq? (flag-def-type fd) 'string) " <value>"]
        [(eq? (flag-def-type fd) 'integer) " <n>"]
        [(eq? (flag-def-type fd) 'accumulate) " <name>"]
        [else ""]))
    (define help-text (flag-def-help fd))
    (define line
      (format "  ~a~a~a~a"
              short-part
              long-part
              type-placeholder
              ;; Pad to column 30
              (make-string (max 1
                                (- 26
                                   (string-length short-part)
                                   (string-length long-part)
                                   (string-length type-placeholder)))
                           #\space)))
    (displayln (string-append line help-text) port))
  (displayln "  doctor                     Run setup and provider diagnostics" port)
  (newline port)
  (displayln "Interactive commands:" port)
  (displayln "  /help                      Show help" port)
  (displayln "  /quit, /exit               Exit session" port)
  (displayln "  /compact                   Trigger compaction now" port)
  (displayln "  /history                   Show session history" port)
  (displayln "  /model [name]              Show or switch model" port)
  (displayln "  /fork [entry-id]           Fork session at given point" port)
  (displayln "  /clear                     Clear transcript (TUI only)" port)
  (displayln "  /interrupt                 Interrupt current turn (TUI only)" port)
  (displayln "  /branches                  List session branches (TUI only)" port)
  (displayln "  /leaves                    List leaf nodes (TUI only)" port)
  (displayln "  /switch <id>               Switch to branch (TUI only)" port)
  (displayln "  /children <id>             Show children of node (TUI only)" port)
  (displayln "  /sessions                  List recent sessions" port)
  (displayln "  /sessions info <id>        Show session details" port)
  (displayln "  /sessions delete <id>      Delete a session" port))

;; ============================================================
;; I/O: print-version
;; ============================================================

;; q-version imported from util/version.rkt (Issue #203)
;; Single source of truth -- do not redefine here.

(define (print-version [port (current-output-port)])
  (displayln (format "q version ~a" q-version) port))
