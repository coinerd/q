#lang racket/base

;; extensions/github-integration.rkt — GitHub Integration Extension (facade)
;;
;; Wave A3: Registers 6 GitHub tools (gh-issue, gh-pr, gh-milestone,
;; gh-board, gh-wave-start, gh-wave-finish) that wrap the gh CLI.
;; Gracefully degrades when gh is not installed.
;;
;; Implementation lives in:
;;   github/tool-schemas.rkt  — 6 tool JSON schemas
;;   github/tool-handlers.rkt — 6 tool handlers + registration functions

(require racket/format
         racket/string
         json
         "define-extension.rkt"
         "hooks.rkt"
         ;; Re-export helpers for backward compat
         (only-in "github/helpers.rkt"
                  gh-binary-path
                  run-command
                  shell-quote
                  valid-identifier?
                  valid-number?
                  valid-state?
                  valid-method?)
         "github/tool-schemas.rkt"
         "github/tool-handlers.rkt")

(provide github-integration-extension
         ;; Re-exported from github/helpers.rkt for backward compat
         gh-binary-path
         run-command
         shell-quote
         valid-identifier?
         valid-number?
         valid-state?
         valid-method?
         ;; Tool handlers
         handle-gh-issue
         handle-gh-pr
         handle-gh-milestone
         handle-gh-board
         handle-gh-wave-start
         handle-gh-wave-finish
         register-github-tools)

;; ============================================================
;; Extension command handler
;; ============================================================

(define (handle-github-command payload)
  (define cmd (hash-ref payload 'command #f))
  (define input-text (hash-ref payload 'input ""))
  (cond
    [(member cmd '("/milestone" "/ms"))
     (hook-amend (hasheq 'text
                         (string-append "GitHub milestone command."
                                        (if (string=? input-text "")
                                            ""
                                            (format " Input: ~a" input-text))
                                        "\nUsage: /milestone [list|status|create] [args]"
                                        "\n\nUse gh-milestone-list, gh-milestone-status,"
                                        " or gh-milestone-create tools for full functionality.")))]
    [(member cmd '("/issue" "/i"))
     (hook-amend (hasheq 'text
                         (string-append
                          "GitHub issue command."
                          (if (string=? input-text "")
                              ""
                              (format " Input: ~a" input-text))
                          "\nUsage: /issue [get|list|create|close] [number|args]"
                          "\n\nUse gh-issue-get, gh-issue-list,"
                          " gh-issue-create, or gh-issue-close for full functionality.")))]
    [(member cmd '("/pr"))
     (hook-amend (hasheq 'text
                         (string-append "GitHub PR command."
                                        (if (string=? input-text "")
                                            ""
                                            (format " Input: ~a" input-text))
                                        "\nUsage: /pr [list|get|create|merge] [number|args]"
                                        "\n\nUse gh-pr-list, gh-pr-get,"
                                        " gh-pr-create, or gh-pr-merge for full functionality.")))]
    [else (hook-pass payload)]))

;; ============================================================
;; Extension definition
;; ============================================================

(define-q-extension github-integration-extension
                    #:version "1.0.0"
                    #:api-version "1"
                    #:on register-tools
                    register-github-tools
                    #:on register-shortcuts
                    register-github-commands
                    #:on execute-command
                    handle-github-command)
