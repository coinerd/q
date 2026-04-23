#lang racket/base

;; extensions/compact-context.rkt — Compact Context Extension
;;
;; Wave B2: Registers compact-context tool that reads planning state
;; and triggers compaction with context preservation.

(require racket/contract
         racket/string
         racket/file
         racket/path
         racket/port
         json
         "define-extension.rkt"
         "dynamic-tools.rkt"
         "context.rkt"
         "hooks.rkt"
         "../tools/tool.rkt")

(provide compact-context-extension
         handle-compact-context
         gather-planning-summary)

;; ============================================================
;; Planning state reader
;; ============================================================

;; Read a planning artifact if it exists, returns #f otherwise
(define (read-artifact-if-exists base-dir name)
  (define p (build-path base-dir name))
  (and (file-exists? p)
       (call-with-input-file p port->string)))

;; Gather planning summary from .planning/ directory
(define (gather-planning-summary [project-dir (current-directory)])
  (define planning-dir (build-path project-dir ".planning"))
  (if (directory-exists? planning-dir)
      (let* ([plan (read-artifact-if-exists planning-dir "PLAN.md")]
             [state (read-artifact-if-exists planning-dir "STATE.md")]
             [handoff (read-artifact-if-exists planning-dir "HANDOFF.json")]
             [validation (read-artifact-if-exists planning-dir "VALIDATION.md")]
             [summary (read-artifact-if-exists planning-dir "SUMMARY.md")]
             [parts (filter string?
                            (list (and plan (format "## PLAN.md\n~a" plan))
                                  (and state (format "## STATE.md\n~a" state))
                                  (and summary (format "## SUMMARY.md\n~a" summary))
                                  (and validation (format "## VALIDATION.md\n~a" validation))
                                  (and handoff (format "## HANDOFF.json\n~a" handoff))))])
        (if (null? parts)
            "No planning artifacts found."
            (string-join parts "\n\n")))
      "No .planning/ directory found."))

;; ============================================================
;; Tool handler
;; ============================================================

(define (handle-compact-context args [exec-ctx #f])
  (define project-dir (hash-ref args 'project_dir (path->string (current-directory))))
  (define reason (hash-ref args 'reason "Manual compaction request"))
  (define instructions (hash-ref args 'instructions ""))
  (define min-percent (hash-ref args 'min_percent 0))

  ;; Gather planning state for preservation
  (define planning-summary (gather-planning-summary project-dir))

  ;; Build compaction context
  (define context-info
    (string-append
     "## Compaction Request\n"
     "Reason: " reason "\n"
     (if (non-empty-string? instructions)
         (format "Instructions: ~a\n" instructions)
         "")
     "\n"
     "## Planning State to Preserve\n"
     planning-summary))

  ;; Return a result that the runtime can use to trigger compaction
  ;; The agent loop checks for this tool result and initiates compaction
  (make-success-result
   (list
    (hasheq 'type "text"
            'text (string-append
                   "Compaction triggered.\n\n"
                   "Planning summary gathered for preservation.\n"
                   "The agent will compact its context and resume.\n\n"
                   "## Context Summary\n"
                   (if (> (string-length planning-summary) 500)
                       (string-append (substring planning-summary 0 500) "...")
                       planning-summary)))
    (hasheq 'type "compaction-trigger"
            'reason reason
            'instructions instructions
            'min_percent min-percent
            'planning_summary planning-summary))))

;; ============================================================
;; Extension definition
;; ============================================================

(define (register-compact-tools ctx)
  (ext-register-tool!
   ctx
   (make-tool
    "compact-context"
    (string-append
     "Trigger context compaction with planning state preservation. "
     "Reads .planning/ artifacts (PLAN.md, STATE.md, HANDOFF.json, etc.) "
     "and injects them into the compaction context so the agent "
     "resumes with full project state awareness.")
    (hasheq 'type
            "object"
            'required
            '()
            'properties
            (hasheq
             'reason
             (hasheq 'type "string"
                     'description "Why compaction is needed")
             'instructions
             (hasheq 'type "string"
                     'description "Extra instructions for post-compaction resume")
             'min_percent
             (hasheq 'type "integer"
                     'description "Min context % to trigger (default 0 = always)")
             'project_dir
             (hasheq 'type "string"
                     'description "Project root directory (default: cwd)")))
    handle-compact-context))
  (hook-pass ctx))

(define-q-extension compact-context-extension
  #:version "1.0.0"
  #:api-version "1"
  #:on register-tools
  register-compact-tools)
