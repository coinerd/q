#lang racket/base

;; extensions/gsd/context-bundle.rkt — Context assembly per phase
;;
;; Wave 1b of v0.21.0: Role-specific context bundles.
;; Instead of dumping the full plan + state into the prompt,
;; assemble a role-specific context bundle with only what the
;; current phase needs.

(require racket/string
         racket/match
         "plan-types.rkt")

(provide assemble-context
         explorer-bundle
         executor-bundle
         verifier-bundle
         ;; Config
         max-bundle-size
         bundle-size-warning?)

;; ============================================================
;; Configuration
;; ============================================================

(define max-bundle-size 4000)

(define (bundle-size-warning? bundle)
  (> (string-length bundle) max-bundle-size))

;; ============================================================
;; Public API
;; ============================================================

;; Assemble a context bundle for the given role.
;; role: 'explorer | 'executor | 'verifier
;; artifacts: hash with keys like 'plan, 'state, 'summaries
;; files: hash mapping file-path to file-contents
(define (assemble-context role artifacts files)
  (case role
    [(explorer) (explorer-bundle artifacts files)]
    [(executor) (executor-bundle artifacts files)]
    [(verifier) (verifier-bundle artifacts files)]
    [else (error 'assemble-context "unknown role: ~a" role)]))

;; ============================================================
;; Explorer bundle
;; ============================================================

;; Explorer gets: project context + user request
;; No plan (doesn't exist yet), no summaries.
(define (explorer-bundle artifacts files)
  (define user-request (hash-ref artifacts 'user-request ""))
  (define project-context (hash-ref artifacts 'project-context ""))
  (define parts
    (filter-string (list "## Your Task"
                         (if (non-empty? user-request)
                             (format "Understand and plan a solution for:\n~a" user-request)
                             "Explore the codebase and understand the problem.")
                         (if (non-empty? project-context)
                             (format "## Project Context\n~a" project-context)
                             #f))))
  (string-join parts "\n\n"))

;; ============================================================
;; Executor bundle
;; ============================================================

;; Executor gets: plan text + STATE + current wave context + referenced files
(define (executor-bundle artifacts files)
  (define plan-text (hash-ref artifacts 'plan ""))
  (define state-text (hash-ref artifacts 'state ""))
  (define wave-index (hash-ref artifacts 'wave-index 0))
  (define wave-text (extract-wave-context plan-text wave-index))
  (define referenced-files (format-referenced-files files))
  (define parts
    (filter-string (list "## Plan (Full)"
                         (if (non-empty? plan-text) plan-text "(no plan)")
                         (if (non-empty? state-text)
                             (format "## Current State\n~a" state-text)
                             #f)
                         (if (non-empty? wave-text)
                             (format "## Current Wave\n~a" wave-text)
                             #f)
                         (if (non-empty? referenced-files)
                             (format "## Referenced Files\n~a" referenced-files)
                             #f))))
  (string-join parts "\n\n"))

;; ============================================================
;; Verifier bundle
;; ============================================================

;; Verifier gets: plan + summaries + verification commands
(define (verifier-bundle artifacts files)
  (define plan-text (hash-ref artifacts 'plan ""))
  (define summaries (hash-ref artifacts 'summaries ""))
  (define verify-commands (extract-verify-commands plan-text))
  (define parts
    (filter-string (list "## Plan Summary"
                         (if (non-empty? plan-text) plan-text "(no plan)")
                         (if (non-empty? summaries)
                             (format "## Wave Summaries\n~a" summaries)
                             #f)
                         (if (non-empty? verify-commands)
                             (format "## Verification Commands\n~a"
                                     (string-join verify-commands "\n"))
                             "## Verification\nRun your test suite to verify all changes."))))
  (string-join parts "\n\n"))

;; ============================================================
;; Internal helpers
;; ============================================================

(define (non-empty? s)
  (and (string? s) (> (string-length s) 0)))

(define (filter-string parts)
  (filter values parts))

(define (extract-wave-context plan-text wave-index)
  (if (non-empty? plan-text)
      (let ([waves (parse-waves-from-markdown plan-text)])
        (define w (plan-wave-ref (gsd-plan waves "" '() '()) wave-index))
        (if w
            (format "Wave ~a: ~a\n~a"
                    (gsd-wave-index w)
                    (gsd-wave-title w)
                    (if (non-empty? (gsd-wave-root-cause w))
                        (format "Root cause: ~a" (gsd-wave-root-cause w))
                        ""))
            ""))
      ""))

(define (format-referenced-files files)
  (if (or (not files) (hash-empty? files))
      ""
      (string-join (for/list ([(path content) (in-hash files)])
                     (format "### ~a\n```\n~a\n```" path content))
                   "\n\n")))

(define (extract-verify-commands plan-text)
  (if (non-empty? plan-text)
      (let ([waves (parse-waves-from-markdown plan-text)])
        (for/list ([w waves]
                   #:when (non-empty? (gsd-wave-verify w)))
          (format "- Wave ~a: ~a" (gsd-wave-index w) (gsd-wave-verify w))))
      '()))
