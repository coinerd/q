#lang racket/base

;; extensions/gsd/plan-context-builder.rkt — Plan context enrichment
;; STABILITY: evolving
;;
;; v0.99.23 B-1/B-2: Enriches the plan-context hash with real wave data.
;; Previously, command-handlers.rkt constructed plan-ctx with empty strings
;; and empty lists, making the verifier blind and disabling §6.1 (skip
;; heuristic) and §6.2 (dynamic risk threshold).

(require racket/string
         racket/port
         racket/system
         "plan-types.rkt")

(provide build-enriched-plan-ctx
         infer-capabilities-from-files
         get-diff-excerpt)

;; ============================================================
;; Plan Context Enrichment
;; ============================================================

;; plan-wave-ref is imported from plan-types.rkt (already provided there).

;; Infer capabilities from the file paths in a wave.
;; Conservative heuristic: only infers 'file-write when .rkt files are present.
;; Does not infer 'shell-exec or 'git-write (too unreliable from paths alone).
;; Returns a list of capability symbols (possibly empty).
(define (infer-capabilities-from-files files)
  (if (and (pair? files)
           (for/or ([f (in-list files)])
             (string-suffix? f ".rkt")))
      '(file-write)
      '()))

;; Get a compact git diff excerpt for the wave's files.
;; Returns a string (possibly empty when no changes or no git).
(define (get-diff-excerpt base-dir files)
  (if (or (null? files) (not base-dir))
      ""
      (with-handlers ([exn:fail? (lambda (_) "")])
        (define file-args (string-join files " "))
        (define port
          (parameterize ([current-directory (if (path? base-dir)
                                                base-dir
                                                (string->path base-dir))])
            (open-input-string (with-output-to-string
                                (lambda ()
                                  (with-handlers ([exn:fail? void])
                                    (system* (find-executable-path "git") "diff" "--stat" "--")))))))
        (define output (string-trim (port->string port)))
        (if (> (string-length output) 2000)
            (string-append (substring output 0 2000) "...")
            output))))

;; Build an enriched plan-context hash for the verification gate.
;;
;; This replaces the static empty-strings plan-ctx that existed before.
;; The verifier LLM now receives:
;;   - Real plan summary (wave titles)
;;   - Real file list from the wave's plan data
;;   - Real (inferred) capabilities — at least 'file-write for .rkt changes
;;   - Diff excerpt from git (when available)
(define (build-enriched-plan-ctx base-dir plan wave-idx)
  (define wave (and plan (plan-wave-ref plan wave-idx)))
  (define wave-files
    (if wave
        (gsd-wave-files wave)
        '()))
  (define wave-title
    (if wave
        (gsd-wave-title wave)
        ""))
  (define plan-summary
    (if plan
        (string-join (for/list ([w (in-list (gsd-plan-waves plan))])
                       (format "W~a: ~a" (gsd-wave-index w) (gsd-wave-title w)))
                     "\n")
        ""))
  (define capabilities-used (infer-capabilities-from-files wave-files))
  (define diff-excerpt (get-diff-excerpt base-dir wave-files))
  (hasheq 'plan-summary
          plan-summary
          'wave-name
          (format "W~a: ~a" wave-idx wave-title)
          'files-changed
          wave-files
          'test-summary
          "tests not run"
          'diff-excerpt
          diff-excerpt
          'capabilities-used
          capabilities-used))
