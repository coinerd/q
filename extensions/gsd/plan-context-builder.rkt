#lang racket/base

;; extensions/gsd/plan-context-builder.rkt — Plan context enrichment
;; STABILITY: evolving
;;
;; v0.99.23 B-1/B-2: Enriches the plan-context hash with real wave data.
;; Previously, command-handlers.rkt constructed plan-ctx with empty strings
;; and empty lists, making the verifier blind and disabling §6.1 (skip
;; heuristic) and §6.2 (dynamic risk threshold).
;;
;; v0.99.24 W1: Enhanced capability inference with:
;;   - FILE-EXTENSION->CAPABILITY table (detects shell-exec from .sh, etc.)
;;   - infer-capabilities-from-tasks (regex-based shell/git detection)
;;   - get-test-summary (descriptive stub or cached file)

(require racket/string
         racket/port
         racket/system
         racket/list
         "plan-types.rkt")

(provide build-enriched-plan-ctx
         infer-capabilities-from-files
         infer-capabilities-from-tasks
         get-diff-excerpt
         get-test-summary
         FILE-EXTENSION->CAPABILITY)

;; ============================================================
;; Plan Context Enrichment
;; ============================================================

;; v0.99.24 W1: File extension → capability mapping table.
;; Maps known file extensions to the capability required to modify them.
;; Conservative: only infers from structural file types, not content.
;; Easy to extend — just add new (extension . capability) pairs.
(define FILE-EXTENSION->CAPABILITY
  '((".rkt" . file-write) (".rktl" . file-write)
                          (".scrbl" . file-write)
                          (".ss" . file-write)
                          (".scm" . file-write)
                          (".sh" . shell-exec)
                          (".py" . file-write)
                          (".md" . file-write)
                          (".json" . file-write)
                          (".yaml" . file-write)
                          (".yml" . file-write)))

;; Infer capabilities from the file paths in a wave.
;; v0.99.24 W1: Enhanced — uses FILE-EXTENSION->CAPABILITY table.
;; Returns a list of capability symbols (possibly empty).
;; Uses remove-duplicates to avoid duplicate capabilities from
;; multiple files of the same extension.
(define (infer-capabilities-from-files files)
  (remove-duplicates (for/fold ([caps '()]) ([f (in-list files)])
                       (for/fold ([cs caps]) ([pair (in-list FILE-EXTENSION->CAPABILITY)])
                         (if (string-suffix? f (car pair))
                             (cons (cdr pair) cs)
                             cs)))))

;; Infer capabilities from a wave's task descriptions.
;; v0.99.24 W1: Uses regex heuristics on task name + action text.
;; Detects shell-exec and git-write from natural language descriptions.
;; Returns a list of capability symbols (possibly empty).
(define (infer-capabilities-from-tasks wave)
  (if (not wave)
      '()
      (let* ([tasks (gsd-wave-tasks wave)]
             [task-text (string-join
                         (for/list ([t (in-list tasks)])
                           (string-append (or (gsd-task-name t) "") " " (or (gsd-task-action t) "")))
                         " ")]
             [caps '()])
        (when (regexp-match? #rx"(?i:shell|bash|command|exec|run )" task-text)
          (set! caps (cons 'shell-exec caps)))
        (when (regexp-match? #rx"(?i:git|commit|push|merge)" task-text)
          (set! caps (cons 'git-write caps)))
        caps)))

;; Get a compact git diff excerpt for the wave's files.
;; v0.99.24 C-3: Fixed dead code — file paths were computed but never passed to git.
;; Uses `git show --stat HEAD -- <files>` instead of `git diff --stat` because
;; at /wave-done time, changes are already committed.
;; Returns a string (possibly empty when no changes or no git).
(define (get-diff-excerpt base-dir files)
  (if (or (null? files) (not base-dir))
      ""
      (with-handlers ([exn:fail? (lambda (_) "")])
        (define output
          (parameterize ([current-directory (if (path? base-dir)
                                                base-dir
                                                (string->path base-dir))])
            (with-output-to-string (lambda ()
                                     (with-handlers ([exn:fail? void])
                                       (apply system*
                                              (find-executable-path "git")
                                              "show"
                                              "--stat"
                                              "--oneline"
                                              "HEAD"
                                              "--"
                                              files))))))
        (define trimmed (string-trim output))
        (if (> (string-length trimmed) 2000)
            (string-append (substring trimmed 0 2000) "...")
            trimmed))))

;; Attempt to read test summary from session artifacts.
;; v0.99.24 W1: Checks for cached test results file. Returns a descriptive
;; message when no data is available. Never throws.
(define (get-test-summary base-dir)
  (define test-log (and base-dir (build-path base-dir ".planning" "test-results.txt")))
  (cond
    [(and test-log (file-exists? test-log))
     (with-handlers ([exn:fail? (lambda (_) "test results unreadable")])
       (string-trim (call-with-input-file test-log port->string)))]
    [else "no test results available for this wave"]))

;; Build an enriched plan-context hash for the verification gate.
;;
;; This replaces the static empty-strings plan-ctx that existed before.
;; The verifier LLM now receives:
;;   - Real plan summary (wave titles)
;;   - Real file list from the wave's plan data
;;   - Real (inferred) capabilities from both files and tasks
;;   - Diff excerpt from git (when available)
;;   - Test summary (descriptive message or cached results)
;;
;; v0.99.24 W1: Now combines file-based AND task-based capability inference.
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
  (define file-caps (infer-capabilities-from-files wave-files))
  (define task-caps (infer-capabilities-from-tasks wave))
  (define capabilities-used (remove-duplicates (append file-caps task-caps)))
  (define diff-excerpt (get-diff-excerpt base-dir wave-files))
  (define test-summary (get-test-summary base-dir))
  (hasheq 'plan-summary
          plan-summary
          'wave-name
          (format "W~a: ~a" wave-idx wave-title)
          'files-changed
          wave-files
          'test-summary
          test-summary
          'diff-excerpt
          diff-excerpt
          'capabilities-used
          capabilities-used))
