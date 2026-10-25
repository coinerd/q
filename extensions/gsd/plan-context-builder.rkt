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
         racket/list
         racket/path
         racket/system
         "plan-types.rkt"
         "effect-ports.rkt"
         "composition-root.rkt"
         "git-root-layout.rkt")

(provide build-enriched-plan-ctx
         infer-capabilities-from-files
         infer-capabilities-from-tasks
         get-diff-excerpt
         get-test-summary
         current-git-root
         find-git-root-dir
         find-git-root
         git-available?
         FILE-EXTENSION->CAPABILITY)

;; ============================================================
;; Git Root Resolution (moved before parameter for forward reference)
;; ============================================================

;; R76 (BUG-0076): canonical resolution delegates to the shared layout-aware
;; helper (git-root-layout.rkt) — Git's own rev-parse answers, never marker
;; sniffing. Supported layouts: a direct repository (start inside one), and
;; the nested-q two-tier workspace ONLY when start/q is a real nested
;; repository rooted exactly at start/q. A decoy outer repository alongside
;; an independent nested q refuses (ambiguous identity); a .git marker with
;; a broken/unusable repository refuses; no repository at all still yields
;; #f (unchanged no-repo contract). Refusals raise exn:fail:git-root with
;; root+reason diagnostics; callers that must tolerate them catch the typed
;; error and fail closed.
(define (find-git-root-dir start-dir)
  (delivery-root-or-raise start-dir))

;; F-7 variant: identical resolution, with the process-level
;; current-git-root parameter as the last-resort explicit fallback (an
;; operator-configured override; marker-checked as before). Refusals still
;; raise — the fallback exists only for the no-repository case.
(define (find-git-root start-dir)
  (define walked (find-git-root-dir start-dir))
  (if walked
      walked
      (let ([param-root (current-git-root)])
        (if (and param-root (git-root-marker? param-root)) param-root #f))))

(define (git-available? base-dir)
  (define git (find-executable-path "git"))
  (define (inside-work-tree? dir)
    (and git
         dir
         (directory-exists? dir)
         (let ([stdout (open-output-string)]
               [stderr (open-output-string)])
           (with-handlers ([exn:fail? (lambda (_) #f)])
             (define exit-code
               (parameterize ([current-output-port stdout]
                              [current-error-port stderr])
                 (system*/exit-code git "-C" dir "rev-parse" "--is-inside-work-tree")))
             (and (zero? exit-code) (string=? (string-trim (get-output-string stdout)) "true"))))))
  (and base-dir (or (inside-work-tree? base-dir) (inside-work-tree? (build-path base-dir "q"))) #t))

;; ============================================================
;; Parameterized git root (W1: cwd migration)
;; ============================================================

;; A parameter that controls the git root used by plan-context-builder.
;; Defaults to (find-git-root-dir (current-directory)).
;; Callers can parameterize this for testing or to override resolution.
(define current-git-root
  (make-parameter
   (with-handlers ([exn:fail? (lambda (_) #f)])
     (find-git-root-dir (current-directory)))
   (lambda (val)
                    (cond
                      [(path? val) val]
                      [(string? val) (string->path val)]
                      [(not val) val]
                      [else (raise-argument-error 'current-git-root "path? string? or #f" val)]))))

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
;; v0.99.83 W1 (F-7): Fixed stderr leak — system* wrote git errors to the terminal.
;; v0.99.90 W0 (#9231): Delegate to the injected git port through
;; current-gsd-effect-ports (composition-root.rkt); the production adapter
;; preserves the exact subprocess command, trimming, and truncation. The
;; current-git-root parameter remains the public override seam. Root
;; resolution also delegates to the port's find-root so system-adapters is
;; the single production implementation (find-git-root-dir stays as the
;; exported compatibility/test API).
;; current-git-root parameter remains the public override seam. Root
;; resolution also delegates to the port's find-root so system-adapters is
;; the single production implementation (find-git-root-dir stays as the
;; exported compatibility/test API).
;; R76 (BUG-0076): resolution goes through the shared layout-aware helper
;; (the port's find-root remains the last fallback for injected ports), and
;; every declared target must resolve INSIDE the selected delivery root
;; before any git invocation. An outside or escaping target raises the
;; typed git-root refusal naming the root — the refusal deliberately is NOT
;; swallowed into an empty excerpt, so a target that would silently read
;; another root fails loudly instead. Pure path validation happens BEFORE
;; the guarded git call.
;; Returns a string (possibly empty when no changes or no git).
(define (get-diff-excerpt base-dir files)
  (if (or (null? files) (not base-dir))
      ""
      (let ()
        (define git-port (gsd-effect-ports-git (current-gsd-effect-ports)))
        (define git-root
          (or (current-git-root)
              (delivery-root-or-raise base-dir)
              ((gsd-git-port-find-root git-port) base-dir)))
        (cond
          [(not git-root) ""]
          [else
           (for ([f (in-list files)])
             (require-delivery-target-in-root! git-root f #:base base-dir #:what "wave diff target"))
           (with-handlers ([exn:fail? (lambda (_) "")])
             ((gsd-git-port-head-summary git-port) git-root files))]))))

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
