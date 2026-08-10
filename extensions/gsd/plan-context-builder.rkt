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
         "plan-types.rkt"
         "effect-ports.rkt"
         "composition-root.rkt")

(provide build-enriched-plan-ctx
         infer-capabilities-from-files
         infer-capabilities-from-tasks
         get-diff-excerpt
         get-test-summary
         current-git-root
         find-git-root-dir
         FILE-EXTENSION->CAPABILITY)

;; ============================================================
;; Git Root Resolution (moved before parameter for forward reference)
;; ============================================================

(define (find-git-root-dir start-dir)
  (define start-path
    (path->complete-path (if (path? start-dir)
                             start-dir
                             (string->path start-dir))))
  (define (has-git? dir)
    (define git-marker (build-path dir ".git"))
    (or (directory-exists? git-marker) (file-exists? git-marker)))
  (define q-sub (build-path start-path "q"))
  (cond
    [(has-git? start-path) start-path]
    [(and (directory-exists? q-sub) (has-git? q-sub)) q-sub]
    [else
     (let loop ([dir start-path])
       (cond
         [(has-git? dir) dir]
         [else
          (define-values (parent _sub _dir?) (split-path dir))
          (if (and parent (path? parent) (not (equal? parent dir)))
              (loop parent)
              #f)]))]))

;; ============================================================
;; Parameterized git root (W1: cwd migration)
;; ============================================================

;; A parameter that controls the git root used by plan-context-builder.
;; Defaults to (find-git-root-dir (current-directory)).
;; Callers can parameterize this for testing or to override resolution.
(define current-git-root
  (make-parameter (find-git-root-dir (current-directory))
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
;; Returns a string (possibly empty when no changes or no git).
(define (get-diff-excerpt base-dir files)
  (if (or (null? files) (not base-dir))
      ""
      (with-handlers ([exn:fail? (lambda (_) "")])
        (define git-port (gsd-effect-ports-git (current-gsd-effect-ports)))
        (define git-root (or (current-git-root) ((gsd-git-port-find-root git-port) base-dir)))
        (cond
          [(not git-root) ""]
          [else ((gsd-git-port-head-summary git-port) git-root files)]))))

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