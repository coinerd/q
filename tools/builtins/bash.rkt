#lang racket/base

;; tools/builtins/bash.rkt — subprocess execution via sandbox/subprocess.rkt
;;
;; Exports:
;;   tool-bash : (hash [exec-ctx]) -> tool-result?
;;   Arguments:
;;     command            (string)  — the shell command to run
;;     timeout            (number, optional) — timeout in seconds
;;     working-directory  (string, optional) — working dir for subprocess
;;   Returns: tool-result with output or error
;;
;; Security considerations:
;;   - Commands run in the user's shell environment (SEC-20)
;;   - Environment is sanitized: API keys, tokens, passwords stripped (SEC-05)
;;   - Working directory is confined to project root in safe-mode (SEC-09)
;;   - Process count limits prevent fork bombs (SEC-12)
;;   - Timeout prevents infinite hangs (default 120s)
;;   - Output is truncated at max-output-bytes to prevent memory exhaustion
;;
;; GC-02: Sandbox limits now read from runtime/settings when available.
;; Process tracking (SEC-12) is wired into every invocation.

(require racket/string
         (only-in "../tool.rkt"
                  make-success-result
                  make-error-result
                  exec-context?
                  exec-context-working-directory
                  exec-context-runtime-settings)
         "../../sandbox/subprocess.rkt"
         "../../sandbox/limits.rkt"
         (only-in "../../util/sandbox-config.rkt"
                  sandbox-enabled?
                  sandbox-timeout
                  sandbox-memory-limit
                  sandbox-max-output
                  sandbox-max-processes)
         (only-in "../../util/path-helpers.rkt" expand-home-path)
         (only-in "../../util/truncation.rkt" truncate-output)
         (only-in "../../util/safe-mode-predicates.rkt" safe-mode?))

(provide tool-bash
         current-warn-on-destructive
         current-block-destructive
         current-extra-destructive-patterns
         destructive-command?
         destructive-patterns
         current-execution-policy
         current-allowed-commands
         execution-policy-allows?
         high-risk-command?)

;; Default timeout in seconds (used when no settings available)
(define DEFAULT-TIMEOUT-SECONDS 120)

;; ── Execution policy (RA-1a, v0.24.7) ──
;; Controls which commands are allowed to execute.
;; 'warn      — current behavior: warn on destructive, allow all
;; 'block     — block destructive commands (same as safe-mode)
;; 'allowlist — only commands in current-allowed-commands execute
(define current-execution-policy (make-parameter 'warn))

;; When execution-policy is 'allowlist, only these base commands execute.
;; Configurable via config.json "execution-policy".allowed list.
(define current-allowed-commands
  (make-parameter '("git" "ls"
                          "cat"
                          "grep"
                          "find"
                          "raco"
                          "racket"
                          "echo"
                          "mkdir"
                          "cp"
                          "mv"
                          "diff"
                          "head"
                          "tail"
                          "wc"
                          "sort"
                          "awk"
                          "sed"
                          "make")))

;; Extract base command (first word) from a shell command string.
(define (extract-base-command command)
  (define trimmed (string-trim command))
  (define space-idx
    (for/first ([c (in-string trimmed)]
                [i (in-naturals)]
                #:when (char=? c #\space))
      i))
  (if space-idx
      (substring trimmed 0 space-idx)
      trimmed))

;; Check if command is allowed under current execution policy.
;; Returns #t if allowed, #f if blocked.
(define (execution-policy-allows? command)
  (define policy (current-execution-policy))
  (case policy
    [(warn block) #t] ; warn/block handled by destructive checks
    [(allowlist)
     (define base (extract-base-command command))
     (member base (current-allowed-commands))]
    [else #t])) ; unknown policy defaults to allow

;; Destructive command patterns (SEC-03, #449)
;; Each pattern uses anchors (^|[&;|\n]) or word boundaries to avoid
;; false positives on benign strings like `echo "shutdown notice"`.
;; Patterns are matched case-insensitively against the full command.
(define destructive-patterns
  ;; Recursive / forceful deletion — anchored at command start
  (list
   #rx"^rm[ ]+.*-[a-zA-Z]*r.*-[a-zA-Z]*f" ;; rm with -r and -f flags
   #rx"^rm[ ]+-rf[ ]+" ;; rm -rf shorthand
   #rx"^rm[ ]+-fr[ ]+" ;; rm -fr shorthand
   #rx"^rm[ ]+-r[ ]+-f[ ]+" ;; rm -r -f
   #rx"^rmdir[ ]+" ;; rmdir
   ;; Also match after pipe/semicolon/&& operators
   #rx"[|;&][ ]*rm[ ]+-rf[ ]+" ;; piped rm -rf
   ;; Disk/filesystem destruction
   #rx"^mkfs[.]" ;; mkfs.*
   #rx"^dd[ ]+if=" ;; dd if=
   #rx"^dd[ ]+.*of=/dev/" ;; dd of=/dev/
   #rx">[ ]*/dev/sd" ;; device file write
   ;; System commands — anchored at command start
   #rx"^shutdown([ ]|$)" ;; shutdown
   #rx"^reboot([ ]|$)" ;; reboot
   #rx"^format[ ]+[A-Za-z]:" ;; Windows format
   #rx"^del[ ]+/" ;; Windows del
   ;; Permission destruction
   #rx"^chmod[ ]+-r[ ]+777[ ]+/" ;; recursive 777 on root
   #rx"^chmod[ ]+000[ ]+/" ;; lock out root
   ;; Pipe-to-shell (must be at pipe boundary)
   #rx"[|][ ]*sh[ ]*$" ;; | sh
   #rx"[|][ ]*bash[ ]*$" ;; | bash
   ;; Critical system file overwrite
   #rx">[ ]*/etc/passwd" ;; passwd overwrite
   #rx">[ ]*/etc/shadow" ;; shadow overwrite
   ;; Git destructive
   #rx"^git[ ]+push[ ]+.*--force" ;; force push
   ;; Root directory operations
   #rx"^mv[ ]+/[ ]+" ;; mv /
   ;; Download-to-shell combos (SEC-A)
   #rx"curl[ ]+.*[|][ ]*sh[ ]*$" ;; curl ... | sh
   #rx"wget[ ]+.*[|][ ]*sh[ ]*$" ;; wget ... | sh
   #rx"eval[ ]+\"[$][(]curl" ;; eval "$(curl ...)"
   #rx"source[ ]+/tmp/" ;; source from temp
   ;; SEC-01 (v0.22.0): Bypass-vector patterns — encoding tricks,
   ;; substitution, and indirection that evade simple pattern matching.
   #rx"[|].*base64" ;; base64 decode pipe bypass
   #rx"[|].*xxd" ;; xxd hex decode pipe bypass
   #rx"\\$\\(" ;; $(...) command substitution
   #rx"`[^`]+`" ;; AUDIT-01: paired backtick command substitution (avoids false positives on lone backticks)
   #rx"^eval[ ]+" ;; eval indirection
   #rx"^exec[ ]+" ;; exec replacement
   ))

;; User-configurable extra patterns (loaded from settings).
;; When non-#f, these EXTEND the default destructive-patterns (SEC-A).
(define current-extra-destructive-patterns (make-parameter #f))

;; Check if a command matches any destructive pattern.
;; Uses regexp matching for token-awareness to avoid false positives.
(define (destructive-command? command)
  (define lower (string-downcase command))
  ;; Extend defaults with extra patterns (never replace)
  (define patterns
    (if (current-extra-destructive-patterns)
        (append destructive-patterns (current-extra-destructive-patterns))
        destructive-patterns))
  (for/or ([pattern (in-list patterns)])
    (regexp-match? pattern lower)))

;; ── High-risk patterns (RA-1b, v0.24.7) ──
;; Subset of destructive-patterns that are especially dangerous.
;; When in warn-only mode, these get a stronger notice in tool output.
(define high-risk-patterns
  (list #rx"^rm[ ]+-rf[ ]+" ;; rm -rf
        #rx"^rm[ ]+-fr[ ]+" ;; rm -fr
        #rx"^rm[ ]+.*-[a-zA-Z]*r.*-[a-zA-Z]*f" ;; rm with -r and -f
        #rx"^mkfs[.]" ;; mkfs.*
        #rx"^dd[ ]+.*of=/dev/" ;; dd of=/dev/
        #rx"^format[ ]+[A-Za-z]:" ;; Windows format
        #rx">[ ]*/etc/passwd" ;; passwd overwrite
        #rx">[ ]*/etc/shadow")) ;; shadow overwrite

;; Check if a command matches any high-risk pattern.
(define (high-risk-command? command)
  (define lower (string-downcase command))
  (for/or ([pattern (in-list high-risk-patterns)])
    (regexp-match? pattern lower)))

;; Optional settings parameter for destructive command warning.
;; When #t (default), emit a warning to stderr before executing.
;; Can be set to #f to suppress warnings.
(define current-warn-on-destructive (make-parameter #t))

;; Optional settings parameter for destructive command blocking (SEC-01).
;; When #t, destructive commands return an error result instead of executing.
;; When #f (default), commands execute (with optional warning).
;; Blocking takes priority over warning.
;; SEC-13 (v0.22.0): Default now defers to safe-mode.
;; When 'safe-mode-default, resolves to (safe-mode?) at call time.
;; Explicitly setting #t/#f overrides this behavior.
(define current-block-destructive (make-parameter 'safe-mode-default))

;; Resolve exec-limits from settings (if provided) or defaults.
;; settings may be a q-settings? struct or #f.
(define (resolve-exec-limits timeout-arg settings)
  (define timeout-secs
    (or timeout-arg (and settings (sandbox-timeout settings)) DEFAULT-TIMEOUT-SECONDS))
  (define max-output
    (if settings
        (sandbox-max-output settings)
        1048576))
  (define max-memory
    (if settings
        (sandbox-memory-limit settings)
        536870912))
  (define max-procs
    (if settings
        (sandbox-max-processes settings)
        10))
  (exec-limits timeout-secs max-output max-memory max-procs))

;; --------------------------------------------------
;; Main tool function
;; --------------------------------------------------

(define (tool-bash args [exec-ctx #f])
  (define command (hash-ref args 'command #f))
  (cond
    [(not command) (make-error-result "Missing required argument: command")]
    [(not (non-empty-string? command)) (make-error-result "command must be a non-empty string")]
    [else
     ;; SEC-13 (v0.22.0): Resolve safe-mode default at call time
     (define block-destructive?
       (let ([v (current-block-destructive)])
         (cond
           [(eq? v 'safe-mode-default) (safe-mode?)]
           [else v])))
     ;; Execution policy gate (RA-1a, v0.24.7)
     (cond
       [(not (execution-policy-allows? command))
        (make-error-result (format "Blocked by execution policy (allowlist mode): ~a" command))]
       ;; Block takes priority
       [(and block-destructive? (destructive-command? command))
        (make-error-result (format "Blocked destructive command: ~a" command))]
       [else
        ;; Optional warning
        (when (and (current-warn-on-destructive) (destructive-command? command))
          (fprintf (current-error-port) "WARNING: Destructive command detected: ~a~n" command))
        (define timeout-arg (hash-ref args 'timeout #f))
        (define raw-work-dir (hash-ref args 'working-directory #f))
        (define work-dir (and raw-work-dir (expand-home-path raw-work-dir)))

        ;; Resolve settings from exec-ctx runtime-settings field
        (define settings (and exec-ctx (exec-context-runtime-settings exec-ctx)))

        ;; Check if sandbox is disabled via settings
        (define use-sandbox?
          (if settings
              (sandbox-enabled? settings)
              #t))
        (when (not use-sandbox?)
          (fprintf (current-error-port) "WARNING: Sandbox disabled via settings~n"))

        ;; Track process for concurrent process limit (SEC-12)
        (track-process!)

        (define result
          (dynamic-wind (lambda () (void))
                        (lambda ()
                          (run-subprocess "/bin/sh"
                                          #:args (list "-c" command)
                                          #:limits (resolve-exec-limits timeout-arg settings)
                                          #:directory
                                          (or work-dir
                                              (and exec-ctx (exec-context-working-directory exec-ctx))
                                              (current-directory))))
                        (lambda () (untrack-process!))))

        (define stdout (subprocess-result-stdout result))
        (define stderr-out (subprocess-result-stderr result))
        ;; Combine stdout and stderr; include stderr if non-empty
        (define raw-combined
          (string-trim (string-append stdout
                                      (if (string=? stderr-out "")
                                          ""
                                          (string-append "\n" stderr-out)))))
        ;; RA-1b (v0.24.7): Inject high-risk notice into tool output
        (define high-risk-notice
          (if (and (not block-destructive?) (high-risk-command? command))
              (string-append "\n[SECURITY NOTICE] This command matched a high-risk "
                             "destructive pattern. Proceed with extreme caution.\n")
              ""))
        ;; When output is empty, provide diagnostic feedback to the LLM
        ;; so it understands the command produced nothing and can change strategy
        (define combined
          (if (string=? raw-combined "")
              (string-append high-risk-notice
                             "(Command produced no output. "
                             "The command may have completed without producing any output, "
                             "or the output was empty. Consider checking: "
                             "the command syntax, file paths, available tools, "
                             "or try a different approach.)")
              (string-append high-risk-notice (truncate-output raw-combined))))
        (make-success-result (list (hasheq 'type "text" 'text combined))
                             (hasheq 'exit-code
                                     (subprocess-result-exit-code result)
                                     'timed-out?
                                     (subprocess-result-timed-out? result)
                                     'duration-ms
                                     (subprocess-result-elapsed-ms result)
                                     'command
                                     command))])]))
