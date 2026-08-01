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

(require racket/contract
         racket/string
         (only-in "../tool.rkt"
                  make-success-result
                  make-error-result
                  exec-context?
                  exec-context-working-directory
                  exec-context-runtime-settings
                  tool-result?)
         "../../sandbox/subprocess.rkt"
         "../../sandbox/limits.rkt"
         (only-in "../../util/sandbox-config.rkt"
                  sandbox-enabled?
                  sandbox-timeout
                  sandbox-memory-limit
                  sandbox-max-output
                  sandbox-max-processes)
         (only-in "../../util/path/path-helpers.rkt" expand-home-path)
         (only-in "../../util/truncation.rkt" truncate-output)
         (only-in "../../util/safe-mode/safe-mode-predicates.rkt" safe-mode?)
         (only-in "bash-safety.rkt"
                  destructive-patterns
                  destructive-command?
                  high-risk-command?
                  structured-destructive-command?)
         (only-in "../shell-risk.rkt"
                  tokenize-shell-command
                  classify-shell-risks
                  shell-risk-finding-type
                  shell-risk-finding-severity))

;; Struct-based config (v0.44.2+, sole config path since v0.46.3)
(provide bash-execution-config
         bash-execution-config?
         bash-execution-config-policy
         bash-execution-config-block-destructive?
         bash-execution-config-warn-on-destructive?
         bash-execution-config-warning-port
         current-bash-execution-config
         destructive-patterns
         current-execution-policy
         current-allowed-commands
         (contract-out [make-bash-execution-config
                        (->* ()
                             (#:policy symbol?
                                       #:block? (or/c boolean? (-> boolean?))
                                       #:warn? boolean?
                                       #:warning-port (or/c output-port? #f))
                             bash-execution-config?)]
                       [effective-bash-config (-> bash-execution-config?)]
                       [destructive-command? (-> string? boolean?)]
                       [execution-policy-allows? (-> string? boolean?)]
                       [high-risk-command? (-> string? boolean?)]
                       [tool-bash (->* (hash?) ((or/c exec-context? #f)) tool-result?)])
         shell-risk-classifier-diagnostic)

;; Default timeout in seconds (used when no settings available)
(define DEFAULT-TIMEOUT-SECONDS 120)

;; ── Execution policy (RA-1a, v0.24.7) ──
;; Controls which commands are allowed to execute.
;; 'warn      — current behavior: warn on destructive, allow all
;; 'block     — block destructive commands (same as safe-mode)
;; 'allowlist — only commands in current-allowed-commands execute
;; DEPRECATED: Use current-bash-execution-config or make-bash-execution-config instead.
;; Removal target: v0.46.0.
(define current-execution-policy (make-parameter 'warn))

;; When execution-policy is 'allowlist, only these base commands execute.
;; Configurable via config.json "execution-policy" key (wired in run-modes.rkt).
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
     (and (member base (current-allowed-commands)) #t)]
    [else #t])) ; unknown policy defaults to allow

;; ── Structured risk classifier shadow mode (v0.70.3) ──
;; Compares regex-based detection with structured classifier.
;; Returns diagnostic string when they disagree, #f when they agree.
(define (shell-risk-classifier-diagnostic command)
  (define regex-destructive? (destructive-command? command))
  (define regex-high-risk? (high-risk-command? command))
  (define findings (classify-shell-risks (tokenize-shell-command command)))
  (define struct-destructive?
    (for/or ([f (in-list findings)])
      (member
       (shell-risk-finding-type f)
       '(destructive high-risk windows-destructive network-pipe command-substitution eval exec))))
  (define struct-critical?
    (for/or ([f (in-list findings)])
      (eq? (shell-risk-finding-severity f) 'critical)))
  (cond
    ;; Regex says destructive but classifier sees nothing
    [(and regex-destructive? (not struct-destructive?))
     (format
      "[CLASSIFIER-DIAG] Regex flagged '~a' as destructive but structured classifier found no risks.
"
      command)]
    ;; Classifier sees destructive but regex does not
    [(and (not regex-destructive?) struct-destructive?)
     (format "[CLASSIFIER-DIAG] Structured classifier found risks in '~a' but regex did not match.
"
             command)]
    ;; Regex says high-risk but classifier sees no critical
    [(and regex-high-risk? (not struct-critical?))
     (format
      "[CLASSIFIER-DIAG] Regex flagged '~a' as high-risk but classifier found no critical severity.
"
      command)]
    [else #f]))

;; v0.44.2 (R5): Struct-based config for per-request bash settings
(struct bash-execution-config (policy block-destructive? warn-on-destructive? warning-port)
  #:transparent)

(define (make-bash-execution-config #:policy [policy (current-execution-policy)]
                                    #:block? [block? (lambda () (safe-mode?))]
                                    #:warn? [warn? #t]
                                    #:warning-port [port #f])
  (bash-execution-config policy block? warn? port))

;; v0.44.4: Active execution config. When #f, tool-bash reads from deprecated parameters.
(define current-bash-execution-config (make-parameter #f))

;; v0.44.4: Resolve effective config from parameter or deprecated params.
(define (effective-bash-config)
  (or (current-bash-execution-config) (make-bash-execution-config)))

(define (get-warning-port)
  (current-error-port))

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
     ;; v0.44.5 (NF3): Resolve effective config (parameter or deprecated fallback)
     (define cfg (effective-bash-config))
     (define policy (bash-execution-config-policy cfg))
     (define block-destructive?
       (let ([v (bash-execution-config-block-destructive? cfg)])
         (cond
           [(procedure? v) (v)] ;; I-13: thunk resolver (safe-mode default)
           [else v])))
     (define warn-on-destructive? (bash-execution-config-warn-on-destructive? cfg))
     (define warning-port (or (bash-execution-config-warning-port cfg) (current-error-port)))
     ;; Execution policy gate (RA-1a, v0.24.7)
     (define (policy-allows? cmd)
       (case policy
         [(warn block) #t]
         [(allowlist)
          (define base (extract-base-command cmd))
          (and (member base (current-allowed-commands)) #t)]
         [else #t]))
     (cond
       [(not (policy-allows? command))
        (make-error-result (format "Blocked by execution policy (allowlist mode): ~a" command))]
       ;; Block takes priority
       [(and block-destructive? (destructive-command? command))
        (make-error-result (format "Blocked destructive command: ~a" command))]
       [else
        ;; Optional warning. Prefer structured classifier for user-visible warning-only UX:
        ;; benign command substitutions such as name=$(basename "$f") should not alarm users
        ;; when the structured classifier finds no risk.
        (define regex-destructive? (destructive-command? command))
        (define structured-destructive? (structured-destructive-command? command))
        (define lower-command (string-downcase command))
        (define benign-substitution-disagreement?
          (and regex-destructive?
               (not structured-destructive?)
               (or (regexp-match? #rx"\\$\\(" lower-command)
                   (regexp-match? #rx"`[^`]+`" lower-command))))
        (define should-warn?
          (or structured-destructive?
              (and regex-destructive? (not benign-substitution-disagreement?))))
        ;; Track destructive warning for inclusion in tool result (not stderr).
        ;; Writing to current-error-port in TUI mode corrupts the screen layout.
        (define destructive-warning
          (if (and warn-on-destructive? should-warn?)
              (format "⚠ WARNING: Destructive command detected: ~a\n" command)
              ""))
        ;; v0.70.3: Shadow diagnostics remain available through
        ;; shell-risk-classifier-diagnostic, but are not printed to stderr/TUI by default.
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
        (define sandbox-warning
          (if (not use-sandbox?) "⚠ WARNING: Sandbox disabled via settings\n" ""))

        ;; Track process for concurrent process limit (SEC-12)
        (track-process!)

        (define result
          (dynamic-wind (lambda () (void))
                        (lambda ()
                          ;; W1 v0.99.77: run under bash (not sh) for bash
                          ;; compatibility (${PIPESTATUS[0]} etc. — dash
                          ;; errors with "Bad substitution"), and enable
                          ;; process-group launch so a timed-out tool call
                          ;; can SIGKILL the whole group (F-18/F-18b).
                          (run-subprocess "/bin/bash"
                                          #:args (list "-c" command)
                                          #:process-group? #t
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
        ;; Combine all warnings + output. Warnings go in tool result (not stderr)
        ;; to avoid corrupting TUI screen layout.
        (define all-warnings (string-append destructive-warning sandbox-warning))
        ;; When output is empty, provide diagnostic feedback to the LLM
        ;; so it understands the command produced nothing and can change strategy
        (define combined
          (if (string=? raw-combined "")
              (string-append all-warnings
                             high-risk-notice
                             "(Command produced no output. "
                             "The command may have completed without producing any output, "
                             "or the output was empty. Consider checking: "
                             "the command syntax, file paths, available tools, "
                             "or try a different approach.)")
              (string-append all-warnings high-risk-notice (truncate-output raw-combined))))
        (make-success-result (list (hasheq 'type "text" 'text combined))
                             (hasheq 'exit-code
                                     (subprocess-result-exit-code result)
                                     'timed-out?
                                     (subprocess-result-timed-out? result)
                                     'duration-ms
                                     (subprocess-result-elapsed-ms result)
                                     'command
                                     command))])]))
