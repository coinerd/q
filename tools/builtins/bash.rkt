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
         (only-in "../../runtime/settings.rkt"
                  sandbox-enabled?
                  sandbox-timeout
                  sandbox-memory-limit
                  sandbox-max-output
                  sandbox-max-processes)
         (only-in "../../util/path-helpers.rkt" expand-home-path)
         (only-in "../../util/truncation.rkt" truncate-output))

(provide tool-bash
         current-warn-on-destructive
         current-block-destructive
         current-extra-destructive-patterns
         destructive-command?
         destructive-patterns)

;; Default timeout in seconds (used when no settings available)
(define DEFAULT-TIMEOUT-SECONDS 120)

;; Destructive command patterns (SEC-03, #449)
;; Each pattern uses anchors (^|[&;|\n]) or word boundaries to avoid
;; false positives on benign strings like `echo "shutdown notice"`.
;; Patterns are matched case-insensitively against the full command.
(define destructive-patterns
  ;; Recursive / forceful deletion — anchored at command start
  (list #rx"^rm[ ]+.*-[a-zA-Z]*r.*-[a-zA-Z]*f" ;; rm with -r and -f flags
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
        ))

;; User-configurable override patterns (loaded from settings).
;; When non-#f, these replace the default destructive-patterns.
(define current-extra-destructive-patterns (make-parameter #f))

;; Check if a command matches any destructive pattern.
;; Uses regexp matching for token-awareness to avoid false positives.
(define (destructive-command? command)
  (define lower (string-downcase command))
  ;; Use user-configured patterns if provided, otherwise defaults
  (define patterns (or (current-extra-destructive-patterns) destructive-patterns))
  (for/or ([pattern (in-list patterns)])
    (regexp-match? pattern lower)))

;; Optional settings parameter for destructive command warning.
;; When #t (default), emit a warning to stderr before executing.
;; Can be set to #f to suppress warnings.
(define current-warn-on-destructive (make-parameter #t))

;; Optional settings parameter for destructive command blocking (SEC-01).
;; When #t, destructive commands return an error result instead of executing.
;; When #f (default), commands execute (with optional warning).
;; Blocking takes priority over warning.
(define current-block-destructive (make-parameter #f))

;; Resolve exec-limits from settings (if provided) or defaults.
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
     ;; Destructive command handling (SEC-01 / SEC-03)
     (cond
       ;; Block takes priority
       [(and (current-block-destructive) (destructive-command? command))
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
        ;; When output is empty, provide diagnostic feedback to the LLM
        ;; so it understands the command produced nothing and can change strategy
        (define combined
          (if (string=? raw-combined "")
              (string-append "(Command produced no output. "
                             "The command may have completed without producing any output, "
                             "or the output was empty. Consider checking: "
                             "the command syntax, file paths, available tools, "
                             "or try a different approach.)")
              (truncate-output raw-combined)))
        (make-success-result (list (hasheq 'type "text" 'text combined))
                             (hasheq 'exit-code
                                     (subprocess-result-exit-code result)
                                     'timed-out?
                                     (subprocess-result-timed-out? result)
                                     'duration-ms
                                     (subprocess-result-elapsed-ms result)
                                     'command
                                     command))])]))
