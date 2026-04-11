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

(require racket/string
         (only-in "../tool.rkt"
                  make-success-result make-error-result
                  exec-context? exec-context-working-directory)
         "../../sandbox/subprocess.rkt"
         "../../sandbox/limits.rkt")

;; Default timeout in seconds
(define DEFAULT-TIMEOUT-SECONDS 120)

(provide tool-bash)

;; --------------------------------------------------
;; Result helpers
;; --------------------------------------------------

(define (err msg)
  (make-error-result msg))

;; --------------------------------------------------
;; Main tool function
;; --------------------------------------------------

(define (tool-bash args [exec-ctx #f])
  (define command (hash-ref args 'command #f))
  (cond
    [(not command)
     (err "Missing required argument: command")]
    [(not (non-empty-string? command))
     (err "command must be a non-empty string")]
    [else
     (define timeout-secs (hash-ref args 'timeout DEFAULT-TIMEOUT-SECONDS))
     (define work-dir (hash-ref args 'working-directory #f))

     (define result
       (run-subprocess
        "/bin/sh"
        #:args (list "-c" command)
        #:limits (exec-limits timeout-secs 1048576 536870912 10)
        #:directory (or work-dir
                        (and exec-ctx (exec-context-working-directory exec-ctx))
                        (current-directory))))

     (define stdout (subprocess-result-stdout result))
     (define stderr-out (subprocess-result-stderr result))
     ;; Combine stdout and stderr; include stderr if non-empty
     (define raw-combined
       (string-trim
        (string-append stdout
                       (if (string=? stderr-out "")
                           ""
                           (string-append "\n" stderr-out)))))
     ;; When output is empty, provide diagnostic feedback to the LLM
     ;; so it understands the command produced nothing and can change strategy
     (define combined
       (if (string=? raw-combined "")
           "(Command produced no output. The command may have completed without producing any output, or the output was empty. Consider
               checking: the command syntax, file paths, available tools, or try a different approach.)"
           raw-combined))
     (make-success-result
      (list (hasheq 'type "text"
                    'text combined))
      (hasheq 'exit-code (subprocess-result-exit-code result)
              'timed-out? (subprocess-result-timed-out? result)
              'duration-ms (subprocess-result-elapsed-ms result)
              'command command))]))
