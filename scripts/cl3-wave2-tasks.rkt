#lang racket/base

;; scripts/cl3-wave2-tasks.rkt — CL3 Wave 2 task fixtures (evaluation-only).
;; Each task is a real q module copied into a disposable git worktree. The
;; benchmark appends a small probe binding, edits its returned integer, and
;; checks the returned value against an oracle independent of the worker.

(require racket/format
         racket/port
         racket/string)

(provide cl3-task?
         cl3-task-name
         cl3-task-module
         cl3-task-typed?
         cl3-task-before
         cl3-task-after
         tasks
         probe-code
         write-task-source!
         probe-expression
         parse-probe)

(struct cl3-task (name module typed? before after) #:transparent)

(define tasks
  (list (cl3-task "small-pure-utility" "util/hash-utils.rkt" #f 41 42)
        (cl3-task "typed-dependency-chain" "runtime/iteration/retry-policy.rkt" #t 7 8)
        (cl3-task "gsd-decision-module" "extensions/gsd/campaign-decision.rkt" #f 3 4)))

(define begin-marker "; CL3-WAVE2 PROBE BEGIN")
(define end-marker "; CL3-WAVE2 PROBE END")

(define (probe-code task value)
  (string-append "\n"
                 begin-marker
                 "\n"
                 "(provide cl3-probe-value)\n"
                 (if (cl3-task-typed? task) "(: cl3-probe-value (-> Integer))\n" "")
                 (format "(define (cl3-probe-value) ~a)\n" value)
                 end-marker
                 "\n"))

(define (strip-probe source)
  (regexp-replace #px"\n*; CL3-WAVE2 PROBE BEGIN.*?; CL3-WAVE2 PROBE END\n*" source "\n"))

(define (write-task-source! root task value)
  (define path (build-path root (cl3-task-module task)))
  (unless (file-exists? path)
    (error 'cl3-wave2 "task module missing: ~a" path))
  (define source (call-with-input-file path port->string))
  (call-with-output-file path
                         (lambda (out)
                           (display (string-append (strip-probe source) (probe-code task value)) out))
                         #:exists 'truncate/replace)
  path)

(define (probe-expression root task)
  (define path (path->string (build-path root (cl3-task-module task))))
  (format (string-append
           "(call-with-values (lambda () ((dynamic-require (list 'file ~s) 'cl3-probe-value))) "
           "(lambda (v) (list v (current-memory-use 'cumulative))))")
          path))

(define (parse-probe text)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define datum (call-with-input-string (string-trim text) read))
    (if (and (list? datum) (= 2 (length datum)) (integer? (cadr datum)))
        (cons (car datum) (cadr datum))
        #f)))
