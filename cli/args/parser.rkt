#lang racket/base

;; q/cli/args/parser.rkt — CLI argument parser
;;
;; Sub-module of cli/args.rkt (split for modularity).
;; Provides parse-cli-args, apply-flag, handle-positional.

(require racket/match
         racket/string
         "flags.rkt" ; flag-def, FLAG-DEFINITIONS, lookup tables, acc helpers
         "config-builder.rkt" ; cli-config, make-initial-acc, acc->cli-config
         )

(provide parse-cli-args
         apply-flag
         handle-positional)

;; Helper: construct a "help" config (used for parse errors and --help)
(define (make-help-config)
  (cli-config 'help #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f #f #f #f #f))

;; ============================================================
;; Pure: parse-cli-args
;; ============================================================

;; Parse a vector of argument strings into a cli-config.
;; Returns a cli-config with command='help if parsing fails.

(define (parse-cli-args [args (current-command-line-arguments)])
  (define vec
    (if (vector? args)
        args
        (list->vector args)))
  (define n (vector-length vec))

  (let loop ([i 0]
             [acc (make-initial-acc)])
    (cond
      ;; -- Done --
      [(>= i n)
       ;; Handle sessions subcommand finalization
       (define cmd (acc-ref acc 'command))
       (cond
         ;; sessions was set -- subcommand was already consumed inline
         [(eq? cmd 'sessions) acc]
         [else (acc->cli-config acc)])]

      ;; -- Known flag? --
      [(hash-ref long-flag-table (vector-ref vec i) #f)
       =>
       (lambda (fd) (apply-flag fd vec n i acc loop))]

      [(hash-ref short-flag-table (vector-ref vec i) #f)
       =>
       (lambda (fd) (apply-flag fd vec n i acc loop))]

      ;; -- Positional argument (prompt) or subcommand --
      ;; Unknown flag -> help
      [(string-prefix? (vector-ref vec i) "--") (make-help-config)]

      [else (handle-positional vec n i acc loop)])))

;; Apply a flag-def: consume the arg at position i, possibly the next arg too,
;; call the apply-fn, and continue the loop.
(define (apply-flag fd vec n i acc loop)
  (define t (flag-def-type fd))
  (cond
    [(eq? t 'boolean)
     (let ([result ((flag-def-apply-fn fd) #t acc)])
       (cond
         [(eq? result 'help) (make-help-config)]
         [(eq? result 'version)
          (cli-config 'version #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f #f #f #f #f)]
         [else (loop (add1 i) result)]))]
    [(or (eq? t 'string) (eq? t 'integer) (eq? t 'accumulate))
     (if (< (add1 i) n)
         (let ([raw-val (vector-ref vec (add1 i))])
           (cond
             [(eq? t 'integer)
              (define num (string->number raw-val))
              (if (and num (exact-positive-integer? num))
                  (loop (+ i 2) ((flag-def-apply-fn fd) num acc))
                  (make-help-config))]
             [else (loop (+ i 2) ((flag-def-apply-fn fd) raw-val acc))]))
         (make-help-config))]
    [(eq? t 'mode) (loop (add1 i) ((flag-def-apply-fn fd) #t acc))]
    ;; Shouldn't happen
    [else (make-help-config)]))

;; Handle positional arguments and subcommands.
(define (handle-positional vec n i acc loop)
  (define arg (vector-ref vec i))
  (cond
    ;; "doctor" subcommand
    [(equal? arg "doctor") (loop (add1 i) (acc-set acc 'command 'doctor))]
    ;; "init" subcommand
    [(equal? arg "init") (loop (add1 i) (acc-set acc 'command 'init))]
    ;; "sessions" subcommand -- q sessions <list|info|delete|verify> [args...]
    [(equal? arg "sessions")
     (if (< (add1 i) n)
         (let ([sub (vector-ref vec (add1 i))])
           (define sub-sym
             (cond
               [(equal? sub "list") 'list]
               [(equal? sub "info") 'info]
               [(equal? sub "delete") 'delete]
               [(equal? sub "verify") 'verify]
               [else #f]))
           (if sub-sym
               (let ([rest (for/list ([j (in-range (+ i 2) n)])
                             (vector-ref vec j))])
                 (cli-config 'sessions
                             #f
                             #f
                             #f
                             'interactive
                             #f
                             #f
                             #f
                             10
                             #f
                             '()
                             #f
                             sub-sym
                             rest
                             #f
                             #f
                             #f
                             #f
                             #f
                             #f))
               (make-help-config)))
         (make-help-config))]
    ;; "verify-session" top-level command -- q verify-session <path> [--repair]
    [(equal? arg "verify-session")
     (if (< (add1 i) n)
         (let* ([path-arg (vector-ref vec (add1 i))]
                [rest-args (for/list ([j (in-range (+ i 2) n)])
                             (vector-ref vec j))])
           (cli-config 'sessions
                       #f
                       #f
                       #f
                       'interactive
                       #f
                       #f
                       #f
                       10
                       #f
                       '()
                       #f
                       'verify
                       (cons path-arg rest-args)
                       #f
                       #f
                       #f
                       #f
                       #f
                       #f))
         (make-help-config))]
    ;; Default: treat as prompt
    ;; Second positional -- ignore
    [(acc-ref acc 'prompt #f) (loop (add1 i) acc)]
    [else (loop (add1 i) (acc-set (acc-set acc 'prompt arg) 'command 'prompt))]))
