#!/usr/bin/env racket
#lang racket/base

;; scripts/ci/compiled-root.rkt — v1.00.30 W3 CLI
;;
;; Producer/consumer entry point for the trusted compiled-root prototype.
;; Two subcommands:
;;
;;   build  — producer: eagerly compile the current checkout modules,
;;            stage the payload, publish atomically to the final root.
;;   run    — consumer: resolve+validate the published root against the
;;            CURRENT checkout identity, then launch the target module in
;;            a subprocess that inherits the per-consumer read-only
;;            resolution mapping. Any resolution failure selects the ONE
;;            eager current-source fallback; producer-era bytes are never
;;            executed unverified.
;;
;; Usage:
;;   racket scripts/ci/compiled-root.rkt build --checkout DIR --module M [--module M2 ...]
;;          --final-dir DIR --label LABEL [--lockfile PATH]
;;   racket scripts/ci/compiled-root.rkt run --checkout DIR --module M
;;          --final-dir DIR --map-dir DIR --trusted-label L [--trusted-label L2 ...]
;;          [--lockfile PATH] [-- ARGS...]  (args after -- go to the child)

(require racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/port
         racket/string
         racket/system
         "../../ci/prepared-environment/compiled-root.rkt"
         "../../ci/prepared-environment/compiled-root-manifest.rkt")

;; Launch a child racket process running MODULE with the compiled-root
;; resolution mapping inherited via command-line flags. Returns
;; (values exit-code stdout/stderr transcript).
(define (launch-with-root root module-path extra-args)
  (define flag-expr (compiled-root-launch-arguments root))
  (define argv
    (append (list (path->string (find-executable-path "racket")))
            flag-expr
            (list "-t" (path->string module-path))
            extra-args))
  (define out-port (open-output-string))
  (define exit-code
    (parameterize ([current-output-port out-port]
                   [current-error-port out-port])
      (apply system*/exit-code (string->path (first argv)) (map string->path/args (rest argv)))))
  (values exit-code (get-output-string out-port)))

;; racket argv entries are strings; keep them verbatim.
(define (string->path/args s)
  s)

(module+ main
  (define mode (make-parameter #f))
  (define checkout (make-parameter #f))
  (define modules (make-parameter '()))
  (define final-dir (make-parameter #f))
  (define staging-dir (make-parameter #f))
  (define map-dir (make-parameter #f))
  (define label (make-parameter "unknown-producer"))
  (define trusted-labels (make-parameter '()))
  (define lockfile (make-parameter #f))
  ;; Raw argv: the child-argument tail after a literal "--" is captured
  ;; first, because command-line consumes the separator and would merge
  ;; the child args into the positional list.
  (define raw-argv (vector->list (current-command-line-arguments)))
  (define (split-dashdash lst)
    (cond
      [(null? lst) (values '() '())]
      [(equal? (car lst) "--") (values '() (cdr lst))]
      [else
       (define-values (head tail) (split-dashdash (cdr lst)))
       (values (cons (car lst) head) tail)]))
  (define-values (flag-argv child-args) (split-dashdash raw-argv))
  ;; Racket's command-line stops flag parsing at the first non-flag
  ;; argument, so the documented `build --checkout DIR ...` order would
  ;; leave every flag unparsed (every required option reported missing).
  ;; command-line accepts #:argv, so move the leading subcommand token to
  ;; the end: flags first, subcommand last.
  (define ordered-argv
    (if (and (pair? flag-argv) (member (car flag-argv) '("build" "run")))
        (append (cdr flag-argv) (list (car flag-argv)))
        flag-argv))

  (define args
    (command-line
     #:program "compiled-root"
     #:argv ordered-argv
     #:once-each ["--checkout" dir "checkout root (producer source of truth)" (checkout dir)]
     ["--module" m "module relative to checkout (repeatable)" (modules (append (modules) (list m)))]
     ["--final-dir" dir "published immutable root directory" (final-dir dir)]
     ["--staging-dir" dir "staging directory for atomic publish" (staging-dir dir)]
     ["--map-dir" dir "per-consumer read-only mapping directory" (map-dir dir)]
     ["--label" l "producer identity label" (label l)]
     ["--trusted-label"
      l
      "accepted producer label (repeatable)"
      (trusted-labels (append (trusted-labels) (list l)))]
     ["--lockfile" p "lockfile checked into root identity" (lockfile p)]
     #:args rest
     rest))

  (define cmd (and (pair? args) (member (first args) '("build" "run")) (first args)))
  (unless cmd
    (error 'compiled-root "usage: compiled-root.rkt build|run [options]; got: ~a" args))

  (define (require-opt! p name)
    (unless (p)
      (error 'compiled-root "missing required option --~a" name))
    (p))

  (match cmd
    ["build"
     (define co (require-opt! checkout "checkout"))
     (define ms (modules))
     (when (null? ms)
       (error 'compiled-root "build requires at least one --module"))
     (define fd (require-opt! final-dir "final-dir"))
     (define sd
       (or (staging-dir) (path->string (make-temporary-file "compiled-root-stage~a" 'directory))))
     (define manifest
       (build-compiled-root! #:checkout co
                             #:modules ms
                             #:staging-dir sd
                             #:producer-label (label)
                             #:producer-trusted? #t
                             #:lockfile (lockfile)))
     (publish-compiled-root! sd fd manifest)
     (printf "published compiled root: ~a (reason: ~a)\n" fd (reason->string #t))]
    ["run"
     (define co (require-opt! checkout "checkout"))
     (define ms (modules))
     (when (null? ms)
       (error 'compiled-root "run requires at least one --module"))
     (define fd (require-opt! final-dir "final-dir"))
     (define md (require-opt! map-dir "map-dir"))
     (when (null? (trusted-labels))
       (error 'compiled-root "run requires at least one --trusted-label"))
     (define root
       (resolve-compiled-root! #:final-dir fd
                               #:checkout co
                               #:trusted-producer-labels (trusted-labels)
                               #:map-dir md
                               #:expect-lockfile-digest (and (lockfile) (sha256-file (lockfile)))))
     (cond
       [(eq? (compiled-root-reason root) #t)
        (define-values (code transcript) (launch-with-root root (first ms) child-args))
        (display transcript)
        (exit code)]
       [else
        ;; Fail closed to the single eager current-source fallback.
        (eprintf "compiled-root: resolution failed (~a); eager fallback\n"
                 (reason->string (compiled-root-reason root)))
        (eager-fallback-compile! co ms)
        (define code
          (apply system*/exit-code (find-executable-path "racket") "-t" (first ms) child-args))
        (exit code)])]))
