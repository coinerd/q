#!/usr/bin/env racket
#lang racket/base
;; BUG-0090 cold/warm cache reproduction supervisor.
;;
;; Runs the REAL tests/test-gsd-generated-metadata-correction.rkt in
;; isolated copies of the tracked q tree:
;;   --mode cold   per trial: fresh `git archive HEAD` copy, every compiled/
;;                 cache deleted, then the suite runs with a fresh HOME and
;;                 isolated XDG cache (never prewarmed -- the R90 exception).
;;   --mode warm   identical per-trial copy and cache deletion, plus one
;;                 `raco make` of the suite in that copy before the run
;;                 (matched warm control).
;;
;; Every child runs with a fresh HOME/XDG_CACHE_HOME and no BUG0087_CASE.
;; Child failures, child timeouts and tool failures all fail the supervisor;
;; the exit code is 0 only if every trial passes. This supervisor performs
;; no assertions of its own: the suite owns correctness, the supervisor owns
;; environment truth (cold vs warm) and honest exit propagation.
;;
;; Usage: racket tests/helpers/metadata-cache-repro.rkt --mode cold --trials 3
;; Pass --overlay while the R90 repair is uncommitted: it copies the exact
;; repair files onto each archived tree (git archive alone carries only
;; committed state, so a post-repair trial needs this until the repair PR
;; lands; without --overlay the copy is pure HEAD).
(require racket/cmdline
         racket/file
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system)
(define-runtime-path src-root-dir "../..")
(define src-root (simplify-path src-root-dir))
(define mode #f)
(define trials 3)
(define timeout-sec 2400)
(define keep? #f)
(define overlay? #f)
(define overlay-files
  (list "extensions/gsd/supplemental-evidence-journal.rkt"
        "extensions/gsd/supplemental-disposition-claim.rkt"
        "extensions/gsd/supplemental-evidence-store-core.rkt"
        "tests/test-gsd-metadata-cold-cache.rkt"
        "tests/helpers/metadata-cache-repro.rkt"))
(command-line
 #:program "metadata-cache-repro"
 #:once-each
 ["--mode" m "cold or warm (required)" (set! mode m)]
 ["--trials" n "number of suite runs (default 3)" (set! trials (max 1 (string->number n)))]
 ["--timeout-sec" s "per-trial child timeout in seconds (default 2400)"
  (set! timeout-sec (string->number s))]
 ["--keep" "keep trial workspaces for diagnosis" (set! keep? #t)]
 ["--overlay" "overlay the uncommitted R90 repair files onto the archived tree"
  (set! overlay? #t)])
(define (fail! format-string . args)
  (eprintf "metadata-cache-repro: ~a\n" (apply format format-string args))
  (exit 2))
(unless (member mode '("cold" "warm")) (fail! "--mode must be cold or warm (got ~s)" mode))
(define git (or (find-executable-path "git") (fail! "git not found on PATH")))
(define tar (or (find-executable-path "tar") (fail! "tar not found on PATH")))
(define racket-exe
  (let ([exec-file (find-system-path 'exec-file)])
    (if (complete-path? exec-file)
        exec-file
        (or (find-executable-path (path->string exec-file)) exec-file))))
(define raco-exe (build-path (path-only racket-exe) "raco"))
(unless (file-exists? raco-exe) (fail! "raco not found next to ~a" racket-exe))
(define head-sha
  (let ()
    (unless (zero? (parameterize ([current-output-port (open-output-nowhere)])
                     (system*/exit-code git "-C" src-root "rev-parse" "HEAD")))
      (fail! "~a is not a git repository" src-root))
    (string-trim
     (call-with-output-string
      (lambda (out)
        (parameterize ([current-output-port out])
          (system*/exit-code git "-C" src-root "rev-parse" "HEAD")))))))
(define suite-rel "tests/test-gsd-generated-metadata-correction.rkt")
(unless (file-exists? (build-path src-root suite-rel)) (fail! "real suite missing: ~a" suite-rel))
;; Every compiled/ directory in the copy must go: source AND cache cold.
(define (delete-compiled-caches! root)
  (define dirs
    (for/list ([p (in-directory root)]
               #:when (and (directory-exists? p)
                           (equal? (file-name-from-path p) (string->path "compiled"))))
      p))
  (for ([d (in-list dirs)]) (delete-directory/files d))
  (length dirs))
(define (fresh-env home cache)
  (define env (environment-variables-copy (current-environment-variables)))
  (environment-variables-set! env #"HOME" (path->bytes home))
  (environment-variables-set! env #"XDG_CACHE_HOME" (path->bytes cache))
  (environment-variables-set! env #"BUG0087_CASE" #f)
  env)
;; Run one child under a custodian with a hard timeout; the child leads its
;; own process group so a timeout takes spawned grandchildren with it.
;; Returns (values exit-code timed-out?).
(define (run-child what exe args work env log-path)
  (define cust (make-custodian))
  (define-values (code timed-out?)
    (parameterize ([current-custodian cust]
                   [current-environment-variables env]
                   [current-directory work])
      (call-with-output-file log-path #:exists 'truncate
        (lambda (log)
          (subprocess-group-enabled #t)
          (define-values (proc _out stdin _err) (apply subprocess log #f 'stdout exe args))
          (define done (make-semaphore))
          (thread (lambda () (subprocess-wait proc) (semaphore-post done)))
          (cond
            [(sync/timeout timeout-sec done) (values (subprocess-status proc) #f)]
            [else
             (eprintf "metadata-cache-repro: ~a timed out after ~as; killing process group\n"
                      what timeout-sec)
             (subprocess-kill proc #t)
             (sync/timeout 15 done)
             (values (subprocess-status proc) #t)])))))
  (custodian-shutdown-all cust)
  (values code timed-out?))
(define (log-tail path)
  (define lines (file->lines path))
  (string-join (take-right lines (min 40 (length lines))) "\n"))
;; Returns 'passed or 'failed; never aborts the whole matrix on one failure.
(define (one-trial index)
  (define ws (make-temporary-file "q-bug0090-~a" 'directory))
  (define work (build-path ws "q"))
  (define home (build-path ws "home"))
  (define cache (build-path ws "cache"))
  (make-directory home)
  (make-directory cache)
  (make-directory work)
  (define tar-path (build-path ws "q.tar"))
  (define suite-log (build-path ws "suite.log"))
  (define (cleanup!) (unless keep? (with-handlers ([exn:fail? void]) (delete-directory/files ws))))
  (printf "trial ~a (~a): copying tracked tree from HEAD ~a\n" index mode head-sha)
  (flush-output)
  (or
   (with-handlers ([exn:fail?
                    (lambda (e)
                      (eprintf "trial ~a infrastructure error: ~a\n" index (exn-message e))
                      (cleanup!)
                      'failed)])
     (check-exit! "git archive" (system*/exit-code git "-C" src-root "archive"
                                                "--format=tar" "-o" tar-path "HEAD"))
     (check-exit! "tar extract" (system*/exit-code tar "-xf" tar-path "-C" work))
     (check-exit! "untarred suite present"
                  (if (file-exists? (build-path work suite-rel)) 0 1))
     ;; git archive carries only COMMITTED state. While the R90 repair is
     ;; still uncommitted (the implementation agent's no-commit constraint),
     ;; --overlay copies the exact repair files onto the archived tree so the
     ;; trials exercise the repair delta over HEAD; without the flag the
     ;; archived tree is pure HEAD. The overlay list is explicit and auditable.
     (when overlay?
       (for ([rel (in-list overlay-files)])
         (define from (build-path src-root rel))
         (unless (file-exists? from) (fail! "overlay source missing: ~a" rel))
         (define to (build-path work rel))
         (make-parent-directory* to)
         (copy-file from to #t))
       (printf "trial ~a (~a): overlaid ~a uncommitted R90 repair files\n"
               index mode (length overlay-files)))
     (define deleted (delete-compiled-caches! work))
     (define env (fresh-env home cache))
     (when (equal? mode "warm")
       (printf "trial ~a (warm): raco make (prewarm) the suite in the copy\n" index)
       (flush-output)
       (define-values (warm-code warm-timeout?)
         (run-child "raco make prewarm" raco-exe
                    (list "make" (path->string (build-path work suite-rel)))
                    work env (build-path ws "prewarm.log")))
       (when (or warm-timeout? (not (zero? warm-code)))
         (eprintf "--- last 40 prewarm log lines ---\n~a\n"
                  (log-tail (build-path ws "prewarm.log")))
         (raise (exn:fail (format "prewarm exit=~a timeout=~a" warm-code warm-timeout?)
                          (current-continuation-marks))))
       (void))
     (printf "trial ~a (~a): running real suite (~a compiled caches deleted)\n" index mode deleted)
     (flush-output)
     (define start (current-inexact-milliseconds))
     (define-values (code timed-out?)
       (run-child "real metadata-correction suite"
                  racket-exe
                  (list (path->string (build-path work "scripts/run-tests.rkt"))
                        "--strict" "--sequential" "--mode" "subprocess" suite-rel)
                  work env suite-log))
     (define seconds (/ (- (current-inexact-milliseconds) start) 1000.0))
     (when (or timed-out? (not (zero? code)))
       (eprintf "--- last 40 log lines (~a) ---\n~a\n" suite-log (log-tail suite-log))
       (raise (exn:fail (format "real suite exit=~a timeout=~a after ~as"
                                code timed-out? (real->decimal-string seconds 1))
                        (current-continuation-marks))))
     (printf "trial ~a (~a): PASSED exit=0 in ~as\n"
             index mode (real->decimal-string seconds 1))
     (cleanup!)
     'passed)
   'failed))
(define (check-exit! what code)
  (unless (zero? code) (raise (exn:fail (format "~a failed (exit ~a)" what code)
                                        (current-continuation-marks)))))
(printf "metadata-cache-repro: mode=~a trials=~a timeout=~as racket=~a head=~a\n"
        mode trials timeout-sec (version) head-sha)
(define results (for/list ([i (in-range trials)]) (one-trial (add1 i))))
(define failures (count (lambda (r) (eq? r 'failed)) results))
(printf "metadata-cache-repro: ~a/~a ~a trials passed\n" (- trials failures) trials mode)
(exit (if (zero? failures) 0 1))
