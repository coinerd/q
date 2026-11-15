#lang racket/base

;; q/scripts/ci/trial-context.rkt — testing-framework refactor §11.5.
;;
;; Writes the trusted-trial context document
;; (.github/workflows/ci-recovery-benchmark.yml, "Bind and validate the
;; trusted trial identity") that is retained BEFORE any reusable workflow code
;; runs, so an offline reconciliation still has the identity-bound context when
;; cancellation prevents the collector from ever starting.
;;
;; This was a `python3 - <<'PY'` heredoc; it is Racket now, so the gate job
;; installs only the pinned toolchain. The document is BYTE-FOR-BYTE
;; field-equivalent to the old one:
;;
;;   trial-id              string    TRIAL_ID
;;   attempt               int       TRIAL_ATTEMPT
;;   treatment             string    TREATMENT
;;   run-id                int       RUN_ID
;;   run-attempt           int       RUN_ATTEMPT
;;   head-sha              string    DISPATCH_SHA (== checked-out HEAD)
;;   workflow-revision-sha string    WORKFLOW_SHA
;;   shard-total           int       FAST_SHARD_COUNT 4/6/8 else 3
;;   shard-plan            string    "0,1,…,n-1"
;;   test-inventory-digest string    sha256 over the SORTED `git ls-files tests`
;;                                   lines, each terminated by "\n"
;;   created-at-utc        string    YYYY-MM-DDTHH:MM:SSZ (UTC)
;;
;; Fail closed: a missing or non-numeric field refuses (exit 1, nothing
;; written) instead of publishing a half-bound context, and a failing
;; `git ls-files` is an error — an empty inventory is never silently digested.
;;
;; Usage:
;;   racket scripts/ci/trial-context.rkt [--out trial-context.json] [--root .]

(require json
         racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/port
         racket/string
         racket/system
         (file "../../util/sha256.rkt"))

(provide trial-context-document
         inventory-digest
         git-inventory-lines
         shard-total
         shard-plan
         created-at-utc)

;; ---------------------------------------------------------------------------
;; Derivations
;; ---------------------------------------------------------------------------

(define (refuse fmt . args)
  (error 'trial-context (apply format fmt args)))

;; The documented shard-count guard: only the study candidates 4/6/8 widen the
;; matrix; anything else (unset, 3, garbage) keeps 3.
(define (shard-total fast-shard-count)
  (if (member fast-shard-count '("4" "6" "8"))
      (string->number fast-shard-count)
      3))

(define (shard-plan total)
  (string-join (for/list ([n (in-range total)]) (number->string n)) ","))

;; sha256 over the sorted inventory lines, each terminated by "\n" — exactly
;; the digest the previous implementation published (and the same digest the
;; collector recomputes with `git ls-files tests | LC_ALL=C sort | sha256sum`).
(define (inventory-digest lines)
  (sha256-hex
   (string->bytes/utf-8
    (apply string-append (for/list ([line (in-list (sort (filter string? lines) string<?))])
                           (string-append line "\n"))))))

;; `git ls-files tests` in `root`, as lines. Fail closed: a non-zero exit is an
;; error, never an empty digest.
(define (git-inventory-lines root)
  (define out (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port (open-output-nowhere)])
      (with-handlers ([exn:fail? (lambda (_e) 127)])
        (system*/exit-code (or (find-executable-path "git") "git")
                           "-C"
                           (path->string (path->complete-path root))
                           "ls-files"
                           "tests"))))
  (unless (zero? code)
    (refuse "git ls-files tests failed in ~a (exit ~a)" root code))
  (define text (get-output-string out))
  (define lines
    (for/list ([line (in-list (string-split text "\n"))] #:unless (string=? line ""))
      line))
  (when (null? lines)
    (refuse "git ls-files tests produced no inventory in ~a" root))
  lines)

;; YYYY-MM-DDTHH:MM:SSZ, UTC.
(define (created-at-utc [seconds (current-seconds)])
  (define d (seconds->date seconds #t))
  (format "~a-~a-~aT~a:~a:~aZ"
          (~r (date-year d) #:min-width 4 #:pad-string "0")
          (~r (date-month d) #:min-width 2 #:pad-string "0")
          (~r (date-day d) #:min-width 2 #:pad-string "0")
          (~r (date-hour d) #:min-width 2 #:pad-string "0")
          (~r (date-minute d) #:min-width 2 #:pad-string "0")
          (~r (date-second d) #:min-width 2 #:pad-string "0")))

;; ---------------------------------------------------------------------------
;; The document
;; ---------------------------------------------------------------------------

(define (env name)
  (define value (getenv name))
  (and value (not (string=? value "")) value))

(define (env-required name)
  (or (env name) (refuse "required environment variable ~a is empty" name)))

(define (env-integer name)
  (define raw (env-required name))
  (define n (string->number raw))
  (unless (and n (exact-integer? n) (>= n 0))
    (refuse "environment variable ~a must be a non-negative integer, got ~s" name raw))
  n)

;; The context document as a JSON object (symbol keys, exactly the documented
;; field set).
(define (trial-context-document #:inventory [inventory #f] [now (current-seconds)])
  (define total (shard-total (env "FAST_SHARD_COUNT")))
  (hash 'trial-id
        (env-required "TRIAL_ID")
        'attempt
        (env-integer "TRIAL_ATTEMPT")
        'treatment
        (env-required "TREATMENT")
        'run-id
        (env-integer "RUN_ID")
        'run-attempt
        (env-integer "RUN_ATTEMPT")
        'head-sha
        (env-required "DISPATCH_SHA")
        'workflow-revision-sha
        (env-required "WORKFLOW_SHA")
        'shard-total
        total
        'shard-plan
        (shard-plan total)
        'test-inventory-digest
        (inventory-digest (or inventory (git-inventory-lines (current-directory))))
        'created-at-utc
        (created-at-utc now)))

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(define (main argv)
  (define out "trial-context.json")
  (define root (current-directory))
  (command-line
   #:program "trial-context.rkt"
   #:once-each
   [("--out") path "Output path (default: trial-context.json)" (set! out path)]
   [("--root") dir "Repository root used for the test inventory (default: cwd)"
    (set! root dir)]
   #:args ()
   (void))
  (define doc
    (with-handlers ([exn:fail? (lambda (e)
                                 (eprintf "trial-context: ~a\n" (exn-message e))
                                 #f)])
      (trial-context-document #:inventory (git-inventory-lines root))))
  (cond
    [(not doc) 1]
    [else
     (call-with-output-file out
       (lambda (o) (write-json doc o) (newline o))
       #:exists 'truncate)
     (printf "trial-context: wrote ~a (trial ~a attempt ~a)\n"
             out
             (hash-ref doc 'trial-id)
             (hash-ref doc 'attempt))
     0]))

(module+ main
  (exit (main (vector->list (current-command-line-arguments)))))
