#lang racket/base

;; scripts/run-tests/hotspot-benchmark.rkt — v1.00.24 W0 focused hotspot
;; baseline collector.
;;
;; Runs an explicit allowlisted list of test files N times each (default ten)
;; using the batch scheduler over subprocesses, records every attempt
;; (including failures and timeouts), and emits a canonical JSON manifest with
;; command, q SHA, environment, scheduler/mode/jobs, selected-path digest,
;; per-file samples, and linear-interpolated median/p95. `--check` regenerates
;; the canonical bytes and verifies schema, sample floor (>= 10 successful per
;; family), and SHA256SUMS byte-identity. Missing or incomparable evidence is
;; reported, never imputed.

(require json
         racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/match
         racket/path
         racket/port
         racket/string
         racket/system
         "sha256.rkt")

(provide hotspot-percentile
         hotspot-file-stats
         hotspot-canonical-json
         hotspot-manifest-errors
         hotspot-artifact-errors
         hotspot-write-sha256sums!
         hotspot-run-one
         hotspot-collect
         hotspot-build-manifest
         run-hotspot-benchmark-main)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (hb-sha256-hex bytes-or-path)
  (define bs
    (if (or (path? bytes-or-path) (string? bytes-or-path))
        (call-with-input-file bytes-or-path sha256)
        (sha256 bytes-or-path)))
  (apply string-append
         (for/list ([b (in-bytes bs)])
           (string (string-ref "0123456789abcdef" (quotient b 16))
                   (string-ref "0123456789abcdef" (remainder b 16))))))

(define (hb-base-dir)
  ;; Repo-root detection: walk up from the run-file directory until we find
  ;; the q checkout root (a directory containing both scripts/ and tests/).
  ;; The original single-parent walk assumed the entry point lived directly
  ;; in scripts/ and mis-resolved for scripts/run-tests/ entry points.
  (let loop ([dir (path-only (path->complete-path (find-system-path 'run-file)))])
    (cond
      [(not dir) (current-directory)]
      [(and (directory-exists? (build-path dir "scripts"))
            (directory-exists? (build-path dir "tests")))
       (path->string (simplify-path dir))]
      [else
       (define up (simplify-path (build-path dir 'up)))
       (if (equal? up dir) (path->string dir) (loop up))])))

(define (hb-q-sha)
  (with-handlers ([exn:fail? (lambda (_) "unknown")])
    (define res
      (with-output-to-string
        (lambda ()
          (system*/exit-code (find-executable-path "git") "rev-parse" "HEAD"))))
    (define s (string-trim res))
    (if (and (>= (string-length s) 40) (regexp-match? #rx"^[0-9a-f]+$" s)) s "unknown")))

;; ---------------------------------------------------------------------------
;; Linear-interpolated percentiles and per-file stats
;; ---------------------------------------------------------------------------

(define (hotspot-percentile xs p)
  ;; exact-rational interpolation; inexact only at the boundary (9.55 stays 9.55)
  (define sorted (sort xs <))
  (when (null? sorted) (error 'hotspot-percentile "no samples"))
  (define n (length sorted))
  (define idx (* (- n 1) (/ p 100)))
  (define lo (floor idx))
  (define hi (ceiling idx))
  (define xlo (list-ref sorted (inexact->exact lo)))
  (define xhi (list-ref sorted (inexact->exact hi)))
  (exact->inexact (+ xlo (* (- idx lo) (- xhi xlo)))))

(define (hotspot-file-stats samples)
  (define passes (filter (lambda (s) (string=? (hash-ref s 'status "fail") "pass")) samples))
  (define failures (length (filter (lambda (s) (string=? (hash-ref s 'status "fail") "fail")) samples)))
  (define timeouts (length (filter (lambda (s) (or (equal? (hash-ref s 'status "") "timeout")
                                                 (equal? (hash-ref s 'timeout "") "timeout"))) samples)))
  (define durs (map (lambda (s) (hash-ref s 'duration_ms 0)) passes))
  (hasheq 'total (length samples)
          'successful (length passes)
          'failures failures
          'timeouts timeouts
          'median_ms (if (null? durs) #f (hotspot-percentile durs 50))
          'p95_ms (if (null? durs) #f (hotspot-percentile durs 95))
          'min_ms (if (null? durs) #f (apply min durs))
          'max_ms (if (null? durs) #f (apply max durs))))

;; ---------------------------------------------------------------------------
;; Canonical JSON
;; ---------------------------------------------------------------------------

(define (hb-json-string s)
  (string-append
   "\""
   (apply string-append
          (for/list ([ch (in-string s)])
            (cond
              [(char=? ch #\\) "\\\\"]
              [(char=? ch #\") "\\\""]
              [(char=? ch #\newline) "\\n"]
              [(char=? ch #\return) "\\r"]
              [(char=? ch #\tab) "\\t"]
              [else (string ch)])))
   "\""))

(define (hb-key<? a b)
  (define (as-str k) (if (symbol? k) (symbol->string k) k))
  (string<? (as-str a) (as-str b)))

(define (hotspot-canonical-json v)
  (cond
    [(string? v) (hb-json-string v)]
    [(boolean? v) (if v "true" "false")]
    [(real? v) (if (exact-integer? v)
                   (number->string v)
                   (~r (exact->inexact v) #:precision '(= 3)))]
    [(symbol? v) (hb-json-string (symbol->string v))]
    [(null? v) "[]"]
    [(pair? v) (string-append "[" (string-join (map hotspot-canonical-json v) ",") "]")]
    [(list? v) (string-append "[" (string-join (map hotspot-canonical-json v) ",") "]")]
    [(vector? v) (hotspot-canonical-json (vector->list v))]
    [(hash? v)
     (string-append
      "{"
      (string-join
       (for/list ([k (in-list (sort (hash-keys v) hb-key<?))])
         (string-append (hb-json-string (if (symbol? k) (symbol->string k) (format "~a" k)))
                        ":"
                        (hotspot-canonical-json (hash-ref v k))))
       ",")
      "}")]
    [else (hb-json-string (format "~a" v))]))

;; ---------------------------------------------------------------------------
;; Manifest schema validation
;; ---------------------------------------------------------------------------

(define required-top-fields
  '("schema" "milestone" "wave" "command" "q_sha" "scheduler" "mode" "jobs"
    "selected_paths_digest" "environment" "inputs" "families"))

(define (hb-missing errors where field got)
  (if got errors (cons (format "~a: missing required field ~a" where field) errors)))

;; Manifests are parsed with string->jsexpr, which yields SYMBOL hash keys,
;; while this validator (and the canonical writer) use STRING keys. Normalize
;; recursively so --check validates the on-disk artifact rather than failing
;; every field lookup on key type alone.
(define (hb-string-keyed v)
  (cond
    [(hash? v)
     (for/hash ([(k val) (in-hash v)])
       (values (if (symbol? k) (symbol->string k) k)
               (hb-string-keyed val)))]
    [(list? v) (map hb-string-keyed v)]
    [else v]))

(define (hotspot-manifest-errors m0)
  (define m (if (hash? m0) (hb-string-keyed m0) m0))
  (define errors '())
  (unless (hash? m)
    (error 'hotspot-manifest-errors "manifest must be a hash table"))
  (for ([f (in-list required-top-fields)])
    (set! errors (hb-missing errors "manifest" f (hash-ref m f #f))))
  (define env (hash-ref m "environment" #f))
  (when (hash? env)
    (for ([f (in-list '("racket_version" "os" "machine" "config_digest"))])
      (set! errors (hb-missing errors "environment" f (hash-ref env f #f)))))
  (define inputs (hash-ref m "inputs" #f))
  (when (hash? inputs)
    (set! errors (hb-missing errors "inputs" "allowlist_sha256" (hash-ref inputs "allowlist_sha256" #f))))
  (define families (hash-ref m "families" #f))
  (when (list? families)
    (for ([fam (in-list families)])
      (define file (hash-ref fam "file" #f))
      (set! errors (hb-missing errors (or file "family") "file" file))
      (define samples (hash-ref fam "samples" #f))
      (set! errors (hb-missing errors (or file "family") "samples" samples))
      (define stats (hash-ref fam "stats" #f))
      (set! errors (hb-missing errors (or file "family") "stats" stats))
      (when (and (list? samples) (pair? samples))
        (for ([s (in-list samples)])
          (set! errors (hb-missing errors (or file "sample") "status" (hash-ref s "status" #f)))
          (set! errors (hb-missing errors (or file "sample") "duration_ms" (hash-ref s "duration_ms" #f))))
        (define passes (filter (lambda (s) (string=? (hash-ref s "status" "") "pass")) samples))
        (define need
          (min 10 (hash-ref m "samples_per_family" 10)))
        (when (< (length passes) need)
          (set! errors
                (cons (format "~a: fewer than ~a successful samples (~a)"
                              file need (length passes))
                      errors)))
        (when (hash? stats)
          (define n-pass (length passes))
          (define n-fail (length (filter (lambda (s) (string=? (hash-ref s "status" "") "fail")) samples)))
          (define n-timeout (length (filter (lambda (s) (string=? (hash-ref s "status" "") "timeout")) samples)))
          (unless (and (equal? (hash-ref stats "successful" -1) n-pass)
                       (equal? (hash-ref stats "failures" -1) n-fail)
                       (equal? (hash-ref stats "timeouts" -1) n-timeout))
            (set! errors
                  (cons (format "~a: stats/sample record inconsistency" file) errors)))))))
  (reverse errors))

;; ---------------------------------------------------------------------------
;; SHA256SUMS artifact
;; ---------------------------------------------------------------------------

(define (hotspot-write-sha256sums! sums-path paths)
  (define dir (path-only (path->complete-path sums-path)))
  (call-with-output-file sums-path
    (lambda (out)
      (for ([p (in-list paths)])
        (fprintf out "~a  ~a~n"
                 (hb-sha256-hex p)
                 (path->string (find-relative-path dir (path->complete-path p))))))
    #:exists 'truncate/replace))

(define (hb-sums-line? line sums-path)
  (regexp-match? #px"^[0-9a-f]{64}  .+" line))

(define (hotspot-artifact-errors manifest-path sums-path manifest)
  (define errors '())
  (define on-disk (with-handlers ([exn:fail? (lambda (_) #f)])
                    (file->string manifest-path)))
  (unless on-disk (set! errors (cons (format "~a: unreadable manifest" manifest-path) errors)))
  (when on-disk
    (define canonical (hotspot-canonical-json manifest))
    (unless (string=? on-disk canonical)
      (set! errors (cons (format "~a: byte drift vs canonical regeneration" manifest-path) errors))))
  (define sums (with-handlers ([exn:fail? (lambda (_) #f)])
                 (file->lines sums-path)))
  (unless sums (set! errors (cons (format "~a: unreadable SHA256SUMS" sums-path) errors)))
  (when sums
    (unless (and (= (length sums) 1) (hb-sums-line? (first sums) sums-path))
      (set! errors (cons (format "~a: malformed SHA256SUMS entry" sums-path) errors)))
    (when (and (= (length sums) 1) (hb-sums-line? (first sums) sums-path))
      (define line (first sums))
      (define recorded-hash (substring line 0 64))
      (define actual (hb-sha256-hex manifest-path))
      (unless (string=? recorded-hash actual)
        (set! errors (cons (format "~a: byte drift vs SHA256SUMS" manifest-path) errors)))))
  (reverse errors))

;; ---------------------------------------------------------------------------
;; Collector
;; ---------------------------------------------------------------------------

(define (hotspot-run-one file #:timeout-s [timeout-s 240] #:base-dir [base-dir (hb-base-dir)])
  (define racket-path (find-executable-path "racket"))
  (define target (path->string (build-path base-dir file)))
  (define started (current-inexact-milliseconds))
  (define stdout-p (open-output-file "/dev/null" #:exists 'append))
  (define status
    (with-handlers ([exn:fail? (lambda (e)
                                 (eprintf "hotspot-spawn-exn: ~a\n" (exn-message e))
                                 "fail")])
        (define-values (proc _sub-stdin _sub-stdout _sub-stderr)
          (subprocess stdout-p #f (current-error-port) racket-path target))
      (define ready (sync/timeout timeout-s proc))
      (cond
        [ready
         (define code (subprocess-status proc))
         (if (zero? code) "pass" "fail")]
        [else
         (with-handlers ([exn:fail? void]) (subprocess-kill proc #t))
         "timeout"])))
  (close-output-port stdout-p)
  (hasheq 'file file
          'status status
          'duration_ms (inexact->exact (floor (- (current-inexact-milliseconds) started)))))

;; batch scheduler: deterministic allowlist order, bounded workers; per-file
;; rounds so sample i of every family starts no earlier than sample i-1 of the
;; same family completes.
(define (hotspot-collect files #:samples [samples 10] #:jobs [jobs 2]
                         #:timeout-s [timeout-s 240] #:base-dir [base-dir (hb-base-dir)])
  (define records (make-hash)) ; file -> (list of sample hashes, newest first)
  (for ([i (in-range samples)])
    (define sema (make-semaphore (max 1 jobs)))
    (define results (make-hash))
    (define threads
      (for/list ([f (in-list files)])
        (thread
         (lambda ()
           (semaphore-wait sema)
           (hash-set! results f (hotspot-run-one f #:timeout-s timeout-s #:base-dir base-dir))
           (semaphore-post sema)))))
    (for ([t (in-list threads)]) (thread-wait t))
    (for ([f (in-list files)])
      (hash-set! records f (cons (hash-ref results f) (hash-ref records f '())))))
  (for/list ([f (in-list (sort files string<?))])
    (define samples-rev (hash-ref records f '()))
    (hasheq 'file f
            'samples (reverse samples-rev)
            'stats (hotspot-file-stats (map (lambda (s) (hasheq 'status (hash-ref s 'status)
                                                                'duration_ms (hash-ref s 'duration_ms)))
                                            (reverse samples-rev))))))

(define (hotspot-build-manifest files
                                #:samples [samples 10]
                                #:jobs [jobs 2]
                                #:timeout-s [timeout-s 240]
                                #:command [command #f]
                                #:wave [wave "W0"]
                                #:manifest-command-path [manifest-command-path #f]
                                #:allowlist-path [allowlist-path #f]
                                #:base-dir [base-dir (hb-base-dir)])
  (define families (hotspot-collect files #:samples samples #:jobs jobs
                                   #:timeout-s timeout-s #:base-dir base-dir))
  (define selected-paths (remove-duplicates (sort files string<?)))
  (hasheq
   "schema" "test-runtime/hotspot-baseline/v1"
   "milestone" "v1.00.24"
   "wave" wave
    "command" (or command
                  (if manifest-command-path
                      (format "racket scripts/run-tests/hotspot-baseline.rkt --manifest ~a --samples ~a --jobs ~a"
                              manifest-command-path samples jobs)
                      (format "racket scripts/run-tests/hotspot-baseline.rkt --samples ~a --jobs ~a" samples jobs)))
   "q_sha" (hb-q-sha)
   "scheduler" "batch"
   "mode" "subprocess"
   "jobs" jobs
   "samples_per_family" samples
   "timeout_s" timeout-s
   "selected_paths_digest"
   (hb-sha256-hex (string->bytes/utf-8 (string-join selected-paths "\n")))
   "environment"
   (hasheq "racket_version" (version)
           "os" (symbol->string (system-type))
           "machine" (symbol->string (system-type 'arch))
           "config_digest"
           (hb-sha256-hex
            (string->bytes/utf-8
             (format "~a:~a:~a:~a:batch:subprocess" (version) (system-type) (system-type 'arch) jobs))))
   "inputs"
   (hasheq "allowlist_sha256"
           (if (and allowlist-path (file-exists? allowlist-path))
               (hb-sha256-hex allowlist-path)
               (hb-sha256-hex (string->bytes/utf-8 (string-join files "\n")))))
   "families" families))

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(define (run-hotspot-benchmark-main argv #:program [program "hotspot-benchmark"])
  (define do-check #f)
  (define samples 10)
  (define jobs 2)
  (define timeout-s 240)
  (define manifest-out #f)
  (define allowlist-file #f)
  (define wave-label "W0")
  (define command-label #f)
  (define family-files '())
  (command-line
    #:program program
   #:argv argv
   #:once-each
   ["--check" "verify an existing manifest byte-identically" (set! do-check #t)]
   ["--samples" n "samples per family (default 10)" (set! samples (string->number n))]
   ["--jobs" n "concurrent subprocesses (default 2)" (set! jobs (string->number n))]
   ["--timeout" s "per-run timeout seconds (default 240)" (set! timeout-s (string->number s))]
   ["--manifest" p "baseline.json output/check path" (set! manifest-out p)]
   ["--allowlist" p "file with one test path per line" (set! allowlist-file p)]
   ["--wave" w "wave label recorded in the manifest (default W0)" (set! wave-label w)]
   ["--command" c "command string recorded in the manifest" (set! command-label c)]
   #:multi
   [("--family" "-f") f "single test file to measure" (set! family-files (cons f family-files))])
  (define files
    (remove-duplicates
     (sort
      (append family-files
              (if allowlist-file
                  (filter (lambda (l) (and (string<? "" l) (not (string-prefix? l "#"))))
                          (file->lines allowlist-file))
                  '()))
      string<?)))
  (cond
    [(and do-check manifest-out)
     (define sums-path (string-append manifest-out "SHA256SUMS"))
     (define manifest
       (with-handlers ([exn:fail? (lambda (e) (error 'hotspot-benchmark "~a" (exn-message e)))])
         (string->jsexpr (file->string manifest-out))))
     (define errors (append (hotspot-manifest-errors manifest)
                            (hotspot-artifact-errors manifest-out sums-path manifest)))
     (for ([e (in-list errors)]) (eprintf "hotspot-benchmark: ~a~n" e))
     (exit (if (null? errors) 0 1))]
    [manifest-out
     (unless (pair? files)
       (error 'hotspot-benchmark "no families given: use --allowlist or --family"))
      (define manifest
        (hotspot-build-manifest files #:samples samples #:jobs jobs #:timeout-s timeout-s
                                #:command command-label
                                #:wave wave-label
                                #:manifest-command-path manifest-out
                                #:allowlist-path allowlist-file))
     (define canonical (hotspot-canonical-json manifest))
     (call-with-output-file manifest-out
       (lambda (out) (display canonical out))
       #:exists 'truncate/replace)
      (hotspot-write-sha256sums! (string-append manifest-out "SHA256SUMS")
                                 (list manifest-out))
      (displayln (format "wrote ~a (~a families)" manifest-out (length files)))
      (define errors (hotspot-manifest-errors manifest))
      (for ([e (in-list errors)]) (eprintf "hotspot-benchmark: ~a~n" e))
      (exit (if (null? errors) 0 1))]
    [else
     (error 'hotspot-benchmark "required: --manifest PATH (add --check to verify)")]))

(module+ main
  (run-hotspot-benchmark-main
   (vector->list (current-command-line-arguments))
   #:program "hotspot-benchmark"))
