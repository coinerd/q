#lang racket/base

;; sandbox/subprocess.rkt — subprocess management under custodians
;;
;; Provides a reusable subprocess runner with resource limits and
;; custodian-based cleanup. Every subprocess runs under its own custodian
;; so that timeout or kill cleans up all associated resources.

(require racket/contract
         racket/port
         racket/string
         "limits.rkt"
         ;; SEC-16 (v0.22.0): consolidated shell-quote
         (only-in "../util/shell-quote.rkt" shell-quote))

(define-logger subprocess)

;; Struct accessors (no contract needed for transparent struct)
(provide subprocess-result
         subprocess-result?
         subprocess-result-exit-code
         subprocess-result-stdout
         subprocess-result-stderr
         subprocess-result-timed-out?
         subprocess-result-elapsed-ms
         subprocess-result-truncated?
         ;; Parameters (no contract needed)
         default-timeout-seconds
         default-max-output-bytes
         current-secret-scrub-denylist
         current-secret-scrub-allowlist
         ;; Contracted functions
         (contract-out [run-subprocess
                        (->* (string?)
                             (#:args (listof string?)
                                     #:limits exec-limits?
                                     #:timeout (or/c number? #f)
                                     #:directory (or/c path-string? #f)
                                     #:environment any/c
                                     #:encoding symbol?)
                             subprocess-result?)]
                       [kill-subprocess! (-> (or/c custodian? #f) void?)]
                       [sanitize-env (->* () (any/c) any/c)]
                       [secret-env-var? (-> string? boolean?)]))

;; --------------------------------------------------
;; Result struct
;; --------------------------------------------------

(struct subprocess-result
        (exit-code ; integer
         stdout ; string
         stderr ; string
         timed-out? ; boolean
         elapsed-ms ; number
         truncated?) ; boolean — output was cut at byte budget
  #:transparent)

;; --------------------------------------------------
;; Bounded port reader — reads incrementally with a byte budget
;; --------------------------------------------------

;; Non-blocking read — reads only bytes already available.
;; BUGFIX: Uses read-bytes-avail! which returns immediately with
;; available data, BUT it blocks waiting for NEW data when the pipe
;; has no data AND is still open (e.g., backgrounded child inherited it).
;; So we use sync/timeout with a short deadline to check availability
;; before each read, and stop when no more data is immediately ready.
(define (read-available-bounded p max-bytes)
  (if (or (not p) (port-closed? p))
      ""
      (with-handlers ([exn:fail? (lambda (e)
                                   (log-subprocess-warning "read-available-bounded: ~a"
                                                           (exn-message e))
                                   "")])
        (define acc (open-output-bytes))
        (define buf (make-bytes (min 4096 max-bytes)))
        (let loop ([remaining max-bytes])
          (when (> remaining 0)
            ;; sync/timeout 0 returns #f only when NO bytes are available.
            ;; When bytes ARE available, it returns the port.
            ;; This prevents blocking when a backgrounded child holds the pipe.
            (define ready? (sync/timeout 0 p))
            (when ready?
              (define n (read-bytes-avail! buf p))
              (cond
                [(eof-object? n) (void)]
                [(number? n)
                 (define to-write (min n remaining))
                 (write-bytes buf acc 0 to-write)
                 (loop (- remaining to-write))]
                [else (void)]))))
        (get-output-string acc))))

(define (read-port-bounded p max-bytes)
  (define-values (reader get-result) (start-bounded-reader p max-bytes "read-port-bounded"))
  (sync reader)
  (define-values (text truncated?) (get-result))
  (if truncated?
      (string-append text (format "\n[output truncated at ~a bytes]" max-bytes))
      text))

;; Start a reader thread immediately after subprocess launch.  This prevents a
;; child that writes more than the OS pipe buffer from blocking forever while
;; the parent is still waiting for process exit.  The reader stores at most
;; max-bytes and drains the remainder so the child can finish; callers can ask
;; for the latest captured value even if timeout cleanup interrupts the thread.
(define (start-bounded-reader p max-bytes label)
  (define acc (open-output-bytes))
  (define lock (make-semaphore 1))
  (define truncated? #f)
  (define (record-bytes! bs [start 0] [end (bytes-length bs)])
    (call-with-semaphore lock (lambda () (write-bytes bs acc start end))))
  (define (mark-truncated!)
    (call-with-semaphore lock (lambda () (set! truncated? #t))))
  (define (snapshot)
    (call-with-semaphore lock
                         (lambda ()
                           (values (bytes->string/utf-8 (get-output-bytes acc) #\uFFFD) truncated?))))
  (define reader
    (thread (lambda ()
              (with-handlers ([exn:fail? (lambda (e)
                                           (log-subprocess-warning "~a: ~a" label (exn-message e)))])
                (unless (or (not p) (port-closed? p))
                  (define buf (make-bytes 4096))
                  (let loop ([remaining max-bytes])
                    (sync p)
                    (cond
                      [(> remaining 0)
                       (define n (read-bytes-avail! buf p))
                       (cond
                         [(eof-object? n) (void)]
                         [(number? n)
                          (define to-record (min n remaining))
                          (record-bytes! buf 0 to-record)
                          (when (> n remaining)
                            (mark-truncated!))
                          (loop (- remaining to-record))]
                         [else (void)])]
                      [else
                       ;; Budget exhausted.  Keep draining to EOF without storing so
                       ;; the child is not blocked by a full pipe.
                       (mark-truncated!)
                       (define n (read-bytes-avail! buf p))
                       (unless (eof-object? n)
                         (loop 0))])))))))
  (values reader snapshot))

;; --------------------------------------------------
;; Kill helper
;; --------------------------------------------------

(define (kill-subprocess! custodian)
  (custodian-shutdown-all custodian))

;; --------------------------------------------------
;; Shell quoting
;; --------------------------------------------------

;; shell-quote — POSIX single-quote escaping for command arguments.
;; SEC-16 (v0.22.0): Consolidated into util/shell-quote.rkt.
;; This file imports from there; no local definition needed.

;; --------------------------------------------------
;; Resolve command to executable path
;; --------------------------------------------------

(define (resolve-command cmd)
  (if (absolute-path? cmd)
      cmd
      (find-executable-path cmd)))

;; --------------------------------------------------
;; Environment sanitization — strip sensitive env vars
;; --------------------------------------------------

;; Patterns that indicate sensitive env vars
(define secret-patterns
  (list #rx"(?i:API.?KEY)"
        #rx"(?i:SECRET)"
        #rx"(?i:TOKEN)"
        #rx"(?i:PASSWORD)"
        #rx"(?i:CREDENTIAL)"
        #rx"(?i:^AUTH$|^AUTH_|_AUTH_)"
        #rx"(?i:GH_PAT)"
        #rx"(?i:_PAT$)"))

;; Built-in implicit allowlist: well-known non-secret env vars that should
;; never be scrubbed even if they partially match a secret pattern.
(define SECRET-IMPLICIT-ALLOWLIST '("XAUTHORITY" "GPG_AUTH_INFO" "AUTHOR" "GPG_TTY" "SSH_AUTH_SOCK"))

;; ── RA-2 (v0.24.7): Configurable secret scrubbing ──
;; Extra denylist: additional regex patterns to scrub (extends secret-patterns).
(define current-secret-scrub-denylist (make-parameter '()))

;; Allowlist: env vars matching these patterns are kept even if they match deny.
(define current-secret-scrub-allowlist (make-parameter '()))

(define (secret-env-var? name)
  (define name-str
    (if (bytes? name)
        (bytes->string/utf-8 name)
        name))
  ;; Check implicit allowlist first — well-known non-secret vars
  (cond
    [(member name-str SECRET-IMPLICIT-ALLOWLIST) #f]
    [else
     ;; Check user allowlist — if matched, never scrub
     (define allowed?
       (for/or ([pat (in-list (current-secret-scrub-allowlist))])
         (regexp-match? pat name-str)))
     (cond
       [allowed? #f]
       [else
        ;; Check default patterns + extra denylist
        (define all-patterns (append secret-patterns (current-secret-scrub-denylist)))
        (for/or ([pat (in-list all-patterns)])
          (regexp-match? pat name-str))])]))

(define (sanitize-env [env (current-environment-variables)])
  (define clean (make-environment-variables))
  (define scrubbed-names '())
  (for ([name (in-list (environment-variables-names env))])
    (cond
      [(secret-env-var? name)
       (define name-str
         (if (bytes? name)
             (bytes->string/utf-8 name)
             name))
       (set! scrubbed-names (cons name-str scrubbed-names))]
      [else
       (define val (environment-variables-ref env name))
       (environment-variables-set! clean name val)]))
  ;; RA-2: Emit audit log when vars are scrubbed
  (unless (null? scrubbed-names)
    (log-subprocess-info "sanitize-env: scrubbed ~a env vars: ~a"
                         (length scrubbed-names)
                         (string-join (sort scrubbed-names string<?) ", ")))
  clean)

;; --------------------------------------------------
;; Main subprocess runner
;; --------------------------------------------------

(define (run-subprocess command
                        #:args [args '()]
                        #:limits [limits (default-exec-limits)]
                        #:timeout [timeout-secs #f]
                        #:directory [dir (current-directory)]
                        #:environment [env (sanitize-env)]
                        #:encoding [encoding 'utf-8])
  (define effective-timeout (or timeout-secs (exec-limits-timeout-seconds limits)))
  (define max-output (exec-limits-max-output-bytes limits))

  (define cust (make-custodian (current-custodian)))
  (define start-ms (current-inexact-milliseconds))

  (with-handlers ([exn:fail? (lambda (e)
                               (custodian-shutdown-all cust)
                               (subprocess-result
                                -1
                                ""
                                (format "Failed to execute: ~a" (exn-message e))
                                #f
                                (inexact->exact (round (- (current-inexact-milliseconds) start-ms)))
                                #f))])

    (define cmd-path (resolve-command command))
    (unless cmd-path
      (error 'run-subprocess "command not found: ~a" command))

    ;; Build shell command line when args are provided
    (define cmd-string
      (if (path? cmd-path)
          (path->string cmd-path)
          cmd-path))

    (define-values (sp stdout-in stdin-out stderr-in)
      (parameterize ([current-custodian cust]
                     [current-directory dir]
                     [current-environment-variables env])
        (if (null? args)
            ;; No args: run command directly
            (subprocess #f #f #f cmd-path)
            ;; With args: pass as direct arg vector — avoids shell injection
            ;; (previously used /bin/sh -c which was vulnerable to injection)
            (apply subprocess #f #f #f cmd-path args))))

    ;; Close stdin immediately and start output drainers before waiting for the
    ;; child.  Waiting first can deadlock when child output fills the OS pipe.
    (close-output-port stdin-out)
    (define-values (stdout-reader get-stdout) (start-bounded-reader stdout-in max-output "stdout"))
    (define-values (stderr-reader get-stderr) (start-bounded-reader stderr-in max-output "stderr"))

    (define (reader-results)
      (define-values (out-str out-truncated?) (get-stdout))
      (define-values (err-str err-truncated?) (get-stderr))
      (values out-str err-str (or out-truncated? err-truncated?)))

    (define (finish-reader! reader port label)
      (unless (sync/timeout 1 reader)
        ;; A backgrounded grandchild can inherit stdout/stderr after the direct
        ;; subprocess exits.  Close our read end instead of waiting forever for
        ;; an EOF that may not arrive until the grandchild exits.
        (with-handlers ([exn:fail?
                         (lambda (e)
                           (log-subprocess-warning "close inherited ~a: ~a" label (exn-message e)))])
          (close-input-port port))
        (sync/timeout 0.25 reader)))

    ;; Wait with timeout
    (define evt-result (sync/timeout effective-timeout sp))

    (cond
      ;; Timeout
      [(not evt-result)
       ;; Kill the subprocess explicitly before shutting custodian.
       ;; May fail if process already dead.
       (with-handlers ([exn:fail? (lambda (e)
                                    (log-subprocess-warning "subprocess-kill: ~a" (exn-message e)))])
         (subprocess-kill sp))
       ;; Give reader threads a bounded chance to observe EOF and flush their
       ;; latest snapshots.  If they do not finish, snapshot still returns the
       ;; latest captured bytes.
       (sync/timeout 1 stdout-reader)
       (sync/timeout 1 stderr-reader)
       (define-values (partial-out partial-err partial-truncated?) (reader-results))
       (define end-ms (current-inexact-milliseconds))
       (custodian-shutdown-all cust)
       (subprocess-result
        -9
        partial-out
        (string-append
         partial-err
         (format "\n[SYS] Command timed out after ~a seconds. Partial output shown above."
                 effective-timeout))
        #t
        (inexact->exact (round (- end-ms start-ms)))
        partial-truncated?)]

      ;; Completed
      [else
       ;; Process exited; readers should finish after draining stdout/stderr.
       ;; Bound the join to preserve the background-child inherited-pipe fix.
       (finish-reader! stdout-reader stdout-in "stdout")
       (finish-reader! stderr-reader stderr-in "stderr")
       (define-values (out-str err-str output-truncated?) (reader-results))
       (define exit-code (subprocess-status sp))

       ;; Close ports; may already be closed by EOF/custodian shutdown.
       (with-handlers ([exn:fail? (lambda (e)
                                    (log-subprocess-warning "close stdout: ~a" (exn-message e)))])
         (close-input-port stdout-in))
       (with-handlers ([exn:fail? (lambda (e)
                                    (log-subprocess-warning "close stderr: ~a" (exn-message e)))])
         (close-input-port stderr-in))
       (custodian-shutdown-all cust)

       (define end-ms (current-inexact-milliseconds))
       (subprocess-result exit-code
                          out-str
                          err-str
                          #f
                          (inexact->exact (round (- end-ms start-ms)))
                          output-truncated?)])))
