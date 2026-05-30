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

(provide
 ;; Struct accessors (no contract needed for transparent struct)
 subprocess-result
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
 (contract-out
  [run-subprocess
   (->* (string?)
        (#:args (listof string?)
         #:limits exec-limits?
         #:timeout (or/c number? #f)
         #:directory (or/c path-string? #f)
         #:environment any/c
         #:encoding symbol?)
        subprocess-result?)]
  [kill-subprocess! (-> any/c void?)]
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

;; Non-blocking read — reads only bytes already available
(define (read-available-bounded p max-bytes)
  (if (or (not p) (port-closed? p))
      ""
      ;; Non-blocking read: swallow port errors (port may close
      ;; mid-read due to custodian shutdown during timeout).
      (with-handlers ([exn:fail? (lambda (e)
                                   (log-subprocess-warning "read-available-bounded: ~a"
                                                           (exn-message e))
                                   "")])
        (define acc (open-output-bytes))
        (let loop ([remaining max-bytes])
          (when (and (> remaining 0) (sync/timeout 0 p))
            (define buf-size (min 4096 remaining))
            (define bs (read-bytes buf-size p))
            (cond
              [(eof-object? bs) (void)]
              [(bytes? bs)
               (write-bytes bs acc)
               (loop (- remaining (bytes-length bs)))]
              [else (void)])))
        (get-output-string acc))))

(define (read-port-bounded p max-bytes)
  (if (or (not p) (port-closed? p))
      ""
      (let loop ([acc (open-output-bytes)]
                 [remaining max-bytes])
        (define buf-size (min 4096 remaining))
        (if (<= remaining 0)
            ;; Output hit the byte budget — drain rest and mark truncated
            ;; Discard remainder past byte budget; port may already be
            ;; closed by custodian, so silently swallow errors.
            (begin
              (with-handlers ([exn:fail? (lambda (e)
                                           (log-subprocess-warning "read-port-bounded discard: ~a"
                                                                   (exn-message e)))])
                (copy-port p (open-output-bytes))) ; discard remainder
              (string-append (get-output-string acc)
                             (format "\n[output truncated at ~a bytes]" max-bytes)))
            (let ([bs (read-bytes buf-size p)])
              (cond
                [(eof-object? bs) (get-output-string acc)]
                [else
                 (write-bytes bs acc)
                 (loop acc (- remaining (bytes-length bs)))]))))))

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

    ;; Close stdin immediately
    (close-output-port stdin-out)

    ;; Wait with timeout
    (define evt-result (sync/timeout effective-timeout sp))

    (cond
      ;; Timeout
      [(not evt-result)
       ;; Collect partial output BEFORE killing — ports are still readable
       ;; Collect partial output before killing; ports may be closed
       ;; by the OS if the process already exited.
       (define partial-out
         (with-handlers ([exn:fail? (lambda (e)
                                      (log-subprocess-warning "partial stdout: ~a" (exn-message e))
                                      "")])
           (read-available-bounded stdout-in max-output)))
       (define partial-err
         (with-handlers ([exn:fail? (lambda (e)
                                      (log-subprocess-warning "partial stderr: ~a" (exn-message e))
                                      "")])
           (read-available-bounded stderr-in max-output)))
       ;; Kill the subprocess explicitly before shutting custodian.
       ;; May fail if process already dead.
       (with-handlers ([exn:fail? (lambda (e)
                                    (log-subprocess-warning "subprocess-kill: ~a" (exn-message e)))])
         (subprocess-kill sp))
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
        #f)] ; truncated? — partial read, not byte-budget truncation

      ;; Completed
      [else
       (define out-str (read-port-bounded stdout-in max-output))
       (define err-str (read-port-bounded stderr-in max-output))
       (define exit-code (subprocess-status sp))
       (define out-truncated? (string-contains? out-str "[output truncated at"))

       ;; Close ports; may already be closed by custodian shutdown.
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
                          out-truncated?)])))
