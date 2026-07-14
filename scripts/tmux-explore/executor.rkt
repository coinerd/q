#lang racket/base

;; scripts/tmux-explore/executor.rkt — Truthful isolated real tmux executor
;;
;; v0.99.50 W3 (TMUX-03): Real mode must launch an actual q TUI in a uniquely
;; named tmux session, observe structured trace completion, retain only
;; redacted artifacts, and delete the copied-credential workspace on every
;; exit path. This module deliberately does not import test helpers.

(require json
         racket/file
         racket/list
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         "../../util/credential-redaction.rkt")

(provide call-with-executor-cleanup
         run-real-scenario)

(define-runtime-path q-root "../..")

(define config-file-names '("config.json" "credentials.json" "config.rkt"))
(define completion-phases '("turn.completed" "stream.turn.completed"))

(define (call-with-executor-cleanup workspace
                                    session-name
                                    thunk
                                    #:stop-session [stop-session (lambda (_name) (void))])
  (dynamic-wind void
                thunk
                (lambda ()
                  ;; Capture stop failure, still remove credential files, then fail loudly.
                  (define stop-error #f)
                  (when session-name
                    (with-handlers ([exn:fail? (lambda (error) (set! stop-error error))])
                      (stop-session session-name)))
                  (when (directory-exists? workspace)
                    (delete-directory/files workspace))
                  (when stop-error
                    (raise stop-error)))))

(define (command-output executable . args)
  (define-values (process stdout stdin stderr) (apply subprocess #f #f #f executable args))
  (close-output-port stdin)
  ;; Drain both pipes concurrently so a verbose child cannot deadlock while
  ;; one pipe waits for EOF and the other fills its OS buffer.
  (define output-box (box ""))
  (define error-box (box ""))
  (define output-reader (thread (lambda () (set-box! output-box (port->string stdout)))))
  (define error-reader (thread (lambda () (set-box! error-box (port->string stderr)))))
  (subprocess-wait process)
  (thread-wait output-reader)
  (thread-wait error-reader)
  (values (subprocess-status process) (unbox output-box) (unbox error-box)))

(define (command-success? executable . args)
  (define-values (status _output _error) (apply command-output executable args))
  (zero? status))

(define (shell-quote value)
  (string-append "'" (string-replace (format "~a" value) "'" "'\"'\"'") "'"))

(define (environment-timeout-ms)
  (define raw (getenv "Q_TMUX_TUI_EXPLORER_TIMEOUT_MS"))
  (define parsed (and raw (string->number raw)))
  (if (and (exact-integer? parsed) (positive? parsed)) parsed 120000))

(define (source-home-path)
  (define raw (getenv "Q_TMUX_TUI_REAL_PROVIDER_HOME"))
  (unless (and raw (not (string=? (string-trim raw) "")))
    (error 'tmux-tui-explore
           "real mode requires Q_TMUX_TUI_REAL_PROVIDER_HOME naming an explicit source HOME"))
  (define path (path->complete-path raw))
  (unless (directory-exists? path)
    (error 'tmux-tui-explore "real-provider source HOME does not exist: ~a" path))
  path)

(define (copy-recognized-config! source-home destination-home)
  (define source-q (build-path source-home ".q"))
  (define destination-q (build-path destination-home ".q"))
  (make-directory* destination-q)
  (define copied
    (for/list ([name (in-list config-file-names)]
               #:when (file-exists? (build-path source-q name)))
      (copy-file (build-path source-q name) (build-path destination-q name) #t)
      name))
  (when (null? copied)
    (error
     'tmux-tui-explore
     "source HOME has none of the recognized q config files: config.json, credentials.json, config.rkt"))
  copied)

(define (delete-copied-config! destination-home copied)
  (for ([name (in-list copied)])
    (define path (build-path destination-home ".q" name))
    (when (file-exists? path)
      (delete-file path))))

(define (safe-tag tag)
  (regexp-replace* #px"[^A-Za-z0-9_.-]+" tag "-"))

(define (write-launcher! path home project tools?)
  (call-with-output-file
   path
   (lambda (out)
     (define racket-path
       (path->string (or (find-executable-path "racket")
                         (error 'tmux-tui-explore "racket executable not found"))))
     (define safe-path (or (getenv "PATH") "/usr/bin:/bin"))
     (fprintf out "#!/bin/sh~n")
     (fprintf out "cd ~a~n" (shell-quote (path->string project)))
     ;; env -i prevents inherited provider keys and unrelated secrets from
     ;; entering q or tool subprocesses. Provider auth comes from temp HOME.
     (fprintf out
              "exec env -i HOME=~a TERM=xterm-256color PATH=~a LANG=C.UTF-8 ~a ~a --tui~a~n"
              (shell-quote (path->string home))
              (shell-quote safe-path)
              (shell-quote racket-path)
              (shell-quote (path->string (build-path q-root "main.rkt")))
              (if tools? "" " --no-tools")))
   #:exists 'replace)
  path)

(define (tmux-session-alive? tmux server-name session-name)
  (command-success? tmux "-L" server-name "has-session" "-t" session-name))

(define (stop-tmux-session! tmux server-name session-name)
  ;; A private -L server plus exact -t target prevents collision with user tmux.
  (when (tmux-session-alive? tmux server-name session-name)
    (unless (command-success? tmux "-L" server-name "kill-session" "-t" session-name)
      (error 'tmux-tui-explore "failed to stop owned tmux session"))
    (when (tmux-session-alive? tmux server-name session-name)
      (error 'tmux-tui-explore "owned tmux session remained alive after cleanup")))
  (void))

(define (capture-pane tmux server-name session-name)
  (define-values (status output _error)
    (command-output tmux "-L" server-name "capture-pane" "-p" "-t" session-name "-S" "-200"))
  (if (zero? status) output ""))

(define (send-line! tmux server-name session-name text)
  (unless (command-success? tmux "-L" server-name "send-keys" "-t" session-name "-l" text)
    (error 'tmux-tui-explore "failed to send scenario prompt"))
  (unless (command-success? tmux "-L" server-name "send-keys" "-t" session-name "Enter")
    (error 'tmux-tui-explore "failed to submit scenario prompt")))

(define (wait-until predicate timeout-ms [poll-ms 250])
  (define deadline (+ (current-inexact-milliseconds) timeout-ms))
  (let loop ()
    (cond
      [(predicate) #t]
      [(>= (current-inexact-milliseconds) deadline) #f]
      [else
       (sleep (/ poll-ms 1000.0))
       (loop)])))

(define (trace-paths sessions-root)
  (if (directory-exists? sessions-root)
      (for/list ([path (in-directory sessions-root)]
                 #:when (and (file-exists? path)
                             (equal? (path->string (file-name-from-path path)) "trace.jsonl")))
        path)
      '()))

(define (read-json-line line)
  (with-handlers ([exn:fail? (lambda (_error) #f)])
    (string->jsexpr line)))

(define (read-trace-events sessions-root)
  (append* (for/list ([path (in-list (sort (trace-paths sessions-root) path<?))])
             (with-handlers ([exn:fail? (lambda (_error) '())])
               (filter hash?
                       (for/list ([line (in-list (file->lines path))]
                                  #:unless (string=? (string-trim line) ""))
                         (read-json-line line)))))))

(define (event-phase event)
  (define raw
    (or (hash-ref event 'phase #f)
        (hash-ref event "phase" #f)
        (hash-ref event 'event #f)
        (hash-ref event "event" "")))
  (string-downcase (format "~a" raw)))

(define (completion-event? event)
  (member (event-phase event) completion-phases))

(define (trace-text events)
  (with-output-to-string (lambda ()
                           (for ([event (in-list events)])
                             (write-json event)
                             (newline)))))

(define (mock-provider-observed? capture events)
  (regexp-match? #px"(?i:mock response|mock-provider|mock_provider)"
                 (string-append capture "\n" (trace-text events))))

(define (write-redacted-artifacts! output-root tag session-name status capture events)
  (make-directory* output-root)
  (define prefix (safe-tag tag))
  (define capture-path (build-path output-root (format "~a-capture.txt" prefix)))
  (define trace-path (build-path output-root (format "~a-trace.jsonl" prefix)))
  (define metadata-path (build-path output-root (format "~a-execution.txt" prefix)))
  (call-with-output-file capture-path
                         (lambda (out) (display (redact-secrets capture) out))
                         #:exists 'replace)
  (call-with-output-file trace-path
                         (lambda (out) (display (redact-secrets (trace-text events)) out))
                         #:exists 'replace)
  (call-with-output-file
   metadata-path
   (lambda (out)
     (fprintf out "session=~a~nstatus=~a~ntrace-events=~a~n" session-name status (length events)))
   #:exists 'replace)
  (path->string metadata-path))

(define (failed-observation output-root tag session-name message)
  (make-directory* output-root)
  (define path (build-path output-root (format "~a-execution-error.txt" (safe-tag tag))))
  (call-with-output-file path (lambda (out) (display (redact-secrets message) out)) #:exists 'replace)
  (hash 'status
        'failed
        'trace-events
        '()
        'capture
        ""
        'mock-provider?
        #f
        'timed-out?
        #f
        'crashed?
        #t
        'artifact-path
        (path->string path)
        'session-name
        session-name))

;; Launch one real scenario. The returned observation is intentionally raw
;; protocol evidence; scripts/tmux-explore/verifiers.rkt owns PASS/FAIL.
(define (run-real-scenario tag prompt output-root #:tools? [tools? #f])
  (define tmux
    (or (find-executable-path "tmux")
        (error 'tmux-tui-explore "tmux executable is required for real mode")))
  (define source-home (source-home-path))
  (define workspace (make-temporary-file "q-tmux-real-~a" 'directory))
  (define destination-home (build-path workspace "home"))
  (define project (build-path workspace "project"))
  (define sessions-root (build-path destination-home ".q" "sessions"))
  (define launcher (build-path workspace "launch.sh"))
  (define unique-suffix
    (format "~a-~a" (inexact->exact (floor (current-inexact-milliseconds))) (random 1000000)))
  (define server-name (format "q-explore-server-~a" unique-suffix))
  (define session-name (format "q-explore-~a" unique-suffix))
  (with-handlers ([exn:fail?
                   (lambda (error)
                     (failed-observation output-root tag session-name (exn-message error)))])
    (call-with-executor-cleanup
     workspace
     session-name
     (lambda ()
       (make-directory* destination-home)
       (make-directory* project)
       (define copied (copy-recognized-config! source-home destination-home))
       (call-with-output-file (build-path project "README.md")
                              (lambda (out)
                                (display "# Explorer Fixture\n\nREADME_FIXTURE_ALPHA\n" out))
                              #:exists 'replace)
       (write-launcher! launcher destination-home project tools?)
       (unless (command-success? tmux
                                 "-L"
                                 server-name
                                 "-f"
                                 "/dev/null"
                                 "new-session"
                                 "-d"
                                 "-s"
                                 session-name
                                 "-x"
                                 "100"
                                 "-y"
                                 "30"
                                 "-c"
                                 (path->string project)
                                 (format "/bin/sh ~a" (shell-quote (path->string launcher))))
         (error 'tmux-tui-explore "failed to start named tmux q session"))
       (define ready?
         (wait-until (lambda ()
                       (and (tmux-session-alive? tmux server-name session-name)
                            (string-contains? (capture-pane tmux server-name session-name) "q>")))
                     30000))
       (unless ready?
         (error 'tmux-tui-explore "q TUI did not become ready in the named tmux session"))
       ;; Read-only tools are auto-approved. Remove every copied config or
       ;; credential file before a tool-capable prompt. If provider setup was
       ;; lazy and this makes the turn fail, the result truthfully remains FAIL.
       (when tools?
         (delete-copied-config! destination-home copied))
       (define baseline-count (length (read-trace-events sessions-root)))
       (send-line! tmux server-name session-name prompt)
       (define timeout-ms (environment-timeout-ms))
       (define completed?
         (wait-until (lambda ()
                       (define all-events (read-trace-events sessions-root))
                       (and (> (length all-events) baseline-count)
                            (findf completion-event? (drop all-events baseline-count))))
                     timeout-ms))
       (define all-events (read-trace-events sessions-root))
       (define events
         (if (>= (length all-events) baseline-count)
             (drop all-events baseline-count)
             '()))
       (define alive? (tmux-session-alive? tmux server-name session-name))
       (define capture (capture-pane tmux server-name session-name))
       (define status
         (cond
           [completed? 'completed]
           [alive? 'timed-out]
           [else 'failed]))
       (define mock? (mock-provider-observed? capture events))
       (define artifact-path
         (write-redacted-artifacts! output-root tag session-name status capture events))
       (hash 'status
             status
             'trace-events
             events
             'capture
             capture
             'provider-confirmed?
             (and completed? (not mock?))
             'mock-provider?
             mock?
             'timed-out?
             (eq? status 'timed-out)
             'crashed?
             (and (not alive?) (not completed?))
             'artifact-path
             artifact-path
             'session-name
             session-name))
     #:stop-session (lambda (name) (stop-tmux-session! tmux server-name name)))))
