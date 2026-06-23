#lang racket/base

;; @speed slow
;; @suite tui-tmux
;; @boundary e2e

;; tests/helpers/tmux-q-harness.rkt — Reusable tmux test harness for q --tui
;;
;; Provides a test-first API for launching and controlling q --tui inside
;; real tmux pseudo-terminal sessions. All tests using this harness are
;; opt-in via the Q_TMUX_TUI_TESTS environment variable:
;;
;;   Q_TMUX_TUI_TESTS unset/0 -> skip real tmux scenarios with clear message
;;   Q_TMUX_TUI_TESTS=1       -> run if tmux is available, skip if not
;;   Q_TMUX_TUI_TESTS=require -> fail if tmux is unavailable
;;
;; Every test uses:
;;   - tmux -f /dev/null (no user tmux config)
;;   - temp HOME (never real HOME)
;;   - temp project/session/artifact directories
;;   - TERM=xterm-256color
;;   - Fixed dimensions: -x 100 -y 30
;;   - Hard timeout per scenario
;;   - Cleanup in dynamic-wind
;;   - No real credentials, no network

(require json
         racket/file
         racket/string
         racket/format
         racket/list
         racket/system
         (only-in racket/port port->string))

;; Struct
(provide (struct-out tmux-q-session)
         (struct-out tmux-env)

         ;; Availability and mode
         tmux-available?
         tmux-test-mode
         tmux-test-mode->symbol
         should-run-tmux-tests?
         require-tmux-or-skip!

         ;; Environment
         make-tmux-test-env

         ;; Session lifecycle
         start-q-tui-session!
         stop-session!
         session-alive?

         ;; Interaction
         send-line!
         send-key!
         resize-session!

         ;; Capture
         capture-pane
         capture-normalized

         ;; Waiting
         wait-for-text
         wait-for-predicate
         wait-for-exit

         ;; Convenience macro
         with-tmux-q-session

         ;; Diagnostics
         write-failure-artifacts!
         text-found-in?
         make-session-name

         ;; Artifact discovery
         find-session-subdirs
         find-first-session-subdir

         ;; Structured trace/event completion
         find-trace-jsonl-paths
         read-trace-events
         trace-entry-phase
         trace-entry-turn-id
         turn-completion-trace-entry?
         latest-turn-completion-event
         wait-for-turn-completion-event

         ;; Real-provider safe helpers
         real-provider-authorized?
         require-real-provider-authorization!
         make-real-provider-tmux-env
         copy-q-config-to-temp-home!
         (struct-out prompt-result)
         send-prompt-and-wait!
         detect-queued-prompts
         assert-no-queued-prompts!
         write-exploration-artifacts!

         ;; Pure functions (for unit testing)
         normalize-pane-output
         build-tmux-new-session-command
         build-tmux-send-keys-command
         build-tmux-send-key-command
         build-tmux-capture-pane-command
         build-tmux-resize-command
         build-tmux-kill-session-command
         build-tmux-list-sessions-command
         redact-sensitive)

;; ============================================================
;; Struct
;; ============================================================

(struct tmux-q-session
        (name ; string: tmux session name
         cwd ; string: project working directory
         home ; string: temp HOME directory
         project-dir ; string: temp project directory
         session-dir ; string: q session dir (under HOME/.q/sessions)
         artifact-dir ; string: where to write failure artifacts
         command ; string: the shell command launched
         cols ; exact-nonnegative-integer
         rows ; exact-nonnegative-integer
         started-at) ; exact-integer: current-inexact-milliseconds at start
  #:transparent)

(struct tmux-env (home project-dir session-dir artifact-dir) #:transparent)

;; ============================================================
;; Availability detection
;; ============================================================

(define (tmux-available?)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define-values (proc out-in _stdin err) (subprocess #f #f #f (find-executable-path "tmux") "-V"))
    (subprocess-wait proc)
    (define ok? (eq? (subprocess-status proc) 0))
    (close-input-port out-in)
    (close-input-port err)
    ok?))

;; ============================================================
;; Test mode (env var handling)
;; ============================================================

(define (tmux-test-mode)
  (getenv "Q_TMUX_TUI_TESTS"))

(define (tmux-test-mode->symbol val)
  (cond
    [(not val) 'skip]
    [(string=? val "") 'skip]
    [(string=? val "0") 'skip]
    [(string=? val "1") 'run]
    [(string=? val "require") 'require]
    [else 'skip]))

(define (should-run-tmux-tests?)
  (define mode (tmux-test-mode->symbol (tmux-test-mode)))
  (case mode
    [(skip) #f]
    [(run) (tmux-available?)]
    [(require)
     (unless (tmux-available?)
       (error 'tmux-tests "Q_TMUX_TUI_TESTS=require but tmux is not available"))
     #t]))

(define (require-tmux-or-skip!)
  (unless (should-run-tmux-tests?)
    (printf "SKIP: tmux tests not enabled (set Q_TMUX_TUI_TESTS=1)~n")))

;; ============================================================
;; Test environment creation
;; ============================================================

(define (make-tmux-test-env #:base-dir [base-dir #f])
  (define tmp-base (or base-dir (find-system-path 'temp-dir)))
  (define stamp (format "~a" (current-inexact-milliseconds)))
  (define home (build-path tmp-base (format "q-tmux-home-~a" stamp)))
  (define project-dir (build-path tmp-base (format "q-tmux-proj-~a" stamp)))
  (define session-dir (build-path home ".q" "sessions"))
  (define artifact-dir (build-path tmp-base (format "q-tmux-art-~a" stamp)))
  (make-directory* home)
  (make-directory* project-dir)
  (make-directory* session-dir)
  (make-directory* artifact-dir)
  (tmux-env (path->string home)
            (path->string project-dir)
            (path->string session-dir)
            (path->string artifact-dir)))

;; ============================================================
;; Pure: ANSI/VT normalization
;; ============================================================

(define (normalize-pane-output text)
  (define step1 (string-replace text "\x00" ""))
  ;; Remove CSI sequences: ESC [ params intermediates final-byte
  (define step2 (regexp-replace* #px"\x1b\\[[0-9;?]*[ -/]*[@-~]" step1 ""))
  ;; Remove OSC sequences: ESC ] ... (BEL or ESC backslash)
  (define step3 (regexp-replace* #px"\x1b\\][^\x07\x1b]*(\x07|\x1b\\\\)" step2 ""))
  ;; Remove DCS/APC/PM/SOS: ESC P/^ _/^X ... (ESC \ or BEL)
  (define step4 (regexp-replace* #px"\x1b[P^_X][^\x1b]*(\x1b\\\\|\x07)" step3 ""))
  ;; Remove single-char ESC sequences (ESC + optional intermediates + final byte)
  (define step5 (regexp-replace* #px"\x1b[ -/]*[@-~]" step4 ""))
  ;; Remove remaining bare ESC characters
  (define step6 (string-replace step5 "\x1b" ""))
  ;; Normalize CRLF -> LF, CR -> empty
  (define step7 (string-replace step6 "\r\n" "\n"))
  (define step8 (string-replace step7 "\r" ""))
  ;; Process backspaces
  (define step9 (process-backspaces step8))
  ;; Normalize lines (trim trailing ws, remove trailing empty lines)
  (normalize-lines step9))

(define (process-backspaces str)
  (define chars (string->list str))
  (define result
    (let loop ([chars chars]
               [stack '()])
      (cond
        [(null? chars) (reverse stack)]
        [(char=? (car chars) #\backspace)
         (loop (cdr chars)
               (if (null? stack)
                   stack
                   (cdr stack)))]
        [else (loop (cdr chars) (cons (car chars) stack))])))
  (list->string result))

(define (normalize-lines str)
  (define lines (string-split str "\n" #:trim? #f))
  (define trimmed (map string-trim-end lines))
  (define no-trailing-empty
    (let drop-end ([ls trimmed])
      (cond
        [(null? ls) '("")]
        [(and (string=? (last ls) "") (> (length ls) 1)) (drop-end (drop-right ls 1))]
        [else ls])))
  (string-join no-trailing-empty "\n"))

(define (string-trim-end str)
  (regexp-replace #px"[ \t]+$" str ""))

;; ============================================================
;; Pure: Command construction
;; ============================================================

(define (build-tmux-new-session-command #:session-name name
                                        #:cols cols
                                        #:rows rows
                                        #:cwd cwd
                                        #:shell-command shell-cmd)
  (list "tmux"
        "-f"
        "/dev/null"
        "new-session"
        "-d"
        "-s"
        name
        "-x"
        (number->string cols)
        "-y"
        (number->string rows)
        "-c"
        cwd
        shell-cmd))

(define (build-tmux-send-keys-command session-name text #:press-enter? [enter? #t])
  (if enter?
      (list "tmux" "send-keys" "-t" session-name text "Enter")
      (list "tmux" "send-keys" "-t" session-name text)))

(define (build-tmux-send-key-command session-name key)
  (list "tmux" "send-keys" "-t" session-name key))

(define (build-tmux-capture-pane-command session-name #:lines [lines 50] #:raw? [raw? #f])
  (define base (list "tmux" "capture-pane" "-t" session-name "-p"))
  (define with-lines
    (if (and lines (> lines 0))
        (append base (list "-S" (format "-~a" lines)))
        base))
  (if raw?
      (append with-lines (list "-e"))
      with-lines))

(define (build-tmux-resize-command session-name cols rows)
  (list "tmux"
        "resize-window"
        "-t"
        session-name
        "-x"
        (number->string cols)
        "-y"
        (number->string rows)))

(define (build-tmux-kill-session-command session-name)
  (list "tmux" "kill-session" "-t" session-name))

(define (build-tmux-list-sessions-command)
  (list "tmux" "list-sessions"))

;; ============================================================
;; Pure: Redaction
;; ============================================================

(define (redact-sensitive text #:home-path [home-path #f])
  ;; Step 1: line-based KEY=VALUE redaction (existing)
  (define lines (string-split text "\n" #:trim? #f))
  (define step1-lines
    (for/list ([line (in-list lines)])
      (cond
        [(regexp-match? #px"^(HOME|.*(KEY|TOKEN|SECRET|PASSWORD|CREDENTIAL).*)=" line)
         (define key-parts (regexp-match #px"^([^=]+)=" line))
         (if key-parts
             (format "~a=<REDACTED>" (cadr key-parts))
             line)]
        [else line])))
  (define step1 (string-join step1-lines "\n"))
  ;; Step 2: redact bearer tokens
  (define step2 (regexp-replace* #px"(?i:bearer) +[A-Za-z0-9._-]+" step1 "Bearer <REDACTED>"))
  ;; Step 3: redact API key patterns (sk-...)
  (define step3 (regexp-replace* #px"sk-[A-Za-z0-9_-]+" step2 "<REDACTED>"))
  ;; Step 4: redact HOME path if provided
  (define step4
    (if (and home-path (> (string-length home-path) 0))
        (string-replace step3 home-path "<HOME>")
        step3))
  step4)

;; ============================================================
;; Pure: Text matching
;; ============================================================

(define (text-found-in? needle haystack)
  (and (string? needle)
       (string? haystack)
       (> (string-length needle) 0)
       (string-contains? haystack needle)))

;; ============================================================
;; Session name generation
;; ============================================================

(define (make-session-name)
  ;; tmux session names must not contain '.' or ':' — use exact integer
  (format "q-test-~a" (current-milliseconds)))

;; ============================================================
;; Artifact discovery
;; ============================================================

;; find-session-subdirs : path-string? -> (listof string?)
;; Returns sorted list of subdirectory paths under the given session-dir.
;; These are the session-id directories q creates (e.g. 01KVSHZK79Y5J42EJHHX38G27D).
(define (find-session-subdirs session-dir-path)
  (with-handlers ([exn:fail? (lambda (e) '())])
    (if (directory-exists? session-dir-path)
        (sort (for/list ([entry (in-list (directory-list session-dir-path))]
                         #:when (directory-exists? (build-path session-dir-path entry)))
                (path->string (build-path session-dir-path entry)))
              string<?)
        '())))

;; find-first-session-subdir : path-string? -> (or/c #f string?)
;; Returns the first session subdirectory path, or #f if none exist.
(define (find-first-session-subdir session-dir-path)
  (define subs (find-session-subdirs session-dir-path))
  (and (pair? subs) (car subs)))

;; ============================================================
;; Structured trace/event completion
;; ============================================================

(define completion-phases
  '("turn.completed" "stream.turn.completed" "turn.cancelled" "stream.turn.cancelled"))

(define (->phase-string v)
  (cond
    [(symbol? v) (symbol->string v)]
    [(string? v) v]
    [else #f]))

(define (hash-ref/keys h keys [default #f])
  (let loop ([keys keys])
    (cond
      [(null? keys) default]
      [(hash-has-key? h (car keys)) (hash-ref h (car keys))]
      [else (loop (cdr keys))])))

(define (find-trace-jsonl-paths session-dir-path)
  (with-handlers ([exn:fail? (lambda (_e) '())])
    (sort (for/list ([subdir (in-list (find-session-subdirs session-dir-path))]
                     #:do [(define trace-path (build-path subdir "trace.jsonl"))]
                     #:when (file-exists? trace-path))
            (path->string trace-path))
          string<?)))

(define (read-trace-events trace-path)
  (with-handlers ([exn:fail? (lambda (_e) '())])
    (if (file-exists? trace-path)
        (call-with-input-file trace-path
                              (lambda (in)
                                (let loop ([acc '()])
                                  (define line (read-line in 'any))
                                  (cond
                                    [(eof-object? line) (reverse acc)]
                                    [(string=? (string-trim line) "") (loop acc)]
                                    [else
                                     (define parsed
                                       (with-handlers ([exn:fail? (lambda (_e) #f)])
                                         (string->jsexpr line)))
                                     (loop (if (hash? parsed)
                                               (cons parsed acc)
                                               acc))]))))
        '())))

(define (trace-entry-phase entry)
  (and (hash? entry) (->phase-string (hash-ref/keys entry '(phase "phase") #f))))

(define (trace-entry-turn-id entry)
  (and (hash? entry) (hash-ref/keys entry '(turnId "turnId" turn-id "turn-id") #f)))

(define (turn-completion-trace-entry? entry #:turn-id [turn-id #f])
  (define phase (trace-entry-phase entry))
  (define entry-turn-id (trace-entry-turn-id entry))
  (and phase
       (if (member phase completion-phases) #t #f)
       (or (not turn-id) (equal? entry-turn-id turn-id))))

(define (latest-turn-completion-event session-dir-path #:turn-id [turn-id #f])
  (define matches
    (for*/list ([trace-path (in-list (find-trace-jsonl-paths session-dir-path))]
                [entry (in-list (read-trace-events trace-path))]
                #:when (turn-completion-trace-entry? entry #:turn-id turn-id))
      entry))
  (and (pair? matches) (last matches)))

(define (wait-for-turn-completion-event sess
                                        #:turn-id [turn-id #f]
                                        #:timeout-ms [timeout-ms 30000]
                                        #:poll-ms [poll-ms 250])
  (define found #f)
  (define ok?
    (wait-for-predicate
     sess
     (lambda ()
       (set! found (latest-turn-completion-event (tmux-q-session-session-dir sess) #:turn-id turn-id))
       found)
     #:timeout-ms timeout-ms
     #:poll-ms poll-ms))
  (and ok? found))

;; ============================================================
;; Real-provider safe helpers
;; ============================================================

(define (real-provider-authorized?)
  (and (equal? (getenv "Q_TMUX_TUI_TESTS") "1")
       (equal? (getenv "Q_TMUX_TUI_REAL_PROVIDER") "1")
       (equal? (getenv "Q_TMUX_TUI_REAL_PROVIDER_CONFIRM") "I_UNDERSTAND_COSTS")))

(define (require-real-provider-authorization!)
  (unless (real-provider-authorized?)
    (error 'tmux-q-harness
           (string-append "real-provider mode requires Q_TMUX_TUI_TESTS=1, "
                          "Q_TMUX_TUI_REAL_PROVIDER=1, and "
                          "Q_TMUX_TUI_REAL_PROVIDER_CONFIRM=I_UNDERSTAND_COSTS"))))

;; make-real-provider-tmux-env : [#:base-dir path-string?] -> tmux-env?
;; Creates an isolated temp environment. Only callable when real-provider
;; gates are satisfied. Does not copy config/credentials automatically;
;; use copy-q-config-to-temp-home! for that.
(define (make-real-provider-tmux-env #:base-dir [base-dir #f])
  (require-real-provider-authorization!)
  (make-tmux-test-env #:base-dir base-dir))

;; Q config files to copy for real-provider mode.
;; Listed by name only — contents are never printed.
(define q-config-files '("config.json" "credentials.json" "config.rkt"))

;; copy-q-config-to-temp-home! : tmux-env? [#:source-home path-string?] -> (listof string?)
;; Copies ~/.q/{config.json,credentials.json,config.rkt} into temp HOME .q/.
;; Returns list of copied file names (not contents). Prints nothing.
(define (copy-q-config-to-temp-home! env
                                     #:source-home
                                     [source-home (path->string (find-system-path 'home-dir))])
  (define source-q-dir (build-path source-home ".q"))
  (define dest-q-dir (build-path (tmux-env-home env) ".q"))
  (make-directory* dest-q-dir)
  (for/list ([fname (in-list q-config-files)]
             #:when (file-exists? (build-path source-q-dir fname)))
    ;; Read bytes and write bytes — never print or log contents
    (define src-bytes (file->bytes (build-path source-q-dir fname)))
    (call-with-output-file (build-path dest-q-dir fname)
                           (lambda (out) (write-bytes src-bytes out))
                           #:exists 'replace)
    fname))

;; prompt-result struct for send-prompt-and-wait!
(struct prompt-result (prompt completion-event capture status) #:transparent)

;; send-prompt-and-wait! : tmux-q-session? string? [#:timeout-ms ...] -> prompt-result?
;; Sends a prompt and waits for structured turn-completion via trace.jsonl.
;; Returns a prompt-result with status 'completed, 'timeout, or 'no-trace.
(define (send-prompt-and-wait! sess prompt #:timeout-ms [timeout-ms 30000] #:poll-ms [poll-ms 250])
  (send-line! sess prompt)
  (define completion-event
    (wait-for-turn-completion-event sess #:timeout-ms timeout-ms #:poll-ms poll-ms))
  (define capture (capture-normalized sess #:lines 80))
  (define status
    (cond
      [completion-event 'completed]
      [(string-contains? capture "trace.jsonl") 'no-trace]
      [else 'timeout]))
  (prompt-result prompt completion-event capture status))

;; detect-queued-prompts : string? -> boolean?
;; Pure: checks normalized pane text for queued-prompt indicators.
(define (detect-queued-prompts text)
  (and (string? text) (string-contains? text "[Queued") #t))

;; assert-no-queued-prompts! : tmux-q-session? -> void?
;; Fails if the pane shows a queued prompt indicator.
(define (assert-no-queued-prompts! sess)
  (define pane (capture-normalized sess #:lines 80))
  (when (detect-queued-prompts pane)
    (error 'assert-no-queued-prompts! "queued prompt detected in pane — automation sent too early")))

;; write-exploration-artifacts! : tmux-q-session? string? [#:status ...] -> path-string?
;; Writes exploration artifacts (not just failures) including:
;; - normalized-capture.txt (redacted)
;; - env-summary.txt (redacted)
;; - trace-events.jsonl (if trace exists)
;; Returns the artifact directory path.
(define (write-exploration-artifacts! sess label #:status [status "exploration"])
  (define art-dir (tmux-q-session-artifact-dir sess))
  (define name (tmux-q-session-name sess))
  ;; Normalized capture (redacted)
  (define normalized (capture-normalized sess #:lines 100))
  (call-with-output-file
   (build-path art-dir (format "~a-normalized.txt" label))
   (lambda (p) (display (redact-sensitive normalized #:home-path (tmux-q-session-home sess)) p))
   #:exists 'replace)
  ;; Env summary (redacted)
  (define env-summary
    (format "Label: ~a\nStatus: ~a\nSession: ~a\nCols: ~a\nRows: ~a\nHOME=<HOME>\nProject=~a\n"
            label
            status
            name
            (tmux-q-session-cols sess)
            (tmux-q-session-rows sess)
            (tmux-q-session-cwd sess)))
  (call-with-output-file (build-path art-dir (format "~a-env-summary.txt" label))
                         (lambda (p) (display (redact-sensitive env-summary) p))
                         #:exists 'replace)
  ;; Trace events summary (if available)
  (define trace-paths (find-trace-jsonl-paths (tmux-q-session-session-dir sess)))
  (unless (null? trace-paths)
    (define all-events (apply append (map read-trace-events trace-paths)))
    (define phases (map trace-entry-phase all-events))
    (define completion-count
      (length (filter (lambda (p) (and p (member p completion-phases))) phases)))
    (call-with-output-file (build-path art-dir (format "~a-trace-summary.txt" label))
                           (lambda (p)
                             (fprintf p "Trace files: ~a\n" (length trace-paths))
                             (fprintf p "Total events: ~a\n" (length all-events))
                             (fprintf p "Completion events: ~a\n" completion-count)
                             (fprintf p "Phases: ~a\n" phases))
                           #:exists 'replace))
  art-dir)

;; ============================================================
;; tmux command execution (internal)
;; ============================================================

(define (run-tmux-command args #:capture? [capture? #f])
  (define tmux-path (find-executable-path "tmux"))
  (cond
    [(not tmux-path) (error 'run-tmux-command "tmux not found")]
    [capture?
     (define-values (proc out-port _stdin err-port) (apply subprocess #f #f #f tmux-path (cdr args)))
     (subprocess-wait proc)
     (define status (subprocess-status proc))
     (define output (port->string out-port))
     (close-input-port out-port)
     (close-input-port err-port)
     (values status output)]
    [else
     (define-values (proc out-port _stdin err-port) (apply subprocess #f #f #f tmux-path (cdr args)))
     (subprocess-wait proc)
     (define status (subprocess-status proc))
     (close-input-port out-port)
     (close-input-port err-port)
     (values status "")]))

;; Compute the q source root from this module's location.
;; The harness is at tests/helpers/tmux-q-harness.rkt, so the q root is ../..
(define q-source-root
  (let* ([this-file (resolved-module-path-name (variable-reference->resolved-module-path
                                                (#%variable-reference)))]
         [this-dir (let-values ([(base _name _dir?) (split-path this-file)])
                     base)])
    (path->string (simplify-path (build-path this-dir ".." "..")))))

;; Build the full shell command for q --tui
(define (build-q-tui-command home-dir project-dir args)
  (define racket-path (find-executable-path "racket"))
  (define main-path (path->string (build-path q-source-root "main.rkt")))
  (format "HOME=~a TERM=xterm-256color ~a ~a ~a"
          home-dir
          (path->string racket-path)
          main-path
          (string-join args " ")))

;; ============================================================
;; Session lifecycle
;; ============================================================

(define (start-q-tui-session! env
                              #:args [args '("--tui" "--no-tools")]
                              #:cols [cols 100]
                              #:rows [rows 30])
  (define name (make-session-name))
  (define home (tmux-env-home env))
  (define project-dir (tmux-env-project-dir env))
  (define session-dir (tmux-env-session-dir env))
  (define artifact-dir (tmux-env-artifact-dir env))
  (define q-cmd (build-q-tui-command home project-dir args))
  (define cmd
    (build-tmux-new-session-command #:session-name name
                                    #:cols cols
                                    #:rows rows
                                    #:cwd project-dir
                                    #:shell-command q-cmd))
  (define-values (status _output) (run-tmux-command cmd))
  (unless (eq? status 0)
    (error 'start-q-tui-session! "tmux new-session failed with status ~a" status))
  (tmux-q-session name
                  project-dir
                  home
                  project-dir
                  session-dir
                  artifact-dir
                  q-cmd
                  cols
                  rows
                  (current-inexact-milliseconds)))

(define (stop-session! sess)
  (define cmd (build-tmux-kill-session-command (tmux-q-session-name sess)))
  (with-handlers ([exn:fail? (lambda (e) (void))])
    (run-tmux-command cmd)))

(define (session-alive? sess)
  (define cmd (build-tmux-list-sessions-command))
  (define-values (status output) (run-tmux-command cmd #:capture? #t))
  (and (eq? status 0) (string-contains? output (tmux-q-session-name sess))))

;; ============================================================
;; Interaction
;; ============================================================

(define (send-line! sess text)
  (define cmd (build-tmux-send-keys-command (tmux-q-session-name sess) text))
  (define-values (status _) (run-tmux-command cmd))
  (unless (eq? status 0)
    (error 'send-line! "tmux send-keys failed with status ~a" status)))

(define (send-key! sess key)
  (define cmd (build-tmux-send-key-command (tmux-q-session-name sess) key))
  (define-values (status _) (run-tmux-command cmd))
  (unless (eq? status 0)
    (error 'send-key! "tmux send-keys failed with status ~a" status)))

;; resize-session! : tmux-q-session? exact-nonnegative-integer? exact-nonnegative-integer? -> void?
;; Resize the tmux window to the given cols x rows.
(define (resize-session! sess cols rows)
  (define cmd (build-tmux-resize-command (tmux-q-session-name sess) cols rows))
  (define-values (status _) (run-tmux-command cmd))
  (unless (eq? status 0)
    (error 'resize-session! "tmux resize-window failed with status ~a" status)))

;; ============================================================
;; Capture
;; ============================================================

(define (capture-pane sess #:raw? [raw? #f] #:lines [lines 50])
  (define cmd (build-tmux-capture-pane-command (tmux-q-session-name sess) #:lines lines #:raw? raw?))
  (define-values (status output) (run-tmux-command cmd #:capture? #t))
  (if (eq? status 0) output ""))

(define (capture-normalized sess #:lines [lines 50])
  (normalize-pane-output (capture-pane sess #:lines lines)))

;; ============================================================
;; Waiting
;; ============================================================

(define (wait-for-predicate sess pred #:timeout-ms [timeout-ms 30000] #:poll-ms [poll-ms 500])
  (define deadline (+ (current-inexact-milliseconds) timeout-ms))
  (let loop ()
    (cond
      [(pred) #t]
      [(> (current-inexact-milliseconds) deadline) #f]
      [else
       (sleep (/ poll-ms 1000.0))
       (loop)])))

(define (wait-for-text sess text #:timeout-ms [timeout-ms 30000] #:poll-ms [poll-ms 500])
  (wait-for-predicate sess
                      (lambda ()
                        (define pane (capture-normalized sess))
                        (text-found-in? text pane))
                      #:timeout-ms timeout-ms
                      #:poll-ms poll-ms))

(define (wait-for-exit sess #:timeout-ms [timeout-ms 15000])
  (wait-for-predicate sess
                      (lambda () (not (session-alive? sess)))
                      #:timeout-ms timeout-ms
                      #:poll-ms 200))

;; ============================================================
;; Failure artifacts
;; ============================================================

(define (write-failure-artifacts! sess reason)
  (define art-dir (tmux-q-session-artifact-dir sess))
  (define name (tmux-q-session-name sess))
  ;; Raw capture
  (define raw (capture-pane sess #:raw? #t))
  (call-with-output-file (build-path art-dir "raw-capture.txt")
                         (lambda (p) (display raw p))
                         #:exists 'replace)
  ;; Normalized capture
  (define normalized (normalize-pane-output raw))
  (call-with-output-file (build-path art-dir "normalized-capture.txt")
                         (lambda (p) (display normalized p))
                         #:exists 'replace)
  ;; Env summary (redacted)
  (define env-summary
    (format "Session: ~a\nCommand: ~a\nReason: ~a\nCols: ~a\nRows: ~a\nHOME=~a\nProject=~a\n"
            name
            (tmux-q-session-command sess)
            reason
            (tmux-q-session-cols sess)
            (tmux-q-session-rows sess)
            (tmux-q-session-home sess)
            (tmux-q-session-cwd sess)))
  (call-with-output-file (build-path art-dir "env-summary.txt")
                         (lambda (p) (display (redact-sensitive env-summary) p))
                         #:exists 'replace)
  ;; Temp tree listing
  (define tree-output
    (with-handlers ([exn:fail? (lambda (e) "Error generating temp tree")])
      (define home (tmux-q-session-home sess))
      (format "HOME tree:\n~a\n" (list-tree home))))
  (call-with-output-file (build-path art-dir "temp-tree.txt")
                         (lambda (p) (display tree-output p))
                         #:exists 'replace)
  art-dir)

(define (list-tree dir)
  (define (walk path prefix)
    (define entries
      (with-handlers ([exn:fail? (lambda (e) '())])
        (sort (directory-list path) path<?)))
    (string-append* (for/list ([entry (in-list entries)])
                      (define full (build-path path entry))
                      (define is-dir (directory-exists? full))
                      (define line (format "~a~a~a\n" prefix entry (if is-dir "/" "")))
                      (if is-dir
                          (string-append line (walk full (string-append prefix "  ")))
                          line))))
  (walk dir ""))

;; ============================================================
;; Convenience macro: with-tmux-q-session
;; ============================================================

(define-syntax-rule (with-tmux-q-session (sess-var) opts body ...)
  (let-values ([(sess-var env) (let ()
                                 (define env (make-tmux-test-env))
                                 (define sess-var (start-q-tui-session! env . opts))
                                 (values sess-var env))])
    (dynamic-wind (lambda () (void))
                  (lambda ()
                    body ...)
                  (lambda ()
                    (with-handlers ([exn:fail? (lambda (e)
                                                 (write-failure-artifacts! sess-var "cleanup"))])
                      (when (session-alive? sess-var)
                        (stop-session! sess-var)))))))
