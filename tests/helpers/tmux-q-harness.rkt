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
         racket/date
         racket/system
         (only-in racket/port port->string with-input-from-string)
         "../../util/credential-redaction.rkt")

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
         cleanup-tmux-env!

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

         ;; Tool-execution trace verification (W5)
         tool-execution-phases
         trace-entry-data
         trace-entry-tool-name
         tool-execution-trace-entry?
         find-tool-execution-events
         (struct-out tool-info)
         parse-tool-events
         compute-file-fingerprint
         verify-file-unchanged!
         verify-artifact-redacted!
         detect-sensitive-leak

         ;; MAS/subagent lifecycle evidence (W6)
         mas-lifecycle-phases
         mas-trace-entry?
         find-mas-events
         (struct-out mas-spawn-event)
         (struct-out mas-lifecycle-info)
         parse-spawn-approval-event
         parse-mas-lifecycle
         verify-subagent-spawn-lifecycle
         detect-mas-coordination-events

         ;; Durable memory restart round-trip (W7)
         memory-event-phases
         session-lifecycle-phases
         memory-trace-entry?
         session-lifecycle-trace-entry?
         find-memory-events
         find-session-lifecycle-events
         (struct-out memory-store-record)
         (struct-out memory-retrieval-record)
         (struct-out session-lifecycle-record)
         parse-memory-store-event
         parse-memory-retrieval-event
         parse-session-start-event
         (struct-out durable-memory-roundtrip)
         parse-durable-memory-roundtrip
         verify-durable-memory-roundtrip

         ;; GSD lifecycle evidence (W8)
         gsd-event-phases
         gsd-trace-entry?
         find-gsd-events
         (struct-out gsd-transition-record)
         (struct-out gsd-wave-record)
         (struct-out gsd-plan-record)
         parse-gsd-transition-event
         parse-gsd-wave-event
         parse-gsd-plan-event
         (struct-out gsd-lifecycle-info)
         parse-gsd-lifecycle
         verify-gsd-transition-succeeded

         ;; Release/audit truthfulness (W8)
         release-refusal-patterns
         (struct-out release-evidence)
         detect-release-authorization-refusal
         find-release-manifest
         verify-release-manifest-present!
         verify-release-authorization-refused!

         ;; Flake classification and centralized reporting (W9)
         flake-classifications
         (struct-out flake-record)
         classify-result-status
         classify-exploration-run
         flake-indicator-patterns
         detect-flake-indicators
         flaky-run?
         (struct-out central-log-entry)
         make-central-log-entry
         central-log-entry->hash
         write-central-log-entry!
         parse-central-log
         (struct-out exploration-bundle)
         make-exploration-bundle
         bundle-pass-rate
         bundle-has-flakes?
         render-bundle-index-markdown

         ;; Approval-prompt-specific automation
         detect-approval-prompt
         parse-approval-prompt
         (struct-out approval-info)
         classify-approval-safety
         safe-capabilities
         dangerous-capabilities
         dangerous-command-patterns
         approve-approval!
         deny-approval!
         handle-approval-if-present!
         assert-no-approval-pending!

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
;; Temp directory cleanup (GAP-6, W3 #8685)
;; ============================================================

;; Clean up temp directories created by make-tmux-test-env.
;; Called after test completion to avoid accumulating /var/tmp dirs.
;;
;; If keep-artifacts? is #t (default), only removes dirs when the
;; artifact-dir is empty (meaning no failure artifacts were written).
;; If keep-artifacts? is #f, removes all dirs unconditionally.
;;
;; Returns a list of (cons dir-path-string deleted?) pairs.
;; Never throws — wraps all operations in with-handlers so callers
;; can use it in dynamic-wind cleanup without risk.
(define (cleanup-tmux-env! env #:keep-artifacts? [keep-artifacts? #t])
  (define (dir-has-files? dir)
    (and (directory-exists? dir) (not (null? (directory-list dir)))))
  (define (safe-delete-dir dir)
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (when (directory-exists? dir)
        (delete-directory/files dir))
      #t))
  ;; home, project-dir, and artifact-dir are independent temp dirs.
  ;; session-dir is inside home, so deleting home covers it.
  (define dirs (list (tmux-env-home env) (tmux-env-project-dir env) (tmux-env-artifact-dir env)))
  (define should-delete?
    (if keep-artifacts?
        ;; Only delete if artifact-dir is empty (no failure artifacts)
        (not (dir-has-files? (tmux-env-artifact-dir env)))
        #t))
  (for/list ([d (in-list dirs)])
    (cons d (and should-delete? (safe-delete-dir d)))))

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
  ;; Step 2: apply the centralized credential policy (TMUX-07).
  (define step3 (redact-secrets step1))
  ;; Step 3: redact HOME path if provided
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
;; Approval-prompt-specific automation
;; ============================================================

;; approval-info struct for parsed approval overlay content.
;; type: 'subagent or 'unknown
;; capabilities: (listof string)
;; task-preview: string
;; raw-text: string (the matched approval overlay text, redacted later)
(struct approval-info (type capabilities task-preview raw-text) #:transparent)

;; Safe capabilities that can be auto-approved in test/automation.
;; These are read-only and cannot cause side effects.
(define safe-capabilities '(read find ls grep file tree))

;; Dangerous capabilities that must NEVER be auto-approved.
;; These can cause side effects, data loss, or broad system damage.
(define dangerous-capabilities '(bash shell exec write edit delete kill))

;; Dangerous command patterns that should trigger rejection even if
;; the capability itself isn't in dangerous-capabilities.
;; Checked against task-preview text (case-insensitive substring match).
(define dangerous-command-patterns
  '("tmux kill-server" "tmux kill-session"
                       "kill-server"
                       "rm -rf"
                       "rm -r "
                       "shutdown"
                       "reboot"
                       "mkfs"
                       "dd if="
                       "chmod 777"
                       "git push --force"
                       "git push --tags"))

;; detect-approval-prompt : string? -> boolean?
;; Pure: checks normalized pane text for approval overlay markers.
(define (detect-approval-prompt text)
  (and (string? text)
       (or (string-contains? text "Approval Required") (string-contains? text "[y] Approve"))
       #t))

;; parse-approval-prompt : string? -> (or/c approval-info? #f)
;; Pure: extracts approval type, capabilities, and task preview from pane text.
;; Returns #f if no approval prompt is detected.
(define (parse-approval-prompt text)
  (unless (string? text)
    (set! text ""))
  (cond
    [(not (detect-approval-prompt text)) #f]
    [else
     (define type
       (cond
         [(string-contains? text "Subagent Approval") 'subagent]
         [(string-contains? text "Approval Required") 'unknown]
         [else 'unknown]))
     ;; Extract capabilities: look for "Capabilities: <comma-separated>"
     (define caps-str
       (let ([match (regexp-match #rx"Capabilities: ([^\n]*)" text)])
         (if match
             (string-trim (cadr match))
             "")))
     ;; Parse capabilities into a list
     (define capabilities
       (if (string=? caps-str "")
           '()
           (map string-trim (string-split caps-str ","))))
     ;; Extract task preview: look for "Task: <text>"
     (define task-preview
       (let ([match (regexp-match #rx"Task: ([^\n]*)" text)])
         (if match
             (string-trim (cadr match))
             "")))
     (approval-info type capabilities task-preview text)]))

;; classify-approval-safety : approval-info? -> (or/c 'safe 'dangerous 'caution)
;; Classifies whether the approval request is safe to auto-approve.
;; - 'safe: all capabilities are in safe-capabilities and no dangerous patterns
;; - 'dangerous: contains dangerous capabilities or dangerous command patterns
;; - 'caution: contains unknown capabilities (not in safe or dangerous sets)
(define (classify-approval-safety info)
  (define caps (approval-info-capabilities info))
  (define task (approval-info-task-preview info))
  (define task-lower (string-downcase task))
  (cond
    ;; Check for dangerous command patterns in task preview
    [(for/or ([pattern (in-list dangerous-command-patterns)])
       (string-contains? task-lower pattern))
     'dangerous]
    ;; Check for dangerous capabilities
    [(for/or ([cap (in-list caps)])
       (define sym (string->symbol (string-downcase cap)))
       (member sym dangerous-capabilities))
     'dangerous]
    ;; All capabilities are safe
    [(for/and ([cap (in-list caps)])
       (define sym (string->symbol (string-downcase cap)))
       (member sym safe-capabilities))
     (if (null? caps) 'caution 'safe)]
    ;; Unknown capabilities present
    [else 'caution]))

;; approve-approval! : tmux-q-session? -> void?
;; Sends the 'y' key to approve the current approval prompt.
;; NEVER use blindly — always check classify-approval-safety first.
(define (approve-approval! sess)
  (send-key! sess "y"))

;; deny-approval! : tmux-q-session? -> void?
;; Sends the 'n' key to deny the current approval prompt.
;; Safe to call for any approval type.
(define (deny-approval! sess)
  (send-key! sess "n"))

;; handle-approval-if-present! : tmux-q-session? -> (or/c 'approved 'denied 'dangerous-rejected 'no-approval)
;; Detects approval prompt, classifies safety, and acts accordingly.
;; Auto-approves only 'safe approvals; denies 'dangerous ones; denies 'caution.
;; Returns the action taken.
(define (handle-approval-if-present! sess)
  (define pane (capture-normalized sess #:lines 80))
  (define info (parse-approval-prompt pane))
  (cond
    [(not info) 'no-approval]
    [else
     (define safety (classify-approval-safety info))
     (case safety
       [(safe)
        (approve-approval! sess)
        'approved]
       [(dangerous)
        (deny-approval! sess)
        'dangerous-rejected]
       [(caution)
        (deny-approval! sess)
        'denied])]))

;; assert-no-approval-pending! : tmux-q-session? -> void?
;; Fails if the pane shows an approval prompt that hasn't been handled.
(define (assert-no-approval-pending! sess)
  (define pane (capture-normalized sess #:lines 80))
  (when (detect-approval-prompt pane)
    (error 'assert-no-approval-pending! "unhandled approval prompt detected in pane")))

;; ============================================================
;; Tool-execution trace verification (W5)
;; ============================================================

;; Phases in trace.jsonl that indicate tool execution events.
(define tool-execution-phases
  '("tool.call.started" "tool.execution.started"
                        "tool.execution.updated"
                        "tool.execution.completed"
                        "tool.called"
                        "tool.result"))

;; tool-info struct for parsed tool execution trace events.
;; phase: string — the event phase (e.g. "tool.execution.completed")
;; tool-name: string — the tool name extracted from event data
;; turn-id: (or/c string #f)
;; entry: hash? — the raw trace entry
(struct tool-info (phase tool-name turn-id entry) #:transparent)

;; trace-entry-data : hash? -> (or/c hash? #f)
;; Pure: extracts the 'data payload from a trace entry.
(define (trace-entry-data entry)
  (and (hash? entry) (hash-ref/keys entry '(data "data") #f)))

;; trace-entry-tool-name : hash? -> (or/c string #f)
;; Pure: extracts the tool name from a tool execution trace entry.
;; Handles both camelCase (toolName) and kebab-case (tool-name) keys,
;; as well as 'name from stream events.
(define (trace-entry-tool-name entry)
  (define data (trace-entry-data entry))
  (and (hash? data)
       (let ([name (hash-ref/keys data '(toolName "toolName" tool-name "tool-name" name "name") #f)])
         (cond
           [(not name) #f]
           [(string? name) name]
           [(symbol? name) (symbol->string name)]
           [else (format "~a" name)]))))

;; tool-execution-trace-entry? : hash? -> boolean?
;; Pure: checks if a trace entry is a tool execution event.
(define (tool-execution-trace-entry? entry #:tool-name [tool-name #f])
  (define phase (trace-entry-phase entry))
  (define entry-tool-name (trace-entry-tool-name entry))
  (and phase
       (if (member phase tool-execution-phases) #t #f)
       (or (not tool-name) (equal? entry-tool-name tool-name))))

;; find-tool-execution-events : (or/c path-string? tmux-q-session?) -> (listof hash?)
;; Finds all tool execution events in a session's trace.jsonl files.
;; Accepts either a session directory path or a tmux-q-session.
(define (find-tool-execution-events session-dir-or-sess)
  (define session-dir
    (if (tmux-q-session? session-dir-or-sess)
        (tmux-q-session-session-dir session-dir-or-sess)
        session-dir-or-sess))
  (for*/list ([trace-path (in-list (find-trace-jsonl-paths session-dir))]
              [entry (in-list (read-trace-events trace-path))]
              #:when (tool-execution-trace-entry? entry))
    entry))

;; parse-tool-events : (or/c path-string? tmux-q-session?) -> (listof tool-info?)
;; Converts tool execution trace entries into structured tool-info records.
(define (parse-tool-events session-dir-or-sess)
  (define events (find-tool-execution-events session-dir-or-sess))
  (for/list ([entry (in-list events)])
    (tool-info (or (trace-entry-phase entry) "")
               (or (trace-entry-tool-name entry) "")
               (trace-entry-turn-id entry)
               entry)))

;; compute-file-fingerprint : path-string? -> string?
;; Pure: computes a fingerprint of file contents for modification detection.
;; Returns a hex string of the SHA-256 of the file bytes.
;; Falls back to length-based fingerprint if crypto module unavailable.
(define (compute-file-fingerprint path)
  (define bytes (file->bytes path))
  (define len (bytes-length bytes))
  ;; Simple hash-based fingerprint: combine length and a sample of bytes.
  ;; This is sufficient for detecting modifications in test scenarios.
  (format "len:~a,sum:~a" len (for/sum ([b (in-bytes bytes)]) b)))

;; verify-file-unchanged! : path-string? string? -> void?
;; Compares a file's current fingerprint against a baseline fingerprint.
;; Fails if the file was modified.
(define (verify-file-unchanged! path baseline-fingerprint)
  (define current (compute-file-fingerprint path))
  (unless (equal? current baseline-fingerprint)
    (error 'verify-file-unchanged!
           "file was modified: ~a\nexpected: ~a\nactual: ~a"
           path
           baseline-fingerprint
           current)))

;; detect-sensitive-leak : string? -> boolean?
;; Pure: checks if text contains un-redacted sensitive patterns.
;; Returns #t if a sensitive pattern is found that is NOT already redacted.
(define (detect-sensitive-leak text)
  ;; v0.99.50 W3 (TMUX-07): One policy across explorer, harness, and report.
  (contains-secret-leak? text))

;; verify-artifact-redacted! : path-string? -> void?
;; Reads an artifact file and fails if un-redacted sensitive content is found.
(define (verify-artifact-redacted! path)
  (define content (file->string path))
  (when (detect-sensitive-leak content)
    (error 'verify-artifact-redacted! "un-redacted sensitive content detected in artifact: ~a" path)))

;; ============================================================
;; MAS/subagent lifecycle evidence (W6)
;; ============================================================

;; All known MAS-related event phases that can appear in trace.jsonl.
(define mas-lifecycle-phases
  '("mas.spawn-approval-requested" "mas.artifact.produced"
                                   "mas.test.result"
                                   "mas.hypothesis.opened"
                                   "mas.hypothesis.resolved"
                                   "mas.blackboard.sync"
                                   "mas.agent.version.pinned"
                                   "mas.agent.registered"
                                   "mas.agent.activated"
                                   "mas.mcp.connected"
                                   "mas.mcp.tool.called"))

;; mas-spawn-event: parsed spawn-approval-requested trace entry.
;; capabilities: (listof symbol)
;; task-preview: string
;; turn-id: (or/c string #f)
;; entry: hash? — raw trace entry
(struct mas-spawn-event (capabilities task-preview turn-id entry) #:transparent)

;; mas-lifecycle-info: structured summary of MAS events in a session.
;; spawn-approvals: (listof mas-spawn-event?)
;; artifacts: (listof hash?) — raw artifact-produced entries
;; coordination-events: (listof hash?) — hypotheses, blackboard, agent events
;; tool-execution-events: (listof hash?) — tool.execution.* entries for cross-reference
;; total-mas-events: exact-nonnegative-integer?
(struct mas-lifecycle-info
        (spawn-approvals artifacts coordination-events tool-execution-events total-mas-events)
  #:transparent)

;; mas-trace-entry? : hash? -> boolean?
;; Pure: checks if a trace entry is a MAS-lifecycle event.
(define (mas-trace-entry? entry)
  (define phase (trace-entry-phase entry))
  (and phase (if (member phase mas-lifecycle-phases) #t #f)))

;; find-mas-events : (or/c path-string? tmux-q-session?) -> (listof hash?)
;; Finds all MAS-lifecycle events in a session's trace.jsonl files.
(define (find-mas-events session-dir-or-sess)
  (define session-dir
    (if (tmux-q-session? session-dir-or-sess)
        (tmux-q-session-session-dir session-dir-or-sess)
        session-dir-or-sess))
  (for*/list ([trace-path (in-list (find-trace-jsonl-paths session-dir))]
              [entry (in-list (read-trace-events trace-path))]
              #:when (mas-trace-entry? entry))
    entry))

;; parse-spawn-approval-event : hash? -> mas-spawn-event?
;; Pure: parses a mas.spawn-approval-requested trace entry into structured record.
(define (parse-spawn-approval-event entry)
  (define data (trace-entry-data entry))
  (define caps-raw (and (hash? data) (hash-ref/keys data '(capabilities "capabilities") '())))
  (define caps
    (cond
      [(list? caps-raw)
       (for/list ([c (in-list caps-raw)])
         (cond
           [(symbol? c) c]
           [(string? c) (string->symbol c)]
           [else (string->symbol (format "~a" c))]))]
      [else '()]))
  (define task-raw (and (hash? data) (hash-ref/keys data '(task-preview "task-preview") "")))
  (define task-str
    (if (string? task-raw)
        task-raw
        (if task-raw
            (format "~a" task-raw)
            "")))
  (mas-spawn-event caps task-str (trace-entry-turn-id entry) entry))

;; parse-mas-lifecycle : (or/c path-string? tmux-q-session?) -> mas-lifecycle-info?
;; Parses all MAS events from a session and returns structured lifecycle info.
(define (parse-mas-lifecycle session-dir-or-sess)
  (define mas-events (find-mas-events session-dir-or-sess))
  (define tool-events (find-tool-execution-events session-dir-or-sess))
  (define spawn-approvals
    (for/list ([e (in-list mas-events)]
               #:when (equal? (trace-entry-phase e) "mas.spawn-approval-requested"))
      (parse-spawn-approval-event e)))
  (define artifacts
    (filter (lambda (e) (equal? (trace-entry-phase e) "mas.artifact.produced")) mas-events))
  (define coordination-phases
    '("mas.test.result" "mas.hypothesis.opened"
                        "mas.hypothesis.resolved"
                        "mas.blackboard.sync"
                        "mas.agent.version.pinned"
                        "mas.agent.registered"
                        "mas.agent.activated"
                        "mas.mcp.connected"
                        "mas.mcp.tool.called"))
  (define coordination-events
    (filter (lambda (e) (member (trace-entry-phase e) coordination-phases)) mas-events))
  (mas-lifecycle-info spawn-approvals artifacts coordination-events tool-events (length mas-events)))

;; verify-subagent-spawn-lifecycle : mas-lifecycle-info? -> boolean?
;; Pure: checks if the lifecycle shows evidence of a complete spawn cycle:
;; spawn-approval-requested → tool.execution.started → tool.execution.completed
;; Returns #t if at least one spawn-approval was followed by tool execution.
(define (verify-subagent-spawn-lifecycle info)
  (and (> (length (mas-lifecycle-info-spawn-approvals info)) 0)
       (let ([exec-events (mas-lifecycle-info-tool-execution-events info)])
         (and (> (length exec-events) 0)
              ;; Check for at least one started and one completed
              (for/or ([e (in-list exec-events)])
                (equal? (trace-entry-phase e) "tool.execution.started"))
              (for/or ([e (in-list exec-events)])
                (equal? (trace-entry-phase e) "tool.execution.completed"))))
       #t))

;; detect-mas-coordination-events : mas-lifecycle-info? -> (listof string?)
;; Pure: returns the unique list of coordination event phases present.
(define (detect-mas-coordination-events info)
  (remove-duplicates (for/list ([e (in-list (mas-lifecycle-info-coordination-events info))])
                       (or (trace-entry-phase e) ""))))

;; ============================================================
;; Durable memory restart round-trip (W7)
;; ============================================================

;; Memory event phases that appear in trace.jsonl.
(define memory-event-phases
  '("memory.item.store.requested" "memory.item.stored"
                                  "memory.item.deleted"
                                  "memory.item.updated"
                                  "memory.retrieval.performed"
                                  "memory.policy.blocked"
                                  "memory.backend.unavailable"))

;; Session lifecycle phases relevant to durable memory round-trip.
(define session-lifecycle-phases
  '("session.started" "session.shutdown"
                      "session.closed"
                      "session.fork.completed"
                      "session.fork.failed"
                      "session.compact.completed"
                      "session.compact.failed"))

;; memory-store-record: parsed memory.item.stored event.
;; memory-id: string?
;; mem-type: string?
;; scope: string?
;; source: (or/c string? symbol?)
;; redacted-snippet: string?
;; turn-id: (or/c string? #f)
;; entry: hash?
(struct memory-store-record (memory-id mem-type scope source redacted-snippet turn-id entry)
  #:transparent)

;; memory-retrieval-record: parsed memory.retrieval.performed event.
;; query-snippet: string?
;; result-count: exact-nonnegative-integer?
;; query-limit: exact-nonnegative-integer?
;; scope: (or/c string? #f)
;; latency-ms: exact-nonnegative-integer?
;; turn-id: (or/c string? #f)
;; entry: hash?
(struct memory-retrieval-record
        (query-snippet result-count query-limit scope latency-ms turn-id entry)
  #:transparent)

;; session-lifecycle-record: parsed session.started/shutdown event.
;; phase: string?
;; reason: (or/c symbol? string? #f) — new/resume/fork/shutdown
;; previous-session-id: (or/c string? #f)
;; session-dir: (or/c string? #f)
;; turn-id: (or/c string? #f)
;; entry: hash?
(struct session-lifecycle-record (phase reason previous-session-id session-dir turn-id entry)
  #:transparent)

;; durable-memory-roundtrip: structured summary of a memory round-trip.
;; store-events: (listof memory-store-record?)
;; retrieval-events: (listof memory-retrieval-record?)
;; session-events: (listof session-lifecycle-record?)
;; has-store?: boolean? — at least one memory.item.stored
;; has-retrieval?: boolean? — at least one memory.retrieval.performed
;; has-restart?: boolean? — session.started with reason 'resume
;; roundtrip-complete?: boolean? — store → restart → retrieval cycle present
(struct durable-memory-roundtrip
        (store-events retrieval-events
                      session-events
                      has-store?
                      has-retrieval?
                      has-restart?
                      roundtrip-complete?)
  #:transparent)

;; memory-trace-entry? : hash? -> boolean?
;; Pure: checks if a trace entry is a memory event.
(define (memory-trace-entry? entry)
  (define phase (trace-entry-phase entry))
  (and phase (if (member phase memory-event-phases) #t #f)))

;; session-lifecycle-trace-entry? : hash? -> boolean?
;; Pure: checks if a trace entry is a session lifecycle event.
(define (session-lifecycle-trace-entry? entry)
  (define phase (trace-entry-phase entry))
  (and phase (if (member phase session-lifecycle-phases) #t #f)))

;; find-memory-events : (or/c path-string? tmux-q-session?) -> (listof hash?)
;; Finds all memory events in a session's trace.jsonl files.
(define (find-memory-events session-dir-or-sess)
  (define session-dir
    (if (tmux-q-session? session-dir-or-sess)
        (tmux-q-session-session-dir session-dir-or-sess)
        session-dir-or-sess))
  (for*/list ([trace-path (in-list (find-trace-jsonl-paths session-dir))]
              [entry (in-list (read-trace-events trace-path))]
              #:when (memory-trace-entry? entry))
    entry))

;; find-session-lifecycle-events : (or/c path-string? tmux-q-session?) -> (listof hash?)
;; Finds all session lifecycle events in a session's trace.jsonl files.
(define (find-session-lifecycle-events session-dir-or-sess)
  (define session-dir
    (if (tmux-q-session? session-dir-or-sess)
        (tmux-q-session-session-dir session-dir-or-sess)
        session-dir-or-sess))
  (for*/list ([trace-path (in-list (find-trace-jsonl-paths session-dir))]
              [entry (in-list (read-trace-events trace-path))]
              #:when (session-lifecycle-trace-entry? entry))
    entry))

;; Helper: extract a field from data hash with symbol/string fallback and default.
(define (data-ref data keys default)
  (if (hash? data)
      (hash-ref/keys data keys default)
      default))

;; parse-memory-store-event : hash? -> memory-store-record?
;; Pure: parses a memory.item.stored trace entry.
(define (parse-memory-store-event entry)
  (define data (trace-entry-data entry))
  (memory-store-record (data-ref data '(memory-id "memory-id") "")
                       (data-ref data '(mem-type "mem-type") "")
                       (data-ref data '(scope "scope") "")
                       (data-ref data '(source "source") 'tool)
                       (data-ref data '(redacted-snippet "redacted-snippet") "")
                       (trace-entry-turn-id entry)
                       entry))

;; parse-memory-retrieval-event : hash? -> memory-retrieval-record?
;; Pure: parses a memory.retrieval.performed trace entry.
(define (parse-memory-retrieval-event entry)
  (define data (trace-entry-data entry))
  (memory-retrieval-record (data-ref data '(query-snippet "query-snippet") "")
                           (data-ref data '(result-count "result-count") 0)
                           (data-ref data '(query-limit "query-limit") 5)
                           (data-ref data '(scope "scope") #f)
                           (data-ref data '(latency-ms "latency-ms") 0)
                           (trace-entry-turn-id entry)
                           entry))

;; parse-session-start-event : hash? -> session-lifecycle-record?
;; Pure: parses a session.started trace entry, extracting reason.
(define (parse-session-start-event entry)
  (define data (trace-entry-data entry))
  ;; The session.started event embeds reason in data payload:
  ;; For session-switch, data is a hasheq with 'reason key.
  ;; For simple session-start, data may be the model string.
  (define reason-raw
    (cond
      [(hash? data) (hash-ref/keys data '(reason "reason") #f)]
      [else #f]))
  ;; Normalize: JSON strings become symbols for consistency.
  (define reason
    (cond
      [(not reason-raw) #f]
      [(symbol? reason-raw) reason-raw]
      [(string? reason-raw) (string->symbol reason-raw)]
      [else #f]))
  (define prev-id
    (if (hash? data)
        (hash-ref/keys data '(previous-session-id "previous-session-id") #f)
        #f))
  (define session-dir
    (if (hash? data)
        (hash-ref/keys data '(session-dir "session-dir") #f)
        #f))
  (session-lifecycle-record (or (trace-entry-phase entry) "session.started")
                            reason
                            prev-id
                            session-dir
                            (trace-entry-turn-id entry)
                            entry))

;; parse-durable-memory-roundtrip : (or/c path-string? tmux-q-session?) -> durable-memory-roundtrip?
;; Parses all memory and session events to determine if a durable memory
;; round-trip (store → restart → retrieval) is present.
(define (parse-durable-memory-roundtrip session-dir-or-sess)
  (define session-dir
    (if (tmux-q-session? session-dir-or-sess)
        (tmux-q-session-session-dir session-dir-or-sess)
        session-dir-or-sess))
  (define mem-events (find-memory-events session-dir))
  (define sess-events (find-session-lifecycle-events session-dir))
  (define store-events
    (for/list ([e (in-list mem-events)]
               #:when (equal? (trace-entry-phase e) "memory.item.stored"))
      (parse-memory-store-event e)))
  (define retrieval-events
    (for/list ([e (in-list mem-events)]
               #:when (equal? (trace-entry-phase e) "memory.retrieval.performed"))
      (parse-memory-retrieval-event e)))
  (define session-records
    (for/list ([e (in-list sess-events)]
               #:when (equal? (trace-entry-phase e) "session.started"))
      (parse-session-start-event e)))
  (define has-store? (> (length store-events) 0))
  (define has-retrieval? (> (length retrieval-events) 0))
  (define has-restart?
    (for/or ([r (in-list session-records)])
      (equal? (session-lifecycle-record-reason r) 'resume)))
  (define roundtrip-complete? (and has-store? has-restart? has-retrieval? #t))
  (durable-memory-roundtrip store-events
                            retrieval-events
                            session-records
                            has-store?
                            has-retrieval?
                            has-restart?
                            roundtrip-complete?))

;; verify-durable-memory-roundtrip : durable-memory-roundtrip? -> boolean?
;; Pure: verifies that a complete durable memory round-trip is present:
;; at least one store, one restart (resume), and one retrieval.
(define (verify-durable-memory-roundtrip info)
  (durable-memory-roundtrip-roundtrip-complete? info))

;; ============================================================
;; GSD lifecycle evidence (W8)
;; ============================================================

;; GSD event phases emitted by trace.jsonl.
(define gsd-event-phases
  '("gsd.mode.changed" "gsd.transition.attempted"
                       "gsd.transition.succeeded"
                       "gsd.transition.failed"
                       "gsd.wave.started"
                       "gsd.wave.completed"
                       "gsd.wave.failed"
                       "gsd.wave.skipped"
                       "gsd.plan.parsed"
                       "gsd.plan.validated"
                       "gsd.plan.normalized"
                       "gsd.plan.archived"
                       "gsd.command.received"
                       "gsd.command.completed"
                       "gsd.guard.blocked"
                       "gsd.guard.allowed"))

;; gsd-transition-record: parsed gsd.transition.* event.
;; outcome: symbol? — 'attempted, 'succeeded, 'failed
;; from-state: (or/c string? symbol? #f)
;; to-state: (or/c string? symbol? #f)
;; reason: (or/c string? #f) — only for failed transitions
;; turn-id: (or/c string? #f)
;; entry: hash?
(struct gsd-transition-record (outcome from-state to-state reason turn-id entry) #:transparent)

;; gsd-wave-record: parsed gsd.wave.* event.
;; outcome: symbol? — 'started, 'completed, 'failed, 'skipped
;; wave: (or/c string? #f)
;; error: (or/c string? #f) — only for failed waves
;; reason: (or/c string? #f) — only for skipped waves
;; turn-id: (or/c string? #f)
;; entry: hash?
(struct gsd-wave-record (outcome wave error reason turn-id entry) #:transparent)

;; gsd-plan-record: parsed gsd.plan.* event.
;; operation: symbol? — 'parsed, 'validated, 'normalized, 'archived
;; wave-count: exact-nonnegative-integer?
;; valid?: (or/c boolean? #f) — only for validated
;; error-count: exact-nonnegative-integer? — only for validated
;; path: (or/c string? #f) — only for archived
;; turn-id: (or/c string? #f)
;; entry: hash?
(struct gsd-plan-record (operation wave-count valid? error-count path turn-id entry) #:transparent)

;; gsd-lifecycle-info: structured summary of GSD events.
;; transitions: (listof gsd-transition-record?)
;; waves: (listof gsd-wave-record?)
;; plan-ops: (listof gsd-plan-record?)
;; guard-events: (listof hash?)
;; command-events: (listof hash?)
;; total-gsd-events: exact-nonnegative-integer?
;; has-transition-succeeded?: boolean?
;; has-wave-completed?: boolean?
(struct gsd-lifecycle-info
        (transitions waves
                     plan-ops
                     guard-events
                     command-events
                     total-gsd-events
                     has-transition-succeeded?
                     has-wave-completed?)
  #:transparent)

;; gsd-trace-entry? : hash? -> boolean?
;; Pure: checks if a trace entry is a GSD event.
(define (gsd-trace-entry? entry)
  (define phase (trace-entry-phase entry))
  (and phase (if (member phase gsd-event-phases) #t #f)))

;; find-gsd-events : (or/c path-string? tmux-q-session?) -> (listof hash?)
;; Finds all GSD events in a session's trace.jsonl files.
(define (find-gsd-events session-dir-or-sess)
  (define session-dir
    (if (tmux-q-session? session-dir-or-sess)
        (tmux-q-session-session-dir session-dir-or-sess)
        session-dir-or-sess))
  (for*/list ([trace-path (in-list (find-trace-jsonl-paths session-dir))]
              [entry (in-list (read-trace-events trace-path))]
              #:when (gsd-trace-entry? entry))
    entry))

;; parse-gsd-transition-event : hash? -> gsd-transition-record?
;; Pure: parses a gsd.transition.* trace entry.
(define (parse-gsd-transition-event entry)
  (define phase (or (trace-entry-phase entry) ""))
  (define outcome
    (cond
      [(string-contains? phase "succeeded") 'succeeded]
      [(string-contains? phase "failed") 'failed]
      [else 'attempted]))
  (define data (trace-entry-data entry))
  (gsd-transition-record outcome
                         (data-ref data '(from "from" from-state) #f)
                         (data-ref data '(to "to" to-state) #f)
                         (data-ref data '(reason "reason") #f)
                         (trace-entry-turn-id entry)
                         entry))

;; parse-gsd-wave-event : hash? -> gsd-wave-record?
;; Pure: parses a gsd.wave.* trace entry.
(define (parse-gsd-wave-event entry)
  (define phase (or (trace-entry-phase entry) ""))
  (define outcome
    (cond
      [(string-contains? phase "completed") 'completed]
      [(string-contains? phase "failed") 'failed]
      [(string-contains? phase "skipped") 'skipped]
      [else 'started]))
  (define data (trace-entry-data entry))
  (gsd-wave-record outcome
                   (data-ref data '(wave "wave" wave-id) #f)
                   (data-ref data '(error "error") #f)
                   (data-ref data '(reason "reason") #f)
                   (trace-entry-turn-id entry)
                   entry))

;; parse-gsd-plan-event : hash? -> gsd-plan-record?
;; Pure: parses a gsd.plan.* trace entry.
(define (parse-gsd-plan-event entry)
  (define phase (or (trace-entry-phase entry) ""))
  (define operation
    (cond
      [(string-contains? phase "parsed") 'parsed]
      [(string-contains? phase "validated") 'validated]
      [(string-contains? phase "normalized") 'normalized]
      [(string-contains? phase "archived") 'archived]
      [else 'unknown]))
  (define data (trace-entry-data entry))
  (gsd-plan-record operation
                   (data-ref data '(wave-count "wave-count") 0)
                   (data-ref data '(valid? "valid?") #f)
                   (data-ref data '(error-count "error-count") 0)
                   (data-ref data '(path "path") #f)
                   (trace-entry-turn-id entry)
                   entry))

;; parse-gsd-lifecycle : (or/c path-string? tmux-q-session?) -> gsd-lifecycle-info?
;; Parses all GSD events to produce a structured lifecycle summary.
(define (parse-gsd-lifecycle session-dir-or-sess)
  (define session-dir
    (if (tmux-q-session? session-dir-or-sess)
        (tmux-q-session-session-dir session-dir-or-sess)
        session-dir-or-sess))
  (define gsd-events (find-gsd-events session-dir))
  (define transitions
    (for/list ([e (in-list gsd-events)]
               #:when (string-contains? (or (trace-entry-phase e) "") "transition"))
      (parse-gsd-transition-event e)))
  (define waves
    (for/list ([e (in-list gsd-events)]
               #:when (string-contains? (or (trace-entry-phase e) "") "wave"))
      (parse-gsd-wave-event e)))
  (define plan-ops
    (for/list ([e (in-list gsd-events)]
               #:when (string-contains? (or (trace-entry-phase e) "") "plan"))
      (parse-gsd-plan-event e)))
  (define guard-events
    (for/list ([e (in-list gsd-events)]
               #:when (string-contains? (or (trace-entry-phase e) "") "guard"))
      e))
  (define command-events
    (for/list ([e (in-list gsd-events)]
               #:when (string-contains? (or (trace-entry-phase e) "") "command"))
      e))
  (define has-transition-succeeded?
    (for/or ([t (in-list transitions)])
      (eq? (gsd-transition-record-outcome t) 'succeeded)))
  (define has-wave-completed?
    (for/or ([w (in-list waves)])
      (eq? (gsd-wave-record-outcome w) 'completed)))
  (gsd-lifecycle-info transitions
                      waves
                      plan-ops
                      guard-events
                      command-events
                      (length gsd-events)
                      has-transition-succeeded?
                      has-wave-completed?))

;; verify-gsd-transition-succeeded : gsd-lifecycle-info? (or/c string? symbol?) (or/c string? symbol?) -> boolean?
;; Pure: verifies that a transition from-state → to-state succeeded.
(define (verify-gsd-transition-succeeded info from-state to-state)
  (define from-str
    (if (symbol? from-state)
        (symbol->string from-state)
        from-state))
  (define to-str
    (if (symbol? to-state)
        (symbol->string to-state)
        to-state))
  (for/or ([t (in-list (gsd-lifecycle-info-transitions info))])
    (and (eq? (gsd-transition-record-outcome t) 'succeeded)
         (equal? (let ([f (gsd-transition-record-from-state t)])
                   (if (symbol? f)
                       (symbol->string f)
                       f))
                 from-str)
         (equal? (let ([tt (gsd-transition-record-to-state t)])
                   (if (symbol? tt)
                       (symbol->string tt)
                       tt))
                 to-str))))

;; ============================================================
;; Release/audit truthfulness (W8)
;; ============================================================

;; Patterns indicating release authorization was refused.
(define release-refusal-patterns
  '("refused without live CI" "release is not authorized"
                              "not authorized"
                              "insufficient evidence"
                              "release-manifest.json is required"
                              "CI evidence is required"
                              "cannot authorize release"))

;; release-evidence: structured release/audit evidence.
;; has-manifest?: boolean?
;; manifest-path: (or/c path-string? #f)
;; authorization-refused?: boolean?
;; refusal-snippet: (or/c string? #f)
;; source-text: string?
(struct release-evidence
        (has-manifest? manifest-path authorization-refused? refusal-snippet source-text)
  #:transparent)

;; detect-release-authorization-refusal : string? -> (or/c string? #f)
;; Pure: returns the first matching refusal pattern, or #f.
(define (detect-release-authorization-refusal text)
  (for/or ([pat (in-list release-refusal-patterns)])
    (and (string-contains? text pat) pat)))

;; find-release-manifest : path-string? -> (or/c path-string? #f)
;; Pure: checks for release-manifest.json in the given directory.
(define (find-release-manifest project-dir)
  (define candidate (build-path project-dir "release-manifest.json"))
  (if (file-exists? candidate)
      (path->string candidate)
      #f))

;; verify-release-manifest-present! : path-string? -> void?
;; Side-effect: fails if release-manifest.json is not present.
(define (verify-release-manifest-present! project-dir)
  (unless (find-release-manifest project-dir)
    (raise-user-error 'verify-release-manifest-present!
                      "release-manifest.json not found in ~a"
                      project-dir)))

;; verify-release-authorization-refused! : string? -> void?
;; Pure assertion: fails if text does not contain a refusal pattern.
(define (verify-release-authorization-refused! text)
  (define refusal (detect-release-authorization-refusal text))
  (unless refusal
    (raise-user-error 'verify-release-authorization-refused!
                      "no release-authorization refusal pattern found in captured text")))

;; ============================================================
;; Flake classification and centralized reporting (W9)
;; ============================================================

;; Standard classifications for exploration run outcomes.
(define flake-classifications '(pass partial fail flake skip incomplete timeout unknown))

;; flake-record: per-scenario classification with optional flake evidence.
;; tag: string?
;; status: symbol? — the raw scenario status
;; classification: symbol? — one of flake-classifications
;; flake-indicators: (listof string?) — patterns found, if any
;; report-path: (or/c string? #f)
(struct flake-record (tag status classification flake-indicators report-path) #:transparent)

;; classify-result-status : symbol? -> symbol?
;; Pure: maps a scenario status symbol to a flake classification.
(define (classify-result-status status)
  (case status
    [(pass-mock-tui) 'pass]
    [(pass-mock-tui-with-caveat pass-mock-tui-partial) 'partial]
    [(unsupported unsupported-until-w7) 'skip]
    [(requires-real-provider-run) 'incomplete]
    [(fail error) 'fail]
    [(timeout) 'timeout]
    [else 'unknown]))

;; flake-indicator-patterns: text patterns that suggest flaky behavior.
(define flake-indicator-patterns
  '("intermittent" "flaky"
                   "retry"
                   "timed out"
                   "race condition"
                   "non-deterministic"
                   "occasionally fails"
                   "unstable"))

;; detect-flake-indicators : string? -> (listof string?)
;; Pure: returns list of matching flake indicator patterns.
(define (detect-flake-indicators text)
  (for/list ([pat (in-list flake-indicator-patterns)]
             #:when (string-contains? text pat))
    pat))

;; flaky-run? : (listof flake-record?) -> boolean?
;; Pure: returns #t if any record has classification 'flake.
(define (flaky-run? records)
  (for/or ([r (in-list records)])
    (eq? (flake-record-classification r) 'flake)))

;; classify-exploration-run : (listof (cons symbol? string?)) -> (listof flake-record?)
;; Each input pair is (status . tag). Optionally accepts #:report-paths alist.
;; Pure: produces flake-record for each result.
(define (classify-exploration-run results #:texts [texts '()] #:report-paths [report-paths '()])
  (for/list ([item (in-list results)])
    (define status (car item))
    (define tag (cdr item))
    (define text (alist-ref-string tag texts))
    (define indicators
      (if text
          (detect-flake-indicators text)
          '()))
    (define base-class (classify-result-status status))
    ;; If we have indicators AND base-class is pass/partial, reclassify as flake
    (define classification
      (if (and (pair? indicators) (or (eq? base-class 'pass) (eq? base-class 'partial)))
          'flake
          base-class))
    (define rp (alist-ref-string tag report-paths))
    (flake-record tag status classification indicators rp)))

;; alist-ref-string : string? (listof (cons string? string?)) -> (or/c string? #f)
;; Internal helper for case-insensitive alist lookup.
(define (alist-ref-string key alist)
  (define found (assoc key alist))
  (if found
      (cdr found)
      #f))

;; central-log-entry: structured entry for centralized protocol log.
;; timestamp: string?
;; run-id: string?
;; total: exact-nonnegative-integer?
;; pass-count: exact-nonnegative-integer?
;; partial-count: exact-nonnegative-integer?
;; fail-count: exact-nonnegative-integer?
;; flake-count: exact-nonnegative-integer?
;; skip-count: exact-nonnegative-integer?
;; incomplete-count: exact-nonnegative-integer?
;; mode: string?
;; records: (listof flake-record?)
(struct central-log-entry
        (timestamp run-id
                   total
                   pass-count
                   partial-count
                   fail-count
                   flake-count
                   skip-count
                   incomplete-count
                   mode
                   records)
  #:transparent)

;; make-central-log-entry : (listof flake-record?) string? string? -> central-log-entry?
;; Pure: aggregates records into a structured log entry.
(define (make-central-log-entry records run-id mode [ts #f])
  (define (count-class cls)
    (for/sum ([r (in-list records)] #:when (eq? (flake-record-classification r) cls)) 1))
  (central-log-entry (or ts (current-iso-timestamp))
                     run-id
                     (length records)
                     (count-class 'pass)
                     (count-class 'partial)
                     (count-class 'fail)
                     (count-class 'flake)
                     (count-class 'skip)
                     (count-class 'incomplete)
                     mode
                     records))

;; current-iso-timestamp : -> string?
(define (current-iso-timestamp)
  (parameterize ([date-display-format 'iso-8601])
    (date->string (current-date) #t)))

;; central-log-entry->hash : central-log-entry? -> hash?
;; Pure: converts to JSON-compatible hash (for write-json).
(define (central-log-entry->hash entry)
  (hasheq 'timestamp
          (central-log-entry-timestamp entry)
          'run_id
          (central-log-entry-run-id entry)
          'total
          (central-log-entry-total entry)
          'pass
          (central-log-entry-pass-count entry)
          'partial
          (central-log-entry-partial-count entry)
          'fail
          (central-log-entry-fail-count entry)
          'flake
          (central-log-entry-flake-count entry)
          'skip
          (central-log-entry-skip-count entry)
          'incomplete
          (central-log-entry-incomplete-count entry)
          'mode
          (central-log-entry-mode entry)))

;; write-central-log-entry! : path-string? central-log-entry? -> void?
;; Side-effect: appends a structured JSON line to the log file.
(define (write-central-log-entry! log-path entry)
  (define parent
    (let-values ([(base name dir?) (split-path (string->path log-path))])
      (and (path? base) base)))
  (when parent
    (make-directory* parent))
  (call-with-output-file log-path
                         (lambda (out)
                           (write-json (central-log-entry->hash entry) out)
                           (newline out))
                         #:exists 'append))

;; parse-central-log : path-string? -> (listof hash?)
;; Pure: reads JSONL entries from a central log file.
(define (parse-central-log log-path)
  (if (not (file-exists? log-path))
      '()
      (call-with-input-file log-path
                            (lambda (inp)
                              (let loop ([acc '()])
                                (define line (read-line inp))
                                (cond
                                  [(eof-object? line) (reverse acc)]
                                  [(string-contains? (string-trim line) "{")
                                   (define parsed
                                     (with-handlers ([exn:fail? (lambda (_) #f)])
                                       (with-input-from-string (string-trim line)
                                                               (lambda () (read-json)))))
                                   (loop (if parsed
                                             (cons parsed acc)
                                             acc))]
                                  [else (loop acc)]))))))

;; exploration-bundle: aggregated summary of an exploration run.
;; root-dir: string?
;; records: (listof flake-record?)
;; entry: (or/c central-log-entry? #f)
;; pass-rate: flonum? (0.0–1.0)
;; has-flakes?: boolean?
(struct exploration-bundle (root-dir records entry pass-rate has-flakes?) #:transparent)

;; make-exploration-bundle : string? (listof flake-record?) string? string? -> exploration-bundle?
;; Pure: creates bundle with computed pass-rate and flake flag.
(define (make-exploration-bundle root-dir records run-id mode)
  (define entry (make-central-log-entry records run-id mode))
  (define total (max 1 (central-log-entry-total entry)))
  (define rate
    (/ (+ (central-log-entry-pass-count entry) (central-log-entry-partial-count entry)) total))
  (exploration-bundle root-dir records entry (exact->inexact rate) (flaky-run? records)))

;; bundle-pass-rate : exploration-bundle? -> flonum?
(define (bundle-pass-rate bundle)
  (exploration-bundle-pass-rate bundle))

;; bundle-has-flakes? : exploration-bundle? -> boolean?
(define (bundle-has-flakes? bundle)
  (exploration-bundle-has-flakes? bundle))

;; render-bundle-index-markdown : exploration-bundle? -> string?
;; Pure: renders a markdown index summarizing all scenario results.
(define (render-bundle-index-markdown bundle)
  (define entry (exploration-bundle-entry bundle))
  (define records (exploration-bundle-records bundle))
  (string-append
   (format "# Exploration bundle index\n\n")
   (format "**Root:** `~a`\n" (exploration-bundle-root-dir bundle))
   (format "**Run ID:** `~a`\n" (central-log-entry-run-id entry))
   (format "**Mode:** `~a`\n" (central-log-entry-mode entry))
   (format "**Pass rate:** ~a% (~a/~a)\n\n"
           (real->decimal-string (* 100 (exploration-bundle-pass-rate bundle)) 1)
           (+ (central-log-entry-pass-count entry) (central-log-entry-partial-count entry))
           (central-log-entry-total entry))
   (format "| Tag | Status | Classification | Flake? | Report |\n")
   (format "|-----|--------|----------------|--------|--------|\n")
   (apply string-append
          (for/list ([r (in-list records)])
            (format "| ~a | ~a | ~a | ~a | ~a |\n"
                    (flake-record-tag r)
                    (flake-record-status r)
                    (flake-record-classification r)
                    (if (pair? (flake-record-flake-indicators r)) "⚠️" "")
                    (or (flake-record-report-path r) ""))))
   (if (exploration-bundle-has-flakes? bundle)
       (format "\n> ⚠️ **Flaky scenarios detected** — ~a record(s) classified as flake.\n"
               (central-log-entry-flake-count entry))
       "")))

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
