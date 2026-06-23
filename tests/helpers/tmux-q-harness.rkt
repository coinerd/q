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

(require racket/file
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

(define (redact-sensitive text)
  (define lines (string-split text "\n" #:trim? #f))
  (define redacted
    (for/list ([line (in-list lines)])
      (cond
        [(regexp-match? #px"^(HOME|.*(KEY|TOKEN|SECRET|PASSWORD|CREDENTIAL).*)=" line)
         (define key-parts (regexp-match #px"^([^=]+)=" line))
         (if key-parts
             (format "~a=<REDACTED>" (cadr key-parts))
             line)]
        [else line])))
  (string-join redacted "\n"))

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
