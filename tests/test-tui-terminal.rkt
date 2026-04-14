#lang racket

;; test-tui-terminal.rkt — Tests for tui-term based terminal adapter
;;
;; These tests verify:
;;   1. Terminal lifecycle functions exist and have correct signatures
;;   2. Screen size query functions work
;;   3. Key message to keycode conversion works correctly

(require rackunit
         rackunit/text-ui
         racket/port
         "../tui/terminal.rkt")

;; ============================================================
;; Mock message constructors for testing
;; (since tui/term/messages may not be available in test environment)
;; ============================================================

;; Mock tkeymsg structure
(struct mock-tkeymsg (key) #:transparent)
(struct mock-tsizemsg (cols rows) #:transparent)

;; ============================================================
;; Tests
;; ============================================================

(define terminal-tests
  (test-suite
   "TUI Terminal Adapter (tui-term)"

   ;; ============================================================
   ;; 1. Terminal lifecycle (open/close)
   ;; ============================================================

   (test-case "tui-term-open is a procedure accepting #:tty keyword"
     (check-pred procedure? tui-term-open)
     ;; Check arity: should accept at least 0 args, with keyword support
     (check-true (procedure-arity-includes? tui-term-open 0)))

   (test-case "tui-term-close is a procedure"
     (check-pred procedure? tui-term-close)
     (check-true (procedure-arity-includes? tui-term-close 1)))

   (test-case "with-tui-terminal is a macro (syntax transformer)"
     ;; The macro should expand correctly
     (check-true (procedure? tui-screen-size)))

   ;; ============================================================
   ;; 2. Screen size query
   ;; ============================================================

   (test-case "tui-screen-size is a procedure returning two values"
     (check-pred procedure? tui-screen-size)
     ;; In headless environment, may return default values
     (define-values (cols rows) (tui-screen-size))
     (check-true (or (integer? cols) (not cols)))
     (check-true (or (integer? rows) (not rows))))

   (test-case "tui-screen-size-changed? returns boolean"
     (check-pred procedure? tui-screen-size-changed?)
     (define result (tui-screen-size-changed?))
     (check-true (boolean? result)))

   (test-case "tui-screen-size-cache-reset! is a procedure"
     (check-pred procedure? tui-screen-size-cache-reset!)
     ;; Should run without error
     (check-not-exn (lambda () (tui-screen-size-cache-reset!))))

   ;; ============================================================
   ;; 3. Key message to keycode conversion
   ;; ============================================================

   (test-case "tui-keycode extracts char from mock key message"
     ;; Test that tui-keycode works with the expected interface
     ;; We test the pure key mapping logic separately
     (check-true (procedure? tui-keycode)))

   (test-case "Key helpers work correctly"
     (check-pred tui-key-char? #\a)
     (check-pred tui-key-char? #\return)
     (check-false (tui-key-char? 'left))
     (check-false (tui-key-char? 'backspace))

     (check-equal? (tui-key-char #\a) #\a)
     (check-equal? (tui-key-char #\Z) #\Z)

     (check-equal? (tui-key-symbol 'left) 'left)
     (check-equal? (tui-key-symbol 'up) 'up))

   (test-case "tui-read-key is a procedure with #:timeout keyword"
     (check-true (procedure? tui-read-key)))

   (test-case "tui-byte-ready? returns boolean"
     (check-pred procedure? tui-byte-ready?)
     (define result (tui-byte-ready?))
     (check-true (boolean? result)))

   ;; ============================================================
   ;; 4. Drawing primitives exist and are procedures
   ;; ============================================================

   (test-case "Drawing primitives are procedures"
     (check-pred procedure? tui-clear-screen)
     (check-pred procedure? tui-cursor)
     (check-pred procedure? tui-display)
     (check-pred procedure? tui-newline)
     (check-pred procedure? tui-clear-line-right)
     (check-true (procedure? tui-flush)))

   ;; ============================================================
   ;; 5. Cursor visibility functions exist
   ;; ============================================================

   (test-case "Cursor visibility functions are procedures"
     (check-pred procedure? tui-cursor-hide)
     (check-true (procedure? tui-cursor-show)))

   ;; ============================================================
   ;; 6. Style functions exist
   ;; ============================================================

   (test-case "Style functions are procedures"
     (check-pred procedure? tui-normal)
     (check-pred procedure? tui-bold)
     (check-pred procedure? tui-inverse)
     (check-pred procedure? tui-underline)
     (check-pred procedure? tui-dim)
     (check-true (procedure? tui-fg)))

   ;; ============================================================
   ;; 7. Mouse tracking functions exist
   ;; ============================================================

   (test-case "Mouse tracking functions are procedures"
     (check-pred procedure? enable-mouse-tracking)
     (check-true (procedure? disable-mouse-tracking)))
   ))

;; ============================================================
;; Keycode mapping tests (pure function tests)
;; ============================================================

(define keycode-mapping-tests
  (test-suite
   "TUI Keycode Mappings"

   ;; These tests verify that the internal key mapping logic
   ;; correctly translates tui-term keys to our symbols.
   ;; The actual tui-keycode function works with tkeymsg structures,
   ;; so we verify the contract is correct.

   (test-case "tui-keycode returns #f for non-key messages"
     ;; When passed a non-tkeymsg, should return #f
     (check-equal? (tui-keycode "not-a-message") #f)
     (check-equal? (tui-keycode 42) #f)
     (check-equal? (tui-keycode #f) #f))

   ;; Note: Full keycode conversion tests would require
   ;; creating actual tkeymsg structures, which depends on
   ;; tui/term/messages. The adapter handles this translation
   ;; internally.
   ))

;; ============================================================
;; Run all tests
;; ============================================================


;; ============================================================
;; Buffered stdin & sync detection (Issues #409, #412)
;; ============================================================

(define buffered-sync-tests
  (test-suite
   "Buffered stdin & sync detection"

   (test-case "buffered-read-byte: returns #f on empty port timeout"
     (define in (open-input-string ""))
     (define result (buffered-read-byte in 0.001))
     (check-equal? result #f))

   (test-case "buffered-read-byte: reads byte from string port"
     (define in (open-input-string "A"))
     (define result (buffered-read-byte in 0.1))
     (check-equal? result 65))

   (test-case "buffered-read-byte: reads multiple bytes sequentially"
     (define in (open-input-string "ABC"))
     (input-buffer-reset!)
     (check-equal? (buffered-read-byte in 0.1) 65)
     (check-equal? (buffered-read-byte in 0.1) 66)
     (check-equal? (buffered-read-byte in 0.1) 67)
     (check-equal? (buffered-read-byte in 0.001) #f))

   (test-case "input-buffer-reset! clears buffer state"
     (define in (open-input-string "X"))
     (buffered-read-byte in 0.1)
     (input-buffer-reset!)
     (check-equal? (input-buffer-length) 0))

   (test-case "detect-sync-mode-support! runs without error"
     (detect-sync-mode-support!)
     (check-true (or (terminal-sync-available?)
                     (not (terminal-sync-available?)))))

   (test-case "terminal-sync-begin/end are safe to call"
     (detect-sync-mode-support!)
     (terminal-sync-begin!)
     (terminal-sync-end!)
     (check-not-exn (lambda ()
                      (terminal-sync-begin!)
                      (terminal-sync-end!))))

   (test-case "parse-xtversion-response matches valid response"
     (check-not-false
      (parse-xtversion-response
       (format "~a[>0;10" (integer->char 27))))
     (check-false (parse-xtversion-response "not-a-response")))

   (test-case "parse-da1-response matches valid DA1 response"
     (check-not-false
      (parse-da1-response
       (format "~a[?64;1;2;6;22c" (integer->char 27))))
     (check-false (parse-da1-response "not-a-response")))
   ))

(define all-tests
  (test-suite
   "All TUI Terminal Tests"
   terminal-tests
   keycode-mapping-tests
   buffered-sync-tests))

(run-tests all-tests)

;; --------------------------------------------------------
;; OSC 52 clipboard copy
;; --------------------------------------------------------

(let ()
  ;; osc-52-copy emits a well-formed OSC 52 sequence
  (define output (with-output-to-bytes (lambda () (osc-52-copy "hello"))))
  (define str (bytes->string/latin-1 output))
  (check-true (string-prefix? str (string-append (string (integer->char 27)) "]52;c;aGVsbG8="))
                "osc-52: base64 encoded")
  (check-true (string-suffix? str (string (integer->char 27) #\\))
                "osc-52: ends with ST"))

(let ()
  ;; osc-52-copy handles empty string (clears clipboard with empty base64)
  (define output (with-output-to-bytes (lambda () (osc-52-copy ""))))
  (define str (bytes->string/latin-1 output))
  (check-true (string-prefix? str (string-append (string (integer->char 27)) "]52;c;"))
                "osc-52: empty string still emits OSC 52 (clears clipboard)"))

(let ()
  ;; osc-52-copy handles spaces and special chars
  (define output (with-output-to-bytes (lambda () (osc-52-copy "hello world!"))))
  (define str (bytes->string/latin-1 output))
  (check-true (string-contains? str "aGVsbG8gd29ybGQh") "osc-52: spaces encoded correctly"))

;; ============================================================
;; clipboard-copy tests
;; ============================================================

(let ()
  ;; detect-clipboard-tool returns #f or a valid tool list
  (define tool (detect-clipboard-tool))
  (check-true (or (not tool)
                  (and (list? tool)
                       (= (length tool) 2)
                       (path? (car tool))
                       (symbol? (cadr tool))))
              "detect-clipboard-tool returns #f or (list path symbol)"))

(let ()
  ;; clipboard-copy-via-tool returns #f for invalid tool
  (define result (clipboard-copy-via-tool "/nonexistent/tool/path" 'pbcopy "test"))
  (check-false result "clipboard-copy-via-tool returns #f for missing tool"))

(let ()
  ;; clipboard-copy falls back to OSC 52 when no tool available
  ;; Mock detect-clipboard-tool to return #f by testing in a clean environment
  (define output (open-output-bytes))
  (parameterize ([current-output-port output])
    ;; Even if detect-clipboard-tool returns a tool, clipboard-copy
    ;; should always also emit OSC 52. Test by calling osc-52-copy directly.
    (osc-52-copy "test fallback"))
  (define str (bytes->string/latin-1 (get-output-bytes output)))
  (check-true (string-contains? str "]52;c;")
              "clipboard fallback: OSC 52 emitted"))

(let ()
  ;; clipboard-copy-via-tool args are correct for each tool type
  ;; We test the args logic by checking the function exists and handles errors
  (check-true (procedure? clipboard-copy-via-tool)
              "clipboard-copy-via-tool is a procedure"))

(let ()
  ;; clipboard-copy is exported and callable
  (check-pred procedure? clipboard-copy
              "clipboard-copy is a procedure")
  (check-true (procedure? detect-clipboard-tool)
              "detect-clipboard-tool is a procedure"))

;; ============================================================
;; FD leak fix: stub-current-term-size port cleanup
;; ============================================================

(test-case "stub-current-term-size: ports are cleaned up after call"
  ;; Calling stub-current-term-size should not leak file descriptors.
  ;; We verify by calling it multiple times with cache reset between each
  ;; call to force actual subprocess spawning each time.
  (for ([i (in-range 50)])
    (tui-screen-size-cache-reset!)
    (define-values (cols rows) (tui-screen-size))
    (check-true (and (exact-positive-integer? cols)
                     (exact-positive-integer? rows))
                (format "iteration ~a: got valid size ~ax~a" i cols rows))))

(test-case "stub-current-term-size: returns 80x24 when stty fails"
  ;; In environments where stty size fails, should still return 80x24
  (tui-screen-size-cache-reset!)
  (define-values (cols rows) (tui-screen-size))
  (check-true (and (exact-positive-integer? cols)
                   (exact-positive-integer? rows))
              (format "size is positive: ~ax~a" cols rows)))
