#lang racket

;; tests/test-vdom-parity.rkt — VDOM render path parity tests
;;
;; BOUNDARY: io
;;
;; Verifies that the vdom render path (render-frame-vdom!) produces
;; identical cell-buffer content as the legacy path (renderer:render-frame!).
;; Both paths use the same section renderers; only the write method differs.

(require rackunit
         rackunit/text-ui
         "../tui/cell-buffer.rkt"
         "../tui/renderer.rkt"
         "../tui/render.rkt"
         "../tui/layout.rkt"
         "../tui/state.rkt"
         "../tui/input.rkt"
         (prefix-in loop: "../tui/tui-render-loop.rkt"))

;; ============================================================
;; Helpers
;; ============================================================

;; Adapter: make cell-buffer-clear! match the current-ubuf-clear signature
(define (cb-clear buf)
  (cell-buffer-clear! buf))

;; Adapter: make cell-buffer-putstring! match the current-ubuf-putstring signature
(define (cb-putstring buf
                      col
                      row
                      str
                      #:fg [fg 7]
                      #:bg [bg 0]
                      #:bold [bold #f]
                      #:underline [underline #f]
                      #:italic [italic #f]
                      #:blink [blink #f])
  (cell-buffer-putstring! buf
                          col
                          row
                          str
                          #:fg fg
                          #:bg bg
                          #:bold bold
                          #:underline underline
                          #:italic italic
                          #:blink blink))

;; Compare two cell-buffers cell-by-cell.
;; Returns #f if identical, or a string describing the first difference.
(define (buffers-diff a b)
  (define cols-a (cell-buffer-cols a))
  (define rows-a (cell-buffer-rows a))
  (define cols-b (cell-buffer-cols b))
  (define rows-b (cell-buffer-rows b))
  (cond
    [(not (= cols-a cols-b)) (format "col count differs: ~a vs ~a" cols-a cols-b)]
    [(not (= rows-a rows-b)) (format "row count differs: ~a vs ~a" rows-a rows-b)]
    [else
     (for*/first ([r (in-range rows-a)]
                  [c (in-range cols-a)]
                  #:when (not (cell-equal? (cell-buffer-ref a c r) (cell-buffer-ref b c r))))
       (define ca (cell-buffer-ref a c r))
       (define cb (cell-buffer-ref b c r))
       (format "diff at (~a,~a): char=~s vs ~s fg=~a vs ~a bg=~a vs ~a bold=~a vs ~a"
               c
               r
               (cell-char ca)
               (cell-char cb)
               (cell-fg ca)
               (cell-fg cb)
               (cell-bg ca)
               (cell-bg cb)
               (cell-bold? ca)
               (cell-bold? cb)))]))

;; Create a transcript entry
(define (make-entry kind text)
  (transcript-entry kind text 0 (hash) #f))

;; Create an input state with text
(define (make-test-input-state #:text [text ""])
  (struct-copy input-state (initial-input-state) [buffer text] [cursor (string-length text)]))

;; ============================================================
;; Test suite
;; ============================================================

(define parity-tests
  (test-suite "VDOM Render Path Parity"

    ;; ----------------------------------------------------------
    ;; Empty frame — just header, status bar, input line
    ;; ----------------------------------------------------------
    (test-case "empty frame parity"
      (define cols 80)
      (define rows 24)
      (define buf-legacy (make-cell-buffer cols rows))
      (define buf-vdom (make-cell-buffer cols rows))
      (define state (initial-ui-state))
      (define inp (make-test-input-state))
      (define layout (compute-layout rows cols))

      ;; Legacy path
      (parameterize ([current-ubuf-clear cb-clear]
                     [current-ubuf-putstring cb-putstring])
        (render-frame! buf-legacy state inp layout))

      ;; VDOM path
      (loop:render-frame-vdom! buf-vdom state inp layout)

      ;; Compare
      (define diff (buffers-diff buf-legacy buf-vdom))
      (check-false diff diff))

    ;; ----------------------------------------------------------
    ;; Frame with transcript entries
    ;; ----------------------------------------------------------
    (test-case "frame with transcript entries parity"
      (define cols 80)
      (define rows 24)
      (define buf-legacy (make-cell-buffer cols rows))
      (define buf-vdom (make-cell-buffer cols rows))

      ;; Create state with entries
      (define state0 (initial-ui-state))
      (define state1 (add-transcript-entry state0 (make-entry 'user "Hello, world!")))
      (define state2 (add-transcript-entry state1 (make-entry 'assistant "Hi there!")))
      (define state3 (add-transcript-entry state2 (make-entry 'user "Tell me about Racket")))

      (define inp (make-test-input-state #:text "test prompt"))
      (define layout (compute-layout rows cols))

      ;; Legacy path
      (parameterize ([current-ubuf-clear cb-clear]
                     [current-ubuf-putstring cb-putstring])
        (render-frame! buf-legacy state3 inp layout))

      ;; VDOM path
      (loop:render-frame-vdom! buf-vdom state3 inp layout)

      ;; Compare
      (define diff (buffers-diff buf-legacy buf-vdom))
      (check-false diff diff))

    ;; ----------------------------------------------------------
    ;; Frame with session/model labels (status bar content)
    ;; ----------------------------------------------------------
    (test-case "status bar with labels parity"
      (define cols 80)
      (define rows 24)
      (define buf-legacy (make-cell-buffer cols rows))
      (define buf-vdom (make-cell-buffer cols rows))
      (define state (initial-ui-state #:session-id "test-session" #:model-name "gpt-4o"))
      (define inp (make-test-input-state))
      (define layout (compute-layout rows cols))

      ;; Legacy path
      (parameterize ([current-ubuf-clear cb-clear]
                     [current-ubuf-putstring cb-putstring])
        (render-frame! buf-legacy state inp layout))

      ;; VDOM path
      (loop:render-frame-vdom! buf-vdom state inp layout)

      ;; Compare — full buffer
      (define diff (buffers-diff buf-legacy buf-vdom))
      (check-false diff diff))

    ;; ----------------------------------------------------------
    ;; Frame with input line content
    ;; ----------------------------------------------------------
    (test-case "input line content parity"
      (define cols 80)
      (define rows 24)
      (define buf-legacy (make-cell-buffer cols rows))
      (define buf-vdom (make-cell-buffer cols rows))
      (define state (initial-ui-state))
      (define inp (make-test-input-state #:text "some longer input text here for testing"))
      (define layout (compute-layout rows cols))

      ;; Legacy path
      (parameterize ([current-ubuf-clear cb-clear]
                     [current-ubuf-putstring cb-putstring])
        (render-frame! buf-legacy state inp layout))

      ;; VDOM path
      (loop:render-frame-vdom! buf-vdom state inp layout)

      ;; Compare — check input row specifically
      (define input-row (sub1 rows))
      (for ([c (in-range cols)])
        (define cell-a (cell-buffer-ref buf-legacy c input-row))
        (define cell-b (cell-buffer-ref buf-vdom c input-row))
        (check-equal? (cell-char cell-a) (cell-char cell-b) (format "input-row char at col ~a" c))))))

;; ============================================================
;; Run
;; ============================================================

(run-tests parity-tests)
