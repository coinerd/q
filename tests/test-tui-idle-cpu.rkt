#lang racket

;; @speed fast  ;; @suite tui

;; tests/test-tui-idle-cpu.rkt — Regression tests for TUI idle CPU hot paths

(require rackunit
         rackunit/text-ui
         "../tui/context.rkt"
         "../tui/terminal.rkt"
         "../tui/tui-render-loop.rkt"
         "../tui/terminal-input.rkt"
         "../tui/cell-buffer.rkt"
         (only-in "../tui/component.rkt" component-state-ref))

(define idle-cpu-tests
  (test-suite "TUI idle CPU regressions"

    (test-case "screen-size cache default is not a 10Hz stty fallback"
      ;; The historical 100ms TTL allowed the idle resize poll to reach
      ;; current-term-size/stty about 10 times per second. Idle fallback should
      ;; be at least 1 second by default.
      (check >= (current-screen-size-cache-ttl-ms) 1000.0))

    (test-case "screen-size provider is cached within TTL"
      (define calls 0)
      (parameterize ([current-screen-size-provider (lambda ()
                                                     (set! calls (add1 calls))
                                                     (values 100 40))]
                     [current-screen-size-cache-ttl-ms 1000.0])
        (tui-screen-size-cache-reset!)
        (for ([i (in-range 20)])
          (define-values (_cols _rows) (tui-screen-size))
          (void))
        (check-equal? calls 1)))

    (test-case "resize polling helper throttles idle checks"
      (check-true (resize-poll-due? 1000.0 #f 1000.0))
      (check-false (resize-poll-due? 1500.0 1000.0 1000.0))
      (check-true (resize-poll-due? 2001.0 1000.0 1000.0))
      (check >= (current-resize-poll-interval-ms) 1000.0))

    (test-case "cursor blink timer does not mark the whole frame dirty"
      (define ctx (make-tui-ctx))
      (reset-idle-render-state!)
      (set-box! (tui-ctx-needs-redraw-box ctx) #f)
      (check-true (apply-cursor-blink-timer! 1000.0))
      (check-true (cursor-blink-redraw-needed?))
      (check-false (unbox (tui-ctx-needs-redraw-box ctx))))

    (test-case "cursor-only blink frame does not re-render transcript component"
      (define ctx (make-tui-ctx))
      (set-box! (tui-ctx-term-box ctx) #f)
      (set-box! (tui-ctx-ubuf-box ctx) (make-cell-buffer 80 24))
      (reset-idle-render-state!)
      (define out (open-output-string))
      (parameterize ([current-screen-size-provider (lambda () (values 80 24))]
                     [current-output-port out])
        (tui-screen-size-cache-reset!)
        (render-frame! ctx)
        (define reg (unbox (tui-ctx-component-registry-box ctx)))
        (define trans-comp (hash-ref reg 'transcript-vdom))
        (define render-count-before (component-state-ref trans-comp 'render-count 0))
        (check-true (apply-cursor-blink-timer! 1000.0))
        (check-true (render-cursor-blink-frame! ctx))
        (define render-count-after (component-state-ref trans-comp 'render-count 0))
        (check-equal? render-count-after render-count-before)))

    (test-case "idle input poll honors timeout on empty pipe"
      (define-values (in out) (make-pipe))
      (define start (current-inexact-milliseconds))
      (define result
        (parameterize ([current-input-port in])
          (real-stdin-read-msg #:timeout 0.05)))
      (define elapsed (- (current-inexact-milliseconds) start))
      (close-output-port out)
      (close-input-port in)
      (check-false result)
      (check-true (>= elapsed 40.0)
                  (format "idle input poll should wait close to timeout; elapsed=~a" elapsed))
      (check-true (< elapsed 250.0) (format "idle input poll should not hang; elapsed=~a" elapsed)))

    (test-case "EOF input poll backs off instead of tight-spinning"
      (define in (open-input-string ""))
      (define start (current-inexact-milliseconds))
      (define result
        (parameterize ([current-input-port in])
          (real-stdin-read-msg #:timeout 0.02)))
      (define elapsed (- (current-inexact-milliseconds) start))
      (close-input-port in)
      (check-false result)
      (check-true (>= elapsed 15.0)
                  (format "EOF input should back off near timeout; elapsed=~a" elapsed)))))

(run-tests idle-cpu-tests)
