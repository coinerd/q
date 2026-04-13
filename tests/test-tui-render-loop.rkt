#lang racket

;; test-tui-render-loop.rkt — Tests for tui/tui-render-loop.rkt
;;
;; Tests module loading, export binding, and pure helpers.
;; render-frame!, tui-main-loop, etc. require a real terminal,
;; so we test only loadable exports and pure functions.

(require rackunit
         rackunit/text-ui
         "../tui/tui-render-loop.rkt"
         "../tui/tui-keybindings.rkt"
         "../tui/sgr.rkt")

(define test-tui-render-loop
  (test-suite "tui/tui-render-loop"

    ;; --------------------------------------------------
    ;; Test 1: Module exports are bound
    ;; --------------------------------------------------
    (test-case "render-frame! is bound"
      (check-pred procedure? render-frame!))

    (test-case "draw-frame is bound (deprecated alias)"
      (check-pred procedure? draw-frame))

    (test-case "next-message is bound"
      (check-pred procedure? next-message))

    (test-case "tui-main-loop is bound"
      (check-pred procedure? tui-main-loop))

    (test-case "drain-events! is bound"
      (check-pred procedure? drain-events!))

    ;; --------------------------------------------------
    ;; Test 2: Re-exported functions are accessible
    ;; --------------------------------------------------
    (test-case "fix-sgr-bg-black is bound"
      (check-pred procedure? fix-sgr-bg-black))

    (test-case "decode-mouse-x10 is bound"
      (check-pred procedure? decode-mouse-x10))

    (test-case "tui-ctx-init-terminal! is bound"
      (check-pred procedure? tui-ctx-init-terminal!))

    (test-case "tui-ctx-resize-ubuf! is bound"
      (check-pred procedure? tui-ctx-resize-ubuf!))

    ;; --------------------------------------------------
    ;; Test 3: fix-sgr-bg-black pure function works
    ;; --------------------------------------------------
    (test-case "fix-sgr-bg-black replaces bg=0 SGR sequences"
      ;; SGR 40 is "bg black" (ANSI code 40). fix-sgr-bg-black should replace
      ;; occurrences of ;40m or 40m (terminal default bg) with ;49m (default bg)
      (define input "\x1b[30;40mhello\x1b[0m")
      (define result (fix-sgr-bg-black input))
      (check-not-false (regexp-match? #rx"49m" result)
                       "bg=0 (40) should be replaced with default bg (49)"))

    (test-case "fix-sgr-bg-black leaves non-black bg unchanged"
      (define input "\x1b[30;41mhello\x1b[0m")
      (define result (fix-sgr-bg-black input))
      (check-false (regexp-match? #rx"49m" result) "bg=red (41) should not be replaced"))

    ;; --------------------------------------------------
    ;; Test 4: decode-mouse-x10 decodes mouse bytes
    ;; --------------------------------------------------
    (test-case "decode-mouse-x10 returns list with mouse type"
      (define result (decode-mouse-x10 32 49 49))
      (check-pred list? result)
      (check-true (>= (length result) 1)))

    ;; --------------------------------------------------
    ;; Test 5: drain-events! on empty channel is safe
    ;; --------------------------------------------------
    (test-case "drain-events! on empty channel does not crash"
      (define ctx (make-tui-ctx))
      (check-not-exn (lambda () (drain-events! ctx))))))

(run-tests test-tui-render-loop)
