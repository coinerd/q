#lang racket/base

;; @speed fast
;; @suite default

;; q/tests/test-gui-streaming-cursor.rkt — Tests for streaming-cursor.rkt

(require rackunit
         rackunit/text-ui
         "../gui/components/streaming-cursor.rkt")

(define-test-suite streaming-cursor-tests
                   (test-case "initial state is inactive"
                     (define s (make-streaming-cursor-state))
                     (check-false (streaming-cursor-active? s))
                     (check-equal? (streaming-cursor-string s) ""))
                   (test-case "cursor string visible when active phase 0"
                     (define s (make-streaming-cursor-state))
                     (hash-set! s 'active? #t)
                     (hash-set! s 'phase 0)
                     (check-equal? (streaming-cursor-string s) "|"))
                   (test-case "cursor string hidden when active phase 1"
                     (define s (make-streaming-cursor-state))
                     (hash-set! s 'active? #t)
                     (hash-set! s 'phase 1)
                     (check-equal? (streaming-cursor-string s) " "))
                   (test-case "toggle phase flips between 0 and 1"
                     (define s (make-streaming-cursor-state))
                     (hash-set! s 'active? #t)
                     (hash-set! s 'phase 0)
                     (streaming-cursor-toggle-phase! s)
                     (check-equal? (hash-ref s 'phase) 1)
                     (streaming-cursor-toggle-phase! s)
                     (check-equal? (hash-ref s 'phase) 0))
                   (test-case "start! sets active and phase 0"
                     (define s (make-streaming-cursor-state))
                     (define notify-count 0)
                     (define (notify!)
                       (set! notify-count (+ notify-count 1)))
                     (streaming-cursor-start! s notify!)
                     (check-true (streaming-cursor-active? s))
                     (check-equal? (hash-ref s 'phase) 0)
                     (check-equal? (streaming-cursor-string s) "|")
                     (streaming-cursor-stop! s))
                   (test-case "stop! sets inactive and empty string"
                     (define s (make-streaming-cursor-state))
                     (hash-set! s 'active? #t)
                     (hash-set! s 'phase 1)
                     (streaming-cursor-stop! s)
                     (check-false (streaming-cursor-active? s))
                     (check-equal? (streaming-cursor-string s) ""))
                   (test-case "multiple start/stop cycles"
                     (define s (make-streaming-cursor-state))
                     (define (noop)
                       (void))
                     (streaming-cursor-start! s noop)
                     (check-true (streaming-cursor-active? s))
                     (streaming-cursor-stop! s)
                     (check-false (streaming-cursor-active? s))
                     (streaming-cursor-start! s noop)
                     (check-true (streaming-cursor-active? s))
                     (streaming-cursor-stop! s)
                     (check-false (streaming-cursor-active? s))))

(module+ main
  (run-tests streaming-cursor-tests))
