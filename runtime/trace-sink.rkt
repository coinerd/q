#lang racket/base

;; runtime/trace-sink.rkt — Trace sink protocol for dependency injection (F9)
;; STABILITY: evolving
;;
;; Defines a trace-sink<%> interface that abstracts trace output.
;; Allows swapping file/port/null sinks without changing trace-logger.

(require racket/class
         racket/contract
         json)

(provide (contract-out [trace-sink<%> any/c]
                       [file-trace-sink% any/c]
                       [json-file-trace-sink% any/c]
                       [port-trace-sink% any/c]
                       [null-trace-sink% any/c]
                       [async-trace-sink% any/c]))

;; Interface: trace-sink<%>
(define trace-sink<%>
  (interface ()
    trace-write! ; jsexpr -> void
    trace-flush! ; -> void
    trace-close!)) ; -> void

;; File-based trace sink (default)
(define file-trace-sink%
  (class* object% (trace-sink<%>)
    (init-field path [exists-mode 'append])
    (super-new)
    (define out #f)

    (define/public (trace-write! entry)
      (unless out
        (set! out (open-output-file path #:exists exists-mode)))
      (write entry out)
      (newline out))

    (define/public (trace-flush!)
      (when out
        (flush-output out)))

    (define/public (trace-close!)
      (when out
        (close-output-port out)
        (set! out #f)))))

;; JSON file trace sink (v0.70.4) — writes jsexpr via write-json
(define json-file-trace-sink%
  (class* object% (trace-sink<%>)
    (init-field path [exists-mode 'append])
    (super-new)
    (define out #f)

    (define/public (trace-write! entry)
      (unless out
        (set! out (open-output-file path #:exists exists-mode)))
      (write-json entry out)
      (newline out))

    (define/public (trace-flush!)
      (when out
        (flush-output out)))

    (define/public (trace-close!)
      (when out
        (close-output-port out)
        (set! out #f)))))

;; Port-based trace sink (for testing/capture)
(define port-trace-sink%
  (class* object% (trace-sink<%>)
    (init-field port)
    (super-new)

    (define/public (trace-write! entry)
      (write entry port)
      (newline port))

    (define/public (trace-flush!) (flush-output port))

    (define/public (trace-close!) (void))))

;; Null trace sink (discards all output)
(define null-trace-sink%
  (class* object% (trace-sink<%>)
    (super-new)

    (define/public (trace-write! entry) (void))

    (define/public (trace-flush!) (void))

    (define/public (trace-close!) (void))))


;; -- Async trace sink (v0.70.4) ------------------------------------------------
;; Wraps an inner sink with a worker thread + bounded mailbox.
;; Backpressure policies:
;;   'block    -- wait until space available (default, audit-safe)
;;   'drop-new -- silently drop new entries when full
;;   'drop-old -- drop oldest entry when full (currently falls back to drop-new)
;;
;; trace-flush! blocks until the worker has flushed the inner sink.
;; trace-close! flushes, closes inner sink, and terminates worker.

(define async-trace-sink%
  (class* object% (trace-sink<%>)
    (init-field inner-sink [capacity 100] [policy 'block])
    (super-new)

    (define space-sema (make-semaphore capacity))
    (define flush-ch (make-channel))
    (define closed-box (box #f))

    (define worker
      (thread
       (lambda ()
         (let loop ()
           (define msg (thread-receive))
           (cond
             [(eq? msg 'flush)
              (send inner-sink trace-flush!)
              (channel-put flush-ch 'done)
              (semaphore-post space-sema)
              (loop)]
             [(eq? msg 'stop)
              (send inner-sink trace-flush!)
              (send inner-sink trace-close!)]
             [else
              (send inner-sink trace-write! msg)
              (semaphore-post space-sema)
              (loop)])))))

    (define (try-send! item)
      (case policy
        [(block)
         (semaphore-wait space-sema)
         (thread-send worker item)]
        [(drop-new)
         (if (semaphore-try-wait? space-sema)
             (thread-send worker item)
             (void))]
        [(drop-old)
         ;; TODO: implement true drop-oldest by using a shared queue
         ;; instead of thread mailbox. For now, fallback to drop-new.
         (if (semaphore-try-wait? space-sema)
             (thread-send worker item)
             (void))]
        [else
         (semaphore-wait space-sema)
         (thread-send worker item)]))

    (define/public (trace-write! entry)
      (unless (unbox closed-box)
        (try-send! entry)))

    (define/public (trace-flush!)
      (unless (unbox closed-box)
        (try-send! 'flush)
        (channel-get flush-ch)))

    (define/public (trace-close!)
      (unless (unbox closed-box)
        (set-box! closed-box #t)
        (try-send! 'stop)
        (thread-wait worker)))))
