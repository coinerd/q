#lang racket/base

;; runtime/trace-sink.rkt — Trace sink protocol for dependency injection (F9)
;; STABILITY: evolving
;;
;; Defines a trace-sink<%> interface that abstracts trace output.
;; Allows swapping file/port/null sinks without changing trace-logger.

(require racket/class
         racket/contract)

(provide (contract-out [trace-sink<%> any/c]
                       [file-trace-sink% any/c]
                       [port-trace-sink% any/c]
                       [null-trace-sink% any/c]))

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
