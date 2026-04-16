#lang racket

;; tests/test-scheduler-hooks.rkt — Wave 11: TS1-TS8 scheduler hook protocol
;;
;; Tests for the complete hook protocol between scheduler and extensions:
;; tool-call-pre (block/amend/fallthrough), tool-result-post (block/amend/fallthrough),
;; file mutation queue serialization, and preflight exception handling.

(require rackunit
         rackunit/text-ui
         "../util/protocol-types.rkt"
         "../util/hook-types.rkt"
         "../agent/event-bus.rkt"
         (only-in "../tools/tool.rkt"
                  make-tool make-tool-registry register-tool! lookup-tool
                  make-success-result make-error-result
                  tool-result? tool-result-content tool-result-is-error?
                  tool-call? tool-call-name tool-call-arguments
                  make-tool-call make-exec-context)
         "../tools/scheduler.rkt")

;; ============================================================
;; Helpers
;; ============================================================

;; Extract text from error result content
(define (error-text tr)
  (define c (tool-result-content tr))
  (if (list? c)
      (let ([first-entry (and (pair? c) (car c))])
        (if (hash? first-entry)
            (hash-ref first-entry 'text "")
            (format "~a" c)))
      (format "~a" c)))

;; Create a simple tool that echoes its args
(define (make-echo-tool #:name [name "echo"])
  (make-tool name "Echo tool" (hasheq)
             (lambda (args ctx)
               (make-success-result (format "echo: ~a" args)))))

;; Create a tool that records execution in a box
(define (make-recording-tool box #:name [name "rec"])
  (make-tool name "Recording tool" (hasheq)
             (lambda (args ctx)
               (set-box! box (cons args (unbox box)))
               (make-success-result "recorded"))))

;; Simple hook dispatcher builder
;; Arguments: alternating hook-point-symbol handler-proc
(define (make-hook-dispatcher . pairs)
  (define table (apply hasheq pairs))
  (lambda (hook-point payload)
    (define handler (hash-ref table hook-point #f))
    (if handler
        (handler payload)
        #f)))

;; ============================================================
;; Tests
;; ============================================================

(define scheduler-hook-tests
  (test-suite
   "Scheduler Hook Protocol Tests"

   ;; ============================================================
   ;; TS1: tool-call-pre hook blocks execution
   ;; ============================================================
   (test-case
    "TS1: tool-call-pre hook blocks execution"
    (define reg (make-tool-registry))
    (register-tool! reg (make-echo-tool))
    (define tc (make-tool-call "tc-1" "echo" (hasheq 'msg "hello")))
    (define dispatcher
      (make-hook-dispatcher
       'tool-call-pre
       (lambda (payload) (hook-block "blocked by policy"))))
    (define result
      (run-tool-batch (list tc) reg #:hook-dispatcher dispatcher))
    (define results (scheduler-result-results result))
    (check-equal? (length results) 1)
    (check-true (tool-result-is-error? (car results))
                "blocked pre hook produces error result")
    (check-not-false (regexp-match? #rx"blocked" (error-text (car results)))
                     "error message mentions blocking"))

   ;; ============================================================
   ;; TS2: tool-call-pre hook amends arguments
   ;; ============================================================
   (test-case
    "TS2: tool-call-pre hook amends arguments"
    (define exec-log (box '()))
    (define reg (make-tool-registry))
    (register-tool! reg (make-recording-tool exec-log))
    (define tc (make-tool-call "tc-2" "rec" (hasheq 'original #t)))
    (define dispatcher
      (make-hook-dispatcher
       'tool-call-pre
       (lambda (payload)
         (hook-amend (hasheq 'args (hasheq 'amended #t))))))
    (define result
      (run-tool-batch (list tc) reg #:hook-dispatcher dispatcher))
    (define results (scheduler-result-results result))
    (check-equal? (length results) 1)
    (check-false (tool-result-is-error? (car results))
                 "amended args should succeed")
    ;; Check that tool received amended args
    (check-equal? (unbox exec-log) (list (hasheq 'amended #t))
                  "tool received amended arguments"))

   ;; ============================================================
   ;; TS3: tool-call-pre hook amend with non-hash payload falls through
   ;; ============================================================
   (test-case
    "TS3: tool-call-pre hook amend with non-hash payload falls through"
    (define exec-log (box '()))
    (define reg (make-tool-registry))
    (register-tool! reg (make-recording-tool exec-log))
    (define tc (make-tool-call "tc-3" "rec" (hasheq 'original #t)))
    (define dispatcher
      (make-hook-dispatcher
       'tool-call-pre
       (lambda (payload)
         ;; Return amend with non-hash payload (a string)
         (hook-amend "not a hash"))))
    (define result
      (run-tool-batch (list tc) reg #:hook-dispatcher dispatcher))
    (define results (scheduler-result-results result))
    (check-false (tool-result-is-error? (car results))
                 "non-hash amend falls through to original args")
    ;; Original args should be used
    (check-equal? (unbox exec-log) (list (hasheq 'original #t))
                  "original args used when amend payload is not a hash"))

   ;; ============================================================
   ;; TS4: tool-result-post hook blocks result
   ;; ============================================================
   (test-case
    "TS4: tool-result-post hook blocks result"
    (define reg (make-tool-registry))
    (register-tool! reg (make-echo-tool))
    (define tc (make-tool-call "tc-4" "echo" (hasheq 'msg "test")))
    (define dispatcher
      (make-hook-dispatcher
       'tool-result-post
       (lambda (payload) (hook-block "result blocked"))))
    (define result
      (run-tool-batch (list tc) reg #:hook-dispatcher dispatcher))
    (define results (scheduler-result-results result))
    (check-true (tool-result-is-error? (car results))
                "blocked result-post hook produces error result")
    (check-not-false (regexp-match? #rx"blocked" (error-text (car results)))
                     "error message mentions result blocking"))

   ;; ============================================================
   ;; TS5: tool-result-post hook amends result
   ;; ============================================================
   (test-case
    "TS5: tool-result-post hook amends result"
    (define reg (make-tool-registry))
    (register-tool! reg (make-echo-tool))
    (define tc (make-tool-call "tc-5" "echo" (hasheq 'msg "test")))
    (define amended-result (make-success-result "amended content"))
    (define dispatcher
      (make-hook-dispatcher
       'tool-result-post
       (lambda (payload)
         (hook-amend (hasheq 'result amended-result)))))
    (define result
      (run-tool-batch (list tc) reg #:hook-dispatcher dispatcher))
    (define results (scheduler-result-results result))
    (check-false (tool-result-is-error? (car results))
                 "amended result should not be error")
    (check-equal? (tool-result-content (car results)) "amended content"
                  "result was amended by hook"))

   ;; ============================================================
   ;; TS6: tool-result-post hook amend with invalid result falls back
   ;; ============================================================
   (test-case
    "TS6: tool-result-post hook amend with invalid result falls back"
    (define reg (make-tool-registry))
    (register-tool! reg (make-echo-tool))
    (define tc (make-tool-call "tc-6" "echo" (hasheq 'msg "original")))
    (define dispatcher
      (make-hook-dispatcher
       'tool-result-post
       (lambda (payload)
         ;; Return amend with non-tool-result value
         (hook-amend (hasheq 'result "not a tool-result")))))
    (define result
      (run-tool-batch (list tc) reg #:hook-dispatcher dispatcher))
    (define results (scheduler-result-results result))
    (check-false (tool-result-is-error? (car results))
                 "invalid amend falls back to original result")
    ;; Original result should be used (echo tool returns formatted args)
    (check-not-false (regexp-match? #rx"echo" (tool-result-content (car results)))
                     "original result preserved when amend is invalid"))

   ;; ============================================================
   ;; TS7: File mutation queue with parallel writes to same file
   ;; ============================================================
   (test-case
    "TS7: file mutation queue serializes parallel writes to same file"
    (define write-count (box 0))
    (define active-writers (box 0))
    (define max-concurrent (box 0))
    (define counter-sema (make-semaphore 1))
    (define reg (make-tool-registry))
    (register-tool! reg
      (make-tool "write" "Write file" (hasheq)
        (lambda (args ctx)
          ;; Track concurrency
          (call-with-semaphore counter-sema
            (lambda ()
              (set-box! active-writers (add1 (unbox active-writers)))
              (when (> (unbox active-writers) (unbox max-concurrent))
                (set-box! max-concurrent (unbox active-writers)))))
          (sleep 0.01) ; simulate work
          (call-with-semaphore counter-sema
            (lambda ()
              (set-box! active-writers (sub1 (unbox active-writers)))))
          (set-box! write-count (add1 (unbox write-count)))
          (make-success-result "written"))))
    (define tcs
      (for/list ([i (in-range 3)])
        (make-tool-call (format "tc-~a" i) "write"
                        (hasheq 'path "/tmp/test-scheduler-file.txt"
                                'content (format "content ~a" i)))))
    (define result
      (run-tool-batch tcs reg #:parallel? #t))
    (define results (scheduler-result-results result))
    (check-equal? (length results) 3 "all three writes completed")
    ;; File mutation queue should serialize writes to same path
    (check-equal? (unbox max-concurrent) 1
                  "file mutation queue prevents concurrent writes to same file"))

   ;; ============================================================
   ;; TS8: Preflight hook throws exception
   ;; ============================================================
   (test-case
    "TS8: preflight hook throws exception"
    (define reg (make-tool-registry))
    (register-tool! reg (make-echo-tool))
    (define tc (make-tool-call "tc-8" "echo" (hasheq 'msg "test")))
    (define dispatcher
      (make-hook-dispatcher
       'tool-call
       (lambda (payload)
         (error "preflight boom!"))))
    (define result
      (run-tool-batch (list tc) reg #:hook-dispatcher dispatcher))
    (define results (scheduler-result-results result))
    (check-true (tool-result-is-error? (car results))
                "preflight exception produces error result")
    (check-not-false (regexp-match? #rx"hook error" (error-text (car results)))
                     "error message mentions hook error")
    (define metadata (scheduler-result-metadata result))
    (check-equal? (hash-ref metadata 'errors) 1
                  "metadata counts 1 error from preflight exception"))))

(module+ main
  (run-tests scheduler-hook-tests))

(module+ test
  (run-tests scheduler-hook-tests))
