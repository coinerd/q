#lang racket

(require rackunit
         rackunit/text-ui
         racket/file
         racket/format
         "../runtime/session-store.rkt"
         "../runtime/compactor.rkt"
         "../runtime/session-index.rkt"
         "../agent/event-bus.rkt"
         "../util/protocol-types.rkt"
         "../util/ids.rkt")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (make-temp-session-dir)
  (define dir (make-temporary-file "q-perf-test-~a" 'directory))
  dir)

(define (make-n-messages n)
  (for/list ([i (in-range n)])
    (make-message (generate-id) #f
                  (if (even? i) 'user 'assistant)
                  'message
                  (list (make-text-part (format "Message ~a" i)))
                  (current-seconds)
                  (hasheq))))

;; ---------------------------------------------------------------------------
;; Performance Test Suite
;; ---------------------------------------------------------------------------

(define performance-tests
  (test-suite
   "Performance Tests"

   ;; -----------------------------------------------------------------------
   ;; 1. Session Store: Append 100 entries in < 500ms
   ;; -----------------------------------------------------------------------
   (test-case "Session Store: append 100 entries in < 500ms"
     (define dir (make-temp-session-dir))
     (define log-path (build-path dir "session.jsonl"))
     (define msgs (make-n-messages 100))
     (define start (current-inexact-milliseconds))
     (for ([msg (in-list msgs)])
       (append-entry! log-path msg))
     (define elapsed (- (current-inexact-milliseconds) start))
     (check-true (< elapsed 500)
                 (format "append 100 entries took ~ams (limit 500ms)" elapsed))
     ;; Verify all entries survived
     (check-equal? (length (load-session-log log-path)) 100)
     (delete-directory/files dir))

   ;; -----------------------------------------------------------------------
   ;; 2. Compaction: compact-history for 1000 messages in < 2s
   ;; -----------------------------------------------------------------------
   (test-case "Compaction: compact-history 1000 messages in < 2s"
     (define msgs (make-n-messages 1000))
     (define start (current-inexact-milliseconds))
     (define result (compact-history msgs))
     (define elapsed (- (current-inexact-milliseconds) start))
     (check-true (< elapsed 2000)
                 (format "compact-history 1000 msgs took ~ams (limit 2000ms)" elapsed))
     (check-pred compaction-result? result)
     (check-true (>= (compaction-result-removed-count result) 0))
     ;; kept-messages should be <= input count
     (check-true (<= (length (compaction-result-kept-messages result)) 1000))
     ;; summary-message should be present when old messages were summarized
     (when (> (compaction-result-removed-count result) 0)
       (check-not-false (compaction-result-summary-message result))))

   ;; -----------------------------------------------------------------------
   ;; 3. Context Assembly: build-tiered-context for 200 messages in < 500ms
   ;; -----------------------------------------------------------------------
   (test-case "Context Assembly: build-tiered-context 200 messages in < 500ms"
     (define msgs (make-n-messages 200))
     (define start (current-inexact-milliseconds))
     (define tc (build-tiered-context msgs))
     (define elapsed (- (current-inexact-milliseconds) start))
     (check-true (< elapsed 500)
                 (format "build-tiered-context 200 msgs took ~ams (limit 500ms)" elapsed))
     (check-pred tiered-context? tc)
     ;; Flattened output should contain some messages
     (check-true (> (length (tiered-context->message-list tc)) 0)))

   ;; -----------------------------------------------------------------------
   ;; 4. Session Index: build-index! for 100 entries in < 500ms
   ;; -----------------------------------------------------------------------
   (test-case "Session Index: build-index! 100 entries in < 500ms"
     (define dir (make-temp-session-dir))
     (define log-path (build-path dir "session.jsonl"))
     (define idx-path (build-path dir "session.idx"))
     (define msgs (make-n-messages 100))
     ;; Write entries first
     (append-entries! log-path msgs)
     (define start (current-inexact-milliseconds))
     (define idx (build-index! log-path idx-path))
     (define elapsed (- (current-inexact-milliseconds) start))
     (check-true (< elapsed 500)
                 (format "build-index! 100 entries took ~ams (limit 500ms)" elapsed))
     (check-true (session-index? idx))
     ;; All 100 entries should be in the index
     (check-equal? (hash-count (session-index-by-id idx)) 100)
     (delete-directory/files dir))

   ;; -----------------------------------------------------------------------
   ;; 5. Event Bus: Publish 1000 events in < 500ms
   ;; -----------------------------------------------------------------------
   (test-case "Event Bus: publish 1000 events in < 500ms"
     (define bus (make-event-bus))
     (define received 0)
     (subscribe! bus (λ (evt) (set! received (add1 received))))
     (define events
       (for/list ([i (in-range 1000)])
         (make-event 'test.event (current-inexact-milliseconds)
                     (format "session-~a" i) (format "turn-~a" i)
                     (hasheq 'index i))))
     (define start (current-inexact-milliseconds))
     (for ([evt (in-list events)])
       (publish! bus evt))
     (define elapsed (- (current-inexact-milliseconds) start))
     (check-true (< elapsed 500)
                 (format "publish 1000 events took ~ams (limit 500ms)" elapsed))
     (check-equal? received 1000))

   ;; -----------------------------------------------------------------------
   ;; 6. JSONL Read/Write Roundtrip: 50 entries in < 200ms
   ;; -----------------------------------------------------------------------
   (test-case "JSONL Read/Write Roundtrip: 50 entries in < 200ms"
     (define dir (make-temp-session-dir))
     (define log-path (build-path dir "roundtrip.jsonl"))
     (define msgs (make-n-messages 50))
     (define start (current-inexact-milliseconds))
     ;; Write all entries
     (append-entries! log-path msgs)
     ;; Read them back
     (define loaded (load-session-log log-path))
     (define elapsed (- (current-inexact-milliseconds) start))
     (check-true (< elapsed 200)
                 (format "JSONL roundtrip 50 entries took ~ams (limit 200ms)" elapsed))
     (check-equal? (length loaded) 50)
     ;; Verify content roundtrip: first and last message text preserved
     (check-equal? (text-part-text (first (message-content (first loaded))))
                   "Message 0")
     (check-equal? (text-part-text (first (message-content (last loaded))))
                   "Message 49")
     (delete-directory/files dir))

   ))

;; ---------------------------------------------------------------------------
;; Run
;; ---------------------------------------------------------------------------

(run-tests performance-tests)
