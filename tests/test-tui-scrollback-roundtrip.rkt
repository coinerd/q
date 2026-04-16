#lang racket

;; tests/test-tui-scrollback-roundtrip.rkt — Scrollback roundtrip and new
;; content preservation tests (Wave 2, W2.2 #880).
;;
;; Tests that scrollback serialization/deserialization preserves entry data
;; and that new events after scrollback load produce correct content.

(require rackunit
         rackunit/text-ui
         "tui/workflow-harness.rkt"
         "../tui/state.rkt"
         "../tui/scrollback.rkt"
         "../util/protocol-types.rkt"
         racket/file)

(define scrollback-roundtrip-tests
  (test-suite
   "Scrollback Roundtrip Tests"

   ;; SR1: Serialize and deserialize preserves entry data
   (test-case "SR1: entry roundtrip preserves kind, text, timestamp"
     (reset-scrollback-id-counter!)
     (define original
       (transcript-entry 'assistant "Hello world" 12345.0 (hasheq 'foo "bar") 42))
     (define jsexpr (transcript-entry->jsexpr original))
     (define restored (jsexpr->transcript-entry jsexpr))
     (check-equal? (transcript-entry-kind restored) 'assistant)
     (check-equal? (transcript-entry-text restored) "Hello world")
     (check-equal? (transcript-entry-timestamp restored) 12345.0)
     ;; Meta should be preserved
     (check-equal? (hash-ref (transcript-entry-meta restored) 'foo #f) "bar"))

   ;; SR2: save-scrollback + load-scrollback file roundtrip
   (test-case "SR2: file save/load roundtrip"
     (reset-scrollback-id-counter!)
     (define tmp-dir (make-temporary-file "scrollback-test-~a" 'directory))
     (define tmp-path (build-path tmp-dir "scrollback.jsonl"))
     (define entries
       (list (transcript-entry 'assistant "msg A" 100 (hash) 0)
             (transcript-entry 'tool-start "[TOOL: read]" 200 (hash) 1)
             (transcript-entry 'assistant "msg B" 300 (hash) 2)))
     ;; Save
     (save-scrollback entries tmp-path)
     (check-true (file-exists? tmp-path))
     ;; Load
     (define loaded (load-scrollback tmp-path))
     (check-equal? (length loaded) 3)
     (check-equal? (transcript-entry-text (first loaded)) "msg A")
     (check-equal? (transcript-entry-text (second loaded)) "[TOOL: read]")
     (check-equal? (transcript-entry-text (third loaded)) "msg B")
     ;; Cleanup
     (delete-directory/files tmp-dir))

   ;; SR3: New content after scrollback load renders correctly
   (test-case "SR3: new events after scrollback load render correctly"
     (reset-scrollback-id-counter!)
     (define tmp-dir (make-temporary-file "scrollback-test-~a" 'directory))
     (define tmp-path (build-path tmp-dir "scrollback.jsonl"))
     ;; Save some scrollback
     (define old-entries
       (list (transcript-entry 'assistant "Old content" 100 (hash) 0)))
     (save-scrollback old-entries tmp-path)
     ;; Load and build initial state (simulating tui-init.rkt logic)
     (define loaded (load-scrollback tmp-path))
     (define max-id
       (for/fold ([m -1]) ([e (in-list loaded)])
         (max m (or (transcript-entry-id e) -1))))
     (define state0
       (struct-copy ui-state
                    (initial-ui-state)
                    [transcript loaded]
                    [next-entry-id (add1 max-id)]))
     ;; Apply new event
     (define new-evt
       (make-test-event "assistant.message.completed"
                        (hash 'content "New content")))
     (define state1 (apply-event-to-state state0 new-evt))
     ;; Render
     (define-values (lines _st) (render-state-strings state1 80 24))
     ;; Both old and new should be visible
     (check-not-false
      (for/or ([l (in-list lines)]) (string-contains? l "Old content"))
      "old scrollback content should render")
     (check-not-false
      (for/or ([l (in-list lines)]) (string-contains? l "New content"))
      "new event content should render")
     ;; Cleanup
     (delete-directory/files tmp-dir))

   ;; SR4: Scrollback trimming at max-entries boundary
   (test-case "SR4: scrollback preserves last N entries on save"
     (reset-scrollback-id-counter!)
     (define tmp-dir (make-temporary-file "scrollback-test-~a" 'directory))
     (define tmp-path (build-path tmp-dir "scrollback.jsonl"))
     ;; Create 5 entries with sequential content
     (define entries
       (for/list ([i (in-range 5)])
         (transcript-entry 'assistant (format "entry ~a" i) (* i 100) (hash) i)))
     (save-scrollback entries tmp-path)
     ;; Load back — all 5 should be present (under max of 500)
     (define loaded (load-scrollback tmp-path))
     (check-equal? (length loaded) 5)
     (check-equal? (transcript-entry-text (first loaded)) "entry 0")
     (check-equal? (transcript-entry-text (last loaded)) "entry 4")
     ;; Cleanup
     (delete-directory/files tmp-dir))))

(run-tests scrollback-roundtrip-tests)
