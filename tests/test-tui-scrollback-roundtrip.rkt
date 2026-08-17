#lang racket

;; @speed fast  ;; @suite tui

;; BOUNDARY: io

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
         "../ui-core/conversation-artifact.rkt"
         "../ui-core/feature-flags.rkt"
         "../ui-core/preferences.rkt"
         "../util/message/protocol-types.rkt"
         racket/file)

(define scrollback-roundtrip-tests
  (test-suite "Scrollback Roundtrip Tests"

    ;; SR1: Serialize and deserialize preserves entry data
    (test-case "SR1: entry roundtrip preserves kind, text, timestamp"
      (reset-scrollback-id-counter!)
      (define original (transcript-entry 'assistant "Hello world" 12345.0 (hasheq 'foo "bar") 42))
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
      (define old-entries (list (transcript-entry 'assistant "Old content" 100 (hash) 0)))
      (save-scrollback old-entries tmp-path)
      ;; Load and build initial state (simulating tui-init.rkt logic)
      (define loaded (load-scrollback tmp-path))
      (define max-id
        (for/fold ([m -1]) ([e (in-list loaded)])
          (max m (or (transcript-entry-id e) -1))))
      (define state0
        (struct-copy ui-state (initial-ui-state) [transcript loaded] [next-entry-id (add1 max-id)]))
      ;; Apply new event
      (define new-evt (make-test-event "assistant.message.completed" (hash 'content "New content")))
      (define state1 (apply-event-to-state state0 new-evt))
      ;; Render
      (define-values (lines _st) (render-state-strings state1 80 24))
      ;; Both old and new should be visible
      (check-not-false (for/or ([l (in-list lines)])
                         (string-contains? l "Old content"))
                       "old scrollback content should render")
      (check-not-false (for/or ([l (in-list lines)])
                         (string-contains? l "New content"))
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
      (delete-directory/files tmp-dir))

    ;; v0.99.37 W3: Invalid-input boundary tests for scrollback deserialization
    ;; §28: Deserialization must degrade gracefully on malformed input.

    (test-case "SR5: empty hash produces default entry"
      (reset-scrollback-id-counter!)
      (define rt (jsexpr->transcript-entry (hash)))
      (check-not-false rt "empty hash should not crash")
      (check-equal? (transcript-entry-kind rt) 'system "missing kind defaults to system")
      (check-equal? (transcript-entry-text rt) "" "missing text defaults to empty"))

    (test-case "SR6: missing text key uses empty-string default"
      (reset-scrollback-id-counter!)
      (define h (hasheq 'kind "assistant" 'timestamp 100))
      (define rt (jsexpr->transcript-entry h))
      (check-equal? (transcript-entry-text rt) ""))

    (test-case "SR7: missing meta key produces empty hash"
      (reset-scrollback-id-counter!)
      (define h (hasheq 'kind "assistant" 'text "hello" 'timestamp 100))
      (define rt (jsexpr->transcript-entry h))
      (check-equal? (hash-count (transcript-entry-meta rt)) 0))

    (test-case "SR8: extra unknown keys are ignored"
      (reset-scrollback-id-counter!)
      (define h (hasheq 'kind "assistant" 'text "hello" 'timestamp 100 'bogusKey 'whatever))
      (define rt (jsexpr->transcript-entry h))
      (check-equal? (transcript-entry-text rt) "hello")
      (check-not-false rt))

    (test-case "SR9: load non-existent file returns empty list"
      (define loaded (load-scrollback "/tmp/nonexistent-scrollback-12345.jsonl"))
      (check-equal? loaded '()))

    (test-case "SR10: round-trip preserves meta with nested hashes"
      (reset-scrollback-id-counter!)
      (define meta (hasheq 'a 1 'nested (hasheq 'b 2)))
      (define original (transcript-entry 'tool-start "[TOOL: read]" 500 meta 10))
      (define jsexpr (transcript-entry->jsexpr original))
      (define restored (jsexpr->transcript-entry jsexpr))
      (check-equal? (hash-ref (transcript-entry-meta restored) 'a) 1)
      (check-equal? (hash-ref (hash-ref (transcript-entry-meta restored) 'nested) 'b) 2))

    (test-case "SR11: canonical artifact is serialized only at scrollback boundary"
      ;; W3: artifact persistence now follows the reasoning-visibility
      ;; policy; roundtrip tests opt into the 'scrollback policy.
      (parameterize ([current-preferences
                      (set-preference (default-preferences) 'reasoning-visibility 'scrollback)])
        (reset-scrollback-id-counter!)
        (define body (make-string 350 #\R))
        (define artifact
          (make-conversation-artifact #:id "session-a:turn-a:thinking"
                                      #:session-id "session-a"
                                      #:turn-id "turn-a"
                                      #:kind 'thinking
                                      #:body body
                                      #:lifecycle 'retained
                                      #:persistence 'scrollback))
        (define original (transcript-entry 'thinking body 500 (hasheq 'artifact artifact) 10))
        (check-true (conversation-artifact? (hash-ref (transcript-entry-meta original) 'artifact)))
        (define encoded (transcript-entry->jsexpr original))
        (check-true (hash? (hash-ref (hash-ref encoded 'meta) 'artifact)))
        (define restored (jsexpr->transcript-entry encoded))
        (define restored-artifact (hash-ref (transcript-entry-meta restored) 'artifact))
        (check-true (conversation-artifact? restored-artifact))
        (check-equal? (conversation-artifact-body restored-artifact) body)
        (check-equal? (transcript-entry-text restored) body)))

    (test-case "SR12: oversized reasoning is byte bounded when persisted"
      (parameterize ([ui-reasoning-artifacts-max-bytes 17]
                     [current-preferences
                      (set-preference (default-preferences) 'reasoning-visibility 'scrollback)])
        (define body (make-string 20 #\λ))
        (define artifact
          (make-conversation-artifact #:id "large"
                                      #:session-id "session-a"
                                      #:turn-id "turn-a"
                                      #:kind 'thinking
                                      #:body body
                                      #:lifecycle 'retained
                                      #:persistence 'scrollback))
        (define encoded
          (transcript-entry->jsexpr
           (transcript-entry 'thinking body 0 (hasheq 'artifact artifact) 1)))
        (check-true (<= (bytes-length (string->bytes/utf-8 (hash-ref encoded 'text))) 17))
        (check-true (<= (bytes-length (string->bytes/utf-8
                                       (hash-ref (hash-ref (hash-ref encoded 'meta) 'artifact)
                                                 'body)))
                        17))))

    (test-case "SR13: newest-first scrollback retains the newest 500 entries"
      (define tmp-dir (make-temporary-file "scrollback-newest-~a" 'directory))
      (define tmp-path (build-path tmp-dir "scrollback.jsonl"))
      (define entries
        (for/list ([i (in-range 500 -1 -1)])
          (transcript-entry 'assistant (number->string i) i (hasheq) i)))
      (save-scrollback entries tmp-path)
      (define loaded (load-scrollback tmp-path))
      (check-equal? (length loaded) 500)
      (check-equal? (transcript-entry-text (first loaded)) "500")
      (check-equal? (transcript-entry-text (last loaded)) "1")
      (delete-directory/files tmp-dir))

    (test-case "SR14: legacy thinking without artifact metadata is byte bounded"
      (parameterize ([ui-reasoning-artifacts-max-bytes 17]
                     [current-preferences
                      (set-preference (default-preferences) 'reasoning-visibility 'scrollback)])
        (define encoded
          (transcript-entry->jsexpr (transcript-entry 'thinking (make-string 20 #\λ) 0 (hasheq) 1)))
        (check-true (<= (bytes-length (string->bytes/utf-8 (hash-ref encoded 'text))) 17))))

    (test-case "SR15: malformed artifact schema is rejected at deserialization boundary"
      (check-exn exn:fail:contract?
                 (lambda ()
                   (jsexpr->transcript-entry
                    (hasheq 'kind
                            "thinking"
                            'text
                            "bad"
                            'timestamp
                            0
                            'meta
                            (hasheq 'artifact
                                    (hasheq 'schema "conversation-artifact" 'schema-version 999)))))))

    ;; W3: reasoning-visibility policy gates what crosses the scrollback
    ;; serialization boundary.
    (test-case "W3-P1: reasoning-visibility never strips thinking bodies at the boundary"
      (define artifact
        (make-conversation-artifact #:id "a"
                                    #:session-id "s"
                                    #:turn-id "t"
                                    #:kind 'thinking
                                    #:body "secret reasoning"
                                    #:lifecycle 'retained
                                    #:persistence 'scrollback))
      (define entry (transcript-entry 'thinking "secret reasoning" 0 (hasheq 'artifact artifact) 1))
      (define encoded
        (parameterize ([current-preferences
                        (set-preference (default-preferences) 'reasoning-visibility 'never)])
          (transcript-entry->jsexpr entry)))
      (check-false (hash-ref (hash-ref encoded 'meta) 'artifact #f)
                   "never: no artifact hash crosses the boundary")
      (check-false (string-contains? (hash-ref encoded 'text) "secret")
                   "never: thinking body is stripped from text"))

    (test-case "W3-P2: reasoning-visibility session keeps artifacts in memory only"
      (define artifact
        (make-conversation-artifact #:id "a"
                                    #:session-id "s"
                                    #:turn-id "t"
                                    #:kind 'thinking
                                    #:body "live reasoning"
                                    #:lifecycle 'retained
                                    #:persistence 'scrollback))
      (define entry (transcript-entry 'thinking "live reasoning" 0 (hasheq 'artifact artifact) 1))
      (define encoded
        (parameterize ([current-preferences
                        (set-preference (default-preferences) 'reasoning-visibility 'session)])
          (transcript-entry->jsexpr entry)))
      (check-false (hash-ref (hash-ref encoded 'meta) 'artifact #f)
                   "session: artifacts never serialize to scrollback")
      (check-false (string-contains? (hash-ref encoded 'text) "live reasoning")
                   "session: reasoning body stays in memory only")
      ;; The live in-memory entry is untouched.
      (check-true (conversation-artifact? (hash-ref (transcript-entry-meta entry) 'artifact))))

    (test-case "W3-P3: reasoning-visibility scrollback serializes full artifacts"
      (define artifact
        (make-conversation-artifact #:id "a"
                                    #:session-id "s"
                                    #:turn-id "t"
                                    #:kind 'thinking
                                    #:body "full reasoning"
                                    #:lifecycle 'retained
                                    #:persistence 'scrollback))
      (define entry (transcript-entry 'thinking "full reasoning" 0 (hasheq 'artifact artifact) 1))
      (define encoded
        (parameterize ([current-preferences
                        (set-preference (default-preferences) 'reasoning-visibility 'scrollback)])
          (transcript-entry->jsexpr entry)))
      (check-true (hash? (hash-ref (hash-ref encoded 'meta) 'artifact #f))
                  "scrollback: artifact hash crosses the boundary")
      (check-equal? (hash-ref encoded 'text) "full reasoning"))

    (test-case "W3-P4: policy does not affect non-reasoning artifacts"
      (define artifact
        (make-conversation-artifact #:id "a"
                                    #:session-id "s"
                                    #:turn-id "t"
                                    #:kind 'tool
                                    #:body "{\"op\":\"read\"}"
                                    #:lifecycle 'retained
                                    #:persistence 'scrollback))
      (define entry (transcript-entry 'tool "op read" 0 (hasheq 'artifact artifact) 1))
      (define encoded
        (parameterize ([current-preferences
                        (set-preference (default-preferences) 'reasoning-visibility 'never)])
          (transcript-entry->jsexpr entry)))
      (check-true (hash? (hash-ref (hash-ref encoded 'meta) 'artifact #f))
                  "never policy leaves non-thinking artifacts untouched"))))

(run-tests scrollback-roundtrip-tests)
