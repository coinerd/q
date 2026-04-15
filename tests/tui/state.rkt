#lang racket

;; tests/tui/state.rkt — Tests for tui/state module

(require rackunit
         rackunit/text-ui
         racket/port
         "../../../q/tui/state.rkt"
         "../../../q/tui/render.rkt"
         "../../../q/tui/scrollback.rkt"
         "../../../q/util/protocol-types.rkt")

;; Helper to make a simple event
(define (make-test-event ev-type
                         payload
                         #:time [time 1000]
                         #:session-id [session-id "test-session"]
                         #:turn-id [turn-id "turn-1"])
  (event 1 ev-type time session-id turn-id payload))

(define state-tests
  (test-suite "TUI State"

    ;; ============================================================
    ;; branch-info struct
    ;; ============================================================

    (test-case "branch-info: all fields populated"
      (let ([b (branch-info "id-1" "parent-1" 'user #t #f)])
        (check-equal? (branch-info-id b) "id-1")
        (check-equal? (branch-info-parent-id b) "parent-1")
        (check-equal? (branch-info-role b) 'user)
        (check-true (branch-info-leaf? b))
        (check-false (branch-info-active? b))))

    (test-case "branch-info: no parent, assistant role"
      (let ([b (branch-info "id-2" #f 'assistant #f #t)])
        (check-equal? (branch-info-id b) "id-2")
        (check-false (branch-info-parent-id b))
        (check-equal? (branch-info-role b) 'assistant)
        (check-false (branch-info-leaf? b))
        (check-true (branch-info-active? b))))

    ;; ============================================================
    ;; initial-ui-state
    ;; ============================================================

    (test-case "initial-ui-state: default values"
      (check-equal? (ui-state-transcript (initial-ui-state)) '() "initial: transcript empty")
      (check-equal? (ui-state-scroll-offset (initial-ui-state)) 0 "initial: scroll-offset 0")
      (check-false (ui-state-busy? (initial-ui-state)) "initial: not busy")
      (check-false (ui-state-session-id (initial-ui-state)) "initial: no session-id")
      (check-false (ui-state-model-name (initial-ui-state)) "initial: no model-name")
      (check-equal? (ui-state-mode (initial-ui-state)) 'chat "initial: mode is chat")
      (check-false (ui-state-status-message (initial-ui-state)) "initial: no status message")
      (check-false (ui-state-pending-tool-name (initial-ui-state)) "initial: no pending tool"))

    (test-case "initial-ui-state: keyword arguments"
      (check-equal? (ui-state-session-id (initial-ui-state #:session-id "s1"))
                    "s1"
                    "initial kwargs: session-id")
      (check-equal? (ui-state-model-name (initial-ui-state #:model-name "gpt-4"))
                    "gpt-4"
                    "initial kwargs: model-name")
      (check-equal? (ui-state-mode (initial-ui-state #:mode 'single)) 'single "initial kwargs: mode"))

    ;; ============================================================
    ;; apply-event-to-state
    ;; ============================================================

    (test-case "apply-event: assistant.message.completed"
      (let* ([s (initial-ui-state)]
             [evt (make-test-event "assistant.message.completed" (hash 'content "Hello!"))]
             [s2 (apply-event-to-state s evt)])
        (check-equal? (length (ui-state-transcript s2)) 1)
        (check-equal? (transcript-entry-kind (first (ui-state-transcript s2))) 'assistant)
        (check-equal? (transcript-entry-text (first (ui-state-transcript s2))) "Hello!")
        (check-false (ui-state-busy? s2))
        (check-false (ui-state-pending-tool-name s2))))

    (test-case "apply-event: tool.call.started"
      (let* ([s (initial-ui-state)]
             [evt (make-test-event "tool.call.started" (hash 'name "bash"))]
             [s2 (apply-event-to-state s evt)])
        (check-equal? (transcript-entry-kind (first (ui-state-transcript s2))) 'tool-start)
        (check-equal? (transcript-entry-text (first (ui-state-transcript s2))) "[TOOL: bash]")
        (check-true (ui-state-busy? s2))
        (check-equal? (ui-state-pending-tool-name s2) "bash")))

    (test-case "apply-event: tool.call.completed"
      (let* ([s (initial-ui-state)]
             [evt (make-test-event "tool.call.completed" (hash 'name "bash"))]
             [s2 (apply-event-to-state s evt)])
        (check-equal? (transcript-entry-kind (first (ui-state-transcript s2))) 'tool-end)
        (check-equal? (transcript-entry-text (first (ui-state-transcript s2))) "[OK: bash]")
        (check-false (ui-state-pending-tool-name s2))))

    (test-case "apply-event: tool.call.failed"
      (let* ([s (initial-ui-state)]
             [evt (make-test-event "tool.call.failed" (hash 'name "bash" 'error "exit code 1"))]
             [s2 (apply-event-to-state s evt)])
        (check-equal? (transcript-entry-kind (first (ui-state-transcript s2))) 'tool-fail)
        (check-equal? (transcript-entry-text (first (ui-state-transcript s2)))
                      "[FAIL: bash] exit code 1")
        (check-false (ui-state-pending-tool-name s2))))

    (test-case "apply-event: runtime.error"
      (let* ([s (initial-ui-state)]
             [evt (make-test-event "runtime.error" (hash 'error "something broke"))]
             [s2 (apply-event-to-state s evt)])
        (check-equal? (transcript-entry-kind (first (ui-state-transcript s2))) 'error)
        (check-equal? (transcript-entry-text (first (ui-state-transcript s2)))
                      "Error: something broke")
        (check-false (ui-state-busy? s2))))

    (test-case "apply-event: session.started"
      (let* ([s (initial-ui-state)]
             [evt (make-test-event "session.started" (hash 'sessionId "sess-123"))]
             [s2 (apply-event-to-state s evt)])
        (check-equal? (ui-state-session-id s2) "sess-123")
        (check-equal? (transcript-entry-text (first (ui-state-transcript s2)))
                      "Session started: sess-123")))

    (test-case "apply-event: session.resumed"
      (let* ([s (initial-ui-state)]
             [evt (make-test-event "session.resumed" (hash 'sessionId "sess-resume"))]
             [s2 (apply-event-to-state s evt)])
        (check-equal? (ui-state-session-id s2) "sess-resume")
        (check-equal? (transcript-entry-text (first (ui-state-transcript s2)))
                      "Session resumed: sess-resume")))

    (test-case "apply-event: turn.started"
      (let* ([s (initial-ui-state)]
             [evt (make-test-event "turn.started" (hash))]
             [s2 (apply-event-to-state s evt)])
        (check-true (ui-state-busy? s2))))

    (test-case "apply-event: turn.completed"
      (let* ([s (struct-copy ui-state (initial-ui-state) [busy? #t])]
             [evt (make-test-event "turn.completed" (hash))]
             [s2 (apply-event-to-state s evt)])
        (check-false (ui-state-busy? s2))))

    (test-case "turn.cancelled clears busy and streaming"
      (define s0
        (struct-copy ui-state
                     (initial-ui-state)
                     [busy? #t]
                     [streaming-text "partial..."]
                     [pending-tool-name "bash"]))
      (define evt (make-test-event "turn.cancelled" (hash)))
      (define s1 (apply-event-to-state s0 evt))
      (check-false (ui-state-busy? s1))
      (check-false (ui-state-streaming-text s1))
      (check-false (ui-state-pending-tool-name s1)))

    (test-case "apply-event: compaction.warning"
      (let* ([s (initial-ui-state)]
             [evt (make-test-event "compaction.warning" (hash 'tokenCount 50000))]
             [s2 (apply-event-to-state s evt)])
        (check-equal? (transcript-entry-text (first (ui-state-transcript s2)))
                      "[compaction warning: 50000 tokens]")))

    (test-case "apply-event: session.forked"
      (let* ([s (initial-ui-state)]
             [evt (make-test-event "session.forked" (hash 'newSessionId "fork-123"))]
             [s2 (apply-event-to-state s evt)])
        (check-equal? (transcript-entry-text (first (ui-state-transcript s2)))
                      "[session forked: fork-123]")))

    (test-case "apply-event: compaction.started"
      (let* ([s (initial-ui-state)]
             [evt (make-test-event "compaction.started" (hash))]
             [s2 (apply-event-to-state s evt)])
        (check-equal? (ui-state-status-message s2) "Compacting...")))

    (test-case "apply-event: compaction.completed"
      (let* ([s (struct-copy ui-state (initial-ui-state) [status-message "Compacting..."])]
             [evt (make-test-event "compaction.completed" (hash))]
             [s2 (apply-event-to-state s evt)])
        (check-false (ui-state-status-message s2))))

    (test-case "apply-event: unknown event returns unchanged state"
      (let* ([s (initial-ui-state)]
             [evt (make-test-event "something.weird" (hash 'foo "bar"))]
             [s2 (apply-event-to-state s evt)])
        (check-eq? s2 s "unknown event returns same state")))

    ;; ============================================================
    ;; add-transcript-entry
    ;; ============================================================

    (test-case "add-transcript-entry: appends entry and resets scroll"
      (let* ([s (struct-copy ui-state (initial-ui-state) [scroll-offset 5])]
             [entry (make-entry 'user "hello" 1000 (hash))]
             [s2 (add-transcript-entry s entry)])
        (check-equal? (length (ui-state-transcript s2)) 1)
        (check-equal? (transcript-entry-kind (first (ui-state-transcript s2))) 'user)
        (check-equal? (ui-state-scroll-offset s2) 0 "add-transcript-entry resets scroll")))

    ;; ============================================================
    ;; visible-entries
    ;; ============================================================

    (test-case "visible-entries: returns all entries (line slicing is renderer's job)"
      (let* ([entries (for/list ([i (in-range 10)])
                        (make-entry 'assistant (format "msg ~a" i) i (hash)))]
             [s (struct-copy ui-state (initial-ui-state) [transcript entries])]
             [vis (visible-entries s 5)])
        (check-equal? (length vis) 10 "visible-entries: returns all entries regardless of height")))

    (test-case "visible-entries: ignores scroll offset (line slicing is renderer's job)"
      (let* ([entries (for/list ([i (in-range 10)])
                        (make-entry 'assistant (format "msg ~a" i) i (hash)))]
             [s (struct-copy ui-state (initial-ui-state) [transcript entries] [scroll-offset 3])]
             [vis (visible-entries s 5)])
        (check-equal? (length vis) 10 "visible-entries: returns all entries regardless of scroll")))

    ;; ============================================================
    ;; scroll-up, scroll-down, scroll-to-bottom, scroll-to-top
    ;; ============================================================

    (test-case "scroll-up: increments offset"
      (let* ([entries (for/list ([i (in-range 10)])
                        (make-entry 'assistant (format "msg ~a" i) i (hash)))]
             [s (struct-copy ui-state (initial-ui-state) [transcript entries])]
             [s2 (scroll-up s)]
             [s3 (scroll-up s2 5)])
        (check-equal? (ui-state-scroll-offset s2) 1)
        (check-equal? (ui-state-scroll-offset s3) 6)))

    (test-case "scroll-down: decrements offset"
      (let* ([s (struct-copy ui-state (initial-ui-state) [scroll-offset 5])]
             [s2 (scroll-down s)]
             [s3 (scroll-down s2 10)])
        (check-equal? (ui-state-scroll-offset s2) 4)
        (check-equal? (ui-state-scroll-offset s3) 0)))

    (test-case "scroll-down: at zero stays zero"
      (let ([s (initial-ui-state)])
        (check-equal? (ui-state-scroll-offset (scroll-down s)) 0 "scroll-down at 0 stays 0")))

    (test-case "scroll-to-bottom: resets offset to zero"
      (let ([s (struct-copy ui-state (initial-ui-state) [scroll-offset 10])])
        (check-equal? (ui-state-scroll-offset (scroll-to-bottom s)) 0)))

    (test-case "scroll-to-top: sets large offset for renderer clamping"
      (let* ([entries (for/list ([i (in-range 10)])
                        (make-entry 'assistant (format "msg ~a" i) i (hash)))]
             [s (struct-copy ui-state (initial-ui-state) [transcript entries])]
             [s-top (scroll-to-top s)])
        (check-true (> (ui-state-scroll-offset s-top) 100)
                    "scroll-to-top sets large offset for multi-line entries")
        (check-true (> (ui-state-scroll-offset s-top) (length entries))
                    "scroll-to-top offset exceeds entry count")))

    ;; ============================================================
    ;; ui-busy?, ui-session-label, ui-model-label, ui-status-text
    ;; ============================================================

    (test-case "ui-busy?: reflects busy state"
      (check-false (ui-busy? (initial-ui-state)))
      (check-true (ui-busy? (struct-copy ui-state (initial-ui-state) [busy? #t]))))

    (test-case "ui-session-label: returns session id or default"
      (check-equal? (ui-session-label (initial-ui-state)) "no session")
      (check-equal? (ui-session-label (initial-ui-state #:session-id "abc")) "abc"))

    (test-case "ui-model-label: returns model name or default"
      (check-equal? (ui-model-label (initial-ui-state)) "no model")
      (check-equal? (ui-model-label (initial-ui-state #:model-name "gpt-4")) "gpt-4"))

    (test-case "ui-status-text: idle / busy / tool / status-message"
      (check-equal? (ui-status-text (initial-ui-state)) "idle")
      (check-equal? (ui-status-text (struct-copy ui-state (initial-ui-state) [busy? #t])) "busy...")
      (check-equal? (ui-status-text
                     (struct-copy ui-state (initial-ui-state) [busy? #t] [pending-tool-name "bash"]))
                    "busy (bash)...")
      (check-equal?
       (ui-status-text (struct-copy ui-state (initial-ui-state) [status-message "Compacting..."]))
       "Compacting..."))

    ;; ============================================================
    ;; Multiple events accumulate
    ;; ============================================================

    (test-case "multiple events accumulate in transcript"
      (let* ([s0 (initial-ui-state)]
             [s1 (apply-event-to-state s0 (make-test-event "session.started" (hash 'sessionId "s1")))]
             [s2 (apply-event-to-state s1 (make-test-event "turn.started" (hash)))]
             [s3 (apply-event-to-state s2 (make-test-event "tool.call.started" (hash 'name "bash")))]
             [s4 (apply-event-to-state s3
                                       (make-test-event "tool.call.completed" (hash 'name "bash")))]
             [s5 (apply-event-to-state s4
                                       (make-test-event "assistant.message.completed"
                                                        (hash 'content "done")))])
        (check-equal? (length (ui-state-transcript s5)) 4 "multi-event: transcript length")
        (check-false (ui-state-busy? s5) "multi-event: not busy at end")))

    ;; ============================================================
    ;; Streaming deltas (model.stream.delta)
    ;; ============================================================

    (test-case "streaming: accumulates deltas and marks busy"
      (let* ([s0 (initial-ui-state)]
             [s1 (apply-event-to-state s0
                                       (make-test-event "model.stream.delta" (hash 'delta "Hello")))]
             [s2 (apply-event-to-state s1
                                       (make-test-event "model.stream.delta"
                                                        (hash 'delta " world")))])
        (check-equal? (ui-state-streaming-text s2) "Hello world" "streaming: accumulates deltas")
        (check-true (ui-state-busy? s2) "streaming: marked busy")))

    (test-case "streaming to complete: clears streaming, uses text for transcript"
      (let* ([s0 (initial-ui-state)]
             [s1 (apply-event-to-state s0
                                       (make-test-event "model.stream.delta" (hash 'delta "Hello")))]
             [s2 (apply-event-to-state s1 (make-test-event "model.stream.delta" (hash 'delta "!")))]
             [s3 (apply-event-to-state s2
                                       (make-test-event "assistant.message.completed"
                                                        (hash 'content "ignored")))])
        (check-equal? (ui-state-streaming-text s3) #f "streaming→complete: streaming cleared")
        (check-equal? (transcript-entry-text (last (ui-state-transcript s3)))
                      "Hello!"
                      "streaming→complete: streamed text used for transcript")
        (check-false (ui-state-busy? s3) "streaming→complete: not busy")))

    (test-case "initial: no streaming text"
      (check-false (ui-state-streaming-text (initial-ui-state)) "initial: no streaming text"))

    ;; ============================================================
    ;; Branch-related initial state
    ;; ============================================================

    (test-case "initial: no current branch and empty visible branches"
      (check-false (ui-state-current-branch (initial-ui-state)) "initial: no current branch")
      (check-equal? (ui-state-visible-branches (initial-ui-state))
                    '()
                    "initial: empty visible branches"))

    ;; ============================================================
    ;; set-current-branch
    ;; ============================================================

    (test-case "set-current-branch: sets branch id"
      (let* ([s (initial-ui-state)]
             [s2 (set-current-branch s "branch-123")])
        (check-equal? (ui-state-current-branch s2)
                      "branch-123"
                      "set-current-branch: sets branch id")))

    (test-case "set-current-branch: overwrites existing"
      (let* ([s (struct-copy ui-state (initial-ui-state) [current-branch "old"])]
             [s2 (set-current-branch s "new")])
        (check-equal? (ui-state-current-branch s2) "new" "set-current-branch: overwrites existing")))

    ;; ============================================================
    ;; set-visible-branches / clear-visible-branches
    ;; ============================================================

    (test-case "set-visible-branches: sets branch list"
      (let* ([s (initial-ui-state)]
             [branches (list (branch-info "b1" #f 'user #t #t)
                             (branch-info "b2" "b1" 'assistant #t #f))]
             [s2 (set-visible-branches s branches)])
        (check-equal? (length (ui-state-visible-branches s2))
                      2
                      "set-visible-branches: sets branch list")
        (check-equal? (branch-info-id (first (ui-state-visible-branches s2))) "b1")))

    (test-case "clear-visible-branches: clears branch list"
      (let* ([s (initial-ui-state)]
             [branches (list (branch-info "b1" #f 'user #t #t))]
             [s2 (set-visible-branches s branches)]
             [s3 (clear-visible-branches s2)])
        (check-equal? (ui-state-visible-branches s3)
                      '()
                      "clear-visible-branches: clears branch list")))

    ;; ============================================================
    ;; Scrollback persistence (R5-1)
    ;; ============================================================

    (test-case "save-scrollback writes JSONL file"
      (let* ([tmpdir (make-temporary-file "scrollback-test-~a" 'directory)]
             [path (build-path tmpdir "scrollback.jsonl")]
             [entries (list (make-entry 'assistant "Hello" 1000 (hash 'foo "bar"))
                            (make-entry 'tool-start "[tool: bash]" 1001 (hash 'name "bash")))])
        (save-scrollback entries path)
        (check-true (file-exists? path) "save-scrollback creates file")
        (define content (file->string path))
        (check-true (string-contains? content "Hello") "file contains entry text")
        (check-true (string-contains? content "bash") "file contains second entry")
        ;; Should be 2 lines
        (define line-count (length (string-split content "\n" #:trim? #f)))
        (check-equal? line-count 3 "two JSONL lines + trailing newline")
        (delete-directory/files tmpdir)))

    (test-case "load-scrollback reads JSONL file"
      (let* ([tmpdir (make-temporary-file "scrollback-test-~a" 'directory)]
             [path (build-path tmpdir "scrollback.jsonl")]
             ;; Write known JSONL content
             [content (string-append
                       "{\"kind\":\"assistant\",\"text\":\"Hi\",\"timestamp\":42,\"meta\":{}}\n"
                       "{\"kind\":\"error\",\"text\":\"oops\",\"timestamp\":43,\"meta\":{}}\n")])
        (display-to-file content path #:exists 'replace)
        (define entries (load-scrollback path))
        (check-equal? (length entries) 2 "load-scrollback: two entries")
        (check-equal? (transcript-entry-kind (first entries)) 'assistant)
        (check-equal? (transcript-entry-text (first entries)) "Hi")
        (check-equal? (transcript-entry-timestamp (first entries)) 42)
        (check-equal? (transcript-entry-kind (second entries)) 'error)
        (delete-directory/files tmpdir)))

    (test-case "load-scrollback returns empty list for missing file"
      (let ([path "/tmp/q-scrollback-nonexistent-test.jsonl"])
        (when (file-exists? path)
          (delete-file path))
        (define entries (load-scrollback path))
        (check-equal? entries '() "load-scrollback: empty for missing file")))

    (test-case "roundtrip: save then load preserves events"
      (let* ([tmpdir (make-temporary-file "scrollback-test-~a" 'directory)]
             [path (build-path tmpdir "scrollback.jsonl")]
             [entries (list (make-entry 'assistant "Hello world" 1000 (hash))
                            (make-entry 'tool-start "[TOOL: bash]" 1001 (hash 'name "bash"))
                            (make-entry 'tool-end "[OK: bash]" 1002 (hash 'name "bash"))
                            (make-entry 'tool-fail
                                        "[FAIL: read] not found"
                                        1003
                                        (hash 'name "read" 'error "not found"))
                            (make-entry 'system "Session started" 1004 (hash))
                            (make-entry 'error "Error: boom" 1005 (hash))
                            (make-entry 'user "What is 2+2?" 1006 (hash)))])
        (save-scrollback entries path)
        (define loaded (load-scrollback path))
        (check-equal? (length loaded) 7 "roundtrip: same entry count")
        (for ([orig (in-list entries)]
              [loaded-entry (in-list loaded)])
          (check-equal? (transcript-entry-kind orig)
                        (transcript-entry-kind loaded-entry)
                        (format "roundtrip kind: ~a" (transcript-entry-kind orig)))
          (check-equal? (transcript-entry-text orig)
                        (transcript-entry-text loaded-entry)
                        (format "roundtrip text: ~a" (transcript-entry-text orig)))
          (check-equal? (transcript-entry-timestamp orig)
                        (transcript-entry-timestamp loaded-entry)
                        (format "roundtrip timestamp: ~a" (transcript-entry-timestamp orig)))
          (check-equal? (transcript-entry-meta orig)
                        (transcript-entry-meta loaded-entry)
                        (format "roundtrip meta: ~a" (transcript-entry-meta orig))))
        (delete-directory/files tmpdir)))

    ;; ============================================================
    ;; #126: Tool calls with arguments and result summary
    ;; ============================================================

    (test-case "apply-event: tool.call.started with arguments"
      (let* ([s (initial-ui-state)]
             [evt (make-test-event "tool.call.started"
                                   (hash 'name "bash" 'arguments "{\"command\":\"ls -la\"}"))]
             [s2 (apply-event-to-state s evt)])
        (define entry (first (ui-state-transcript s2)))
        (check-equal? (transcript-entry-kind entry) 'tool-start)
        (check-true (string-contains? (transcript-entry-text entry) "[TOOL: bash]"))
        (check-true (string-contains? (transcript-entry-text entry) "ls -la")
                    "arguments shown in text")
        (check-equal? (hash-ref (transcript-entry-meta entry) 'name) "bash")
        (check-true (string-contains? (hash-ref (transcript-entry-meta entry) 'arguments) "ls -la")
                    "arguments stored in meta")))

    (test-case "apply-event: tool.call.completed with result"
      (let* ([s (initial-ui-state)]
             [evt (make-test-event "tool.call.completed" (hash 'name "bash" 'result "3 files found"))]
             [s2 (apply-event-to-state s evt)])
        (define entry (first (ui-state-transcript s2)))
        (check-equal? (transcript-entry-kind entry) 'tool-end)
        (check-true (string-contains? (transcript-entry-text entry) "[OK: bash]"))
        (check-true (string-contains? (transcript-entry-text entry) "3 files found")
                    "result shown in text")
        (check-equal? (hash-ref (transcript-entry-meta entry) 'name) "bash")
        (check-true (string-contains? (hash-ref (transcript-entry-meta entry) 'result)
                                      "3 files found")
                    "result stored in meta")))

    (test-case "apply-event: tool.call.failed with error"
      (let* ([s (initial-ui-state)]
             [evt (make-test-event "tool.call.failed" (hash 'name "bash" 'error "exit 1"))]
             [s2 (apply-event-to-state s evt)])
        (define entry (first (ui-state-transcript s2)))
        (check-equal? (transcript-entry-kind entry) 'tool-fail)
        (check-true (string-contains? (transcript-entry-text entry) "[FAIL: bash]"))
        (check-true (string-contains? (transcript-entry-text entry) "exit 1") "error shown in text")))

    (test-case "apply-event: tool.call.started without arguments"
      (let* ([s (initial-ui-state)]
             [evt (make-test-event "tool.call.started" (hash 'name "read"))]
             [s2 (apply-event-to-state s evt)])
        (define entry (first (ui-state-transcript s2)))
        (check-equal? (transcript-entry-text entry)
                      "[TOOL: read]"
                      "no arguments: just shows tool name")))

    (test-case "apply-event: tool.call.completed without result"
      (let* ([s (initial-ui-state)]
             [evt (make-test-event "tool.call.completed" (hash 'name "read"))]
             [s2 (apply-event-to-state s evt)])
        (define entry (first (ui-state-transcript s2)))
        (check-equal? (transcript-entry-text entry) "[OK: read]" "no result: just shows tool name")))

    ;; ============================================================
    ;; String helpers: truncate-string, extract-arg-summary
    ;; ============================================================

    (test-case "truncate-string: short string unchanged"
      (check-equal? (truncate-string "hello" 10) "hello"))

    (test-case "truncate-string: long string truncated with ellipsis"
      (check-equal? (truncate-string "hello world" 6) "hello…"))

    (test-case "truncate-string: exact length unchanged"
      (check-equal? (truncate-string "hello" 5) "hello"))

    (test-case "extract-arg-summary: JSON object shows first value"
      (check-true (string-contains? (extract-arg-summary "{\"command\":\"ls -la\"}") "ls -la")))

    (test-case "extract-arg-summary: empty JSON object returns (no args)"
      (check-equal? (extract-arg-summary "{}") "(no args)"))

    (test-case "extract-arg-summary: invalid JSON falls back to truncation"
      (check-true (string-contains? (extract-arg-summary "not json at all") "not json")))

    (test-case "extract-arg-summary: long value is truncated"
      (define long-val (make-string 100 #\x))
      (define result (extract-arg-summary (format "{\"cmd\":\"~a\"}" long-val)))
      (check-true (<= (string-length result) 60) "long value truncated to 60 chars"))))

(run-tests state-tests)

;; --------------------------------------------------------
;; Selection state helpers
;; --------------------------------------------------------

(let ()
  (define state (initial-ui-state))
  (check-false (ui-state-sel-anchor state) "initial-ui-state: sel-anchor is #f")
  (check-false (ui-state-sel-end state) "initial-ui-state: sel-end is #f")
  (check-false (has-selection? state) "has-selection? returns #f initially"))

(let ()
  (define state (initial-ui-state))
  (define next (set-selection-anchor state 5 10))
  (check-equal? (ui-state-sel-anchor next) '(5 . 10) "set-selection-anchor sets anchor")
  (check-equal? (ui-state-sel-end next) '(5 . 10) "set-selection-anchor also sets end"))

(let ()
  (define state (set-selection-anchor (initial-ui-state) 5 10))
  (define next (set-selection-end state 20 30))
  (check-equal? (ui-state-sel-end next) '(20 . 30) "set-selection-end updates end")
  (check-equal? (ui-state-sel-anchor next) '(5 . 10) "set-selection-end preserves anchor"))

(let ()
  (define state (set-selection-end (set-selection-anchor (initial-ui-state) 5 10) 20 30))
  (check-not-false (has-selection? state) "has-selection? returns truthy when both are set"))

(let ()
  (define state
    (clear-selection (set-selection-end (set-selection-anchor (initial-ui-state) 5 10) 20 30)))
  (check-false (ui-state-sel-anchor state) "clear-selection clears anchor")
  (check-false (ui-state-sel-end state) "clear-selection clears end")
  (check-false (has-selection? state) "has-selection? returns #f after clear"))

(let ()
  ;; Scrolling clears selection (Bug #748: selection highlight stayed at fixed rows)
  (define s (set-selection-anchor (initial-ui-state) 5 10))
  (define scrolled (scroll-up s))
  (check-false (has-selection? scrolled) "scroll-up clears selection")
  (check-false (ui-state-sel-anchor scrolled) "scroll-up clears sel-anchor")
  (check-false (ui-state-sel-end scrolled) "scroll-up clears sel-end"))

(let ()
  ;; scroll-down also clears selection
  (define s (set-selection-end (set-selection-anchor (initial-ui-state) 5 10) 20 30))
  (define scrolled (scroll-down s))
  (check-false (has-selection? scrolled) "scroll-down clears selection")
  (check-false (ui-state-sel-anchor scrolled) "scroll-down clears sel-anchor"))

(let ()
  ;; scroll-up preserves offset when no selection
  (define s (scroll-up (initial-ui-state) 3))
  (check-equal? (ui-state-scroll-offset s) 3 "scroll-up offset preserved without selection"))

(let ()
  ;; Selection helpers preserve other state fields
  (define state
    (add-transcript-entry (initial-ui-state #:session-id "test")
                          (make-entry 'assistant "hello" 0 (hash))))
  (define state+sel (set-selection-anchor state 5 10))
  (check-equal? (ui-state-session-id state+sel) "test" "set-selection-anchor preserves session-id")
  (check-equal? (ui-state-scroll-offset state+sel) 0 "set-selection-anchor preserves scroll-offset"))

(test-case "model.request.started marks busy"
  (define s0 (struct-copy ui-state (initial-ui-state) [busy? #f]))
  (define evt (make-test-event "model.request.started" (hash)))
  (define s1 (apply-event-to-state s0 evt))
  (check-true (ui-state-busy? s1)))

(test-case "tool.call.blocked shows in transcript"
  (define s0 (struct-copy ui-state (initial-ui-state) [pending-tool-name "bash"]))
  (define evt (make-test-event "tool.call.blocked" (hash 'name "bash" 'reason "security policy")))
  (define s1 (apply-event-to-state s0 evt))
  (check-false (ui-state-pending-tool-name s1))
  (define last-entry (last (ui-state-transcript s1)))
  (check-equal? (transcript-entry-kind last-entry) 'system)
  (check-not-false (string-contains? (transcript-entry-text last-entry) "tool blocked")))

(test-case "context.built is passthrough"
  (define s0 (struct-copy ui-state (initial-ui-state) [busy? #t] [session-id "s1"]))
  (define evt (make-test-event "context.built" (hash 'tokenCount 42)))
  (define s1 (apply-event-to-state s0 evt))
  (check-eq? s1 s0 "context.built returns state unchanged"))

;; ============================================================
;; W4-2: Additional event handler edge cases
;; ============================================================

(test-case "model.stream.delta accumulates text"
  (define s0 (struct-copy ui-state (initial-ui-state) [streaming-text "Hello"]))
  (define evt (make-test-event "model.stream.delta" (hash 'delta " world")))
  (define s1 (apply-event-to-state s0 evt))
  (check-equal? (ui-state-streaming-text s1) "Hello world")
  (check-true (ui-state-busy? s1)))

(test-case "multiple events maintain transcript order"
  (define s0 (initial-ui-state))
  (define s1 (apply-event-to-state s0 (make-test-event "turn.started" (hash))))
  (define s2 (apply-event-to-state s1 (make-test-event "model.stream.delta" (hash 'delta "Hi"))))
  (define s3
    (apply-event-to-state s2 (make-test-event "assistant.message.completed" (hash 'content "Hi"))))
  (check-false (ui-state-busy? s3))
  (define transcript (ui-state-transcript s3))
  (check-equal? (length transcript) 1)
  (check-equal? (transcript-entry-kind (first transcript)) 'assistant))

(test-case "turn.started marks busy"
  (define s0 (initial-ui-state))
  (define evt (make-test-event "turn.started" (hash)))
  (define s1 (apply-event-to-state s0 evt))
  (check-true (ui-state-busy? s1)))

;; ============================================================
;; Overlay state tests (#643)
;; ============================================================

(test-case "overlay-state: initial state has no overlay"
  (define s0 (initial-ui-state))
  (check-false (ui-state-active-overlay s0))
  (check-false (overlay-active? s0)))

(test-case "overlay-state: show-overlay activates overlay"
  (define s0 (initial-ui-state))
  (define content (list (styled-line (list (styled-segment "test" '())))))
  (define s1 (show-overlay s0 'command-palette content "/h"))
  (check-true (overlay-active? s1))
  (check-equal? (overlay-state-type (ui-state-active-overlay s1)) 'command-palette)
  (check-equal? (overlay-state-input (ui-state-active-overlay s1)) "/h")
  (check-equal? (length (overlay-state-content (ui-state-active-overlay s1))) 1))

(test-case "overlay-state: dismiss-overlay clears overlay"
  (define s0 (initial-ui-state))
  (define content (list (styled-line (list (styled-segment "test" '())))))
  (define s1 (show-overlay s0 'command-palette content "/"))
  (check-true (overlay-active? s1))
  (define s2 (dismiss-overlay s1))
  (check-false (overlay-active? s2))
  (check-false (ui-state-active-overlay s2)))

(test-case "overlay-state: update-overlay-input changes input"
  (define s0 (initial-ui-state))
  (define content (list (styled-line (list (styled-segment "test" '())))))
  (define s1 (show-overlay s0 'command-palette content "/"))
  (define s2 (update-overlay-input s1 "/he"))
  (check-equal? (overlay-state-input (ui-state-active-overlay s2)) "/he"))

(test-case "overlay-state: update-overlay-input on no overlay is no-op"
  (define s0 (initial-ui-state))
  (define s1 (update-overlay-input s0 "/he"))
  (check-false (overlay-active? s1)))

(test-case "overlay-state: show then dismiss returns to normal"
  (define s0 (initial-ui-state))
  (define content (list (styled-line (list (styled-segment "test" '())))))
  (define s1 (show-overlay s0 'command-palette content "/help"))
  (check-true (overlay-active? s1))
  (define s2 (dismiss-overlay s1))
  (check-false (overlay-active? s2))
  ;; Transcript and other state preserved
  (check-equal? (ui-state-transcript s2) '())
  (check-equal? (ui-state-scroll-offset s2) 0))
