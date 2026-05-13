#lang racket

;; BOUNDARY: integration

;; tests/test-working-set.rkt — Unit tests for runtime/working-set.rkt

(require rackunit
         rackunit/text-ui
         racket/list
         "../runtime/working-set.rkt")

;; Simple mock message struct for testing
(struct mock-msg (id text) #:transparent)
(define (mock-id m)
  (mock-msg-id m))
(define (mock-tokens m)
  (string-length (mock-msg-text m)))

;; Helper to make a tool-call hash
(define (make-tool name [path ""])
  (hasheq 'name name 'arguments (hasheq 'path path)))

(define working-set-tests
  (test-suite "Working Set Tests"

    ;; ── T01: Empty working set ──
    (test-case "T01: empty working set has zero entries and zero tokens"
      (define ws (make-working-set))
      (check-equal? (working-set-entry-count ws) 0)
      (check-equal? (working-set-token-count ws) 0)
      (check-equal? (length (working-set-entries ws)) 0))

    ;; ── T02: read adds entry ──
    (test-case "T02: read tool call adds entry with correct path and message id"
      (define ws (make-working-set))
      (define tc (list (make-tool "read" "/tmp/foo.rkt")))
      (define rm (list (mock-msg "m1" "content of foo")))
      (working-set-update! ws tc rm mock-id mock-tokens)
      (check-equal? (working-set-entry-count ws) 1)
      (define e (car (working-set-entries ws)))
      (check-equal? (ws-entry-path e) "/tmp/foo.rkt")
      (check-equal? (ws-entry-message-id e) "m1")
      (check-equal? (ws-entry-token-estimate e) 14))

    ;; ── T03: second read refreshes ──
    (test-case "T03: second read of same path refreshes entry"
      (define ws (make-working-set))
      (working-set-update! ws
                           (list (make-tool "read" "/tmp/foo.rkt"))
                           (list (mock-msg "m1" "first read"))
                           mock-id
                           mock-tokens)
      (define old-ts (ws-entry-timestamp (car (working-set-entries ws))))
      (sleep 1)
      (working-set-update! ws
                           (list (make-tool "read" "/tmp/foo.rkt"))
                           (list (mock-msg "m2" "second read longer"))
                           mock-id
                           mock-tokens)
      (check-equal? (working-set-entry-count ws) 1)
      (define e (car (working-set-entries ws)))
      (check-equal? (ws-entry-message-id e) "m2")
      (check-equal? (ws-entry-token-estimate e) 18)
      (check-true (> (ws-entry-timestamp e) old-ts)))

    ;; ── T04: edit removes entry ──
    (test-case "T04: edit tool call removes entry for matching path"
      (define ws (make-working-set))
      (working-set-update! ws
                           (list (make-tool "read" "/tmp/foo.rkt"))
                           (list (mock-msg "m1" "content"))
                           mock-id
                           mock-tokens)
      (check-equal? (working-set-entry-count ws) 1)
      (working-set-update! ws
                           (list (make-tool "edit" "/tmp/foo.rkt"))
                           (list (mock-msg "m2" "edit result"))
                           mock-id
                           mock-tokens)
      (check-equal? (working-set-entry-count ws) 0))

    ;; ── T05: write removes entry ──
    (test-case "T05: write tool call removes entry for matching path"
      (define ws (make-working-set))
      (working-set-update! ws
                           (list (make-tool "read" "/tmp/foo.rkt"))
                           (list (mock-msg "m1" "content"))
                           mock-id
                           mock-tokens)
      (check-equal? (working-set-entry-count ws) 1)
      (working-set-update! ws
                           (list (make-tool "write" "/tmp/foo.rkt"))
                           (list (mock-msg "m2" "written"))
                           mock-id
                           mock-tokens)
      (check-equal? (working-set-entry-count ws) 0))

    ;; ── T06: bash does not modify ──
    (test-case "T06: bash tool call does not modify working set"
      (define ws (make-working-set))
      (working-set-update! ws
                           (list (make-tool "read" "/tmp/foo.rkt"))
                           (list (mock-msg "m1" "content"))
                           mock-id
                           mock-tokens)
      (working-set-update! ws
                           (list (make-tool "bash" ""))
                           (list (mock-msg "m2" "ls -la"))
                           mock-id
                           mock-tokens)
      (check-equal? (working-set-entry-count ws) 1))

    ;; ── T07: max-entries eviction ──
    (test-case "T07: max-entries limit evicts oldest"
      (define ws (make-working-set #:max-entries 3))
      (for ([i (in-range 5)])
        (working-set-update! ws
                             (list (make-tool "read" (format "/tmp/f~a.rkt" i)))
                             (list (mock-msg (format "m~a" i) "x"))
                             mock-id
                             mock-tokens))
      (check-equal? (working-set-entry-count ws) 3)
      (define paths (map ws-entry-path (working-set-entries ws)))
      ;; Newest 3 should be f4, f3, f2
      (check-equal? paths '("/tmp/f4.rkt" "/tmp/f3.rkt" "/tmp/f2.rkt")))

    ;; ── T08: max-tokens eviction ──
    (test-case "T08: max-tokens limit evicts LRU when budget exceeded"
      (define ws (make-working-set #:max-entries 10 #:max-tokens 25))
      (for ([i (in-range 3)])
        (working-set-update! ws
                             (list (make-tool "read" (format "/tmp/f~a.rkt" i)))
                             (list (mock-msg (format "m~a" i) (make-string 10 #\a)))
                             mock-id
                             mock-tokens))
      ;; Each entry = 10 tokens, 3 entries = 30 tokens > 25 budget
      (check-equal? (working-set-entry-count ws) 2)
      (check-equal? (working-set-token-count ws) 20))

    ;; ── T09: reset clears all ──
    (test-case "T09: working-set-reset! clears all entries"
      (define ws (make-working-set))
      (working-set-update! ws
                           (list (make-tool "read" "/tmp/foo.rkt"))
                           (list (mock-msg "m1" "content"))
                           mock-id
                           mock-tokens)
      (check-equal? (working-set-entry-count ws) 1)
      (working-set-reset! ws)
      (check-equal? (working-set-entry-count ws) 0)
      (check-equal? (working-set-token-count ws) 0))

    ;; ── T10: resolve messages ──
    (test-case "T10: working-set-resolve-messages returns correct structs"
      (define ws (make-working-set))
      (working-set-update! ws
                           (list (make-tool "read" "/tmp/foo.rkt"))
                           (list (mock-msg "m1" "content foo"))
                           mock-id
                           mock-tokens)
      (working-set-update! ws
                           (list (make-tool "read" "/tmp/bar.rkt"))
                           (list (mock-msg "m2" "content bar"))
                           mock-id
                           mock-tokens)
      (define all-msgs
        (list (mock-msg "m0" "system") (mock-msg "m1" "content foo") (mock-msg "m2" "content bar")))
      (define resolved (working-set-resolve-messages ws all-msgs mock-id))
      (check-equal? (length resolved) 2)
      ;; Order: newest first (bar, then foo)
      (check-equal? (mock-msg-id (car resolved)) "m2")
      (check-equal? (mock-msg-id (cadr resolved)) "m1"))

    ;; ── T11: resolve skips missing ──
    (test-case "T11: working-set-resolve-messages skips missing entries"
      (define ws (make-working-set))
      (working-set-update! ws
                           (list (make-tool "read" "/tmp/foo.rkt"))
                           (list (mock-msg "m1" "content"))
                           mock-id
                           mock-tokens)
      (define all-msgs (list (mock-msg "m0" "system")))
      (define resolved (working-set-resolve-messages ws all-msgs mock-id))
      (check-equal? (length resolved) 0))

    ;; ── T12: mixed sequence ──
    (test-case "T12: mixed read/edit/write sequence produces correct state"
      (define ws (make-working-set))
      ;; Read foo, bar, baz
      (for ([p '("/tmp/foo.rkt" "/tmp/bar.rkt" "/tmp/baz.rkt")]
            [id '("m1" "m2" "m3")])
        (working-set-update! ws
                             (list (make-tool "read" p))
                             (list (mock-msg id "content"))
                             mock-id
                             mock-tokens))
      (check-equal? (working-set-entry-count ws) 3)
      ;; Edit foo → removed
      (working-set-update! ws
                           (list (make-tool "edit" "/tmp/foo.rkt"))
                           (list (mock-msg "m4" "done"))
                           mock-id
                           mock-tokens)
      (check-equal? (working-set-entry-count ws) 2)
      (define paths (map ws-entry-path (working-set-entries ws)))
      (check-not-false (member "/tmp/bar.rkt" paths))
      (check-not-false (member "/tmp/baz.rkt" paths))
      (check-false (member "/tmp/foo.rkt" paths))
      ;; Write bar → removed
      (working-set-update! ws
                           (list (make-tool "write" "/tmp/bar.rkt"))
                           (list (mock-msg "m5" "done"))
                           mock-id
                           mock-tokens)
      (check-equal? (working-set-entry-count ws) 1)
      ;; Read foo again → added back
      (working-set-update! ws
                           (list (make-tool "read" "/tmp/foo.rkt"))
                           (list (mock-msg "m6" "new content"))
                           mock-id
                           mock-tokens)
      (check-equal? (working-set-entry-count ws) 2))

    ;; ── T13: stability ──
    (test-case "T13: working set survives multiple updates without corruption"
      (define ws (make-working-set #:max-entries 5))
      (for ([_ (in-range 50)])
        (define action (random 4))
        (define path (format "/tmp/f~a.rkt" (random 10)))
        (case action
          [(0) ;; read
           (working-set-update! ws
                                (list (make-tool "read" path))
                                (list (mock-msg (format "m~a" (random 1000)) "x"))
                                mock-id
                                mock-tokens)]
          [(1 2) ;; edit or write
           (working-set-update! ws
                                (list (make-tool (if (= action 1) "edit" "write") path))
                                (list (mock-msg "mx" "done"))
                                mock-id
                                mock-tokens)]
          [(3) ;; bash
           (working-set-update! ws
                                (list (make-tool "bash" ""))
                                (list (mock-msg "my" "ls"))
                                mock-id
                                mock-tokens)]))
      ;; After 50 random ops, ws should still be valid
      (check-true (>= (working-set-entry-count ws) 0))
      (check-true (>= (working-set-token-count ws) 0))
      (check-true (<= (working-set-entry-count ws) 5)))))

(run-tests working-set-tests)
