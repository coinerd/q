#lang racket

;;; tests/test-replay.rkt — property-based replay test harness
;;;
;;; Property-based tests for session replay functionality.
;;; Covers the four key properties from TEST_STRATEGY.md §6:
;;;   1. replay(log) reconstructs same visible tree
;;;   2. append-only invariant (never rewrites prior entries)
;;;   3. fork produces valid branch
;;;   4. compaction preserves queryable history
;;;
;;; All tests are deterministic (fixed RNG seeds for reproducibility).

(require rackunit
         rackunit/text-ui
         racket/file
         racket/random
         racket/list
         racket/string
         racket/hash
         "../util/protocol-types.rkt"
         "../runtime/session-store.rkt"
         "../runtime/agent-session.rkt"
         "../runtime/session-index.rkt"
         "../runtime/compactor.rkt"
         (only-in "../runtime/token-compaction.rkt" token-compaction-config)
         "../agent/event-bus.rkt"
         "../llm/model.rkt"
         "../llm/provider.rkt"
         (only-in "../tools/tool.rkt" make-tool-registry))

;; ============================================================
;; Deterministic Random Generation (for reproducible tests)
;; ============================================================

;; Fixed seeds for deterministic test generation
(define SEED-1 12345)
(define SEED-2 54321)
(define SEED-3 99999)
(define SEED-4 11111)

;; Make a pseudo-random generator with fixed seed
(define (make-seeded-rng seed)
  (vector->pseudo-random-generator (vector seed seed seed seed seed seed)))

;; Random element from list
(define (random-choice rng lst)
  (list-ref lst (random (length lst) rng)))

;; Random integer in range [min, max)
(define (random-range rng min max)
  (define range (- max min))
  (if (> range 0)
      (+ min (random range rng))
      min))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-dir)
  (make-temporary-file "q-replay-test-~a" 'directory))

(define (session-path dir)
  (build-path dir "session.jsonl"))

(define (make-test-message id parent-id role kind [text "hello"] [timestamp #f])
  (make-message id
                parent-id
                role
                kind
                (list (make-text-part text))
                (or timestamp (current-seconds))
                (hasheq)))

;; Build a visible tree representation from messages
;; Returns a hash of id -> (list children-ids) representing the tree structure
(define (build-visible-tree messages)
  (define tree (make-hash))
  ;; Initialize all nodes
  (for ([msg (in-list messages)])
    (hash-set! tree (message-id msg) '()))
  ;; Build parent->children relationships
  (for ([msg (in-list messages)])
    (define pid (message-parent-id msg))
    (when pid
      (hash-update! tree pid (lambda (children) (append children (list (message-id msg)))) '())))
  tree)

;; Check if two trees are equivalent
(define (trees-equivalent? tree1 tree2)
  (define keys1 (sort (hash-keys tree1) string<?))
  (define keys2 (sort (hash-keys tree2) string<?))
  (and (= (length keys1) (length keys2))
       (for/and ([k (in-list keys1)])
         (equal? (hash-ref tree1 k '()) (hash-ref tree2 k '())))))

;; Extract all entry IDs in order from a session log file
(define (extract-entry-ids path)
  (if (file-exists? path)
      (let ([entries (load-session-log path)]) (map message-id entries))
      '()))

;; Count total entries in session log
(define (count-entries path)
  (length (load-session-log path)))

;; Get raw file content for comparison
(define (get-file-content path)
  (if (file-exists? path)
      (file->string path)
      ""))

;; Mock provider for testing
(define (make-mock-provider-for-replay response-text)
  (make-provider
   (lambda () "mock")
   (lambda () (hash 'streaming #t 'token-counting #t))
   (lambda (req)
     (make-model-response (list (hash 'type "text" 'text response-text)) (hash) "mock" 'stop))
   (lambda (req) (list (stream-chunk response-text #f #f #t)))))

;; ============================================================
;; Session Tree Generator (deterministic)
;; ============================================================

;; Generate a random session tree structure
;; Returns a list of messages with parent-child relationships forming a tree
(define (generate-session-tree rng [max-depth 4] [branching-factor 3])
  (define messages '())
  (define counter 0)

  (define (next-id)
    (set! counter (add1 counter))
    (format "msg-~a" counter))

  ;; Generate a branch starting from parent-id at given depth
  (define (gen-branch parent-id depth)
    (when (< depth max-depth)
      (define num-children (random-range rng 1 (add1 branching-factor)))
      (for ([i (in-range num-children)])
        (define id (next-id))
        (define role (random-choice rng '(user assistant tool system)))
        (define kind (random-choice rng '(message tool-result compaction-summary)))
        (set! counter (add1 counter))
        (define msg
          (make-test-message id
                             parent-id
                             role
                             kind
                             (format "Message ~a at depth ~a" id depth)
                             (+ 1000000 counter)))
        (set! messages (cons msg messages))
        ;; Recursively generate children for this node
        (gen-branch id (add1 depth)))))

  ;; Start with root node
  (define root-id (next-id))
  (define root (make-test-message root-id #f 'system 'message "Root" 1000000))
  (set! messages (cons root messages))

  ;; Generate tree from root
  (gen-branch root-id 0)

  ;; Return in chronological order (reverse because we cons'd)
  (reverse messages))

;; Generate a linear chain (no forks)
(define (generate-linear-chain rng length)
  (define messages '())
  (define parent-id #f)
  (for ([i (in-range length)])
    (define id (format "chain-~a" i))
    (define role (if (even? i) 'user 'assistant))
    (define msg
      (make-test-message id parent-id role 'message (format "Chain message ~a" i) (+ 1000000 i)))
    (set! messages (cons msg messages))
    (set! parent-id id))
  (reverse messages))

;; Generate a session with intentional forks (multiple children per parent)
(define (generate-forked-session rng num-forks messages-per-fork)
  (define messages '())
  (define counter 0)

  (define (next-id)
    (set! counter (add1 counter))
    (format "fork-msg-~a" counter))

  ;; Root
  (define root-id (next-id))
  (define root (make-test-message root-id #f 'system 'message "Root" 1000000))
  (set! messages (cons root messages))

  ;; Create fork points
  (define fork-points (list root-id))

  (for ([f (in-range num-forks)])
    ;; Pick a random existing node as fork point
    (define fork-parent (random-choice rng fork-points))
    ;; Add multiple children to create a fork
    (define new-children '())
    (for ([i (in-range messages-per-fork)])
      (define id (next-id))
      (set! counter (add1 counter))
      (define role (random-choice rng '(user assistant)))
      (define msg
        (make-test-message id
                           fork-parent
                           role
                           'message
                           (format "Fork ~a branch ~a" f i)
                           (+ 1000000 counter)))
      (set! new-children (cons id new-children))
      (set! messages (cons msg messages)))
    ;; Add new children as potential future fork points
    (set! fork-points (append fork-points new-children)))

  (reverse messages))

;; ============================================================
;; Property 1: replay(log) reconstructs same visible tree
;; ============================================================

(define-test-suite test-property-replay-reconstructs-tree
                   (test-case "PROPERTY: replay preserves empty session tree"
                     (define rng (make-seeded-rng SEED-1))
                     (define dir (make-temp-dir))
                     (define path (session-path dir))

                     ;; Empty session
                     (define original-tree (build-visible-tree '()))
                     (define replayed (replay-session path))
                     (define replayed-tree (build-visible-tree replayed))

                     (check-true (trees-equivalent? original-tree replayed-tree)
                                 "Empty session tree should be preserved after replay")
                     (delete-directory/files dir #:must-exist? #f))
                   (test-case "PROPERTY: replay preserves linear chain tree structure"
                     (define rng (make-seeded-rng SEED-1))
                     (define dir (make-temp-dir))
                     (define path (session-path dir))

                     ;; Generate deterministic linear chain
                     (define messages (generate-linear-chain rng 10))
                     (append-entries! path messages)

                     ;; Build original tree
                     (define original-tree (build-visible-tree messages))

                     ;; Replay and rebuild tree
                     (define replayed (replay-session path))
                     (define replayed-tree (build-visible-tree replayed))

                     (check-true (trees-equivalent? original-tree replayed-tree)
                                 "Linear chain tree should be preserved after replay")
                     (check-equal? (length replayed) (length messages))
                     (delete-directory/files dir #:must-exist? #f))
                   (test-case "PROPERTY: replay preserves forked session tree structure"
                     (define rng (make-seeded-rng SEED-2))
                     (define dir (make-temp-dir))
                     (define path (session-path dir))

                     ;; Generate deterministic forked session
                     (define messages (generate-forked-session rng 5 3))
                     (append-entries! path messages)

                     ;; Build original tree
                     (define original-tree (build-visible-tree messages))

                     ;; Replay and rebuild tree
                     (define replayed (replay-session path))
                     (define replayed-tree (build-visible-tree replayed))

                     (check-true (trees-equivalent? original-tree replayed-tree)
                                 "Forked session tree should be preserved after replay")
                     (check-equal? (length replayed) (length messages))
                     (delete-directory/files dir #:must-exist? #f))
                   (test-case "PROPERTY: replay preserves complex random tree structure"
                     (define rng (make-seeded-rng SEED-3))
                     (define dir (make-temp-dir))
                     (define path (session-path dir))

                     ;; Generate deterministic complex tree
                     (define messages (generate-session-tree rng 5 3))
                     (append-entries! path messages)

                     ;; Build original tree
                     (define original-tree (build-visible-tree messages))

                     ;; Replay and rebuild tree
                     (define replayed (replay-session path))
                     (define replayed-tree (build-visible-tree replayed))

                     (check-true (trees-equivalent? original-tree replayed-tree)
                                 "Complex session tree should be preserved after replay")
                     (check-equal? (length replayed) (length messages))
                     (delete-directory/files dir #:must-exist? #f))
                   (test-case "PROPERTY: multiple replays produce identical results"
                     (define rng (make-seeded-rng SEED-4))
                     (define dir (make-temp-dir))
                     (define path (session-path dir))

                     ;; Generate and write session
                     (define messages (generate-session-tree rng 4 2))
                     (append-entries! path messages)

                     ;; Multiple replays should produce identical results
                     (define replay1 (replay-session path))
                     (define replay2 (replay-session path))
                     (define replay3 (load-session-log path))

                     (check-equal? (map message-id replay1) (map message-id replay2))
                     (check-equal? (map message-id replay2) (map message-id replay3))
                     (delete-directory/files dir #:must-exist? #f))
                   (test-case "PROPERTY: replay preserves parent-child relationships"
                     (define rng (make-seeded-rng SEED-1))
                     (define dir (make-temp-dir))
                     (define path (session-path dir))

                     ;; Generate session with known parent relationships
                     (define messages (generate-forked-session rng 3 2))
                     (append-entries! path messages)

                     (define replayed (replay-session path))

                     ;; Check each message preserves its parent
                     (for ([orig (in-list messages)]
                           [repl (in-list replayed)])
                       (check-equal? (message-id repl) (message-id orig))
                       (check-equal? (message-parent-id repl) (message-parent-id orig)))
                     (delete-directory/files dir #:must-exist? #f)))

;; ============================================================
;; Property 2: append-only invariant (never rewrites prior entries)
;; ============================================================

(define-test-suite
 test-property-append-only-invariant
 (test-case "PROPERTY: append-entry! only adds entries, never modifies existing"
   (define dir (make-temp-dir))
   (define path (session-path dir))

   ;; Initial entries with deterministic IDs
   (define msgs-1
     (list (make-test-message "append-test-0" #f 'user 'message "First" 1000)
           (make-test-message "append-test-1" "append-test-0" 'assistant 'message "Second" 1001)
           (make-test-message "append-test-2" "append-test-1" 'user 'message "Third" 1002)))
   (append-entries! path msgs-1)

   ;; Capture state after first write
   (define content-1 (get-file-content path))
   (define ids-1 (extract-entry-ids path))

   ;; Append more entries
   (define msgs-2
     (list (make-test-message "append-test-3" "append-test-2" 'assistant 'message "Fourth" 1003)
           (make-test-message "append-test-4" "append-test-3" 'user 'message "Fifth" 1004)))

   (for ([m (in-list msgs-2)])
     (append-entry! path m))

   ;; Verify original entries unchanged
   (define ids-2 (extract-entry-ids path))
   (check-equal? (take ids-2 (length ids-1)) ids-1 "Original entry IDs should be unchanged")
   (check-true (string-prefix? (get-file-content path) content-1)
               "File should start with original content")
   (delete-directory/files dir #:must-exist? #f))
 (test-case "PROPERTY: append-entries! is atomic and append-only"
   (define rng (make-seeded-rng SEED-2))
   (define dir (make-temp-dir))
   (define path (session-path dir))

   ;; First batch
   (define msgs-1 (generate-session-tree rng 3 2))
   (append-entries! path msgs-1)
   (define count-1 (count-entries path))

   ;; Second batch
   (define msgs-2 (generate-session-tree rng 2 2))
   (append-entries! path msgs-2)
   (define count-2 (count-entries path))

   ;; Total should be sum (append-only)
   (check-equal? count-2 (+ count-1 (length msgs-2)) "Total entries should equal sum of batches")
   (delete-directory/files dir #:must-exist? #f))
 (test-case "PROPERTY: file size monotonically increases with appends"
   (define dir (make-temp-dir))
   (define path (session-path dir))

   (define sizes '())

   ;; Record size after each append
   (for ([i (in-range 5)])
     (define msgs
       (list (make-test-message (format "size-test-~a" i)
                                (if (= i 0)
                                    #f
                                    (format "size-test-~a" (sub1 i)))
                                'user
                                'message)))
     (append-entries! path msgs)
     (set! sizes (cons (file-size path) sizes)))

   ;; Verify monotonic increase
   (define sizes-ascending (reverse sizes))
   (for ([a (in-list sizes-ascending)]
         [b (in-list (cdr sizes-ascending))])
     (check-true (> b a) (format "File size should increase: ~a -> ~a" a b)))
   (delete-directory/files dir #:must-exist? #f))
 (test-case "PROPERTY: entry IDs remain unique after multiple appends"
   (define dir (make-temp-dir))
   (define path (session-path dir))

   ;; Append multiple batches with manually unique IDs
   (for ([batch (in-range 3)])
     (define msgs
       (for/list ([i (in-range 5)])
         (make-test-message (format "unique-id-~a-~a" batch i)
                            (if (and (= batch 0) (= i 0))
                                #f
                                (format "unique-id-~a-~a"
                                        (if (= i 0)
                                            (sub1 batch)
                                            batch)
                                        (if (= i 0)
                                            4
                                            (sub1 i))))
                            'user
                            'message
                            (format "Batch ~a message ~a" batch i)
                            (+ 1000000 (* batch 100) i))))
     (append-entries! path msgs))

   ;; Check all IDs are unique
   (define all-ids (extract-entry-ids path))
   (check-equal? (length all-ids) 15)
   (check-equal? (length all-ids) (length (remove-duplicates all-ids)))
   (delete-directory/files dir #:must-exist? #f))
 (test-case "PROPERTY: corruption cannot happen via normal append operations"
   (define dir (make-temp-dir))
   (define path (session-path dir))

   ;; Append many entries
   (for ([i (in-range 50)])
     (define msg
       (make-test-message (format "corrupt-test-~a" i)
                          (if (= i 0)
                              #f
                              (format "corrupt-test-~a" (sub1 i)))
                          'user
                          'message))
     (append-entry! path msg))

   ;; Verify integrity
   (define report (verify-session-integrity path))
   (check-equal? (hash-ref report 'invalid-entries) '())
   (check-false (hash-ref report 'truncated-at-end?))
   (check-true (hash-ref report 'entry-order-valid?))
   (delete-directory/files dir #:must-exist? #f)))

;; ============================================================
;; Property 3: fork produces valid branch
;; ============================================================

(define-test-suite
 test-property-fork-produces-valid-branch
 (test-case "PROPERTY: fork at specific point creates valid partial branch"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define prov (make-mock-provider-for-replay "fork test"))

   ;; Create original session
   (define config
     (hash 'provider prov 'tool-registry (make-tool-registry) 'event-bus bus 'session-dir dir))
   (define orig-session (make-agent-session config))

   ;; Add some messages with deterministic IDs
   (define messages
     (list (make-test-message "fork-test-0" #f 'system 'message "Root" 1000)
           (make-test-message "fork-test-1" "fork-test-0" 'user 'message "First" 1001)
           (make-test-message "fork-test-2" "fork-test-1" 'assistant 'message "Second" 1002)
           (make-test-message "fork-test-3" "fork-test-2" 'user 'message "Third" 1003)
           (make-test-message "fork-test-4" "fork-test-3" 'assistant 'message "Fourth" 1004)))
   (define log-path (build-path (agent-session-session-dir orig-session) "session.jsonl"))
   (append-entries! log-path messages)

   ;; Fork at middle point (index 3 = 4th message)
   (define fork-point-id "fork-test-3")
   (define forked-session (fork-session orig-session fork-point-id))

   ;; Verify fork has correct subset (4 messages: 0,1,2,3)
   (define forked-history (session-history forked-session))
   (check-equal? (length forked-history) 4)

   ;; Verify IDs match original up to fork point
   (for ([i (in-range 4)])
     (check-equal? (message-id (list-ref forked-history i)) (message-id (list-ref messages i))))

   ;; Verify fork has new session ID
   (check-not-equal? (session-id forked-session) (session-id orig-session))
   (delete-directory/files dir #:must-exist? #f))
 (test-case "PROPERTY: fork preserves tree structure of partial history"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define prov (make-mock-provider-for-replay "fork tree"))

   (define config
     (hash 'provider prov 'tool-registry (make-tool-registry) 'event-bus bus 'session-dir dir))
   (define orig-session (make-agent-session config))

   ;; Create deterministic forked session structure
   ;; Root -> A, B; A -> C, D
   (define messages
     (list (make-test-message "fork-tree-root" #f 'system 'message "Root" 1000)
           (make-test-message "fork-tree-a" "fork-tree-root" 'user 'message "A" 1001)
           (make-test-message "fork-tree-b" "fork-tree-root" 'assistant 'message "B" 1002)
           (make-test-message "fork-tree-c" "fork-tree-a" 'user 'message "C" 1003)
           (make-test-message "fork-tree-d" "fork-tree-a" 'assistant 'message "D" 1004)))
   (define log-path (build-path (agent-session-session-dir orig-session) "session.jsonl"))
   (append-entries! log-path messages)

   ;; Build original tree
   (define original-tree (build-visible-tree messages))

   ;; Fork at "fork-tree-a" (index 1) - should preserve root and A
   (define forked (fork-session orig-session "fork-tree-a"))
   (define forked-history (session-history forked))
   (define forked-tree (build-visible-tree forked-history))

   ;; Forked tree should have root->A relationship preserved
   (check-not-false (member "fork-tree-a" (hash-ref forked-tree "fork-tree-root" '())))

   ;; All nodes in fork should have same children as original (if within fork)
   (check-equal? (hash-ref forked-tree "fork-tree-root" '()) (list "fork-tree-a"))
   (delete-directory/files dir #:must-exist? #f))
 (test-case "PROPERTY: fork at non-existent point copies all entries"
   (define rng (make-seeded-rng SEED-3))
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define prov (make-mock-provider-for-replay "fork all"))

   (define config
     (hash 'provider prov 'tool-registry (make-tool-registry) 'event-bus bus 'session-dir dir))
   (define orig-session (make-agent-session config))

   (define messages (generate-linear-chain rng 5))
   (define log-path (build-path (agent-session-session-dir orig-session) "session.jsonl"))
   (append-entries! log-path messages)

   ;; Fork at non-existent ID
   (define forked (fork-session orig-session "non-existent-id"))
   (define forked-history (session-history forked))

   ;; Should have all messages
   (check-equal? (length forked-history) (length messages))
   (delete-directory/files dir #:must-exist? #f))
 (test-case "PROPERTY: fork without point copies full history"
   (define rng (make-seeded-rng SEED-4))
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define prov (make-mock-provider-for-replay "fork full"))

   (define config
     (hash 'provider prov 'tool-registry (make-tool-registry) 'event-bus bus 'session-dir dir))
   (define orig-session (make-agent-session config))

   (define messages (generate-session-tree rng 3 2))
   (define log-path (build-path (agent-session-session-dir orig-session) "session.jsonl"))
   (append-entries! log-path messages)

   ;; Fork without specifying point
   (define forked (fork-session orig-session))
   (define forked-history (session-history forked))

   ;; Should have all messages
   (check-equal? (length forked-history) (length messages))
   (check-equal? (map message-id forked-history) (map message-id messages))
   (delete-directory/files dir #:must-exist? #f))
 (test-case "PROPERTY: forked session can continue independently"
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define prov (make-mock-provider-for-replay "fork continue"))

   (define config
     (hash 'provider prov 'tool-registry (make-tool-registry) 'event-bus bus 'session-dir dir))
   (define orig-session (make-agent-session config))

   ;; Initial messages with deterministic IDs
   (define messages
     (list (make-test-message "fork-ind-0" #f 'system 'message "Root" 1000)
           (make-test-message "fork-ind-1" "fork-ind-0" 'user 'message "First" 1001)
           (make-test-message "fork-ind-2" "fork-ind-1" 'assistant 'message "Second" 1002)))
   (define log-path (build-path (agent-session-session-dir orig-session) "session.jsonl"))
   (append-entries! log-path messages)

   ;; Fork
   (define forked (fork-session orig-session))

   ;; Add different messages to each
   (define orig-new (make-test-message "orig-new" "fork-ind-2" 'user 'message "Orig new" 1003))
   (define fork-new (make-test-message "fork-new" "fork-ind-2" 'assistant 'message "Fork new" 1003))

   (append-entry! log-path orig-new)
   (append-entry! (build-path (agent-session-session-dir forked) "session.jsonl") fork-new)

   ;; Verify independence
   (define orig-history (session-history orig-session))
   (define forked-history (session-history forked))

   (check-not-false (member "orig-new" (map message-id orig-history)))
   (check-false (member "fork-new" (map message-id orig-history)))
   (check-not-false (member "fork-new" (map message-id forked-history)))
   (check-false (member "orig-new" (map message-id forked-history)))
   (delete-directory/files dir #:must-exist? #f))
 (test-case "PROPERTY: fork preserves deterministic replay property"
   (define rng (make-seeded-rng SEED-2))
   (define dir (make-temp-dir))
   (define bus (make-event-bus))
   (define prov (make-mock-provider-for-replay "fork replay"))

   (define config
     (hash 'provider prov 'tool-registry (make-tool-registry) 'event-bus bus 'session-dir dir))
   (define orig-session (make-agent-session config))

   (define messages (generate-forked-session rng 3 2))
   (define log-path (build-path (agent-session-session-dir orig-session) "session.jsonl"))
   (append-entries! log-path messages)

   ;; Fork and replay both
   (define forked (fork-session orig-session (message-id (list-ref messages 4))))

   (define orig-replay (replay-session log-path))
   (define fork-replay
     (replay-session (build-path (agent-session-session-dir forked) "session.jsonl")))

   ;; Both should be valid replays
   (check-true (> (length orig-replay) (length fork-replay)))
   (check-equal? (take (map message-id orig-replay) (length fork-replay))
                 (map message-id fork-replay))
   (delete-directory/files dir #:must-exist? #f)))

;; ============================================================
;; Property 4: compaction preserves queryable history
;; ============================================================

(define-test-suite
 test-property-compaction-preserves-queryable-history
 (test-case "PROPERTY: compaction preserves all entry IDs in log"
   (define rng (make-seeded-rng SEED-1))
   (define dir (make-temp-dir))
   (define path (session-path dir))

   ;; Create session with many messages
   (define messages (generate-session-tree rng 4 3))
   (append-entries! path messages)

   ;; Get IDs before compaction
   (define ids-before (extract-entry-ids path))

   ;; Compact history
   (define loaded (load-session-log path))
   (define low-tc (token-compaction-config 10 5 20))
   (compact-and-persist! loaded path #:token-config low-tc)

   ;; Get IDs after compaction
   (define ids-after (extract-entry-ids path))

   ;; All original IDs should still be present
   (for ([id (in-list ids-before)])
     (check-not-false (member id ids-after) (format "Original ID ~a should still be in log" id)))
   (delete-directory/files dir #:must-exist? #f))
 (test-case "PROPERTY: compaction adds summary but doesn't remove entries"
   (define rng (make-seeded-rng SEED-2))
   (define dir (make-temp-dir))
   (define path (session-path dir))

   ;; Create session
   (define messages (make-n-messages 30))
   (for ([m (in-list messages)])
     (append-entry! path m))

   (define count-before (count-entries path))

   ;; Compact
   (define loaded (load-session-log path))
   (define low-tc (token-compaction-config 10 5 20))
   (compact-and-persist! loaded path #:token-config low-tc)

   (define count-after (count-entries path))

   ;; Should have original count + 1 summary
   (check-equal? count-after (add1 count-before))
   (delete-directory/files dir #:must-exist? #f))
 (test-case "PROPERTY: query operations work after compaction"
   (define rng (make-seeded-rng SEED-3))
   (define dir (make-temp-dir))
   (define path (session-path dir))

   ;; Create and index session
   (define messages (generate-forked-session rng 4 2))
   (append-entries! path messages)

   (define idx-path (build-path (path-only path) "session.index"))
   (define idx-before (build-index! path idx-path))

   ;; Compact
   (define loaded (load-session-log path))
   (define low-tc (token-compaction-config 10 5 20))
   (compact-and-persist! loaded path #:token-config low-tc)

   ;; Rebuild index after compaction
   (define idx-after (build-index! path idx-path))

   ;; Index should still be valid — after compaction has original entries + summary
   (check-true (> (hash-count (session-index-by-id idx-after))
                  (hash-count (session-index-by-id idx-before))))

   ;; Lookup operations should work
   (for ([msg (in-list messages)])
     (define found (lookup-entry idx-after (message-id msg)))
     (check-not-false found)
     (check-equal? (message-id found) (message-id msg)))
   (delete-directory/files dir #:must-exist? #f))
 (test-case "PROPERTY: tree structure preserved after compaction"
   (define rng (make-seeded-rng SEED-4))
   (define dir (make-temp-dir))
   (define path (session-path dir))

   ;; Create forked session
   (define messages (generate-forked-session rng 5 3))
   (append-entries! path messages)

   ;; Build tree before
   (define tree-before (build-visible-tree (load-session-log path)))

   ;; Compact
   (define loaded (load-session-log path))
   (define low-tc (token-compaction-config 10 5 20))
   (compact-and-persist! loaded path #:token-config low-tc)

   ;; Build tree after
   (define tree-after (build-visible-tree (load-session-log path)))

   ;; Original relationships should still exist
   (for ([(id children) (in-hash tree-before)])
     (check-equal? (hash-ref tree-after id '()) children))
   (delete-directory/files dir #:must-exist? #f))
 (test-case "PROPERTY: replay after compaction includes all original entries"
   (define rng (make-seeded-rng SEED-1))
   (define dir (make-temp-dir))
   (define path (session-path dir))

   ;; Create session
   (define messages (generate-session-tree rng 4 2))
   (append-entries! path messages)

   ;; Original replay
   (define replay-before (replay-session path))

   ;; Compact
   (define loaded (load-session-log path))
   (define low-tc (token-compaction-config 10 5 20))
   (compact-and-persist! loaded path #:token-config low-tc)

   ;; Replay after compaction
   (define replay-after (replay-session path))

   ;; Original entries should be subset of replay-after
   (define ids-before (map message-id replay-before))
   (define ids-after (map message-id replay-after))

   (for ([id (in-list ids-before)])
     (check-not-false (member id ids-after)))
   (delete-directory/files dir #:must-exist? #f))
 (test-case "PROPERTY: compaction summary is queryable"
   (define rng (make-seeded-rng SEED-2))
   (define dir (make-temp-dir))
   (define path (session-path dir))

   ;; Create session
   (define messages (make-n-messages 25))
   (for ([m (in-list messages)])
     (append-entry! path m))

   ;; Compact
   (define loaded (load-session-log path))
   (define low-tc (token-compaction-config 10 5 20))
   (define result (compact-and-persist! loaded path #:token-config low-tc))

   ;; Load and find summary
   (define all-entries (load-session-log path))
   (define summary-entries
     (filter (lambda (m) (eq? (message-kind m) 'compaction-summary)) all-entries))

   (check-equal? (length summary-entries) 1)
   (check-equal? (message-role (first summary-entries)) 'system)

   ;; Summary should have metadata
   (define meta (message-meta (first summary-entries)))
   (check-equal? (hash-ref meta 'type) "compaction")
   (check-true (positive? (hash-ref meta 'removedCount))
               (format "expected positive removedCount, got ~a" (hash-ref meta 'removedCount)))
   (delete-directory/files dir #:must-exist? #f))
 (test-case "PROPERTY: no compaction when under threshold"
   (define rng (make-seeded-rng SEED-3))
   (define dir (make-temp-dir))
   (define path (session-path dir))

   ;; Small session
   (define messages (make-n-messages 5))
   (for ([m (in-list messages)])
     (append-entry! path m))

   (define count-before (count-entries path))

   ;; Compact with high threshold
   (define loaded (load-session-log path))
   (define high-tc (token-compaction-config 50 20 1000000))
   (define result (compact-and-persist! loaded path #:token-config high-tc))

   (define count-after (count-entries path))

   ;; Should be no change (no summary added)
   (check-equal? count-after count-before)
   (check-false (compaction-result-summary-message result))
   (delete-directory/files dir #:must-exist? #f)))

;; ============================================================
;; Helper for make-n-messages (used by compactor tests)
;; ============================================================

(define (make-n-messages n)
  (for/list ([i (in-range n)])
    (make-test-message (format "msg~a" i)
                       #f
                       'user
                       'message
                       (format "Message ~a content" i)
                       (+ 1000000 i))))

;; ============================================================
;; Combined Test Suite
;; ============================================================

(define-test-suite test-replay-properties
                   test-property-replay-reconstructs-tree
                   test-property-append-only-invariant
                   test-property-fork-produces-valid-branch
                   test-property-compaction-preserves-queryable-history)

;; Run all tests
(run-tests test-replay-properties)
