#lang racket

;;; tests/test-session-index.rkt — tests for runtime/session-index.rkt

(require rackunit
         rackunit/text-ui
         racket/file
         "../agent/types.rkt"
         "../runtime/session-store.rkt"
         "../runtime/session-index.rkt")

;; ── Helpers ──

(define (make-temp-dir)
  (make-temporary-file "q-index-test-~a" 'directory))

(define (session-path dir)
  (build-path dir "session.jsonl"))

(define (index-path dir)
  (build-path dir "session.index"))

(define (bookmarks-path dir)
  (build-path dir "session-bookmarks.json"))

(define (make-test-message id parent-id role kind)
  (make-message id parent-id role kind
                (list (make-text-part "hello"))
                (current-seconds)
                (hasheq)))

;; ── Test suite ──

(define-test-suite test-session-index

  ;; ── build-index! basic ──

  (test-case "build-index! creates index from session log"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entries! sp
                     (list (make-test-message "root" #f 'system 'message)
                           (make-test-message "c1" "root" 'user 'message)
                           (make-test-message "c2" "root" 'assistant 'message)))
    (define idx (build-index! sp ip))
    (check-true (session-index? idx))
    (check-equal? (hash-count (session-index-by-id idx)) 3)
    (delete-directory/files dir #:must-exist? #f))

  (test-case "build-index! creates index file on disk"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entry! sp (make-test-message "root" #f 'system 'message))
    (build-index! sp ip)
    (check-pred file-exists? ip)
    (delete-directory/files dir #:must-exist? #f))

  (test-case "build-index! works on empty/nonexistent log"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (define idx (build-index! sp ip))
    (check-true (session-index? idx))
    (check-equal? (hash-count (session-index-by-id idx)) 0)
    (delete-directory/files dir #:must-exist? #f))

  ;; ── lookup-entry ──

  (test-case "lookup-entry finds entry by id"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entries! sp
                     (list (make-test-message "root" #f 'system 'message)
                           (make-test-message "c1" "root" 'user 'message)))
    (define idx (build-index! sp ip))
    (define found (lookup-entry idx "c1"))
    (check-not-false found)
    (check-equal? (message-id found) "c1")
    (check-equal? (message-parent-id found) "root")
    (delete-directory/files dir #:must-exist? #f))

  (test-case "lookup-entry returns #f for missing id"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entry! sp (make-test-message "root" #f 'system 'message))
    (define idx (build-index! sp ip))
    (check-false (lookup-entry idx "nonexistent"))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── children-of ──

  (test-case "children-of returns children of a node"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entries! sp
                     (list (make-test-message "root" #f 'system 'message)
                           (make-test-message "c1" "root" 'user 'message)
                           (make-test-message "c2" "root" 'assistant 'message)
                           (make-test-message "c3" "c1" 'tool 'tool-result)))
    (define idx (build-index! sp ip))
    (define children (children-of idx "root"))
    (check-equal? (sort (map message-id children) string<?) '("c1" "c2"))
    (define c1-children (children-of idx "c1"))
    (check-equal? (map message-id c1-children) '("c3"))
    (delete-directory/files dir #:must-exist? #f))

  (test-case "children-of returns empty list for leaf node"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entries! sp
                     (list (make-test-message "root" #f 'system 'message)
                           (make-test-message "c1" "root" 'user 'message)))
    (define idx (build-index! sp ip))
    (check-equal? (children-of idx "c1") '())
    (delete-directory/files dir #:must-exist? #f))

  (test-case "children-of returns empty list for unknown id"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entry! sp (make-test-message "root" #f 'system 'message))
    (define idx (build-index! sp ip))
    (check-equal? (children-of idx "nonexistent") '())
    (delete-directory/files dir #:must-exist? #f))

  ;; ── leaf-nodes ──

  (test-case "leaf-nodes returns nodes with no children"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    ;; root -> c1 -> c3
    ;;      -> c2
    (append-entries! sp
                     (list (make-test-message "root" #f 'system 'message)
                           (make-test-message "c1" "root" 'user 'message)
                           (make-test-message "c2" "root" 'assistant 'message)
                           (make-test-message "c3" "c1" 'tool 'tool-result)))
    (define idx (build-index! sp ip))
    (define leaves (leaf-nodes idx))
    (define leaf-ids (sort (map message-id leaves) string<?))
    (check-equal? leaf-ids '("c2" "c3"))
    (delete-directory/files dir #:must-exist? #f))

  (test-case "leaf-nodes on single root"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entry! sp (make-test-message "root" #f 'system 'message))
    (define idx (build-index! sp ip))
    (check-equal? (map message-id (leaf-nodes idx)) '("root"))
    (delete-directory/files dir #:must-exist? #f))

  (test-case "leaf-nodes on empty index"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (define idx (build-index! sp ip))
    (check-equal? (leaf-nodes idx) '())
    (delete-directory/files dir #:must-exist? #f))

  ;; ── resolve-active-leaf ──

  (test-case "resolve-active-leaf returns last appended leaf by default"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entries! sp
                     (list (make-test-message "root" #f 'system 'message)
                           (make-test-message "c1" "root" 'user 'message)
                           (make-test-message "c2" "c1" 'assistant 'message)))
    (define idx (build-index! sp ip))
    (define active (resolve-active-leaf idx))
    (check-equal? (message-id active) "c2")
    (delete-directory/files dir #:must-exist? #f))

  (test-case "resolve-active-leaf on single root returns root"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entry! sp (make-test-message "root" #f 'system 'message))
    (define idx (build-index! sp ip))
    (check-equal? (message-id (resolve-active-leaf idx)) "root")
    (delete-directory/files dir #:must-exist? #f))

  (test-case "resolve-active-leaf returns last leaf in append order among multiple leaves"
    ;; root -> branch-a
    ;;      -> branch-b
    ;; Last appended is branch-b, so it should be active leaf
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entries! sp
                     (list (make-test-message "root" #f 'system 'message)
                           (make-test-message "branch-a" "root" 'user 'message)
                           (make-test-message "branch-b" "root" 'assistant 'message)))
    (define idx (build-index! sp ip))
    (check-equal? (message-id (resolve-active-leaf idx)) "branch-b")
    (delete-directory/files dir #:must-exist? #f))

  (test-case "resolve-active-leaf on empty index returns #f"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (define idx (build-index! sp ip))
    (check-false (resolve-active-leaf idx))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── Fork detection via index ──

  (test-case "fork detection: two children of same parent are both leaves"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entries! sp
                     (list (make-test-message "root" #f 'system 'message)
                           (make-test-message "fork-a" "root" 'user 'message)
                           (make-test-message "fork-b" "root" 'assistant 'message)))
    (define idx (build-index! sp ip))
    (define leaves (leaf-nodes idx))
    (define leaf-ids (sort (map message-id leaves) string<?))
    (check-equal? leaf-ids '("fork-a" "fork-b"))
    ;; children-of root should show both forks
    (define children (children-of idx "root"))
    (check-equal? (sort (map message-id children) string<?) '("fork-a" "fork-b"))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── load-index ──

  (test-case "load-index loads previously saved index"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entries! sp
                     (list (make-test-message "root" #f 'system 'message)
                           (make-test-message "c1" "root" 'user 'message)))
    (define idx1 (build-index! sp ip))
    ;; Load from disk
    (define idx2 (load-index ip))
    (check-true (session-index? idx2))
    (check-equal? (hash-count (session-index-by-id idx2)) 2)
    (check-equal? (message-id (lookup-entry idx2 "root")) "root")
    (check-equal? (message-id (lookup-entry idx2 "c1")) "c1")
    (delete-directory/files dir #:must-exist? #f))

  (test-case "load-index on nonexistent file returns empty index"
    (define dir (make-temp-dir))
    (define ip (index-path dir))
    (define idx (load-index ip))
    (check-true (session-index? idx))
    (check-equal? (hash-count (session-index-by-id idx)) 0)
    (delete-directory/files dir #:must-exist? #f))

  ;; ── Index rebuild from log ──

  (test-case "index rebuild from log matches original semantics"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entries! sp
                     (list (make-test-message "root" #f 'system 'message)
                           (make-test-message "c1" "root" 'user 'message)
                           (make-test-message "c2" "c1" 'assistant 'message)
                           (make-test-message "c3" "c1" 'tool 'tool-result)))
    (define idx1 (build-index! sp ip))
    ;; Delete the index file (simulating crash/corruption)
    (delete-file ip)
    (check-false (file-exists? ip))
    ;; Rebuild from log
    (define idx2 (build-index! sp ip))
    ;; Verify semantics match
    (check-equal? (hash-count (session-index-by-id idx2))
                  (hash-count (session-index-by-id idx1)))
    (check-equal? (sort (map message-id (leaf-nodes idx2)) string<?)
                  (sort (map message-id (leaf-nodes idx1)) string<?))
    (check-equal? (message-id (resolve-active-leaf idx2))
                  (message-id (resolve-active-leaf idx1)))
    (check-equal? (sort (map message-id (children-of idx2 "root")) string<?)
                  (sort (map message-id (children-of idx1 "root")) string<?))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── Crash recovery: skip corrupted lines ──

  (test-case "build-index! handles corrupted lines gracefully"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entry! sp (make-test-message "root" #f 'system 'message))
    ;; Inject a corrupted line
    (call-with-output-file sp
      (lambda (out) (displayln "BROKEN!!!" out))
      #:mode 'text
      #:exists 'append)
    (append-entry! sp (make-test-message "c1" "root" 'user 'message))
    ;; build-index should only index valid entries
    (define idx (build-index! sp ip))
    (check-equal? (hash-count (session-index-by-id idx)) 2)
    (check-equal? (message-id (lookup-entry idx "root")) "root")
    (check-equal? (message-id (lookup-entry idx "c1")) "c1")
    (delete-directory/files dir #:must-exist? #f))

  ;; ── Deep tree ──

  (test-case "deep tree: chain of 10 entries"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (define msgs
      (for/list ([i (in-range 10)])
        (define id (format "id~a" i))
        (define pid (if (= i 0) #f (format "id~a" (sub1 i))))
        (make-test-message id pid 'user 'message)))
    (append-entries! sp msgs)
    (define idx (build-index! sp ip))
    ;; All 10 indexed
    (check-equal? (hash-count (session-index-by-id idx)) 10)
    ;; Only last is leaf
    (check-equal? (map message-id (leaf-nodes idx)) '("id9"))
    ;; Active leaf is last
    (check-equal? (message-id (resolve-active-leaf idx)) "id9")
    ;; Each has exactly one child except the last
    (for ([i (in-range 9)])
      (check-equal? (map message-id (children-of idx (format "id~a" i)))
                    (list (format "id~a" (add1 i)))))
    (check-equal? (children-of idx "id9") '())
    (delete-directory/files dir #:must-exist? #f))

  ;; ── parent-id mapping ──

  (test-case "parent-id mapping is correct"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entries! sp
                     (list (make-test-message "a" #f 'system 'message)
                           (make-test-message "b" "a" 'user 'message)
                           (make-test-message "c" "b" 'assistant 'message)))
    (define idx (build-index! sp ip))
    (check-false (message-parent-id (lookup-entry idx "a")))
    (check-equal? (message-parent-id (lookup-entry idx "b")) "a")
    (check-equal? (message-parent-id (lookup-entry idx "c")) "b")
    (delete-directory/files dir #:must-exist? #f))

  ;; ============================================================
  ;; Bookmark API Tests
  ;; ============================================================

  ;; ── bookmark struct ──

  (test-case "bookmark struct creation and accessors"
    (define bm (make-bookmark "bm-1" "entry-1" "my-label" 12345))
    (check-pred bookmark? bm)
    (check-equal? (bookmark-id bm) "bm-1")
    (check-equal? (bookmark-entry-id bm) "entry-1")
    (check-equal? (bookmark-label bm) "my-label")
    (check-equal? (bookmark-timestamp bm) 12345))

  ;; ── add-bookmark! ──

  (test-case "add-bookmark! creates bookmark with unique id"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entry! sp (make-test-message "root" #f 'system 'message))
    (define idx (build-index! sp ip))
    (define bm-id (add-bookmark! idx "root" "important"))
    (check-pred string? bm-id)
    (check-true (string-prefix? bm-id "bm-"))
    (define bm (get-bookmark idx bm-id))
    (check-not-false bm)
    (check-equal? (bookmark-entry-id bm) "root")
    (check-equal? (bookmark-label bm) "important")
    (delete-directory/files dir #:must-exist? #f))

  (test-case "add-bookmark! replaces bookmark with same label"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entries! sp
                     (list (make-test-message "root" #f 'system 'message)
                           (make-test-message "c1" "root" 'user 'message)))
    (define idx (build-index! sp ip))
    (define bm-id1 (add-bookmark! idx "root" "my-label"))
    (define bm-id2 (add-bookmark! idx "c1" "my-label"))
    ;; Same label should replace previous bookmark
    (check-equal? (length (list-bookmarks idx)) 1)
    (define bm (find-bookmark-by-label idx "my-label"))
    (check-not-false bm)
    (check-equal? (bookmark-entry-id bm) "c1")
    (delete-directory/files dir #:must-exist? #f))

  ;; ── remove-bookmark! ──

  (test-case "remove-bookmark! deletes existing bookmark"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entry! sp (make-test-message "root" #f 'system 'message))
    (define idx (build-index! sp ip))
    (define bm-id (add-bookmark! idx "root" "to-remove"))
    (check-equal? (length (list-bookmarks idx)) 1)
    (check-true (remove-bookmark! idx bm-id))
    (check-equal? (length (list-bookmarks idx)) 0)
    (check-false (get-bookmark idx bm-id))
    (delete-directory/files dir #:must-exist? #f))

  (test-case "remove-bookmark! returns #f for nonexistent id"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entry! sp (make-test-message "root" #f 'system 'message))
    (define idx (build-index! sp ip))
    (check-false (remove-bookmark! idx "nonexistent-id"))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── list-bookmarks ──

  (test-case "list-bookmarks returns all bookmarks"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entries! sp
                     (list (make-test-message "root" #f 'system 'message)
                           (make-test-message "c1" "root" 'user 'message)))
    (define idx (build-index! sp ip))
    (check-equal? (length (list-bookmarks idx)) 0)
    (add-bookmark! idx "root" "first")
    (add-bookmark! idx "c1" "second")
    (define bms (list-bookmarks idx))
    (check-equal? (length bms) 2)
    (define labels (sort (map bookmark-label bms) string<?))
    (check-equal? labels '("first" "second"))
    (delete-directory/files dir #:must-exist? #f))

  (test-case "list-bookmarks returns empty list for no bookmarks"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entry! sp (make-test-message "root" #f 'system 'message))
    (define idx (build-index! sp ip))
    (check-equal? (list-bookmarks idx) '())
    (delete-directory/files dir #:must-exist? #f))

  ;; ── find-bookmark-by-label ──

  (test-case "find-bookmark-by-label finds exact match"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entry! sp (make-test-message "root" #f 'system 'message))
    (define idx (build-index! sp ip))
    (add-bookmark! idx "root" "find-me")
    (define found (find-bookmark-by-label idx "find-me"))
    (check-not-false found)
    (check-equal? (bookmark-label found) "find-me")
    (check-equal? (bookmark-entry-id found) "root")
    (delete-directory/files dir #:must-exist? #f))

  (test-case "find-bookmark-by-label returns #f for no match"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entry! sp (make-test-message "root" #f 'system 'message))
    (define idx (build-index! sp ip))
    (add-bookmark! idx "root" "some-label")
    (check-false (find-bookmark-by-label idx "other-label"))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── get-bookmark ──

  (test-case "get-bookmark retrieves by id"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entry! sp (make-test-message "root" #f 'system 'message))
    (define idx (build-index! sp ip))
    (define bm-id (add-bookmark! idx "root" "retrievable"))
    (define bm (get-bookmark idx bm-id))
    (check-not-false bm)
    (check-equal? (bookmark-id bm) bm-id)
    (check-equal? (bookmark-label bm) "retrievable")
    (delete-directory/files dir #:must-exist? #f))

  (test-case "get-bookmark returns #f for unknown id"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entry! sp (make-test-message "root" #f 'system 'message))
    (define idx (build-index! sp ip))
    (check-false (get-bookmark idx "unknown-id"))
    (delete-directory/files dir #:must-exist? #f))

  ;; ── Bookmark persistence ──

  (test-case "save-bookmarks! creates sidecar file"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (define bp (bookmarks-path dir))
    (append-entry! sp (make-test-message "root" #f 'system 'message))
    (define idx (build-index! sp ip))
    (add-bookmark! idx "root" "persistent-label")
    (save-bookmarks! sp idx)
    (check-pred file-exists? bp)
    (delete-directory/files dir #:must-exist? #f))

  (test-case "save-bookmarks! and load-bookmarks roundtrip"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entries! sp
                     (list (make-test-message "root" #f 'system 'message)
                           (make-test-message "c1" "root" 'user 'message)))
    (define idx (build-index! sp ip))
    (define bm-id1 (add-bookmark! idx "root" "root-bookmark"))
    (define bm-id2 (add-bookmark! idx "c1" "c1-bookmark"))
    (save-bookmarks! sp idx)
    ;; Load bookmarks into new index
    (define loaded-bms (load-bookmarks sp))
    (check-equal? (length loaded-bms) 2)
    (define loaded-labels (sort (map bookmark-label loaded-bms) string<?))
    (check-equal? loaded-labels '("c1-bookmark" "root-bookmark"))
    (delete-directory/files dir #:must-exist? #f))

  (test-case "load-bookmarks returns empty list for nonexistent file"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define bms (load-bookmarks sp))
    (check-equal? bms '())
    (delete-directory/files dir #:must-exist? #f))

  (test-case "load-bookmarks handles corrupted file gracefully"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define bp (bookmarks-path dir))
    ;; Write invalid JSON
    (call-with-output-file bp
      (lambda (out) (display "NOT VALID JSON{" out))
      #:mode 'text
      #:exists 'truncate)
    (define bms (load-bookmarks sp))
    (check-equal? bms '())
    (delete-directory/files dir #:must-exist? #f))

  (test-case "bookmarks survive session-resume via sidecar"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entries! sp
                     (list (make-test-message "root" #f 'system 'message)
                           (make-test-message "c1" "root" 'user 'message)))
    ;; Create and save bookmarks
    (define idx1 (build-index! sp ip))
    (define bm-id (add-bookmark! idx1 "c1" "resume-test"))
    (save-bookmarks! sp idx1)
    ;; Simulate resume: build new index and load bookmarks
    (define idx2 (build-index! sp ip))
    (for ([bm (in-list (load-bookmarks sp))])
      (hash-set! (session-index-bookmarks idx2) (bookmark-id bm) bm))
    (define bm (get-bookmark idx2 bm-id))
    (check-not-false bm)
    (check-equal? (bookmark-label bm) "resume-test")
    (check-equal? (bookmark-entry-id bm) "c1")
    (delete-directory/files dir #:must-exist? #f))

  ;; ============================================================
  ;; Active Leaf API Tests
  ;; ============================================================

  (test-case "active-leaf returns most recent entry"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entries! sp
                     (list (make-test-message "root" #f 'system 'message)
                           (make-test-message "c1" "root" 'user 'message)
                           (make-test-message "c2" "c1" 'assistant 'message)))
    (define idx (build-index! sp ip))
    (define al (active-leaf idx))
    (check-not-false al)
    (check-equal? (message-id al) "c2")
    (delete-directory/files dir #:must-exist? #f))

  (test-case "active-leaf returns #f for empty index"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (define idx (build-index! sp ip))
    (check-false (active-leaf idx))
    (delete-directory/files dir #:must-exist? #f))

  (test-case "switch-leaf! changes active leaf"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    ;; root -> branch-a
    ;;      -> branch-b
    (append-entries! sp
                     (list (make-test-message "root" #f 'system 'message)
                           (make-test-message "branch-a" "root" 'user 'message)
                           (make-test-message "branch-b" "root" 'assistant 'message)))
    (define idx (build-index! sp ip))
    ;; Default: last leaf (branch-b)
    (check-equal? (message-id (active-leaf idx)) "branch-b")
    ;; Switch to branch-a
    (switch-leaf! idx "branch-a")
    (check-equal? (message-id (active-leaf idx)) "branch-a")
    ;; Switch back to branch-b
    (switch-leaf! idx "branch-b")
    (check-equal? (message-id (active-leaf idx)) "branch-b")
    (delete-directory/files dir #:must-exist? #f))

  (test-case "mark-active-leaf! persists in index"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entries! sp
                     (list (make-test-message "root" #f 'system 'message)
                           (make-test-message "c1" "root" 'user 'message)
                           (make-test-message "c2" "c1" 'assistant 'message)))
    (define idx (build-index! sp ip))
    ;; Mark c1 as active leaf (even though it has children)
    (mark-active-leaf! idx "c1")
    (define al (active-leaf idx))
    (check-not-false al)
    (check-equal? (message-id al) "c1")
    ;; Verify it's different from resolve-active-leaf (which returns c2)
    (check-equal? (message-id (resolve-active-leaf idx)) "c2")
    (delete-directory/files dir #:must-exist? #f))

  (test-case "switch-leaf! returns #f for unknown id"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entry! sp (make-test-message "root" #f 'system 'message))
    (define idx (build-index! sp ip))
    (check-false (switch-leaf! idx "nonexistent")))

  (test-case "mark-active-leaf! returns #f for unknown id"
    (define dir (make-temp-dir))
    (define sp (session-path dir))
    (define ip (index-path dir))
    (append-entry! sp (make-test-message "root" #f 'system 'message))
    (define idx (build-index! sp ip))
    (check-false (mark-active-leaf! idx "nonexistent")))
  )

;; Run
(run-tests test-session-index)
