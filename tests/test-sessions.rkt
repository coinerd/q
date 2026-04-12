#lang racket

;; tests/test-sessions.rkt — tests for interfaces/sessions.rkt
;;
;; Covers session scanning, listing, filtering, metadata access,
;; info display, deletion, formatting helpers, and error handling.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         json
         "../util/jsonl.rkt"
         "../interfaces/sessions.rkt")

;; ── Helpers ──

(define (make-temp-session-dir)
  (make-temporary-file "q-sessions-test-~a" 'directory))

(define (create-session! session-dir session-id #:entries [entries '()])
  (define dir (build-path session-dir session-id))
  (make-directory* dir)
  (define jsonl-path (build-path dir "session.jsonl"))
  (when (pair? entries)
    (for ([e (in-list entries)])
      (jsonl-append! jsonl-path e)))
  jsonl-path)

(define (make-test-entry model [ts 1000000000])
  (hasheq 'role "assistant"
          'content (list (hasheq 'type "text" 'text "hello"))
          'model model
          'timestamp ts))

;; ── Test suite ──

(define-test-suite test-sessions

  ;; ============================================================
  ;; scan-session-dirs
  ;; ============================================================

  (test-case "scan-session-dirs returns empty for nonexistent directory"
    (check-equal? (scan-session-dirs "/nonexistent/path/sessions") '()))

  (test-case "scan-session-dirs returns empty for empty directory"
    (define dir (make-temp-session-dir))
    (check-equal? (scan-session-dirs dir) '())
    (delete-directory/files dir))

  (test-case "scan-session-dirs finds sessions with session.jsonl"
    (define dir (make-temp-session-dir))
    (create-session! dir "sess-1" #:entries (list (make-test-entry "gpt-4")))
    (create-session! dir "sess-2" #:entries (list (make-test-entry "claude-3")))
    (define result (scan-session-dirs dir))
    (check-equal? (length result) 2)
    ;; Each entry is (list session-id dir-path)
    (define ids (map car result))
    (check-not-false (member "sess-1" ids))
    (check-not-false (member "sess-2" ids))
    (delete-directory/files dir))

  (test-case "scan-session-dirs ignores dirs without session.jsonl"
    (define dir (make-temp-session-dir))
    (make-directory* (build-path dir "not-a-session"))
    ;; Write a non-session file in there
    (call-with-output-file (build-path dir "not-a-session" "other.txt")
      (lambda (out) (display "data" out)))
    (check-equal? (scan-session-dirs dir) '())
    (delete-directory/files dir))

  (test-case "scan-session-dirs sorts by mtime, newest first"
    (define dir (make-temp-session-dir))
    (create-session! dir "old-session" #:entries (list (make-test-entry "m1" 1000)))
    (create-session! dir "new-session" #:entries (list (make-test-entry "m2" 2000)))
    (define result (scan-session-dirs dir))
    (check-equal? (length result) 2)
    ;; Newest should be first
    (check-equal? (car (first result)) "new-session")
    (check-equal? (car (second result)) "old-session")
    (delete-directory/files dir))

  ;; ============================================================
  ;; read-session-metadata
  ;; ============================================================

  (test-case "read-session-metadata returns metadata hash with expected keys"
    (define dir (make-temp-session-dir))
    (define session-path (build-path dir "sess-meta"))
    (create-session! dir "sess-meta" #:entries (list (make-test-entry "gpt-4")))
    (define meta (read-session-metadata "sess-meta" session-path))
    (check-equal? (hash-ref meta 'id) "sess-meta")
    (check-equal? (hash-ref meta 'message-count) 1)
    (check-equal? (hash-ref meta 'model) "gpt-4")
    (check-true (>= (hash-ref meta 'size-bytes) 1))
    (check-true (>= (hash-ref meta 'mtime) 0))
    (delete-directory/files dir))

  (test-case "read-session-metadata counts multiple entries"
    (define dir (make-temp-session-dir))
    (create-session! dir "sess-multi"
                     #:entries (list (make-test-entry "m1")
                                     (make-test-entry "m2")
                                     (make-test-entry "m3")))
    (define meta (read-session-metadata "sess-multi" (build-path dir "sess-multi")))
    (check-equal? (hash-ref meta 'message-count) 3)
    (check-equal? (hash-ref meta 'model) "m1")
    (delete-directory/files dir))

  (test-case "read-session-metadata returns 'unknown' model when no model field"
    (define dir (make-temp-session-dir))
    (define session-dir (build-path dir "sess-nomodel"))
    (make-directory* session-dir)
    (define jsonl-path (build-path session-dir "session.jsonl"))
    (jsonl-append! jsonl-path (hasheq 'role "user" 'content "hi"))
    (define meta (read-session-metadata "sess-nomodel" session-dir))
    (check-equal? (hash-ref meta 'model) "unknown")
    (delete-directory/files dir))

  (test-case "read-session-metadata handles empty session.jsonl"
    (define dir (make-temp-session-dir))
    (define session-dir (build-path dir "sess-empty"))
    (make-directory* session-dir)
    (close-output-port (open-output-file (build-path session-dir "session.jsonl")))
    (define meta (read-session-metadata "sess-empty" session-dir))
    (check-equal? (hash-ref meta 'message-count) 0)
    (check-equal? (hash-ref meta 'model) "unknown")
    (check-equal? (hash-ref meta 'size-bytes) 0)
    (delete-directory/files dir))

  ;; ============================================================
  ;; sessions-list
  ;; ============================================================

  (test-case "sessions-list returns empty for nonexistent directory"
    (check-equal? (sessions-list "/nonexistent") '()))

  (test-case "sessions-list returns metadata for sessions"
    (define dir (make-temp-session-dir))
    (create-session! dir "s1" #:entries (list (make-test-entry "gpt-4")))
    (create-session! dir "s2" #:entries (list (make-test-entry "claude")))
    (define result (sessions-list dir))
    (check-equal? (length result) 2)
    ;; Both sessions should appear; order depends on mtime which may be equal
    (define ids (map (lambda (m) (hash-ref m 'id)) result))
    (check-not-false (member "s1" ids))
    (check-not-false (member "s2" ids))
    (delete-directory/files dir))

  (test-case "sessions-list respects #:limit"
    (define dir (make-temp-session-dir))
    (for ([i (in-range 5)])
      (create-session! dir (format "s~a" i) #:entries (list (make-test-entry "m" (+ 1000 i)))))
    (define result (sessions-list dir #:limit 3))
    (check-equal? (length result) 3)
    (delete-directory/files dir))

  (test-case "sessions-list #:sort 'by-size sorts by size"
    (define dir (make-temp-session-dir))
    (create-session! dir "small" #:entries (list (make-test-entry "m")))
    (create-session! dir "big" #:entries (list (make-test-entry "m1")
                                                (make-test-entry "m2")
                                                (make-test-entry "m3")
                                                (make-test-entry "m4")
                                                (make-test-entry "m5")))
    (define result (sessions-list dir #:sort 'by-size))
    (check-equal? (length result) 2)
    (check-true (>= (hash-ref (first result) 'size-bytes)
                    (hash-ref (second result) 'size-bytes)))
    (delete-directory/files dir))

  ;; ============================================================
  ;; sessions-list->strings
  ;; ============================================================

  (test-case "sessions-list->strings produces header + rows"
    (define dir (make-temp-session-dir))
    (create-session! dir "s1" #:entries (list (make-test-entry "gpt-4")))
    (define sessions (sessions-list dir))
    (define lines (sessions-list->strings sessions))
    (check-true (>= (length lines) 2)) ; header + at least 1 row
    (check-true (string-contains? (first lines) "ID"))
    (check-true (string-contains? (first lines) "Model"))
    (delete-directory/files dir))

  (test-case "sessions-list->strings handles empty list"
    (define lines (sessions-list->strings '()))
    (check-equal? (length lines) 1) ; header only
    (check-true (string-contains? (first lines) "ID")))

  ;; ============================================================
  ;; sessions-info
  ;; ============================================================

  (test-case "sessions-info returns #f for nonexistent session"
    (define dir (make-temp-session-dir))
    (check-false (sessions-info dir "nonexistent-session"))
    (delete-directory/files dir))

  (test-case "sessions-info returns metadata for existing session"
    (define dir (make-temp-session-dir))
    (create-session! dir "sess-info" #:entries (list (make-test-entry "gpt-4")))
    (define info (sessions-info dir "sess-info"))
    (check-not-false info)
    (check-equal? (hash-ref info 'id) "sess-info")
    (check-equal? (hash-ref info 'message-count) 1)
    (check-equal? (hash-ref info 'model) "gpt-4")
    (check-true (path? (hash-ref info 'path)))
    (delete-directory/files dir))

  (test-case "sessions-info counts tool calls"
    (define dir (make-temp-session-dir))
    (define session-dir (build-path dir "sess-tools"))
    (make-directory* session-dir)
    (define jsonl-path (build-path session-dir "session.jsonl"))
    (jsonl-append! jsonl-path (hasheq 'role "assistant"
                                       'content (list (hasheq 'type "tool-call"
                                                              'id "tc1"
                                                              'name "read")
                                                      (hasheq 'type "text"
                                                              'text "result"))))
    (define info (sessions-info dir "sess-tools"))
    (check-equal? (hash-ref info 'tool-call-count) 1)
    (delete-directory/files dir))

  (test-case "sessions-info counts branches (entries with parentId)"
    (define dir (make-temp-session-dir))
    (define session-dir (build-path dir "sess-branch"))
    (make-directory* session-dir)
    (define jsonl-path (build-path session-dir "session.jsonl"))
    (jsonl-append! jsonl-path (hasheq 'role "user" 'content "hi"))
    (jsonl-append! jsonl-path (hasheq 'role "assistant"
                                       'content "hello"
                                       'parentId "entry-1"))
    (define info (sessions-info dir "sess-branch"))
    (check-equal? (hash-ref info 'branch-count) 1)
    (delete-directory/files dir))

  ;; ============================================================
  ;; sessions-info->string
  ;; ============================================================

  (test-case "sessions-info->string returns 'not found' for #f"
    (check-equal? (sessions-info->string #f) "Session not found."))

  (test-case "sessions-info->string formats info hash"
    (define info (hasheq 'id "test-id"
                         'model "gpt-4"
                         'message-count 5
                         'tool-call-count 2
                         'size-bytes 1024
                         'mtime 0
                         'branch-count 0
                         'path (build-path "/tmp" "q-test-session")))
    (define str (sessions-info->string info))
    (check-true (string-contains? str "test-id"))
    (check-true (string-contains? str "gpt-4"))
    (check-true (string-contains? str "5"))
    (check-true (string-contains? str "2")))

  ;; ============================================================
  ;; sessions-delete
  ;; ============================================================

  (test-case "sessions-delete returns 'not-found for nonexistent session"
    (define dir (make-temp-session-dir))
    (check-equal? (sessions-delete dir "nonexistent") 'not-found)
    (delete-directory/files dir))

  (test-case "sessions-delete removes session directory and returns 'ok"
    (define dir (make-temp-session-dir))
    (create-session! dir "to-delete" #:entries (list (make-test-entry "m")))
    (check-true (directory-exists? (build-path dir "to-delete")))
    (check-equal? (sessions-delete dir "to-delete") 'ok)
    (check-false (directory-exists? (build-path dir "to-delete")))
    (delete-directory/files dir))

  (test-case "sessions-delete with confirm?: 'y' deletes"
    (define dir (make-temp-session-dir))
    (create-session! dir "confirm-del" #:entries (list (make-test-entry "m")))
    (define in (open-input-string "y\n"))
    (define out (open-output-string))
    (check-equal? (sessions-delete dir "confirm-del"
                                   #:confirm? #t
                                   #:in in #:out out)
                  'ok)
    (check-false (directory-exists? (build-path dir "confirm-del")))
    (delete-directory/files dir))

  (test-case "sessions-delete with confirm?: 'n' returns 'cancelled"
    (define dir (make-temp-session-dir))
    (create-session! dir "confirm-keep" #:entries (list (make-test-entry "m")))
    (define in (open-input-string "n\n"))
    (define out (open-output-string))
    (check-equal? (sessions-delete dir "confirm-keep"
                                   #:confirm? #t
                                   #:in in #:out out)
                  'cancelled)
    ;; Session should still exist
    (check-true (directory-exists? (build-path dir "confirm-keep")))
    (delete-directory/files dir))

  (test-case "sessions-delete with confirm?: empty input returns 'cancelled"
    (define dir (make-temp-session-dir))
    (create-session! dir "confirm-empty" #:entries (list (make-test-entry "m")))
    (define in (open-input-string "\n"))
    (define out (open-output-string))
    (check-equal? (sessions-delete dir "confirm-empty"
                                   #:confirm? #t
                                   #:in in #:out out)
                  'cancelled)
    (delete-directory/files dir))

  ;; ============================================================
  ;; Session path construction
  ;; ============================================================

  (test-case "session path is built from session-dir and session-id"
    (define dir (make-temp-session-dir))
    (create-session! dir "path-test" #:entries (list (make-test-entry "m")))
    (define info (sessions-info dir "path-test"))
    (check-equal? (hash-ref info 'path) (build-path dir "path-test"))
    (delete-directory/files dir))

  ;; ============================================================
  ;; sessions-list with many sessions and limit edge cases
  ;; ============================================================

  (test-case "sessions-list #:limit larger than count returns all"
    (define dir (make-temp-session-dir))
    (create-session! dir "s1" #:entries (list (make-test-entry "m")))
    (define result (sessions-list dir #:limit 100))
    (check-equal? (length result) 1)
    (delete-directory/files dir))

  (test-case "sessions-list #:limit 0 returns empty"
    (define dir (make-temp-session-dir))
    (create-session! dir "s1" #:entries (list (make-test-entry "m")))
    (define result (sessions-list dir #:limit 0))
    (check-equal? (length result) 0)
    (delete-directory/files dir))
  )

;; Run
(run-tests test-sessions)
