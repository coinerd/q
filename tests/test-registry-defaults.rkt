#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

(require rackunit
         "../tools/registry-defaults.rkt"
         "../tools/tool.rkt"
         "../tools/tool-struct.rkt")

;; ============================================================
;; register-default-tools! — registers all tools
;; ============================================================

(test-case "register-default-tools! registers all 35 built-in tools"
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (define names (sort (tool-names reg) string<?))
  (check equal?
         names
         (sort '("bash" "browser_check_local_app"
                        "browser_click"
                        "browser_close"
                        "browser_extract"
                        "browser_observe"
                        "browser_open"
                        "browser_press"
                        "browser_screenshot"
                        "browser_scroll"
                        "browser_type"
                        "cleanup-expired-memory"
                        "clear-memory"
                        "consolidate-memory"
                        "date"
                        "delete-lines"
                        "delete-memory"
                        "edit"
                        "find"
                        "firecrawl"
                        "grep"
                        "list-memory"
                        "ls"
                        "read"
                        "record_conclusion"
                        "save-conclusion"
                        "search-memory"
                        "session_recall"
                        "set-task-state"
                        "skill-route"
                        "spawn-subagent"
                        "spawn-subagents"
                        "store-memory"
                        "update-memory"
                        "write")
               string<?)))

(test-case "each registered tool has correct name"
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (for ([name '("read" "write"
                       "edit"
                       "bash"
                       "grep"
                       "find"
                       "ls"
                       "date"
                       "firecrawl"
                       "spawn-subagent"
                       "session_recall")])
    (define t (lookup-tool reg name))
    (check-pred tool? t (format "tool ~a should exist" name))
    (check-equal? (tool-name t) name)))

(test-case "each registered tool has a non-empty description"
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (for ([name '("read" "write"
                       "edit"
                       "bash"
                       "grep"
                       "find"
                       "ls"
                       "date"
                       "firecrawl"
                       "spawn-subagent"
                       "session_recall")])
    (define t (lookup-tool reg name))
    (check-true (> (string-length (tool-description t)) 0)
                (format "tool ~a should have a description" name))))

(test-case "each registered tool has a schema hash"
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (for ([name '("read" "write"
                       "edit"
                       "bash"
                       "grep"
                       "find"
                       "ls"
                       "date"
                       "firecrawl"
                       "spawn-subagent"
                       "session_recall")])
    (define t (lookup-tool reg name))
    (check-pred hash? (tool-schema t) (format "tool ~a schema should be a hash" name))
    (check-equal? (hash-ref (tool-schema t) 'type #f)
                  "object"
                  (format "tool ~a schema should have type object" name))))

(test-case "each registered tool has a procedure execute"
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (for ([name '("read" "write"
                       "edit"
                       "bash"
                       "grep"
                       "find"
                       "ls"
                       "date"
                       "firecrawl"
                       "spawn-subagent"
                       "session_recall")])
    (define t (lookup-tool reg name))
    (check-pred procedure? (tool-execute t) (format "tool ~a execute should be a procedure" name))))

;; ============================================================
;; register-default-tools! with #:only filter
;; ============================================================

(test-case "register-default-tools! with #:only registers subset"
  (define reg (make-tool-registry))
  (register-default-tools! reg #:only '("read" "write"))
  (check-equal? (sort (tool-names reg) string<?) '("read" "write")))

(test-case "register-default-tools! with #:only empty registers nothing"
  (define reg (make-tool-registry))
  (register-default-tools! reg #:only '())
  (check-equal? (tool-names reg) '()))
