#lang racket

;; tests/workflows/fixtures/session-assert.rkt — JSONL log assertions
;;
;; Assertion helpers for verifying session JSONL files.
;; Uses #lang racket for internal defines in cond.

(require racket/file
         racket/list
         "../../../util/jsonl.rkt"
         "../../../util/protocol-types.rkt"
         "../../../runtime/session-store.rkt")

(provide check-session-jsonl-valid
         check-session-contains-turns
         check-session-tool-sequence
         check-session-tree-structure
         session-log-entries
         entries-with-role)

;; ============================================================
;; Read helpers
;; ============================================================

(define (session-log-entries log-path)
  (if (file-exists? log-path)
      ;; #773: Filter out session-info (version header) entries
      (filter (lambda (m) (not (eq? (message-kind m) 'session-info))) (load-session-log log-path))
      '()))

(define (entries-with-role entries role)
  (filter (lambda (m) (eq? (message-role m) role)) entries))

;; ============================================================
;; check-session-jsonl-valid
;; ============================================================

(define (check-session-jsonl-valid log-path)
  (with-handlers ([exn:fail? (lambda (e) (format "exception: ~a" (exn-message e)))])
    (cond
      [(not (file-exists? log-path)) "log file does not exist"]
      [else
       (define entries (session-log-entries log-path))
       (cond
         [(null? entries) "log file is empty"]
         [else
          (define timestamps (map message-timestamp entries))
          (define monotonic?
            (for/and ([a (in-list timestamps)]
                      [b (in-list (cdr timestamps))])
              (<= a b)))
          (if monotonic? #t "timestamps are not monotonic")])])))

;; ============================================================
;; check-session-contains-turns
;; ============================================================

(define (check-session-contains-turns log-path
                                      #:user-turns [expected-user #f]
                                      #:assistant-turns [expected-assistant #f])
  (define entries (session-log-entries log-path))
  (define user-entries (entries-with-role entries 'user))
  (define assistant-entries (entries-with-role entries 'assistant))
  (define user-count (length user-entries))
  (define assistant-count (length assistant-entries))

  (cond
    [(and expected-user (not (= user-count expected-user)))
     (format "expected ~a user turns, got ~a" expected-user user-count)]
    [(and expected-assistant (not (= assistant-count expected-assistant)))
     (format "expected ~a assistant turns, got ~a" expected-assistant assistant-count)]
    [else #t]))

;; ============================================================
;; check-session-tool-sequence
;; ============================================================

(define (check-session-tool-sequence log-path expected-tools)
  (define entries (session-log-entries log-path))
  (define tool-names
    (for*/list ([m (in-list entries)]
                #:when (eq? (message-role m) 'assistant)
                [p (in-list (message-content m))]
                #:when (tool-call-part? p))
      (tool-call-part-name p)))

  (cond
    [(not (= (length tool-names) (length expected-tools)))
     (format "expected ~a tool calls ~a, got ~a tool calls ~a"
             (length expected-tools)
             expected-tools
             (length tool-names)
             tool-names)]
    [(for/and ([a (in-list tool-names)]
               [b (in-list expected-tools)])
       (equal? a b))
     #t]
    [else (format "expected tool sequence ~a, got ~a" expected-tools tool-names)]))

;; ============================================================
;; check-session-tree-structure
;; ============================================================

(define (check-session-tree-structure log-path)
  (define entries (session-log-entries log-path))
  (cond
    [(null? entries) #t]
    [else
     (define ids
       (for/hash ([m (in-list entries)])
         (values (message-id m) #t)))
     (define first-parent (message-parent-id (first entries)))
     (cond
       [first-parent "first message should have #f parent"]
       [else
        (define orphan-parents
          (for/list ([m (in-list (cdr entries))]
                     #:when (and (message-parent-id m)
                                 (not (hash-has-key? ids (message-parent-id m)))))
            (message-parent-id m)))
        (if (null? orphan-parents)
            #t
            (format "orphan parent IDs: ~a" orphan-parents))])]))
