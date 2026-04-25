#lang racket/base

;; q/tools/builtins/session-recall.rkt — Session Recall Tool (#1391)
;;
;; Lets the agent retrieve earlier session entries that are not in
;; the current LLM context window. The session log is immutable —
;; this tool reads from the session-index.
;;
;; Three retrieval modes:
;;   1. entry_ids — return specific messages by ID
;;   2. query     — text search over all entries
;;   3. range     — return entries between two IDs (inclusive)
;;
;; Results are ephemeral: returned as a tool result for this turn only.
;; Max 5 entries per call to prevent context bloat.

(require racket/function
         racket/list
         racket/string
         "../tool.rkt"
         (only-in "../../util/protocol-types.rkt"
                  message-id
                  message-role
                  message-content
                  text-part?
                  text-part-text))

(provide tool-session-recall)

;; Max entries returned per recall call
(define MAX-RECALL-ENTRIES 5)

;; ── Transparent struct accessors (no runtime import) ───────
;; session-index struct fields: by-id, children, entry-order,
;;   bookmarks, active-leaf-id, bookmark-sem
;; struct->vector indices: 0=name, 1=by-id, 2=children, 3=entry-order

(define (idx-by-id idx)
  (vector-ref (struct->vector idx) 1))

(define (idx-entry-order idx)
  (vector-ref (struct->vector idx) 3))

(define (lookup-entry idx id)
  (hash-ref (idx-by-id idx) id #f))

;; ============================================================
;; Tool definition
;; ============================================================

(define (tool-session-recall args [exec-ctx #f])
  (define idx
    (and exec-ctx
         (exec-context-session-metadata exec-ctx)
         (hash-ref (exec-context-session-metadata exec-ctx) 'session-index #f)))

  (cond
    [(not idx) (make-error-result "session_recall: no session index available")]
    [else
     (define entry-ids (hash-ref args 'entry_ids #f))
     (define query (hash-ref args 'query #f))
     (define range (hash-ref args 'range #f))
     (cond
       [(and entry-ids (list? entry-ids) (not (null? entry-ids))) (recall-by-ids idx entry-ids)]
       [(and query (string? query) (not (string=? query ""))) (recall-by-query idx query)]
       [(and range (hash? range)) (recall-by-range idx range)]
       [else (make-error-result "session_recall: provide one of: entry_ids, query, or range")])]))

;; ============================================================
;; Recall by IDs
;; ============================================================

(define (recall-by-ids idx ids)
  (define entries
    (for/list ([id (in-list ids)]
               [i (in-naturals)]
               #:break (>= i MAX-RECALL-ENTRIES))
      (define entry (lookup-entry idx id))
      (cond
        [entry entry]
        [else #f])))
  (define found (filter identity entries))
  (cond
    [(null? found) (make-error-result "session_recall: no entries found for given IDs")]
    [else
     (define text (format-recalled-entries found))
     (make-success-result (list (hasheq 'type "text" 'text text)))]))

;; ============================================================
;; Recall by query (text search)
;; ============================================================

(define (recall-by-query idx query)
  (define query-low (string-downcase query))
  (define all-entries (vector->list (idx-entry-order idx)))
  (define matches
    (for/list ([m (in-list all-entries)]
               [i (in-naturals)]
               #:break (>= i MAX-RECALL-ENTRIES)
               #:when (string-contains? (string-downcase (extract-msg-text m)) query-low))
      m))
  (cond
    [(null? matches)
     (make-success-result
      (list (hasheq 'type "text" 'text (format "No matching entries found for query: ~a" query))))]
    [else
     (define text (format-recalled-entries matches))
     (make-success-result (list (hasheq 'type "text" 'text text)))]))

;; ============================================================
;; Recall by range
;; ============================================================

(define (recall-by-range idx range)
  (define from-id (hash-ref range 'from #f))
  (define to-id (hash-ref range 'to #f))
  (cond
    [(not (and from-id to-id))
     (make-error-result "session_recall: range requires 'from' and 'to' IDs")]
    [else
     (define all-entries (vector->list (idx-entry-order idx)))
     ;; Walk entries, collecting those between from-id and to-id inclusive
     (define matches
       (let loop ([remaining all-entries]
                  [started #f]
                  [done #f]
                  [acc '()])
         (cond
           [done (reverse acc)]
           [(null? remaining) (reverse acc)]
           [else
            (define m (car remaining))
            (cond
              [(equal? (message-id m) from-id) (loop (cdr remaining) #t #f (cons m acc))]
              [(and started (equal? (message-id m) to-id)) (reverse (cons m acc))]
              [started (loop (cdr remaining) #t #f (cons m acc))]
              [else (loop (cdr remaining) #f #f acc)])])))
     (cond
       [(null? matches) (make-error-result "session_recall: no entries found in given range")]
       [else
        (define text (format-recalled-entries matches))
        (make-success-result (list (hasheq 'type "text" 'text text)))])]))

;; ============================================================
;; Formatting
;; ============================================================

(define (format-recalled-entries entries)
  (define lines
    (for/list ([m (in-list entries)])
      (format "Entry ~a (~a):\n  ~a"
              (message-id m)
              (message-role m)
              (truncate-string (extract-msg-text m) 200))))
  (string-append "[Recalled Session Entries]\n\n" (string-join lines "\n\n")))

(define (extract-msg-text msg)
  (define parts (message-content msg))
  (define texts
    (for/list ([part (in-list parts)]
               #:when (text-part? part))
      (text-part-text part)))
  (string-join texts " "))

(define (truncate-string s max-len)
  (if (<= (string-length s) max-len)
      s
      (string-append (substring s 0 (- max-len 3)) "...")))
