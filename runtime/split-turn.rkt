#lang racket/base

;; runtime/split-turn.rkt — Split-turn detection and handling (#690-#692)
;;
;; When a compaction window split falls in the middle of a multi-message
;; turn (user → assistant → tool_calls → tool_results → next user), we
;; must detect the split and produce a turn-prefix summary so the
;; assistant retains context about the truncated turn.
;;
;; Turn model:
;;   A "turn" starts with a user message and includes all subsequent
;;   assistant, tool-call, and tool-result messages until the next user
;;   message (or end of history).
;;
;;   Turn boundary = user-role message (or system-instruction).
;;
;; #690: detect-split-turn — check if split index falls mid-turn
;; #691: generate-turn-prefix — summarize the partial turn
;; #692: parent feature combining both

(require racket/contract
         racket/list
         racket/string
         "../util/protocol-types.rkt")

(provide
 ;; #690: Split-turn detection
 split-turn-result?
 find-split-turn
 split-turn-result-split-index
 split-turn-result-turn-start-index
 split-turn-result-turn-messages
 split-turn-result-is-split?
 ;; #691: Turn-prefix generation
 generate-turn-prefix
 ;; Turn boundary helpers
 find-turn-start
 messages-at-turn-boundary
 turn-start-index)

;; ============================================================
;; #690: Split-turn detection
;; ============================================================

;; Result of split-turn analysis.
;; split-index     : where the compaction cut was made
;; turn-start-index: index of the start of the turn containing the cut
;; turn-messages   : messages in the partial turn (from turn-start to split-index)
;; is-split?       : #t if the cut falls mid-turn (turn-start != split-index)
(struct split-turn-result (split-index
                           turn-start-index
                           turn-messages
                           is-split?)
  #:transparent)

;; Check if a message is a turn-start (user role or system-instruction kind).
(define (turn-start? msg)
  (or (eq? (message-role msg) 'user)
      (eq? (message-kind msg) 'system-instruction)))

;; Find the index of the start of the turn containing the given index.
;; Walks backward from index to find the nearest turn-start message.
(define (turn-start-index messages idx)
  (let loop ([i idx])
    (cond
      [(< i 0) 0]
      [(turn-start? (list-ref messages i)) i]
      [else (loop (sub1 i))])))

;; Find the full turn start index (exported alias for clarity).
(define (find-turn-start messages idx)
  (turn-start-index messages idx))

;; Analyze whether a split at split-index falls mid-turn.
;; A split at index N means messages[0..N) are old (to summarize)
;; and messages[N..] are recent (to keep). The split is clean if
;; messages[N] starts a new turn (user or system-instruction).
;; Returns a split-turn-result.
(define (find-split-turn messages split-index)
  (cond
    ;; Edge cases: no messages or split at end
    [(or (null? messages) (>= split-index (length messages)))
     (split-turn-result split-index split-index '() #f)]
    ;; Split at index 0 = everything is recent, no split
    [(= split-index 0)
     (split-turn-result 0 0 '() #f)]
    [else
     ;; Check if the first "recent" message starts a new turn
     (define first-recent (list-ref messages split-index))
     (define at-turn-boundary? (turn-start? first-recent))
     (cond
       [at-turn-boundary?
        ;; Clean split — no turn is truncated
        (split-turn-result split-index split-index '() #f)]
       [else
        ;; Mid-turn split — find where this partial turn starts
        (define ts-idx (turn-start-index messages split-index))
        (define turn-msgs (take (drop messages ts-idx) (- split-index ts-idx)))
        (split-turn-result split-index ts-idx turn-msgs #t)])]))

;; Adjust a split index to align with the nearest turn boundary.
;; If the split falls mid-turn, moves backward to the turn start.
;; Returns the adjusted index.
(define (messages-at-turn-boundary messages split-index)
  (define result (find-split-turn messages split-index))
  (if (split-turn-result-is-split? result)
      (split-turn-result-turn-start-index result)
      split-index))

;; ============================================================
;; #691: Turn-prefix generation
;; ============================================================

;; Generate a text summary prefix for a partial turn.
;; This captures the essence of what was happening in the truncated turn
;; so the assistant has context even though the full turn is summarized.
;;
;; The prefix includes:
;;   - Role annotations for each message
;;   - Text content (truncated per message if very long)
;;   - A structural summary header
(define (generate-turn-prefix turn-messages)
  (cond
    [(null? turn-messages) ""]
    [else
     (define parts
       (for/list ([msg (in-list turn-messages)])
         (define role (message-role msg))
         (define text (extract-message-text msg))
         (define truncated (if (> (string-length text) 500)
                               (string-append (substring text 0 500) "...")
                               text))
         (format "[~a] ~a" role truncated)))
     (string-append
      "--- TURN PREFIX (partial turn before compaction) ---\n"
      (string-join parts "\n")
      "\n--- END TURN PREFIX ---")]))

;; Extract text from a single message
(define (extract-message-text msg)
  (define content (message-content msg))
  (cond
    [(string? content) content]
    [(list? content)
     (string-append*
      (for/list ([part (in-list content)]
                 #:when (text-part? part))
        (text-part-text part)))]
    [else ""]))
