#lang racket/base

;; runtime/compactor.rkt — history summarization and compaction
;;
;; Provides:
;;   compaction-strategy          — struct: summary-window-size, keep-recent-count
;;   compaction-result            — struct: summary-message, removed-count, kept-messages
;;   compact-history              — summarize history and produce compaction entries (ADVISORY-ONLY)
;;   compact-and-persist!         — compact history AND write summary entry to session log
;;   build-summary-window         — determine what to summarize vs what to keep recent
;;   write-compaction-entry!      — write a compaction entry to session log
;;   compaction-result->message-list — flatten result into a single message list
;;   default-strategy             — default compaction strategy
;;   default-summarize            — simple concatenation summarizer
;;
;; Key invariant: compaction NEVER silently destroys prior history.
;; The original log remains reconstructible. compact-history produces
;; a NEW message list without modifying or deleting the originals.

(require racket/contract
         racket/match
         racket/string
         racket/list
         racket/set
         "../util/protocol-types.rkt"
         "../util/message-helpers.rkt"
         (only-in "../util/telemetry.rkt" with-telemetry)
         "../runtime/session-store.rkt"
         (only-in "../runtime/compaction-prompts.rkt"
                  format-messages-for-summary
                  MAX-TOOL-RESULT-CHARS
                  MAX-SUMMARY-WORDS)
         (only-in "../util/hook-types.rkt"
                  hook-result
                  hook-result?
                  hook-result-action
                  hook-result-payload)
         (only-in "token-compaction.rkt"
                  build-token-summary-window
                  default-token-compaction-config
                  token-compaction-config)
         (only-in "split-turn.rkt"
                  find-split-turn
                  split-turn-result-is-split?
                  split-turn-result-turn-messages
                  generate-turn-prefix))

(provide compaction-strategy
         compaction-strategy?
         compaction-strategy-summary-window-size
         compaction-strategy-keep-recent-count
         compaction-result
         compaction-result?
         struct:compaction-result
         compaction-result-summary-message
         compaction-result-removed-count
         compaction-result-kept-messages
         ;; H-02: Contract-wrapped compaction functions
         (contract-out [compact-history
                        (->* (list?)
                             (#:summarize-fn procedure?
                                             #:previous-summary (or/c string? #f)
                                             #:hook-dispatcher (or/c procedure? #f)
                                             #:token-config any/c)
                             compaction-result?)]
                       [compact-and-persist!
                        (->* (list? (or/c path-string? #f))
                             (#:summarize-fn procedure?
                                             #:previous-summary (or/c string? #f)
                                             #:hook-dispatcher (or/c procedure? #f)
                                             #:token-config any/c)
                             compaction-result?)]
                       [build-summary-window (-> list? compaction-strategy? (values list? list?))]
                       [write-compaction-entry! (-> (or/c path-string? #f) compaction-result? void?)]
                       [compaction-result->message-list (-> compaction-result? list?)]
                       [default-strategy (-> compaction-strategy?)]
                       [default-summarize (-> list? string?)]
                       [extract-file-tracker (-> list? hash?)]
                       [find-previous-file-tracker (-> list? hash?)]
                       [merge-file-trackers (->* () () #:rest list? hash?)]))

;; ============================================================
;; Structs
;; ============================================================

;; Compaction strategy parameters
(struct compaction-strategy (summary-window-size keep-recent-count) #:transparent)
;; summary-window-size : integer — max number of past messages to summarize
;; keep-recent-count   : integer — how many recent messages to keep as-is

;; Compaction result
(struct compaction-result (summary-message removed-count kept-messages) #:transparent)
;; summary-message : (or/c message? #f) — a single 'system message with the summary text, or #f if nothing was summarized
;; removed-count   : integer — how many messages were replaced by the summary
;; kept-messages   : (listof message?) — the recent messages that were kept verbatim

;; ============================================================
;; Default strategy and summarize function
;; ============================================================

(define (default-strategy)
  (compaction-strategy 50 20))

(define (default-summarize messages)
  ;; Simple summarization: produce a structured summary string.
  ;; Concatenates the text content of each message.
  ;; In production, this would call the LLM to produce a real summary.
  (define n (length messages))
  (define parts
    (for/list ([m (in-list messages)])
      (define content (message-content m))
      (define text
        (string-join (for/list ([part (in-list content)]
                                #:when (text-part? part))
                       (text-part-text part))
                     " "))
      (format "[~a] ~a" (message-role m) text)))
  (format "[Compaction summary of ~a messages]\n~a" n (string-join parts "\n")))

;; ============================================================
;; LLM-powered summarization (v0.8.9)
;; ============================================================

;; Walk messages to find file paths from tool calls.
;; Returns a hash: 'readFiles -> list of paths,
;;                  'modifiedFiles -> list of paths.
(define (extract-file-tracker messages)
  (define reads (mutable-set))
  (define writes (mutable-set))
  (for ([m (in-list messages)])
    (define content (message-content m))
    (for ([part (in-list content)])
      (cond
        [(tool-call-part? part)
         (define tool-name (tool-call-part-name part))
         (define args (tool-call-part-arguments part))
         (define path
           (cond
             [(hash? args) (hash-ref args 'path #f)]
             [(string? args)
              ;; Try to extract path from JSON string
              (and (string-contains? args "path")
                   (let ([m (regexp-match #rx"\"path\"[[:space:]]*:[[:space:]]*\"([^\"]+)\"" args)])
                     (and m (cadr m))))]
             [else #f]))
         (when path
           (cond
             [(member tool-name '("read" "find" "grep" "ls")) (set-add! reads path)]
             [(member tool-name '("edit" "write")) (set-add! writes path)]))]
        ;; Tool results may contain file paths in content
        [(tool-result-part? part) (void)])))
  (hasheq 'readFiles (set->list reads) 'modifiedFiles (set->list writes)))

;; Find the most recent compaction-summary message in the list.
;; Returns the text of the summary or #f.
(define (find-previous-summary messages)
  (for/first ([m (in-list (reverse messages))]
              #:when (eq? (message-kind m) 'compaction-summary))
    (string-join (for/list ([part (in-list (message-content m))]
                            #:when (text-part? part))
                   (text-part-text part))
                 "")))

;; #768: Find the file tracker from the most recent compaction summary.
;; Returns a hash like (hasheq 'readFiles (...) 'modifiedFiles (...)) or (hasheq).
(define (find-previous-file-tracker messages)
  (or (for/first ([m (in-list (reverse messages))]
                  #:when (eq? (message-kind m) 'compaction-summary))
        (define meta (message-meta m))
        (hash-ref meta 'fileTracker (hasheq)))
      (hasheq)))

;; #768: Merge two file trackers, deduplicating file paths.
(define (merge-file-trackers . trackers)
  (define all-reads (mutable-set))
  (define all-writes (mutable-set))
  (for ([ft (in-list trackers)])
    (for ([path (in-list (hash-ref ft 'readFiles '()))])
      (set-add! all-reads path))
    (for ([path (in-list (hash-ref ft 'modifiedFiles '()))])
      (set-add! all-writes path)))
  (hasheq 'readFiles (set->list all-reads) 'modifiedFiles (set->list all-writes)))

;; ============================================================
;; build-summary-window
;; ============================================================

;; Split messages into old (to summarize) and recent (to keep).
;; The "old" portion is further capped at summary-window-size.
;; Returns: (values old-messages recent-messages)
(define (build-summary-window messages strategy)
  (define total (length messages))
  (define keep-recent (compaction-strategy-keep-recent-count strategy))
  (define window-size (compaction-strategy-summary-window-size strategy))
  (match (<= total keep-recent)
    ;; All messages fit in the "recent" window — nothing to summarize
    [#t (values '() messages)]
    [_
     (define split-point (max 0 (- total keep-recent)))
     (define all-old (take messages split-point))
     (define recent (drop messages split-point))
     ;; Cap the old messages at summary-window-size
     ;; (take the LAST window-size of the old messages, to summarize the most recent of the old)
     (define old-to-summarize
       (if (> (length all-old) window-size)
           (take-right all-old window-size)
           all-old))
     (values old-to-summarize recent)]))

;; ============================================================
;; Inline cutpoint adjustment (avoids circular dep with cutpoint-rules.rkt)
;; ============================================================

;; Check if a message contains tool-call parts.
;; has-tool-calls? and tool-result-message? now imported from util/message-helpers.rkt

;; Check if placing a cut BEFORE index `idx` is valid.
;; A cut at N means messages[0..N) are old, messages[N..] are recent.
(define (valid-cutpoint? messages idx)
  (cond
    [(or (< idx 0) (> idx (length messages))) #f]
    [(or (= idx 0) (= idx (length messages))) #t]
    [else
     (define msg-at (list-ref messages idx))
     (cond
       [(tool-result-message? msg-at) #f]
       [(eq? (message-role msg-at) 'user) #t]
       [(eq? (message-role msg-at) 'assistant)
        (define msg-before (list-ref messages (sub1 idx)))
        (not (has-tool-calls? msg-before))]
       [else #t])]))

;; Adjust a proposed cut point backward to nearest valid position.
(define (adjust-cutpoint messages proposed-idx)
  (match (valid-cutpoint? messages proposed-idx)
    [#t proposed-idx]
    [_
     (let loop ([i (sub1 proposed-idx)])
       (cond
         [(<= i 0) 0]
         [(valid-cutpoint? messages i) i]
         [else (loop (sub1 i))]))]))

;; ============================================================
;; compact-history
;; ============================================================

;; Main compaction function.
;; Returns a compaction-result. Does NOT modify the original log.
;;
;; CONTRACT: This function is ADVISORY-ONLY. It does NOT modify the session log
;; or touch the filesystem in any way. To persist the compaction, use
;; `compact-and-persist!` or call `write-compaction-entry!` explicitly.
;;
;; v0.8.9: Added #:hook-dispatcher for extension hooks,
;;         #:previous-summary for iterative compaction.
;; v0.9.1 (#636): Hook blocking now actually prevents compaction.
;; M-05: Removed #:provider/#:model-name. Use #:summarize-fn with
;;        make-llm-summarize-fn from compactor-llm-bridge.rkt instead.
(define (compact-history messages
                         #:summarize-fn [summarize-fn default-summarize]
                         #:previous-summary [prev-summary #f]
                         #:hook-dispatcher [hook-dispatcher #f]
                         #:token-config [token-config (default-token-compaction-config)]
                         #:now [now-fn current-inexact-milliseconds])
  ;; Dispatch 'session-before-compact hook if dispatcher provided
  ;; Returns identity result if hook blocks, otherwise proceeds with compaction.
  ;; #769: Hook can provide custom summary via amend action.
  (define hook-res
    (and hook-dispatcher
         (hook-dispatcher 'session-before-compact (hasheq 'message-count (length messages)))))
  (cond
    [(and (hook-result? hook-res) (eq? (hook-result-action hook-res) 'block))
     ;; Hook blocked compaction — return identity result immediately
     (compaction-result #f 0 messages)]
    [else
     ;; Use token-based window split instead of fixed-count
     (define-values (old recent) (build-token-summary-window messages token-config))
     ;; Adjust cutpoint to a valid boundary (e.g., don't split mid-tool-call)
     (define-values (adjusted-old adjusted-recent)
       (if (and (pair? old) (pair? recent))
           (let ([cut-idx (adjust-cutpoint messages (length old))])
             (if (and cut-idx (> cut-idx 0) (< cut-idx (length messages)))
                 (values (take messages cut-idx) (drop messages cut-idx))
                 (values old recent)))
           (values old recent)))
     (match adjusted-old
       ;; Nothing to summarize — return identity result
       ['() (compaction-result #f 0 adjusted-recent)]
       [_
        ;; #767: Detect split-turn — if cut falls mid-turn, generate turn prefix
        (define turn-prefix
          (let ([split-result (find-split-turn messages (length adjusted-old))])
            (if (split-turn-result-is-split? split-result)
                (generate-turn-prefix (split-turn-result-turn-messages split-result))
                "")))
        ;; #768: Build cumulative file tracker merging current batch with previous compaction
        (define current-ft (extract-file-tracker adjusted-old))
        (define previous-ft (find-previous-file-tracker adjusted-recent))
        (define cumulative-ft (merge-file-trackers current-ft previous-ft))
        ;; #769: Check for hook-provided custom summary
        (define custom-summary
          (and (hook-result? hook-res)
               (eq? (hook-result-action hook-res) 'amend)
               (let ([payload (hook-result-payload hook-res)])
                 (and (hash? payload) (hash-ref payload 'summary #f)))))
        ;; Build summarization — use custom summary if provided, else standard path
        ;; M-05: Always use summarize-fn (no more direct LLM coupling)
        (define base-summary-text (or custom-summary (summarize-fn adjusted-old)))
        ;; Append turn prefix to summary if split-turn detected
        (define summary-text
          (if (string=? turn-prefix "")
              base-summary-text
              (string-append base-summary-text "\n\n" turn-prefix)))
        ;; Build file tracker metadata for the summary message
        (define file-tracker-meta
          (if (> (hash-count cumulative-ft) 0)
              (hasheq 'fileTracker cumulative-ft)
              (hasheq)))
        (define first-kept-id
          (if (pair? adjusted-recent)
              (message-id (car adjusted-recent))
              #f))
        (define summary-msg
          (make-message (format "compaction-~a" (now-fn))
                        #f ; no parent — compaction summaries are root-level context
                        'system
                        'compaction-summary
                        (list (make-text-part summary-text))
                        (current-seconds)
                        (hash-set (hash-set (hash-set file-tracker-meta 'type "compaction")
                                            'removedCount
                                            (length adjusted-old))
                                  'firstKeptEntryId
                                  first-kept-id)))
        (compaction-result summary-msg (length adjusted-old) adjusted-recent)])]))

;; ============================================================
;; compaction-result->message-list
;; ============================================================

;; Flatten a compaction-result into a single message list.
;; If a summary exists, prepends it before the kept messages.
(define (compaction-result->message-list result)
  (define summary (compaction-result-summary-message result))
  (define kept (compaction-result-kept-messages result))
  (if summary
      (cons summary kept)
      kept))

;; ============================================================
;; compact-and-persist!
;; ============================================================

;; Compacts history AND persists the summary entry to the session log.
;; Returns the compaction-result for inspection.
;; This is the "persisting" variant — the summary entry becomes part of
;; the durable session history.
;; The original messages are NOT deleted — they remain in the log.
;; Future context assembly can use the summary + recent messages
;; instead of replaying the full history.
(define (compact-and-persist! messages
                              session-log-path
                              #:summarize-fn [summarize-fn default-summarize]
                              #:previous-summary [prev-summary #f]
                              #:hook-dispatcher [hook-dispatcher #f]
                              #:token-config [token-config (default-token-compaction-config)])
  (with-telemetry "compact-and-persist"
                  (let ([result (compact-history messages
                                                 #:summarize-fn summarize-fn
                                                 #:previous-summary prev-summary
                                                 #:hook-dispatcher hook-dispatcher
                                                 #:token-config token-config)])
                    (write-compaction-entry! session-log-path result)
                    result)))

;; ============================================================
;; write-compaction-entry!
;; ============================================================

;; Write a compaction-summary entry to the session log.
;; The entry records what was summarized. Original entries remain intact.
;; If no compaction happened (summary is #f), this is a no-op.
(define (write-compaction-entry! session-path compaction-res)
  (define summary (compaction-result-summary-message compaction-res))
  (when summary
    (append-entry! session-path summary))
  (void))
;; v0.31.x milestone placeholder
