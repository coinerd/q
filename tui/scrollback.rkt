#lang racket/base
;; scrollback.rkt — Scrollback buffer for TUI

(require racket/contract
         racket/file
         racket/list
         "state.rkt"
         "../ui-core/conversation-artifact.rkt"
         "../ui-core/feature-flags.rkt"
         "../ui-core/preferences.rkt"
         "../util/json/jsonl.rkt"
         json)

;; Conversion
(provide (contract-out [transcript-entry->jsexpr (-> any/c hash?)]
                       [jsexpr->transcript-entry (-> hash? any/c)]
                       [save-scrollback (-> (listof any/c) (or/c path-string? path?) void?)]
                       [load-scrollback (-> path-string? (listof any/c))]
                       [reset-scrollback-id-counter! (-> void?)]))

;; Maximum number of scrollback entries to keep on disk.
(define scrollback-max-entries 500)

;; Global counter for assigning IDs to deserialized entries.
;; Ensures reloaded entries get unique IDs so the render cache works.
(define scrollback-id-counter (box 0))

;; Reset the counter (for test isolation).
(define (reset-scrollback-id-counter!)
  (set-box! scrollback-id-counter 0))

;; Assign a unique ID from the scrollback counter.
(define (next-scrollback-id)
  (define id (unbox scrollback-id-counter))
  (set-box! scrollback-id-counter (add1 id))
  id)

;; W3 (v1.00.02): the reasoning persistence policy (from the live user
;; preference snapshot, reasoning-visibility) gates what crosses the
;; serialization boundary:
;;   'scrollback — serialize FULL artifacts (byte bounded, as before)
;;   'session   — artifacts live in memory ONLY; they never reach disk
;;   'never     — reasoning bodies are stripped even from legacy text
;; Non-thinking artifacts (tool/error/...) are not reasoning and are
;; unaffected by the policy.
(define reasoning-session-marker "[reasoning not persisted (reasoning-visibility: session)]")
(define reasoning-never-marker "[reasoning stripped (reasoning-visibility: never)]")

;; Serialize a transcript-entry to a JSON-compatible hash.
(define (transcript-entry->jsexpr entry)
  (define policy (reasoning-visibility-policy (current-preferences)))
  (define meta (transcript-entry-meta entry))
  (define artifact (hash-ref meta 'artifact #f))
  (define persisted-artifact
    (and (conversation-artifact? artifact)
         ;; Only REASONING (thinking) artifacts are policy-gated; tool/error
         ;; artifacts are not reasoning and always serialize as before.
         (or (not (eq? (conversation-artifact-kind artifact) 'thinking)) (eq? policy 'scrollback))
         (if (eq? (conversation-artifact-kind artifact) 'thinking)
             (artifact-limit-body artifact (ui-reasoning-artifacts-max-bytes))
             artifact)))
  (define persisted-meta
    (cond
      [persisted-artifact (hash-set meta 'artifact persisted-artifact)]
      ;; The policy suppressed a reasoning artifact: strip it entirely so
      ;; the on-disk scrollback carries no reasoning payload.
      [(hash-ref meta 'artifact #f) (hash-remove meta 'artifact)]
      [else meta]))
  (define persisted-text
    (cond
      [(and persisted-artifact (eq? (conversation-artifact-kind persisted-artifact) 'thinking))
       (conversation-artifact-body persisted-artifact)]
      [(eq? (transcript-entry-kind entry) 'thinking)
       (case policy
         [(scrollback)
          ;; Legacy/raw thinking rows still cross the same persistence boundary.
          (conversation-artifact-body
           (artifact-limit-body (make-conversation-artifact #:id "scrollback-boundary"
                                                            #:session-id "legacy"
                                                            #:turn-id "legacy"
                                                            #:kind 'thinking
                                                            #:body (transcript-entry-text entry))
                                (ui-reasoning-artifacts-max-bytes)))]
         [(never) reasoning-never-marker]
         [else reasoning-session-marker])]
      [else (transcript-entry-text entry)]))
  (hasheq 'kind
          (symbol->string (transcript-entry-kind entry))
          'text
          persisted-text
          'timestamp
          (transcript-entry-timestamp entry)
          'id
          (or (transcript-entry-id entry) 0)
          'meta
          (hash->jsexpr-deep persisted-meta)))

;; Deserialize a jsexpr hash back to a transcript-entry.
;; Assigns a unique ID so the render cache can track the entry.
(define (jsexpr->transcript-entry h)
  (transcript-entry (string->symbol (hash-ref h 'kind "system"))
                    (hash-ref h 'text "")
                    (hash-ref h 'timestamp 0)
                    (jsexpr->hash-deep (hash-ref h 'meta (hash)))
                    (next-scrollback-id)))

;; ── Lifecycle audit (BUG-0001, v1.00.00) ──
;;
;; Contract: the scrollback path is SESSION-SCOPED. Callers pass
;; <base>/<session-id>/scrollback.jsonl (see tui-init.rkt
;; create-tui-session; previously this was a single global
;; <base>/scrollback.jsonl shared by every session, so each new session
;; loaded the previous session's buffer — BUG-0001).
;;
;; Save/load lifecycle, as audited at fix time:
;;   * load-scrollback is called exactly once per TUI process, at startup
;;     (load-tui-scrollback) BEFORE any buffer exists, so there is never an
;;     old in-memory buffer that could be flushed into the new session's
;;     file at load time.
;;   * save-scrollback is called exactly once per TUI process, at exit
;;     (run-tui-loop cleanup), always AFTER the loop ends; with a
;;     per-session path the buffer can only ever be flushed into the
;;     session that produced it.
;;   * In-process session switches (the /go campaign runner) do not load or
;;     save scrollback; the buffer belongs to the interactive session and
;;     is flushed at exit into that session's own per-session file.
;;   * Missing file at load (first-ever session) returns '() — tolerated
;;     without error; jsonl-read-last yields '() for absent files.

;; Save transcript-entries to a JSONL file.
;; Atomically rewrites with only the last scrollback-max-entries entries
;; to prevent unbounded file growth.
;; Accepts both string? and path? for the path argument.
(define (save-scrollback entries path)
  (define path-str
    (if (path? path)
        (path->string path)
        path))
  ;; ui-state transcript order is newest-first; retain the newest prefix.
  (define trimmed (take entries (min (length entries) scrollback-max-entries)))
  (define jsexprs (map transcript-entry->jsexpr trimmed))
  ;; Atomic rewrite: write to temp then rename
  (define tmp-path (string-append path-str ".tmp"))
  (call-with-output-file tmp-path
                         (lambda (out)
                           (for ([entry (in-list jsexprs)])
                             (write-json entry out)
                             (newline out)))
                         #:mode 'text
                         #:exists 'replace)
  (rename-file-or-directory tmp-path path-str #t))

;; Load transcript-entries from a JSONL file.
;; Returns '() if the file does not exist.
(define (load-scrollback path)
  ;; Persisted order is newest-first, so reading the last lines would discard
  ;; the newest entries in an oversized legacy file.
  (define raw (jsonl-read-all-valid path))
  (map jsexpr->transcript-entry (take raw (min (length raw) scrollback-max-entries))))

;; Deep hash → nested jsexpr conversion (handles nested hashes)
(define (value->jsexpr-deep value)
  (cond
    [(conversation-artifact? value) (artifact->jsexpr value)]
    [(hash? value) (hash->jsexpr-deep value)]
    [(list? value) (map value->jsexpr-deep value)]
    [else value]))

(define (hash->jsexpr-deep h)
  (for/hash ([(k v) (in-hash h)])
    (values k (value->jsexpr-deep v))))

;; Deep jsexpr → nested hash conversion.  Artifact hashes are schema checked
;; here, at the scrollback boundary, and restored to canonical live structs.
(define (jsexpr->hash-deep value)
  (cond
    [(and (hash? value) (equal? (hash-ref value 'schema #f) "conversation-artifact"))
     (jsexpr->artifact value)]
    [(hash? value)
     (for/hash ([(k v) (in-hash value)])
       (values k (jsexpr->hash-deep v)))]
    [(list? value) (map jsexpr->hash-deep value)]
    [else value]))
