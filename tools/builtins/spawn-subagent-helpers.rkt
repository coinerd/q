#lang racket/base

;; tools/builtins/spawn-subagent-helpers.rkt — Pure functions for spawn-subagent
;;
;; W2 v0.99.35: Extracted from spawn-subagent.rkt to create a clean boundary
;; between pure request/result normalization and effectful process orchestration.
;;
;; All functions in this module are pure (no I/O, no mutation, no parameters).
;; They can be tested in isolation without mocking providers or event buses.
;;
;; Boundary contract:
;;   INPUTS:  Plain data (hashes, lists, strings, symbols, message structs)
;;   OUTPUTS: Plain data (strings, lists of symbols, normalized hashes)
;;   EFFECTS: None — safe to call from any context

(require racket/string
         racket/list
         (only-in file/sha1 sha1)
         (only-in "../../util/json/checksum.rkt" sha256-string)
         (only-in "../../util/capability.rkt" valid-capability?)
         (only-in "../../util/message/message.rkt" make-message message-role message-content)
         (only-in "../../util/content/content-parts.rkt"
                  text-part?
                  text-part-text
                  tool-call-part?
                  tool-call-part-name))

(provide normalize-capabilities
         normalize-capabilities/strict
         immutable-canonical-copy
         canonical-datum-string
         sha256-digest
         requires-hitl-approval?
         bounded-delegated-capabilities
         valid-plan-id?
         extract-assistant-text
         extract-text-summary
         SUBAGENT-SUMMARY-MAX-CHARS
         classify-terminal-status
         make-safe-result-metadata
         result-has-content?)

;; ============================================================
;; Capability normalization
;; ============================================================

;; Normalize raw capability input to a validated list of symbols, or #f.
;;
;; Accepts: #f, '(), a single string/symbol, or a list of strings/symbols.
;; Returns: #f when no valid capabilities remain, or a list of symbols.
;;
;; This deduplicates the logic previously inlined in both:
;;   - parse-subagent-config (spawn-subagent.rkt)
;;   - parse-job-capabilities (spawn-subagent.rkt)
(define (normalize-capabilities caps-raw)
  (cond
    [(not caps-raw) #f]
    [(null? caps-raw) #f]
    [(or (string? caps-raw) (symbol? caps-raw))
     (define sym
       (if (string? caps-raw)
           (string->symbol caps-raw)
           caps-raw))
     (if (valid-capability? sym)
         (list sym)
         #f)]
    [(list? caps-raw)
     (define filtered
       (filter valid-capability?
               (map (lambda (c)
                      (if (string? c)
                          (string->symbol c)
                          c))
                    caps-raw)))
     (if (null? filtered) #f filtered)]
    [else #f]))

;; Strict variant for an explicitly supplied batch capability declaration.
;; Unlike the legacy normalizer, it rejects the complete value when even one
;; member is malformed or unknown.  Omitted declarations never call this
;; function, preserving the legacy #f/unrestricted behavior.
(define (normalize-capabilities/strict caps-raw)
  (define raw-list
    (cond
      [(not caps-raw) '()]
      [(or (string? caps-raw) (symbol? caps-raw)) (list caps-raw)]
      [(list? caps-raw) caps-raw]
      [else
       (raise-argument-error 'normalize-capabilities/strict
                             "a capability string, symbol, list, or #f"
                             caps-raw)]))
  (define normalized
    (for/list ([cap (in-list raw-list)])
      (define sym
        (cond
          [(string? cap) (string->symbol cap)]
          [(symbol? cap) cap]
          [else
           (raise-argument-error 'normalize-capabilities/strict
                                 "a list containing only capability strings or symbols"
                                 caps-raw)]))
      (unless (and (valid-capability? sym) (not (eq? sym 'any)))
        (raise-argument-error 'normalize-capabilities/strict
                              "a list containing only concrete delegated capabilities"
                              caps-raw))
      sym))
  ;; Preserve explicit empty authority as '().  Omission is represented by #f
  ;; by the caller, so an empty declaration can never become unrestricted.
  (remove-duplicates normalized eq?))

(define DEFAULT-DELEGATED-CAPABILITIES '(read-only file-write shell-exec))

(define (bounded-delegated-capabilities declared-caps parent-capabilities)
  (define parent-unrestricted? (and (list? parent-capabilities) (memq 'any parent-capabilities)))
  (define requested (if (eq? declared-caps #f) DEFAULT-DELEGATED-CAPABILITIES declared-caps))
  (cond
    [(or parent-unrestricted? (not (list? parent-capabilities))) requested]
    [(eq? declared-caps #f)
     (filter (lambda (capability) (memq capability parent-capabilities)) requested)]
    [else
     (define excess
       (filter (lambda (capability) (not (memq capability parent-capabilities))) requested))
     (unless (null? excess)
       (error 'spawn-subagent "delegated capabilities exceed parent session authority: ~a" excess))
     requested]))

(define (valid-plan-id? value)
  (and (string? value) (regexp-match? #px"^[A-Za-z0-9][A-Za-z0-9._:-]{0,127}$" value)))

;; ============================================================
;; Immutable canonical request snapshots
;; ============================================================

;; Deep-copy request data into immutable containers.  Strings and bytes are
;; copied as immutable values so later mutation of the original request cannot
;; alter an approval snapshot.
(define (immutable-canonical-copy value)
  (cond
    [(hash? value)
     (for/hash ([(key item) (in-hash value)])
       (values (immutable-canonical-copy key) (immutable-canonical-copy item)))]
    [(list? value) (map immutable-canonical-copy value)]
    [(pair? value)
     (cons (immutable-canonical-copy (car value)) (immutable-canonical-copy (cdr value)))]
    [(vector? value)
     (vector->immutable-vector (for/vector ([item (in-vector value)])
                                 (immutable-canonical-copy item)))]
    [(string? value) (string->immutable-string (string-copy value))]
    [(bytes? value) (bytes->immutable-bytes (bytes-copy value))]
    [(box? value) (box-immutable (immutable-canonical-copy (unbox value)))]
    [else value]))

;; Deterministic, type-tagged encoding used only for identity digests.  Hash
;; entries are sorted by their encoded key; list order remains significant.
(define (canonical-datum-string value)
  (define (encode item)
    (cond
      [(hash? item)
       (define entries
         (sort (for/list ([(key val) (in-hash item)])
                 (cons (encode key) (encode val)))
               string<?
               #:key car))
       (string-append "h{"
                      (apply string-append
                             (for/list ([entry (in-list entries)])
                               (format "~a=~a;" (car entry) (cdr entry))))
                      "}")]
      [(list? item) (string-append "l[" (apply string-append (map encode item)) "]")]
      [(pair? item) (format "p(~a.~a)" (encode (car item)) (encode (cdr item)))]
      [(vector? item)
       (string-append "v["
                      (apply string-append
                             (for/list ([part (in-vector item)])
                               (encode part)))
                      "]")]
      [(string? item) (format "s~s" item)]
      [(symbol? item) (format "y~s" item)]
      [(bytes? item) (format "b~s" item)]
      [(boolean? item) (if item "t" "f")]
      [(number? item) (format "n~s" item)]
      [(void? item) "zvoid"]
      [else (format "o~s" item)]))
  (encode value))

;; SHA-256 for exact task strings and deterministic canonical SHA-256 for
;; structured snapshots/raw requests.
(define (sha256-digest value)
  (string->immutable-string (sha256-string (if (string? value)
                                               value
                                               (canonical-datum-string value)))))

;; ============================================================
;; HITL approval check
;; ============================================================

;; Check if subagent capabilities require HITL approval.
;; Returns #t when capabilities include shell-exec or git-write.
;; Returns #f for #f, '(), or read-only/file-write-only capabilities.
(define (requires-hitl-approval? capabilities)
  (and (list? capabilities)
       (pair? capabilities)
       (or (memq 'shell-exec capabilities) (memq 'git-write capabilities))))

;; ============================================================
;; Result text extraction
;; ============================================================

;; Extract text content from assistant messages in a message list.
;;
;; For each assistant message:
;;   - String content → the string
;;   - List content → extract strings and text-parts, join with \n
;;   - Tool-call-only messages → summarize tool names as [called: tool1, tool2]
;;
;; Non-assistant messages (system, user, tool) are skipped.
;; Multiple assistant messages are joined with \n.
(define (extract-assistant-text messages)
  (string-join (for/list ([m (in-list messages)]
                          #:when (eq? (message-role m) 'assistant))
                 (define content (message-content m))
                 (cond
                   [(string? content) content]
                   [(list? content)
                    (define strings-only
                      (for/list ([c (in-list content)]
                                 #:when (string? c))
                        c))
                    (define text-parts
                      (for/list ([c (in-list content)]
                                 #:when (text-part? c))
                        (text-part-text c)))
                    (define tool-parts
                      (for/list ([c (in-list content)]
                                 #:when (tool-call-part? c))
                        (tool-call-part-name c)))
                    (define all-text (append strings-only text-parts))
                    (cond
                      [(pair? all-text) (string-join all-text "\n")]
                      [(pair? tool-parts) (format "[called: ~a]" (string-join tool-parts ", "))]
                      [else ""])]
                   [else (format "~a" content)]))
               "\n"))

;; ============================================================
;; Text summary truncation
;; ============================================================

;; Maximum characters in subagent result summaries.
;; Bug fix: Raised from 200→4000 chars — the old 200-char limit destroyed
;; nearly all useful content from subagent tasks.
(define SUBAGENT-SUMMARY-MAX-CHARS 4000)

;; Extract a text summary from tool result content.
;; Truncates to max-chars characters with ellipsis.
(define (extract-text-summary content [max-chars SUBAGENT-SUMMARY-MAX-CHARS])
  (define full-text
    (string-join (for/list ([c (in-list (if (list? content)
                                            content
                                            '()))])
                   (cond
                     [(and (hash? c) (hash-ref c 'text #f)) (hash-ref c 'text "")]
                     [(string? c) c]
                     [else ""]))
                 "\n"))
  (if (> (string-length full-text) max-chars)
      (string-append (substring full-text 0 (- max-chars 3)) "...")
      full-text))

;; ============================================================
;; Typed terminal outcomes (v0.99.50 W1 — TMUX-04)
;; ============================================================

;; Classify a raw loop final-status into a canonical typed terminal outcome.
;;
;; Raw statuses from run-subagent-loop:
;;   'complete             → subagent finished normally
;;   'max-turns-reached    → ran out of turns
;;   'cancelled            → HITL-denied (transport-ineligible, W2)
;;   (other symbols)       → unexpected loop exit
;;
;; Returns one of these canonical typed symbols:
;;   'completed            — finished with non-empty result text
;;   'approved-empty       — finished but produced empty result
;;   'timed-out            — hit max turns without completing
;;   'failed               — unexpected loop exit
;;   'cancelled            — HITL-denied
;;
;; This function is pure — it takes the raw status symbol and a
;; result-has-content? boolean, returning the canonical typed outcome.
(define (classify-terminal-status raw-status has-content?)
  (cond
    [(eq? raw-status 'complete) (if has-content? 'completed 'approved-empty)]
    [(eq? raw-status 'max-turns-reached) 'timed-out]
    [(eq? raw-status 'cancelled) 'cancelled]
    [else 'failed]))

;; Check whether result text is non-empty (after trimming).
(define (result-has-content? text)
  (and (string? text) (> (string-length (string-trim text)) 0)))

;; Build safe trace metadata for a subagent result.
;; Logs presence, size, SHA-1 digest, IDs, and status WITHOUT logging
;; unrestricted child content.
;;
;; All inputs are plain data; output is a plain hash suitable for JSON.
;; v0.99.50 W1 (TMUX-04): Adds typed 'terminal-status while keeping
;; legacy 'status (raw loop status) for backward compatibility.
(define (make-safe-result-metadata result-text session-id terminal-status [raw-status #f])
  (define trimmed (or (and (string? result-text) result-text) ""))
  (define size (string-length trimmed))
  (define digest (sha1 (open-input-string trimmed)))
  (define base
    (hasheq 'result-present?
            (result-has-content? trimmed)
            'content-size
            size
            'content-digest
            digest
            'session-id
            (or session-id "")
            'terminal-status
            (symbol->string terminal-status)))
  (if raw-status
      (hash-set base 'status (symbol->string raw-status))
      base))
