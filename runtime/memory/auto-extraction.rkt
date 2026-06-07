#lang racket/base
;; runtime/memory/auto-extraction.rkt — Post-response memory extraction
;;
;; v0.95.10: Optional automatic extraction of reusable facts from agent responses.
;; Disabled by default. Must be explicitly enabled via parameter or session config.
;;
;; Safety:
;;   - Secret patterns are blocked (API keys, tokens, passwords, private keys)
;;   - Raw tool output, entire file contents, and credentials are never stored
;;   - Every candidate emits memory.stored or memory.policy.blocked event
;;   - Extraction failure does NOT fail the agent turn
;;   - Only low-risk semantic/procedural facts are stored

(require racket/string
         "types.rkt"
         "protocol.rkt"
         (only-in "policy.rkt"
                  policy-allows-store?
                  redact-memory-content
                  default-blocked-content-patterns
                  effective-memory-scope
                  memory-persistent-write-allowed?)
         (only-in "backends/helpers.rkt" current-iso-8601) ; F32
         (only-in "service.rkt" current-memory-backend current-memory-policy)
         (only-in "../../agent/event-structs/memory-events.rkt"
                  make-mem-item-stored-event
                  make-mem-policy-blocked-event
                  make-mem-store-requested-event)) ; F13

;; ---------------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------------

;; Master switch — disabled by default
(define current-auto-extraction-enabled (make-parameter #f))

;; F10/M13-F7: Deterministic feature-based confidence estimator.
;; Replaces naive length-only heuristic with multi-factor scoring.
;; Factors: conciseness, factual indicators, noise penalty, length cap.
(define (estimate-confidence content)
  (cond
    [(not (string? content)) 0.1]
    [(< (string-length content) 15) 0.2] ; too short to be meaningful
    [else
     (define len (string-length content))
     (define lower (string-downcase content))
     ;; Base confidence from length sweet spot
     (define base
       (cond
         [(<= len 50) 0.55]
         [(<= len 150) 0.7]
         [(<= len 300) 0.6]
         [else 0.35]))
     ;; Concise factual boost: ends with period, no question marks
     (define factual-boost
       (cond
         [(regexp-match? #rx"\\?$" content) -0.15] ; question → not a fact
         [(regexp-match? #rx"\\.$" content) 0.1] ; declarative → good
         [else 0.0]))
     ;; Project/process keyword boost
     (define keyword-boost
       (if (or (string-contains? lower "config")
               (string-contains? lower "project")
               (string-contains? lower "command")
               (string-contains? lower "function")
               (string-contains? lower "module")
               (string-contains? lower "error")
               (string-contains? lower "file"))
           0.1
           0.0))
     ;; Noise penalty: too many sentences or bullet points
     (define noise-penalty
       (let ([sentence-count (length (regexp-match* #rx"[.!?]+" content))]
             [bullet-count (length (regexp-match* #rx"[-*•]" content))])
         (cond
           [(> sentence-count 4) -0.1]
           [(> bullet-count 3) -0.1]
           [else 0.0])))
     ;; Rambling penalty: very long without breaks
     (define rambling-penalty (if (and (> len 200) (not (regexp-match? #rx"\n" content))) -0.1 0.0))
     ;; Clamp to [0.1, 1.0]
     (min 1.0 (max 0.1 (+ base factual-boost keyword-boost noise-penalty rambling-penalty)))]))

;; Minimum confidence threshold for auto-extracted items (P2-8)
(define current-auto-extraction-min-confidence (make-parameter 0.5))

;; F32/F41: Sensitivity classification for auto-extracted content (P3-10).
;; NOTE: This is a placeholder heuristic for v0.95.10 alpha.
;; Substring matching on "internal"/"private"/"confidential" produces false
;; positives (e.g., "internal medicine"). Should be improved before stable.
(define (classify-sensitivity content)
  (define lower (string-downcase content))
  (if (or (string-contains? lower "internal")
          (string-contains? lower "private")
          (string-contains? lower "confidential"))
      'internal
      'public))

;; F21: Secret patterns for first-pass filtering.
;; This is a subset of policy.rkt's default-blocked-content-patterns.
;; Defense-in-depth: auto-extraction does a first-pass filter here, then
;; policy-allows-store? does a second-pass with the full pattern set.
(define secret-patterns
  (list #px"(?i:api.?key.*[=:].{10,})"
        #px"(?i:bearer\\s+\\S{15,})"
        #px"AKIA[A-Z0-9]{10,}"
        #px"-----BEGIN.*PRIVATE KEY"
        #px"(?i:password.*[=:].{6,})"
        #px"(?i:token.*[=:].{15,})"
        #px"(?:DATABASE_URL|MONGO_URI|REDIS_URL|SECRET_KEY).*[=:].{5,}"))

;; F22: Use shared redact-memory-content from policy for broader pattern coverage
(define (redact-snippet content)
  (define safe
    (if (> (string-length content) 80)
        (substring content 0 80)
        content))
  (redact-memory-content safe))

;; Content too long to be a fact (likely raw output)
(define max-fact-length 500)

;; ---------------------------------------------------------------------------
;; Extraction result
;; ---------------------------------------------------------------------------

(struct extraction-result (action item reason) #:transparent)
;; action: 'stored | 'blocked | 'skipped
;; item: memory-item or #f
;; reason: string

;; ---------------------------------------------------------------------------
;; Content safety checks
;; ---------------------------------------------------------------------------

(define (contains-secret? content)
  (for/or ([pattern (in-list secret-patterns)])
    (regexp-match? pattern content)))

(define (looks-like-raw-output? content)
  (or (> (string-length content) max-fact-length)
      ;; Heuristic: very long lines suggest file contents
      (for/or ([line (in-list (string-split content "\n"))])
        (> (string-length line) 200))))

(define (looks-like-file-dump? content)
  ;; Multiple lines with typical code/file markers
  (define lines (string-split content "\n"))
  (and (> (length lines) 5)
       (for/or ([line (in-list lines)])
         (or (string-contains? line "import ")
             (string-contains? line "require ")
             (string-contains? line "func ")
             (string-contains? line "def ")
             (string-contains? line "class ")))))

;; ---------------------------------------------------------------------------
;; Extract candidates from response text
;; ---------------------------------------------------------------------------

;; Simple extraction: splits on double-newline, filters by length and safety.
;; Returns list of candidate strings.
(define (extract-candidates text)
  (define paragraphs (string-split text "\n\n"))
  (for/list ([p (in-list paragraphs)]
             #:when (and (> (string-length (string-trim p)) 20)
                         (< (string-length p) max-fact-length)
                         (not (contains-secret? p))
                         (not (looks-like-raw-output? p))
                         (not (looks-like-file-dump? p))))
    (string-trim p)))

;; ---------------------------------------------------------------------------
;; Main extraction function
;; ---------------------------------------------------------------------------

;; Try to extract and store memories from response text.
;; Returns list of extraction-results. Does NOT raise on failure.
(define (try-auto-extract response-text
                          #:backend backend
                          #:policy policy
                          #:session-id session-id
                          #:project-root project-root
                          #:on-event [on-event void]
                          #:on-typed-event [on-typed-event void])
  (cond
    [(not (current-auto-extraction-enabled))
     (list (extraction-result 'skipped #f "Auto-extraction disabled"))]
    [(not backend) (list (extraction-result 'skipped #f "No memory backend configured"))]
    [(not (string? response-text)) (list (extraction-result 'skipped #f "Response is not a string"))]
    [(= (string-length response-text) 0) (list (extraction-result 'skipped #f "Empty response"))]
    [else
     (define candidates (extract-candidates response-text))
     (for/list ([content (in-list candidates)])
       (with-handlers
           ([exn:fail?
             (lambda (e)
               (extraction-result 'skipped #f (format "Extraction error: ~a" (exn-message e))))])
         (define id
           (format "auto_~a_~a"
                   (current-seconds)
                   (modulo (inexact->exact (round (* 1000 (current-inexact-milliseconds)))) 1000000)))
         (define now (current-iso-8601)) ; F32: use shared helper
         (define sensitivity (classify-sensitivity content))
         (define confidence (estimate-confidence content)) ; F10: variable confidence
         (define scope (effective-memory-scope #f project-root)) ; F14: respect default scope
         (define item
           (memory-item
            id
            'semantic
            scope ; F14
            content
            (hasheq 'source
                    'auto-extraction
                    'session-id
                    session-id
                    'project-root
                    project-root
                    'tags
                    '()
                    'origin-tool-call-id
                    "auto-extraction")
            (hasheq 'sensitivity sensitivity 'confidence confidence 'expires-at #f 'supersedes '())
            now
            now))
         ;; F13: Emit store.requested for every candidate (SPEC §6)
         (on-typed-event (make-mem-store-requested-event #:candidate-id id
                                                         #:mem-type 'semantic
                                                         #:scope scope
                                                         #:source 'auto-extraction
                                                         #:session-id session-id
                                                         #:turn-id "auto"))
         (cond
           [(< confidence (current-auto-extraction-min-confidence))
            (extraction-result 'skipped #f "Below minimum confidence threshold")]
           [(contains-secret? content)
            (on-event 'blocked item "Contains secret pattern")
            (on-typed-event (make-mem-policy-blocked-event #:action 'store
                                                           #:reason "Contains secret pattern"
                                                           #:source 'auto-extraction
                                                           #:session-id session-id
                                                           #:turn-id "auto"
                                                           #:redacted-snippet
                                                           (redact-snippet content)))
            (extraction-result 'blocked #f "Contains secret pattern")]
           [(not (policy-allows-store? policy item))
            (define reason ; F12: specific reason
              (cond
                [(not (memory-persistent-write-allowed?)) "Safe mode prevents persistent storage"]
                [else "Store blocked by memory policy"]))
            (on-event 'blocked item reason)
            (on-typed-event (make-mem-policy-blocked-event #:action 'store
                                                           #:reason reason
                                                           #:source 'auto-extraction
                                                           #:session-id session-id
                                                           #:turn-id "auto"
                                                           #:redacted-snippet
                                                           (redact-snippet content)))
            (extraction-result 'blocked item reason)]
           [else
            (define result (gen:store-memory! backend item))
            (cond
              [(memory-result-ok? result)
               (on-event 'stored item #f)
               (on-typed-event (make-mem-item-stored-event
                                #:memory-id (memory-item-id item)
                                #:mem-type (memory-item-type item)
                                #:scope (memory-item-scope item)
                                #:source 'auto-extraction
                                #:session-id session-id
                                #:turn-id "auto"
                                #:redacted-snippet (redact-snippet content))) ; F11: non-empty snippet
               (extraction-result 'stored item #f)]
              [else
               (extraction-result
                'blocked
                #f
                (format "Store failed: ~a"
                        (hash-ref (memory-result-error result) 'message "unknown")))])])))]))

;; ---------------------------------------------------------------------------
;; F32: format-iso-now/pad2 removed — using current-iso-8601 from helpers.rkt

;; ---------------------------------------------------------------------------
;; Provide
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; Lifecycle helper for post-turn hook
;; ---------------------------------------------------------------------------

;; Non-fatal post-turn auto-extraction. Called from the agent loop after
;; a completed assistant text turn. Catches all exceptions silently.
;; Returns void — extraction results are observable via events only.
(define (maybe-auto-extract-after-response! response-text
                                            #:session-id session-id
                                            #:project-root [project-root #f]
                                            #:on-event [on-event void]
                                            #:on-typed-event [on-typed-event void])
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "auto-extraction error: ~a" (exn-message e))))])
    (define backend (current-memory-backend))
    (define policy (current-memory-policy))
    (when (and (current-auto-extraction-enabled) backend)
      (try-auto-extract response-text
                        #:backend backend
                        #:policy policy
                        #:session-id session-id
                        #:project-root (or project-root ".")
                        #:on-event on-event
                        #:on-typed-event on-typed-event))))

(provide current-auto-extraction-enabled
         current-auto-extraction-min-confidence
         classify-sensitivity
         estimate-confidence ; F10
         try-auto-extract
         extraction-result
         extraction-result?
         extraction-result-action
         extraction-result-item
         extraction-result-reason
         maybe-auto-extract-after-response!
         ;; Low-level checks for testing
         contains-secret?
         looks-like-raw-output?
         looks-like-file-dump?
         extract-candidates)
