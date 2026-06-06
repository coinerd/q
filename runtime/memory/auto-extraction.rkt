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
         "policy.rkt"
         (only-in "../../agent/event-structs/memory-events.rkt"
                  make-mem-item-stored-event
                  make-mem-policy-blocked-event))

;; ---------------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------------

;; Master switch — disabled by default
(define current-auto-extraction-enabled (make-parameter #f))

;; Minimum confidence threshold for auto-extracted items (P2-8)
(define current-auto-extraction-min-confidence (make-parameter 0.5))

;; Sensitivity classification for auto-extracted content (P3-10)
(define (classify-sensitivity content)
  (define lower (string-downcase content))
  (if (or (string-contains? lower "internal")
          (string-contains? lower "private")
          (string-contains? lower "confidential"))
      'internal
      'public))

;; Secret patterns that block storage
(define secret-patterns
  (list #px"(?i:api.?key.*[=:].{10,})"
        #px"(?i:bearer\\s+\\S{15,})"
        #px"AKIA[A-Z0-9]{10,}"
        #px"-----BEGIN.*PRIVATE KEY"
        #px"(?i:password.*[=:].{6,})"
        #px"(?i:token.*[=:].{15,})"
        #px"(?:DATABASE_URL|MONGO_URI|REDIS_URL|SECRET_KEY).*[=:].{5,}"))

(define (redact-snippet content)
  (define safe
    (if (> (string-length content) 80)
        (substring content 0 80)
        content))
  (regexp-replace* #px"(?i:api[_-]?key|password|token|secret).{0,20}" safe "[REDACTED]"))

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
         (define now (format-iso-now))
         (define sensitivity (classify-sensitivity content))
         (define confidence 0.5)
         (define item
           (memory-item
            id
            'semantic
            'project
            content
            (hasheq 'source
                    'auto-extraction
                    'session-id
                    session-id
                    'project-root
                    project-root
                    'tags
                    '())
            (hasheq 'sensitivity sensitivity 'confidence confidence 'expires-at #f 'supersedes '())
            now
            now))
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
            (on-event 'blocked item "Blocked by policy")
            (on-typed-event (make-mem-policy-blocked-event #:action 'store
                                                           #:reason "Blocked by policy"
                                                           #:source 'auto-extraction
                                                           #:session-id session-id
                                                           #:turn-id "auto"
                                                           #:redacted-snippet ""))
            (extraction-result 'blocked item "Blocked by policy")]
           [else
            (define result (gen:store-memory! backend item))
            (cond
              [(memory-result-ok? result)
               (on-event 'stored item #f)
               (on-typed-event (make-mem-item-stored-event #:memory-id (memory-item-id item)
                                                           #:mem-type (memory-item-type item)
                                                           #:scope (memory-item-scope item)
                                                           #:source 'auto-extraction
                                                           #:session-id session-id
                                                           #:turn-id "auto"
                                                           #:redacted-snippet ""))
               (extraction-result 'stored item #f)]
              [else
               (extraction-result
                'blocked
                #f
                (format "Store failed: ~a"
                        (hash-ref (memory-result-error result) 'message "unknown")))])])))]))

;; ---------------------------------------------------------------------------
;; ISO timestamp helper
;; ---------------------------------------------------------------------------

(define (format-iso-now)
  (define ts (current-seconds))
  (define d (seconds->date ts #f))
  (format "~a-~a-~aT~a:~a:~aZ"
          (date-year d)
          (pad2 (date-month d))
          (pad2 (date-day d))
          (pad2 (date-hour d))
          (pad2 (date-minute d))
          (pad2 (date-second d))))

(define (pad2 n)
  (if (< n 10)
      (format "0~a" n)
      (format "~a" n)))

;; ---------------------------------------------------------------------------
;; Provide
;; ---------------------------------------------------------------------------

(provide current-auto-extraction-enabled
         current-auto-extraction-min-confidence
         classify-sensitivity
         try-auto-extract
         extraction-result
         extraction-result?
         extraction-result-action
         extraction-result-item
         extraction-result-reason
         ;; Low-level checks for testing
         contains-secret?
         looks-like-raw-output?
         looks-like-file-dump?
         extract-candidates)
