#lang racket/base

;; util/content-helpers.rkt — shared content-part → string helpers
;;
;; Extracted from agent/loop.rkt and cli/render.rkt to eliminate
;; near-duplication (QUAL-03).  Unified via optional handle-hash?
;; parameter (QUAL-13).

(require racket/contract
         racket/format
         racket/string)

(provide (contract-out [result-content->string (->* (any/c) (#:handle-hash? boolean?) string?)]
                       [tool-result-content->string (-> any/c string?)]))

;; Convert result content to a string for the API.
;; Content may be a list of content-part hashes, plain strings, or nested data.
;; When #:handle-hash? is #t, top-level hashes are unwrapped (used by tool-result variant).
(define MAX-CONTENT-STRING-LEN 4000)

(define (result-content->string content #:handle-hash? [handle-hash? #f])
  (cond
    [(string? content) content]
    [(and handle-hash? (hash? content))
     ;; Bug fix: guard against binary/base64 data (browser screenshots, etc.)
     ;; Without this, a 270K base64 string gets dumped verbatim into the API context.
     (cond
       [(hash-has-key? content 'data)
        (define mime (hash-ref content 'mime-type "unknown"))
        (define data-len
          (let ([d (hash-ref content 'data #f)])
            (if (string? d)
                (string-length d)
                0)))
        (format "[binary ~a data: ~a bytes]" mime data-len)]
       [(hash-has-key? content 'text) (hash-ref content 'text)]
       [else
        ;; B4 fix: Preserve metadata fields (session-id, status, url) before truncating.
        ;; Without this, browser_open results lose session-id → LLM hallucinates ID.
        (let ([s (format "~a" content)])
          (if (> (string-length s) MAX-CONTENT-STRING-LEN)
              (hash->metadata-summary content)
              s))])]
    [(list? content)
     (string-join (for/list ([part (in-list content)])
                    (cond
                      [(string? part) part]
                      [(and (hash? part) (equal? (hash-ref part 'type #f) "image"))
                       (format "[image: ~a, ~a chars]"
                               (hash-ref part 'mimeType "unknown")
                               (string-length (hash-ref part 'data "")))]
                      [(hash? part) (hash-ref part 'text (format "~a" part))]
                      [else (format "~a" part)]))
                  "\n")]
    [else (format "~a" content)]))

;; B4 fix: Build a structured summary from a hash, preserving metadata fields
;; (session-id, status, url, title) while truncating large content fields.
(define METADATA-KEYS '(session-id status url title))

(define (hash->metadata-summary h)
  (define meta-parts
    (for/list ([k (in-list METADATA-KEYS)]
               #:when (hash-has-key? h k))
      (format "~a: ~a" k (hash-ref h k))))
  (define content-parts
    (for/list ([(k v) (in-hash h)]
               #:unless (member k METADATA-KEYS))
      (format "~a: ~a chars" k (min (string-length (format "~a" v)) 999999))))
  (string-append (if (null? meta-parts)
                     ""
                     (format "[~a] " (string-join meta-parts ", ")))
                 (format "[truncated: ~a]" (string-join content-parts ", "))))

;; Convert tool-result content to a display string.
;; Same as result-content->string but also handles top-level hashes
;; (e.g. a single content hash with a 'text key).
(define (tool-result-content->string content)
  (result-content->string content #:handle-hash? #t))
