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
        (let ([s (format "~a" content)])
          (if (> (string-length s) MAX-CONTENT-STRING-LEN)
              (format "[truncated hash content: ~a chars]" (string-length s))
              s))])]
    [(list? content)
     (string-join (for/list ([part (in-list content)])
                    (cond
                      [(string? part) part]
                      [(hash? part) (hash-ref part 'text (format "~a" part))]
                      [else (format "~a" part)]))
                  "\n")]
    [else (format "~a" content)]))

;; Convert tool-result content to a display string.
;; Same as result-content->string but also handles top-level hashes
;; (e.g. a single content hash with a 'text key).
(define (tool-result-content->string content)
  (result-content->string content #:handle-hash? #t))
