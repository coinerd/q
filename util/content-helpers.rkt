#lang racket/base

;; util/content-helpers.rkt — shared content-part → string helpers
;;
;; Extracted from agent/loop.rkt and cli/render.rkt to eliminate
;; near-duplication (QUAL-03).  Unified via optional handle-hash?
;; parameter (QUAL-13).

(require racket/format
         racket/string)

;; Content extraction helpers
(provide result-content->string
         tool-result-content->string)

;; Convert result content to a string for the API.
;; Content may be a list of content-part hashes, plain strings, or nested data.
;; When #:handle-hash? is #t, top-level hashes are unwrapped (used by tool-result variant).
(define (result-content->string content #:handle-hash? [handle-hash? #f])
  (cond
    [(string? content) content]
    [(and handle-hash? (hash? content)) (hash-ref content 'text (format "~a" content))]
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
