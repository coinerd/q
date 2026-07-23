#lang racket

;; test-image-content-part.rkt — image-part content type tests
;;
;; Tests for the new image-part struct added in v0.98.1 W0.

;; @speed fast
(require rackunit
         "../util/content/content-parts.rkt")

;; ---------------------------------------------------------------------------
;; Construction & predicates
;; ---------------------------------------------------------------------------

(test-case "make-image-part creates image-part"
  (define ip (make-image-part "image/png" "base64data==" "high"))
  (check-pred image-part? ip)
  (check-pred content-part? ip)
  (check-equal? (image-part-mime-type ip) "image/png")
  (check-equal? (image-part-data ip) "base64data==")
  (check-equal? (image-part-detail ip) "high"))

(test-case "make-image-part without detail"
  (define ip (make-image-part "image/jpeg" "data"))
  (check-false (image-part-detail ip)))

(test-case "image-part is NOT text-part or tool-result-part"
  (define ip (make-image-part "image/png" "data"))
  (check-false (text-part? ip))
  (check-false (tool-result-part? ip))
  (check-false (tool-call-part? ip)))

;; ---------------------------------------------------------------------------
;; Serialization round-trip
;; ---------------------------------------------------------------------------

(test-case "image-part round-trips through JSON"
  (define ip (make-image-part "image/png" "abc123==" "auto"))
  (define js (content-part->jsexpr ip))
  (check-equal? (hash-ref js 'type) "image")
  (check-equal? (hash-ref js 'mimeType) "image/png")
  (check-equal? (hash-ref js 'data) "abc123==")
  (check-equal? (hash-ref js 'detail) "auto")
  (define rt (jsexpr->content-part js))
  (check-pred image-part? rt)
  (check-equal? (image-part-mime-type rt) "image/png")
  (check-equal? (image-part-data rt) "abc123==")
  (check-equal? (image-part-detail rt) "auto"))

(test-case "image-part without detail round-trips"
  (define ip (make-image-part "image/png" "data"))
  (define js (content-part->jsexpr ip))
  (check-false (hash-ref js 'detail #f))
  (define rt (jsexpr->content-part js))
  (check-false (image-part-detail rt)))

;; ---------------------------------------------------------------------------
;; Existing types still work
;; ---------------------------------------------------------------------------

(test-case "text-part round-trip still works"
  (define tp (make-text-part "hello"))
  (define js (content-part->jsexpr tp))
  (check-equal? (hash-ref js 'type) "text")
  (define rt (jsexpr->content-part js))
  (check-pred text-part? rt)
  (check-equal? (text-part-text rt) "hello"))

(test-case "tool-result-part round-trip still works"
  (define tr (make-tool-result-part "call-1" "result text" #f))
  (define js (content-part->jsexpr tr))
  (check-equal? (hash-ref js 'type) "tool-result")
  (define rt (jsexpr->content-part js))
  (check-pred tool-result-part? rt)
  (check-equal? (tool-result-part-content rt) "result text"))
