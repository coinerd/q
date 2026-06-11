#lang racket

;; llm/vision-helpers.rkt — Shared helpers for vision/multimodal providers
;;
;; Extracted from duplicated code in anthropic.rkt and gemini.rkt (NF-10).

(provide parse-data-url)

;; Parse a data URL "data:<mime>;base64,<data>" and return (values mime data)
;; Falls back to "image/png" + raw url if parsing fails.
(define (parse-data-url url)
  (define m (regexp-match #rx"^data:([^;]+);base64,(.*)$" url))
  (if m
      (values (cadr m) (caddr m))
      (values "image/png" url)))
