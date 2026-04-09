#lang racket

(require rackunit
         "../skills/types.rkt")

;; skills/prompt-template.rkt re-exports render-template from skills/types.rkt.
;; We test it through the re-export module.

(require "../skills/prompt-template.rkt")

;; ============================================================
;; render-template (via prompt-template.rkt re-export)
;; ============================================================

(test-case "prompt-template render-template replaces vars"
  (define result (render-template "{{name}} says hi" (hash 'name "Alice")))
  (check-equal? result "Alice says hi"))

(test-case "prompt-template render-template handles empty vars"
  (define result (render-template "no vars" (hash)))
  (check-equal? result "no vars"))

(test-case "prompt-template render-template handles whitespace in var"
  (define result (render-template "{{ name }}" (hash 'name "Bob")))
  (check-equal? result "Bob"))
