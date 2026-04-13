#lang racket/base

;; skills/prompt-template.rkt — re-export template rendering
;;
;; Prompt template rendering is defined in skills/template.rkt.
;; This module re-exports the binding for backward compatibility.

(require "template.rkt")

(provide ;; Re-export template rendering
 (all-from-out "template.rkt"))
