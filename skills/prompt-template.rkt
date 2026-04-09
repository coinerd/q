#lang racket

;; skills/prompt-template.rkt — re-export template rendering from types.rkt
;;
;; Prompt template rendering is defined in skills/types.rkt.
;; This module re-exports the binding for backward compatibility.

(require (only-in "types.rkt"
                  render-template))

(provide render-template)
