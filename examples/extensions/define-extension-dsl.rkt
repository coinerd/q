#lang racket/base

;; examples/extensions/define-extension-dsl.rkt — using the define-q-extension macro
;;
;; Demonstrates the DSL macro for defining extensions with cleaner syntax.
;; Equivalent to hello-world.rkt but using the macro instead of raw structs.
;;
;; Usage:
;;   (load-extension! registry "examples/extensions/define-extension-dsl.rkt")

(require "../../extensions/define-extension.rkt"
         "../../extensions/hooks.rkt")

;; The define-q-extension macro creates an extension struct with a cleaner DSL.
;; #:on hook-point handler registers a hook handler.
;; The macro binds the result to a variable matching the name symbol.
(define-q-extension define-extension-dsl
  #:version "1.0.0"
  #:api-version "1"
  #:on turn-start (lambda (payload)
                    (log-info "define-extension-dsl: Turn started via DSL")
                    (hook-pass payload))
  #:on turn-end (lambda (payload)
                  (log-info "define-extension-dsl: Turn ended via DSL")
                  (hook-pass payload)))

;; The loader expects 'the-extension, so alias it.
(define the-extension define-extension-dsl)

(provide the-extension)

;; Key concepts:
;;   1. define-q-extension is a macro that creates the extension struct for you
;;   2. The extension name comes from the symbol (define-extension-dsl -> "define-extension-dsl")
;;   3. Use #:version and #:api-version for metadata
;;   4. Use #:on hook-point handler to register each hook
;;   5. The result is bound to a variable matching the name symbol
;;   6. Alias to the-extension for the loader to find it
