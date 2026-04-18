#lang racket/base

;; examples/extensions/hello-world.rkt — minimal extension example (#1212)
;;
;; The simplest possible extension:
;;   - Registers a hook on 'turn-start that logs a greeting
;;   - Demonstrates: extension struct, hook registration, hook-pass
;;
;; Usage:
;;   (load-extension! registry "examples/extensions/hello-world.rkt")

(require "../../extensions/api.rkt"
         "../../extensions/hooks.rkt")

(provide the-extension)

;; Define the extension using the low-level extension struct.
;; The hooks field maps hook-point symbols to handler functions.
;; Each handler receives a payload and must return a hook-result.
(define the-extension
  (extension "hello-world"
             "1.0.0"
             "1"
             (hasheq 'turn-start
                     (lambda (payload)
                       (log-info "hello-world: Turn started!")
                       (hook-pass payload)))))

;; Key concepts:
;;   1. `extension` struct has 4 fields: name, version, api-version, hooks
;;   2. hooks is a hasheq mapping hook-point symbols -> handler functions
;;   3. Handler functions take a payload and return (hook-pass), (hook-amend new-payload), or (hook-block reason)
;;   4. The module must `provide` the-extension for the loader to find it
