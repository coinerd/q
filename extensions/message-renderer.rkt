#lang racket/base

;; extensions/message-renderer.rkt — Custom Message Type Rendering (#725-#727)
;;
;; Provides:
;;   #725: Anchor-based overlay positioning (constants + predicate in tui/state)
;;   #726: register-message-renderer for custom message types
;;   #727: Parent feature

(require racket/contract
         racket/match
         "../tui/render.rkt")

(provide
 ;; #726: Message renderer registry
 (struct-out message-renderer)
 make-message-renderer
 register-message-renderer!
 unregister-message-renderer!
 lookup-message-renderer
 list-message-renderers
 render-custom-message

 ;; #727: Registry struct
 (struct-out renderer-registry)
 make-renderer-registry)

;; ============================================================
;; Structs
;; ============================================================

;; A custom message renderer registered by an extension
(struct message-renderer
  (message-type  ; symbol — the message kind to match
   renderer-fn   ; (hash? -> (listof styled-line)) — renders payload to styled lines
   ext-name      ; string — registering extension name
   )
  #:transparent)

;; Thread-safe renderer registry
(struct renderer-registry
  (renderers-box   ; box of (hash/c symbol? message-renderer?)
   semaphore       ; semaphore for thread safety
   )
  #:transparent)

;; ============================================================
;; Registry operations
;; ============================================================

(define (make-renderer-registry)
  (renderer-registry (box (hash)) (make-semaphore 1)))

(define (register-message-renderer! registry renderer)
  (call-with-semaphore (renderer-registry-semaphore registry)
    (lambda ()
      (define renderers (unbox (renderer-registry-renderers-box registry)))
      (set-box! (renderer-registry-renderers-box registry)
                (hash-set renderers
                          (message-renderer-message-type renderer)
                          renderer)))))

(define (unregister-message-renderer! registry message-type)
  (call-with-semaphore (renderer-registry-semaphore registry)
    (lambda ()
      (define renderers (unbox (renderer-registry-renderers-box registry)))
      (set-box! (renderer-registry-renderers-box registry)
                (hash-remove renderers message-type)))))

(define (lookup-message-renderer registry message-type)
  (define renderers (unbox (renderer-registry-renderers-box registry)))
  (hash-ref renderers message-type #f))

(define (list-message-renderers registry)
  (hash-values (unbox (renderer-registry-renderers-box registry))))

;; ============================================================
;; Rendering
;; ============================================================

;; Render a message using a custom renderer if registered.
;; Returns (listof styled-line) or #f if no renderer found.
(define (render-custom-message registry message-type payload)
  (define renderer (lookup-message-renderer registry message-type))
  (if renderer
      ((message-renderer-renderer-fn renderer) payload)
      #f))

;; Convenience constructor
(define (make-message-renderer message-type renderer-fn ext-name)
  (message-renderer message-type renderer-fn ext-name))
