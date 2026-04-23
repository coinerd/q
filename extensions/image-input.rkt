#lang racket/base

;; extensions/image-input.rkt — Multi-modal Image Input Extension
;;
;; Phase E: image-input tool for base64 image encoding
;; and multi-modal message construction.

(require racket/string
         racket/file
         racket/path
         net/base64
         "define-extension.rkt"
         "dynamic-tools.rkt"
         "context.rkt"
         "hooks.rkt"
         "../tools/tool.rkt")

(provide the-extension
         image-input-extension
         handle-image-input
         image->base64
         extension->media-type
         make-image-message)

(define (image->base64 path)
  (string-trim (bytes->string/latin-1 (base64-encode (file->bytes path)))))

(define (extension->media-type path)
  (define ext (or (path-get-extension (string->path path)) #""))
  (cond
    [(equal? ext #".png") "image/png"]
    [(equal? ext #".jpg") "image/jpeg"]
    [(equal? ext #".jpeg") "image/jpeg"]
    [(equal? ext #".gif") "image/gif"]
    [(equal? ext #".webp") "image/webp"]
    [else "image/png"]))

(define (make-image-message text image-path)
  (define b64 (image->base64 image-path))
  (define mt (extension->media-type image-path))
  (hasheq
   'type
   "multi-modal"
   'content
   (list
    (hasheq 'type "text" 'text text)
    (hasheq 'type "image_url" 'image_url (hasheq 'url (string-append "data:" mt ";base64," b64))))))

(define (handle-image-input args [exec-ctx #f])
  (define action (hash-ref args 'action "encode"))
  (cond
    [(string=? action "encode")
     (define path (hash-ref args 'path ""))
     (when (string=? path "")
       (error 'image-input "path is required for encode"))
     (unless (file-exists? path)
       (error 'image-input (format "File not found: ~a" path)))
     (define b64 (image->base64 path))
     (make-success-result (list (hasheq 'type
                                        "text"
                                        'text
                                        (format "Encoded ~a (~a, ~a chars base64)"
                                                path
                                                (extension->media-type path)
                                                (string-length b64)))))]
    [(string=? action "message")
     (define text (hash-ref args 'text ""))
     (define path (hash-ref args 'path ""))
     (when (string=? path "")
       (error 'image-input "path is required for message"))
     (define msg (make-image-message text path))
     (make-success-result
      (list (hasheq 'type "text" 'text (format "Multi-modal message created with image ~a" path))
            msg))]
    [else (make-error-result (format "Unknown action: ~a" action))]))

(define (register-image-tools ctx)
  (ext-register-tool! ctx
                      "image-input"
                      (string-append "Multi-modal image input. "
                                     "Actions: encode (base64), message (multi-modal). "
                                     "PNG, JPEG, GIF, WebP.")
                      (hasheq 'type
                              "object"
                              'required
                              '("action" "path")
                              'properties
                              (hasheq 'action
                                      (hasheq 'type "string" 'description "encode|message")
                                      'path
                                      (hasheq 'type "string" 'description "Image file path")
                                      'text
                                      (hasheq 'type "string" 'description "Text for message")))
                      handle-image-input)
  (hook-pass ctx))

(define-q-extension image-input-extension
                    #:version "1.0.0"
                    #:api-version "1"
                    #:on register-tools
                    register-image-tools)

(define the-extension image-input-extension)
