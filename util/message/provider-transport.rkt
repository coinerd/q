#lang racket/base

;; Canonical provider-neutral tool transport.
;; STABILITY: internal
;;
;; Adapters consume the OpenAI-shaped request envelope produced here and
;; translate only at native boundaries. Legacy underscore spellings are
;; accepted at response ingress but are never emitted.

(require racket/contract
         racket/list
         racket/string
         json
         (only-in "../content/content-parts.rkt"
                  text-part?
                  text-part-text
                  image-part?
                  image-part-mime-type
                  image-part-data
                  image-part-detail
                  tool-call-part?
                  tool-call-part-id
                  tool-call-part-name
                  tool-call-part-arguments
                  tool-result-part?
                  tool-result-part-tool-call-id
                  tool-result-part-content)
         (only-in "message.rkt" message? message-role message-content)
         (only-in "../content/content-helpers.rkt" result-content->string))

(provide (contract-out [make-provider-tool-call (-> (or/c string? #f) (or/c string? #f) any/c hash?)]
                       [make-provider-assistant-message (-> string? (listof hash?) hash?)]
                       [make-provider-tool-result-message (-> string? string? hash?)]
                       [provider-tool-call-type? (-> any/c boolean?)]
                       [provider-tool-stop-reason? (-> any/c boolean?)]
                       [provider-completion-stop-reason? (-> any/c boolean?)]
                       [messages->provider-hashes (-> (listof message?) (listof hash?))]))

(define (make-provider-tool-call id name arguments)
  (hasheq 'id (or id "") 'type "function" 'function (hasheq 'name (or name "") 'arguments arguments)))

(define (make-provider-assistant-message text tool-calls)
  ;; v0.99.58 FIX: Always include content key. OpenAI-compatible APIs reject
  ;; assistant messages that have tool_calls but no content field (400:
  ;; "messages parameter is illegal"). Use JSON null for empty text.
  (hasheq 'role
          "assistant"
          'content
          (if (string=? text "")
              (json-null)
              text)
          'tool_calls
          tool-calls))

(define (make-provider-tool-result-message tool-call-id content)
  (hasheq 'role "tool" 'tool_call_id tool-call-id 'content content))

(define (provider-tool-call-type? value)
  (or (equal? value "tool-call") (equal? value "tool_call")))

(define (provider-tool-stop-reason? value)
  (and (memq value '(tool-calls tool_calls)) #t))

(define (provider-completion-stop-reason? value)
  (eq? value 'stop))

(define (parts->text parts)
  (cond
    [(string? parts) parts]
    [(list? parts)
     (string-join (for/list ([part (in-list parts)]
                             #:when (text-part? part))
                    (text-part-text part))
                  "")]
    [else (format "~a" parts)]))

(define (message->provider-hashes msg)
  (define role (message-role msg))
  (define parts (message-content msg))
  (cond
    [(eq? role 'user)
     (define images
       (if (list? parts)
           (filter image-part? parts)
           '()))
     (if (null? images)
         (list (hasheq 'role "user" 'content (parts->text parts)))
         (list (hasheq 'role
                       "user"
                       'content
                       (cons (hasheq 'type "text" 'text (parts->text parts))
                             (for/list ([image (in-list images)])
                               (hasheq 'type
                                       "image_url"
                                       'image_url
                                       (hasheq 'url
                                               (format "data:~a;base64,~a"
                                                       (image-part-mime-type image)
                                                       (image-part-data image))
                                               'detail
                                               (or (image-part-detail image) "auto"))))))))]
    [(eq? role 'assistant)
     (define text (parts->text parts))
     (define calls
       (if (list? parts)
           (for/list ([part (in-list parts)]
                      #:when (tool-call-part? part))
             (make-provider-tool-call (tool-call-part-id part)
                                      (tool-call-part-name part)
                                      (tool-call-part-arguments part)))
           '()))
     (list (if (null? calls)
               (hasheq 'role "assistant" 'content text)
               (make-provider-assistant-message text calls)))]
    [(eq? role 'tool)
     (if (list? parts)
         (for/list ([part (in-list parts)]
                    #:when (tool-result-part? part))
           (make-provider-tool-result-message (tool-result-part-tool-call-id part)
                                              (result-content->string (tool-result-part-content part)
                                                                      #:handle-hash? #t)))
         '())]
    [else
     (list (hasheq 'role
                   (if (symbol? role)
                       (symbol->string role)
                       (format "~a" role))
                   'content
                   (parts->text parts)))]))

(define (messages->provider-hashes messages)
  (append* (map message->provider-hashes messages)))
