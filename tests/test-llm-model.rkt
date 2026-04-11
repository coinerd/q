#lang racket

(require rackunit
         "../llm/model.rkt")

;; ============================================================
;; model-request
;; ============================================================

(test-case "make-model-request creates struct"
  (define req (make-model-request '("msg1") #f (hasheq)))
  (check-pred model-request? req)
  (check-equal? (model-request-messages req) '("msg1"))
  (check-false (model-request-tools req))
  (check-equal? (model-request-settings req) (hasheq)))

(test-case "model-request->jsexpr includes tools when present"
  (define req (make-model-request '() '(tool1) (hasheq 'temp 0.7)))
  (define j (model-request->jsexpr req))
  (check-equal? (hash-ref j 'tools) '(tool1))
  (check-equal? (hash-ref j 'settings) (hasheq 'temp 0.7)))

(test-case "model-request->jsexpr omits tools when #f"
  (define req (make-model-request '() #f (hasheq)))
  (define j (model-request->jsexpr req))
  (check-false (hash-has-key? j 'tools)))

(test-case "jsexpr->model-request round-trip"
  (define req (make-model-request '("hi") '(t) (hasheq 'x 1)))
  (define restored (jsexpr->model-request (model-request->jsexpr req)))
  (check-equal? (model-request-messages restored) '("hi"))
  (check-equal? (model-request-tools restored) '(t)))

;; ============================================================
;; model-response
;; ============================================================

(test-case "make-model-response creates struct"
  (define resp (make-model-response '("content")
                                     (hasheq 'total-tokens 10)
                                     "gpt-4"
                                     'stop))
  (check-pred model-response? resp)
  (check-equal? (model-response-model resp) "gpt-4")
  (check-eq? (model-response-stop-reason resp) 'stop))

(test-case "model-response->jsexpr serializes stop-reason as string"
  (define resp (make-model-response '() (hasheq) "model" 'length))
  (define j (model-response->jsexpr resp))
  (check-equal? (hash-ref j 'stopReason) "length"))

(test-case "jsexpr->model-response round-trip"
  (define resp (make-model-response '("hi") (hasheq 't 5) "m" 'stop))
  (define restored (jsexpr->model-response (model-response->jsexpr resp)))
  (check-equal? (model-response-model restored) "m")
  (check-eq? (model-response-stop-reason restored) 'stop))

;; ============================================================
;; stream-chunk
;; ============================================================

(test-case "stream-chunk struct"
  (define ch (stream-chunk "hello" #f #f #f))
  (check-pred stream-chunk? ch)
  (check-equal? (stream-chunk-delta-text ch) "hello")
  (check-false (stream-chunk-done? ch)))

(test-case "stream-chunk done sentinel"
  (define ch (stream-chunk #f #f (hasheq) #t))
  (check-pred stream-chunk-done? ch)
  (check-false (stream-chunk-delta-text ch)))
