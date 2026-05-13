#lang racket/base

;; BOUNDARY: integration

;; tests/test-define-tool.rkt — Tests for define-tool macro

(require rackunit
         racket/string
         "../tools/define-tool.rkt"
         "../tools/tool.rkt"
         "../tools/tool-struct.rkt")

;; ============================================================
;; Test 1: Basic tool with properties
;; ============================================================

(define-tool
 test-read
 #:description "Read file contents"
 #:required ("path")
 #:properties
 [(path "string" "Path to file") (offset "integer" "Line offset") (limit "integer" "Max lines")]
 (lambda (args exec-ctx) (make-success-result (format "read: ~a" (hash-ref args 'path "none")))))

(test-case "define-tool: creates a tool struct"
  (check-pred tool? test-read)
  (check-equal? (tool-name test-read) "test-read")
  (check-equal? (tool-schema test-read)
                (hasheq 'type
                        "object"
                        'required
                        '("path")
                        'properties
                        (hasheq 'path
                                (hasheq 'type "string" 'description "Path to file")
                                'offset
                                (hasheq 'type "integer" 'description "Line offset")
                                'limit
                                (hasheq 'type "integer" 'description "Max lines")))))

(test-case "define-tool: handler works"
  (define result ((tool-execute test-read) (hasheq 'path "/tmp/test.txt") #f))
  (check-false (tool-result-is-error? result))
  (check-true (string-contains? (format "~a" (tool-result-content result)) "read: /tmp/test.txt")))

;; ============================================================
;; Test 2: Tool with no required params
;; ============================================================

(define-tool test-list
             #:description "List directory"
             #:required ()
             #:properties [(path "string" "Directory path")]
             (lambda (args exec-ctx) (make-success-result "listed")))

(test-case "define-tool: no required params"
  (check-pred tool? test-list)
  (check-equal? (tool-name test-list) "test-list")
  (define schema (tool-schema test-list))
  (check-equal? (hash-ref schema 'required) '()))

;; ============================================================
;; Test 3: Tool with no properties
;; ============================================================

(define-tool test-ping
             #:description "Health check"
             #:required ()
             #:properties []
             (lambda (args exec-ctx) (make-success-result "pong")))

(test-case "define-tool: no properties"
  (check-pred tool? test-ping)
  (check-equal? (tool-name test-ping) "test-ping")
  (define schema (tool-schema test-ping))
  (check-equal? (hash-ref schema 'required) '())
  (check-equal? (hash-ref schema 'properties) (hasheq)))

;; ============================================================
;; Test 4: Tool handler returns error
;; ============================================================

(define-tool test-fail
             #:description "Always fails"
             #:required ()
             #:properties []
             (lambda (args exec-ctx) (make-error-result "intentional failure")))

(test-case "define-tool: handler returns error"
  (define result ((tool-execute test-fail) (hasheq) #f))
  (check-true (tool-result-is-error? result)))
