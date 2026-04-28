#lang racket

;; tests/test-schema-macro.rkt — Tests for tools/schema-macro.rkt
;;
;; Verifies the define-tool-schema macro and tool-schema function
;; produce correct JSON Schema hashes.

(require rackunit
         "../tools/schema-macro.rkt")

;; ============================================================
;; define-tool-schema macro
;; ============================================================

(test-case "define-tool-schema without description"
  (define-tool-schema test-schema-1
                      #:required ("path")
                      #:properties [("path" "string" "File path") ("offset" "integer" "Line offset")])
  (check-equal? (hash-ref test-schema-1 'type) "object")
  (check-equal? (hash-ref test-schema-1 'required) '("path"))
  (check-false (hash-ref test-schema-1 'description #f))
  (define props (hash-ref test-schema-1 'properties))
  (check-equal? (hash-ref (hash-ref props 'path) 'type) "string")
  (check-equal? (hash-ref (hash-ref props 'offset) 'type) "integer"))

(test-case "define-tool-schema with description"
  (define-tool-schema test-schema-2
                      #:description "A test tool"
                      #:required ("name" "value")
                      #:properties [("name" "string" "The name") ("value" "integer" "The value")])
  (check-equal? (hash-ref test-schema-2 'type) "object")
  (check-equal? (hash-ref test-schema-2 'description) "A test tool")
  (check-equal? (hash-ref test-schema-2 'required) '("name" "value"))
  (define props (hash-ref test-schema-2 'properties))
  (check-equal? (hash-ref (hash-ref props 'name) 'type) "string")
  (check-equal? (hash-ref (hash-ref props 'value) 'description) "The value"))

(test-case "define-tool-schema with empty required"
  (define-tool-schema test-schema-3 #:required () #:properties [("format" "string" "Output format")])
  (check-equal? (hash-ref test-schema-3 'required) '())
  (define props (hash-ref test-schema-3 'properties))
  (check-equal? (hash-ref (hash-ref props 'format) 'type) "string"))

;; ============================================================
;; tool-schema function (programmatic)
;; ============================================================

(test-case "tool-schema function builds correct hash"
  (define s (tool-schema '("x" "y") '(("x" "string" "X val") ("y" "integer" "Y val"))))
  (check-equal? (hash-ref s 'type) "object")
  (check-equal? (hash-ref s 'required) '("x" "y"))
  (define props (hash-ref s 'properties))
  (check-equal? (hash-ref (hash-ref props 'x) 'type) "string")
  (check-equal? (hash-ref (hash-ref props 'y) 'type) "integer"))

(test-case "tool-schema function with description"
  (define s (tool-schema '() '(("q" "string" "Query")) #:description "Search tool"))
  (check-equal? (hash-ref s 'description) "Search tool")
  (check-equal? (hash-ref s 'required) '()))

(test-case "tool-schema function without description"
  (define s (tool-schema '() '()))
  (check-false (hash-ref s 'description #f)))
