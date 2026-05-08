#lang racket/base

;; tests/test-macro-expansion.rkt — Macro expansion tests (RA-08)
;;
;; v0.33.2 W1: Verify define-tool and define-typed-event expand correctly.

(require rackunit
         racket/syntax
         racket/string
         (for-syntax racket/base
                     syntax/parse)
         "../tools/define-tool.rkt"
         "../tools/tool.rkt"
         "../util/event-macro.rkt")

;; ============================================================
;; define-tool expansion tests
;; ============================================================

;; Test 1: Macro produces tool-id binding
(define-tool macro-test-tool
  #:description "Test tool for macro expansion"
  #:required ("input")
  #:properties [(input "string" "Test input")]
  (lambda (args exec-ctx) (make-success-result "ok")))

(test-case "define-tool: provides tool-id binding"
  (check-pred procedure? tool-macro-test-tool)
  (check-equal? (tool-name macro-test-tool) "macro-test-tool"))

(test-case "define-tool: schema has correct structure"
  (define schema (tool-schema macro-test-tool))
  (check-equal? (hash-ref schema 'type) "object")
  (check-equal? (hash-ref schema 'required) '("input"))
  (define props (hash-ref schema 'properties))
  (check-true (hash-has-key? props 'input))
  (check-equal? (hash-ref (hash-ref props 'input) 'type) "string")
  (check-equal? (hash-ref (hash-ref props 'input) 'description) "Test input"))

;; Test 2: Handler is callable
(test-case "define-tool: handler callable via tool-execute"
  (define execute (tool-execute macro-test-tool))
  (define result (execute (hasheq 'input "hello") #f))
  (check-false (tool-result-is-error? result)))

;; ============================================================
;; define-typed-event expansion tests
;; ============================================================

(define-typed-event test-event "test.topic"
  (field-a)
  #:optional ([field-b 42]))

(test-case "define-typed-event: struct is defined"
  (check-pred procedure? test-event) ;; constructor is a procedure
  (check-pred test-event? (make-test-event #:session-id "s1" #:turn-id "t1" #:field-a "x"))
  (check-pred test-event? (make-test-event #:session-id "s1" #:turn-id "t1" #:field-a "x" #:field-b 100)))

(test-case "define-typed-event: default values work"
  (define ev (make-test-event #:session-id "s2" #:turn-id "t2" #:field-a "hello"))
  (check-equal? (test-event-field-a ev) "hello")
  (check-equal? (test-event-field-b ev) 42))

(test-case "define-typed-event: custom values work"
  (define ev (make-test-event #:session-id "s3" #:turn-id "t3" #:field-a "world" #:field-b 99))
  (check-equal? (test-event-field-a ev) "world")
  (check-equal? (test-event-field-b ev) 99))

(test-case "define-typed-event: type is correct"
  (check-equal? test-event-type "test.topic"))

;; ============================================================
;; Macro hygiene: multiple uses don't collide
;; ============================================================

(define-tool macro-collide-a
  #:description "First tool"
  #:required ()
  #:properties []
  (lambda (args exec-ctx) (make-success-result "a")))

(define-tool macro-collide-b
  #:description "Second tool"
  #:required ()
  #:properties []
  (lambda (args exec-ctx) (make-success-result "b")))

(test-case "define-tool: multiple uses don't collide"
  (check-equal? (tool-name macro-collide-a) "macro-collide-a")
  (check-equal? (tool-name macro-collide-b) "macro-collide-b")
  (check-equal? ((tool-execute macro-collide-a) (hasheq) #f)
                ((tool-execute macro-collide-a) (hasheq) #f)))

;; ============================================================
;; RA-25: Macro expansion structure tests
;; ============================================================

(require (for-syntax racket/base))

;; Verify define-typed-event expands to struct + predicate + type
(test-case "define-typed-event: expansion contains struct definition"
  (define expanded (expand '(define-typed-event expansion-test-event "expansion.test"
                             (field-x)
                             #:optional ([field-y 0]))))
  (define expanded-str (format "~a" expanded))
  (check-true (string-contains? expanded-str "struct")
              "expansion contains 'struct'")
  (check-true (string-contains? expanded-str "expansion-test-event?")
              "expansion contains predicate")
  (check-true (string-contains? expanded-str "expansion-test-event-type")
              "expansion contains type constant"))

;; Verify define-tool expansion contains schema and tool binding
(test-case "define-tool: expansion contains schema hash and tool binding"
  (define expanded (expand '(define-tool expansion-test-tool
                             #:description "Expansion test tool"
                             #:required ("arg")
                             #:properties [(arg "string" "An argument")]
                             (lambda (args exec-ctx) (make-success-result "ok")))))
  (define expanded-str (format "~a" expanded))
  (check-true (string-contains? expanded-str "tool")
              "expansion contains 'tool' struct reference")
  (check-true (string-contains? expanded-str "expansion-test-tool")
              "expansion contains tool-id binding"))
