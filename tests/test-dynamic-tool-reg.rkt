#lang racket

;; tests/test-dynamic-tool-reg.rkt — tests for Dynamic Tool Registration (#676)
;;
;; Covers:
;;   #672: Tool schema validator and register-tool! API
;;   #673: Merge extension tools into LLM request tool list
;;   #674: promptSnippet field in tool definitions
;;   #675: Tool result interception hook

(require rackunit
         "../tools/tool.rkt"
         "../extensions/hooks.rkt"
         "../extensions/api.rkt"
         "../util/hook-types.rkt")

;; ============================================================
;; #672: Tool schema validator
;; ============================================================

(test-case "validate-tool-schema accepts valid schema"
  (check-true
   (validate-tool-schema
    (hasheq 'type "object"
            'properties (hasheq 'path (hasheq 'type "string"))
            'required '("path")))))

(test-case "validate-tool-schema accepts minimal valid schema"
  (check-true
   (validate-tool-schema (hasheq 'type "object"))))

(test-case "validate-tool-schema rejects non-hash"
  (check-exn exn:fail?
    (lambda () (validate-tool-schema "not a hash"))))

(test-case "validate-tool-schema rejects missing type"
  (check-exn exn:fail?
    (lambda () (validate-tool-schema (hasheq 'properties (hasheq))))))

(test-case "validate-tool-schema rejects wrong type"
  (check-exn exn:fail?
    (lambda () (validate-tool-schema (hasheq 'type "array")))))

(test-case "validate-tool-schema rejects non-hash properties"
  (check-exn exn:fail?
    (lambda () (validate-tool-schema (hasheq 'type "object" 'properties "bad")))))

(test-case "validate-tool-schema rejects non-list required"
  (check-exn exn:fail?
    (lambda () (validate-tool-schema (hasheq 'type "object" 'required "bad")))))

;; ============================================================
;; #672: register-tool! with validation
;; ============================================================

(test-case "register-tool! accepts valid tool"
  (define reg (make-tool-registry))
  (define t (make-tool "my-tool"
                       "A test tool"
                       (hasheq 'type "object"
                               'properties (hasheq 'x (hasheq 'type "integer"))
                               'required '("x"))
                       (lambda (args) args)))
  (register-tool! reg t)
  (check-equal? (tool-names reg) '("my-tool")))

(test-case "register-tool! rejects duplicate name"
  (define reg (make-tool-registry))
  (register-tool! reg (make-tool "dup" "desc" (hasheq 'type "object") void))
  (check-exn exn:fail?
    (lambda ()
      (register-tool! reg (make-tool "dup" "other" (hasheq 'type "object") void)))))

(test-case "lookup-tool finds registered tool"
  (define reg (make-tool-registry))
  (register-tool! reg (make-tool "find-me" "desc" (hasheq 'type "object") void))
  (check-false (not (lookup-tool reg "find-me"))))

(test-case "lookup-tool returns #f for missing tool"
  (define reg (make-tool-registry))
  (check-false (lookup-tool reg "nonexistent")))

(test-case "unregister-tool! removes tool"
  (define reg (make-tool-registry))
  (register-tool! reg (make-tool "temp" "desc" (hasheq 'type "object") void))
  (unregister-tool! reg "temp")
  (check-false (lookup-tool reg "temp")))

;; ============================================================
;; #674: promptSnippet field
;; ============================================================

(test-case "make-tool without prompt-snippet defaults to #f"
  (define t (make-tool "no-snippet" "desc" (hasheq 'type "object") void))
  (check-false (tool-prompt-snippet t)))

(test-case "make-tool with prompt-snippet stores it"
  (define t (make-tool "with-snippet"
                       "desc"
                       (hasheq 'type "object")
                       void
                       #:prompt-snippet "Use this tool to search files"))
  (check-equal? (tool-prompt-snippet t) "Use this tool to search files"))

(test-case "tool->jsexpr omits promptSnippet when #f"
  (define t (make-tool "no-ps" "desc" (hasheq 'type "object") void))
  (define j (tool->jsexpr t))
  (check-false (hash-has-key? (hash-ref j 'function) 'promptSnippet)))

(test-case "tool->jsexpr includes promptSnippet when present"
  (define t (make-tool "with-ps"
                       "desc"
                       (hasheq 'type "object")
                       void
                       #:prompt-snippet "Search for patterns"))
  (define j (tool->jsexpr t))
  (check-equal? (hash-ref (hash-ref j 'function) 'promptSnippet)
                "Search for patterns"))

;; ============================================================
;; #673: merge-tool-lists
;; ============================================================

(test-case "merge-tool-lists with empty extension list returns base"
  (define base (list (hasheq 'type "function"
                             'function (hasheq 'name "read" 'description "Read" 'parameters (hasheq)))))
  (check-equal? (merge-tool-lists base '()) base))

(test-case "merge-tool-lists appends new extension tools"
  (define base (list (hasheq 'type "function"
                             'function (hasheq 'name "read" 'description "Read" 'parameters (hasheq)))))
  (define ext (list (hasheq 'type "function"
                            'function (hasheq 'name "ext-tool" 'description "Ext" 'parameters (hasheq)))))
  (define result (merge-tool-lists base ext))
  (check-equal? (length result) 2)
  (check-equal? (hash-ref (hash-ref (car result) 'function) 'name) "read")
  (check-equal? (hash-ref (hash-ref (cadr result) 'function) 'name) "ext-tool"))

(test-case "merge-tool-lists overrides base tool with same name"
  (define base (list (hasheq 'type "function"
                             'function (hasheq 'name "read" 'description "Built-in Read" 'parameters (hasheq)))))
  (define ext (list (hasheq 'type "function"
                            'function (hasheq 'name "read" 'description "Enhanced Read" 'parameters (hasheq)))))
  (define result (merge-tool-lists base ext))
  (check-equal? (length result) 1)
  (check-equal? (hash-ref (hash-ref (car result) 'function) 'description) "Enhanced Read"))

(test-case "merge-tool-lists preserves base order"
  (define base (list (hasheq 'type "function"
                             'function (hasheq 'name "bash" 'description "Bash" 'parameters (hasheq)))
                     (hasheq 'type "function"
                             'function (hasheq 'name "read" 'description "Read" 'parameters (hasheq)))))
  (define ext (list (hasheq 'type "function"
                            'function (hasheq 'name "my-ext" 'description "Ext" 'parameters (hasheq)))))
  (define result (merge-tool-lists base ext))
  (define names (map (lambda (t) (hash-ref (hash-ref t 'function) 'name)) result))
  (check-equal? names '("bash" "read" "my-ext")))

;; ============================================================
;; #675: Tool result interception (via hook dispatch)
;; ============================================================

(test-case "tool-result hook can amend results"
  (define reg (make-extension-registry))
  (register-extension! reg
    (extension "interceptor" "0.1" "1.0"
               (hasheq 'tool-result
                       (lambda (payload)
                         (hook-amend
                          (hasheq 'intercepted #t
                                  'original payload))))))
  (define result (dispatch-hooks 'tool-result "original-data" reg))
  (check-equal? (hook-result-action result) 'amend)
  (check-equal? (hash-ref (hook-result-payload result) 'intercepted) #t))

(test-case "tool-result hook can block results"
  (define reg (make-extension-registry))
  (register-extension! reg
    (extension "blocker" "0.1" "1.0"
               (hasheq 'tool-result
                       (lambda (payload) (hook-block "blocked")))))
  (define result (dispatch-hooks 'tool-result "data" reg))
  (check-equal? (hook-result-action result) 'block))

(test-case "tool-result hook passes through when no handlers"
  (define reg (make-extension-registry))
  (define result (dispatch-hooks 'tool-result "data" reg))
  (check-equal? (hook-result-action result) 'pass))

(test-case "tool-result is advisory — errors default to pass"
  (check-false (critical-hook? 'tool-result)))

;; ============================================================
;; Combined: Extension registers tools via register-tools hook
;; ============================================================

(test-case "register-tools hook can provide tool definitions"
  (define reg (make-extension-registry))
  (register-extension! reg
    (extension "tool-provider" "0.1" "1.0"
               (hasheq 'register-tools
                       (lambda (payload)
                         (hook-amend
                          (hasheq 'tools
                                  (list (hasheq 'type "function"
                                                'function (hasheq 'name "ext-search"
                                                                  'description "Search"
                                                                  'parameters
                                                                  (hasheq 'type "object"
                                                                          'properties
                                                                          (hasheq 'q (hasheq 'type "string"))
                                                                          'required '("q")))))))))))
  (define result (dispatch-hooks 'register-tools (hasheq) reg))
  (check-equal? (hook-result-action result) 'amend)
  (define ext-tools (hash-ref (hook-result-payload result) 'tools))
  (check-equal? (length ext-tools) 1)
  (check-equal?
   (hash-ref (hash-ref (car ext-tools) 'function) 'name)
   "ext-search"))

(test-case "register-tools is advisory — errors default to pass"
  (check-false (critical-hook? 'register-tools)))
