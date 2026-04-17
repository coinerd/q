#lang racket

(require rackunit
         "../tools/tool.rkt")

(test-case "make-tool with prompt-guidelines"
  (define t
    (make-tool "mytool"
               "A test tool"
               (hasheq)
               void
               #:prompt-guidelines "Always use this tool for file operations"))
  (check-equal? (tool-prompt-guidelines t) "Always use this tool for file operations"))

(test-case "make-tool without prompt-guidelines defaults to #f"
  (define t (make-tool "mytool" "A test tool" (hasheq) void))
  (check-false (tool-prompt-guidelines t)))

(test-case "set-active-tools! filters list-tools"
  (define reg (make-tool-registry))
  (register-tool! reg (make-tool "tool-a" "A" (hasheq) void))
  (register-tool! reg (make-tool "tool-b" "B" (hasheq) void))
  (register-tool! reg (make-tool "tool-c" "C" (hasheq) void))
  ;; All active by default
  (check-equal? (length (list-active-tools reg)) 3)
  ;; Activate only a and c
  (set-active-tools! reg '("tool-a" "tool-c"))
  (check-equal? (length (list-active-tools reg)) 2)
  (check-equal? (sort (map tool-name (list-active-tools reg)) string<?) '("tool-a" "tool-c")))

(test-case "set-active-tools! #f resets to all active"
  (define reg (make-tool-registry))
  (register-tool! reg (make-tool "tool-a" "A" (hasheq) void))
  (register-tool! reg (make-tool "tool-b" "B" (hasheq) void))
  (set-active-tools! reg '("tool-a"))
  (check-equal? (length (list-active-tools reg)) 1)
  (set-active-tools! reg #f)
  (check-equal? (length (list-active-tools reg)) 2))

(test-case "tool-active? checks active set"
  (define reg (make-tool-registry))
  (register-tool! reg (make-tool "tool-a" "A" (hasheq) void))
  (register-tool! reg (make-tool "tool-b" "B" (hasheq) void))
  (check-true (tool-active? reg "tool-a"))
  (set-active-tools! reg '("tool-b"))
  (check-false (tool-active? reg "tool-a"))
  (check-true (tool-active? reg "tool-b")))

(test-case "list-active-tools-jsexpr filters correctly"
  (define reg (make-tool-registry))
  (register-tool! reg (make-tool "tool-a" "A" (hasheq) void))
  (register-tool! reg (make-tool "tool-b" "B" (hasheq) void))
  (set-active-tools! reg '("tool-a"))
  (define js (list-active-tools-jsexpr reg))
  (check-equal? (length js) 1)
  (check-equal? (hash-ref (hash-ref (car js) 'function) 'name) "tool-a"))

(test-case "tool->jsexpr includes promptGuidelines when set"
  (define t (make-tool "mytool" "desc" (hasheq) void #:prompt-guidelines "Use carefully"))
  (define js (tool->jsexpr t))
  (check-equal? (hash-ref (hash-ref js 'function) 'promptGuidelines) "Use carefully"))

(test-case "tool->jsexpr omits promptGuidelines when #f"
  (define t (make-tool "mytool" "desc" (hasheq) void))
  (define js (tool->jsexpr t))
  (check-false (hash-ref (hash-ref js 'function) 'promptGuidelines #f)))

(test-case "unregister-tool! removes from registry"
  (define reg (make-tool-registry))
  (register-tool! reg (make-tool "tool-a" "A" (hasheq) void))
  (check-equal? (length (list-tools reg)) 1)
  (unregister-tool! reg "tool-a")
  (check-equal? (length (list-tools reg)) 0))
