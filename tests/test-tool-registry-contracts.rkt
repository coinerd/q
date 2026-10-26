#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: contract

;; tests/test-tool-registry-contracts.rkt -- R3: registry contract boundary tests
;; @boundary unit

(require rackunit
         rackunit/text-ui
         "../tools/registry.rkt"
         (only-in "../tools/tool.rkt"
                  make-tool
                  tool-execute
                  tool-result-details
                  tool-result-is-error?
                  tool-schema
                  validate-tool-args)
         ;; R94 (BUG-0094): save-conclusion wire schema + registry pipeline
         (only-in "../tools/registry-table.rkt"
                  register-tools-from-specs!
                  tool-spec-name
                  tool-spec-schema
                  tool-spec-description)
         (only-in "../tools/registry-table/skill-tools.rkt" skill-tool-specs)
         ;; R96 (BUG-0096): honest bounded read schema pins
         (only-in "../tools/registry-table/core-tools.rkt" core-tool-specs)
         (only-in "../tools/tool.rkt" tool-description)
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion-category
                  task-conclusion-relevance-tags))

(define (find-spec name)
  (findf (lambda (s) (equal? (tool-spec-name s) name)) skill-tool-specs))

(define (save-conclusion-reg)
  (define reg (make-tool-registry))
  (register-tools-from-specs! reg skill-tool-specs #:only '("save-conclusion"))
  reg)

;; R94: error results carry an EMPTY details hash, so "no stored conclusion"
;; means the '_conclusion key is absent — not that details is #f.
(define (stored-conclusion result)
  (define d (tool-result-details result))
  (and (hash? d) (hash-ref d '_conclusion #f)))

(define contract-suite
  (test-suite "tool registry contract boundaries"

    (test-case "tool-registry-contracts: register-tool! rejects non-tool"
      (define reg (make-tool-registry))
      (check-exn exn:fail:contract? (lambda () (register-tool! reg "not-a-tool"))))

    (test-case "lookup-tool with #f returns #f"
      (define reg (make-tool-registry))
      (check-equal? (lookup-tool reg #f) #f))

    (test-case "set-active-tools! rejects non-strings"
      (define reg (make-tool-registry))
      (check-exn exn:fail:contract? (lambda () (set-active-tools! reg '(1 2 3)))))

    (test-case "tool-active? requires string name"
      (define reg (make-tool-registry))
      (check-exn exn:fail:contract? (lambda () (tool-active? reg 123))))

    (test-case "unregister-tool! requires string name"
      (define reg (make-tool-registry))
      (check-exn exn:fail:contract? (lambda () (unregister-tool! reg 42))))

    (test-case "make-tool-registry returns registry"
      (check-pred tool-registry? (make-tool-registry)))

    (test-case "tool-names returns strings"
      (define reg (make-tool-registry))
      (define t (make-tool "test" "desc" (hasheq 'type "object" 'properties (hasheq)) void))
      (register-tool! reg t)
      (define names (tool-names reg))
      (check-true (andmap string? names)))

    (test-case "list-active-tools-jsexpr returns hashes"
      (define reg (make-tool-registry))
      (define t (make-tool "test" "desc" (hasheq 'type "object" 'properties (hasheq)) void))
      (register-tool! reg t)
      (define js (list-active-tools-jsexpr reg))
      (check-true (andmap hash? js)))

    ;; ============================================================
    ;; R94 (BUG-0094): save-conclusion wire schema contract — strings
    ;; stay the advertised types (no broadening), and the registry
    ;; pipeline (JSON-decoded args -> validate-tool-args -> handler)
    ;; accepts the five known string categories while rejecting
    ;; malformed values with zero side effects.
    ;; ============================================================

    (test-case "tool-registry-contracts: R94 wire schema keeps strings advertised"
      (for ([name (in-list '("save-conclusion" "record_conclusion"))])
        (define spec (find-spec name))
        (check-not-false spec)
        (define props (hash-ref (tool-spec-schema spec) 'properties))
        (check-equal? (hash-ref (hash-ref props 'category) 'type) "string"
                      (format "~a category stays string-typed" name))
        (define tags-prop (hash-ref props 'tags))
        (check-equal? (hash-ref tags-prop 'type) "array"
                      (format "~a tags stay array-typed" name))
        (check-equal? (hash-ref (hash-ref tags-prop 'items) 'type) "string"
                      (format "~a tag items stay string-typed" name))))

    (test-case "tool-registry-contracts: R94 registry schema matches spec schema"
      (define reg (save-conclusion-reg))
      (define t (lookup-tool reg "save-conclusion"))
      (check-not-false t)
      (check-equal? (tool-schema t) (tool-spec-schema (find-spec "save-conclusion"))))

    (test-case
     "tool-registry-contracts: R94 registry pipeline accepts JSON string categories"
      (define reg (save-conclusion-reg))
      (define t (lookup-tool reg "save-conclusion"))
      (for ([cat-str (in-list '("fact" "decision" "pattern" "error-cause" "test-result"))]
            [cat-sym (in-list '(fact decision pattern error-cause test-result))])
        (define args (hasheq 'content "wire" 'category cat-str))
        (check-true (validate-tool-args t args))
        (define result ((tool-execute t) args #f))
        (check-false (tool-result-is-error? result)
                     (format "string category ~a should succeed" cat-str))
        (check-eq? (task-conclusion-category
                     (hash-ref (tool-result-details result) '_conclusion))
                   cat-sym)))

    (test-case "tool-registry-contracts: R94 registry pipeline normalizes string tags"
      (define reg (save-conclusion-reg))
      (define t (lookup-tool reg "save-conclusion"))
      (define args (hasheq 'content "wire" 'tags '("alpha" "beta")))
      (check-true (validate-tool-args t args))
      (define result ((tool-execute t) args #f))
      (check-false (tool-result-is-error? result))
      (check-equal?
       (task-conclusion-relevance-tags (hash-ref (tool-result-details result) '_conclusion))
       '(alpha beta)))

    (test-case
     "tool-registry-contracts: R94 symbol category is schema-rejected on the wire path"
      ;; The wire schema advertises string; internal symbol callers use the
      ;; handler boundary directly, not the schema-validated wire path.
      (define reg (save-conclusion-reg))
      (define t (lookup-tool reg "save-conclusion"))
      (check-exn exn:fail?
                 (lambda () (validate-tool-args t (hasheq 'content "x" 'category 'fact)))))

    (test-case
     "tool-registry-contracts: R94 malformed values produce handler errors, no details"
      (define reg (save-conclusion-reg))
      (define t (lookup-tool reg "save-conclusion"))
      (for ([bad-args
             (in-list (list (hasheq 'content "x" 'category "Fact")
                            (hasheq 'content "x" 'category "")
                            (hasheq 'content "x" 'category "nope")
                            (hasheq 'content "x" 'tags (list "ok" 42))))])
        (check-true (validate-tool-args t bad-args))
        (define result ((tool-execute t) bad-args #f))
        (check-true (tool-result-is-error? result))
        (check-false (stored-conclusion result)
                     "rejection must not store a conclusion")))

    (test-case "tool-registry-contracts: R96 read spec declares bounded pagination"
      (define read-spec
        (findf (lambda (s) (string=? (tool-spec-name s) "read")) core-tool-specs))
      (check-not-false read-spec)
      (when read-spec
        (define schema (tool-spec-schema read-spec))
        (define props (hash-ref schema 'properties))
        (define offset-prop (hash-ref props 'offset))
        (define limit-prop (hash-ref props 'limit))
        ;; positive 1-based offset, positive limit with default/cap documented
        (check-equal? (hash-ref offset-prop 'type) "integer")
        (check-equal? (hash-ref offset-prop 'minimum) 1)
        (check-true (string-contains? (hash-ref offset-prop 'description) "1-based"))
        (check-true (string-contains? (hash-ref offset-prop 'description) "default 1"))
        (check-equal? (hash-ref limit-prop 'type) "integer")
        (check-equal? (hash-ref limit-prop 'minimum) 1)
        (check-true (string-contains? (hash-ref limit-prop 'description) "2000")
                    "limit description documents the 2000 default/cap")
        ;; spec description documents the byte cap + oversized-line error and
        ;; promises no full-file read
        (define desc (tool-spec-description read-spec))
        (check-true (string-contains? desc "50000") "byte cap documented")
        (check-true (string-contains? desc "oversized") "oversized-line error documented")
        (check-false (string-contains? desc "full file") "no full-file promise")
        (check-false (string-contains? desc "default all") "no default-all promise")))

    (test-case "tool-registry-contracts: R96 registered read tool matches spec and stays honest"
      (define reg (make-tool-registry))
      (register-tools-from-specs! reg core-tool-specs #:only '("read"))
      (define t (lookup-tool reg "read"))
      (check-not-false t)
      (when t
        (define read-spec
          (findf (lambda (s) (string=? (tool-spec-name s) "read")) core-tool-specs))
        (check-equal? (tool-schema t) (tool-spec-schema read-spec))
        (define desc (tool-description t))
        (check-true (string-contains? desc "50000") "tool description documents byte cap")
        (check-true (string-contains? desc "2000") "tool description documents line cap")
        (check-true (string-contains? desc "offset") "tool description documents offset")
        (check-false (string-contains? desc "default all") "no default-all promise")
        (check-false (string-contains? desc "full file") "no full-file promise")
        ;; the define-tool-declared schema on the tool itself also carries minimums
        (define tool-props (hash-ref (tool-schema t) 'properties))
        (check-equal? (hash-ref (hash-ref tool-props 'offset) 'minimum) 1)
        (check-equal? (hash-ref (hash-ref tool-props 'limit) 'minimum) 1)))))

(run-tests contract-suite 'verbose)
