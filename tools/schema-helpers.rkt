#lang racket/base
;; tools/schema-helpers.rkt — Tool schema validation and hint formatting
;; Extracted from tools/tool.rkt (v0.30.8 W0)
;; STABILITY: stable

(require racket/set
         (only-in racket/string string-contains? string-join)
         (only-in "tool-struct.rkt" tool? tool-name tool-schema))

(provide validate-tool-args
         validate-tool-schema
         format-tool-schema-hint
         merge-tool-lists)

;; ============================================================
;; Tool schema validation
;; ============================================================

(define (validate-tool-schema schema)
  (and (hash? schema)
       (hash-has-key? schema 'type)
       (equal? (hash-ref schema 'type) "object")
       (hash-has-key? schema 'properties)
       (hash? (hash-ref schema 'properties))))

;; Merges a list of extension-provided tool jsexprs into the base tool list.
(define (merge-tool-lists base-tools extension-tools)
  (define ext-by-name
    (for/hash ([t (in-list extension-tools)])
      (define fn (hash-ref t 'function t))
      (values (hash-ref fn 'name 'unknown) t)))
  (append base-tools
          (for/list ([(name spec) (in-hash ext-by-name)])
            spec)))

;; ============================================================
;; Argument validation
;; ============================================================

(define (validate-tool-args t args)
  (unless (hash? args)
    (raise (exn:fail (format "validate-tool-args: args must be a hash, got ~a" args)
                     (current-continuation-marks))))
  (define schema (tool-schema t))
  ;; Only validate if schema declares required or properties
  (define required (hash-ref schema 'required #f))
  (define properties (hash-ref schema 'properties #f))
  ;; Check required keys
  (when (and required (list? required))
    (for ([key (in-list required)])
      (unless (hash-has-key? args
                             (if (string? key)
                                 (string->symbol key)
                                 key))
        (raise
         (exn:fail
          (format
           "validate-tool-args: missing required argument '~a' for tool '~a'.~a"
           key
           (tool-name t)
           (if (and (eq? key 'path) (eq? (tool-name t) "read"))
               " You must include 'path' in every read call, even when making parallel calls with different offsets."
               ""))
          (current-continuation-marks))))))
  ;; Check types for present keys
  (when (and properties (hash? properties))
    (for ([(arg-key arg-val) (in-hash args)])
      (define prop-spec
        (or (hash-ref properties arg-key #f) (hash-ref properties (symbol->string arg-key) #f)))
      (when prop-spec
        (define expected-type (hash-ref prop-spec 'type #f))
        (when expected-type
          (unless (type-matches? arg-val expected-type)
            (raise
             (exn:fail
              (format "validate-tool-args: argument '~a' expected type '~a', got ~v for tool '~a'"
                      arg-key
                      expected-type
                      arg-val
                      (tool-name t))
              (current-continuation-marks))))))))
  #t)

;; Basic type checking against JSON Schema type strings
(define (type-matches? v type-str)
  (case type-str
    [("string") (string? v)]
    [("integer") (exact-integer? v)]
    [("number") (real? v)]
    [("boolean") (boolean? v)]
    [("object") (hash? v)]
    [("array") (list? v)]
    [else #t])) ; unknown type spec -> pass

;; ============================================================
;; Tool schema hint formatting (v0.19.3 Wave 1)
;; ============================================================

(define (format-tool-schema-hint t)
  (define schema (tool-schema t))
  (define props (hash-ref schema 'properties (hasheq)))
  (define required
    (for/set ([r (in-list (hash-ref schema 'required '()))])
      (if (string? r)
          (string->symbol r)
          r)))
  (define param-strs
    (for/list ([(k v) (in-hash props)])
      (define key-sym
        (if (string? k)
            (string->symbol k)
            k))
      (define type-str (hash-ref v 'type "any"))
      (if (set-member? required key-sym)
          (format "~a: ~a" key-sym type-str)
          (format "~a?: ~a" key-sym type-str))))
  ;; Sort: required first, then optional, alphabetically within each group
  (define sorted
    (sort param-strs
          (lambda (a b)
            (define a-req? (not (string-contains? a "?")))
            (define b-req? (not (string-contains? b "?")))
            (cond
              [(and a-req? (not b-req?)) #t]
              [(and (not a-req?) b-req?) #f]
              [else (string<? a b)]))))
  (format "~a(~a)" (tool-name t) (string-join sorted ", ")))
