#lang racket/base
;; tools/schema-helpers.rkt — Tool schema validation and hint formatting
;; Extracted from tools/tool.rkt (v0.30.8 W0)
;; STABILITY: stable

(require racket/set
         (only-in racket/string string-contains? string-join)
         (only-in "tool-struct.rkt" tool? tool-name tool-schema))

(provide validate-tool-args
         validate-tool-schema
         validate-tool-schema-strict
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

;; Strict schema validation — runs at tool construction time (#16, v0.99.72 W1).
;; Returns (void) on success, raises exn:fail on any schema violation.
;; Covers: top-level shape, property schemas, type keywords, array items,
;; enum values, numeric constraints, and string constraints.
(define (validate-tool-schema-strict schema tool-name-str)
  (define (fail! msg)
    (raise (exn:fail (format "Invalid schema for tool '~a': ~a" tool-name-str msg)
                     (current-continuation-marks))))
  (unless (hash? schema)
    (fail! (format "expected hash, got ~v" schema)))
  ;; Top-level must have type "object"
  (define type-val (hash-ref schema 'type #f))
  (unless type-val
    (fail! "missing 'type' key"))
  (unless (equal? type-val "object")
    (fail! (format "'type' must be \"object\", got ~v" type-val)))
  ;; Top-level must have 'properties (hash)
  (define props (hash-ref schema 'properties #f))
  (unless props
    (fail! "missing 'properties' key"))
  (unless (hash? props)
    (fail! (format "'properties' must be a hash, got ~v" props)))
  ;; Validate 'required if present
  (define required (hash-ref schema 'required #f))
  (when required
    (unless (list? required)
      (fail! (format "'required' must be a list, got ~v" required)))
    (for ([key (in-list required)])
      (unless (string? key)
        (fail! (format "'required' list element must be a string, got ~v" key)))
      ;; Key might be a string in required but a symbol in properties keys
      (define prop-key (string->symbol key))
      (unless (or (hash-has-key? props key) (hash-has-key? props prop-key))
        (fail! (format "required key '~a' not found in 'properties'" key)))))
  ;; Validate each property schema
  (for ([(prop-key prop-val) (in-hash props)])
    (unless (hash? prop-val)
      (fail! (format "property '~a' schema must be a hash, got ~v" prop-key prop-val)))
    (validate-property-schema prop-key prop-val fail!))
  #;(void))

(define (validate-property-schema prop-key prop-val fail!)
  ;; Validate 'type if present
  (define type-val (hash-ref prop-val 'type #f))
  (when type-val
    (define valid-types '("string" "integer" "number" "boolean" "object" "array"))
    (unless (and (string? type-val) (member type-val valid-types))
      (fail! (format "property '~a' has unknown type ~v (expected one of ~a)"
                     prop-key
                     type-val
                     valid-types))))
  ;; Validate 'items for array type
  (when (and type-val (equal? type-val "array"))
    (define items-val (hash-ref prop-val 'items #f))
    (unless items-val
      (fail! (format "property '~a' is type \"array\" but missing 'items'" prop-key)))
    (unless (hash? items-val)
      (fail! (format "property '~a' 'items' must be a hash schema, got ~v" prop-key items-val)))
    ;; Nested item schema validation
    (define item-type (hash-ref items-val 'type #f))
    (when item-type
      (define valid-types '("string" "integer" "number" "boolean" "object" "array"))
      (unless (and (string? item-type) (member item-type valid-types))
        (fail! (format "property '~a' 'items' type ~v is invalid" prop-key item-type))))
    ;; Validate nested enum in items
    (validate-property-enum prop-key items-val fail!)
    ;; Validate 'minLength/'maxLength in nested string schemas
    (validate-property-string-constraints prop-key items-val fail!))
  ;; Validate 'enum if present
  (validate-property-enum prop-key prop-val fail!)
  ;; Validate numeric constraints
  (validate-property-numeric-constraints prop-key prop-val fail!)
  ;; Validate string constraints
  (validate-property-string-constraints prop-key prop-val fail!)
  ;; Validate 'minItems/'maxItems for array type
  (when (and type-val (equal? type-val "array"))
    (validate-property-array-constraints prop-key prop-val fail!))
  ;; Validate 'minLength/'maxLength/'pattern for string type
  (when (and type-val (equal? type-val "string"))
    (define min-len (hash-ref prop-val 'minLength #f))
    (when (and min-len (not (exact-nonnegative-integer? min-len)))
      (fail!
       (format "property '~a' 'minLength' must be nonnegative integer, got ~v" prop-key min-len)))
    (define max-len (hash-ref prop-val 'maxLength #f))
    (when (and max-len (not (exact-nonnegative-integer? max-len)))
      (fail!
       (format "property '~a' 'maxLength' must be nonnegative integer, got ~v" prop-key max-len)))
    (define pattern (hash-ref prop-val 'pattern #f))
    (when (and pattern (not (string? pattern)))
      (fail! (format "property '~a' 'pattern' must be a string, got ~v" prop-key pattern)))))

(define (validate-property-enum prop-key prop-val fail!)
  (define enum-val (hash-ref prop-val 'enum #f))
  (when enum-val
    (unless (list? enum-val)
      (fail! (format "property '~a' 'enum' must be a list, got ~v" prop-key enum-val)))
    (for ([item (in-list enum-val)])
      (unless (string? item)
        (fail! (format "property '~a' 'enum' element must be a string, got ~v" prop-key item))))))

(define (validate-property-numeric-constraints prop-key prop-val fail!)
  (define minimum (hash-ref prop-val 'minimum #f))
  (when (and minimum (not (real? minimum)))
    (fail! (format "property '~a' 'minimum' must be a number, got ~v" prop-key minimum)))
  (define maximum (hash-ref prop-val 'maximum #f))
  (when (and maximum (not (real? maximum)))
    (fail! (format "property '~a' 'maximum' must be a number, got ~v" prop-key maximum))))

(define (validate-property-string-constraints prop-key prop-val fail!)
  (define min-len (hash-ref prop-val 'minLength #f))
  (when (and min-len (not (exact-nonnegative-integer? min-len)))
    (fail! (format "property '~a' 'minLength' must be nonnegative integer, got ~v" prop-key min-len)))
  (define max-len (hash-ref prop-val 'maxLength #f))
  (when (and max-len (not (exact-nonnegative-integer? max-len)))
    (fail!
     (format "property '~a' 'maxLength' must be nonnegative integer, got ~v" prop-key max-len))))

(define (validate-property-array-constraints prop-key prop-val fail!)
  (define min-items (hash-ref prop-val 'minItems #f))
  (when (and min-items (not (exact-nonnegative-integer? min-items)))
    (fail!
     (format "property '~a' 'minItems' must be nonnegative integer, got ~v" prop-key min-items)))
  (define max-items (hash-ref prop-val 'maxItems #f))
  (when (and max-items (not (exact-nonnegative-integer? max-items)))
    (fail!
     (format "property '~a' 'maxItems' must be nonnegative integer, got ~v" prop-key max-items))))

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
