#lang racket/base

;; util/event-macro.rkt -- event struct definition macros + serializer registry
;;
;; Provides:
;;   define-typed-event  -- full code generation: struct, constructor, type,
;;                          fields, serializer, deserializer, provide
;;   lookup-event-fields -- runtime lookup of field names by struct name symbol
;;   Serializer registry -- register/lookup serialization functions by type string
;;
;; H-01: Macro auto-generates serialization/deserialization, eliminating
;; the need for manual match arms in event-json.rkt.

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/string
                     racket/list)
         racket/string
         (only-in "../agent/event-structs/base.rkt" typed-event))

(provide define-typed-event
         lookup-event-fields
         register-event-fields!
         ;; Serializer registry
         register-event-serializer!
         register-event-deserializer!
         lookup-event-serializer
         lookup-event-deserializer
         ;; R-14: Parameterized registries for test isolation
         current-event-field-registry
         current-event-serializer-registry
         current-event-deserializer-registry
         with-fresh-event-registries
         ;; JSON key conversion
         field->json-key)

;; ===========================================================
;; Event field registry (I-12, I-23) -- canonical runtime mechanism for serialization
;; ===========================================================

;; R-14: Parameterized registries for test isolation
(define current-event-field-registry (make-parameter (make-hasheq)))

(define (register-event-fields! name fields)
  (hash-set! (current-event-field-registry) name fields))

(define (lookup-event-fields name)
  (hash-ref (current-event-field-registry) name #f))

;; ===========================================================
;; Event serializer registry (H-01)
;; ===========================================================

;; R-14: Parameterized registries for test isolation
(define current-event-serializer-registry (make-parameter (make-hash)))
(define current-event-deserializer-registry (make-parameter (make-hash)))

(define (register-event-serializer! type-str fn)
  (hash-set! (current-event-serializer-registry) type-str fn))

(define (register-event-deserializer! type-str fn)
  (hash-set! (current-event-deserializer-registry) type-str fn))

(define (lookup-event-serializer type-str)
  (hash-ref (current-event-serializer-registry) type-str #f))

(define (lookup-event-deserializer type-str)
  (hash-ref (current-event-deserializer-registry) type-str #f))

;; R-14: Test isolation helper -- fresh registries in parameterize scope
(define-syntax-rule (with-fresh-event-registries body ...)
  (parameterize ([current-event-field-registry (make-hasheq)]
                 [current-event-serializer-registry (make-hash)]
                 [current-event-deserializer-registry (make-hash)])
    body ...))

;; ===========================================================
;; JSON key conversion (H-01)
;; ===========================================================

;; Convert Racket field name to JSON key:
;;   duration-ms -> durationMs, is-error? -> isError, model -> model
;; Preserves underscores: finish_reason -> finish_reason
(define (field->json-key field-sym)
  (define s (symbol->string field-sym))
  (define stripped
    (if (string-suffix? s "?")
        (substring s 0 (sub1 (string-length s)))
        s))
  (define parts (string-split stripped "-"))
  (if (<= (length parts) 1)
      (string->symbol stripped)
      (string->symbol
       (string-append (car parts)
                      (apply string-append
                             (map (lambda (p)
                                    (if (string=? p "")
                                        ""
                                        (string-append (string (char-upcase (string-ref p 0)))
                                                       (substring p 1))))
                                  (cdr parts)))))))

;; ===========================================================
;; define-typed-event macro (unified clause, I-23, H-01)
;; ===========================================================

(begin-for-syntax
  (define (make-kw-arg-list fields defaults)
    (apply append
           (for/list ([f fields]
                      [d defaults])
             (define kw (datum->syntax f (string->keyword (symbol->string (syntax-e f)))))
             (if d
                 (list kw #`[#,f #,d])
                 (list kw f)))))

  ;; Build JSON key list from field list + optional json-keys mapping
  (define (resolve-json-keys all-fields json-keys-stx)
    (define jk-list
      (if (syntax? json-keys-stx)
          (syntax->list json-keys-stx)
          '()))
    (if (null? jk-list)
        ;; Default: use field->json-key conversion
        (for/list ([f all-fields])
          (define fsym (syntax-e f))
          (define key (field->json-key-stx fsym))
          (cons fsym key))
        ;; Explicit mapping: (field1 key1 field2 key2 ...) -> ((field . key) ...)
        (let loop ([pairs jk-list]
                   [acc '()])
          (if (null? pairs)
              (reverse acc)
              (if (null? (cdr pairs))
                  (reverse acc) ;; odd count, skip
                  (loop (cddr pairs)
                        (cons (cons (syntax-e (car pairs)) (syntax-e (cadr pairs))) acc)))))))

  (define (field->json-key-stx field-sym)
    (define s (symbol->string field-sym))
    (define stripped
      (if (string-suffix? s "?")
          (substring s 0 (sub1 (string-length s)))
          s))
    (define parts (string-split stripped "-"))
    (if (<= (length parts) 1)
        (string->symbol stripped)
        (string->symbol
         (string-append (car parts)
                        (apply string-append
                               (map (lambda (p)
                                      (if (string=? p "")
                                          ""
                                          (string-append (string (char-upcase (string-ref p 0)))
                                                         (substring p 1))))
                                    (cdr parts)))))))

  ;; Build defaults list: required fields use #:defaults or ""
  ;; Optional fields use their specified default
  (define (resolve-defaults req-fields req-defaults-stx opt-fields opt-defaults)
    (define rd-list
      (if (syntax? req-defaults-stx)
          (syntax->list req-defaults-stx)
          '()))
    ;; Parse req-defaults: (field1 default1 field2 default2 ...)
    (define rd-hash
      (let loop ([pairs rd-list]
                 [h (hasheq)])
        (if (or (null? pairs) (null? (cdr pairs)))
            h
            (loop (cddr pairs) (hash-set h (syntax-e (car pairs)) (cadr pairs))))))
    (for/list ([f (append req-fields opt-fields)])
      (define fsym (syntax-e f))
      (cond
        [(hash-has-key? rd-hash fsym) (hash-ref rd-hash fsym)]
        [(member (syntax-e f) (map syntax-e opt-fields))
         ;; Find the corresponding opt-default
         (define idx (index-of (map syntax-e opt-fields) (syntax-e f)))
         (if idx
             (list-ref opt-defaults idx)
             #'#f)]
        [else #'#""]))))

(define-syntax (define-typed-event stx)
  (syntax-parse stx
    [(_ event-name:id
        type-str:str
        (req-field:id ...)
        (~optional (~seq #:optional ([opt-field:id opt-default:expr] ...))
                   #:defaults ([(opt-field 1) '()] [(opt-default 1) '()]))
        (~optional (~seq #:json-keys json-keys) #:defaults ([json-keys #'()]))
        (~optional (~seq #:defaults req-defaults) #:defaults ([req-defaults #'()]))
        (~optional (~and #:no-serialize ns-flag) #:defaults ([ns-flag #'#f])))
     (define opt-raw (attribute opt-field))
     (define opt-default-raw (attribute opt-default))
     (define opt-fields
       (cond
         [(not opt-raw) '()]
         [(syntax? opt-raw) (syntax->list opt-raw)]
         [(list? opt-raw) opt-raw]
         [else '()]))
     (define opt-defaults
       (cond
         [(not opt-default-raw) '()]
         [(syntax? opt-default-raw) (syntax->list opt-default-raw)]
         [(list? opt-default-raw) opt-default-raw]
         [else '()]))
     (define skip-serialize? (eq? (syntax-e (attribute ns-flag)) '#:no-serialize))
     (define req-fields (syntax->list #'(req-field ...)))
     (define all-fields (append req-fields opt-fields))
     (define all-defaults (append (map (lambda (_) #f) req-fields) opt-defaults))
     (define make-name (format-id #'event-name "make-~a" #'event-name))
     (define type-name (format-id #'event-name "~a-type" #'event-name))
     (define fields-name (format-id #'event-name "~a-fields" #'event-name))
     (define kw-args (make-kw-arg-list all-fields all-defaults))

     ;; JSON key resolution
     (define field-key-pairs (resolve-json-keys all-fields (attribute json-keys)))

     ;; Deserialization defaults
     (define deser-defaults
       (resolve-defaults req-fields (attribute req-defaults) opt-fields opt-defaults))

     ;; Generate serializer body: (hasheq 'key1 (accessor1 evt) 'key2 (accessor2 evt) ...)
     (define serializer-body
       (apply append
              (for/list ([fk field-key-pairs])
                (define fsym (car fk))
                (define json-key (cdr fk))
                (define accessor-id (format-id #'event-name "~a-~a" #'event-name fsym))
                (list #`'#,json-key #`(#,accessor-id evt)))))

     ;; Generate deserialization body: constructor call with hash-ref for each field
     (define deser-args
       (for/list ([fk field-key-pairs]
                  [default deser-defaults])
         (define json-key (cdr fk))
         #`(hash-ref h '#,json-key #,default)))

     (with-syntax ([make-name make-name]
                   [type-name type-name]
                   [fields-name fields-name]
                   [(all-field ...) all-fields]
                   [(kw-arg ...) kw-args]
                   [(serializer-pair ...) serializer-body]
                   [(deser-arg ...) deser-args])
       (if skip-serialize?
           #'(begin
               (struct event-name typed-event (all-field ...) #:transparent)
               (define type-name type-str)
               (define fields-name '(all-field ...))
               ;; I-12: Register field names for event-struct->hasheq runtime lookup
               (register-event-fields! 'event-name fields-name)
               (define (make-name #:session-id session-id
                                  #:turn-id turn-id
                                  #:timestamp [timestamp (current-seconds)]
                                  kw-arg ...)
                 (event-name type-str timestamp session-id turn-id all-field ...))
               (provide (struct-out event-name)
                        make-name
                        type-name
                        fields-name))
           #'(begin
               (struct event-name typed-event (all-field ...) #:transparent)
               (define type-name type-str)
               (define fields-name '(all-field ...))
               ;; I-12: Register field names for event-struct->hasheq runtime lookup
               (register-event-fields! 'event-name fields-name)
               (define (make-name #:session-id session-id
                                  #:turn-id turn-id
                                  #:timestamp [timestamp (current-seconds)]
                                  kw-arg ...)
                 (event-name type-str timestamp session-id turn-id all-field ...))
               ;; H-01: Auto-generated serializer
               (register-event-serializer! type-str (lambda (evt) (hasheq serializer-pair ...)))
               ;; H-01: Auto-generated deserializer
               (register-event-deserializer! type-str
                                             (lambda (type ts sid tid h)
                                               (event-name type ts sid tid deser-arg ...)))
               (provide (struct-out event-name)
                        make-name
                        type-name
                        fields-name))))]))
