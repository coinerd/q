#lang racket/base

;; extensions/manifest.rkt -- q package manifest (qpm.json) read/write/validate
;;
;; Provides:
;;   - qpm-manifest struct with all package metadata fields
;;   - Validation, serialization (JSON), file I/O, comparison, and checksum

(require racket/contract
         racket/string
         racket/list
         racket/port
         racket/file
         json
         "../util/checksum.rkt")

;; ============================================================
;; Provides
;; ============================================================

(provide
 ;; Struct & predicate
 (struct-out qpm-manifest)
 qpm-type?
 ;; Constructor with keyword args
 (contract-out
  [make-qpm-manifest
   (->* [#:name string?
         #:version string?
         #:api-version string?
         #:type (or/c 'extension 'skill 'bundle)
         #:description string?
         #:author string?]
        [#:compat (or/c #f string?)
         #:files (listof string?)
         #:checksum (or/c #f string?)
         #:entry (or/c #f string?)
         #:homepage (or/c #f string?)
         #:license (or/c #f string?)]
        qpm-manifest?)]
  [validate-manifest     (-> qpm-manifest? (values boolean? (listof string?)))]
  [qpm-manifest->jsexpr  (-> qpm-manifest? hash?)]
  [jsexpr->qpm-manifest  (-> any/c (or/c qpm-manifest? #f))]
  [read-qpm-manifest     (-> (or/c path-string? path?) (or/c qpm-manifest? #f))]
  [write-qpm-manifest    (-> qpm-manifest? (or/c path-string? path?) void?)]
  [qpm-manifest=?        (-> qpm-manifest? qpm-manifest? boolean?)]
  [compute-manifest-checksum (-> (or/c path-string? path?) string?)]))

;; ============================================================
;; Struct definition
;; ============================================================

(struct qpm-manifest
  (name         ; string?
   version      ; string?
   api-version  ; string?
   type         ; (or/c 'extension 'skill 'bundle)
   description  ; string?
   author       ; string?
   compat       ; (or/c #f string?)
   files        ; (listof string?)
   checksum     ; (or/c #f string?)
   entry        ; (or/c #f string?)
   homepage     ; (or/c #f string?)
   license)     ; (or/c #f string?)
  #:transparent)

;; ============================================================
;; Predicates
;; ============================================================

(define (qpm-type? v)
  (and (symbol? v) (member v '(extension skill bundle)) #t))

;; ============================================================
;; Constructor
;; ============================================================

(define (make-qpm-manifest #:name name
                           #:version version
                           #:api-version api-version
                           #:type type
                           #:description description
                           #:author author
                           #:compat [compat #f]
                           #:files [files '()]
                           #:checksum [checksum #f]
                           #:entry [entry #f]
                           #:homepage [homepage #f]
                           #:license [license #f])
  (qpm-manifest name version api-version type description author
                compat files checksum entry homepage license))

;; ============================================================
;; Validation helpers
;; ============================================================

;; Loose semver check: digit(s).digit(s).digit(s)
(define (looks-like-semver? s)
  (and (string? s)
       (regexp-match? #rx"^[0-9]+\\.[0-9]+\\.[0-9]+" s)))

;; Semver range: e.g. ">=0.4.2", "^1.0.0", "~2.3.0"
(define (looks-like-semver-range? s)
  (and (string? s)
       (regexp-match? #rx"[<>=~^]" s)))

;; api-version: plain digit string like "1"
(define (looks-like-api-version? s)
  (and (string? s)
       (regexp-match? #rx"^[0-9]+$" s)))

;; ============================================================
;; Validation
;; ============================================================

(define (validate-manifest m)
  (define errors '())
  (define (add-error! msg)
    (set! errors (append errors (list msg))))
  ;; name
  (let ([n (qpm-manifest-name m)])
    (cond
      [(not (string? n))             (add-error! "name must be a string")]
      [(string=? (string-trim n) "") (add-error! "name must not be empty")]))
  ;; version
  (unless (looks-like-semver? (qpm-manifest-version m))
    (add-error! (format "version must look like semver (x.y.z), got: ~a"
                        (qpm-manifest-version m))))
  ;; api-version
  (unless (looks-like-api-version? (qpm-manifest-api-version m))
    (add-error! (format "api-version must be a digit string, got: ~a"
                        (qpm-manifest-api-version m))))
  ;; type
  (unless (qpm-type? (qpm-manifest-type m))
    (add-error! (format "type must be extension, skill, or bundle, got: ~a"
                        (qpm-manifest-type m))))
  ;; description
  (let ([d (qpm-manifest-description m)])
    (cond
      [(not (string? d))             (add-error! "description must be a string")]
      [(string=? (string-trim d) "") (add-error! "description must not be empty")]))
  ;; author
  (let ([a (qpm-manifest-author m)])
    (cond
      [(not (string? a))             (add-error! "author must be a string")]
      [(string=? (string-trim a) "") (add-error! "author must not be empty")]))
  ;; compat (optional)
  (let ([c (qpm-manifest-compat m)])
    (when (and c (not (looks-like-semver-range? c)))
      (add-error! (format "compat must look like a semver range, got: ~a" c))))
  ;; Return
  (values (null? errors) errors))

;; ============================================================
;; Serialization: manifest to jsexpr
;; ============================================================

(define (qpm-manifest->jsexpr m)
  (define h (make-hasheq))
  (hash-set! h 'name        (qpm-manifest-name m))
  (hash-set! h 'version     (qpm-manifest-version m))
  (hash-set! h 'api_version (qpm-manifest-api-version m))
  (hash-set! h 'type        (symbol->string (qpm-manifest-type m)))
  (hash-set! h 'description (qpm-manifest-description m))
  (hash-set! h 'author      (qpm-manifest-author m))
  (hash-set! h 'files       (qpm-manifest-files m))
  ;; Optional fields: only include if non-#f
  (let ([c (qpm-manifest-compat m)])
    (when c (hash-set! h 'compat c)))
  (let ([c (qpm-manifest-checksum m)])
    (when c (hash-set! h 'checksum c)))
  (let ([e (qpm-manifest-entry m)])
    (when e (hash-set! h 'entry e)))
  (let ([h2 (qpm-manifest-homepage m)])
    (when h2 (hash-set! h 'homepage h2)))
  (let ([l (qpm-manifest-license m)])
    (when l (hash-set! h 'license l)))
  ;; Convert to write-json-compatible: use string keys with underscores
  (for/hasheq ([(k v) (in-hash h)])
    (values (string->symbol (string-replace (symbol->string k) "-" "_")) v)))

;; ============================================================
;; Deserialization: jsexpr to manifest
;; ============================================================

;; JSON uses underscore-separated keys (api_version, not api-version)
;; to comply with write-json requirements (symbols only).

(define (json-get j key)
  ;; Try both underscore and hyphen versions
  (hash-ref j key
    (lambda ()
      (define alt (string->symbol
                    (string-replace (symbol->string key) "_" "-")))
      (hash-ref j alt
        (lambda () #f)))))

(define (json-req j key)
  (or (json-get j key)
      (error (format "missing required key: ~a" key))))

(define (jsexpr->qpm-manifest j)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (unless (hash? j) (error "not a hash"))
    (define type-val (json-get j 'type))
    (define type-sym
      (cond
        [(symbol? type-val) type-val]
        [(string? type-val) (string->symbol type-val)]
        [else (error "bad type value")]))
    (make-qpm-manifest
     #:name        (json-req j 'name)
     #:version     (json-req j 'version)
     #:api-version (or (json-get j 'api_version) (json-get j 'api-version) "")
     #:type        type-sym
     #:description (json-req j 'description)
     #:author      (json-req j 'author)
     #:compat      (json-get j 'compat)
     #:files       (or (json-get j 'files) '())
     #:checksum    (json-get j 'checksum)
     #:entry       (json-get j 'entry)
     #:homepage    (json-get j 'homepage)
     #:license     (json-get j 'license))))

;; ============================================================
;; File I/O
;; ============================================================

(define (read-qpm-manifest path)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (call-with-input-file path
      (lambda (in)
        (define j (read-json in))
        (jsexpr->qpm-manifest j))
      #:mode 'text)))

(define (write-qpm-manifest m path)
  (define h (qpm-manifest->jsexpr m))
  (call-with-output-file path
    (lambda (out)
      (write-json h out)
      (newline out))
    #:exists 'replace
    #:mode 'text))

;; ============================================================
;; Comparison
;; ============================================================

(define (qpm-manifest=? a b)
  (and (equal? (qpm-manifest-name a)        (qpm-manifest-name b))
       (equal? (qpm-manifest-version a)     (qpm-manifest-version b))
       (equal? (qpm-manifest-api-version a) (qpm-manifest-api-version b))
       (equal? (qpm-manifest-type a)        (qpm-manifest-type b))
       (equal? (qpm-manifest-description a) (qpm-manifest-description b))
       (equal? (qpm-manifest-author a)      (qpm-manifest-author b))
       (equal? (qpm-manifest-compat a)      (qpm-manifest-compat b))
       (equal? (qpm-manifest-files a)       (qpm-manifest-files b))
       (equal? (qpm-manifest-checksum a)    (qpm-manifest-checksum b))
       (equal? (qpm-manifest-entry a)       (qpm-manifest-entry b))
       (equal? (qpm-manifest-homepage a)    (qpm-manifest-homepage b))
       (equal? (qpm-manifest-license a)     (qpm-manifest-license b))))

;; ============================================================
;; Checksum -- SHA-256 of all listed files, sorted and concatenated
;; Delegates to sha256-string from util/checksum.rkt

(define (compute-manifest-checksum manifest-dir)
  (define qpm-path (build-path manifest-dir "qpm.json"))
  (define m (read-qpm-manifest qpm-path))
  (unless m
    (error 'compute-manifest-checksum "cannot read ~a" qpm-path))
  (define sorted-files (sort (qpm-manifest-files m) string<?))
  (define digest-input
    (call-with-output-string
     (lambda (out)
       (for ([f (in-list sorted-files)])
         (define full-path (build-path manifest-dir f))
         (call-with-input-file full-path
           (lambda (in) (display (port->string in) out)))))))
  (sha256-string digest-input))
