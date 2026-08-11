#lang racket/base

;; @speed fast
;; @suite security

;; Test-only loader and integrity gate for v0.99.91 W2-B wire fixtures.
;; This module deliberately imports no provider parser. Provider-specific parser
;; dispatch stays explicit in tests/test-provider-differential-fixtures.rkt.

(provide differential-fixture-version
         differential-fixture-root
         differential-fixture-providers
         differential-fixture-kinds
         differential-fixture-entries
         differential-fixture-entry?
         differential-fixture-entry-provider
         differential-fixture-entry-kind
         differential-fixture-entry-path
         differential-fixture-entry-representation
         differential-fixture-entry-sha256
         differential-fixture-entry-expected
         differential-fixture-entry-note
         differential-fixture-entry-for
         load-differential-bytes
         load-differential-json
         check-differential-fixtures!)

(require json
         openssl
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         (only-in file/sha1 bytes->hex-string)
         "../../util/credential-redaction.rkt")

(define differential-fixture-version 1)
(define-runtime-path differential-fixture-root/raw "../fixtures/provider-differential/v1")
(define-runtime-path manifest-path "../fixtures/provider-differential/v1/manifest.json")
(define differential-fixture-root (simplify-path differential-fixture-root/raw))

(define required-providers '(anthropic gemini openai-compatible azure-openai))
(define required-kinds '(framing tools usage malformed timeout))
(define required-manifest-keys '(entries fixture_set fixture_version kinds manifest_schema providers))
(define required-entry-keys '(expected kind note path provider representation sha256))

(define manifest (call-with-input-file manifest-path read-json))

(define differential-fixture-providers (map string->symbol (hash-ref manifest 'providers '())))
(define differential-fixture-kinds (map string->symbol (hash-ref manifest 'kinds '())))

(struct differential-fixture-entry (provider kind path representation sha256 expected note)
  #:transparent)

(define differential-fixture-entries
  (for/list ([raw (in-list (hash-ref manifest 'entries '()))])
    (differential-fixture-entry (string->symbol (hash-ref raw 'provider ""))
                                (string->symbol (hash-ref raw 'kind ""))
                                (hash-ref raw 'path "")
                                (string->symbol (hash-ref raw 'representation ""))
                                (hash-ref raw 'sha256 "")
                                (hash-ref raw 'expected (hash))
                                (hash-ref raw 'note ""))))

(define (differential-fixture-entry-for provider kind)
  (for/first ([fixture (in-list differential-fixture-entries)]
              #:when (and (eq? provider (differential-fixture-entry-provider fixture))
                          (eq? kind (differential-fixture-entry-kind fixture))))
    fixture))

(define (entry-full-path fixture)
  (build-path differential-fixture-root (differential-fixture-entry-path fixture)))

(define (load-differential-bytes fixture)
  (file->bytes (entry-full-path fixture)))

(define (load-differential-json fixture)
  (unless (eq? (differential-fixture-entry-representation fixture) 'json)
    (raise-arguments-error 'load-differential-json
                           "fixture is not represented as JSON"
                           "path"
                           (differential-fixture-entry-path fixture)))
  (call-with-input-file (entry-full-path fixture) read-json))

(define (sha256-file path)
  (call-with-input-file path (lambda (in) (bytes->hex-string (sha256-bytes in))) #:mode 'binary))

(define (relative-file-paths)
  (sort (for/list ([path (in-directory differential-fixture-root)]
                   #:when (and (file-exists? path)
                               (not (equal? (path->string (file-name-from-path path))
                                            "manifest.json"))))
          (path->string (find-relative-path differential-fixture-root path)))
        string<?))

(define (duplicates values)
  (remove-duplicates (for/list ([value (in-list values)]
                                #:when (> (count (lambda (candidate) (equal? candidate value)) values)
                                          1))
                       value)))

(define (safe-relative-path? value)
  (and (string? value)
       (positive? (string-length value))
       (not (absolute-path? (string->path value)))
       (not (regexp-match? #px"(^|[/\\\\])\\.\\.($|[/\\\\])" value))
       (not (string-contains? value "\\"))))

(define (expected-representation kind)
  (if (member kind '(framing malformed)) 'bytes 'json))

(define forbidden-fixture-text
  (list #px"(?i:authorization[ ]*:)"
        #px"(?i:proxy-authorization[ ]*:)"
        #px"(?i:x-api-key[ ]*:)"
        #px"(?i:set-cookie[ ]*:)"
        #px"(?i:cookie[ ]*:)"
        #px"-----BEGIN [A-Z ]*PRIVATE KEY-----"
        (regexp (string-append "/" "home/"))
        #px"[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}"))

(define environment-secret-names
  '("ANTHROPIC_API_KEY" "GEMINI_API_KEY"
                        "GOOGLE_API_KEY"
                        "OPENAI_API_KEY"
                        "AZURE_OPENAI_API_KEY"
                        "GITHUB_TOKEN"
                        "GH_TOKEN"))

(define (fixture-security-problems fixture bytes)
  (define path (differential-fixture-entry-path fixture))
  (define text (bytes->string/utf-8 bytes #\uFFFD))
  (append (if (contains-secret-leak? text)
              (list (list 'secret-like-text path))
              '())
          (for/list ([pattern (in-list forbidden-fixture-text)]
                     #:when (regexp-match? pattern text))
            (list 'forbidden-capture-text path (object-name pattern)))
          (for/list ([name (in-list environment-secret-names)]
                     #:do [(define value (getenv name))]
                     #:when (and value (> (string-length value) 8) (string-contains? text value)))
            (list 'environment-secret-leak path name))))

(define (json-schema-problems fixture)
  (if (eq? (differential-fixture-entry-representation fixture) 'json)
      (with-handlers ([exn:fail? (lambda (error)
                                   (list (list 'invalid-json
                                               (differential-fixture-entry-path fixture)
                                               (exn-message error))))])
        (define document (load-differential-json fixture))
        (append (if (equal? (hash-ref document 'fixture_schema #f) 1)
                    '()
                    (list (list 'fixture-schema (differential-fixture-entry-path fixture))))
                (if (equal? (hash-ref document 'provider #f)
                            (symbol->string (differential-fixture-entry-provider fixture)))
                    '()
                    (list (list 'fixture-provider (differential-fixture-entry-path fixture))))
                (if (equal? (hash-ref document 'kind #f)
                            (symbol->string (differential-fixture-entry-kind fixture)))
                    '()
                    (list (list 'fixture-kind (differential-fixture-entry-path fixture))))))
      '()))

(define (check-differential-fixtures!)
  (define raw-entries (hash-ref manifest 'entries '()))
  (define pairs
    (for/list ([fixture (in-list differential-fixture-entries)])
      (list (differential-fixture-entry-provider fixture) (differential-fixture-entry-kind fixture))))
  (define paths (map differential-fixture-entry-path differential-fixture-entries))
  (define expected-pairs
    (for*/list ([provider (in-list required-providers)]
                [kind (in-list required-kinds)])
      (list provider kind)))
  (define problems
    (append
     (if (equal? (sort (hash-keys manifest) symbol<?) required-manifest-keys)
         '()
         (list (list 'manifest-keys (sort (hash-keys manifest) symbol<?))))
     (if (equal? (hash-ref manifest 'fixture_set #f) "provider-differential")
         '()
         '((fixture-set)))
     (if (equal? (hash-ref manifest 'fixture_version #f) differential-fixture-version)
         '()
         '((fixture-version)))
     (if (equal? (hash-ref manifest 'manifest_schema #f) 1)
         '()
         '((manifest-schema)))
     (if (equal? differential-fixture-providers required-providers)
         '()
         '((provider-order)))
     (if (equal? differential-fixture-kinds required-kinds)
         '()
         '((kind-order)))
     (if (equal? pairs expected-pairs)
         '()
         (list (list 'entry-order pairs)))
     (for/list ([pair (in-list (duplicates pairs))])
       (list 'duplicate-pair pair))
     (for/list ([path (in-list (duplicates paths))])
       (list 'duplicate-path path))
     (if (equal? (sort paths string<?) (relative-file-paths))
         '()
         (list (list 'manifest-file-bijection (sort paths string<?) (relative-file-paths))))
     (for/list ([raw (in-list raw-entries)]
                #:unless (equal? (sort (hash-keys raw) symbol<?) required-entry-keys))
       (list 'entry-keys (hash-ref raw 'path #f)))
     (apply append
            (for/list ([fixture (in-list differential-fixture-entries)])
              (define path (differential-fixture-entry-path fixture))
              (define full-path (entry-full-path fixture))
              (define kind (differential-fixture-entry-kind fixture))
              (define representation (differential-fixture-entry-representation fixture))
              (append (if (safe-relative-path? path)
                          '()
                          (list (list 'unsafe-path path)))
                      (if (file-exists? full-path)
                          '()
                          (list (list 'missing-file path)))
                      (if (eq? representation (expected-representation kind))
                          '()
                          (list (list 'representation path representation)))
                      (if (and (eq? representation 'bytes) (string-suffix? path ".sse"))
                          '()
                          (if (and (eq? representation 'json) (string-suffix? path ".json"))
                              '()
                              (list (list 'extension path))))
                      (if (and (string? (differential-fixture-entry-note fixture))
                               (positive? (string-length (differential-fixture-entry-note fixture))))
                          '()
                          (list (list 'missing-note path)))
                      (if (and (file-exists? full-path)
                               (equal? (sha256-file full-path)
                                       (differential-fixture-entry-sha256 fixture)))
                          '()
                          (list (list 'digest path)))
                      (if (file-exists? full-path)
                          (fixture-security-problems fixture (file->bytes full-path))
                          '())
                      (if (file-exists? full-path)
                          (json-schema-problems fixture)
                          '()))))))
  problems)
