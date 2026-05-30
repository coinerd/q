#lang racket/base

;; runtime/credentials/file-backend.rkt — JSON file credential backend
;; Extracted from credential-backend.rkt (v0.73.5 A-4)

(require racket/contract
         racket/string
         json
         racket/file
         racket/path
         "../../util/json-helpers.rkt"
         "../../util/error-helpers.rkt"
         (only-in "protocol.rkt"
                  credential-backend
                  credential-backend?))

(provide make-file-credential-backend)

(define (make-file-credential-backend [path #f])
  (define cred-path (or path (build-path (find-system-path 'home-dir) ".q" "credentials.json")))
  (credential-backend "file"
                      ;; store!
                      (λ (be provider-name api-key) (file-store! cred-path provider-name api-key))
                      ;; load
                      (λ (be provider-name env-var) (file-load cred-path provider-name))
                      ;; delete!
                      (λ (be provider-name) (file-delete! cred-path provider-name))
                      ;; list
                      (λ (be) (file-list-providers cred-path))
                      ;; available?
                      (λ (be) (file-available? cred-path))))

(define (file-load-raw path)
  (cond
    [(not (file-exists? path)) (hash)]
    [else
     (with-safe-fallback (hash)
                         (define content (read-json-file path))
                         (if (eof-object? content)
                             (hash)
                             (let ([providers (hash-ref content 'providers (hash))])
                               (for/hash ([(k v) (in-hash providers)])
                                 (values (if (symbol? k)
                                             (symbol->string k)
                                             k)
                                         v)))))]))

(define (file-store! path provider-name api-key)
  (define existing (file-load-raw path))
  (define updated (hash-set existing provider-name (hasheq 'api-key api-key)))
  (define file-content
    (hasheq 'providers
            (for/hash ([(k v) (in-hash updated)])
              (values (if (string? k)
                          (string->symbol k)
                          k)
                      v))))
  (file-write-atomic! path file-content))

(define (file-load path provider-name)
  (define all (file-load-raw path))
  (define entry (hash-ref all provider-name #f))
  (cond
    [(not entry) #f]
    [else
     (define key (hash-ref entry 'api-key #f))
     (if (and key (non-empty-string? key))
         (hasheq 'api-key key 'source "file" 'provider provider-name)
         #f)]))

(define (file-delete! path provider-name)
  (define all (file-load-raw path))
  (define updated (hash-remove all provider-name))
  (define file-content
    (hasheq 'providers
            (for/hash ([(k v) (in-hash updated)])
              (values (if (string? k)
                          (string->symbol k)
                          k)
                      v))))
  (file-write-atomic! path file-content))

(define (file-list-providers path)
  (hash-keys (file-load-raw path)))

(define (file-available? path)
  (cond
    [(file-exists? path) #t]
    [else
     (define dir (path-only path))
     (and dir (or (directory-exists? dir) (with-safe-fallback #f (make-directory* dir) #t)))]))

(define (file-write-atomic! path data)
  (define dir (path-only path))
  (when (and dir (not (directory-exists? dir)))
    (make-directory* dir))
  (define tmp (make-temporary-file "credential-~a.tmp" #f (or dir (find-system-path 'temp-dir))))
  (with-handlers ([exn:fail? (λ (e)
                               (with-safe-fallback (void) (delete-file tmp))
                               (raise e))])
    (file-or-directory-permissions tmp #o600)
    (write-json-file tmp data)
    (rename-file-or-directory tmp path #t)
    (file-or-directory-permissions path #o600)))
