#lang racket/base

;; runtime/oauth.rkt — OAuth2 configuration and token management
;;
;; Provides OAuth2 structs, URL generation, and token lifecycle
;; for SSO/corporate auth flows. Token exchange and refresh are
;; stub implementations pending an HTTP client library.

(require racket/string
         racket/format
         json
         racket/file
         racket/path)

;; OAuth config struct
(provide (struct-out oauth-config)
         ;; OAuth token struct
         (struct-out oauth-token)
         ;; Predicates / validation
         oauth-token-expired?
         valid-oauth-config?
         ;; URL generation
         oauth-authorize-url
         ;; Token exchange (stub)
         oauth-exchange-code
         ;; Token refresh (stub)
         oauth-refresh-token
         ;; Token persistence
         store-oauth-token!
         get-oauth-token
         get-valid-oauth-token
         ;; OAuth token file
         oauth-token-file-path
         load-oauth-tokens
         save-oauth-token-file!
         ;; Serialization helpers
         oauth-token->jsexpr
         jsexpr->oauth-token)

;; ═══════════════════════════════════════════════════════════════════
;; Structs
;; ═══════════════════════════════════════════════════════════════════

;; OAuth configuration for a provider
(struct oauth-config
        (authorize-url ; string — e.g. "https://accounts.google.com/o/oauth2/v2/auth"
         token-url ; string — e.g. "https://oauth2.googleapis.com/token"
         client-id ; string
         client-secret ; string
         scopes ; (listof string) — e.g. '("openid" "email")
         redirect-port) ; exact-positive-integer? — localhost callback port, e.g. 8089
  #:transparent)

;; Stored OAuth token
(struct oauth-token
        (access-token refresh-token
                      expires-at ; epoch seconds
                      token-type ; usually "Bearer"
                      scope)
  #:transparent)

;; ═══════════════════════════════════════════════════════════════════
;; Validation
;; ═══════════════════════════════════════════════════════════════════

;; Check if an oauth-token has expired, with a safety margin.
(define (oauth-token-expired? tok [margin-seconds 60])
  (> (current-seconds) (- (oauth-token-expires-at tok) margin-seconds)))

;; Validate oauth-config fields.
(define (valid-oauth-config? cfg)
  (and (oauth-config? cfg)
       (string? (oauth-config-authorize-url cfg))
       (string? (oauth-config-token-url cfg))
       (string? (oauth-config-client-id cfg))
       (> (string-length (oauth-config-client-id cfg)) 0)
       (exact-positive-integer? (oauth-config-redirect-port cfg))))

;; ═══════════════════════════════════════════════════════════════════
;; URL generation
;; ═══════════════════════════════════════════════════════════════════

;; Generate the authorization URL for browser redirect.
(define (oauth-authorize-url cfg state-param)
  (format
   "~a?client_id=~a&redirect_uri=http://localhost:~a/callback&response_type=code&scope=~a&state=~a"
   (oauth-config-authorize-url cfg)
   (oauth-config-client-id cfg)
   (oauth-config-redirect-port cfg)
   (string-join (oauth-config-scopes cfg) "+")
   state-param))

;; ═══════════════════════════════════════════════════════════════════
;; Token exchange & refresh (stubs)
;; ═══════════════════════════════════════════════════════════════════

;; Exchange authorization code for token.
;; Returns oauth-token on success, #f on failure.
;; STUB: Requires an HTTP client library (e.g. libcurl or net/http)
;; to perform the actual POST to the token endpoint.
(define (oauth-exchange-code cfg code)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    ;; Build the POST body for reference / future implementation
    (define _post-data
      (format
       "code=~a&client_id=~a&client_secret=~a&redirect_uri=http://localhost:~a/callback&grant_type=authorization_code"
       code
       (oauth-config-client-id cfg)
       (oauth-config-client-secret cfg)
       (oauth-config-redirect-port cfg)))
    ;; TODO: HTTP POST to (oauth-config-token-url cfg) with _post-data
    ;; Parse JSON response into oauth-token struct
    #f))

;; Refresh an expired token.
;; Returns oauth-token on success, #f on failure.
;; STUB: Same HTTP client dependency as oauth-exchange-code.
(define (oauth-refresh-token cfg tok)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    ;; Build the POST body for reference / future implementation
    (define _post-data
      (format "refresh_token=~a&client_id=~a&client_secret=~a&grant_type=refresh_token"
              (oauth-token-refresh-token tok)
              (oauth-config-client-id cfg)
              (oauth-config-client-secret cfg)))
    ;; TODO: HTTP POST to (oauth-config-token-url cfg) with _post-data
    ;; Parse JSON response into oauth-token struct
    #f))

;; ═══════════════════════════════════════════════════════════════════
;; Serialization helpers
;; ═══════════════════════════════════════════════════════════════════

;; Convert oauth-token to a JSON-friendly hash.
(define (oauth-token->jsexpr tok)
  (hasheq 'access-token
          (oauth-token-access-token tok)
          'refresh-token
          (oauth-token-refresh-token tok)
          'expires-at
          (oauth-token-expires-at tok)
          'token-type
          (oauth-token-token-type tok)
          'scope
          (oauth-token-scope tok)))

;; Convert a JSON hash back to oauth-token.
(define (jsexpr->oauth-token v)
  (and (hash? v)
       (hash-ref v 'access-token #f)
       (oauth-token (hash-ref v 'access-token)
                    (hash-ref v 'refresh-token "")
                    (hash-ref v 'expires-at 0)
                    (hash-ref v 'token-type "Bearer")
                    (hash-ref v 'scope ""))))

;; ═══════════════════════════════════════════════════════════════════
;; Token persistence
;; ═══════════════════════════════════════════════════════════════════

;; Default OAuth token file path: ~/.q/oauth-tokens.json
(define (oauth-token-file-path)
  (build-path (find-system-path 'home-dir) ".q" "oauth-tokens.json"))

;; Load all OAuth tokens from the token file.
;; Returns a hash: provider-name (string) → oauth-token
(define (load-oauth-tokens [path (oauth-token-file-path)])
  (cond
    [(not (file-exists? path)) (hash)]
    [else
     (with-handlers ([exn:fail? (lambda (e) (hash))])
       (define content (call-with-input-file path read-json))
       (if (eof-object? content)
           (hash)
           (for/hash ([(k v) (in-hash content)])
             (values (if (symbol? k)
                         (symbol->string k)
                         k)
                     (jsexpr->oauth-token v)))))]))

;; Save all OAuth tokens to the token file.
(define (save-oauth-token-file! tokens [path (oauth-token-file-path)])
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "save-oauth-token-file! failed: ~a"
                                                    (exn-message e))))])
    (define dir (path-only path))
    (when (and dir (not (directory-exists? dir)))
      (make-directory* dir))
    (define file-content
      (for/hash ([(k v) (in-hash tokens)])
        (values (if (string? k)
                    (string->symbol k)
                    k)
                (oauth-token->jsexpr v))))
    (define tmp (make-temporary-file "oauth-token-~a.tmp" #f (or dir (find-system-path 'temp-dir))))
    (with-handlers ([exn:fail? (lambda (e)
                                 (with-handlers ([exn:fail? (lambda (_) (void))])
                                   (delete-file tmp))
                                 (raise e))])
      (call-with-output-file tmp (lambda (out) (write-json file-content out)) #:exists 'truncate)
      (rename-file-or-directory tmp path #t)
      (file-or-directory-permissions path #o600))))

;; Store an OAuth token for a provider. Persists immediately.
(define (store-oauth-token! provider-name tok #:path [path (oauth-token-file-path)])
  (define existing (load-oauth-tokens path))
  (define updated (hash-set existing provider-name tok))
  (save-oauth-token-file! updated path)
  (void))

;; Retrieve an OAuth token for a provider. Returns oauth-token or #f.
(define (get-oauth-token provider-name #:path [path (oauth-token-file-path)])
  (define tokens (load-oauth-tokens path))
  (define v (hash-ref tokens provider-name #f))
  (and (oauth-token? v) v))

;; Get a valid (non-expired) token, refreshing if necessary.
;; Returns oauth-token or #f.
(define (get-valid-oauth-token provider-name cfg #:path [path (oauth-token-file-path)])
  (define tok (get-oauth-token provider-name #:path path))
  (cond
    [(not tok) #f]
    [(not (oauth-token-expired? tok)) tok]
    [else
     (define refreshed (oauth-refresh-token cfg tok))
     (and refreshed
          (begin
            (store-oauth-token! provider-name refreshed #:path path)
            refreshed))]))
