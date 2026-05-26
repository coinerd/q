#lang racket/base

;; runtime/oauth.rkt — OAuth2 configuration and token management
;;
;; Provides OAuth2 structs, URL generation, and token lifecycle
;; for SSO/corporate auth flows. Token exchange and refresh are
;; stub implementations pending an HTTP client library.

(require "../util/json-helpers.rkt")
(require "../util/errors.rkt")
(require racket/contract
         racket/string
         racket/format
         json
         racket/file
         racket/port
         racket/path
         net/uri-codec
         net/http-client
         net/url)

;; OAuth config struct
(provide oauth-config
         oauth-config?
         oauth-config-authorize-url
         oauth-config-token-url
         oauth-config-client-id
         oauth-config-client-secret
         oauth-config-scopes
         oauth-config-redirect-port
         ;; OAuth token struct
         oauth-token
         oauth-token?
         oauth-token-access-token
         oauth-token-refresh-token
         oauth-token-expires-at
         oauth-token-token-type
         oauth-token-scope
         (contract-out
          ;; Feature availability
          [oauth-available? (-> boolean?)]
          ;; Predicates / validation
          [oauth-token-expired? (->* (oauth-token?) (exact-integer?) boolean?)]
          [valid-oauth-config? (-> any/c boolean?)]
          ;; URL generation
          [oauth-authorize-url (->* (oauth-config? string?) ((or/c string? #f)) string?)]
          ;; Token exchange
          [oauth-exchange-code
           (->* (oauth-config? string?) (#:code-verifier (or/c string? #f)) (or/c oauth-token? #f))]
          ;; Token refresh
          [oauth-refresh-token (-> oauth-config? oauth-token? (or/c oauth-token? #f))]
          ;; Token persistence
          [oauth-token-file-path (-> path?)]
          [load-oauth-tokens (->* () ((or/c path-string? path?)) hash?)]
          [save-oauth-token-file! (->* (hash?) ((or/c path-string? path?)) void?)]
          [store-oauth-token! (->* (string? oauth-token?) (#:path (or/c path-string? path?)) void?)]
          [get-oauth-token (->* (string?) (#:path (or/c path-string? path?)) (or/c oauth-token? #f))]
          [get-valid-oauth-token
           (->* (string? oauth-config?) (#:path (or/c path-string? path?)) (or/c oauth-token? #f))]
          ;; Serialization helpers
          [oauth-token->jsexpr (-> oauth-token? hash?)]
          [jsexpr->oauth-token (-> any/c (or/c oauth-token? #f))])
         current-oauth-http-sendrecv)

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
;; Feature availability
;; ═══════════════════════════════════════════════════════════════════

;; Returns #t — OAuth token exchange/refresh now implemented.
(define (oauth-available?)
  #t)

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
;; Includes PKCE code_challenge when challenge is provided.
;; W5.7 (S4-06): URI-encode all query parameters
(define (oauth-authorize-url cfg state-param [code-challenge #f])
  (define base-url
    (format "~a?client_id=~a&redirect_uri=~a&response_type=code&scope=~a&state=~a"
            (oauth-config-authorize-url cfg)
            (uri-encode (oauth-config-client-id cfg))
            (uri-encode (format "http://localhost:~a/callback" (oauth-config-redirect-port cfg)))
            (uri-encode (string-join (oauth-config-scopes cfg) " "))
            (uri-encode state-param)))
  (if code-challenge
      (string-append base-url
                     "&code_challenge="
                     (uri-encode code-challenge)
                     "&code_challenge_method=S256")
      base-url))

;; ═══════════════════════════════════════════════════════════════════
;; Injectable HTTP transport (for testing)
;; ═══════════════════════════════════════════════════════════════════

;; Injectable HTTP transport. When set, called instead of http-sendrecv.
;; Signature: (-> string? string? #:ssl? boolean? #:method bytes? #:headers (listof string?) #:data bytes?
;;                (values bytes? (listof string?) input-port?))
(define current-oauth-http-sendrecv (make-parameter #f))

;; Perform HTTP POST for token operations. Uses injected transport if available.
(define (oauth-http-post host path ssl? headers body-bytes)
  (define transport (current-oauth-http-sendrecv))
  (if transport
      (transport host path #:ssl? ssl? #:method #"POST" #:headers headers #:data body-bytes)
      (http-sendrecv host path #:ssl? ssl? #:method #"POST" #:headers headers #:data body-bytes)))

;; ═══════════════════════════════════════════════════════════════════
;; Token exchange & refresh
;; ═══════════════════════════════════════════════════════════════════

;; Exchange authorization code for token.
;; Returns oauth-token on success, #f on failure.
(define (oauth-exchange-code cfg code #:code-verifier [code-verifier #f])
  (define token-url (oauth-config-token-url cfg))
  (define redirect-uri (format "http://localhost:~a/callback" (oauth-config-redirect-port cfg)))
  (define body-parts
    (append (list (cons 'grant_type "authorization_code")
                  (cons 'code code)
                  (cons 'client_id (oauth-config-client-id cfg))
                  (cons 'redirect_uri redirect-uri))
            (if (oauth-config-client-secret cfg)
                (list (cons 'client_secret (oauth-config-client-secret cfg)))
                '())
            (if code-verifier
                (list (cons 'code_verifier code-verifier))
                '())))
  (define body-str (alist->form-urlencoded body-parts))
  (define body-bytes (string->bytes/utf-8 body-str))
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define u (string->url token-url))
    (define host (url-host u))
    (define ssl? (equal? (url-scheme u) "https"))
    (define path-str
      (string-append "/" (string-join (map (lambda (p) (path/param-path p)) (url-path u)) "/")))
    (define-values (status _headers response-in)
      (oauth-http-post host
                       path-str
                       ssl?
                       (list "Content-Type: application/x-www-form-urlencoded")
                       body-bytes))
    (define response-bytes (port->bytes response-in))
    (close-input-port response-in)
    (unless (regexp-match? #rx#"^HTTP/1.[01] 2" status)
      (log-warning "oauth-exchange-code: non-2xx status: ~a" status))
    (define response-json (bytes->jsexpr response-bytes))
    (define access-token (hash-ref response-json 'access_token #f))
    (cond
      [(not access-token) #f]
      [else
       (define expires-in (hash-ref response-json 'expires_in 3600))
       (oauth-token access-token
                    (hash-ref response-json 'refresh_token "")
                    (+ (current-seconds) expires-in)
                    (hash-ref response-json 'token_type "Bearer")
                    (hash-ref response-json 'scope ""))])))

;; Refresh an expired token.
;; Returns oauth-token on success, #f on failure.
(define (oauth-refresh-token cfg tok)
  (define token-url (oauth-config-token-url cfg))
  (define body-parts
    (append (list (cons 'grant_type "refresh_token")
                  (cons 'refresh_token (oauth-token-refresh-token tok))
                  (cons 'client_id (oauth-config-client-id cfg)))
            (if (oauth-config-client-secret cfg)
                (list (cons 'client_secret (oauth-config-client-secret cfg)))
                '())))
  (define body-str (alist->form-urlencoded body-parts))
  (define body-bytes (string->bytes/utf-8 body-str))
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define u (string->url token-url))
    (define host (url-host u))
    (define ssl? (equal? (url-scheme u) "https"))
    (define path-str
      (string-append "/" (string-join (map (lambda (p) (path/param-path p)) (url-path u)) "/")))
    (define-values (status _headers response-in)
      (oauth-http-post host
                       path-str
                       ssl?
                       (list "Content-Type: application/x-www-form-urlencoded")
                       body-bytes))
    (define response-bytes (port->bytes response-in))
    (close-input-port response-in)
    (unless (regexp-match? #rx#"^HTTP/1.[01] 2" status)
      (log-warning "oauth-refresh-token: non-2xx status: ~a" status))
    (define response-json (bytes->jsexpr response-bytes))
    (define access-token (hash-ref response-json 'access_token #f))
    (cond
      [(not access-token) #f]
      [else
       (define expires-in (hash-ref response-json 'expires_in 3600))
       (oauth-token access-token
                    (hash-ref response-json 'refresh_token (oauth-token-refresh-token tok))
                    (+ (current-seconds) expires-in)
                    (hash-ref response-json 'token_type "Bearer")
                    (hash-ref response-json 'scope ""))])))

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
       (define content (read-json-file path))
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
                               (log-warning "save-oauth-token-file! failed: ~a" (exn-message e)))])
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
      (write-json-file tmp file-content)
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
