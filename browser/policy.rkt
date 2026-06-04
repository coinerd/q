#lang racket

;; browser/policy.rkt — Browser URL/IP policy engine (default-deny)
;;
;; Security-critical module: validates URLs, classifies IPs,
;; enforces domain allowlists, and classifies action risk.
;; All browser targets must pass through this module before reaching any adapter.

(require racket/string
         racket/match
         "types.rkt"
         "../util/error/errors.rkt")

(provide
 ;; Settings
 browser-policy-settings?
 make-browser-policy-settings
 browser-policy-settings-allowed-schemes
 browser-policy-settings-allowed-domains
 browser-policy-settings-allowed-localhost-ports
 browser-policy-settings-blocked-private-networks?
 browser-policy-settings-blocked-paths
 default-browser-policy-settings

 ;; URL validation
 validate-browser-url

 ;; IP classification
 classify-ip

 ;; Action risk
 classify-browser-action

 ;; Composed policy check (raises on violation)
 check-browser-policy!)

;; ---------------------------------------------------------------------------
;; Policy settings
;; ---------------------------------------------------------------------------

(struct browser-policy-settings
  (allowed-schemes           ; (listof string?) — e.g. '("https" "http")
   allowed-domains           ; (listof string?) — empty = all public domains
   allowed-localhost-ports   ; (listof exact-nonnegative-integer?)
   blocked-private-networks? ; boolean?
   blocked-paths)            ; (listof string?) — e.g. '("/admin" "/debug")
  #:transparent)

(define (make-browser-policy-settings
         #:allowed-schemes [allowed-schemes '("https" "http")]
         #:allowed-domains [allowed-domains '()]
         #:allowed-localhost-ports [allowed-localhost-ports '()]
         #:blocked-private-networks? [blocked-private-networks? #t]
         #:blocked-paths [blocked-paths '()])
  (browser-policy-settings allowed-schemes allowed-domains
                           allowed-localhost-ports blocked-private-networks?
                           blocked-paths))

(define (default-browser-policy-settings)
  (make-browser-policy-settings))

;; ---------------------------------------------------------------------------
;; IP classification
;; ---------------------------------------------------------------------------

(define (classify-ip addr)
  (define s (string-trim addr))
  (define parts (and (regexp-match? #rx"^[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+$" s)
                     (map string->number (string-split s "."))))
  (cond
    [(not parts) 'unknown]
    [else
     (define a (first parts))
     (define b (second parts))
     (cond
       ;; Cloud metadata: 169.254.169.254
       [(and (= a 169) (= b 254) (= (third parts) 169) (= (fourth parts) 254))
        'cloud-metadata]
       ;; Loopback: 127.x.x.x
       [(= a 127) 'loopback]
       ;; Link-local: 169.254.x.x
       [(and (= a 169) (= b 254)) 'link-local]
       ;; Private: 10.x.x.x
       [(= a 10) 'private]
       ;; Private: 172.16-31.x.x
       [(and (= a 172) (<= 16 b 31)) 'private]
       ;; Private: 192.168.x.x
       [(and (= a 192) (= b 168)) 'private]
       [else 'public])]))

;; ---------------------------------------------------------------------------
;; URL validation — returns (values ok? reason-string)
;; ---------------------------------------------------------------------------

(define (validate-browser-url url-str settings)
  (cond
    [(or (not url-str) (string=? url-str ""))
     (values #f "empty URL")]
    [else
     (define parsed (parse-url url-str))
     (cond
       [(not parsed) (values #f "malformed URL")]
       [else
        (match-define (list scheme host port path) parsed)
        (run-url-checks scheme host port path settings)])]))

(define (parse-url url-str)
  ;; scheme://host[:port][/path]
  (define m (regexp-match #rx"^([a-zA-Z][a-zA-Z0-9+.-]*):(?://([^/:]+)(?::([0-9]+))?)?(/.*)?$" url-str))
  (cond
    [(not m) #f]
    [else
     (define scheme (string-downcase (second m)))
     (define host (and (third m) (string-downcase (third m))))
     (define port (and (fourth m) (string->number (fourth m))))
     (define path (or (fifth m) "/"))
     (list scheme host port path)]))

(define (run-url-checks scheme host port path settings)
  (match-define (browser-policy-settings allowed-schemes allowed-domains
                                         allowed-localhost-ports block-private? blocked-paths)
    settings)

  ;; 1. Scheme check
  (cond
    [(not (member scheme allowed-schemes))
     (values #f (format "scheme '~a' not allowed" scheme))]

    ;; 2. No host (scheme-only URL) — already passed scheme check
    [(not host)
     (values #t "ok")]

    ;; 3. Loopback / localhost — check port allowlist
    [(or (eq? (classify-ip host) 'loopback)
         (string=? host "localhost"))
     (if (and port (member port allowed-localhost-ports))
         (values #t "ok")
         (values #f (format "localhost port ~a not allowed" (or port "default"))))]

    ;; 4. IP classification for private networks
    [(and block-private?
          (let ([cls (classify-ip host)])
            (memq cls '(private cloud-metadata link-local))))
     (values #f (format "~a IP blocked: ~a"
                        (classify-ip host) host))]

    ;; 5. Domain allowlist (non-empty list)
    [(and (pair? allowed-domains)
          (not (member host allowed-domains)))
     (values #f (format "domain '~a' not in allowlist" host))]

    ;; 6. Path blocking
    [(for/or ([blocked blocked-paths])
       (string-prefix? path blocked))
     (values #f (format "path '~a' blocked" path))]

    ;; 7. All checks passed
    [else (values #t "ok")]))

;; ---------------------------------------------------------------------------
;; Action risk classification
;; ---------------------------------------------------------------------------

(define (classify-browser-action action)
  (cond
    [(browser-action-navigate? action) 'navigate]
    [(browser-action-click? action) 'write]
    [(browser-action-type? action) 'write]
    [(browser-action-press? action) 'write]
    [(browser-action-scroll? action) 'read]
    [(browser-action-extract? action) 'read]
    [(browser-action-screenshot? action) 'read]
    [(browser-action-wait? action) 'read]
    [else 'unknown]))

;; ---------------------------------------------------------------------------
;; Composed policy check (raises q-browser-error on violation)
;; ---------------------------------------------------------------------------

(define (check-browser-policy! target action settings)
  (define url (browser-target-url target))
  (define-values (ok? reason) (validate-browser-url url settings))
  (unless ok?
    (raise-browser-error
     (format "browser policy blocked: ~a" reason)
     'url-blocked
     (hash 'url url 'reason reason))))
