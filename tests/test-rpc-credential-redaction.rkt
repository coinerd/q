#lang racket

;; @speed fast
;; @suite security

(require rackunit
         racket/string
         "../interfaces/rpc-mode.rkt")

(test-case "RPC responses and notifications recursively redact credential data"
  (define response-json
    (rpc-response->json
     (rpc-response
      "1"
      (hash 'nested (hash "access_token" "rpc-access-raw") 'authorization "rpc-auth-raw" 'safe "kept")
      #f)))
  (check-false (string-contains? response-json "rpc-access-raw"))
  (check-false (string-contains? response-json "rpc-auth-raw"))
  (check-true (string-contains? response-json "kept"))

  (define notification-json
    (rpc-notification->json (rpc-notification 'provider.event
                                              (hash 'refresh-token "rpc-refresh-raw" 'safe "kept"))))
  (check-false (string-contains? notification-json "rpc-refresh-raw"))
  (check-true (string-contains? notification-json "kept")))

(test-case "RPC handler exception text is redacted at serialization"
  (define req (rpc-request "2" 'explode (hash)))
  (define response
    (dispatch-rpc-request req
                          (hash 'explode
                                (lambda (_params) (error 'provider "access_token=rpc-error-raw")))))
  (define encoded (rpc-response->json response))
  (check-false (string-contains? encoded "rpc-error-raw"))
  (check-true (string-contains? encoded "<REDACTED>")))
