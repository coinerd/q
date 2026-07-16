#lang racket

;; @speed fast
;; @suite security

(require rackunit
         racket/string
         "../llm/openai-compatible.rkt"
         "../llm/openrouter.rkt"
         "../runtime/settings-core.rkt")

(define (all-writes value)
  (list (format "~a" value)
        (format "~s" value)
        (format "~v" value)
        (with-output-to-string (lambda () (display value)))
        (with-output-to-string (lambda () (write value)))
        (with-output-to-string (lambda () (print value)))))

(test-case "provider config structs never print API keys"
  (define openai (openai-config "openai-key-raw" "https://api.example" "m" #f #f))
  (define openrouter (openrouter-config "openrouter-key-raw" "https://router.example" "m" #f #f))
  (for ([rendered (in-list (append (all-writes openai) (all-writes openrouter)))])
    (check-false (string-contains? rendered "openai-key-raw"))
    (check-false (string-contains? rendered "openrouter-key-raw")))
  (check-equal? (openai-config-api-key openai) "openai-key-raw")
  (check-equal? (openrouter-config-api-key openrouter) "openrouter-key-raw"))

(test-case "q-settings never print provider or broker credentials"
  (define raw
    (hasheq 'providers
            (hasheq 'openai (hasheq 'api-key "settings-api-key-raw"))
            'mas
            (hasheq 'broker (hasheq 'capability-secret "settings-capability-raw"))))
  (define settings (q-settings raw raw raw))
  (for ([rendered (in-list (all-writes settings))])
    (check-false (string-contains? rendered "settings-api-key-raw"))
    (check-false (string-contains? rendered "settings-capability-raw")))
  (check-equal? (hash-ref (hash-ref (q-settings-merged settings) 'providers) 'openai)
                (hasheq 'api-key "settings-api-key-raw")))
