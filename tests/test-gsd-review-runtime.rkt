#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @isolation subprocess
;; @covers extensions/gsd/review-session.rkt
(require rackunit
         racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/system
         json
         "../runtime/agent-session.rkt"
         "../runtime/provider/provider-factory.rkt"
         "../tools/tool.rkt"
         "../util/loop-result.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../llm/http-helpers.rkt"
         "../runtime/provider/model-registry.rkt"
         "../util/message/message.rkt"
         "../util/content/content-parts.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/delivery-journal.rkt")
(define-runtime-path adapter "../extensions/gsd/review-session.rkt")
(define-runtime-path repo "..")
(define-runtime-path this-test "test-gsd-review-runtime.rkt")
(define manifest (make-campaign-manifest 1 "w0" '() '() "constraints"))
(define plan (campaign-manifest-hash manifest))
(define (fixture f)
  (define root (make-temporary-file "review-runtime-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (persist-campaign!
      root
      (make-campaign-record
       plan
       manifest
       (list (make-campaign-wave 0 "w0" 'awaiting-delivery 1 (campaign-attempt "author" 4 0)))
       #f
       4
       'test
       0
       0))
     (record-delivery-receipt! root
                               plan
                               0
                               (hasheq 'repo
                                       (path->string repo)
                                       'branch
                                       "campaign/w0"
                                       'head
                                       (make-string 40 #\b)
                                       'tree
                                       (make-string 40 #\c)
                                       'origin
                                       "https://github.com/example/q.git"
                                       'verified-at
                                       1
                                       'evidence
                                       "Verify"
                                       'attempt-id
                                       "author"
                                       'attempt-fence
                                       4))
     (f root))
   (lambda () (delete-directory/files root))))
(define result-json
  (jsexpr->string (hasheq 'kind
                          "request-changes"
                          'reviewer
                          "forged-model-identity"
                          'scope
                          (make-string 70 #\s)
                          'report
                          (make-string 70 #\r)
                          'findings
                          '("real finding"))))
(define (response text)
  (make-loop-result (list (make-message "m" #f 'assistant 'message (list (make-text-part text)) 0 #f))
                    'stop
                    (hasheq 'usage (hasheq 'prompt_tokens 5 'completion_tokens 6 'total_tokens 11))))
;; Real settings/auth files, isolated from the user's config and environment.
(define coding-url "https://api.z.ai/api/coding/paas/v4")
(define (configured-fixture providers credentials f)
  (fixture
   (lambda (root)
     (make-directory* (build-path root ".q"))
     (for ([name '("config.json" "credentials.json")]
           [data (list (hasheq 'providers providers) (hasheq 'providers credentials))])
       (call-with-output-file (build-path root ".q" name)
         (lambda (out) (write-json data out))))
     (define env (make-environment-variables #"HOME" (path->bytes root)))
     (parameterize ([current-environment-variables env]
                    [current-directory root]
                    [current-security-guard
                     (make-security-guard (current-security-guard) void
                                          (lambda _ (error 'test "network forbidden")))])
       (f root)))))
(define (provider-config [url coding-url] [models '("glm-5.3")])
  (hasheq 'base-url url 'models models))
(define invalid-registrations
  (list (hasheq 'glm-5.3 (provider-config "https://example.com/api/coding/paas/v4"))
        (hasheq 'glm-5.3 (provider-config "http://api.z.ai/api/coding/paas/v4"))
        (hasheq 'glm-5.3 (provider-config "https://api.z.ai/api/paas/v4"))
        (hasheq 'glm-5.3 (provider-config "https://api.z.ai.evil/api/coding/paas/v4"))
        (hasheq 'glm-5.3 (provider-config "https://api.z.ai:444/api/coding/paas/v4"))
        (hasheq 'glm-5.3 (provider-config "https://user@api.z.ai/api/coding/paas/v4"))
        (hasheq 'glm-5.3 (provider-config (string-append coding-url "?redirect=evil")))
        (hasheq 'glm-5.3 (provider-config (string-append coding-url "#fragment")))
        (hasheq 'glm-5.3 (provider-config coding-url '("glm-5.3-flash")))
        (hasheq 'glm-5.3 (provider-config coding-url '("glm-5.2")))
        (hasheq 'anthropic (provider-config))
        (hasheq 'mock (provider-config))
        (hasheq 'glm-5.3 (provider-config)
                'anthropic (provider-config "https://other.example/v1"))
        (hasheq 'glm-5.3 (provider-config) 'zai-coding-plan (provider-config))
        (hasheq 'glm-5.3 (provider-config coding-url '("glm-5.3" "glm-5.3")))
        (hasheq 'glm-5.3 (provider-config coding-url '("glm-5.3" "zai-coding-plan/glm-5.3")))
        (hasheq)))
(define (review-request)
  (hasheq 'review-mode "manual-compatibility"
          'wave 0 'head (make-string 40 #\b) 'branch "campaign/w0"
          'attempt-id "author" 'attempt-fence 4 'prompt "Review"))
(module+ test
  ;; Racket fixes home-dir at process startup (HOME alone cannot rebind it).
  ;; Reexec once with an empty PLTUSERHOME so neither settings nor auth can
  ;; consult the operator's actual q configuration, including missing-key cases.
  (define hermetic? (equal? (getenv "Q_REVIEW_RUNTIME_HERMETIC") "1"))
  (unless hermetic?
    (define home (make-temporary-file "review-config-home-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (define env (environment-variables-copy (current-environment-variables)))
       (environment-variables-set! env #"PLTUSERHOME" (path->bytes home))
       (environment-variables-set! env #"Q_REVIEW_RUNTIME_HERMETIC" #"1")
       (parameterize ([current-environment-variables env])
         (check-true
          (system* (find-executable-path "raco") "test" (path->string this-test)))))
     (lambda () (delete-directory/files home))))
  (when hermetic?
  (test-case "production rejects missing owned proof before model launch, deadline ceiling is 600"
    (fixture
     (lambda (root)
       (define make-runner (dynamic-require adapter 'make-auto-review-runner))
       (check-exn exn:fail? (lambda () (make-runner root plan #:timeout-seconds 601)))
       (define launched? #f)
       (define runner
         (make-runner root plan #:provider-builder (lambda (_) (set! launched? #t) (build-mock-provider))
                      #:checkout-resolver (lambda (_) repo)))
       (define request (hash-set (review-request) 'review-mode "owned-proof-audit-v1"))
       (check-equal? (hash-ref (runner request) 'kind) "review-failed")
       (check-false launched?)
       (check-equal? (hash-ref (load-delivery-journal root plan 0) 'review-rejection-count 0) 0))))
  (test-case "default builder resolves actual q glm-5.3 registry alias without network"
    (configured-fixture
     (hasheq 'glm-5.3 (hasheq 'base-url coding-url 'models '("glm-5.3")))
     (hasheq 'glm-5.3 (hasheq 'api-key "hermetic-review-credential"))
     (lambda (root)
       (define launched? #f)
       (define runner
         ((dynamic-require adapter 'make-auto-review-runner)
          root plan #:checkout-resolver (lambda (_) root)
          #:session-run (lambda (_session _prompt)
                          (set! launched? #t)
                          (response result-json))))
       (check-equal? (hash-ref (runner (review-request)) 'kind) "request-changes")
       (check-true launched?))))
  (test-case "pure exact selector rejects mismatches and ambiguity, regardless of defaults"
    (define resolve (dynamic-require `(submod ,adapter test-support) 'resolve-review-model))
    (for ([providers (in-list invalid-registrations)])
      (check-exn exn:fail?
                 (lambda () (resolve (hasheq 'providers providers
                                            'default-provider "glm-5.3" 'default-model "glm-5.3")))))
    (for ([providers
           (in-list (list (hasheq 'glm-5.3 (provider-config))
                          (hasheq 'zai-coding-plan (provider-config))
                          (hasheq 'openai-compatible (provider-config))
                          (hasheq 'glm-5.3 (hasheq 'base-url coding-url 'default-model "glm-5.3"))
                          (hasheq 'glm-5.3
                                  (provider-config coding-url (list (hasheq 'id "glm-5.3"))))
                          (hasheq 'glm-5.3
                                  (provider-config coding-url '("zai-coding-plan/glm-5.3")))))])
      (check-equal? (model-resolution-model-name (resolve (hasheq 'providers providers))) "glm-5.3")))
  (test-case "production default builder fails closed before session launch"
    (define credentials
      (for/hasheq ([name '(glm-5.3 zai-coding-plan openai-compatible anthropic mock)])
        (values name (hasheq 'api-key "hermetic-review-credential"))))
    (define scenarios
      (append (for/list ([providers (in-list invalid-registrations)]) (cons providers credentials))
              (list (cons (hasheq 'glm-5.3 (provider-config)) (hasheq))
                    (cons (hasheq 'glm-5.3 (provider-config))
                          (hasheq 'glm-5.3 (hasheq 'api-key "   "))))))
    (for ([scenario (in-list scenarios)])
      (configured-fixture
       (car scenario) (cdr scenario)
       (lambda (root)
         (define launched? #f)
         (define runner
           ((dynamic-require adapter 'make-auto-review-runner)
            root plan #:checkout-resolver (lambda (_) root)
            #:session-run (lambda (_session _prompt) (set! launched? #t) (response result-json))))
         (check-equal? (hash-ref (runner (review-request)) 'kind) "review-failed")
         (check-false launched?)))))
  (test-case "real default provider sends exact API model, including explicitly registered harness alias"
    (define build (dynamic-require `(submod ,adapter test-support) 'build-review-provider))
    (for ([registration '("glm-5.3" "zai-coding-plan/glm-5.3" "glm-5.3" "glm-5.3")]
          [name '(glm-5.3 glm-5.3 zai-coding-plan openai-compatible)])
      (configured-fixture
       (hasheq name (provider-config coding-url (list registration))
               'glm-5.3-flash (provider-config coding-url '("glm-5.3-flash")))
       (hasheq name (hasheq 'api-key "hermetic-review-credential"))
       (lambda (root)
         (define p (build root))
         (check-false (provider-is-mock? p))
         (define sent-model #f)
         (parameterize
             ([current-provider-http-sendrecv
               (lambda (host path #:port port #:ssl? ssl? #:method _method #:headers _headers #:data data)
                 ;; Never print headers/credential data, even on assertion failure.
                 (check-equal? host "api.z.ai")
                 (check-equal? (string-append "https://" host path)
                               "https://api.z.ai/api/coding/paas/v4/chat/completions")
                 (check-equal? port 443)
                 (check-true ssl?)
                 (set! sent-model (hash-ref (bytes->jsexpr data) 'model))
                 (values #"HTTP/1.1 200 OK" '()
                         (open-input-string
                          "{\"model\":\"glm-5.3\",\"choices\":[{\"message\":{\"content\":\"ok\"},\"finish_reason\":\"stop\"}]}")))])
           (provider-send p (make-model-request '() #f (hasheq))))
         (check-equal? sent-model "glm-5.3")))))
  (test-case "production session-run reads actual agent final JSON (completed termination)"
    (fixture
     (lambda (root)
       (define runner
         ((dynamic-require adapter 'make-auto-review-runner)
          root
          plan
          #:checkout-resolver (lambda (_) repo)
          #:provider-builder
          (lambda (_)
            (make-mock-provider (make-model-response
                                 (list (hasheq 'type "text" 'text result-json))
                                 (hasheq 'prompt_tokens 3 'completion_tokens 4 'total_tokens 7)
                                 "glm-5.3"
                                 'stop)))))
       (define result (runner (review-request)))
       (check-equal? (hash-ref result 'kind) "request-changes")
       (check-regexp-match #rx"^q-session:" (hash-ref result 'reviewer)))))
  (test-case "real fresh sessions, read/bash only, final JSON and runtime-owned identity"
    (fixture (lambda (root)
               (define make-runner (dynamic-require adapter 'make-auto-review-runner))
               (define ids '())
               (define runner
                 (make-runner root
                              plan
                              #:provider-builder (lambda (_) (build-mock-provider))
                              #:checkout-resolver (lambda (_) repo)
                              #:session-run
                              (lambda (session prompt)
                                (set! ids (cons (session-id session) ids))
                                (check-equal? (sort (tool-names (session-tool-registry session))
                                                    string<?)
                                              '("bash" "read"))
                                (check-false (getenv "GH_TOKEN"))
                                (check-regexp-match #rx"Return ONLY.*JSON" prompt)
                                (response result-json))))
               (define req (review-request))
               (parameterize ([current-environment-variables
                               (environment-variables-copy (current-environment-variables))])
                 (putenv "GH_TOKEN" "must-not-reach-reviewer")
                 (define first (runner req))
                 (define second (runner req))
                 (check-equal? (hash-ref first 'kind) "request-changes")
                 (check-not-equal? (hash-ref first 'reviewer) "forged-model-identity")
                 (check-not-equal? (hash-ref first 'reviewer) (hash-ref second 'reviewer))
                 (check-equal? (length (remove-duplicates ids)) 2))
               (define w (car (campaign-record-waves (load-campaign-record root plan))))
               (check-equal? (usage-summary-total-tokens (wave-usage-summary w)) 22))))
  (test-case "durable cancellation while reviewer runs refuses its otherwise valid rejection"
    (fixture (lambda (root)
               (define runner
                 ((dynamic-require adapter 'make-auto-review-runner)
                  root
                  plan
                  #:provider-builder (lambda (_) (build-mock-provider))
                  #:checkout-resolver (lambda (_) repo)
                  #:session-run
                  (lambda (_session _prompt)
                    (define rec (load-campaign-record root plan))
                    (set-campaign-cancellation! rec (make-campaign-cancellation "stop review" 0))
                    (persist-campaign! root rec)
                    (response result-json))))
               (check-equal? (hash-ref (runner (review-request)) 'kind) "review-failed")
               (check-equal? (hash-ref (load-delivery-journal root plan 0) 'review-rejection-count 0)
                             0))))
  (test-case "post-session checkout drift refuses JSON but still accounts reviewer usage"
    (fixture (lambda (root)
               (define resolutions 0)
               (define runner
                 ((dynamic-require adapter 'make-auto-review-runner)
                  root
                  plan
                  #:provider-builder (lambda (_) (build-mock-provider))
                  #:checkout-resolver (lambda (_)
                                        (set! resolutions (add1 resolutions))
                                        (when (= resolutions 2)
                                          (error 'checkout "tracked source drift"))
                                        repo)
                  #:session-run (lambda (_session _prompt) (response result-json))))
               (check-equal? (hash-ref (runner (review-request)) 'kind) "review-failed")
               (check-equal? resolutions 2)
               (check-equal?
                (usage-summary-input-tokens
                 (wave-usage-summary (car (campaign-record-waves (load-campaign-record root plan)))))
                5))))
  (test-case "provider initialization is bounded before any reviewer tools or session"
    (fixture (lambda (root)
               (define runner
                 ((dynamic-require adapter 'make-auto-review-runner)
                  root
                  plan
                  #:checkout-resolver (lambda (_) repo)
                  #:timeout-seconds 0.02
                  #:provider-builder (lambda (_)
                                       (sleep 10)
                                       (error 'test "must be cancelled"))
                  #:session-run (lambda (_session _prompt) (error 'test "must not reach reviewer"))))
               (check-equal? (hash-ref (runner (review-request)) 'kind) "review-failed"))))
  (test-case "malformed genuine final and bounded timeout are infrastructure, not rejection"
    (fixture (lambda (root)
               (define make-runner (dynamic-require adapter 'make-auto-review-runner))
               (for ([mode '(malformed timeout)])
                 (define runner
                   (make-runner root
                                plan
                                #:timeout-seconds 0.1
                                #:provider-builder (lambda (_) (build-mock-provider))
                                #:checkout-resolver (lambda (_) repo)
                                #:session-run (lambda (_session _prompt)
                                                (if (eq? mode 'timeout)
                                                    (sleep 5)
                                                    (response "not JSON")))))
                 (check-equal? (hash-ref (runner (review-request)) 'kind) "review-failed")))))))
