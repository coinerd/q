#lang racket/base
;; @speed fast
;; @suite extensions
;; @boundary integration
;; @covers extensions/gsd/review-session.rkt
(require rackunit
         racket/file
         racket/list
         racket/runtime-path
         json
         "../runtime/agent-session.rkt"
         "../runtime/provider/provider-factory.rkt"
         "../tools/tool.rkt"
         "../util/loop-result.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../util/message/message.rkt"
         "../util/content/content-parts.rkt"
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt"
         "../extensions/gsd/delivery-journal.rkt")
(define-runtime-path adapter "../extensions/gsd/review-session.rkt")
(define-runtime-path repo "..")
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
(module+ test
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
       (define result
         (runner (hasheq 'wave
                         0
                         'head
                         (make-string 40 #\b)
                         'branch
                         "campaign/w0"
                         'attempt-id
                         "author"
                         'attempt-fence
                         4
                         'prompt
                         "Review")))
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
               (define req
                 (hasheq 'wave
                         0
                         'head
                         (make-string 40 #\b)
                         'branch
                         "campaign/w0"
                         'attempt-id
                         "author"
                         'attempt-fence
                         4
                         'prompt
                         "Review the pinned diff"))
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
               (check-equal? (hash-ref (runner (hasheq 'wave
                                                       0
                                                       'head
                                                       (make-string 40 #\b)
                                                       'branch
                                                       "campaign/w0"
                                                       'attempt-id
                                                       "author"
                                                       'attempt-fence
                                                       4
                                                       'prompt
                                                       "Review"))
                                       'kind)
                             "review-failed")
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
               (check-equal? (hash-ref (runner (hasheq 'wave
                                                       0
                                                       'head
                                                       (make-string 40 #\b)
                                                       'branch
                                                       "campaign/w0"
                                                       'attempt-id
                                                       "author"
                                                       'attempt-fence
                                                       4
                                                       'prompt
                                                       "Review"))
                                       'kind)
                             "review-failed")
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
               (check-equal? (hash-ref (runner (hasheq 'wave
                                                       0
                                                       'head
                                                       (make-string 40 #\b)
                                                       'branch
                                                       "campaign/w0"
                                                       'attempt-id
                                                       "author"
                                                       'attempt-fence
                                                       4
                                                       'prompt
                                                       "Review"))
                                       'kind)
                             "review-failed"))))
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
                 (check-equal? (hash-ref (runner (hasheq 'wave
                                                         0
                                                         'head
                                                         (make-string 40 #\b)
                                                         'branch
                                                         "campaign/w0"
                                                         'attempt-id
                                                         "author"
                                                         'attempt-fence
                                                         4
                                                         'prompt
                                                         "review"))
                                         'kind)
                               "review-failed"))))))
