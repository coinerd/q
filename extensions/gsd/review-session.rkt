#lang racket/base
;; STABILITY: internal
;; Extension session adapter shared by TUI and SDK /go requests. Never forks, resumes,
;; switches into, or borrows context/tools from the implementation session.
(require json
         racket/file
         racket/list
         racket/path
         racket/string
         racket/set
         "../../runtime/agent-session.rkt"
         "../../runtime/settings.rkt"
         "../../runtime/provider/provider-factory.rkt"
         "../../runtime/provider/model-registry.rkt"
         "../../runtime/auth/auth-store.rkt"
         "../../util/hash-helpers.rkt"
         "../../util/event/event-bus.rkt"
         "../../util/cancellation.rkt"
         "../../util/loop-result.rkt"
         "../../util/message/message.rkt"
         "../../util/content/content-parts.rkt"
         "../../tools/tool.rkt"
         "../../tools/registry-defaults.rkt"
         "../../tools/permission-gate.rkt"
         "../../sandbox/subprocess.rkt"
         "../../sandbox/gateway-bridge.rkt"
         "campaign-state.rkt"
         "campaign-repository.rkt"
         "campaign-budgets.rkt"
         "delivery-journal.rkt"
         "review-usage.rkt"
         "review-proof.rkt"
         "review-audit-format.rkt"
         (only-in "review-worktree.rkt" resolve-review-checkout))
(provide make-auto-review-runner
         resolve-review-checkout
         review-final-json)
(define review-model "zai-coding-plan/glm-5.3")
;; A precise, factual final-output contract failure. Distinct from
;; transport/checkout/accounting failures so the runner's generic catch can
;; never hide WHY the reviewer's final text was rejected.
(struct exn:fail:review-final exn:fail ())
(define (final-output-refuse message)
  (raise (exn:fail:review-final message (current-continuation-marks))))
;; Canonical final-output grammar: the reviewer's whole final assistant text
;; is exactly one JSON object, either bare or as the sole content of exactly
;; one Markdown fenced code block (optional `json` language tag). No prose,
;; no multiple fences, no non-json tag, no trailing content.
(define whole-fence-pattern
  #px"(?s:^```[ \t]*(?i:json)?[ \t]*\r?\n(.*?)\r?\n?```[ \t]*$)")
(define (normalize-final-text text)
  (when (> (string-length text) (* 1024 1024))
    (final-output-refuse "final reviewer text exceeds the 1 MiB character limit"))
  (define trimmed (string-trim text))
  (cond
    [(string-prefix? trimmed "```")
     (define m (regexp-match whole-fence-pattern trimmed))
     (unless m
       (final-output-refuse
        "final reviewer text is not exactly one whole-text fenced block (optional json tag; no prose, multiple fences or trailing content)"))
     (second m)]
    [else trimmed]))
;; string->jsexpr ignores trailing input; the WHOLE normalized text must be
;; exactly one JSON value. Trailing garbage or a second value fail closed.
(define (parse-whole-final-json text)
  (define in (open-input-string text))
  (define value
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (define detail (exn-message e))
                       (final-output-refuse
                        (format "final reviewer text is not valid JSON: ~a"
                                (substring detail 0 (min 160 (string-length detail))))))])
      (read-json in)))
  (when (eof-object? value)
    (final-output-refuse "final reviewer text is empty; exactly one JSON object is required"))
  (let skip ()
    (define c (peek-char in))
    (when (and (char? c) (char-whitespace? c))
      (read-char in)
      (skip)))
  (unless (eof-object? (peek-char in))
    (final-output-refuse
     "final reviewer text has trailing content after the JSON value; exactly one JSON object is required"))
  value)
(define (review-final-json result session-identity)
  (unless (and (loop-result? result)
               (memq (loop-result-termination-reason result) '(completed stop end-turn)))
    (final-output-refuse "reviewer did not terminate normally"))
  (define final
    (for/first ([m (in-list (reverse (loop-result-messages result)))]
                #:when (and (message? m) (eq? (message-role m) 'assistant)))
      m))
  (unless final
    (final-output-refuse "reviewer returned no final assistant message"))
  (define text
    (string-join (for/list ([part (in-list (message-content final))]
                            #:when (text-part? part))
                   (text-part-text part))
                 ""))
  ;; No extraction from tool output, other messages, or prose around the
  ;; verdict; only a sole whole-text fence is normalized away.
  (define data (parse-whole-final-json (normalize-final-text text)))
  (unless (hash? data)
    (final-output-refuse "final reviewer JSON is not an object"))
  (hash-set data 'reviewer (string-append "q-session:" session-identity)))
;; The harness label is not necessarily a q registry name. Do not use the
;; registry's first-match/default resolution: duplicates must fail closed.
(define (resolve-review-model config)
  (define candidates
    (filter (lambda (entry) (member (model-entry-name entry) (list "glm-5.3" review-model)))
            (available-models (make-model-registry-from-config config))))
  (unless (= (length candidates) 1)
    (error 'review-session "exact GLM-5.3 registration missing or ambiguous; no fallback review"))
  (define entry (car candidates))
  (define name (model-entry-provider-name entry))
  (define cfg (model-entry-provider-config entry))
  (define url (flex-ref cfg 'base-url #f))
  ;; Only OpenAI-compatible Coding Plan transports, never another vendor's
  ;; adapter or the general ZAI endpoint. Reject credentials/query in URLs too.
  (unless (and (member name '("glm-5.3" "zai-coding-plan" "openai-compatible"))
               (string? url)
               (regexp-match? #px"^https://api\\.z\\.ai(?::443)?/api/coding/paas/v4/?$" url))
    (error 'review-session "GLM-5.3 provider/official HTTPS Coding Plan endpoint mismatch"))
  ;; A literally registered harness alias is allowed, but never sent on wire.
  (model-resolution "glm-5.3" name url cfg))
(define (build-review-provider cwd)
  (define resolution (resolve-review-model (q-settings-merged (load-settings cwd))))
  (define name (model-resolution-provider-name resolution))
  (define cfg (model-resolution-provider-config resolution))
  (define cred (lookup-credential name cfg #:project-dir cwd))
  (unless cred
    (error 'review-session "requested GLM-5.3 credentials unavailable; no fallback review"))
  (define p
    (create-provider-for-name name
                              (model-resolution-base-url resolution)
                              (credential-api-key cred)
                              (model-resolution-model-name resolution)
                              (flex-ref cfg 'max-tokens #f)))
  (when (provider-is-mock? p)
    (error 'review-session "requested GLM-5.3 provider unavailable; no fallback review"))
  p)
;; Private test/probe interface, not part of the extension's public runner API.
(module+ test-support
  (provide resolve-review-model
           build-review-provider))
(define (run-review-session session prompt)
  (define-values (_updated result) (run-prompt! session prompt #:max-iterations 80))
  result)
(define (failed reason)
  (hasheq 'kind "review-failed" 'reason reason))
;; The linked behavioral RED identity, when the trusted owned authority links
;; one. A model can never introduce its own: the sha comes from the pinned
;; owner-written authority inside the controller request.
(define (request-red-sha request)
  (define authority (hash-ref request 'owned-authority #f))
  (define red (and (hash? authority) (hash-ref authority 'red #f)))
  (define reference (and (hash? red) (hash-ref red 'reference #f)))
  (and (hash? reference) (hash-ref reference 'sha256 #f)))
(define (make-auto-review-runner root
                                 plan
                                 #:provider-builder [provider-builder build-review-provider]
                                 #:checkout-resolver [checkout-resolver resolve-review-checkout]
                                 #:session-run [session-run run-review-session]
                                 #:timeout-seconds [timeout 600])
  (unless (and (real? timeout) (< 0 timeout) (<= timeout 600))
    (raise-argument-error 'make-auto-review-runner "positive timeout <=600 seconds" timeout))
  (lambda (request)
    (with-handlers
        ([exn:fail:review-final?
          (lambda (e) (failed (format "reviewer final output rejected: ~a" (exn-message e))))]
         [exn:fail?
          (lambda (_)
            (failed
             "review session failed (transport, checkout, accounting or malformed final JSON)"))])
      (define wave (hash-ref request 'wave))
      (define journal (load-delivery-journal root plan wave))
      (define receipt (and journal (hash-ref journal 'receipt #f)))
      (define mode (hash-ref request 'review-mode #f))
      (define audit? (equal? mode owned-audit-mode))
      (unless (or audit? (equal? mode "manual-compatibility"))
        (error 'review-session "production review requires owned-proof audit mode"))
      (when audit?
        (validate-receipt-owned-proof! receipt root plan wave)
        (unless (and (hash? (hash-ref receipt 'owned-proof #f))
                     (equal? (hash-ref request 'proof-sha256 #f)
                             (hash-ref (hash-ref receipt 'owned-proof) 'sha256))
                     (equal? (hash-ref request 'tree #f) (hash-ref receipt 'tree #f))
                     (equal? (hash-ref request 'generation #f)
                             (length (hash-ref journal 'receipt-history '()))))
          (error 'review-session "stale owned-proof audit pin")))
      (define (live?)
        (define rec (load-campaign-record root plan))
        (define current (load-delivery-journal root plan wave))
        (and rec
             (not (campaign-record-cancellation rec))
             (not (campaign-record-budget-pause rec))
             (= (campaign-fence-token rec) (hash-ref receipt 'attempt-fence))
             (equal? receipt (and current (hash-ref current 'receipt #f)))
             (or (not audit?)
                 (equal? (hash-ref request 'generation #f)
                         (length (hash-ref current 'receipt-history '()))))
             (for/or ([w (in-list (campaign-record-waves rec))])
               (define a (campaign-wave-current-attempt w))
               (and (= wave (campaign-wave-index w))
                    a
                    (equal? (campaign-attempt-id a) (hash-ref receipt 'attempt-id))
                    (= (campaign-attempt-fence-token a) (hash-ref receipt 'attempt-fence))))))
      (unless (and (hash? receipt)
                   (live?)
                   (for/and ([key '(head branch attempt-id attempt-fence)])
                     (equal? (hash-ref request key #f) (hash-ref receipt key #f)))
                   (not (pause-campaign-if-over-budget! root plan)))
        (error 'review-session "stale, cancelled or budget-paused reviewer dispatch"))
      (define cwd (checkout-resolver receipt))
      (define token (make-cancellation-token))
      (define cust (make-custodian))
      (define answer (make-channel))
      (define session #f)
      (define env (sanitize-env))
      (define session-root (build-path root ".planning" "campaigns" plan "review-sessions"))
      (make-directory* session-root)
      (define private-home (build-path session-root "tool-home"))
      (make-directory* private-home)
      ;; Do not inherit credential agents, Git helpers, shell startup files,
      ;; or the initiating user's CLI credential configuration.
      (for ([name '(#"SSH_AUTH_SOCK" #"GPG_AUTH_INFO"
                                     #"GIT_ASKPASS"
                                     #"SSH_ASKPASS"
                                     #"BASH_ENV"
                                     #"ENV"
                                     #"GIT_CONFIG_COUNT"
                                     #"GIT_CONFIG_SYSTEM"
                                     #"GIT_CONFIG_GLOBAL"
                                     #"GH_CONFIG_DIR"
                                     #"XDG_CONFIG_HOME"
                                     #"XDG_DATA_HOME")])
        (environment-variables-set! env name #f))
      (for ([name '(#"HOME" #"GH_CONFIG_DIR" #"XDG_CONFIG_HOME" #"XDG_DATA_HOME")])
        (environment-variables-set! env name (path->bytes private-home)))
      (environment-variables-set! env #"GIT_TERMINAL_PROMPT" #"0")
      (define prompt
        (string-append
         (hash-ref request 'prompt)
         "\nYou are a FRESH independent non-author reviewer. Use only read/bash to inspect source and actual outputs."
         " Do not edit source, evidence, refs, or publish. Credentials are unavailable."
         " AUDIT owned gates; do not rerun full fast/focused/format/compile/lint or RED."
         " Never claim you executed inherited gates. Missing proof remains review-failed, not approval."
         " You have at most "
         (number->string timeout)
         " seconds including tool inspection; avoid long replay jobs."
         " Return ONLY one JSON object as your final assistant text, in the requested result schema:"
         " bare JSON, or exactly one ```json fenced block containing only the object;"
         " no prose, multiple fences, trailing text, or additional values."
         " The runtime supplies reviewer identity; do not invent one.\n"
         (if audit?
             (string-append
              "Result schema: {\"kind\":\"approved\"|\"request-changes\"|\"review-failed\","
              "\"scope\":\"at least 64 characters\",\"report\":\"at least 64 characters\","
              "\"findings\":[\"nonempty finding\"],\"reason\":\"factual failure detail\","
              "\"audit-ack\":{\"mode\":\"owned-proof-audit-v1\","
              "\"verification-origin\":\"audited-owned\",\"source-read\":true,\"raw-output-read\":true,"
              (if (request-red-sha request)
                  (string-append "\"red-ack\":{\"proof-sha256\":\""
                                 (request-red-sha request)
                                 "\",\"raw-output-read\":true},")
                  "")
              "\"binding\":"
              (jsexpr->string (audit-binding request))
              "}}."
              " Echo binding exactly only AFTER reading source, manifest and actual raw outputs."
              " When a red-ack is requested, read the linked RED manifest and its actual raw"
              " failure output first; never acknowledge a red you have not read."
              " No validation/gate execution facts in model output."
              " An acknowledgement is an attestation, not runtime proof of which files were read."
              " For review-failed, reason states the concrete failure actually encountered;"
              " copied schema text such as \"infrastructure only\" is rejected as malformed output.\n")
             (string-append
              "Explicit manual compatibility result schema: {\"kind\":\"approved\"|\"request-changes\"|\"review-failed\","
              "\"scope\":\"at least 64 characters\",\"report\":\"at least 64 characters\","
              "\"findings\":[\"nonempty finding\"],\"reason\":\"factual failure detail\","
              "\"validation\":{\"red-first\":{\"command\":\"actual command\",\"failure\":\"actual failure >=32 chars\"},"
              "\"focused-tests\":{\"result\":\"passed\",\"command\":\"actual command\"},"
              "\"format-compile\":{\"result\":\"passed\",\"command\":\"actual command\"},"
              "\"lint\":{\"result\":\"passed\",\"command\":\"actual command\"},"
              "\"fast\":{\"result\":\"passed\",\"command\":\"actual command\"},"
              "\"remaining-items\":[],\"planning-sync\":\"current\"}}."
              " Findings required only for rejection, validation only for approval, reason only for failure."
              " A failure reason states the actual failure encountered; copied schema text is rejected.\n"))))
      (dynamic-wind
       void
       (lambda ()
         (parameterize ([current-custodian cust])
           (thread
            (lambda ()
              (channel-put
               answer
               (cons 'completed
                     (with-handlers ([exn:fail? (lambda (_) #f)])
                       ;; Bound trusted provider initialization too. Its credential
                       ;; remains in the closure, never the review tool environment.
                       (define provider (provider-builder cwd))
                       (parameterize ([current-directory cwd]
                                      [current-environment-variables env]
                                      [current-execution-plane-enabled #f])
                         (define tools (make-tool-registry))
                         (register-default-tools! tools #:only '("read" "bash"))
                         (set! session
                               (make-agent-session
                                (hasheq 'provider
                                        provider
                                        'model-name
                                        "glm-5.3"
                                        'tool-registry
                                        tools
                                        'event-bus
                                        (make-event-bus)
                                        'session-dir
                                        session-root
                                        'project-dir
                                        cwd
                                        'extension-registry
                                        #f
                                        'memory-backend
                                        #f
                                        'cancellation-token
                                        token
                                        'system-instructions
                                        '("Independent review only; return factual final JSON.")
                                        'permission-config
                                        (make-default-permission-config #:auto-approved
                                                                        (set "read" "bash")))))
                         (session-run session prompt))))))))
         (define deadline (+ (current-inexact-milliseconds) (* 1000 timeout)))
         (define result
           (let wait ()
             (cond
               [(or (not (live?)) (>= (current-inexact-milliseconds) deadline)) #f]
               [else
                (define got (sync/timeout 0.1 answer))
                (if got
                    (cdr got)
                    (wait))])))
         (cancel-token! token)
         (custodian-shutdown-all cust)
         ;; Every launched review, including incomplete usage, is accounted
         ;; before accepting a verdict or allowing the next implementation.
         (define budget-stop (account-review-usage! root plan wave result))
         (cond
           [(or budget-stop (not (live?)))
            (failed "review cancelled, stale, or campaign budget exhausted")]
           [(not result)
            (failed "review session timeout or infrastructure failure; no rejection retry")]
           [else
            ;; Reauthenticate the existing checkout after read/bash execution:
            ;; tracked writes or implementation drift cannot earn approval.
            (unless (equal? cwd (checkout-resolver receipt))
              (error 'review-session "review checkout changed during session"))
            (when audit?
              (validate-receipt-owned-proof! receipt root plan wave))
            (define final (review-final-json result (session-id session)))
            (when (and audit? (member (hash-ref final 'kind #f) '("approved" "request-changes")))
              (unless (and (not (hash-has-key? final 'validation))
                           (valid-audit-ack? (hash-ref final 'audit-ack #f) (audit-binding request)))
                (final-output-refuse "invalid owned-proof audit acknowledgement"))
              (define ack (hash-ref final 'audit-ack))
              (cond
                [(request-red-sha request)
                 (unless (valid-audit-red-ack? (hash-ref ack 'red-ack #f) (request-red-sha request))
                   (final-output-refuse "invalid owned behavioral RED acknowledgement"))]
                [(hash-has-key? ack 'red-ack)
                 (final-output-refuse "RED acknowledged without a linked owned red proof")]))
            final]))
       (lambda ()
         (cancel-token! token)
         (custodian-shutdown-all cust))))))
