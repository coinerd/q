#lang racket/base

;;; tools/permission-gate.rkt — Permission gates for tool execution.
;;;
;;; Provides an interactive approval flow so dangerous tools (edit, write, bash,
;;; etc.) must be explicitly approved before execution.  Safe / read-only tools
;;; bypass the check entirely.
;;;
;;; Policy modes (v0.54.4):
;;;   'strict     — unknown tools require approval (deny-by-default). Default.
;;;   'permissive — unknown tools are auto-approved (allow-by-default).
;;;
;;; Exports:
;;;   permission-config, permission-config?     — config struct
;;;   make-default-permission-config             — headless-friendly defaults
;;;   make-strict-permission-config              — hard-deny (W1 v0.99.66)
;;;   make-permissive-permission-config          — --auto-approve opt-in (W2 v0.99.66)
;;;   tool-needs-approval?                       — predicate
;;;   request-approval                           — invoke the callback
;;;   permission-config-auto-approved-tools      — accessor
;;;   permission-config-needs-approval-tools     — accessor
;;;   permission-config-approval-callback        — accessor
;;;   permission-config-policy-mode              — accessor
;;;
;;; Source of truth: the auto-approved / needs-approval tool sets are
;;; defined once in tool-classification.rkt (the single classification
;;; source introduced in v0.99.66).  This module consumes them to
;;; populate default configs; it no longer maintains a parallel list.

(require racket/set
         racket/contract
         racket/list
         racket/string
         "tool-classification.rkt"
         (only-in "../runtime/approval/broker.rkt"
                  current-approval-channel
                  register-approval-request-for-channel!
                  approval-await-grant
                  cancel-approval-request!)
         (only-in "../util/credential-redaction.rkt" redact-credential-data)
         (only-in "../util/json/checksum.rkt" sha256-string))

;; ============================================================
;; Struct
;; ============================================================

(struct permission-config
        (auto-approved-tools ; (set/c string?)
         needs-approval-tools ; (set/c string?)
         approval-callback ; returns boolean? or opaque approval-grant?
         policy-mode) ; (or/c 'strict 'permissive)
  #:transparent)

;; ============================================================
;; Default config
;; ============================================================

(define (approval-callback? value)
  (and (procedure? value)
       (or (procedure-arity-includes? value 2) (procedure-arity-includes? value 3))))

;; make-default-permission-config — general-purpose config builder.
;;
;; v0.99.66 (W2, finding #2 HIGH): the default approval callback now
;; DENIES by default.  A dangerous tool that reaches the approval
;; branch is blocked unless the caller explicitly supplies an
;; approving callback (e.g. interactive TUI broker or --auto-approve).
(define (make-default-permission-config #:auto-approved [auto-approved #f]
                                        #:needs-approval [needs-approval #f]
                                        #:callback [callback #f]
                                        #:policy-mode [mode 'strict])
  (permission-config (or auto-approved auto-approved-tool-names)
                     (or needs-approval needs-approval-tool-names)
                     (or callback (lambda (tool-name args) #f)) ; W2: deny-by-default
                     (if (memq mode '(strict permissive)) mode 'strict)))

;; make-strict-permission-config — the hard-deny config.
;;
;; v0.99.66 (W1, finding #1 CRITICAL): this is the default assigned to
;; every exec-context that does not explicitly configure permissions.
;; It never auto-approves a dangerous tool and never returns #f/'skip.
;; Safe (read-only) tools bypass the gate entirely via classification.
(define (make-strict-permission-config)
  (make-default-permission-config #:callback (lambda (tool-name args) #f)))

;; make-permissive-permission-config — explicit opt-in for dangerous tools.
;;
;; v0.99.66 (W2): used by --auto-approve and trusted-CLI paths.  All
;; dangerous calls are approved.  This is the ONLY way to get
;; auto-approval of dangerous tools post-W2.
(define (make-permissive-permission-config)
  (make-default-permission-config #:callback (lambda (tool-name args) #t) #:policy-mode 'permissive))

;; Deterministic type-tagged encoding for an exact invocation commitment.
;; Hash order is normalized while sequence order remains significant.
(define (canonical-approval-datum value)
  (define (encode item)
    (cond
      [(hash? item)
       (define entries
         (sort (for/list ([(key val) (in-hash item)])
                 (cons (encode key) (encode val)))
               string<?
               #:key car))
       (string-append "h{"
                      (apply string-append
                             (for/list ([entry (in-list entries)])
                               (format "~a=~a;" (car entry) (cdr entry))))
                      "}")]
      [(list? item) (string-append "l[" (apply string-append (map encode item)) "]")]
      [(pair? item) (format "p(~a.~a)" (encode (car item)) (encode (cdr item)))]
      [(vector? item)
       (string-append "v["
                      (apply string-append
                             (for/list ([part (in-vector item)])
                               (encode part)))
                      "]")]
      [(box? item) (format "x(~a)" (encode (unbox item)))]
      [(string? item) (format "s~s" item)]
      [(symbol? item) (format "y~s" item)]
      [(keyword? item) (format "k~s" item)]
      [(bytes? item) (format "b~s" item)]
      [(boolean? item) (if item "t" "f")]
      [(number? item) (format "n~s" item)]
      [(void? item) "zvoid"]
      [else (format "o~s" item)]))
  (encode value))

;; Commits to the tool identity and the exact immutable final argument object
;; produced by the scheduler after hooks and scheduler-owned injections.
(define (tool-approval-commitment-digest tool-name args)
  (sha256-string (canonical-approval-datum (hasheq 'tool-name tool-name 'arguments args))))

(define (bounded-control-safe value limit)
  (define redacted (redact-credential-data value))
  (define raw
    (if (string? redacted)
        redacted
        (format "~s" redacted)))
  (define safe
    (list->string (for/list ([char (in-string raw)])
                    (define code (char->integer char))
                    (if (or (< code 32) (and (>= code 127) (<= code 159))) #\space char))))
  (define compact (string-join (string-split safe) " "))
  (if (> (string-length compact) limit)
      (substring compact 0 limit)
      compact))

(define (publish-safely publisher event-type payload)
  (when (procedure? publisher)
    (with-handlers ([exn:fail? (lambda (_) (void))])
      (publisher event-type payload))))

;; Interactive permission callback backed solely by the runtime broker. The
;; publisher only transports correlation telemetry; it can never grant.
(define (perform-interactive-approval tool-name args publisher)
  (define channel (current-approval-channel))
  (define commitment-digest (tool-approval-commitment-digest tool-name args))
  (define base-presentation
    (hasheq 'approval-kind
            "tool"
            'tool-name
            (bounded-control-safe tool-name 80)
            'arguments-preview
            (bounded-control-safe args 500)
            'arguments-digest
            (sha256-string (canonical-approval-datum args))))
  (define presentation-digest (sha256-string (canonical-approval-datum base-presentation)))
  (define presentation (hash-set base-presentation 'presentation-digest presentation-digest))
  (cond
    [(or (not channel) (not (procedure? publisher)))
     (when (and (not channel) (procedure? publisher))
       (publish-safely publisher
                       "tool.approval-terminal"
                       (hasheq 'commitment-digest
                               commitment-digest
                               'presentation-digest
                               presentation-digest
                               'terminal-status
                               "denied-headless")))
     #f]
    [else
     (define request-id
       (register-approval-request-for-channel! channel commitment-digest presentation))
     (cond
       [(not request-id) #f]
       [else
        (define terminal-status "cancelled")
        (dynamic-wind void
                      (lambda ()
                        (with-handlers ([exn:fail? (lambda (_)
                                                     (set! terminal-status "publisher-error")
                                                     #f)])
                          (publisher "tool.approval-requested"
                                     (hasheq 'request-id
                                             request-id
                                             'commitment-digest
                                             commitment-digest
                                             'presentation-digest
                                             presentation-digest))
                          (define-values (outcome grant)
                            (approval-await-grant request-id commitment-digest))
                          (set! terminal-status (symbol->string outcome))
                          (and (eq? outcome 'approved) grant)))
                      (lambda ()
                        (cancel-approval-request! request-id)
                        (publish-safely publisher
                                        "tool.approval-terminal"
                                        (hasheq 'request-id
                                                request-id
                                                'commitment-digest
                                                commitment-digest
                                                'presentation-digest
                                                presentation-digest
                                                'terminal-status
                                                terminal-status))))])]))

(define interactive-approval-callback
  (case-lambda
    ;; Direct two-argument callback use has no publisher authority and denies.
    [(tool-name args) #f]
    [(tool-name args publisher) (perform-interactive-approval tool-name args publisher)]))

(define (make-interactive-permission-config)
  (make-default-permission-config #:callback interactive-approval-callback))

;; ============================================================
;; Predicate — does this tool call require approval?
;; ============================================================

(define (tool-needs-approval? config tool-name)
  (cond
    ;; Spawn tools still require authorization, but their implementation owns
    ;; the approval lifecycle; the scheduler recognizes this classification and
    ;; skips only its generic callback.
    [(tool-name-tool-owned-approval? tool-name) #t]
    ;; Explicitly auto-approved -> no
    [(set-member? (permission-config-auto-approved-tools config) tool-name) #f]
    ;; Explicitly in the needs-approval set -> yes
    [(set-member? (permission-config-needs-approval-tools config) tool-name) #t]
    ;; Unknown tool -- policy-dependent
    [(eq? (permission-config-policy-mode config) 'permissive) #f]
    ;; Strict mode (default) -- unknown tools require approval
    [else #t]))

;; ============================================================
;; Request approval — invoke the callback
;; ============================================================

(define (request-approval config tool-name args [publisher #f])
  (define callback (permission-config-approval-callback config))
  (if (procedure-arity-includes? callback 3)
      (callback tool-name args publisher)
      (callback tool-name args)))

;; ============================================================
;; Provides
;; ============================================================

(provide permission-config
         permission-config?
         permission-config-auto-approved-tools
         permission-config-needs-approval-tools
         permission-config-approval-callback
         permission-config-policy-mode
         make-strict-permission-config
         make-permissive-permission-config
         make-interactive-permission-config
         tool-approval-commitment-digest
         (contract-out [make-default-permission-config
                        (->* ()
                             (#:auto-approved (or/c (set/c string?) #f)
                                              #:needs-approval (or/c (set/c string?) #f)
                                              #:callback (or/c approval-callback? #f)
                                              #:policy-mode (or/c 'strict 'permissive))
                             permission-config?)])
         tool-needs-approval?
         request-approval)
