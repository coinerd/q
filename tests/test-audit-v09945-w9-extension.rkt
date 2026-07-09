#lang racket/base
;; W9 — Extension subsystem real-world audit
;; Tests: extension struct, registry, hooks, tiers, manifest, loader,
;;   context, events, combinators, dynamic-tools, ext-commands,
;;   message-inject, quarantine, catalog, compact-context
;; @suite default
;; @speed fast

(require rackunit
         racket/list
         racket/file
         racket/port
         racket/string
         json
         ;; Core extension types
         (only-in "../extensions/api.rkt"
                  make-extension-registry
                  register-extension!
                  unregister-extension!
                  lookup-extension
                  list-extensions
                  handlers-for-point
                  extension-registry?
                  extension
                  extension?
                  extension-name
                  extension-version
                  extension-api-version
                  extension-hooks)
         ;; Hook types
         "../util/hook-types.rkt"
         ;; Tiers
         "../extensions/tiers.rkt"
         ;; Combinators
         "../extensions/combinators.rkt"
         ;; Hooks dispatch
         "../extensions/hooks.rkt"
         ;; Manifest
         "../extensions/manifest.rkt"
         ;; Manifest audit
         "../extensions/manifest-audit.rkt"
         ;; Compact context extension
         (only-in "../extensions/compact-context.rkt" gather-planning-summary)
         ;; Quarantine
         "../extensions/quarantine.rkt"
         ;; Command types
         "../util/command-types.rkt"
         ;; Extension context
         (only-in "../extensions/context.rkt"
                  make-extension-ctx
                  ctx-session-id
                  ctx-session-dir
                  ctx-model
                  ctx-session-model
                  ctx-session-messages
                  ctx-session-token-count
                  ctx-cwd
                  extension-ctx?)
         ;; Extension events
         (only-in "../extensions/events.rkt"
                  ext-subscribe!
                  ext-publish!
                  ext-unsubscribe!
                  ext-unsubscribe-all!
                  ext-subscription-ids)
         ;; Event bus
         "../util/event/event-bus.rkt"
         ;; Extension catalog
         (only-in "../runtime/extension-catalog.rkt"
                  ext-info
                  ext-info?
                  ext-info-name
                  valid-extension-name?))

;; ---------------------------------------------------------------------------
;; 1. Extension Struct
;; ---------------------------------------------------------------------------

(test-case "audit-ext-struct-construction"
  (define ext (extension "test-ext" "1.0.0" "1" (hasheq)))
  (check-true (extension? ext))
  (check-equal? (extension-name ext) "test-ext")
  (check-equal? (extension-version ext) "1.0.0")
  (check-equal? (extension-api-version ext) "1")
  (check-equal? (extension-hooks ext) (hasheq)))

(test-case "audit-ext-struct-with-hooks"
  (define hooks (hasheq 'context.assembly (lambda (payload) (hook-pass payload))))
  (define ext (extension "hooked" "0.5.0" "1" hooks))
  (check-equal? (hash-count (extension-hooks ext)) 1)
  (check-true (hash-has-key? (extension-hooks ext) 'context.assembly)))

(test-case "audit-ext-struct-guard-rejects-non-string"
  (check-exn exn:fail? (lambda () (extension 123 "1.0.0" "1" (hasheq))))
  (check-exn exn:fail? (lambda () (extension "x" 123 "1" (hasheq))))
  (check-exn exn:fail? (lambda () (extension "x" "1.0.0" 123 (hasheq))))
  (check-exn exn:fail? (lambda () (extension "x" "1.0.0" "1" "not-a-hash"))))

(test-case "audit-ext-struct-transparent"
  (define ext (extension "t" "1.0.0" "1" (hasheq)))
  ;; Transparent struct can be equal? compared
  (check-equal? ext (extension "t" "1.0.0" "1" (hasheq))))

;; ---------------------------------------------------------------------------
;; 2. Extension Registry
;; ---------------------------------------------------------------------------

(test-case "audit-registry-empty"
  (define reg (make-extension-registry))
  (check-true (extension-registry? reg))
  (check-equal? (length (list-extensions reg)) 0))

(test-case "audit-registry-register-and-lookup"
  (define reg (make-extension-registry))
  (define ext (extension "foo" "1.0.0" "1" (hasheq)))
  (register-extension! reg ext)
  (check-equal? (length (list-extensions reg)) 1)
  (define found (lookup-extension reg "foo"))
  (check-true (extension? found))
  (check-equal? (extension-name found) "foo"))

(test-case "audit-registry-unregister"
  (define reg (make-extension-registry))
  (register-extension! reg (extension "a" "1.0.0" "1" (hasheq)))
  (register-extension! reg (extension "b" "2.0.0" "1" (hasheq)))
  (check-equal? (length (list-extensions reg)) 2)
  (unregister-extension! reg "a")
  (check-equal? (length (list-extensions reg)) 1)
  (check-false (lookup-extension reg "a"))
  (check-true (extension? (lookup-extension reg "b"))))

(test-case "audit-registry-replace-on-reregister"
  (define reg (make-extension-registry))
  (register-extension! reg (extension "dup" "1.0.0" "1" (hasheq)))
  (register-extension! reg (extension "dup" "2.0.0" "1" (hasheq)))
  (check-equal? (length (list-extensions reg)) 1)
  (define found (lookup-extension reg "dup"))
  (check-equal? (extension-version found) "2.0.0"))

(test-case "audit-registry-lookup-nonexistent"
  (define reg (make-extension-registry))
  (check-false (lookup-extension reg "nope")))

(test-case "audit-registry-insertion-order"
  (define reg (make-extension-registry))
  (register-extension! reg (extension "first" "1.0.0" "1" (hasheq)))
  (register-extension! reg (extension "second" "1.0.0" "1" (hasheq)))
  (register-extension! reg (extension "third" "1.0.0" "1" (hasheq)))
  (define names (map extension-name (list-extensions reg)))
  (check-equal? names '("first" "second" "third")))

(test-case "audit-registry-handlers-for-point"
  (define reg (make-extension-registry))
  (define h1 (lambda (p) (hook-pass p)))
  (define h2 (lambda (p) (hook-pass p)))
  (register-extension! reg (extension "ext-a" "1.0.0" "1" (hasheq 'context.assembly h1)))
  (register-extension! reg
                       (extension "ext-b" "1.0.0" "1" (hasheq 'context.assembly h2 'tool-call h1)))
  (define handlers (handlers-for-point reg 'context.assembly))
  (check-equal? (length handlers) 2)
  (check-equal? (car (car handlers)) "ext-a")
  (check-equal? (car (cadr handlers)) "ext-b")
  ;; Only ext-b has tool-call
  (define tc-handlers (handlers-for-point reg 'tool-call))
  (check-equal? (length tc-handlers) 1)
  (check-equal? (car (car tc-handlers)) "ext-b"))

;; ---------------------------------------------------------------------------
;; 3. Hook Result Types
;; ---------------------------------------------------------------------------

(test-case "audit-hook-pass-default"
  (define r (hook-pass))
  (check-true (hook-result? r))
  (check-equal? (hook-result-action r) 'pass)
  (check-false (hook-result-payload r)))

(test-case "audit-hook-pass-with-payload"
  (define r (hook-pass "unchanged"))
  (check-equal? (hook-result-action r) 'pass)
  (check-equal? (hook-result-payload r) "unchanged"))

(test-case "audit-hook-amend"
  (define r (hook-amend "new-payload"))
  (check-equal? (hook-result-action r) 'amend)
  (check-equal? (hook-result-payload r) "new-payload"))

(test-case "audit-hook-block"
  (define r (hook-block "blocked-reason"))
  (check-equal? (hook-result-action r) 'block)
  (check-equal? (hook-result-payload r) "blocked-reason"))

(test-case "audit-hook-schema-version"
  (check-true (exact-integer? (hook-schema-version)))
  (check-true (> (hook-schema-version) 0)))

(test-case "audit-hook-valid-actions-for"
  ;; Known hook point with specific actions
  (define actions (valid-hook-actions-for 'tool-call))
  (check-not-false (member 'pass actions) "tool-call should allow pass")
  (check-not-false (member 'block actions) "tool-call should allow block")
  ;; Unknown hook point gets default actions
  (define default-actions (valid-hook-actions-for 'totally-unknown-hook))
  (check-not-false (member 'pass default-actions) "unknown gets default pass")
  (check-not-false (member 'amend default-actions) "unknown gets default amend"))

(test-case "audit-hook-validate-result"
  ;; Valid action for this hook
  (check-true (validate-hook-result 'tool-call (hook-pass)))
  (check-true (validate-hook-result 'tool-call (hook-block)))
  ;; message-update only allows amend
  (check-true (validate-hook-result 'message-update (hook-amend "new")))
  ;; pass is NOT valid for message-update (schema says amend-only)
  (check-false (validate-hook-result 'message-update (hook-pass))))

(test-case "audit-hook-valid-name?"
  (check-true (valid-hook-name? 'tool-call))
  (check-true (valid-hook-name? 'context.assembly))
  (check-true (valid-hook-name? 'turn-start))
  (check-false (valid-hook-name? 'totally-made-up-hook)))

;; ---------------------------------------------------------------------------
;; 4. Tier System
;; ---------------------------------------------------------------------------

(test-case "audit-tier-predicates"
  (check-true (tier? 'hooks))
  (check-true (tier? 'commands))
  (check-true (tier? 'session))
  (check-true (tier? 'providers))
  (check-true (tier? 'tui))
  (check-false (tier? 'admin))
  (check-false (tier? 'superuser)))

(test-case "audit-tier-capabilities-cumulative"
  ;; hooks = lowest tier, should have 1 capability
  (check-equal? (length (tier-capabilities 'hooks)) 1)
  ;; tui = highest tier, should have all capabilities
  (check-equal? (length (tier-capabilities 'tui)) 7)
  ;; Cumulative: commands includes hooks capabilities
  (check-true (capability-allowed? 'commands 'hook-dispatch))
  (check-true (capability-allowed? 'commands 'command-register))
  (check-false (capability-allowed? 'commands 'session-lifecycle)))

(test-case "audit-tier-hook-point-mapping"
  (check-equal? (hook-point-tier 'context-assembly) 'hooks)
  (check-equal? (hook-point-tier 'command-dispatch) 'commands)
  (check-equal? (hook-point-tier 'session-start) 'session)
  (check-equal? (hook-point-tier 'provider-register) 'providers)
  (check-equal? (hook-point-tier 'tui-panel) 'tui)
  ;; Unknown hook points default to 'hooks
  (check-equal? (hook-point-tier 'some-random-hook) 'hooks))

(test-case "audit-tier-api-version-validation"
  (check-true (valid-api-version? "1"))
  (check-false (valid-api-version? "2"))
  (check-false (valid-api-version? "0"))
  (check-false (valid-api-version? "")))

(test-case "audit-tier-validate-extension"
  ;; Extension with only hooks-tier hooks, declared as 'hooks
  (define ext-hooks
    (extension "ext1" "1.0.0" "1" (hasheq 'context-assembly (lambda (p) (hook-pass p)))))
  (check-true (eq? #t (validate-extension-tier ext-hooks 'hooks)))
  (check-true (extension-tier-valid? ext-hooks 'hooks))
  ;; Extension with commands-tier hook but declared as 'hooks — violation
  (define ext-cmd
    (extension "ext2" "1.0.0" "1" (hasheq 'command-dispatch (lambda (p) (hook-pass p)))))
  (check-true (list? (validate-extension-tier ext-cmd 'hooks)))
  (check-false (extension-tier-valid? ext-cmd 'hooks))
  ;; Same extension with tier 'commands — valid
  (check-true (extension-tier-valid? ext-cmd 'commands)))

;; ---------------------------------------------------------------------------
;; 5. Hook Dispatch
;; ---------------------------------------------------------------------------

(test-case "audit-dispatch-pass-through"
  (define reg (make-extension-registry))
  (define h (lambda (payload) (hook-pass payload)))
  (register-extension! reg (extension "pass-ext" "1.0.0" "1" (hasheq 'context.assembly h)))
  (define result (dispatch-hooks 'context.assembly "original" reg))
  (check-equal? (hook-result-action result) 'pass)
  (check-equal? (hook-result-payload result) "original"))

(test-case "audit-dispatch-amend"
  (define reg (make-extension-registry))
  (define h (lambda (payload) (hook-amend "amended")))
  (register-extension! reg (extension "amend-ext" "1.0.0" "1" (hasheq 'context.assembly h)))
  (define result (dispatch-hooks 'context.assembly "original" reg))
  (check-equal? (hook-result-action result) 'amend)
  (check-equal? (hook-result-payload result) "amended"))

(test-case "audit-dispatch-block"
  (define reg (make-extension-registry))
  (define h (lambda (payload) (hook-block "blocked")))
  (register-extension! reg (extension "block-ext" "1.0.0" "1" (hasheq 'tool-call h)))
  (define result (dispatch-hooks 'tool-call "original" reg))
  (check-equal? (hook-result-action result) 'block)
  (check-equal? (hook-result-payload result) "blocked"))

(test-case "audit-dispatch-multiple-extensions"
  (define reg (make-extension-registry))
  (register-extension!
   reg
   (extension "ext1" "1.0.0" "1" (hasheq 'context.assembly (lambda (p) (hook-pass p)))))
  (register-extension!
   reg
   (extension "ext2" "1.0.0" "1" (hasheq 'context.assembly (lambda (p) (hook-amend "amended")))))
  (define result (dispatch-hooks 'context.assembly "original" reg))
  (check-equal? (hook-result-action result) 'amend)
  (check-equal? (hook-result-payload result) "amended"))

(test-case "audit-dispatch-block-stops-chain"
  (define reg (make-extension-registry))
  ;; First handler blocks — second should never run
  (define ran #f)
  (register-extension!
   reg
   (extension "blocker" "1.0.0" "1" (hasheq 'tool-call (lambda (p) (hook-block "stop")))))
  (register-extension! reg
                       (extension "after"
                                  "1.0.0"
                                  "1"
                                  (hasheq 'tool-call
                                          (lambda (p)
                                            (set! ran #t)
                                            (hook-pass p)))))
  (define result (dispatch-hooks 'tool-call "data" reg))
  (check-equal? (hook-result-action result) 'block)
  (check-false ran "second handler should not have run"))

(test-case "audit-dispatch-amend-chains-payload"
  (define reg (make-extension-registry))
  ;; First amends, second sees amended payload
  (register-extension! reg
                       (extension "first"
                                  "1.0.0"
                                  "1"
                                  (hasheq 'context.assembly
                                          (lambda (p) (hook-amend (string-append p "+1"))))))
  (register-extension! reg
                       (extension "second"
                                  "1.0.0"
                                  "1"
                                  (hasheq 'context.assembly
                                          (lambda (p) (hook-amend (string-append p "+2"))))))
  (define result (dispatch-hooks 'context.assembly "start" reg))
  (check-equal? (hook-result-action result) 'amend)
  (check-equal? (hook-result-payload result) "start+1+2"))

(test-case "audit-dispatch-no-handlers"
  (define reg (make-extension-registry))
  (define result (dispatch-hooks 'context.assembly "payload" reg))
  (check-equal? (hook-result-action result) 'pass)
  (check-equal? (hook-result-payload result) "payload"))

;; ---------------------------------------------------------------------------
;; 6. Combinators
;; ---------------------------------------------------------------------------

(test-case "audit-combinator-with-timeout-success"
  (define result (with-timeout 5000 (lambda () 42)))
  (check-equal? result 42))

(test-case "audit-combinator-with-timeout-fires"
  (define result
    (with-timeout 50
                  (lambda ()
                    (sleep 5)
                    42)
                  #:on-timeout (lambda () 'timed-out)))
  (check-equal? result 'timed-out))

(test-case "audit-combinator-with-timeout-on-error"
  (define result (with-timeout 5000 (lambda () (error "boom")) #:on-error (lambda (e) 'caught)))
  (check-equal? result 'caught))

(test-case "audit-combinator-with-error-policy-critical"
  ;; Critical hook error → should return error-default
  (define result (with-error-policy #t (lambda () (error "fail")) (lambda () (hook-block "failed"))))
  (check-equal? (hook-result-action result) 'block))

(test-case "audit-combinator-with-error-policy-advisory"
  ;; Advisory hook error → should return error-default
  (define result (with-error-policy #f (lambda () (error "fail")) (lambda () (hook-pass "default"))))
  (check-equal? (hook-result-action result) 'pass))

(test-case "audit-combinator-with-hook-validation-valid"
  (define result
    (with-hook-validation "test-ext" 'tool-call (hook-pass "data") (lambda () (hook-block))))
  (check-true (hook-result? result))
  (check-equal? (hook-result-action result) 'pass))

(test-case "audit-combinator-with-hook-validation-invalid"
  ;; Non-hook-result returned → default used
  (define result (with-hook-validation "test-ext" 'tool-call 42 (lambda () (hook-block "default"))))
  (check-equal? (hook-result-action result) 'block)
  (check-equal? (hook-result-payload result) "default"))

;; ---------------------------------------------------------------------------
;; 7. Manifest
;; ---------------------------------------------------------------------------

(test-case "audit-manifest-construction"
  (define m
    (make-qpm-manifest #:name "my-ext"
                       #:version "1.0.0"
                       #:api-version "1"
                       #:type 'extension
                       #:description "A test extension"
                       #:author "tester"))
  (check-true (qpm-manifest? m))
  (check-equal? (qpm-manifest-name m) "my-ext")
  (check-equal? (qpm-manifest-version m) "1.0.0")
  (check-equal? (qpm-manifest-type m) 'extension))

(test-case "audit-manifest-validate-valid"
  (define m
    (make-qpm-manifest #:name "valid"
                       #:version "1.2.3"
                       #:api-version "1"
                       #:type 'extension
                       #:description "Valid"
                       #:author "a"))
  (define-values (ok? errors) (validate-manifest m))
  (check-true ok?)
  (check-equal? errors '()))

(test-case "audit-manifest-validate-invalid-version"
  (define m
    (make-qpm-manifest #:name "bad"
                       #:version "not-semver"
                       #:api-version "1"
                       #:type 'extension
                       #:description "Bad"
                       #:author "a"))
  (define-values (ok? errors) (validate-manifest m))
  (check-false ok?)
  (check-true (> (length errors) 0)))

(test-case "audit-manifest-validate-bad-api-version"
  (define m
    (make-qpm-manifest #:name "bad"
                       #:version "1.0.0"
                       #:api-version "not-digits"
                       #:type 'extension
                       #:description "Bad"
                       #:author "a"))
  (define-values (ok? errors) (validate-manifest m))
  (check-false ok?))

(test-case "audit-manifest-validate-unsafe-paths"
  (define m
    (make-qpm-manifest #:name "bad"
                       #:version "1.0.0"
                       #:api-version "1"
                       #:type 'extension
                       #:description "Bad"
                       #:author "a"
                       #:files '("../etc/passwd" "/etc/shadow" "ok.rkt")))
  (define-values (ok? errors) (validate-manifest m))
  (check-false ok?)
  (check-true (> (length errors) 0)))

(test-case "audit-manifest-safe-path-predicate"
  (check-true (safe-manifest-file-path? "src/main.rkt"))
  (check-true (safe-manifest-file-path? "lib/helper.rkt"))
  (check-false (safe-manifest-file-path? "../etc/passwd"))
  (check-false (safe-manifest-file-path? "/etc/shadow"))
  (check-false (safe-manifest-file-path? "C:/Windows/system32"))
  (check-false (safe-manifest-file-path? "")))

(test-case "audit-manifest-jsexpr-round-trip"
  (define m
    (make-qpm-manifest #:name "rt"
                       #:version "2.0.0"
                       #:api-version "1"
                       #:type 'skill
                       #:description "Round trip"
                       #:author "tester"
                       #:homepage "https://example.com"
                       #:license "MIT"))
  (define j (qpm-manifest->jsexpr m))
  (check-true (hash? j))
  (check-equal? (hash-ref j 'name) "rt")
  (define m2 (jsexpr->qpm-manifest j))
  (check-true (qpm-manifest? m2))
  (check-true (qpm-manifest=? m m2)))

(test-case "audit-manifest-type-predicate"
  (check-true (qpm-type? 'extension))
  (check-true (qpm-type? 'skill))
  (check-true (qpm-type? 'bundle))
  (check-false (qpm-type? 'plugin))
  (check-false (qpm-type? "extension")))

(test-case "audit-manifest-compare"
  (define m1
    (make-qpm-manifest #:name "x"
                       #:version "1.0.0"
                       #:api-version "1"
                       #:type 'extension
                       #:description "d"
                       #:author "a"))
  (define m2
    (make-qpm-manifest #:name "x"
                       #:version "1.0.0"
                       #:api-version "1"
                       #:type 'extension
                       #:description "d"
                       #:author "a"))
  (define m3
    (make-qpm-manifest #:name "x"
                       #:version "2.0.0"
                       #:api-version "1"
                       #:type 'extension
                       #:description "d"
                       #:author "a"))
  (check-true (qpm-manifest=? m1 m2))
  (check-false (qpm-manifest=? m1 m3)))

;; ---------------------------------------------------------------------------
;; 8. Extension Context
;; ---------------------------------------------------------------------------

(test-case "audit-ctx-construction-minimal"
  (define ctx
    (make-extension-ctx #:session-id "test-session"
                        #:session-dir "/tmp/test"
                        #:event-bus #f
                        #:extension-registry #f))
  (check-true (extension-ctx? ctx))
  (check-equal? (ctx-session-id ctx) "test-session")
  (check-equal? (ctx-session-dir ctx) "/tmp/test"))

(test-case "audit-ctx-optional-fields-default"
  (define ctx
    (make-extension-ctx #:session-id "s" #:session-dir "/tmp" #:event-bus #f #:extension-registry #f))
  (check-false (ctx-model ctx))
  (check-false (ctx-cwd ctx)))

(test-case "audit-ctx-with-model"
  (define ctx
    (make-extension-ctx #:session-id "s"
                        #:session-dir "/tmp"
                        #:event-bus #f
                        #:extension-registry #f
                        #:model-name "claude-3"))
  (check-equal? (ctx-model ctx) "claude-3")
  (check-equal? (ctx-session-model ctx) "claude-3"))

(test-case "audit-ctx-session-messages-empty"
  (define ctx
    (make-extension-ctx #:session-id "s" #:session-dir "/tmp" #:event-bus #f #:extension-registry #f))
  (check-equal? (ctx-session-messages ctx) '()))

(test-case "audit-ctx-session-messages-with-data"
  (define msgs (list (hasheq 'role 'user 'text "hi")))
  (define ctx
    (make-extension-ctx #:session-id "s"
                        #:session-dir "/tmp"
                        #:event-bus #f
                        #:extension-registry #f
                        #:session-messages msgs))
  (check-equal? (length (ctx-session-messages ctx)) 1))

(test-case "audit-ctx-token-count-empty"
  (define ctx
    (make-extension-ctx #:session-id "s" #:session-dir "/tmp" #:event-bus #f #:extension-registry #f))
  (check-true (hash? (ctx-session-token-count ctx))))

;; ---------------------------------------------------------------------------
;; 9. Extension Events (subscription tracking)
;; ---------------------------------------------------------------------------

(test-case "audit-ext-events-subscribe-and-publish"
  (define bus (make-event-bus))
  (define received '())
  (define sub-id (ext-subscribe! bus "my-ext" (lambda (evt) (set! received (cons evt received)))))
  (check-true (exact-nonnegative-integer? sub-id))
  (check-not-false (member sub-id (ext-subscription-ids "my-ext")))
  (check-equal? (length received) 0))

(test-case "audit-ext-events-unsubscribe-all"
  (define bus (make-event-bus))
  (ext-subscribe! bus "ext-a" (lambda (e) (void)))
  (ext-subscribe! bus "ext-a" (lambda (e) (void)))
  (ext-subscribe! bus "ext-b" (lambda (e) (void)))
  (check-equal? (length (ext-subscription-ids "ext-a")) 2)
  (check-equal? (length (ext-subscription-ids "ext-b")) 1)
  (ext-unsubscribe-all! bus "ext-a")
  (check-equal? (length (ext-subscription-ids "ext-a")) 0)
  ;; ext-b should be unaffected
  (check-equal? (length (ext-subscription-ids "ext-b")) 1))

;; ---------------------------------------------------------------------------
;; 10. Command Types
;; ---------------------------------------------------------------------------

(test-case "audit-cmd-entry-construction"
  (define e (cmd-entry "/help" "Show help" 'general '() '("h" "?")))
  (check-true (cmd-entry? e))
  (check-equal? (cmd-entry-name e) "/help")
  (check-equal? (cmd-entry-summary e) "Show help")
  (check-equal? (cmd-entry-category e) 'general)
  (check-equal? (cmd-entry-aliases e) '("h" "?")))

(test-case "audit-cmd-register-and-lookup"
  (define reg (hasheq))
  (define e (cmd-entry "/foo" "Foo command" 'general '() '()))
  (define reg2 (register-command! reg e))
  (define found (lookup-command reg2 "/foo"))
  (check-true (cmd-entry? found))
  (check-equal? (cmd-entry-name found) "/foo")
  (check-false (lookup-command reg2 "/bar")))

;; ---------------------------------------------------------------------------
;; 11. Quarantine (state management)
;; ---------------------------------------------------------------------------

(test-case "audit-quarantine-extension-state-unknown"
  ;; Fresh quarantine dir should show 'unknown for any name
  (define temp-dir (make-temporary-file "quarantine-~a" 'directory))
  (check-not-false temp-dir)
  (define state
    (parameterize ([current-quarantine-dir temp-dir])
      (extension-state "never-seen")))
  (check-equal? state 'unknown)
  (delete-directory/files temp-dir))

(test-case "audit-quarantine-disable-and-state"
  (define temp-dir (make-temporary-file "quarantine-~a" 'directory))
  (define state-before
    (parameterize ([current-quarantine-dir temp-dir])
      (extension-state "my-ext")))
  (check-equal? state-before 'unknown)
  (parameterize ([current-quarantine-dir temp-dir])
    (disable-extension! "my-ext"))
  (define state-after
    (parameterize ([current-quarantine-dir temp-dir])
      (extension-state "my-ext")))
  (check-equal? state-after 'disabled)
  (delete-directory/files temp-dir))

(test-case "audit-quarantine-disable-idempotent"
  (define temp-dir (make-temporary-file "quarantine-~a" 'directory))
  (parameterize ([current-quarantine-dir temp-dir])
    (disable-extension! "ext1")
    (disable-extension! "ext1") ;; double-disable should not error
    (check-equal? (extension-state "ext1") 'disabled))
  (delete-directory/files temp-dir))

(test-case "audit-quarantine-format-status"
  (define temp-dir (make-temporary-file "quarantine-~a" 'directory))
  (define status
    (parameterize ([current-quarantine-dir temp-dir])
      (format-extension-status "unknown-ext")))
  (check-true (string? status))
  (check-true (string-contains? status "unknown-ext"))
  (delete-directory/files temp-dir))

(test-case "audit-quarantine-list-empty"
  (define temp-dir (make-temporary-file "quarantine-~a" 'directory))
  (define q-list
    (parameterize ([current-quarantine-dir temp-dir])
      (list-quarantined)))
  (check-equal? q-list '())
  (delete-directory/files temp-dir))

;; ---------------------------------------------------------------------------
;; 12. Extension Catalog
;; ---------------------------------------------------------------------------

(test-case "audit-catalog-valid-extension-name"
  (check-true (valid-extension-name? "my-ext"))
  (check-true (valid-extension-name? "my_ext"))
  (check-true (valid-extension-name? "myext123"))
  (check-true (valid-extension-name? "A-B_C"))
  ;; Invalid names
  (check-false (valid-extension-name? "../etc"))
  (check-false (valid-extension-name? "ext with spaces"))
  (check-false (valid-extension-name? "ext;rm-rf"))
  (check-false (valid-extension-name? ""))
  (check-false (valid-extension-name? 123)))

(test-case "audit-catalog-ext-info-struct"
  (define info (ext-info "name" #f "1.0.0" "desc"))
  (check-true (ext-info? info))
  (check-equal? (ext-info-name info) "name"))

;; ---------------------------------------------------------------------------
;; 13. Manifest Audit
;; ---------------------------------------------------------------------------

(test-case "audit-manifest-audit-missing-dir"
  (define issues (audit-package "/nonexistent/path/that/does/not/exist"))
  (check-true (> (length issues) 0))
  (check-true (string-contains? (car issues) "manifest")))

(test-case "audit-manifest-audit-valid-package"
  ;; Create a temp dir with a valid manifest and file
  (define temp-dir (make-temporary-file "pkg-~a" 'directory))
  (define m
    (make-qpm-manifest #:name "test-pkg"
                       #:version "1.0.0"
                       #:api-version "1"
                       #:type 'extension
                       #:description "Test"
                       #:author "tester"
                       #:files '("main.rkt")))
  (write-qpm-manifest m (build-path temp-dir "qpm.json"))
  (call-with-output-file (build-path temp-dir "main.rkt")
                         (lambda (out) (displayln "#lang racket/base" out))
                         #:exists 'replace)
  (define issues (audit-package temp-dir))
  (check-equal? issues '() (format "expected no issues, got: ~a" issues))
  (delete-directory/files temp-dir))

(test-case "audit-manifest-audit-missing-file"
  (define temp-dir (make-temporary-file "pkg-~a" 'directory))
  (define m
    (make-qpm-manifest #:name "test-pkg"
                       #:version "1.0.0"
                       #:api-version "1"
                       #:type 'extension
                       #:description "Test"
                       #:author "tester"
                       #:files '("missing.rkt")))
  (write-qpm-manifest m (build-path temp-dir "qpm.json"))
  (define issues (audit-package temp-dir))
  (check-true (> (length issues) 0))
  (check-true (ormap (lambda (s) (string-contains? s "missing.rkt")) issues))
  (delete-directory/files temp-dir))

;; ---------------------------------------------------------------------------
;; 14. Compact Context Extension
;; ---------------------------------------------------------------------------

(test-case "audit-compact-context-gather-summary-no-dir"
  (define summary (gather-planning-summary "/nonexistent/path"))
  (check-true (string? summary))
  (check-true (string-contains? summary "No .planning")))

(test-case "audit-compact-context-gather-summary-with-dir"
  (define temp-dir (make-temporary-file "proj-~a" 'directory))
  (define planning-dir (build-path temp-dir ".planning"))
  (make-directory planning-dir)
  (call-with-output-file (build-path planning-dir "PLAN.md")
                         (lambda (out) (display "# Test Plan" out))
                         #:exists 'replace)
  (call-with-output-file (build-path planning-dir "STATE.md")
                         (lambda (out) (display "# Test State" out))
                         #:exists 'replace)
  (define summary (gather-planning-summary temp-dir))
  (check-true (string? summary))
  (check-true (string-contains? summary "PLAN.md"))
  (check-true (string-contains? summary "Test Plan"))
  (check-true (string-contains? summary "STATE.md"))
  (check-true (string-contains? summary "Test State"))
  (delete-directory/files temp-dir))

;; ---------------------------------------------------------------------------
;; 15. with-hook-block-guard helper
;; ---------------------------------------------------------------------------

(test-case "audit-hook-block-guard-pass"
  (define reg (make-extension-registry))
  (register-extension! reg
                       (extension "ext" "1.0.0" "1" (hasheq 'tool-call (lambda (p) (hook-pass p)))))
  (define result
    (with-hook-block-guard reg
                           'tool-call
                           "payload"
                           (lambda (block-payload) 'blocked)
                           (lambda () 'passed)))
  (check-equal? result 'passed))

(test-case "audit-hook-block-guard-block"
  (define reg (make-extension-registry))
  (register-extension!
   reg
   (extension "ext" "1.0.0" "1" (hasheq 'tool-call (lambda (p) (hook-block "denied")))))
  (define result
    (with-hook-block-guard reg
                           'tool-call
                           "payload"
                           (lambda (block-payload) (string-append "blocked:" block-payload))
                           (lambda () 'passed)))
  (check-equal? result "blocked:denied"))

;; ---------------------------------------------------------------------------
;; 16. Shared Command Types
;; ---------------------------------------------------------------------------

(test-case "audit-shared-command-construction"
  (define cmd (shared-command 'help "" 'none 'tui))
  (check-true (shared-command? cmd))
  (check-equal? (shared-command-name cmd) 'help)
  (check-equal? (shared-command-args cmd) "")
  (check-equal? (shared-command-kind cmd) 'none)
  (check-equal? (shared-command-source cmd) 'tui))
