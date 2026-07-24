#lang racket

;; @speed fast
;; @suite security

(require rackunit
         json
         racket/file
         (only-in "../cli/args.rkt"
                  cli-config
                  cli-config-auto-approve?
                  cli-config->runtime-config
                  parse-cli-args)
         (only-in "../runtime/settings.rkt" q-settings)
         (only-in "../runtime/settings-query.rkt" setting-permission-mode)
         (only-in "../runtime/session/session-config.rkt"
                  config-permission-config
                  hash->session-config)
         (only-in "../runtime/tool-coordinator.rkt" permission-config-for-execution)
         (only-in "../tools/permission-gate.rkt"
                  make-strict-permission-config
                  make-interactive-permission-config
                  permission-config-approval-callback
                  permission-config-policy-mode
                  request-approval)
         (only-in "../runtime/approval/broker.rkt"
                  make-approval-channel
                  set-approval-channel!
                  clear-approval-channel!
                  approval-decide!)
         (only-in "../tools/registry.rkt" make-tool-registry register-tool!)
         (only-in "../tools/tool.rkt" make-success-result make-tool)
         (only-in "../wiring/mode-helpers.rkt" resolve-permission-config)
         (only-in "../wiring/run-modes.rkt" make-mcp-governed-execute-fn reload-config!))

(define (settings-with-permission-mode value)
  (define merged (hash 'security (hash 'permission-mode value)))
  (q-settings (hash) (hash) merged))

(test-case "--auto-approve is a positive boolean CLI flag"
  (define default-cfg (parse-cli-args '()))
  (define approving-cfg (parse-cli-args '("--auto-approve")))
  (check-false (cli-config-auto-approve? default-cfg))
  (check-true (cli-config-auto-approve? approving-cfg))
  (check-true (hash-ref (cli-config->runtime-config approving-cfg) 'cli-auto-approve? #f)))

(test-case "adding auto-approve does not break positional cli-config constructors"
  (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f #f #f #f #f))
  (check-false (cli-config-auto-approve? cfg)))

(test-case "security.permission-mode accepts strict and permissive"
  (check-equal? (setting-permission-mode (settings-with-permission-mode "strict")) 'strict)
  (check-equal? (setting-permission-mode (settings-with-permission-mode 'permissive)) 'permissive))

(test-case "missing or invalid permission mode fails closed"
  (check-equal? (setting-permission-mode (q-settings (hash) (hash) (hash))) 'strict)
  (check-equal? (setting-permission-mode (settings-with-permission-mode "invalid")) 'strict)
  (check-equal? (setting-permission-mode (settings-with-permission-mode #t)) 'strict))

(test-case "permission precedence is explicit over CLI over settings over TUI over strict"
  (define strict-settings (settings-with-permission-mode "strict"))
  (define permissive-settings (settings-with-permission-mode "permissive"))
  (define explicit (make-strict-permission-config))

  (check-eq? (resolve-permission-config strict-settings #:explicit explicit #:cli-auto-approve? #t)
             explicit)
  (check-equal? (permission-config-policy-mode (resolve-permission-config strict-settings
                                                                          #:cli-auto-approve? #t))
                'permissive)
  (check-equal? (permission-config-policy-mode (resolve-permission-config permissive-settings))
                'permissive)
  (define tui-config (resolve-permission-config strict-settings #:tui? #t))
  (check-equal? (permission-config-policy-mode tui-config) 'strict)
  (define approved? (box #f))
  (dynamic-wind (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 100)))
                (lambda ()
                  (define grant
                    (request-approval tui-config
                                      "bash"
                                      (hasheq 'command "echo tui")
                                      (lambda (type payload)
                                        (when (string=? type "tool.approval-requested")
                                          (set-box! approved? #t)
                                          (approval-decide! (hash-ref payload 'request-id)
                                                            (hash-ref payload 'commitment-digest)
                                                            #t)))))
                  (check-true (unbox approved?))
                  (check-not-false grant))
                clear-approval-channel!)
  (check-equal?
   (permission-config-policy-mode (resolve-permission-config (q-settings (hash) (hash) (hash))))
   'strict))

(test-case "resolved permission config reaches normal agent execution"
  (define resolved (resolve-permission-config (settings-with-permission-mode "permissive")))
  (define session-cfg (hash->session-config (hash 'permission-config resolved)))
  (check-eq? (config-permission-config session-cfg) resolved)
  (check-eq? (permission-config-for-execution session-cfg #f) resolved)
  (define explicit (make-strict-permission-config))
  (check-eq? (permission-config-for-execution session-cfg explicit) explicit)
  (check-true ((permission-config-approval-callback (permission-config-for-execution session-cfg #f))
               "bash"
               (hash))))

(test-case "resolved permission config reaches governed MCP execution"
  (define registry (make-tool-registry))
  (define executed? (box #f))
  (register-tool! registry
                  (make-tool "permission-probe"
                             "permission wiring probe"
                             (hasheq 'type "object" 'properties (hasheq))
                             (lambda (args ctx)
                               (set-box! executed? #t)
                               (make-success-result "ok"))))
  (define execute
    (make-mcp-governed-execute-fn
     registry
     #:permission-config (resolve-permission-config (settings-with-permission-mode "permissive"))))
  (execute "permission-probe" (hasheq))
  (check-true (unbox executed?)))

(test-case "reload re-resolves settings while preserving CLI precedence"
  (define dir (make-temporary-file "q-permission-reload-~a" 'directory))
  (define config-path (build-path dir "config.json"))
  (dynamic-wind
   void
   (lambda ()
     (call-with-output-file config-path
                            (lambda (out)
                              (write-json (hash 'security (hash 'permission-mode "permissive")) out))
                            #:exists 'replace)
     (define base
       (hash->session-config (hash 'project-dir
                                   dir
                                   'home-dir
                                   dir
                                   'config-path
                                   config-path
                                   'context-assembly-profile
                                   'off
                                   'permission-config
                                   (make-strict-permission-config))))
     (define-values (reloaded _) (reload-config! base))
     (check-equal? (permission-config-policy-mode (config-permission-config reloaded)) 'permissive)

     ;; A positive CLI flag remains authoritative after another reload.
     (call-with-output-file config-path
                            (lambda (out)
                              (write-json (hash 'security (hash 'permission-mode "strict")) out))
                            #:exists 'replace)
     (define cli-base
       (hash->session-config (hash 'project-dir
                                   dir
                                   'home-dir
                                   dir
                                   'config-path
                                   config-path
                                   'context-assembly-profile
                                   'off
                                   'cli-auto-approve?
                                   #t
                                   'permission-config
                                   (make-strict-permission-config))))
     (define-values (cli-reloaded __) (reload-config! cli-base))
     (check-equal? (permission-config-policy-mode (config-permission-config cli-reloaded))
                   'permissive)

     ;; A TUI marker survives reload and reselects interactive strict approval.
     (define tui-base
       (hash->session-config (hash 'project-dir
                                   dir
                                   'home-dir
                                   dir
                                   'config-path
                                   config-path
                                   'context-assembly-profile
                                   'off
                                   'tui-interactive-approval?
                                   #t
                                   'permission-config
                                   (make-interactive-permission-config))))
     (define-values (tui-reloaded ___) (reload-config! tui-base))
     (define requested? (box #f))
     (dynamic-wind
      (lambda () (set-approval-channel! (make-approval-channel #:timeout-ms 100)))
      (lambda ()
        (check-not-false (request-approval (config-permission-config tui-reloaded)
                                           "bash"
                                           (hasheq 'command "echo reloaded")
                                           (lambda (type payload)
                                             (when (string=? type "tool.approval-requested")
                                               (set-box! requested? #t)
                                               (approval-decide! (hash-ref payload 'request-id)
                                                                 (hash-ref payload 'commitment-digest)
                                                                 #t)))))
        (check-true (unbox requested?)))
      clear-approval-channel!))
   (lambda () (delete-directory/files dir #:must-exist? #f))))
