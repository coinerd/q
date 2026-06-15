;; docs/config-schema.rktd
;; Machine-readable configuration schema for q.
;; Generated from runtime/settings-query.rkt accessor contracts.
;; Format: ((domain (key type default description since-version) ...) ...)
;;
;; STABILITY: public — tools may parse this to validate configs.

((agent
  (parallel-tools
   (type . boolean)
   (default . #f)
   (description . "Whether tools may be executed in parallel within a batch")
   (since . "0.14.0"))
  (http-request-timeout
   (type . number)
   (default . 300)
   (description . "Overall HTTP request timeout in seconds (connection + response)")
   (since . "0.14.0"))
  (warn-on-destructive
   (type . boolean)
   (default . #f)
   (description . "Emit warning when destructive commands are detected")
   (since . "0.20.0"))
  (trace-enabled
   (type . boolean)
   (default . #f)
   (description . "Enable trace logging for debugging")
   (since . "0.14.0"))
  (trace-max-files
   (type . exact-positive-integer)
   (default . 100)
   (description . "Maximum number of trace files to keep")
   (since . "0.14.0"))
  (credential-policy
   (type . (or/c auto keychain-preferred keychain-required env-only))
   (default . auto)
   (description . "Credential resolution policy")
   (since . "0.25.0"))
  (shell-risk-classifier
   (type . (or/c regex structured both))
   (default . regex)
   (description . "Shell command risk classification method")
   (since . "0.30.0"))
  (default-provider
   (type . (or/c string #f))
   (default . #f)
   (description . "Default LLM provider name")
   (since . "0.14.0"))
  (default-model
   (type . (or/c string #f))
   (default . #f)
   (description . "Default model name for the default provider")
   (since . "0.14.0")))

 (context-assembly
  (context-assembly-profile
   (type . symbol)
   (default . balanced)
   (description . "Context assembly strategy profile")
   (since . "0.77.0")))

 (memory
  (memory-injection-budget
   (type . (or/c exact-positive-integer #f))
   (default . #f)
   (description . "Token budget for memory injection into context")
   (since . "0.95.0"))
  (memory-backend
   (type . (or/c symbol hash #f))
   (default . #f)
   (description . "Memory backend configuration")
   (since . "0.95.0"))
  (memory-auto-extraction-enabled
   (type . boolean)
   (default . #f)
   (description . "Enable automatic memory extraction from tool results")
   (since . "0.95.0"))
  (memory-auto-extraction-min-confidence
   (type . (and/c real? (between/c 0 1)))
   (default . 0.5)
   (description . "Minimum confidence threshold for auto-extraction")
   (since . "0.95.0"))
  (memory-user-scope-enabled
   (type . boolean)
   (default . #f)
   (description . "Enable user-scoped memory (cross-project)")
   (since . "0.95.0"))
  (memory-auto-reflection-enabled
   (type . boolean)
   (default . #f)
   (description . "Enable automatic memory reflection/consolidation")
   (since . "0.95.0"))
  (memory-auto-reflection-min-items
   (type . (and/c exact-positive-integer (>=/c 2)))
   (default . 5)
   (description . "Minimum items before triggering auto-reflection")
   (since . "0.95.0"))
  (reflection-prompt-enabled
   (type . boolean)
   (default . #f)
   (description . "Enable reflection prompt display")
   (since . "0.95.0"))
  (auto-distillation-enabled
   (type . (or/c boolean unset))
   (default . unset)
   (description . "Enable automatic context distillation")
   (since . "0.97.0")))

 (steering
  (steering-gentle-threshold
   (type . number)
   (default . 0.7)
   (description . "Token usage threshold for gentle steering warning")
   (since . "0.40.0"))
  (steering-strong-threshold
   (type . number)
   (default . 0.85)
   (description . "Token usage threshold for strong steering warning")
   (since . "0.40.0"))
  (steering-hard-cap
   (type . number)
   (default . 0.95)
   (description . "Token usage threshold for hard cap (stops generation)")
   (since . "0.40.0"))
  (steering-same-file-dedup
   (type . boolean)
   (default . #f)
   (description . "Deduplicate reads from the same file in context")
   (since . "0.40.0")))

 (security
  (sandbox-enabled
   (type . boolean)
   (default . #f)
   (description . "Enable sandboxed command execution")
   (since . "0.20.0"))
  (sandbox-timeout
   (type . number)
   (default . 120)
   (description . "Sandbox execution timeout in seconds")
   (since . "0.20.0"))
  (sandbox-memory-limit
   (type . number)
   (default . 512)
   (description . "Sandbox memory limit in MB")
   (since . "0.20.0"))
  (sandbox-max-output
   (type . number)
   (default . 100000)
   (description . "Maximum output bytes from sandboxed commands")
   (since . "0.20.0"))
  (sandbox-max-processes
   (type . number)
   (default . 4)
   (description . "Maximum concurrent sandboxed processes")
   (since . "0.20.0")))

 (mas
  (broker-enabled
   (type . boolean)
   (default . #f)
   (description . "Master gate for remote execution via TCP broker. If false, no TCP listeners are started and all execution is local.")
   (since . "0.99.12"))
  (broker-remote-host
   (type . string)
   (default . "localhost")
   (description . "Hostname or IP of the remote executor node")
   (since . "0.99.12"))
  (broker-remote-port
   (type . exact-positive-integer)
   (default . 8443)
   (description . "TCP port of the remote executor node")
   (since . "0.99.12"))
  (broker-cert-dir
   (type . string)
   (default . "~/.q/certs/")
   (description . "Directory containing CA, client cert, and client key PEM files")
   (since . "0.99.12"))
  (broker-capability-secret
   (type . (or/c string #f))
   (default . #f)
   (description . "HMAC-SHA256 secret for signing capability tokens. Required when broker is enabled.")
   (since . "0.99.12"))))
