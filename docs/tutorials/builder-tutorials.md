# Builder Tutorials: Tools, Providers, and Extensions

Learn how to extend q by building custom tools, provider adapters, and extensions.

---

## Table of Contents

1. [Introduction](#introduction)
2. [Tutorial 1: Building a Custom Tool](#tutorial-1-building-a-custom-tool)
3. [Tutorial 2: Building a Provider Adapter](#tutorial-2-building-a-provider-adapter)
4. [Tutorial 3: Building an Extension](#tutorial-3-building-an-extension)
5. [Packaging and Sharing](#packaging-and-sharing)
6. [Testing Your Extensions](#testing-your-extensions)

---

## Introduction

q is designed for extensibility at three levels:

| Layer | What you extend | When to use |
|-------|----------------|-------------|
| **Tools** | Agent capabilities | Add new actions (file ops, API calls, calculations) |
| **Providers** | LLM backends | Connect to new AI services or local models |
| **Extensions** | Agent lifecycle | Hook into events, enforce policies, inject context |

All three follow the same pattern: define a struct, implement a contract, register it.

---

## Tutorial 1: Building a Custom Tool

Tools are the agent's actions — read files, run commands, search code. You can add your own.

### Tool Anatomy

A tool has four parts:

| Field | Type | Purpose |
|-------|------|---------|
| `name` | string | Unique identifier (e.g. `"weather"`) |
| `description` | string | What the tool does — the LLM reads this |
| `schema` | hash | JSON Schema describing parameters |
| `execute` | procedure | The function that runs when the tool is called |

### Step 1: Define the Schema

The schema tells the LLM what parameters to provide:

```racket
(define weather-schema
  (hasheq 'type "object"
          'properties
          (hasheq 'city (hasheq 'type "string"
                                'description "City name")
                  'units (hasheq 'type "string"
                                 'description "Temperature units: celsius or fahrenheit"
                                 'enum '("celsius" "fahrenheit")))
          'required '("city")))
```

### Step 2: Implement the Handler

The handler receives two arguments: a params hash and an execution context:

```racket
(define (weather-execute params ctx)
  (define city (hash-ref params 'city))
  (define units (hash-ref params 'units "celsius"))

  ;; In a real tool, call an API here.
  ;; For this tutorial, return mock data.
  (define temp (if (equal? units "fahrenheit") 72 22))

  (make-success-result
   (format "Weather in ~a: ~a°~a, sunny"
           city
           temp
           (if (equal? units "fahrenheit") "F" "C"))))
```

### Step 3: Create and Register the Tool

```racket
#lang racket/base

(require q/tools/tool)

;; ... schema and handler from above ...

(define weather-tool
  (make-tool "weather"
             "Get current weather for a city. Returns temperature and conditions."
             weather-schema
             weather-execute))

;; Register with the tool registry
(register-tool! (tool-registry) weather-tool)
```

### Complete Example

```racket
#lang racket/base

;; tools/weather.rkt — example custom tool

(require q/tools/tool
         racket/contract)

;; ── Schema ──

(define weather-schema
  (hasheq 'type "object"
          'properties
          (hasheq 'city (hasheq 'type "string"
                                'description "City name")
                  'units (hasheq 'type "string"
                                 'description "celsius or fahrenheit"
                                 'enum '("celsius" "fahrenheit")))
          'required '("city")))

;; ── Handler ──

(define (weather-execute params ctx)
  (define city (hash-ref params 'city))
  (define units (hash-ref params 'units "celsius"))

  ;; Mock data — replace with real API call
  (define temp (if (equal? units "fahrenheit") 72 22))
  (define conditions "sunny")

  (make-success-result
   (hasheq 'city city
           'temperature temp
           'units units
           'conditions conditions)))

;; ── Register ──

(define weather-tool
  (make-tool "weather"
             "Get current weather for a city."
             weather-schema
             weather-execute))

(provide weather-tool)
```

### Handler Context

The execution context (`ctx`) provides:

| Field | Type | Purpose |
|-------|------|---------|
| `working-directory` | path | Where the agent is running |
| `cancellation-token` | canceller | For aborting long operations |
| `event-publisher` | procedure | Publish events to the event bus |
| `runtime-settings` | hash | Current runtime configuration |
| `call-id` | string | Unique ID for this tool invocation |

### Return Values

Use these helpers to construct results:

```racket
;; Success
(make-success-result "plain text result")
(make-success-result (hasheq 'key "value"))  ; structured

;; Error
(make-error-result "File not found: /path/to/file")
```

---

## Tutorial 2: Building a Provider Adapter

Provider adapters connect q to LLM backends. Each adapter wraps the provider's API into q's standard interface.

### Provider Anatomy

A provider implements a dispatch protocol — a function that maps symbols to implementations:

| Symbol | Returns | Description |
|--------|---------|-------------|
| `'name` | string | Provider identifier |
| `'capabilities` | hash | Feature flags (streaming, tools, etc.) |
| `'send` | procedure | Synchronous request → response |
| `'stream` | procedure | Streaming request → chunk generator |

### Step 1: Define the Adapter

```racket
#lang racket/base

(require q/llm/provider
         q/llm/model
         racket/contract
         json)

;; ── Configuration ──

(define (make-my-provider config)
  (define base-url (hash-ref config 'base-url "http://localhost:8080/v1"))
  (define api-key  (hash-ref config 'api-key ""))

  ;; Return a provider struct
  (provider
   ;; dispatch procedure
   (λ (op . args)
     (case op
       [(name)         "my-provider"]
       [(capabilities) (hasheq 'streaming #t 'tools #t)]
       [(send)         (apply send-request base-url api-key args)]
       [(stream)       (apply stream-request base-url api-key args)]
       [else           (error 'my-provider "unknown op: ~a" op)]))))
```

### Step 2: Implement Send

The `send` operation takes a `model-request` and returns a `model-response`:

```racket
(define (send-request base-url api-key request)
  ;; request is a model-request struct with:
  ;;   - model-request-model
  ;;   - model-request-messages
  ;;   - model-request-tools
  ;;   - model-request-temperature
  ;;   - model-request-max-tokens

  ;; Build HTTP request to your provider's API
  (define response-json
    (call-provider-api base-url api-key
                       (model-request->api-json request)))

  ;; Parse into model-response struct
  (model-response
   (parse-content response-json)
   (parse-tool-calls response-json)
   (parse-usage response-json)
   (model-request-model request)))
```

### Step 3: Implement Streaming

The `stream` operation returns a generator of stream chunks:

```racket
(require racket/generator)

(define (stream-request base-url api-key request)
  ;; Return a generator that yields stream-chunk structs
  (in-generator
   (let ([chunks (call-provider-streaming base-url api-key request)])
     (for ([chunk chunks])
       ;; stream-chunk has: delta (string), done? (boolean)
       (yield (stream-chunk (parse-delta chunk)
                            (parse-done? chunk)))))))
```

### Step 4: Register in Config

Add your provider to `.q/config.json`:

```json
{
  "providers": {
    "my-provider": {
      "base-url": "http://localhost:8080/v1",
      "api-key-env": "MY_PROVIDER_KEY",
      "models": ["my-model-v1"],
      "default-model": "my-model-v1"
    }
  }
}
```

Use it:

```bash
raco q --model my-provider/my-model-v1 "Hello"
```

### Model Types Reference

**model-request** fields:

| Field | Type | Description |
|-------|------|-------------|
| `model` | string | Model identifier |
| `messages` | list | Conversation messages |
| `tools` | list | Available tools (tool schemas) |
| `temperature` | number | Sampling temperature |
| `max-tokens` | integer | Max output tokens |
| `stream?` | boolean | Whether streaming requested |

**model-response** fields:

| Field | Type | Description |
|-------|------|-------------|
| `content` | string or list | Response text or content parts |
| `tool-calls` | list | Tool invocation requests |
| `usage` | hash | Token usage stats |
| `model` | string | Actual model used |

**stream-chunk** fields:

| Field | Type | Description |
|-------|------|-------------|
| `delta` | string | Incremental text |
| `done?` | boolean | True when stream ends |

---

## Tutorial 3: Building an Extension

Extensions hook into the agent's lifecycle to observe, modify, or block actions.

### Extension Anatomy

An extension is a struct with:

| Field | Type | Purpose |
|-------|------|---------|
| `name` | string | Unique identifier |
| `version` | string | Semantic version |
| `api-version` | string | Extension API version (currently `"1"`) |
| `hooks` | hash | Map of hook-point → handler procedure |

### Hook Points

| Hook Point | When it fires | Context includes |
|------------|---------------|------------------|
| `turn-start` | Beginning of each agent turn | Session info, current prompt |
| `turn-end` | End of each agent turn | Full turn data, response |
| `tool-call` | Before a tool executes | Tool name, arguments |
| `tool-result` | After a tool returns | Tool output, duration |
| `context` | When context is built | Messages, system prompt |
| `before-provider-request` | Before LLM call | Model request |

### Handler Return Values

Handlers can return:

| Action | Meaning |
|--------|---------|
| `pass` | Allow the action to proceed |
| `amend` | Modify the data and proceed |
| `block` | Stop the action with a reason |

### Using `define-q-extension`

The easiest way to create an extension is with the `define-q-extension` macro:

```racket
#lang racket/base

;; .q/extensions/audit-log/audit.rkt

(require q/extensions/define-extension
         racket/port)

;; ── Log file path ──

(define log-path (build-path (find-system-path 'home) ".q" "audit.log"))

;; ── Handlers ──

(define (on-tool-call ctx)
  (define tool-name (hash-ref ctx 'tool-name "unknown"))
  (define args (hash-ref ctx 'arguments (hasheq)))
  (log-event (format "TOOL-CALL: ~a ~a" tool-name args))
  ;; Return pass — just observing, not modifying
  (hasheq 'action "pass"))

(define (on-tool-result ctx)
  (define tool-name (hash-ref ctx 'tool-name "unknown"))
  (define result (hash-ref ctx 'result ""))
  (log-event (format "TOOL-RESULT: ~a (length: ~a)"
                      tool-name
                      (string-length (format "~a" result))))
  (hasheq 'action "pass"))

(define (on-turn-start ctx)
  (define turn (hash-ref ctx 'turn-number 0))
  (log-event (format "TURN-START: turn ~a" turn))
  (hasheq 'action "pass"))

;; ── Logging helper ──

(define (log-event msg)
  (define timestamp (current-seconds))
  (with-output-to-file log-path
    (λ ()
      (displayln (format "[~a] ~a" timestamp msg)))
    #:exists 'append))

;; ── Define the extension ──

(define-q-extension audit-log
  #:version "1.0.0"
  #:api-version "1"
  #:on tool-call on-tool-call
  #:on tool-result on-tool-result
  #:on turn-start on-turn-start)
```

### Example: Policy Enforcement Extension

Block dangerous operations and enforce team rules:

```racket
#lang racket/base

;; .q/extensions/team-policy/policy.rkt

(require q/extensions/define-extension
         racket/string)

;; ── Deny list ──

(define denied-commands
  '("rm -rf /" "rm -rf ~" "DROP TABLE" ":(){ :|:& };:" "mkfs"))

(define denied-paths
  '("/etc/passwd" "/etc/shadow" "/boot/"))

;; ── Handlers ──

(define (check-tool-call ctx)
  (define tool-name (hash-ref ctx 'tool-name #f))
  (define args (hash-ref ctx 'arguments (hasheq)))

  (cond
    ;; Check bash commands
    [(equal? tool-name "bash")
     (define cmd (hash-ref args 'command ""))
     (check-bash-command cmd)]

    ;; Check file writes to sensitive paths
    [(or (equal? tool-name "write")
         (equal? tool-name "edit"))
     (define path (hash-ref args 'path ""))
     (check-file-path path)]

    [else
     (hasheq 'action "pass")]))

(define (check-bash-command cmd)
  (for ([denied (in-list denied-commands)])
    (when (string-contains? cmd denied)
      (return-block
       (format "Command blocked by team policy: contains '~a'" denied))))
  (hasheq 'action "pass"))

(define (check-file-path path)
  (for ([denied (in-list denied-paths)])
    (when (string-prefix? path denied)
      (return-block
       (format "Path blocked by team policy: ~a is protected" denied))))
  (hasheq 'action "pass"))

(define (return-block reason)
  (hasheq 'action "block"
          'reason reason))

;; ── Define the extension ──

(define-q-extension team-policy
  #:version "1.0.0"
  #:api-version "1"
  #:on tool-call check-tool-call)
```

### Example: Context Injection Extension

Add project metadata to every agent turn:

```racket
#lang racket/base

;; .q/extensions/project-context/context.rkt

(require q/extensions/define-extension)

(define (inject-context ctx)
  ;; Add build info to the agent's context
  (hasheq 'action "amend"
          'context
          (hasheq 'system-append
                  (format "Project: my-app v2.1.0~nBuild: ~a~nBranch: main"
                          (current-seconds))))

(define-q-extension project-context
  #:version "1.0.0"
  #:api-version "1"
  #:on turn-start inject-context))
```

---

## Packaging and Sharing

### Extension Manifest (`qpm.json`)

Every extension must have a manifest in its root directory:

```json
{
  "name": "audit-log",
  "version": "1.0.0",
  "api-version": "1",
  "type": "extension",
  "description": "Logs all tool calls and turn events to ~/.q/audit.log",
  "author": "your-team",
  "entry": "audit.rkt",
  "files": ["audit.rkt"],
  "compat": ">=0.6.0",
  "homepage": "https://github.com/your-team/q-extensions",
  "license": "MIT"
}
```

### Manifest Fields

| Field | Required | Description |
|-------|----------|-------------|
| `name` | Yes | Unique package name |
| `version` | Yes | Semantic version |
| `api-version` | Yes | Extension API version (`"1"`) |
| `type` | Yes | `"extension"`, `"skill"`, or `"bundle"` |
| `description` | Yes | What the package does |
| `author` | Yes | Author or team name |
| `entry` | No | Main Racket file (defaults to `main.rkt`) |
| `files` | No | List of included files |
| `compat` | No | q version compatibility (e.g. `">=0.6.0"`) |
| `homepage` | No | Project URL |
| `license` | No | License identifier |

### Directory Structure

```
.q/extensions/
└── my-extension/
    ├── qpm.json          # Required manifest
    └── my-extension.rkt  # Extension code
```

For multi-file extensions:

```
.q/extensions/
└── my-complex-extension/
    ├── qpm.json
    ├── main.rkt           # Entry point
    ├── helpers.rkt         # Utilities
    └── templates/
        └── report.md       # Static assets
```

### Sharing Within a Team

Commit the extension to the project repo:

```bash
git add .q/extensions/my-extension/
git commit -m "feat: add my-extension for automated reporting"
```

### Sharing as a Standalone Package

Create a standalone Racket package with the extension, then install it:

```bash
# In the extension's repo
raco pkg install

# Or from a Git URL
raco pkg install --auto https://github.com/your-team/q-extension-foo
```

---

## Testing Your Extensions

### Using the Test Harness

q provides a test harness for extensions in `q/extensions/test-harness.rkt`:

```racket
#lang racket

(require rackunit
         q/extensions/test-harness
         q/extensions/api)

;; Create a test extension
(define test-ext
  (extension "test-ext"
             "1.0.0"
             "1"
             (hasheq 'tool-call
                     (λ (ctx)
                       (hasheq 'action "pass")))))

;; Register and test
(define reg (make-extension-registry))
(register-extension! reg test-ext)

;; Get handlers for a hook point
(define handlers (handlers-for-point reg 'tool-call))
(check-equal? (length handlers) 1)

;; Simulate a hook invocation
(define ctx (hasheq 'tool-name "bash"
                    'arguments (hasheq 'command "ls")))
(define result ((cdar handlers) ctx))
(check-equal? (hash-ref result 'action) "pass")
```

### Testing Tool Execution

```racket
#lang racket

(require rackunit
         q/tools/tool)

;; Define and test a tool
(define my-tool
  (make-tool "test-tool"
             "A test tool"
             (hasheq 'type "object"
                     'properties (hasheq 'x (hasheq 'type "integer"))
                     'required '("x"))
             (λ (params ctx)
               (make-success-result
                (* 2 (hash-ref params 'x))))))

;; Validate schema
(check-not-exn
 (λ () (validate-tool-args my-tool (hasheq 'x 5))))

;; Test execution
(define result (tool-execute my-tool
                             (hasheq 'x 5)
                             (make-exec-context "." #f #f #f "test-id" #f)))
(check-equal? (tool-result-content result) 10)
```

### Running Tests

```bash
# Run all tests
raco test tests/

# Run a specific test file
raco test tests/test-extension-api.rkt
```

---

## Quick Reference

### Tool Checklist

- [ ] Define JSON Schema for parameters
- [ ] Implement handler: `(params ctx) → tool-result`
- [ ] Create with `make-tool`
- [ ] Register with `register-tool!`
- [ ] Write tests using `rackunit`

### Provider Checklist

- [ ] Implement dispatch for `'name`, `'capabilities`, `'send`, `'stream`
- [ ] Parse API responses into `model-response` structs
- [ ] Handle streaming with `stream-chunk` generator
- [ ] Add to config with `api-key-env` (no hardcoded keys)
- [ ] Test with `raco q --model your-provider/model "hello"`

### Extension Checklist

- [ ] Choose hook points
- [ ] Implement handlers returning `pass`, `amend`, or `block`
- [ ] Use `define-q-extension` macro for clean definition
- [ ] Create `qpm.json` manifest
- [ ] Place in `.q/extensions/<name>/`
- [ ] Write tests with `test-harness`

---

## See Also

- [Architecture: Tools](../docs/architecture/tools.md) — Tool system design
- [Architecture: Extensions](../docs/architecture/extensions.md) — Extension hook points
- [Configuration Guide](../docs/getting-started/configuration.md) — Provider configuration
- [Team Setup Guide](team-setup.md) — Team onboarding and shared configs
