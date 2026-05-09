<!-- verified-against: 0.34.9 --># Extension Development Guide

This guide walks you through creating, testing, and activating a custom
extension for q.

## What Are Extensions?

Extensions are Racket modules that plug into q's agent loop to add tools,
slash commands, and event hooks — without modifying core code. They are
discovered and loaded dynamically by `extensions/loader.rkt` and governed
by a tiered capability system (`extensions/tiers.rkt`) with five privilege
levels: **hooks → commands → session → providers → tui**.

Each extension uses the `define-q-extension` macro from
`extensions/define-extension.rkt` to declare its name, version, API version,
and hook implementations.

## Directory Structure

Create a new file under `extensions/`:

```
q/extensions/
├── my-extension.rkt       ← your extension module
└── ...
```

If your extension needs supporting files, use a directory:

```
q/extensions/my-extension/
├── main.rkt               ← entry point (the extension module)
├── helpers.rkt             ← private helpers
└── qpm.json               ← optional manifest for package metadata
```

## Required Exports

Every extension **must** provide a binding called `the-extension` (or a
custom name registered with the loader). Use `define-q-extension`:

```racket
#lang racket/base

(require "define-extension.rkt"
         "context.rkt")

(provide the-extension)

(define-q-extension the-extension
  #:name       "my-extension"
  #:version    "1.0.0"
  #:api-version 1
  #:tier       'commands
  #:hooks
  (hasheq 'register-tools
          (lambda (ctx)
            ;; register tools here
            (void))
          'register-shortcuts
          (lambda (ctx)
            ;; register slash commands here
            (void))))
```

### Hook Points

| Hook | Called when | Purpose |
|------|-------------|---------|
| `register-tools` | Extension loaded | Register tools into the tool registry |
| `register-shortcuts` | Extension loaded | Register slash commands |
| `resources-discover` | Runtime | Contribute dynamic resource paths |

## Registering Tools

Use `dynamic-tools.rkt` to register tools from inside a hook:

```racket
(require "dynamic-tools.rkt"
         "tool-api.rkt")

;; Inside the 'register-tools hook:
(lambda (ctx)
  (define registry (extension-context-tool-registry ctx))
  (register-tool!
   registry
   (make-tool
    #:name      "my-tool"
    #:description "Does something useful"
    #:handler
    (lambda (args)
      ;; args is a hash of parameters
      (hasheq 'status "ok")))))
```

## Registering Slash Commands

Use `ext-commands.rkt`:

```racket
(require "ext-commands.rkt")

;; Inside the 'register-shortcuts hook:
(lambda (ctx)
  (register-command!
   (extension-context-command-registry ctx)
   "/my-command"
   "Description of what /my-command does"
   (lambda (args)
     ;; Handle the command, return a string or hash
     "Command output")))
```

## Testing Your Extension

Place tests alongside other extension tests in `q/tests/`:

```
q/tests/test-my-extension.rkt
```

Use the `rackunit` framework with `test-case` blocks:

```racket
#lang racket

(require rackunit
         "../extensions/my-extension.rkt"
         "../extensions/api.rkt"
         "../extensions/context.rkt")

(define test-ctx
  (make-extension-context
   #:tool-registry (make-tool-registry)
   #:command-registry (make-command-registry)))

(test-case "extension registers its tools"
  ((hash-ref (extension-hooks the-extension) 'register-tools) test-ctx)
  (define tools (registered-tools (extension-context-tool-registry test-ctx)))
  (check-not-empty tools)
  (check-equal? (tool-name (first tools)) "my-tool"))

(test-case "extension registers slash commands"
  ((hash-ref (extension-hooks the-extension) 'register-shortcuts) test-ctx)
  (define cmds (registered-commands (extension-context-command-registry test-ctx)))
  (check-not-empty cmds))
```

For more complex scenarios, use the test harness in
`extensions/test-harness.rkt` which provides mock contexts and tool registries.

Run your tests:

```bash
raco test tests/test-my-extension.rkt
```

Or the full suite:

```bash
racket scripts/run-tests.rkt
```

## Activating in Settings

Extensions are activated by listing them in `~/.q/config.json`:

```json
{
  "extensions": {
    "enabled": ["my-extension"]
  }
}
```

When q starts, the extension loader discovers all `.rkt` files in
`extensions/`, validates their manifests (if present), checks API version
compatibility, and loads only the enabled extensions.

You can also enable all discovered extensions:

```json
{
  "extensions": {
    "enabled": "*"
  }
}
```

### Tier Permissions

The `tier` field in your extension definition controls what capabilities it
can access:

| Tier | Capabilities |
|------|-------------|
| `hooks` | Register hooks and tools only |
| `commands` | Hooks + slash commands |
| `session` | Session read/write access |
| `providers` | Custom LLM provider registration |
| `tui` | Full TUI surface access |

Start with the lowest tier your extension needs. Requesting a higher tier
than necessary will fail the load.

## Next Steps

- Browse the existing extensions in `q/extensions/` for real-world examples
- Read `docs/style-guide.md` for code style conventions
- See `extensions/EXTENSIONS_INVENTORY.md` for the full extension catalog
