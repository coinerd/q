# ADR 0020: Browser Feature Architecture

## Status: Accepted

## Context

The q coding agent needs browser interaction capabilities for local development workflows: checking local apps, capturing screenshots, extracting page content, and performing basic UI interactions.

## Decision

We adopt a **sidecar architecture** with a Racket adapter layer:

1. **Sidecar Process**: Node.js + Playwright runs as a child process, communicating via JSONL over stdin/stdout
2. **Adapter Contract**: Racket defines a `browser-adapter` interface that backends implement (mock for testing, Playwright for real)
3. **Service Layer**: `SecureBrowserService` composes adapter + policy + session management + audit logging
4. **Tool Integration**: 9 primitive tools + 1 composite workflow tool registered in the tool registry

### Why a Sidecar?

- **Isolation**: Browser crashes don't take down the agent
- **Language Ecosystem**: Playwright's Node.js API is the most mature browser automation library
- **Zombie Prevention**: Racket custodians ensure the sidecar process is cleaned up
- **Testability**: Mock adapter allows full testing without Node.js

### Why JSONL over stdin/stdout?

- **Simplicity**: No HTTP server, no port management, no CORS
- **Reliability**: stdin/stdout are always available for child processes
- **Structured**: JSON Lines provides natural request/response framing with UUID correlation
- **Debuggable**: Easy to trace by capturing stdin/stdout

### Security Model

- **Policy-first**: All URLs validated against allow-list before navigation
- **Risk classification**: HIGH-risk tools (click, type, press) require permission
- **Audit trail**: Every browser action emits events
- **Safe mode**: Browser tools disabled when safe mode is active
- **Local-only default**: Only localhost/127.0.0.1 URLs allowed by default

### Adapter Contract

The `browser-adapter` struct wraps 6 functions:
- `open`: Create session + navigate
- `close`: Close session
- `navigate`: Navigate to URL
- `observe`: Extract page state
- `act`: Perform action (click/type/press/scroll/extract/screenshot)
- `screenshot`: Capture screenshot

This contract allows any browser backend to be plugged in without changing the service or tool layers.

## Consequences

- Browser feature is fully testable without Node.js installed (mock adapter)
- Adding new browser backends only requires implementing the adapter contract
- Sidecar process adds a startup cost but provides crash isolation
- JSONL protocol is human-readable for debugging
- Security is enforced at the service layer, not the adapter layer
