# Browser Feature Guide

## Overview

The browser feature allows the q coding agent to interact with web pages — navigate, observe, click, type, extract text, take screenshots, and run health checks on local apps. It is designed for **local development workflows**: checking that a local app renders correctly, verifying text content, capturing screenshots for review, and testing UI interactions.

## Architecture

```
┌──────────────────┐     ┌─────────────────┐     ┌──────────────────────┐
│  Tool Handlers    │────▶│  SecureBrowser   │────▶│  Browser Adapter     │
│  (10 tools)       │     │  Service          │     │  ├─ Mock (testing)   │
│  + 1 workflow     │     │  (policy+session) │     │  └─ Playwright      │
└──────────────────┘     └─────────────────┘     │     (sidecar)         │
                          ┌─────────────────┐     └──────────────────────┘
                          │  Event Bus       │          JSONL IPC
                          │  (audit events)  │     ┌──────────────────────┐
                          └─────────────────┘     │  Node.js Sidecar      │
                                                   │  (Playwright)         │
                                                   └──────────────────────┘
```

### Layer Policy

The `browser/` directory is a **peer of `tools/`** — it imports only from `agent/event` and `util`. It never imports from `runtime/` or `tui/`.

## Tool Reference

### Basic Tools (9)

| Tool | Risk | Description |
|------|------|-------------|
| `browser_open` | LOW | Open URL in a new browser session |
| `browser_observe` | LOW | Extract page state (URL, title, text) |
| `browser_click` | HIGH | Click an element by CSS selector |
| `browser_type` | HIGH | Type text into an element |
| `browser_press` | HIGH | Press a keyboard key |
| `browser_extract` | LOW | Extract specific content from page |
| `browser_screenshot` | LOW | Capture a screenshot (base64) |
| `browser_scroll` | LOW | Scroll the page |
| `browser_close` | LOW | Close a browser session |

### Workflow Tool (1)

| Tool | Risk | Description |
|------|------|-------------|
| `browser_check_local_app` | MEDIUM | Quick health check: open → screenshot → extract → close |

#### browser_check_local_app

Returns a composite result:
```json
{
  "status": "ok",
  "url": "http://localhost:3000",
  "title": "My App",
  "text": "page text content...",
  "console_errors": [],
  "loaded_successfully": true,
  "load_time_ms": 142,
  "page_errors": [],
  "screenshot_mime": "image/png",
  "screenshot_data": "<base64>"
}
```

Parameters:
- `url` (required) — Local app URL
- `timeout_ms` (optional, default 10000) — Navigation timeout
- `selector` (optional) — CSS selector to wait for
- `screenshot` (optional, default true) — Whether to capture screenshot

## Security Model

1. **Policy Engine**: URL validation blocks non-local origins by default (localhost/127.0.0.1 only)
2. **Permission Gates**: HIGH-risk tools (click, type, press) require explicit permission
3. **Safe Mode**: Browser tools are disabled in safe mode
4. **Audit Trail**: All browser actions emit events to the event bus
5. **Session Limits**: Max concurrent sessions and actions per session are configurable

## Configuration

Browser settings are managed via `browser/settings.rkt`:

```racket
(define settings (browser-settings
                  #:max-sessions 3
                  #:max-actions-per-session 100
                  #:allowed-ports '(3000 8080 5173 4200)
                  #:blocked-domains '("evil.com")))
```

## Playwright Sidecar

The real browser backend uses a Node.js sidecar process communicating via JSONL over stdin/stdout.

### Setup

```bash
cd q/sidecars/playwright
npm install
```

### Protocol

Each request is a JSON line:
```json
{"id": "uuid", "type": "navigate", "params": {"url": "http://localhost:3000", "sessionId": "bs-123"}}
```

Response:
```json
{"id": "uuid", "success": true, "data": {"url": "...", "title": "...", ...}}
```

Error:
```json
{"id": "uuid", "success": false, "error": {"code": "timeout", "message": "Navigation timed out"}}
```

### Error Codes

| Code | Category | Description |
|------|----------|-------------|
| `timeout` | timeout | Navigation/action timed out |
| `adapter-error` | adapter-error | Playwright operation failed |
| `sidecar-crash` | sidecar-crash | Sidecar process died |
| `session-not-found` | session-expired | Session ID not found |
| `policy-violation` | policy-violation | URL blocked by policy |

## Troubleshooting

### "no browser service configured"
The browser service must be initialized before using browser tools. Use `make-secure-browser-service` with a mock or Playwright adapter.

### "browser policy blocked: localhost port XXXX not allowed"
Add the port to `browser-settings-allowed-ports`.

### "sidecar-crash"
The Node.js sidecar process terminated. Check that Node.js >= 18 and `npm install` has been run in `sidecars/playwright/`.

### Tests pass but integration tests are skipped
Integration tests require Node.js to be installed. Install Node.js >= 18 to run them.
