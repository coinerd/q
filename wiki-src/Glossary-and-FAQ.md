# Glossary and FAQ

## Glossary

| Term | Definition |
|------|-----------|
| **Agent loop** | The core cycle: send prompt → receive response → execute tools → repeat until done |
| **Session** | A conversation with the LLM, stored as append-only JSONL |
| **Turn** | One iteration of the agent loop (prompt + response + tool calls) |
| **Provider** | An LLM backend adapter (OpenAI, Anthropic, Gemini, or local) |
| **Tool** | A capability exposed to the LLM (bash, read, write, edit, grep, find, ls) |
| **Extension** | A plugin that hooks into the agent lifecycle (tool interception, logging, etc.) |
| **Safe mode** | A restricted mode where dangerous tool calls are blocked or sandboxed |
| **Compaction** | Summarizing old turns to reduce context window usage |
| **JSONL** | JSON Lines format — one JSON object per line, used for session logs |
| **Event bus** | Pub/sub system for agent events (tool calls, responses, errors) |
| **Token budget** | Maximum tokens allowed before compaction triggers |
| **Scheduler** | Manages concurrent tool execution and resource limits |

## FAQ

### What is q?
A local-first, extensible coding agent runtime written in Racket. It runs in your terminal (CLI or TUI) and connects to LLM providers (OpenAI, Anthropic, Gemini, or local models).

### How do I install q?
```bash
git clone https://github.com/coinerd/q.git
cd q
raco pkg install
```
Requires Racket 8.10+. See the [installation guide](https://github.com/coinerd/q/blob/main/q/docs/install.md).

### How do I configure a provider?
Run `q --init` for the interactive wizard, or edit `~/.q/config.json` directly:
```json
{
  "default-model": "gpt-4o",
  "providers": {
    "openai": { "api-key": "sk-..." }
  }
}
```

### How do I start a session?
```bash
q                          # Interactive CLI
q "fix the bug in main.rkt"  # One-shot mode
q --tui                    # Terminal UI
```

### How do I resume a session?
```bash
q --session <session-id>
```
Use `q --sessions` to list available sessions.

### How do I write an extension?
See [[Writing-Extensions]] and the [extension API docs](https://github.com/coinerd/q/blob/main/q/extensions/api.rkt).

### How do I enable safe mode?
```bash
q --safe                   # CLI safe mode
```
Or set in config: `"safe-mode": true`. This sandboxes bash commands and restricts file access.

### Where are sessions stored?
In `~/.q/sessions/` as JSONL files. Each turn is one line.

### Where is the canonical documentation?
In the main repository: [README](https://github.com/coinerd/q/blob/main/README.md), [docs/](https://github.com/coinerd/q/tree/main/docs), and [CONTRIBUTING.md](https://github.com/coinerd/q/blob/main/CONTRIBUTING.md).

### What is this wiki for?
Navigation, onboarding, architecture maps, troubleshooting, and contributor guidance.
