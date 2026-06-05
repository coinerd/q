#!/bin/bash
# Generate all doc and blog sub-pages for the q agent website

SITE="/home/user/src/q-agent/q/website"

# Common HTML head + nav for sub-pages (relative to site root)
# $1 = page title, $2 = relative path prefix (e.g., "../" for sub-pages)

generate_head() {
  local title="$1"
  local prefix="$2"
  cat <<HEAD
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>${title} — q Agent</title>
  <style>
    *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
    :root {
      --bg: #0d1117; --bg-card: #161b22; --bg-code: #1c2128;
      --border: #30363d; --text: #e6edf3; --text-dim: #8b949e;
      --accent: #58a6ff; --accent2: #7ee787; --accent3: #d2a8ff;
      --warn: #f0883e; --radius: 12px; --max-w: 1120px;
    }
    html { scroll-behavior: smooth; }
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Helvetica, Arial, sans-serif;
      background: var(--bg); color: var(--text); line-height: 1.7;
      -webkit-font-smoothing: antialiased;
    }
    a { color: var(--accent); text-decoration: none; }
    a:hover { text-decoration: underline; }
    code { background: var(--bg-code); border: 1px solid var(--border); border-radius: 6px; padding: 2px 7px; font-size: 0.88em; font-family: 'SF Mono', 'Fira Code', monospace; }
    pre { background: var(--bg-code); border: 1px solid var(--border); border-radius: var(--radius); padding: 20px; overflow-x: auto; margin: 16px 0; }
    pre code { background: none; border: none; padding: 0; font-size: 0.9em; }
    .container { max-width: var(--max-w); margin: 0 auto; padding: 0 24px; }
    nav { border-bottom: 1px solid var(--border); padding: 14px 0; position: sticky; top: 0; background: var(--bg); z-index: 100; }
    nav .container { display: flex; justify-content: space-between; align-items: center; }
    .logo { font-size: 1.3em; font-weight: 700; color: var(--text); }
    .logo span { color: var(--accent); }
    nav ul { list-style: none; display: flex; gap: 28px; }
    nav a { color: var(--text-dim); font-size: 0.95em; }
    nav a:hover, nav a.active { color: var(--text); text-decoration: none; }
    .page-header { padding: 72px 0 32px; border-bottom: 1px solid var(--border); }
    .page-header h1 { font-size: 2.4em; margin-bottom: 8px; }
    .page-header p { color: var(--text-dim); font-size: 1.1em; max-width: 600px; }
    .breadcrumb { margin-bottom: 16px; }
    .breadcrumb a { color: var(--text-dim); font-size: 0.9em; }
    .breadcrumb span { color: var(--text-dim); margin: 0 6px; }
    article { padding: 48px 0; }
    article h2 { font-size: 1.6em; margin: 32px 0 16px; color: var(--accent2); }
    article h3 { font-size: 1.25em; margin: 24px 0 12px; color: var(--accent3); }
    article p { margin: 12px 0; max-width: 720px; }
    article ul, article ol { margin: 12px 0 12px 24px; max-width: 720px; }
    article li { margin: 6px 0; }
    article blockquote { border-left: 3px solid var(--accent); padding: 8px 16px; margin: 16px 0; color: var(--text-dim); max-width: 720px; }
    .status-badge { display: inline-block; background: rgba(126,231,135,0.15); color: var(--accent2); border-radius: 20px; padding: 4px 12px; font-size: 0.85em; margin-bottom: 16px; }
    .nav-back { display: inline-flex; align-items: center; gap: 6px; color: var(--text-dim); margin-bottom: 24px; font-size: 0.95em; }
    footer { border-top: 1px solid var(--border); padding: 32px 0; text-align: center; color: var(--text-dim); font-size: 0.9em; }
    footer .links { display: flex; gap: 24px; justify-content: center; margin-bottom: 16px; }
    @media (max-width: 768px) { nav ul { gap: 16px; } .page-header { padding: 48px 0 24px; } .page-header h1 { font-size: 1.8em; } }
  </style>
</head>
<body>
<nav>
  <div class="container">
    <a href="${prefix}index.html" class="logo"><span>q</span> agent</a>
    <ul>
      <li><a href="${prefix}index.html">Home</a></li>
      <li><a href="${prefix}docs.html">Docs</a></li>
      <li><a href="${prefix}blog.html">Blog</a></li>
      <li><a href="https://github.com/coinerd/q" target="_blank">GitHub ↗</a></li>
    </ul>
  </div>
</nav>
HEAD
}

generate_footer() {
  cat <<FOOTER

<footer>
  <div class="container">
    <div class="links">
      <a href="https://github.com/coinerd/q" target="_blank">GitHub</a>
      <a href="https://github.com/coinerd/q/blob/main/LICENSE" target="_blank">MIT License</a>
      <a href="https://github.com/coinerd/q/blob/main/CONTRIBUTING.md" target="_blank">Contributing</a>
      <a href="https://github.com/coinerd/q/blob/main/CHANGELOG.md" target="_blank">Changelog</a>
    </div>
    <p>© 2026 q Agent Project · Built with ❤️ and Racket</p>
  </div>
</footer>
</body>
</html>
FOOTER
}

# ═══════════════════════════════════════════════
# DOC SUB-PAGES
# ═══════════════════════════════════════════════

# ── docs/install.html ──
{
  generate_head "Installation Guide" "../"
  cat <<'CONTENT'
<div class="page-header">
  <div class="container">
    <div class="breadcrumb"><a href="../docs.html">Docs</a><span>›</span>Installation Guide</div>
    <h1>📥 Installation Guide</h1>
    <p>Get q installed and running on macOS or Linux in minutes.</p>
  </div>
</div>

<article>
  <div class="container">

    <h2>Prerequisites</h2>
    <ul>
      <li><strong>Racket 8.10+</strong> — q is built on Racket. Install from <a href="https://racket-lang.org/" target="_blank">racket-lang.org</a> or your package manager.</li>
      <li><strong>An LLM provider API key</strong> — OpenAI, Anthropic, or Google Gemini.</li>
      <li><strong>Git</strong> — for cloning the repository.</li>
    </ul>

    <h2>One-Command Install</h2>
    <p>The fastest way to get started:</p>
    <pre><code>bash &lt;(curl -sL https://raw.githubusercontent.com/coinerd/q/main/install.sh)</code></pre>
    <p>This script clones the repo, installs Racket dependencies, and sets up the <code>q</code> command on your PATH.</p>

    <h2>Manual Install</h2>
    <pre><code># Clone the repository
git clone https://github.com/coinerd/q.git
cd q

# Install Racket dependencies
raco pkg install

# Link the q command
raco exe -o /usr/local/bin/q main.rkt</code></pre>

    <h2>Configure Your Provider</h2>
    <p>Run the init wizard to set up your first provider:</p>
    <pre><code>q init</code></pre>
    <p>This walks you through selecting a provider, entering your API key, and choosing defaults.</p>

    <h3>Environment Variables</h3>
    <p>Alternatively, set your API key via environment variable:</p>
    <pre><code># OpenAI
export OPENAI_API_KEY="sk-..."

# Anthropic
export ANTHROPIC_API_KEY="sk-ant-..."

# Google Gemini
export GEMINI_API_KEY="..."</code></pre>

    <h2>Troubleshooting</h2>

    <h3>Racket Not Found</h3>
    <p>Make sure Racket is on your PATH: <code>racket --version</code>. If not, add it:</p>
    <pre><code>export PATH="$PATH:/Applications/Racket v8.10/bin"</code></pre>

    <h3>Permission Denied on Install</h3>
    <p>If the one-command installer fails, try manual install and use <code>sudo</code> for the <code>raco exe</code> step, or install to a user-writable directory.</p>

    <h3>API Key Issues</h3>
    <p>Verify your key works with <code>q config test</code>. Check that the key has the right model access for your chosen provider.</p>

    <div style="margin-top: 48px; padding: 24px; background: var(--bg-card); border: 1px solid var(--border); border-radius: var(--radius);">
      <h3 style="margin-top: 0;">Next Steps</h3>
      <p>Now that q is installed, learn how to use it:</p>
      <ul>
        <li><a href="goal-mode.html">🎯 Goal Mode Guide</a> — autonomous task execution</li>
        <li><a href="providers.html">🔌 Provider Setup</a> — configure your LLM provider</li>
        <li><a href="configuration.html">🔧 Configuration Reference</a> — customize q's behavior</li>
      </ul>
    </div>

  </div>
</article>
CONTENT
  generate_footer
} > "$SITE/docs/install.html"

# ── docs/providers.html ──
{
  generate_head "Provider Setup" "../"
  cat <<'CONTENT'
<div class="page-header">
  <div class="container">
    <div class="breadcrumb"><a href="../docs.html">Docs</a><span>›</span>Provider Setup</div>
    <h1>🔌 Provider Setup</h1>
    <p>Configure OpenAI, Anthropic, Google Gemini, or local LLMs. Swap providers without changing your workflow.</p>
  </div>
</div>

<article>
  <div class="container">

    <h2>Supported Providers</h2>
    <p>q supports multiple LLM providers through a unified provider interface. You can switch providers at any time without changing how you interact with the agent.</p>

    <h3>OpenAI</h3>
    <pre><code># config.json
{
  "provider": "openai",
  "model": "gpt-4o",
  "api_key_env": "OPENAI_API_KEY"
}</code></pre>
    <p>Supports GPT-4o, GPT-4 Turbo, GPT-3.5 Turbo, and all OpenAI models. Streaming is enabled by default.</p>

    <h3>Anthropic</h3>
    <pre><code>{
  "provider": "anthropic",
  "model": "claude-sonnet-4-20250514",
  "api_key_env": "ANTHROPIC_API_KEY"
}</code></pre>
    <p>Supports Claude Opus, Sonnet, and Haiku models. Full streaming support with token-level output.</p>

    <h3>Google Gemini</h3>
    <pre><code>{
  "provider": "gemini",
  "model": "gemini-2.0-flash",
  "api_key_env": "GEMINI_API_KEY"
}</code></pre>
    <p>Supports Gemini Pro and Flash models via the Google AI API.</p>

    <h3>Local / OpenAI-Compatible</h3>
    <pre><code>{
  "provider": "openai-compat",
  "model": "llama3",
  "base_url": "http://localhost:8080/v1",
  "api_key_env": "LOCAL_API_KEY"
}</code></pre>
    <p>Works with any server that exposes an OpenAI-compatible API, including Ollama, LM Studio, vLLM, and text-generation-webui.</p>

    <h2>Provider Abstraction</h2>
    <p>q's provider system (<a href="adr-event-bus.html">ADR-3</a>) uses a common interface so all providers behave consistently:</p>
    <ul>
      <li><strong>Streaming</strong> — all providers stream tokens in real-time</li>
      <li><strong>Tool calls</strong> — structured tool use works uniformly across providers</li>
      <li><strong>Context management</strong> — context window limits are handled automatically</li>
      <li><strong>Error handling</strong> — rate limits, timeouts, and retries are built in</li>
    </ul>

    <h2>Swapping Providers</h2>
    <p>Change providers at runtime:</p>
    <pre><code># Via CLI flag
q --provider anthropic --model claude-sonnet-4-20250514

# Via config.json
q config set provider anthropic
q config set model claude-sonnet-4-20250514</code></pre>
    <p>Your session history, tools, and extensions remain unchanged — only the LLM backend switches.</p>

    <h2>Security</h2>
    <p>API keys are never stored in plain text. See <a href="adr-credential-redaction.html">ADR-9: Credential Redaction</a> for details on how q protects your credentials in logs and error messages.</p>

  </div>
</article>
CONTENT
  generate_footer
} > "$SITE/docs/providers.html"

# ── docs/configuration.html ──
{
  generate_head "Configuration Reference" "../"
  cat <<'CONTENT'
<div class="page-header">
  <div class="container">
    <div class="breadcrumb"><a href="../docs.html">Docs</a><span>›</span>Configuration Reference</div>
    <h1>🔧 Configuration Reference</h1>
    <p>Complete reference for config.json, environment variables, and credential management.</p>
  </div>
</div>

<article>
  <div class="container">

    <h2>config.json</h2>
    <p>The primary configuration file is <code>~/.config/q/config.json</code> (or <code>config.json</code> in the project root). It supports the following fields:</p>

    <pre><code>{
  "provider": "openai",
  "model": "gpt-4o",
  "api_key_env": "OPENAI_API_KEY",
  "max_tokens": 4096,
  "temperature": 0.7,
  "safe_mode": false,
  "sandbox": true,
  "session_dir": "~/.local/share/q/sessions",
  "log_level": "info",
  "streaming": true,
  "context_window": 128000,
  "tools": {
    "enabled": ["bash", "read", "write", "edit", "grep", "glob"],
    "disabled": []
  },
  "extensions": {
    "directory": "~/.config/q/extensions",
    "enabled": []
  }
}</code></pre>

    <h2>Environment Variables</h2>
    <table style="border-collapse: collapse; width: 100%; max-width: 720px; margin: 16px 0;">
      <tr style="border-bottom: 1px solid var(--border);">
        <th style="text-align: left; padding: 8px 12px; color: var(--accent);">Variable</th>
        <th style="text-align: left; padding: 8px 12px; color: var(--accent);">Purpose</th>
      </tr>
      <tr style="border-bottom: 1px solid var(--border);">
        <td style="padding: 8px 12px;"><code>OPENAI_API_KEY</code></td>
        <td style="padding: 8px 12px;">OpenAI API key</td>
      </tr>
      <tr style="border-bottom: 1px solid var(--border);">
        <td style="padding: 8px 12px;"><code>ANTHROPIC_API_KEY</code></td>
        <td style="padding: 8px 12px;">Anthropic API key</td>
      </tr>
      <tr style="border-bottom: 1px solid var(--border);">
        <td style="padding: 8px 12px;"><code>GEMINI_API_KEY</code></td>
        <td style="padding: 8px 12px;">Google Gemini API key</td>
      </tr>
      <tr style="border-bottom: 1px solid var(--border);">
        <td style="padding: 8px 12px;"><code>Q_SESSION_DIR</code></td>
        <td style="padding: 8px 12px;">Override session storage directory</td>
      </tr>
      <tr style="border-bottom: 1px solid var(--border);">
        <td style="padding: 8px 12px;"><code>Q_CONFIG</code></td>
        <td style="padding: 8px 12px;">Path to config.json</td>
      </tr>
      <tr style="border-bottom: 1px solid var(--border);">
        <td style="padding: 8px 12px;"><code>Q_LOG_LEVEL</code></td>
        <td style="padding: 8px 12px;">Log verbosity: debug, info, warn, error</td>
      </tr>
    </table>

    <h2>Safe Mode</h2>
    <p>When <code>safe_mode</code> is enabled, q restricts dangerous operations:</p>
    <ul>
      <li>File writes require explicit confirmation</li>
      <li>Shell commands are sandboxed</li>
      <li>Network access is disabled for tools</li>
      <li>Extensions cannot load native code</li>
    </ul>
    <p>See <a href="adr-sandboxing-boundary.html">ADR-7: Sandboxing Boundary</a> for the full safety model.</p>

    <h2>Tool Configuration</h2>
    <p>Control which built-in tools are available:</p>
    <pre><code>"tools": {
  "enabled": ["bash", "read", "write", "edit", "grep", "glob", "web-search"],
  "disabled": ["bash"]  // explicitly disable
}</code></pre>
    <p>Disabled tools take precedence — if a tool appears in both lists, it will be disabled.</p>

    <h2>Credential Backends</h2>
    <p>q supports multiple credential storage backends:</p>
    <ul>
      <li><strong>Environment variables</strong> — default, keys read from env</li>
      <li><strong>Keychain</strong> — macOS Keychain integration</li>
      <li><strong>Encrypted file</strong> — AES-256 encrypted credential store</li>
      <li><strong>Pass</strong> — integration with the <code>pass</code> password manager</li>
    </ul>

  </div>
</article>
CONTENT
  generate_footer
} > "$SITE/docs/configuration.html"

# ── docs/goal-mode.html ──
{
  generate_head "Goal Mode Guide" "../"
  cat <<'CONTENT'
<div class="page-header">
  <div class="container">
    <div class="breadcrumb"><a href="../docs.html">Docs</a><span>›</span>Goal Mode Guide</div>
    <h1>🎯 Goal Mode Guide</h1>
    <p>Use q's autonomous goal loop to plan, execute, and verify complex tasks end-to-end.</p>
  </div>
</div>

<article>
  <div class="container">

    <h2>What is Goal Mode?</h2>
    <p>Goal Mode is q's autonomous execution engine. Instead of sending individual prompts, you give q a high-level goal and it autonomously plans, executes, and verifies the work using a finite state machine (FSM).</p>

    <h2>The GSD State Machine</h2>
    <p>Goal Mode uses the <strong>Get Stuff Done (GSD)</strong> state machine with five states:</p>
    <pre><code>idle → exploration → planning → implementation → verification → debugging
                                    ↑                                  │
                                    └──────────────────────────────────┘</code></pre>
    <p>See <a href="adr-gsd-state-machine.html">ADR-11: GSD State Machine</a> for the full design rationale.</p>

    <h3>State Descriptions</h3>
    <ul>
      <li><strong>Idle</strong> — Waiting for a goal to be set.</li>
      <li><strong>Exploration</strong> — Reading code, understanding the codebase, gathering context.</li>
      <li><strong>Planning</strong> — Creating a structured plan with phases and deliverables.</li>
      <li><strong>Implementation</strong> — Executing the plan: writing code, editing files.</li>
      <li><strong>Verification</strong> — Running tests, checking outputs, validating results.</li>
      <li><strong>Debugging</strong> — Fixing issues found during verification.</li>
    </ul>

    <h2>Setting a Goal</h2>
    <pre><code># From the command line
q goal "Add input validation to the user registration form"

# From within the TUI
/goal Add input validation to the user registration form</code></pre>

    <h2>Evidence Collection</h2>
    <p>Goal Mode requires evidence of progress at each transition:</p>
    <ul>
      <li><strong>Exploration → Planning</strong>: Must show understanding of relevant files</li>
      <li><strong>Planning → Implementation</strong>: Must produce a structured plan</li>
      <li><strong>Implementation → Verification</strong>: Must list all changes made</li>
      <li><strong>Verification → Complete</strong>: Must show passing test results</li>
    </ul>

    <h2>Goal Criteria</h2>
    <p>When a goal is set, you can specify acceptance criteria:</p>
    <pre><code>q goal --criteria "All tests pass" \
         --criteria "No lint warnings" \
         --criteria "Coverage ≥ 80%" \
         "Refactor the authentication module"</code></pre>

    <h2>Monitoring Progress</h2>
    <p>During goal execution, q emits events that the TUI renders in real-time:</p>
    <ul>
      <li>State transitions are logged</li>
      <li>File reads and writes are tracked</li>
      <li>Test results are displayed</li>
      <li>Conclusions are recorded and summarized</li>
    </ul>

    <h2>Tips</h2>
    <ul>
      <li>Start with <strong>smaller, well-scoped goals</strong> — the FSM works best with clear boundaries.</li>
      <li>Use <strong>--max-turns</strong> to limit autonomous execution depth.</li>
      <li>Review the <strong>plan before implementation begins</strong> — you can pause and adjust.</li>
      <li>The <strong>debugging state</strong> has a configurable retry limit to prevent infinite loops.</li>
    </ul>

  </div>
</article>
CONTENT
  generate_footer
} > "$SITE/docs/goal-mode.html"

# ── docs/architecture.html ──
{
  generate_head "Architecture Overview" "../"
  cat <<'CONTENT'
<div class="page-header">
  <div class="container">
    <div class="breadcrumb"><a href="../docs.html">Docs</a><span>›</span>Architecture</div>
    <h1>🏛️ Architecture Overview</h1>
    <p>Understand how q works under the hood — the event bus, the agent loop, and the state machine.</p>
  </div>
</div>

<article>
  <div class="container">

    <h2>Core Design Principles</h2>
    <p>q is built on a <strong>Small Trusted Core</strong> (<a href="adr-small-trusted-core.html">ADR-1</a>) — a minimal runtime responsible for exactly three things:</p>
    <ol>
      <li><strong>Message routing</strong> — dispatching between user, LLM, and tools</li>
      <li><strong>Event dispatch</strong> — emitting typed events to all subscribers</li>
      <li><strong>Session lifecycle</strong> — creating, persisting, and closing sessions</li>
    </ol>
    <p>Everything else — tools, LLM providers, UI frontends — is pluggable outside the core.</p>

    <h2>Event Bus</h2>
    <p>The central nervous system of q. Every significant action emits a typed event:</p>
    <pre><code>UserMessage → MessageSent → ToolInvoked → ToolResult → AssistantResponse</code></pre>
    <p>Multiple consumers subscribe independently — the TUI, session logger, extensions, and JSON interface all react to the same events without coupling.</p>
    <p>See <a href="adr-event-bus.html">ADR-3: Event Bus Architecture</a> for the full design.</p>

    <h2>Agent Loop</h2>
    <p>The main loop follows a turn-based architecture:</p>
    <ol>
      <li><strong>Context assembly</strong> — build the prompt with system instructions, history, tools, and extension hooks</li>
      <li><strong>LLM call</strong> — send the context to the provider, stream the response</li>
      <li><strong>Tool execution</strong> — if the LLM requests tool calls, execute them</li>
      <li><strong>Event emission</strong> — emit results to all subscribers</li>
      <li><strong>State update</strong> — record conclusions, update session state</li>
    </ol>
    <p>See <a href="adr-context-manager.html">ADR-12: Context Assembly</a> for how context is built dynamically.</p>

    <h2>GSD State Machine</h2>
    <p>When running in Goal Mode, the agent loop is governed by the GSD FSM:</p>
    <pre><code>idle → exploration → planning → implementation → verification → debugging
                                    ↑                                  │
                                    └──────────────────────────────────┘</code></pre>
    <p>Each state transition requires evidence. See <a href="adr-gsd-state-machine.html">ADR-11</a> for details.</p>

    <h2>Session Storage</h2>
    <p>Sessions are stored as append-only JSONL files (<a href="adr-session-log.html">ADR-2</a>). This makes them:</p>
    <ul>
      <li><strong>Crash-safe</strong> — just read up to the last valid line</li>
      <li><strong>Auditable</strong> — <code>grep</code> or <code>jq</code> work directly</li>
      <li><strong>Replayable</strong> — replay events to reconstruct any state</li>
    </ul>

    <h2>Extension System</h2>
    <p>Extensions hook into the event bus to add custom behavior:</p>
    <ul>
      <li><strong>Pre/post hooks</strong> — intercept messages, tool calls, and responses</li>
      <li><strong>Custom tools</strong> — add new capabilities to the agent</li>
      <li><strong>Event handlers</strong> — react to any event type</li>
    </ul>
    <p>See <a href="adr-extension-hook-model.html">ADR-6: Extension Hook Model</a> for the full extension API.</p>

    <h2>Security Model</h2>
    <p>Security is enforced at the architecture level:</p>
    <ul>
      <li><strong>Sandboxing</strong> — shell commands run in restricted environments (<a href="adr-sandboxing-boundary.html">ADR-7</a>)</li>
      <li><strong>Credential redaction</strong> — API keys are never logged (<a href="adr-credential-redaction.html">ADR-9</a>)</li>
      <li><strong>Interface separation</strong> — untrusted code never touches the core (<a href="adr-interface-separation.html">ADR-5</a>)</li>
    </ul>

  </div>
</article>
CONTENT
  generate_footer
} > "$SITE/docs/architecture.html"

# ── docs/tutorials.html ──
{
  generate_head "Tutorials & Interface Modes" "../"
  cat <<'CONTENT'
<div class="page-header">
  <div class="container">
    <div class="breadcrumb"><a href="../docs.html">Docs</a><span>›</span>Tutorials</div>
    <h1>🖥️ Tutorials &amp; Interface Modes</h1>
    <p>Use q via CLI for scripts, TUI for interactive work, JSON for machines, or RPC for integration.</p>
  </div>
</div>

<article>
  <div class="container">

    <h2>CLI Mode</h2>
    <p>The simplest way to use q — pipe prompts in, get responses out:</p>
    <pre><code># Interactive chat
q chat

# One-shot prompt
q prompt "Explain this function: $(cat main.rkt)"

# Run a goal
q goal "Add error handling to all public functions"</code></pre>

    <h3>CLI Reference</h3>
    <table style="border-collapse: collapse; width: 100%; max-width: 720px; margin: 16px 0;">
      <tr style="border-bottom: 1px solid var(--border);">
        <th style="text-align: left; padding: 8px 12px; color: var(--accent);">Command</th>
        <th style="text-align: left; padding: 8px 12px; color: var(--accent);">Description</th>
      </tr>
      <tr style="border-bottom: 1px solid var(--border);"><td style="padding: 8px 12px;"><code>q chat</code></td><td style="padding: 8px 12px;">Start interactive chat</td></tr>
      <tr style="border-bottom: 1px solid var(--border);"><td style="padding: 8px 12px;"><code>q prompt &lt;text&gt;</code></td><td style="padding: 8px 12px;">Send a single prompt</td></tr>
      <tr style="border-bottom: 1px solid var(--border);"><td style="padding: 8px 12px;"><code>q goal &lt;text&gt;</code></td><td style="padding: 8px 12px;">Start autonomous goal mode</td></tr>
      <tr style="border-bottom: 1px solid var(--border);"><td style="padding: 8px 12px;"><code>q init</code></td><td style="padding: 8px 12px;">Run setup wizard</td></tr>
      <tr style="border-bottom: 1px solid var(--border);"><td style="padding: 8px 12px;"><code>q config</code></td><td style="padding: 8px 12px;">View/edit configuration</td></tr>
      <tr style="border-bottom: 1px solid var(--border);"><td style="padding: 8px 12px;"><code>q inspect &lt;session&gt;</code></td><td style="padding: 8px 12px;">Inspect a past session</td></tr>
      <tr style="border-bottom: 1px solid var(--border);"><td style="padding: 8px 12px;"><code>q replay &lt;session&gt;</code></td><td style="padding: 8px 12px;">Replay a session</td></tr>
      <tr style="border-bottom: 1px solid var(--border);"><td style="padding: 8px 12px;"><code>q export &lt;session&gt;</code></td><td style="padding: 8px 12px;">Export session as markdown</td></tr>
    </table>

    <h2>TUI Mode</h2>
    <p>The terminal user interface provides a rich, interactive experience:</p>
    <pre><code>q tui</code></pre>
    <p>Features include:</p>
    <ul>
      <li>Syntax-highlighted code output</li>
      <li>Real-time streaming display</li>
      <li>File diff previews</li>
      <li>Goal mode progress visualization</li>
      <li>Session history browser</li>
    </ul>

    <h2>JSON Mode</h2>
    <p>For machine-to-machine communication:</p>
    <pre><code>echo '{"prompt": "List all TODO comments"}' | q json</code></pre>
    <p>Returns structured JSON with the response, tool calls, and events. Ideal for IDE extensions and CI pipelines.</p>

    <h2>RPC / SDK</h2>
    <p>Embed q in your own Racket applications:</p>
    <pre><code>#lang racket
(require q/sdk)

(define agent (make-agent #:provider "openai" #:model "gpt-4o"))
(define response (agent-ask agent "What does this project do?"))
(displayln (response-text response))</code></pre>

  </div>
</article>
CONTENT
  generate_footer
} > "$SITE/docs/tutorials.html"

# ── docs/extension-guide.html ──
{
  generate_head "Extension Guide" "../"
  cat <<'CONTENT'
<div class="page-header">
  <div class="container">
    <div class="breadcrumb"><a href="../docs.html">Docs</a><span>›</span>Extension Guide</div>
    <h1>🧩 Extension Guide</h1>
    <p>Extend q with custom tools, event handlers, and hooks.</p>
  </div>
</div>

<article>
  <div class="container">

    <h2>Extension Architecture</h2>
    <p>Extensions run outside the trusted core and interact with the agent through the event bus. They can:</p>
    <ul>
      <li>Subscribe to events and react to agent actions</li>
      <li>Register custom tools that the LLM can invoke</li>
      <li>Hook into the message pipeline (pre/post processing)</li>
      <li>Add custom context to prompts</li>
    </ul>
    <p>See <a href="adr-extension-hook-model.html">ADR-6: Extension Hook Model</a> for the design rationale.</p>

    <h2>Creating an Extension</h2>
    <p>An extension is a Racket module that exports a registration function:</p>
    <pre><code>#lang racket
(provide register-extension)

(define (register-extension event-bus)
  ;; Subscribe to events
  (event-bus-subscribe event-bus 'tool-invoked
    (λ (event)
      (printf "Tool called: ~a~n" (event-data event))))

  ;; Register a custom tool
  (event-bus-register-tool event-bus
    'my-tool
    "Does something custom"
    (λ (args)
      (format "Result: ~a" args))))</code></pre>

    <h2>Hook Points</h2>
    <p>Extensions can hook into specific points in the agent loop:</p>
    <ul>
      <li><strong>pre-message</strong> — modify the user message before sending to the LLM</li>
      <li><strong>post-response</strong> — process the LLM response before displaying</li>
      <li><strong>pre-tool</strong> — intercept tool calls before execution</li>
      <li><strong>post-tool</strong> — process tool results after execution</li>
      <li><strong>context-assembly</strong> — inject additional context into the prompt</li>
    </ul>

    <h2>Security Boundaries</h2>
    <p>Extensions are sandboxed and cannot:</p>
    <ul>
      <li>Access the core event bus directly (only through the registered interface)</li>
      <li>Modify session files</li>
      <li>Bypass credential redaction</li>
      <li>Load native code without explicit user approval</li>
    </ul>
    <p>See <a href="adr-sandboxing-boundary.html">ADR-7: Sandboxing Boundary</a> for details.</p>

  </div>
</article>
CONTENT
  generate_footer
} > "$SITE/docs/extension-guide.html"

# ── docs/sdk-guide.html ──
{
  generate_head "SDK Guide" "../"
  cat <<'CONTENT'
<div class="page-header">
  <div class="container">
    <div class="breadcrumb"><a href="../docs.html">Docs</a><span>›</span>SDK Guide</div>
    <h1>📦 SDK Guide</h1>
    <p>Embed q in your own Racket applications with the SDK interface.</p>
  </div>
</div>

<article>
  <div class="container">

    <h2>Getting Started</h2>
    <pre><code>#lang racket
(require q/sdk)

;; Create an agent with default configuration
(define agent (make-agent))

;; Send a prompt and get a response
(define response (agent-ask agent "Explain the event bus architecture"))
(displayln (response-text response))

;; List available tools
(for ([tool (agent-tools agent)])
  (printf "~a: ~a~n" (tool-name tool) (tool-description tool)))</code></pre>

    <h2>Provider Configuration</h2>
    <pre><code>;; Use a specific provider
(define agent (make-agent
  #:provider "anthropic"
  #:model "claude-sonnet-4-20250514"
  #:api-key (getenv "ANTHROPIC_API_KEY")))

;; Override temperature
(agent-set-parameter! agent 'temperature 0.5)</code></pre>

    <h2>Event Handling</h2>
    <pre><code>;; Subscribe to streaming events
(agent-on-token agent
  (λ (token)
    (display token)
    (flush-output)))

;; Subscribe to tool calls
(agent-on-tool-call agent
  (λ (call)
    (printf "Calling tool: ~a(~a)~n"
            (tool-call-name call)
            (tool-call-arguments call))))</code></pre>

    <h2>Session Management</h2>
    <pre><code>;; Create a named session
(define agent (make-agent #:session "my-project-session"))

;; Resume an existing session
(define agent (resume-agent "my-project-session"))

;; Export session
(agent-export agent "session-export.md")</code></pre>

    <h2>Goal Mode via SDK</h2>
    <pre><code>;; Start an autonomous goal
(define goal-result
  (agent-goal agent "Add error handling to all public functions"
    #:max-turns 10
    #:criteria '("All tests pass" "No lint warnings")))

;; Check if goal was achieved
(if (goal-achieved? goal-result)
  (printf "Goal achieved in ~a turns~n" (goal-turns goal-result))
  (printf "Goal failed: ~a~n" (goal-reason goal-result)))</code></pre>

  </div>
</article>
CONTENT
  generate_footer
} > "$SITE/docs/sdk-guide.html"

# ── docs/security-trust-model.html ──
{
  generate_head "Security & Trust Model" "../"
  cat <<'CONTENT'
<div class="page-header">
  <div class="container">
    <div class="breadcrumb"><a href="../docs.html">Docs</a><span>›</span>Security &amp; Trust Model</div>
    <h1>🔒 Security &amp; Trust Model</h1>
    <p>How q keeps your code, credentials, and system safe.</p>
  </div>
</div>

<article>
  <div class="container">

    <h2>Small Trusted Core</h2>
    <p>Security starts with the architecture. The q core is minimal — only message routing, event dispatch, and session lifecycle. Everything else runs outside the trusted boundary.</p>
    <p>See <a href="adr-small-trusted-core.html">ADR-1: Small Trusted Core</a>.</p>

    <h2>Sandboxing</h2>
    <p>Tool execution is sandboxed by default:</p>
    <ul>
      <li><strong>Shell commands</strong> run in a restricted environment with configurable allowlists</li>
      <li><strong>File access</strong> is scoped to the project directory by default</li>
      <li><strong>Network access</strong> from tools is disabled unless explicitly enabled</li>
      <li><strong>Resource limits</strong> prevent runaway processes</li>
    </ul>
    <p>See <a href="adr-sandboxing-boundary.html">ADR-7: Sandboxing Boundary</a>.</p>

    <h2>Credential Protection</h2>
    <p>API keys and secrets are protected at every level:</p>
    <ul>
      <li><strong>Never logged</strong> — credentials are redacted from all log output and events</li>
      <li><strong>Never stored in config</strong> — keys are referenced by environment variable name, not value</li>
      <li><strong>Encrypted at rest</strong> — when using the encrypted credential backend</li>
      <li><strong>Session-safe</strong> — keys never appear in session JSONL files</li>
    </ul>
    <p>See <a href="adr-credential-redaction.html">ADR-9: Credential Redaction</a>.</p>

    <h2>Interface Separation</h2>
    <p>The core never trusts data from extensions, tools, or UI frontends without validation:</p>
    <ul>
      <li>All inter-component communication goes through typed interfaces</li>
      <li>Extensions cannot modify core state directly</li>
      <li>The event bus validates event shapes before dispatch</li>
    </ul>
    <p>See <a href="adr-interface-separation.html">ADR-5: Interface Separation</a>.</p>

    <h2>Safe Mode</h2>
    <p>Enable safe mode for maximum protection:</p>
    <pre><code>q --safe-mode chat</code></pre>
    <p>Safe mode adds confirmation prompts for all destructive operations: file writes, shell commands, and network requests.</p>

    <h2>Session Integrity</h2>
    <p>Sessions use an append-only JSONL format that is:</p>
    <ul>
      <li><strong>Crash-safe</strong> — partially written lines are discarded on recovery</li>
      <li><strong>Tamper-evident</strong> — each event includes a sequence number</li>
      <li><strong>Replayable</strong> — sessions can be replayed to verify exactly what happened</li>
    </ul>

  </div>
</article>
CONTENT
  generate_footer
} > "$SITE/docs/security-trust-model.html"

echo "=== Doc sub-pages created ==="
ls -la "$SITE/docs/"
