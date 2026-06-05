#!/bin/bash
# Generate ADR sub-pages and blog posts

SITE="/home/user/src/q-agent/q/website"

# Common HTML generator (same as before)
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
    .meta { color: var(--text-dim); font-size: 0.9em; margin-bottom: 24px; display: flex; gap: 16px; flex-wrap: wrap; }
    .meta-item { background: var(--bg-code); border: 1px solid var(--border); border-radius: 6px; padding: 4px 10px; }
    footer { border-top: 1px solid var(--border); padding: 32px 0; text-align: center; color: var(--text-dim); font-size: 0.9em; }
    footer .links { display: flex; gap: 24px; justify-content: center; margin-bottom: 16px; }
    @media (max-width: 768px) { nav ul { gap: 16px; } .page-header { padding: 48px 0 24px; } .page-header h1 { font-size: 1.8em; } }
    /* Blog styles */
    .blog-post { border-bottom: 1px solid var(--border); padding: 40px 0; }
    .blog-post:last-child { border-bottom: none; }
    .blog-date { color: var(--text-dim); font-size: 0.85em; margin-bottom: 8px; }
    .blog-excerpt { color: var(--text-dim); font-size: 1em; max-width: 600px; }
    .blog-tag { display: inline-block; background: rgba(88,166,255,0.15); color: var(--accent); border-radius: 20px; padding: 2px 10px; font-size: 0.8em; margin-right: 6px; }
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
# ADR SUB-PAGES
# ═══════════════════════════════════════════════

# ── docs/adr-small-trusted-core.html ──
{
  generate_head "ADR-1: Small Trusted Core" "../"
  cat <<'CONTENT'
<div class="page-header">
  <div class="container">
    <div class="breadcrumb"><a href="../docs.html">Docs</a><span>›</span><a href="architecture.html">Architecture</a><span>›</span>ADR-1</div>
    <h1>🏗️ ADR-1: Small Trusted Core</h1>
    <p>The foundational principle: minimal dependencies, no interface coupling, security at the architecture level.</p>
  </div>
</div>

<article>
  <div class="container">
    <div class="meta">
      <span class="status-badge">✅ Accepted</span>
    </div>

    <h2>Context</h2>
    <p>Coding agents operate with significant power — they read files, execute commands, and modify source code. This power demands strong safety guarantees. A large monolithic core is difficult to audit: the more code that runs in the trusted path, the harder it is to reason about correctness and security.</p>

    <h2>Decision</h2>
    <p>q adopts a minimal trusted core with well-defined extension points. The core is responsible for exactly three things:</p>
    <ol>
      <li><strong>Message routing</strong> — dispatching requests between the user, the LLM, and tools.</li>
      <li><strong>Event dispatch</strong> — emitting typed events to all subscribers.</li>
      <li><strong>Session lifecycle</strong> — creating, persisting, and closing sessions.</li>
    </ol>
    <p>Everything else — tools, LLM providers, UI frontends — is pluggable outside the core.</p>

    <h2>Consequences</h2>
    <h3>Easier</h3>
    <ul>
      <li>Auditing core behavior</li>
      <li>Reasoning about safety-critical paths</li>
      <li>Testing the core in isolation</li>
    </ul>
    <h3>Harder</h3>
    <ul>
      <li>Extension quality varies since extensions are not part of the trusted core</li>
      <li>The boundary between core and extensions must be clearly documented and enforced</li>
    </ul>

    <div style="margin-top: 48px; padding: 24px; background: var(--bg-card); border: 1px solid var(--border); border-radius: var(--radius);">
      <h3 style="margin-top: 0;">Related</h3>
      <ul>
        <li><a href="adr-interface-separation.html">ADR-5: Interface Separation</a></li>
        <li><a href="adr-sandboxing-boundary.html">ADR-7: Sandboxing Boundary</a></li>
        <li><a href="adr-credential-redaction.html">ADR-9: Credential Redaction</a></li>
        <li><a href="security-trust-model.html">Security &amp; Trust Model</a></li>
      </ul>
    </div>
  </div>
</article>
CONTENT
  generate_footer
} > "$SITE/docs/adr-small-trusted-core.html"

# ── docs/adr-session-log.html ──
{
  generate_head "ADR-2: Append-Only JSONL Session Log" "../"
  cat <<'CONTENT'
<div class="page-header">
  <div class="container">
    <div class="breadcrumb"><a href="../docs.html">Docs</a><span>›</span><a href="architecture.html">Architecture</a><span>›</span>ADR-2</div>
    <h1>📜 ADR-2: Append-Only JSONL Session Log</h1>
    <p>Crash-safe, auditable session storage using append-only JSON lines.</p>
  </div>
</div>

<article>
  <div class="container">
    <div class="meta">
      <span class="status-badge">✅ Accepted</span>
    </div>

    <h2>Context</h2>
    <p>q needs durable session history that survives crashes and can be replayed for debugging or auditing. Traditional database-backed session stores add complexity and failure modes. Mutable log formats make crash recovery fragile — partially written records can corrupt the entire log.</p>

    <h2>Decision</h2>
    <p>Sessions are stored as append-only JSONL files. Each agent event (user message, assistant response, tool call, tool result, system event) is serialized as a single JSON object on one line. No record is ever mutated in place.</p>
    <p>Session files are named by session ID and written to a configurable directory.</p>

    <h2>Consequences</h2>
    <h3>Easier</h3>
    <ul>
      <li><strong>Crash recovery</strong> — just read up to the last valid line</li>
      <li><strong>Auditing</strong> — <code>grep</code> or <code>jq</code> work directly on session files</li>
      <li><strong>Replay</strong> — replay events in order to reconstruct state</li>
      <li><strong>Implementation</strong> — no transaction or locking logic needed</li>
    </ul>
    <h3>Harder</h3>
    <ul>
      <li>Files grow without bound; a compaction mechanism is required for long-running sessions</li>
      <li>Random access to a specific event requires scanning from the start or maintaining a separate index</li>
    </ul>

    <div style="margin-top: 48px; padding: 24px; background: var(--bg-card); border: 1px solid var(--border); border-radius: var(--radius);">
      <h3 style="margin-top: 0;">Related</h3>
      <ul>
        <li><a href="adr-event-bus.html">ADR-3: Event Bus Architecture</a></li>
        <li><a href="architecture.html">Architecture Overview</a></li>
      </ul>
    </div>
  </div>
</article>
CONTENT
  generate_footer
} > "$SITE/docs/adr-session-log.html"

# ── docs/adr-event-bus.html ──
{
  generate_head "ADR-3: Event Bus Architecture" "../"
  cat <<'CONTENT'
<div class="page-header">
  <div class="container">
    <div class="breadcrumb"><a href="../docs.html">Docs</a><span>›</span><a href="architecture.html">Architecture</a><span>›</span>ADR-3</div>
    <h1>📡 ADR-3: Event Bus Architecture</h1>
    <p>Deterministic event bus that decouples the agent loop from interfaces, enabling streaming and audit logging.</p>
  </div>
</div>

<article>
  <div class="container">
    <div class="meta">
      <span class="status-badge">✅ Accepted</span>
    </div>

    <h2>Context</h2>
    <p>Multiple consumers need to react to agent events: the TUI updates its display, the session logger persists events, extensions run hooks, and the JSON mode interface streams output. Without a central dispatch mechanism, producers become coupled to every consumer, making the system brittle and hard to extend.</p>

    <h2>Decision</h2>
    <p>q uses a central event bus with publish/subscribe semantics. All significant agent actions — message sent, tool invoked, session started, error raised — emit typed events to the bus. Consumers subscribe to event types they care about.</p>
    <p>Events are dispatched synchronously in subscription order within a single agent turn, ensuring deterministic behavior for testing and replay.</p>

    <h2>Event Categories</h2>
    <ul>
      <li><strong>Session events</strong> — session start, end, pause, resume</li>
      <li><strong>Message events</strong> — user message, assistant response, system message</li>
      <li><strong>Tool events</strong> — tool invoked, tool result, tool error</li>
      <li><strong>Stream events</strong> — token received, stream start, stream end</li>
      <li><strong>Turn events</strong> — turn start, turn end, context assembled</li>
      <li><strong>Provider events</strong> — LLM call start, response received, error</li>
      <li><strong>Hook events</strong> — pre-hook, post-hook, hook error</li>
      <li><strong>Iteration events</strong> — loop iteration, goal progress</li>
      <li><strong>Context pressure events</strong> — context budget warnings, compaction</li>
    </ul>

    <h2>Consequences</h2>
    <h3>Easier</h3>
    <ul>
      <li>Loose coupling between layers — new consumers can be added without modifying producers</li>
      <li>Testability — events can be captured and asserted</li>
      <li>Extensibility — extensions subscribe to events they need</li>
    </ul>
    <h3>Harder</h3>
    <ul>
      <li>Event ordering across async boundaries requires care</li>
      <li>Debugging event chains can be non-obvious since the flow is implicit</li>
      <li>Subscribers that block or fail must not stall the entire bus</li>
    </ul>

    <div style="margin-top: 48px; padding: 24px; background: var(--bg-card); border: 1px solid var(--border); border-radius: var(--radius);">
      <h3 style="margin-top: 0;">Related</h3>
      <ul>
        <li><a href="adr-session-log.html">ADR-2: Append-Only JSONL Session Log</a></li>
        <li><a href="adr-extension-hook-model.html">ADR-6: Extension Hook Model</a></li>
        <li><a href="extension-guide.html">Extension Guide</a></li>
      </ul>
    </div>
  </div>
</article>
CONTENT
  generate_footer
} > "$SITE/docs/adr-event-bus.html"

# ── docs/adr-interface-separation.html ──
{
  generate_head "ADR-5: Interface Separation" "../"
  cat <<'CONTENT'
<div class="page-header">
  <div class="container">
    <div class="breadcrumb"><a href="../docs.html">Docs</a><span>›</span><a href="architecture.html">Architecture</a><span>›</span>ADR-5</div>
    <h1>🔌 ADR-5: Interface Separation</h1>
    <p>The core is strictly UI-independent. Interfaces are pure event consumers.</p>
  </div>
</div>

<article>
  <div class="container">
    <div class="meta">
      <span class="status-badge">✅ Accepted</span>
    </div>

    <h2>Context</h2>
    <p>q serves multiple user interfaces: an interactive CLI, a terminal UI (TUI), a JSON streaming mode for programmatic access, and an SDK for embedding. Each interface has different display capabilities and interaction patterns. Embedding UI logic in the core agent loop would make it impossible to reuse core logic across interfaces.</p>

    <h2>Decision</h2>
    <p>The core is strictly UI-independent. Interfaces are pure event consumers: they subscribe to the event bus and render events in their own format (terminal markup, JSON lines, API responses). No UI code exists in the core module.</p>
    <p>Shared state (session ID, model name, configuration) is passed explicitly through the session and context, never through global UI variables.</p>

    <h2>Interface Modes</h2>
    <ul>
      <li><strong>CLI</strong> — text input/output, suitable for scripts and pipes</li>
      <li><strong>TUI</strong> — rich terminal display with syntax highlighting and streaming</li>
      <li><strong>JSON</strong> — structured JSON output for programmatic access</li>
      <li><strong>SDK</strong> — Racket library API for embedding in applications</li>
    </ul>

    <h2>Consequences</h2>
    <h3>Easier</h3>
    <ul>
      <li>Each interface evolves independently</li>
      <li>Adding a new interface (e.g., a web API) requires only a new event consumer</li>
      <li>Testing the core without any UI attached is straightforward</li>
    </ul>
    <h3>Harder</h3>
    <ul>
      <li>Interfaces must handle all relevant event types to avoid silent data loss</li>
      <li>The event contract between core and interfaces must be stable and well-documented</li>
    </ul>

    <div style="margin-top: 48px; padding: 24px; background: var(--bg-card); border: 1px solid var(--border); border-radius: var(--radius);">
      <h3 style="margin-top: 0;">Related</h3>
      <ul>
        <li><a href="adr-small-trusted-core.html">ADR-1: Small Trusted Core</a></li>
        <li><a href="adr-event-bus.html">ADR-3: Event Bus Architecture</a></li>
        <li><a href="tutorials.html">Tutorials &amp; Interface Modes</a></li>
      </ul>
    </div>
  </div>
</article>
CONTENT
  generate_footer
} > "$SITE/docs/adr-interface-separation.html"

# ── docs/adr-extension-hook-model.html ──
{
  generate_head "ADR-6: Extension Hook Model" "../"
  cat <<'CONTENT'
<div class="page-header">
  <div class="container">
    <div class="breadcrumb"><a href="../docs.html">Docs</a><span>›</span><a href="architecture.html">Architecture</a><span>›</span>ADR-6</div>
    <h1>🪝 ADR-6: Extension Hook Model</h1>
    <p>Named dispatch points for powerful extensibility without forking the core.</p>
  </div>
</div>

<article>
  <div class="container">
    <div class="meta">
      <span class="status-badge">✅ Accepted</span>
    </div>

    <h2>Context</h2>
    <p>Extensions need well-defined interception points to modify agent behavior — adding context to prompts, validating tool calls, post-processing responses — without modifying core code. A free-form monkey-patching approach makes the system unpredictable and hard to debug.</p>

    <h2>Decision</h2>
    <p>q exposes a hook system with named dispatch points (e.g., <code>pre-tool-call</code>, <code>post-response</code>, <code>session-start</code>). Extensions register handler functions for specific hooks. Each handler returns one of:</p>
    <ul>
      <li><strong>pass</strong> — continue without modification.</li>
      <li><strong>block</strong> — abort the action with an optional error message.</li>
      <li><strong>modify</strong> — transform the data and continue.</li>
    </ul>
    <p>Hooks execute in registration order. The core invokes hooks at clearly documented points in the agent loop.</p>

    <h2>Hook Points</h2>
    <pre><code>pre-message       → Modify user message before sending to LLM
post-response     → Process LLM response before displaying
pre-tool-call     → Intercept tool calls before execution
post-tool-call    → Process tool results after execution
context-assembly  → Inject additional context into the prompt
session-start     → Run setup when a session begins
session-end       → Run cleanup when a session closes</code></pre>

    <h2>Consequences</h2>
    <h3>Easier</h3>
    <ul>
      <li>Powerful extensibility without forking core</li>
      <li>Extensions are self-contained and discoverable</li>
      <li>Hook behavior is auditable</li>
    </ul>
    <h3>Harder</h3>
    <ul>
      <li>Hook ordering matters and can cause subtle bugs if extensions conflict</li>
      <li>Error isolation — ensuring one extension's crash doesn't bring down the agent — requires defensive coding in the hook dispatcher</li>
    </ul>

    <div style="margin-top: 48px; padding: 24px; background: var(--bg-card); border: 1px solid var(--border); border-radius: var(--radius);">
      <h3 style="margin-top: 0;">Related</h3>
      <ul>
        <li><a href="adr-event-bus.html">ADR-3: Event Bus Architecture</a></li>
        <li><a href="adr-sandboxing-boundary.html">ADR-7: Sandboxing Boundary</a></li>
        <li><a href="extension-guide.html">Extension Guide</a></li>
      </ul>
    </div>
  </div>
</article>
CONTENT
  generate_footer
} > "$SITE/docs/adr-extension-hook-model.html"

# ── docs/adr-sandboxing-boundary.html ──
{
  generate_head "ADR-7: Sandboxing Boundary" "../"
  cat <<'CONTENT'
<div class="page-header">
  <div class="container">
    <div class="breadcrumb"><a href="../docs.html">Docs</a><span>›</span><a href="architecture.html">Architecture</a><span>›</span>ADR-7</div>
    <h1>🛡️ ADR-7: Sandboxing Boundary</h1>
    <p>Defense in depth: containing tool execution to reduce the blast radius of unintended actions.</p>
  </div>
</div>

<article>
  <div class="container">
    <div class="meta">
      <span class="status-badge">✅ Accepted</span>
    </div>

    <h2>Context</h2>
    <p>q tools execute arbitrary operations: shell commands, file reads and writes, code evaluation. Without containment, a malicious or misbehaving prompt could instruct the agent to damage the host system. Defense in depth is essential.</p>

    <h2>Decision</h2>
    <p>q enforces a sandboxing boundary around tool execution:</p>
    <ul>
      <li><strong>Racket sandbox</strong> — User-facing code evaluation runs in Racket's built-in sandbox, which restricts available modules and resource limits.</li>
      <li><strong>Subprocess limits</strong> — Shell commands execute through a managed subprocess with configurable timeouts, working directory restrictions, and optional allow/deny lists for commands.</li>
      <li><strong>File scope</strong> — File read/write tools operate within a configurable project root and refuse path traversal outside it.</li>
    </ul>
    <p>The sandbox boundary is enforced at the tool layer, not at the core, keeping the core simple and delegating containment to the appropriate level.</p>

    <h2>Consequences</h2>
    <h3>Easier</h3>
    <ul>
      <li>Defense in depth reduces the blast radius of unintended actions</li>
      <li>Sandbox parameters are configurable per-project</li>
      <li>Tool authors have a clear security contract</li>
    </ul>
    <h3>Harder</h3>
    <ul>
      <li>Sandbox escape is still possible with sufficient effort — this is a mitigation, not a guarantee</li>
      <li>Overly strict sandboxes may break legitimate workflows, requiring careful tuning of permissions</li>
    </ul>

    <div style="margin-top: 48px; padding: 24px; background: var(--bg-card); border: 1px solid var(--border); border-radius: var(--radius);">
      <h3 style="margin-top: 0;">Related</h3>
      <ul>
        <li><a href="adr-small-trusted-core.html">ADR-1: Small Trusted Core</a></li>
        <li><a href="adr-credential-redaction.html">ADR-9: Credential Redaction</a></li>
        <li><a href="security-trust-model.html">Security &amp; Trust Model</a></li>
      </ul>
    </div>
  </div>
</article>
CONTENT
  generate_footer
} > "$SITE/docs/adr-sandboxing-boundary.html"

# ── docs/adr-credential-redaction.html ──
{
  generate_head "ADR-9: Credential Redaction" "../"
  cat <<'CONTENT'
<div class="page-header">
  <div class="container">
    <div class="breadcrumb"><a href="../docs.html">Docs</a><span>›</span><a href="architecture.html">Architecture</a><span>›</span>ADR-9</div>
    <h1>🔐 ADR-9: Credential Redaction</h1>
    <p>API keys are never exposed in logs, error messages, or debug output.</p>
  </div>
</div>

<article>
  <div class="container">
    <div class="meta">
      <span class="status-badge">✅ Accepted</span>
    </div>

    <h2>Context</h2>
    <p>q stores API keys and OAuth tokens for LLM providers. These credentials appear in several places: environment variables, config files, session logs, and in-memory structs. If credential structs use <code>#:transparent</code>, Racket's default printer will display the full API key in error messages, REPL output, and debug logs — a security risk.</p>

    <h2>Decision</h2>
    <p>Credential structs use opaque printing with redaction:</p>
    <ol>
      <li><strong><code>credential</code> struct</strong> — No <code>#:transparent</code>. Custom <code>gen:custom-write</code> masks the API key to <code>****</code>. Implements <code>gen:equal+hash</code> for test comparison without exposing the key in printed output.</li>
      <li><strong><code>redacted-credential</code> struct</strong> — Opaque. Stores only the last-4 chars of the key for display purposes.</li>
      <li><strong>No raw key logging</strong> — The auth store never writes raw keys to session logs or trace files. Only <code>redacted-credential</code> values appear in events.</li>
    </ol>

    <h2>Consequences</h2>
    <h3>Easier</h3>
    <ul>
      <li>API keys are never accidentally exposed in logs, error reports, or debug output</li>
      <li>Tests can still compare credentials via <code>equal?</code></li>
    </ul>
    <h3>Harder</h3>
    <ul>
      <li>Debugging credential issues requires explicit accessor calls rather than printing the struct</li>
      <li>The <code>gen:equal+hash</code> implementation must stay in sync with the struct fields</li>
    </ul>
    <h3>Risks</h3>
    <p>If a future developer adds <code>#:transparent</code> back for "convenience", the protection is lost. A security lint rule (<code>scripts/lint-security.rkt</code>) catches this pattern.</p>

    <div style="margin-top: 48px; padding: 24px; background: var(--bg-card); border: 1px solid var(--border); border-radius: var(--radius);">
      <h3 style="margin-top: 0;">Related</h3>
      <ul>
        <li><a href="adr-sandboxing-boundary.html">ADR-7: Sandboxing Boundary</a></li>
        <li><a href="adr-small-trusted-core.html">ADR-1: Small Trusted Core</a></li>
        <li><a href="security-trust-model.html">Security &amp; Trust Model</a></li>
      </ul>
    </div>
  </div>
</article>
CONTENT
  generate_footer
} > "$SITE/docs/adr-credential-redaction.html"

# ── docs/adr-gsd-state-machine.html ──
{
  generate_head "ADR-11: GSD State Machine" "../"
  cat <<'CONTENT'
<div class="page-header">
  <div class="container">
    <div class="breadcrumb"><a href="../docs.html">Docs</a><span>›</span><a href="architecture.html">Architecture</a><span>›</span>ADR-11</div>
    <h1>🔄 ADR-11: GSD State Machine</h1>
    <p>The finite-state machine powering goal-directed autonomous execution.</p>
  </div>
</div>

<article>
  <div class="container">
    <div class="meta">
      <span class="status-badge">✅ Accepted</span>
      <span class="meta-item">v0.25.3</span>
    </div>

    <h2>Context</h2>
    <p>The GSD (Getting Stuff Done) planning system managed wave state through ad-hoc flag checks and imperative mutations scattered across multiple modules. This made it difficult to reason about valid state transitions, debug wave execution failures, and add new features without regressions.</p>

    <h2>Decision</h2>
    <p>Rewrote the GSD wave executor as an explicit state machine with defined states and validated transitions:</p>
    <pre><code>idle → exploring → plan-written → executing → verifying → idle
                                      ↑                         │
                                      └─────────────────────────┘</code></pre>

    <h3>Key Components</h3>
    <ul>
      <li><strong>Canonical runtime state</strong> — <code>gsd-runtime-state</code> struct replaces hash-table state. All consumers use struct accessors.</li>
      <li><strong>Command result structs</strong> — <code>gsd-command-result</code> with <code>gsd-ok</code>/<code>gsd-err</code> constructors replace ad-hoc responses.</li>
      <li><strong>Unified policy module</strong> — <code>gsd-decide-action</code> centralizes all guard decisions.</li>
      <li><strong>Transaction wrappers</strong> — <code>with-gsd-transaction</code> provides snapshot+rollback semantics.</li>
      <li><strong>Event telemetry</strong> — Stable event names following <code>gsd.&lt;category&gt;.&lt;action&gt;</code> convention with correlation IDs.</li>
      <li><strong>Normalized Plan IR</strong> — Immutable IR between parsing, validation, and execution.</li>
    </ul>

    <h3>Key Files</h3>
    <table style="border-collapse: collapse; width: 100%; max-width: 720px; margin: 16px 0;">
      <tr style="border-bottom: 1px solid var(--border);">
        <th style="text-align: left; padding: 8px 12px; color: var(--accent);">Module</th>
        <th style="text-align: left; padding: 8px 12px; color: var(--accent);">Responsibility</th>
      </tr>
      <tr style="border-bottom: 1px solid var(--border);"><td style="padding: 8px 12px;"><code>state-machine.rkt</code></td><td style="padding: 8px 12px;">State transitions, tool permissions</td></tr>
      <tr style="border-bottom: 1px solid var(--border);"><td style="padding: 8px 12px;"><code>runtime-state-types.rkt</code></td><td style="padding: 8px 12px;">Runtime state struct</td></tr>
      <tr style="border-bottom: 1px solid var(--border);"><td style="padding: 8px 12px;"><code>policy.rkt</code></td><td style="padding: 8px 12px;">Unified policy decisions</td></tr>
      <tr style="border-bottom: 1px solid var(--border);"><td style="padding: 8px 12px;"><code>events.rkt</code></td><td style="padding: 8px 12px;">Event telemetry</td></tr>
      <tr style="border-bottom: 1px solid var(--border);"><td style="padding: 8px 12px;"><code>core.rkt</code></td><td style="padding: 8px 12px;">Command dispatch, transactions</td></tr>
      <tr style="border-bottom: 1px solid var(--border);"><td style="padding: 8px 12px;"><code>plan-types.rkt</code></td><td style="padding: 8px 12px;">Plan/wave types, normalized IR</td></tr>
      <tr style="border-bottom: 1px solid var(--border);"><td style="padding: 8px 12px;"><code>wave-executor.rkt</code></td><td style="padding: 8px 12px;">Wave execution engine</td></tr>
    </table>

    <h2>Consequences</h2>
    <ul>
      <li>All wave state transitions are explicit and validated</li>
      <li>Invalid transitions raise clear errors instead of silently corrupting state</li>
      <li>Policy decisions centralized in one module for consistency</li>
      <li>Event telemetry enables observability without coupling</li>
      <li>Test coverage: 221 GSD tests across policy, state-machine, core, planning, events, fitness</li>
    </ul>

    <div style="margin-top: 48px; padding: 24px; background: var(--bg-card); border: 1px solid var(--border); border-radius: var(--radius);">
      <h3 style="margin-top: 0;">Related</h3>
      <ul>
        <li><a href="adr-context-manager.html">ADR-12: Context Assembly</a></li>
        <li><a href="goal-mode.html">Goal Mode Guide</a></li>
        <li><a href="adr-event-bus.html">ADR-3: Event Bus Architecture</a></li>
      </ul>
    </div>
  </div>
</article>
CONTENT
  generate_footer
} > "$SITE/docs/adr-gsd-state-machine.html"

# ── docs/adr-context-manager.html ──
{
  generate_head "ADR-12: Context Assembly Architecture" "../"
  cat <<'CONTENT'
<div class="page-header">
  <div class="container">
    <div class="breadcrumb"><a href="../docs.html">Docs</a><span>›</span><a href="architecture.html">Architecture</a><span>›</span>ADR-12</div>
    <h1>🧠 ADR-12: Context Assembly Architecture</h1>
    <p>How q dynamically builds context for each turn — system prompts, history, tools, and extension hooks.</p>
  </div>
</div>

<article>
  <div class="container">
    <div class="meta">
      <span class="status-badge">✅ Accepted</span>
      <span class="meta-item">v0.25.3</span>
    </div>

    <h2>Context</h2>
    <p>The context assembly pipeline was split across two modules: <code>context-builder.rkt</code> (production tree-walk) and <code>context-manager.rkt</code> (dead code, never called outside tests). Logic was duplicated: token estimation, first-user pinning, and pair-preserving budget fitting existed in both modules with divergent implementations. LLM summarization in context-manager was a stub that always concatenated.</p>

    <h2>Decision</h2>
    <p>Unified the context assembly into a three-module architecture:</p>
    <ol>
      <li><strong><code>runtime/context-policy.rkt</code></strong> — Shared policy functions: token estimation, first-user pinning, pair-preserving budget fitting.</li>
      <li><strong><code>runtime/context-manager.rkt</code></strong> — Production pipeline: tree-walk → pin → budget → summarize → catalog → reassemble. LLM summarization via compactor's <code>llm-summarize</code>.</li>
      <li><strong><code>runtime/context-builder.rkt</code></strong> — Legacy tree-walk fallback (no summarization).</li>
    </ol>

    <h3>Production Pipeline</h3>
    <pre><code>assemble-context:
  1. Tree-walk: enumerate all context entries
  2. Pin: ensure first user message is always included
  3. Budget: fit messages within token budget (pair-preserving)
  4. Summarize: LLM-summarize excluded entries (when provider available)
  5. Catalog: generate observability metadata
  6. Reassemble: build final message list for the LLM</code></pre>

    <h2>Consequences</h2>
    <ul>
      <li>Single source of truth for token estimation, pinning, and budget fitting</li>
      <li>LLM summarization integrated into the production pipeline</li>
      <li>context-manager is the primary production pipeline (no longer dead code)</li>
      <li>New strategies (e.g., relevance-based inclusion) can be added without touching shared policy</li>
    </ul>

    <div style="margin-top: 48px; padding: 24px; background: var(--bg-card); border: 1px solid var(--border); border-radius: var(--radius);">
      <h3 style="margin-top: 0;">Related</h3>
      <ul>
        <li><a href="adr-gsd-state-machine.html">ADR-11: GSD State Machine</a></li>
        <li><a href="adr-event-bus.html">ADR-3: Event Bus Architecture</a></li>
        <li><a href="architecture.html">Architecture Overview</a></li>
      </ul>
    </div>
  </div>
</article>
CONTENT
  generate_footer
} > "$SITE/docs/adr-context-manager.html"

echo "=== ADR pages created ==="
ls -la "$SITE/docs/"adr-*.html
