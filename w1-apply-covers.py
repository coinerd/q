#!/usr/bin/env python3
"""W1: insert reviewed @covers tags into pilot-area test files.

Policy (reviewed one-by-one):
- A test file T is tagged for pilot-area module M only when BOTH hold:
  (a) T's own (require ...) actually references M (resolved relative to T's dir)
  (b) T's file stem matches M's file stem exactly after stripping test-/-test,
      OR the pair is in the hand-curated FAMILY list below (area-prefixed names
      like test-gsd-core.rkt -> extensions/gsd/core.rkt).
- Anything ambiguous is left unmapped (selector keeps its safe fallback).
- Existing @covers tags are never modified; new ones are appended after them.
"""
import os, re, sys, collections

ROOT = os.getcwd()
TESTS = os.path.join(ROOT, "tests")
PILOT = ("runtime/", "tools/", "llm/", "extensions/", "tui/")

REQ = re.compile(r'"((?:\.\./)+[^"]+\.rkt)"')

# Hand-curated family pairs (reviewed): test -> module
FAMILY = {
    "tests/test-extension-api.rkt": "extensions/api.rkt",
    "tests/test-hof-combinators.rkt": "extensions/combinators.rkt",
    "tests/test-extension-context.rkt": "extensions/context.rkt",
    "tests/test-ext-dynamic-tools.rkt": "extensions/dynamic-tools.rkt",
    "tests/test-github-issue-ops.rkt": "extensions/github/handlers/issue-ops.rkt",
    "tests/test-github-milestone-ops.rkt": "extensions/github/handlers/milestone-ops.rkt",
    "tests/test-github-helpers.rkt": "extensions/github/helpers.rkt",
    "tests/test-gsd-command-normalization.rkt": "extensions/gsd-planning/command-normalization.rkt",
    "tests/test-gsd-execution-policy.rkt": "extensions/gsd-planning/execution-policy.rkt",
    "tests/test-gsd-plan-diff.rkt": "extensions/gsd-planning/plan-diff.rkt",
    "tests/test-gsd-archive.rkt": "extensions/gsd/archive.rkt",
    "tests/test-gsd-campaign-repository.rkt": "extensions/gsd/campaign-repository.rkt",
    "tests/test-gsd-campaign-state.rkt": "extensions/gsd/campaign-state.rkt",
    "tests/test-gsd-command-parser.rkt": "extensions/gsd/command-parser.rkt",
    "tests/test-gsd-composition-root.rkt": "extensions/gsd/composition-root.rkt",
    "tests/test-gsd-context-bundle.rkt": "extensions/gsd/context-bundle.rkt",
    "tests/test-gsd-core.rkt": "extensions/gsd/core.rkt",
    "tests/test-gsd-delivery-verifier.rkt": "extensions/gsd/delivery-verifier.rkt",
    "tests/test-gsd-effect-ports.rkt": "extensions/gsd/effect-ports.rkt",
    "tests/test-gsd-events.rkt": "extensions/gsd/events.rkt",
    "tests/test-gsd-github-port.rkt": "extensions/gsd/github-port.rkt",
    "tests/test-gsd-go-orchestrator.rkt": "extensions/gsd/go-orchestrator.rkt",
    "tests/test-gsd-plan-types.rkt": "extensions/gsd/plan-types.rkt",
    "tests/test-gsd-plan-validator.rkt": "extensions/gsd/plan-validator.rkt",
    "tests/test-gsd-policy.rkt": "extensions/gsd/policy.rkt",
    "tests/test-gsd-prompts.rkt": "extensions/gsd/prompts.rkt",
    "tests/test-gsd-responsibility-inventory.rkt": "extensions/gsd/responsibility-inventory.rkt",
    "tests/test-gsd-state-machine.rkt": "extensions/gsd/state-machine.rkt",
    "tests/test-gsd-wave-completion.rkt": "extensions/gsd/wave-completion.rkt",
    "tests/test-gsd-wave-docs.rkt": "extensions/gsd/wave-docs.rkt",
    "tests/test-gsd-wave-executor.rkt": "extensions/gsd/wave-executor.rkt",
    "tests/test-gsd-wave-runner-port.rkt": "extensions/gsd/wave-runner-port.rkt",
    "tests/test-gsd-wave-status.rkt": "extensions/gsd/wave-status.rkt",
    "tests/test-extension-loader.rkt": "extensions/loader.rkt",
    "tests/test-racket-tooling-analysis.rkt": "extensions/racket-tooling/analysis.rkt",
    "tests/test-llm-model.rkt": "llm/model.rkt",
    "tests/test-llm-timing.rkt": "llm/timing.rkt",
    "tests/test-approval-broker.rkt": "runtime/approval/broker.rkt",
    "tests/test-context-assembly-budgeting.rkt": "runtime/context-assembly/budgeting.rkt",
    "tests/test-context-assembly-serialization.rkt": "runtime/context-assembly/serialization.rkt",
    "tests/test-task-state-inference.rkt": "runtime/context-assembly/state-inference.rkt",
    "tests/test-context-selection-authority.rkt": "runtime/context-selection/authority.rkt",
    "tests/test-credential-protocol.rkt": "runtime/credentials/protocol.rkt",
    "tests/test-step-directive.rkt": "runtime/iteration/directive.rkt",
    "tests/test-iteration-retry-policy.rkt": "runtime/iteration/retry-policy.rkt",
    "tests/test-memory-auto-extraction.rkt": "runtime/memory/auto-extraction.rkt",
    "tests/test-gap5-conclusion-bridge.rkt": "runtime/memory/conclusion-bridge.rkt",
    "tests/test-memory-migration.rkt": "runtime/memory/migration.rkt",
    "tests/test-memory-protocol.rkt": "runtime/memory/protocol.rkt",
    "tests/test-memory-search.rkt": "runtime/memory/search.rkt",
    "tests/test-memory-service.rkt": "runtime/memory/service.rkt",
    "tests/test-memory-types.rkt": "runtime/memory/types.rkt",
    "tests/test-session-index-query.rkt": "runtime/session-index/query.rkt",
    "tests/test-session-index-schema.rkt": "runtime/session-index/schema.rkt",
    "tests/test-task-ledger-codec.rkt": "runtime/task-memory/codec.rkt",
    "tests/test-task-memory-projection.rkt": "runtime/task-memory/projection.rkt",
    "tests/test-task-ledger-replay.rkt": "runtime/task-memory/replay.rkt",
    "tests/test-task-ledger-types.rkt": "runtime/task-memory/types.rkt",
    "tests/test-tool-delete-lines.rkt": "tools/builtins/delete-lines.rkt",
    "tests/test-tool-edit.rkt": "tools/builtins/edit.rkt",
    "tests/test-tool-read.rkt": "tools/builtins/read.rkt",
    "tests/test-tool-write.rkt": "tools/builtins/write.rkt",
    "tests/test-tool-middleware.rkt": "tools/middleware.rkt",
    "tests/test-tui-builtins.rkt": "tui/builtins.rkt",
    "tests/test-tui-command-parse.rkt": "tui/command-parse.rkt",
    "tests/test-tui-layout.rkt": "tui/layout.rkt",
    "tests/test-tui-renderer.rkt": "tui/renderer.rkt",
    "tests/test-tui-terminal.rkt": "tui/terminal.rkt",
    "tests/test-tui-atomic-state.rkt": "tui/state.rkt",
    "tests/test-tui-overlay-state.rkt": "tui/state.rkt",
    "tests/test-tui-selection-state.rkt": "tui/state.rkt",
    "tests/tui/test-render.rkt": "tui/render.rkt",
    "tests/tui/test-render-status-line.rkt": "tui/render/status-line.rkt",
    "tests/tui/test-render-message-layout.rkt": "tui/render/message-layout.rkt",
    "tests/tui/test-state.rkt": "tui/state.rkt",
    "tests/tui/test-terminal.rkt": "tui/terminal.rkt",
    "tests/tui/test-layout.rkt": "tui/layout.rkt",
    "tests/tui/test-keymap.rkt": "tui/keymap.rkt",
    "tests/tui/test-theme.rkt": "tui/theme.rkt",
    "tests/tui/test-palette.rkt": "tui/palette.rkt",
    "tests/tui/test-markdown.rkt": "tui/markdown.rkt",
    "tests/tui/test-clipboard.rkt": "tui/clipboard.rkt",
    "tests/tui/test-input.rkt": "tui/input.rkt",
    "tests/tui/test-commands.rkt": "tui/commands.rkt",
}

COVERS_RX = re.compile(r";+[ \t]*@covers[ \t]+(.*)$")

def stem(p):
    fn = os.path.basename(p)[:-4]
    if fn.startswith("test-"): fn = fn[4:]
    if fn.endswith("-test"): fn = fn[:-5]
    return fn

def requires_of(test_rel):
    """Return set of pilot-area module paths (repo-relative) required by test."""
    p = os.path.join(ROOT, test_rel)
    out = set()
    try:
        with open(p, encoding="utf-8", errors="replace") as f:
            content = f.read()
    except Exception:
        return out
    for m in REQ.finditer(content):
        resolved = os.path.normpath(os.path.join(os.path.dirname(test_rel), m.group(1)))
        resolved = resolved.replace(os.sep, "/")
        if resolved.startswith("q/"):
            resolved = resolved[2:]
        elif resolved.startswith("../q/"):
            resolved = resolved[5:]
        if resolved.startswith(PILOT) and os.path.exists(os.path.join(ROOT, resolved)):
            out.add(resolved)
    return out

test_files = []
for dirpath, dirnames, filenames in os.walk(TESTS):
    dirnames[:] = [d for d in dirnames if d != "compiled"]
    for fn in sorted(filenames):
        if fn.endswith(".rkt"):
            test_files.append(os.path.relpath(os.path.join(dirpath, fn), ROOT).replace(os.sep, "/"))
test_files.sort()

mapping = collections.defaultdict(list)
rejected_family = []
for t in test_files:
    reqs = requires_of(t)
    for m in sorted(reqs):
        if stem(t) == stem(m):
            mapping[t].append(m)
for t, m in sorted(FAMILY.items()):
    if not os.path.exists(os.path.join(ROOT, t)):
        rejected_family.append((t, m, "test-missing")); continue
    reqs = requires_of(t)
    if m not in reqs:
        rejected_family.append((t, m, "not-required")); continue
    if m not in mapping[t]:
        mapping[t].append(m)

if rejected_family:
    print("REJECTED FAMILY PAIRS (review!):")
    for t, m, why in rejected_family:
        print(f"  {t} -> {m} ({why})")
    sys.exit(1)

def existing_covers(lines):
    found = set()
    for line in lines[:50]:
        s = line.strip() if isinstance(line, str) else ""
        mm = COVERS_RX.match(s)
        if mm:
            for tok in re.split(r"[ \t,]+", mm.group(1).strip()):
                if tok: found.add(tok)
    return found

changed = 0
total_pairs = 0
for t in sorted(mapping):
    p = os.path.join(ROOT, t)
    with open(p, encoding="utf-8", errors="strict") as f:
        lines = f.read().split("\n")
    have = existing_covers(lines)
    to_add = [m for m in mapping[t] if m not in have]
    if not to_add:
        continue
    total_pairs += len(to_add)
    tag_lines = [f";; @covers {m}" for m in to_add]
    insert_at = None
    for i, line in enumerate(lines[:50]):
        s = line.strip() if isinstance(line, str) else ""
        if COVERS_RX.match(s):
            insert_at = i + 1
    if insert_at is None:
        for i, line in enumerate(lines[:5]):
            if isinstance(line, str) and line.startswith("#lang"):
                insert_at = i + 1
                break
    if insert_at is None:
        insert_at = 1
    lines[insert_at:insert_at] = tag_lines
    with open(p, "w", encoding="utf-8") as f:
        f.write("\n".join(lines))
    changed += 1

mods = sorted({m for ms in mapping.values() for m in ms})
print(f"test files mapped: {len(mapping)}  (edited this run: {changed})")
print(f"new tag lines inserted: {total_pairs}")
print(f"distinct modules covered: {len(mods)}")
by_area = collections.Counter(m.split("/")[0] for m in mods)
print("modules by area:", dict(by_area))
