#!/usr/bin/env python3
"""setup-github-project.py — Configure the "q Roadmap & Delivery" GitHub Projects V2 board.

Project already created: PVT_kwHOAJo6gM4BUXfi (https://github.com/users/coinerd/projects/1)
Start, Target, Iteration fields already exist.
Status field exists with default options — needs update.
Need to create: Priority, Area, Effort, Risk, OwnerNote
"""

import json
import sys
import os
import urllib.request
import urllib.error

API = "https://api.github.com/graphql"
REPO = "coinerd/q"
OWNER = "coinerd"
PROJECT_ID = "PVT_kwHOAJo6gM4BUXfi"

def get_token():
    for src in [os.environ.get("GH_PAT", ""), os.path.expanduser("~/GH_PAT")]:
        token = src.strip()
        if not token:
            try:
                with open(os.path.expanduser("~/GH_PAT")) as f:
                    token = f.read().strip()
            except FileNotFoundError:
                pass
        if token:
            return token
    print("ERROR: No GH_PAT found")
    sys.exit(1)

TOKEN = get_token()

def ghql(query, variables=None):
    body = {"query": query}
    if variables:
        body["variables"] = variables
    req = urllib.request.Request(
        API,
        data=json.dumps(body).encode(),
        headers={"Authorization": f"bearer {TOKEN}", "Content-Type": "application/json"},
    )
    try:
        with urllib.request.urlopen(req) as resp:
            data = json.loads(resp.read())
    except urllib.error.HTTPError as e:
        err = e.read().decode()
        print(f"HTTP {e.code}: {err[:500]}")
        return {"errors": [{"message": f"HTTP {e.code}"}]}
    if "errors" in data:
        for err in data["errors"][:3]:
            print(f"  GraphQL: {err.get('message', '')[:200]}")
        return data
    return data

# ============================================================
print("=" * 60)
print("q Roadmap & Delivery — GitHub Projects V2 Setup (Phase 2)")
print("=" * 60)
print()

# --- Step 1: Update Status field options ---
print("=== Step 1: Update Status field ===")
STATUS_FIELD_ID = "PVTSSF_lAHOAJo6gM4BUXfizhBgIbg"

status_options = [
    ("Inbox", "New untriaged work", "GRAY"),
    ("Ready", "Triaged and ready to pick up", "BLUE"),
    ("In progress", "Currently being worked on", "YELLOW"),
    ("Blocked", "Waiting on something", "RED"),
    ("In review", "Under review", "PURPLE"),
    ("Done", "Completed", "GREEN"),
]

# Use updateProjectV2Field to replace options
opts_str = ", ".join(
    f'{{name: "{n}", description: "{d}", color: {c}}}'
    for n, d, c in status_options
)
result = ghql(f"""mutation {{
    updateProjectV2Field(input: {{
        projectId: "{PROJECT_ID}",
        fieldId: "{STATUS_FIELD_ID}",
        name: "Status",
        singleSelectOptions: [{opts_str}]
    }}) {{
        projectV2Field {{
            ... on ProjectV2SingleSelectField {{ id name options {{ id name }} }}
        }}
    }}
}}""")
if "errors" not in result:
    fd = result["data"]["updateProjectV2Field"]["projectV2Field"]
    print(f"  ✓ Status updated ({len(fd['options'])} options)")
    for o in fd["options"]:
        print(f"    - {o['name']}")
else:
    print("  ✗ Failed to update Status")

print()

# --- Step 2: Create missing single-select fields ---
print("=== Step 2: Create custom fields ===")
print()

select_fields = [
    ("Priority", [
        ("P0", "Critical ship immediately", "RED"),
        ("P1", "High ship this release", "ORANGE"),
        ("P2", "Medium ship soon", "YELLOW"),
        ("P3", "Low nice to have", "GRAY"),
    ]),
    ("Area", [
        ("Core", "Agent core loop types event bus", "PURPLE"),
        ("Provider", "LLM provider adapters", "BLUE"),
        ("TUI", "Terminal UI", "GREEN"),
        ("CLI", "Command-line interface", "ORANGE"),
        ("RPC/SDK", "RPC mode and SDK bindings", "BLUE"),
        ("Packages", "Racket package distribution", "YELLOW"),
        ("Extensions", "Extension platform", "PINK"),
        ("Docs", "Documentation", "GRAY"),
        ("Security", "Security hardening", "RED"),
        ("CI", "Continuous integration", "BLUE"),
        ("Benchmarks", "Performance benchmarks", "GREEN"),
    ]),
    ("Effort", [
        ("XS", "Under 1 hour", "GREEN"),
        ("S", "1 to 3 hours", "BLUE"),
        ("M", "Half a day", "YELLOW"),
        ("L", "Full day or more", "ORANGE"),
        ("XL", "Multi-day effort", "RED"),
    ]),
    ("Risk", [
        ("Low", "Straightforward change", "GREEN"),
        ("Medium", "Some complexity or dependency", "YELLOW"),
        ("High", "Architectural or breaking change", "RED"),
    ]),
]

for field_name, options in select_fields:
    opts = ", ".join(
        f'{{name: "{n}", description: "{d}", color: {c}}}'
        for n, d, c in options
    )
    print(f"  Creating: {field_name} ({len(options)} options)")
    result = ghql(f"""mutation {{
        createProjectV2Field(input: {{
            projectId: "{PROJECT_ID}",
            name: "{field_name}",
            dataType: SINGLE_SELECT,
            singleSelectOptions: [{opts}]
        }}) {{
            projectV2Field {{
                ... on ProjectV2SingleSelectField {{ id name options {{ id name }} }}
            }}
        }}
    }}""")
    if "errors" not in result:
        fd = result["data"]["createProjectV2Field"]["projectV2Field"]
        print(f"    ✓ {fd['name']} → {fd['id']}")
    else:
        print(f"    ✗ Failed")

# OwnerNote (text)
print(f"  Creating: OwnerNote (TEXT)")
result = ghql(f"""mutation {{
    createProjectV2Field(input: {{
        projectId: "{PROJECT_ID}",
        name: "OwnerNote",
        dataType: TEXT
    }}) {{
        projectV2Field {{
            ... on ProjectV2Field {{ id name dataType }}
        }}
    }}
}}""")
if "errors" not in result:
    fd = result["data"]["createProjectV2Field"]["projectV2Field"]
    print(f"    ✓ {fd.get('name', 'OwnerNote')} → {fd.get('id', '?')}")
else:
    print(f"    ✗ Failed")

print()

# --- Step 3: Create saved views ---
print("=== Step 3: Create saved views ===")
print()

views = [
    ("Triage", "TABLE_LAYOUT"),
    ("Current Release", "BOARD_LAYOUT"),
    ("Backlog", "TABLE_LAYOUT"),
    ("Roadmap", "ROADMAP_LAYOUT"),
    ("Review Queue", "TABLE_LAYOUT"),
    ("Docs / Onboarding", "BOARD_LAYOUT"),
    ("Package / Ecosystem Work", "TABLE_LAYOUT"),
    ("This Iteration", "BOARD_LAYOUT"),
]

for view_name, layout in views:
    print(f"  Creating: {view_name}")
    result = ghql(f"""mutation {{
        createProjectV2View(input: {{
            projectId: "{PROJECT_ID}",
            name: "{view_name}",
            layout: {layout}
        }}) {{
            projectV2View {{ id name number }}
        }}
    }}""")
    if "errors" not in result:
        v = result["data"]["createProjectV2View"]["projectV2View"]
        print(f"    ✓ {v['name']} → view #{v['number']}")
    else:
        print(f"    ✗ Failed")

print()

# --- Step 4: Add all open issues to project ---
print("=== Step 4: Add open issues ===")
print()

req = urllib.request.Request(
    f"https://api.github.com/repos/{REPO}/issues?state=open&per_page=100",
    headers={"Authorization": f"token {TOKEN}", "Accept": "application/vnd.github.v3+json"},
)
with urllib.request.urlopen(req) as resp:
    issues = [i for i in json.loads(resp.read()) if "pull_request" not in i]

print(f"  Found {len(issues)} open issues")
for issue in issues:
    num = issue["number"]
    node_id = issue["node_id"]
    title = issue["title"][:50]
    print(f"  #{num}: {title}...", end=" ", flush=True)
    result = ghql("""mutation($pid: ID!, $cid: ID!) {
        addProjectV2ItemById(input: {projectId: $pid, contentId: $cid}) { item { id } }
    }""", {"pid": PROJECT_ID, "cid": node_id})
    if "errors" not in result:
        iid = result["data"]["addProjectV2ItemById"]["item"]["id"][:16]
        print(f"✓ {iid}...")
    else:
        print("✗")

print()

# --- Step 5: Verify ---
print("=== Step 5: Verify ===")
print()

result = ghql(f"""{{
  node(id: "{PROJECT_ID}") {{
    ... on ProjectV2 {{
      title url
      fields(first: 30) {{
        nodes {{
          ... on ProjectV2Field {{ id name dataType }}
          ... on ProjectV2SingleSelectField {{ id name dataType options {{ id name }} }}
          ... on ProjectV2IterationField {{ id name dataType }}
        }}
      }}
      views(first: 10) {{
        nodes {{ id name number }}
      }}
      items(first: 5) {{
        totalCount
      }}
    }}
  }}
}}""")

if "errors" not in result:
    proj = result["data"]["node"]
    print(f"  Project: {proj['title']}")
    print(f"  URL: {proj['url']}")
    print(f"  Fields ({len(proj['fields']['nodes'])}):")
    for f in proj["fields"]["nodes"]:
        opts = f.get("options", [])
        if opts:
            print(f"    {f['name']} ({f['dataType']}) — {len(opts)} options")
        else:
            print(f"    {f['name']} ({f['dataType']})")
    print(f"  Views ({len(proj['views']['nodes'])}):")
    for v in proj["views"]["nodes"]:
        print(f"    #{v['number']}: {v['name']}")
    print(f"  Items: {proj['items']['totalCount']}")

print()
print("=" * 60)
print("SETUP COMPLETE")
print()
print("MANUAL POST-SETUP STEPS:")
print()
print("1. Configure view filters (click each view → filter bar):")
print("   - Triage:           Status = Inbox")
print("   - Current Release:  Milestone = v0.7.0, group by Status")
print("   - Backlog:          Status ≠ Done, sort by Priority")
print("   - Roadmap:          Status ≠ Done, date fields Start/Target")
print("   - Review Queue:     Status = In review")
print("   - Docs / Onboarding: Label = docs, Status ≠ Done")
print("   - Package/Ecosystem: Area = Packages, Status ≠ Done")
print("   - This Iteration:   Iteration = @current, group by Status")
print()
print("2. Enable built-in automations (Settings → Workflows):")
print("   - Item added → Status = Inbox")
print("   - Issue/PR closed → Status = Done")
print("   - PR merged → Status = Done")
print()
print("3. Set up auto-add workflows:")
print("   - is:open  (add all open issues)")
print("   - is:open label:bug")
print("   - is:open label:docs")
print()
print("4. Add insights/charts:")
print("   - Current release by Status")
print("   - Open work by Area / Priority")
print()
print("=" * 60)
