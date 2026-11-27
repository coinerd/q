# CL5 P2-F feasibility pilot — evaluation-only workflow template

This file is deliberately stored under `docs/reports/`, **not** under `.github/workflows/`, so it is
inert on `main`. To run the pilot it is copied to `.github/workflows/cl5-p2f-pilot.yml` on a temporary
feature branch based on `e56b3f82`, together with `docs/reports/CL5-P2F-WORKLOAD.json`. The branch is
not merged. Only `workflow_dispatch` triggers it; no push or PR event runs it.

The workflow validates that the tested tree differs from the pinned SHA only by those two files, then
records host metadata, runs one warm-up and five measured invocations over the frozen eight-file
workload, and uploads all artifacts. It spawns exactly one runner per invocation and never calls
`./qt` or a nested runner.

```yaml
name: CL5 P2-F host-comparability pilot (evaluation only)

on:
  workflow_dispatch:
    inputs:
      source-sha:
        description: 'Dispatch head SHA (must match github.sha)'
        required: true
        type: string

permissions:
  contents: read
  actions: read

concurrency:
  group: cl5-p2f-pilot-${{ inputs.source-sha }}
  cancel-in-progress: false

jobs:
  pilot:
    runs-on: ubuntu-latest
    timeout-minutes: 60
    steps:
      - uses: actions/checkout@v7
        with:
          persist-credentials: false

      - name: Bind the pinned tree
        env:
          SOURCE_SHA: ${{ inputs.source-sha }}
          DISPATCH_SHA: ${{ github.sha }}
          WORKFLOW_SHA: ${{ github.workflow_sha }}
          PINNED_SHA: e56b3f827bd6fbdb728babb0d2dd944da7f72c94
        shell: bash
        run: |
          set -euo pipefail
          test "$SOURCE_SHA" = "$DISPATCH_SHA"
          echo "$SOURCE_SHA" | grep -Eq '^[0-9a-f]{40}$'
          test "$(git rev-parse HEAD)" = "$SOURCE_SHA"
          test "$(git rev-parse HEAD^)" = "$PINNED_SHA"
          allowed='.github/workflows/cl5-p2f-pilot.yml
          docs/reports/CL5-P2F-WORKLOAD.json'
          actual="$(git diff --name-only HEAD^ HEAD)"
          test "$actual" = "$allowed"
          mkdir -p cl5-p2f
          printf 'dispatch_sha=%s\nworkflow_sha=%s\npinned_sha=%s\n' \
            "$DISPATCH_SHA" "$WORKFLOW_SHA" "$PINNED_SHA" > cl5-p2f/provenance.txt

      - uses: ./.github/actions/setup-racket
        with:
          racket-version: '8.10'

      - name: Record the measurement environment
        shell: bash
        run: |
          set -euo pipefail
          {
            printf 'date=%s\n' "$(date -Is)"
            printf 'uname=%s\n' "$(uname -a)"
            printf 'os=%s\n' "$(. /etc/os-release; echo "$PRETTY_NAME")"
            printf 'arch=%s\n' "$(uname -m)"
            printf 'cpu_count=%s\n' "$(nproc)"
            printf 'cpu_model=%s\n' "$(grep -m1 'model name' /proc/cpuinfo)"
            printf 'memory=%s\n' "$(free -h | awk '/^Mem:/{print $2}')"
            printf 'cpu_max=%s\n' "$(cat /sys/fs/cgroup/cpu.max 2>/dev/null || echo unavailable)"
            printf 'load_before=%s\n' "$(cat /proc/loadavg)"
            printf 'racket_version=%s\n' "$(racket --version)"
            printf 'racket_exe_sha256=%s\n' "$(sha256sum "$(command -v racket)" | cut -d' ' -f1)"
            printf 'prepared_env_state=%s\n' "${Q_PREPARED_ENV_STATE:-unavailable}"
            printf 'prepared_restore_ms=%s\n' "${Q_PREPARED_ENV_RESTORE_MS:-}"
            printf 'prepared_fallback_ms=%s\n' "${Q_PREPARED_ENV_FALLBACK_MS:-}"
          } > cl5-p2f/environment.txt

      - name: Materialise the frozen workload
        shell: bash
        run: |
          set -euo pipefail
          python3 - <<'PY' > cl5-p2f/files.txt
          import json
          d = json.load(open('docs/reports/CL5-P2F-WORKLOAD.json'))
          assert d['schema'] == 'cl5-p2f-workload/1'
          assert len(d['files']) == 8
          for f in d['files']:
              print(f['path'])
          PY
          test "$(wc -l < cl5-p2f/files.txt)" -eq 8

      - name: Warm-up and five measured invocations
        shell: bash
        run: |
          set -euo pipefail
          mapfile -t FILES < cl5-p2f/files.txt
          printf 'argv=racket scripts/run-tests/runner.rkt --jobs 4 --json-out cl5-p2f/run-<i>.json <8 files>\n' \
            > cl5-p2f/schedule.txt
          for i in 0 1 2 3 4 5; do
            printf 'load_before_run_%s=%s\n' "$i" "$(cat /proc/loadavg)" >> cl5-p2f/schedule.txt
            start=$(date +%s%3N)
            racket scripts/run-tests/runner.rkt \
              --jobs 4 --json-out "cl5-p2f/run-$i.json" "${FILES[@]}" \
              > "cl5-p2f/run-$i.log" 2>&1
            end=$(date +%s%3N)
            printf 'run=%s wall_ms=%s status=%s\n' "$i" "$((end-start))" "$?" \
              >> cl5-p2f/schedule.txt
          done
          printf 'load_after=%s\n' "$(cat /proc/loadavg)" >> cl5-p2f/schedule.txt

      - name: Upload pilot evidence
        if: always()
        uses: actions/upload-artifact@v7
        with:
          name: cl5-p2f-pilot
          path: cl5-p2f/
          if-no-files-found: error
```
