# CL5 P2-F feasibility pilot — evaluation-only workflow template

This document is the reviewed template. GitHub resolves a dispatch by workflow filename against the
default branch, so the executable copy is deployed at `.github/workflows/cl5-p2f-pilot.yml` on `main`
(branch-only deployment returned 404). It has no push or PR trigger, is not a required check, and
selects no tests; this file remains the human-readable copy.

The workflow validates that the tested tree differs from the pinned SHA only by its own file and the
three `CL5-P2F-*` reports, then
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
    # Eight files sum to 670 s of CI elapsed. Worst case (no overlap at all) six
    # invocations are 4020 s = 67 min, plus setup; 120 min leaves room without
    # turning a job timeout into a measurement sample.
    timeout-minutes: 120
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
          git merge-base --is-ancestor "$PINNED_SHA" HEAD
          # The workflow must live on the default branch to be dispatchable by
          # filename, so the tested tree is pinned by diffing against the frozen
          # SHA instead of by checking the parent commit.
          allowed='.github/workflows/cl5-p2f-pilot.yml
          docs/reports/CL5-P2F-DESIGN.md
          docs/reports/CL5-P2F-PILOT-WORKFLOW.md
          docs/reports/CL5-P2F-WORKLOAD.json'
          actual="$(git diff --name-only "$PINNED_SHA" HEAD)"
          test "$actual" = "$allowed"
          mkdir -p cl5-p2f
          printf 'dispatch_sha=%s\nworkflow_sha=%s\npinned_sha=%s\nworkload_sha256=3e93bd21ec840814bdd6d8c26a3ca9314602e4e9e64ef9987e46975bfd380dba\n' \
            "$DISPATCH_SHA" "$WORKFLOW_SHA" "$PINNED_SHA" > cl5-p2f/provenance.txt

      # This isolated job has no same-run prepared-environment producer, so the
      # guarded restore would always be skipped. Opt out EXPLICITLY and record the
      # action's own outputs; never read Q_PREPARED_ENV_* env vars that the action
      # never exports.
      - uses: ./.github/actions/setup-racket
        id: setup
        with:
          racket-version: '8.10'
          prepared-environment: 'off'

      - name: Record the measurement environment
        env:
          SETUP_STATE: ${{ steps.setup.outputs.prepared-env-state }}
          SETUP_RESTORE_MS: ${{ steps.setup.outputs.prepared-env-restore-ms }}
          SETUP_FALLBACK_MS: ${{ steps.setup.outputs.prepared-env-fallback-ms }}
          SETUP_DURATION_S: ${{ steps.setup.outputs.eager-setup-seconds }}
          CACHE_STATE: ${{ steps.setup.outputs.cache-state }}
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
            printf 'prepared_env_state=%s\n' "$SETUP_STATE"
            printf 'prepared_restore_ms=%s\n' "${SETUP_RESTORE_MS:-empty}"
            printf 'prepared_fallback_ms=%s\n' "${SETUP_FALLBACK_MS:-empty}"
            printf 'setup_duration_s=%s\n' "${SETUP_DURATION_S:-empty}"
            printf 'racket_addon_cache_state=%s\n' "$CACHE_STATE"
          } > cl5-p2f/environment.txt

      - name: Materialise the frozen workload
        shell: bash
        run: |
          set -euo pipefail
          echo "3e93bd21ec840814bdd6d8c26a3ca9314602e4e9e64ef9987e46975bfd380dba  docs/reports/CL5-P2F-WORKLOAD.json" | sha256sum -c -
          python3 - <<'PY' > cl5-p2f/files.txt
          import json
          d = json.load(open('docs/reports/CL5-P2F-WORKLOAD.json'))
          assert d['schema'] == 'cl5-p2f-workload/1'
          assert d['pinned_test_tree_sha'] == 'e56b3f827bd6fbdb728babb0d2dd944da7f72c94'
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
          : > cl5-p2f/schedule.txt
          for i in 0 1 2 3 4 5; do
            printf 'load_before_run_%s=%s\n' "$i" "$(cat /proc/loadavg)" >> cl5-p2f/schedule.txt
            argv="racket scripts/run-tests/runner.rkt --jobs 4 --json-out cl5-p2f/run-$i.json ${FILES[*]}"
            printf 'argv_run_%s=%s\n' "$i" "$argv" >> cl5-p2f/schedule.txt
            start=$(date +%s%3N)
            rc=0
            racket scripts/run-tests/runner.rkt \
              --jobs 4 --json-out "cl5-p2f/run-$i.json" "${FILES[@]}" \
              > "cl5-p2f/run-$i.log" 2>&1 || rc=$?
            end=$(date +%s%3N)
            printf 'run=%s wall_ms=%s rc=%s\n' "$i" "$((end-start))" "$rc" \
              >> cl5-p2f/schedule.txt
            test "$rc" -eq 0
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
