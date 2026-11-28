# CL5 W3 paired-measurement pilot workflow (reviewed template)

Reviewed template for the dispatch-only workflow deployed at
`.github/workflows/cl5-w3-paired-pilot.yml`. It has no push/PR trigger, is not a required check and
selects no production tests. The deployed file is authoritative; this copy records the exact
reviewed revision before dispatch.

```yaml
name: CL5 W3 same-runner paired measurement pilot (evaluation only)

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
  group: cl5-w3-paired-pilot-${{ inputs.source-sha }}
  cancel-in-progress: false

jobs:
  pilot:
    runs-on: ubuntu-latest
    timeout-minutes: 90
    steps:
      - uses: actions/checkout@v7
        with:
          persist-credentials: false
          fetch-depth: 0

      - name: Bind the pinned tree
        env:
          SOURCE_SHA: ${{ inputs.source-sha }}
          DISPATCH_SHA: ${{ github.sha }}
          WORKFLOW_SHA: ${{ github.workflow_sha }}
          PINNED_SHA: ecb92a5e846a48ee710d9e5b9bed9ca0b0631d5b
        shell: bash
        run: |
          set -euo pipefail
          test "$SOURCE_SHA" = "$DISPATCH_SHA"
          echo "$SOURCE_SHA" | grep -Eq '^[0-9a-f]{40}$'
          test "$(git rev-parse HEAD)" = "$SOURCE_SHA"
          git merge-base --is-ancestor "$PINNED_SHA" HEAD
          allowed='.github/workflows/cl5-w3-paired-pilot.yml
          README.md
          artifacts/tier-ownership/v1.00.32-w0/ownership-matrix.json
          docs/architecture/subsystem-manifest.rktd
          docs/reports/CL5-W3-PAIRED-PILOT-DESIGN.md
          docs/reports/CL5-W3-PAIRED-PILOT-WORKFLOW.md
          docs/reports/CL5-W3-PAIRED-PILOT-WORKLOAD.json
          scripts/cl5/paired-pilot-selection.rkt
          tests/.coverage-manifest.json
          tests/test-cl5-w3-paired-pilot.rkt'
          test "$(git diff --name-only "$PINNED_SHA" HEAD)" = "$allowed"
          mkdir -p cl5-w3-paired
          printf 'dispatch_sha=%s\nworkflow_sha=%s\npinned_sha=%s\n' \
            "$DISPATCH_SHA" "$WORKFLOW_SHA" "$PINNED_SHA" > cl5-w3-paired/provenance.txt

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
          . /etc/os-release
          {
            printf 'date=%s\n' "$(date -Is)"
            printf 'os_id=%s\n' "$ID"
            printf 'os_version_id=%s\n' "$VERSION_ID"
            printf 'os_pretty_name=%s\n' "$PRETTY_NAME"
            printf 'arch=%s\n' "$(uname -m)"
            printf 'runner_arch=%s\n' "${RUNNER_ARCH:-}"
            printf 'image_os=%s\n' "${ImageOS:-}"
            printf 'image_version=%s\n' "${ImageVersion:-}"
            printf 'cpu_count=%s\n' "$(nproc)"
            printf 'cpu_model=%s\n' "$(grep -m1 'model name' /proc/cpuinfo)"
            printf 'memory_kb=%s\n' "$(awk '/^MemTotal:/{print $2; exit}' /proc/meminfo)"
            printf 'cpu_max=%s\n' "$(cat /sys/fs/cgroup/cpu.max 2>/dev/null || echo unavailable)"
            printf 'kernel_release=%s\n' "$(cat /proc/sys/kernel/osrelease)"
            printf 'racket_version=%s\n' "$(racket --version)"
            printf 'racket_exe_sha256=%s\n' "$(sha256sum "$(command -v racket)" | cut -d' ' -f1)"
            printf 'prepared_env_state=%s\n' "$SETUP_STATE"
            printf 'prepared_restore_ms=%s\n' "${SETUP_RESTORE_MS:-empty}"
            printf 'prepared_fallback_ms=%s\n' "${SETUP_FALLBACK_MS:-empty}"
            printf 'setup_duration_s=%s\n' "${SETUP_DURATION_S:-empty}"
            printf 'racket_addon_cache_state=%s\n' "$CACHE_STATE"
          } > cl5-w3-paired/environment.txt

      - name: Validate measured host before measurement
        shell: bash
        run: |
          set -euo pipefail
          get() {
            awk -F= -v k="$1" '$1 == k { print substr($0, index($0, "=") + 1); exit }' \
              cl5-w3-paired/environment.txt
          }
          bad=0
          require() {
            if [ "$2" != "$3" ]; then
              printf 'HOST MISMATCH %s: observed=%q expected=%q\n' "$1" "$2" "$3" >&2
              bad=1
            fi
          }
          require os_id "$(get os_id)" ubuntu
          require os_version_id "$(get os_version_id)" 26.04
          require os_pretty_name "$(get os_pretty_name)" 'Ubuntu 26.04.1 LTS'
          require image_os "$(get image_os)" ubuntu26
          require image_version "$(get image_version)" 20261122.9.1
          require arch "$(get arch)" x86_64
          require runner_arch "$(get runner_arch)" X64
          require cpu_count "$(get cpu_count)" 4
          require racket_exe_sha256 "$(get racket_exe_sha256)" \
            7674f17865fa795b21641828d839e689e01a2f17bbdd2b7d71cc59cceafa873c
          case "$(get cpu_model)" in *'AMD EPYC '*) ;; *)
            printf 'HOST MISMATCH cpu_model: %s\n' "$(get cpu_model)" >&2; bad=1;;
          esac
          case "$(get racket_version)" in *'8.10'*) ;; *)
            printf 'HOST MISMATCH racket_version: %s\n' "$(get racket_version)" >&2; bad=1;;
          esac
          mem="$(get memory_kb)"
          awk -v m="$mem" 'BEGIN { exit !(m >= 15500000 && m <= 17500000) }' || {
            printf 'HOST MISMATCH memory_kb: %s\n' "$mem" >&2; bad=1
          }
          test "$bad" -eq 0

      - name: Materialise the frozen reduced surface
        shell: bash
        run: |
          set -euo pipefail
          echo "f4f557ef3a646c0977bb52683670586d848f02212b6d5aa837a831d6841cf0d8  docs/reports/CL5-W3-PAIRED-PILOT-WORKLOAD.json" | sha256sum -c -
          racket -e '(require json) (define d (call-with-input-file "docs/reports/CL5-W3-PAIRED-PILOT-WORKLOAD.json" read-json)) (unless (equal? (hash-ref d (quote schema)) "cl5-w3-paired-pilot/1") (error "bad schema")) (unless (equal? (hash-ref d (quote pinned_test_tree_sha)) "ecb92a5e846a48ee710d9e5b9bed9ca0b0631d5b") (error "bad pin")) (unless (= (length (hash-ref d (quote comprehensive_reduced_files))) 8) (error "bad comprehensive surface")) (unless (= (length (hash-ref d (quote selected_projected_files))) 2) (error "bad selected surface")) (for ([p (in-list (hash-ref d (quote comprehensive_reduced_files)))]) (displayln p))' > cl5-w3-paired/files-comprehensive.txt
          racket -e '(require json) (define d (call-with-input-file "docs/reports/CL5-W3-PAIRED-PILOT-WORKLOAD.json" read-json)) (for ([p (in-list (hash-ref d (quote selected_projected_files)))]) (displayln p))' > cl5-w3-paired/files-selected.txt
          test "$(wc -l < cl5-w3-paired/files-comprehensive.txt)" -eq 8
          test "$(wc -l < cl5-w3-paired/files-selected.txt)" -eq 2

      - name: Warm-up and alternating measured pairs
        shell: bash
        run: |
          set -euo pipefail
          : > cl5-w3-paired/schedule.txt
          run_arm() {
            local arm="$1" label="$2" out="$3"
            local overhead=0 start end rc
            if [ "$arm" = selected ]; then
              start=$(date +%s%3N)
              racket scripts/cl5/paired-pilot-selection.rkt \
                --workload docs/reports/CL5-W3-PAIRED-PILOT-WORKLOAD.json \
                --out "cl5-w3-paired/selection-$label.json"
              end=$(date +%s%3N)
              overhead=$((end-start))
            fi
            mapfile -t FILES < "cl5-w3-paired/files-$arm.txt"
            printf 'load_before=%s\n' "$(cat /proc/loadavg)" >> cl5-w3-paired/schedule.txt
            printf 'invocation=%s-%s arm=%s files=%s selection_overhead_ms=%s\n' \
              "$arm" "$label" "$arm" "${#FILES[@]}" "$overhead" >> cl5-w3-paired/schedule.txt
            start=$(date +%s%3N)
            rc=0
            racket scripts/run-tests/runner.rkt --jobs 4 \
              --json-out "cl5-w3-paired/run-$arm-$label.json" "${FILES[@]}" \
              > "cl5-w3-paired/run-$arm-$label.log" 2>&1 || rc=$?
            end=$(date +%s%3N)
            printf 'result=%s-%s wall_ms=%s rc=%s\n' "$arm" "$label" "$((end-start))" "$rc" \
              >> cl5-w3-paired/schedule.txt
            test "$rc" -eq 0
          }
          run_arm selected warm0
          run_arm comprehensive warm0
          run_arm selected pair1
          run_arm comprehensive pair1
          run_arm comprehensive pair2
          run_arm selected pair2
          run_arm selected pair3
          run_arm comprehensive pair3
          printf 'load_after=%s\n' "$(cat /proc/loadavg)" >> cl5-w3-paired/schedule.txt

      - name: Upload pilot evidence
        if: always()
        uses: actions/upload-artifact@v7
        with:
          name: cl5-w3-paired-pilot
          path: cl5-w3-paired/
          if-no-files-found: error
```
