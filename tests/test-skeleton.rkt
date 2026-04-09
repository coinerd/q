#lang racket

(require rackunit
         racket/runtime-path)

;; All planned module paths from BLUEPRINT/MODULES.md
;; Paths relative to q/ root directory
(define planned-module-paths
  '("main.rkt"
    ;; llm/
    "llm/model.rkt"
    "llm/provider.rkt"
    "llm/openai-compatible.rkt"
    "llm/anthropic.rkt"
    "llm/stream.rkt"
    "llm/token-budget.rkt"
    ;; agent/
    "agent/types.rkt"
    "agent/state.rkt"
    "agent/event-bus.rkt"
    "agent/queue.rkt"
    "agent/loop.rkt"
    ;; runtime/
    "runtime/agent-session.rkt"
    "runtime/session-store.rkt"
    "runtime/session-index.rkt"
    "runtime/compactor.rkt"
    "runtime/resource-loader.rkt"
    "runtime/model-registry.rkt"
    "runtime/auth-store.rkt"
    "runtime/settings.rkt"
    ;; tools/
    "tools/tool.rkt"
    "tools/scheduler.rkt"
    "tools/builtins/read.rkt"
    "tools/builtins/write.rkt"
    "tools/builtins/edit.rkt"
    "tools/builtins/bash.rkt"
    "tools/builtins/grep.rkt"
    "tools/builtins/find.rkt"
    "tools/builtins/ls.rkt"
    ;; extensions/
    "extensions/api.rkt"
    "extensions/hooks.rkt"
    "extensions/loader.rkt"
    "extensions/define-extension.rkt"
    ;; skills/
    "skills/skill-loader.rkt"
    "skills/prompt-template.rkt"
    "skills/context-files.rkt"
    ;; interfaces/
    "interfaces/cli.rkt"
    "interfaces/tui.rkt"
    "interfaces/json-mode.rkt"
    "interfaces/rpc-mode.rkt"
    "interfaces/sdk.rkt"
    ;; sandbox/
    "sandbox/subprocess.rkt"
    "sandbox/evaluator.rkt"
    "sandbox/limits.rkt"
    ;; util/
    "util/jsonl.rkt"
    "util/ids.rkt"
    "util/diff.rkt"
    "util/paths.rkt"))

;; The test file is at q/tests/test-skeleton.rkt
;; Module files are at q/<rel-path> relative to the q/ directory
;; So from tests/ we go up one level to reach q/
(define-runtime-path here ".")
(define q-root (simplify-path (build-path here "..")))

;; Test: all planned module files exist on disk
(for ([rel (in-list planned-module-paths)])
  (define full-path (build-path q-root rel))
  (check-true (file-exists? full-path)
              (format "Module file missing: ~a" rel)))

;; Test: each module file starts with #lang racket or #lang racket/base
(for ([rel (in-list planned-module-paths)])
  (define full-path (build-path q-root rel))
  (with-input-from-file full-path
    (lambda ()
      (define first-line (read-line))
      (check-true (or (equal? first-line "#lang racket")
                      (equal? first-line "#lang racket/base"))
                  (format "Module ~a does not start with #lang racket or #lang racket/base (got: ~a)"
                          rel first-line)))))

;; Test: info.rkt exists at project root (one level above q/)
(define project-root (simplify-path (build-path q-root "..")))
(define info-path (build-path project-root "info.rkt"))
(check-true (file-exists? info-path) "info.rkt missing at project root")

;; Test: total module count matches BLUEPRINT plan
(check-equal? (length planned-module-paths) 48
              "Planned module count should be 48 (from BLUEPRINT/MODULES.md)")
