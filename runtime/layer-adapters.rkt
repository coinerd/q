#lang racket/base
;; STABILITY: public

;; runtime/layer-adapters.rkt — Stable adapter facade for upward layer imports
;; STABILITY: internal
;;
;; ARCH-01 (layer exception): The runtime layer formally imports from tools/ and
;; extensions/ (upper layers). This module provides a SINGLE import point for
;; those upward dependencies, making the boundary explicit and auditable.
;;
;; Consumers in runtime/ should import from THIS module instead of directly
;; from tools/ or extensions/. This prevents layer violations from spreading
;; and allows the underlying implementations to change without breaking callers.
;;
;; Re-exported from tools/:
;;   - list-tools-jsexpr, merge-tool-lists (tool schema assembly)
;;   - run-tool-batch, scheduler-result, scheduler-result-results (tool execution)
;;   - make-exec-context, make-error-result (execution context)
;;   - tool-result?, tool-registry? (predicates)
;;
;; Re-exported from extensions/:
;;   - dispatch-hooks (context assembly hook dispatch)
;;   - make-extension-ctx (extension context factory)
;;   - manifest operations (package management adapter)
;;   - catalog metadata operations (extension discovery adapter)

(require racket/contract
         (only-in "../tools/tool.rkt"
                  list-tools-jsexpr
                  merge-tool-lists
                  lookup-tool
                  tool-required-capability
                  tool-result?
                  tool-registry?
                  make-exec-context
                  make-error-result)
         (only-in "../tools/permission-gate.rkt" permission-config? make-default-permission-config)
         (only-in "../tools/scheduler.rkt" run-tool-batch scheduler-result scheduler-result-results)
         (only-in "../extensions/hooks.rkt" dispatch-hooks)
         (only-in "../extensions/context.rkt" make-extension-ctx)
         (only-in "../extensions/api.rkt"
                  extension-registry?
                  list-extensions
                  extension-name
                  extension-version)
         (only-in "../extensions/loader.rkt" get-extension-name-from-path)
         (only-in "../extensions/manifest.rkt"
                  read-qpm-manifest
                  validate-manifest
                  compute-manifest-checksum
                  qpm-manifest-name
                  qpm-manifest-version
                  qpm-manifest-checksum
                  qpm-manifest-files)
         (only-in "../extensions/gsd/session-state.rkt" current-gsd-ctx gsd-session-ctx?)
         ;; W2: the per-session trusted scratch allocation (tools/scratch.rkt).
         ;; The runtime session owner allocates/binds exactly one root per agent
         ;; session; the tool coordinator stamps it onto the exec-context.
         (only-in "../tools/scratch.rkt"
                  allocate-session-scratch-root!
                  allocated-session-scratch-root
                  bind-session-scratch-root!
                  release-session-scratch-root!
                  take-adopted-session-scratch-root!))

;; Tool schema assembly
(provide list-tools-jsexpr
         merge-tool-lists
         ;; Trusted registry metadata for capability admission.
         lookup-tool
         tool-required-capability
         ;; Tool execution
         run-tool-batch
         scheduler-result
         scheduler-result-results
         ;; Tool predicates
         tool-result?
         tool-registry?
         ;; Execution context
         make-exec-context
         make-error-result
         ;; Extension hooks
         dispatch-hooks
         ;; Extension context factory
         make-extension-ctx
         ;; Extension registry + listing
         extension-registry?
         list-extensions
         ;; Permission gate
         permission-config?
         make-default-permission-config
         ;; GSD session context
         current-gsd-ctx
         gsd-session-ctx?
         ;; W2 per-session trusted scratch allocation
         allocate-session-scratch-root!
         allocated-session-scratch-root
         bind-session-scratch-root!
         release-session-scratch-root!
         take-adopted-session-scratch-root!
         extension-name
         extension-version
         get-extension-name-from-path
         read-qpm-manifest
         validate-manifest
         compute-manifest-checksum
         qpm-manifest-name
         qpm-manifest-version
         qpm-manifest-checksum
         qpm-manifest-files)
