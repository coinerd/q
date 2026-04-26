#!/usr/bin/env racket
#lang racket

;; scripts/test-gsd-sdk-live.rkt — Live GSD planning test via SDK
;;
;; Exercises the q:plan → q:go workflow against the real LLM provider
;; from ~/.q/config.json, observing GSD state transitions.
;;
;; Updated for v0.20.5: uses q:plan / q:go convenience API instead of
;; manual hook dispatch.

(require racket/port
         racket/string
         racket/set
         "../runtime/settings.rkt"
         (only-in "../llm/provider.rkt" provider-name)
         "../runtime/provider-factory.rkt"
         "../extensions/loader.rkt"
         "../extensions/gsd-planning.rkt"
         "../agent/event-bus.rkt"
         (only-in "../util/event.rkt" event-ev event-payload)
         "../tools/tool.rkt"
         "../interfaces/sdk.rkt"
         (only-in "../extensions/gsd-planning-state.rkt" gsd-snapshot)
         "../util/cancellation.rkt")

;; ============================================================
;; 1. Build real provider + extensions via SDK
;; ============================================================
(displayln "=== Step 1: Setup ===")
(define project-dir (current-directory))
(define settings (load-settings project-dir))
(define prov (build-provider (hasheq 'project-dir project-dir) settings))
(displayln (format "  Provider: ~a" (provider-name prov)))

;; Clean stale artifacts
(reset-all-gsd-state!)
(define plan-path (build-path project-dir ".planning" "PLAN.md"))
(when (file-exists? plan-path)
  (delete-file plan-path)
  (displayln "  Deleted stale PLAN.md"))

(define rt
  (make-runtime #:provider prov
                #:auto-load-extensions? #t
                #:project-dir project-dir
                #:max-iterations 25
                #:session-dir "/tmp/q-gsd-sdk-test"))

;; Show registered tools + extensions
(define tool-reg (runtime-config-tool-registry (runtime-rt-config rt)))
(displayln (format "  Tools: ~a registered" (length (list-tools tool-reg))))

;; Subscribe to events
(define bus (runtime-config-event-bus (runtime-rt-config rt)))
(define event-log (box '()))
(subscribe! bus (λ (evt) (set-box! event-log (append (unbox event-log) (list evt)))))

;; ============================================================
;; 2. Open session (extensions pre-registered automatically)
;; ============================================================
(displayln "")
(displayln "=== Step 2: Open session ===")
(define rt-open (open-session rt))
(displayln (format "  GSD: ~a" (q:gsd-status)))
;; Show tools now that extensions are registered
(define tool-reg2 (runtime-config-tool-registry (runtime-rt-config rt-open)))
(displayln (format "  Tools after open-session: ~a" (length (list-tools tool-reg2))))
(displayln "")

;; ============================================================
;; 3. q:plan — one-liner GSD planning
;; ============================================================
(displayln "════════════════════════════════════════════════════════")
(displayln "=== Step 3: q:plan ===")
(displayln "════════════════════════════════════════════════════════")
(displayln "  Running q:plan...")
(displayln "")

(define-values (rt-after-plan plan-result)
  (q:plan rt-open "Create a file /tmp/hello.txt containing just 'Hello World'"))

(displayln "")
(displayln "=== After q:plan ===")
(define snap1 (gsd-snapshot))
(displayln (format "  mode: ~a" (hash-ref snap1 'mode)))
(displayln (format "  plan-tool-budget: ~a" (hash-ref snap1 'plan-tool-budget)))
(displayln (format "  PLAN.md exists: ~a" (file-exists? plan-path)))
(when (file-exists? plan-path)
  (define txt (file->string plan-path))
  (displayln (format "  PLAN.md: ~a bytes" (string-length txt)))
  (displayln "  --- head ---")
  (displayln (string-trim (if (> (string-length txt) 600)
                              (string-append (substring txt 0 600) "\n...")
                              txt)))
  (displayln "  ---"))
(displayln "")

;; ============================================================
;; 4. q:go — one-liner GSD execution
;; ============================================================
(displayln "════════════════════════════════════════════════════════")
(displayln "=== Step 4: q:go ===")
(displayln "════════════════════════════════════════════════════════")
(displayln "  Running q:go...")
(displayln "")

(define-values (rt-after-go go-result) (q:go rt-after-plan))

(displayln "")
(displayln "=== After q:go ===")
(define snap2 (gsd-snapshot))
(displayln (format "  mode: ~a" (hash-ref snap2 'mode)))
(displayln (format "  total-waves: ~a" (hash-ref snap2 'total-waves)))
(displayln (format "  completed-waves: ~a" (set-count (hash-ref snap2 'completed-waves))))
(displayln (format "  go-read-budget: ~a" (hash-ref snap2 'go-read-budget)))
(displayln "")

;; ============================================================
;; 5. Verify /tmp/hello.txt was created
;; ============================================================
(displayln "════════════════════════════════════════════════════════")
(displayln "=== Step 5: Verify output ===")
(displayln "════════════════════════════════════════════════════════")
(displayln (format "  /tmp/hello.txt exists: ~a" (file-exists? "/tmp/hello.txt")))
(when (file-exists? "/tmp/hello.txt")
  (displayln (format "  Content: ~a" (string-trim (file->string "/tmp/hello.txt")))))
(displayln "")

;; ============================================================
;; 6. Event summary
;; ============================================================
(displayln "════════════════════════════════════════════════════════")
(displayln "=== Event Summary ===")
(displayln "════════════════════════════════════════════════════════")
(define names (remove-duplicates (map (lambda (e) (event-ev e)) (unbox event-log))))
(displayln (format "  Event types: ~a" names))
(displayln (format "  Total events: ~a" (length (unbox event-log))))
(displayln "")
(displayln "=== DONE ===")
