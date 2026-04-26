#!/usr/bin/env racket
#lang racket

;; scripts/test-gsd-sdk-live.rkt — Live GSD planning test via SDK
;;
;; Exercises the /plan → /go workflow against the real LLM provider
;; from ~/.q/config.json, observing GSD state transitions.

(require racket/port
         racket/string
         "../runtime/settings.rkt"
         (only-in "../llm/provider.rkt" provider-name)
         "../runtime/provider-factory.rkt"
         "../extensions/loader.rkt"
         "../extensions/hooks.rkt"
         (only-in "../extensions/api.rkt"
                  make-extension-registry
                  register-extension!
                  list-extensions
                  extension-name)
         "../agent/event-bus.rkt"
         (only-in "../util/event.rkt" event-event event-payload)
         "../tools/tool.rkt"
         "../tools/registry-defaults.rkt"
         "../interfaces/sdk.rkt"
         "../extensions/gsd-planning-state.rkt"
         "../util/cancellation.rkt")

(define (fmt-hash h)
  (if (hash? h)
      (string-join (for/list ([(k v) (in-hash h)])
                     (format "~a=~a" k v))
                   ", ")
      ""))

;; ============================================================
;; 1. Build real provider + tools + extensions
;; ============================================================
(displayln "=== Step 1: Setup ===")
(define project-dir (current-directory))
(define settings (load-settings project-dir))
(define prov (build-provider (hasheq 'project-dir project-dir) settings))
(displayln (format "  Provider: ~a" (provider-name prov)))

(define bus (make-event-bus))
(define tool-reg (make-tool-registry))
(register-default-tools! tool-reg)
(displayln (format "  Tools: ~a registered" (length (list-tools tool-reg))))

(define ext-reg (make-extension-registry))
(for ([ext (discover-extensions project-dir)])
  (register-extension! ext-reg ext))
(displayln (format "  Extensions: ~a"
                   (string-join (map extension-name (list-extensions ext-reg)) ", ")))

;; Event subscriber
(define event-log (box '()))
(subscribe! bus (λ (evt) (set-box! event-log (append (unbox event-log) (list evt)))))

;; Clean stale artifacts
(reset-all-gsd-state!)
(define plan-path (build-path project-dir ".planning" "PLAN.md"))
(when (file-exists? plan-path)
  (delete-file plan-path)
  (displayln "  Deleted stale PLAN.md"))

(define rt
  (make-runtime #:provider prov
                #:tool-registry tool-reg
                #:extension-registry ext-reg
                #:event-bus bus
                #:max-iterations 25
                #:session-dir "/tmp/q-gsd-sdk-test"))

;; ============================================================
;; 2. Open session
;; ============================================================
(displayln "")
(displayln "=== Step 2: Open session ===")
(define rt-open (open-session rt))
(displayln (format "  GSD: ~a" (gsd-status)))
(displayln "")

;; ============================================================
;; 3. /plan — dispatch hook, then run prompt
;; ============================================================
(displayln "════════════════════════════════════════════════════════")
(displayln "=== Step 3: /plan ===")
(displayln "════════════════════════════════════════════════════════")

(define plan-input "/plan Create a file /tmp/hello.txt containing just 'Hello World'")
(define plan-hook
  (dispatch-hooks 'execute-command (hasheq 'command "/plan" 'input plan-input) ext-reg))
(define plan-prompt
  (if (and plan-hook
           (eq? (hook-result-action plan-hook) 'amend)
           (hash-ref (hook-result-payload plan-hook) 'submit #f))
      (hash-ref (hook-result-payload plan-hook) 'submit)
      plan-input))

(displayln (format "  Prompt: ~a chars" (string-length plan-prompt)))
(displayln "  Running...")
(displayln "")

(define-values (rt-after-plan plan-result) (run-prompt! rt-open plan-prompt))

(displayln "")
(displayln "=== After /plan ===")
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
;; 4. /go — dispatch hook, then run prompt
;; ============================================================
(displayln "════════════════════════════════════════════════════════")
(displayln "=== Step 4: /go ===")
(displayln "════════════════════════════════════════════════════════")

(define go-hook (dispatch-hooks 'execute-command (hasheq 'command "/go" 'input "/go") ext-reg))
(define go-prompt
  (if (and go-hook
           (eq? (hook-result-action go-hook) 'amend)
           (hash-ref (hook-result-payload go-hook) 'submit #f))
      (hash-ref (hook-result-payload go-hook) 'submit)
      "/go"))

(displayln (format "  Prompt: ~a chars" (string-length go-prompt)))
(displayln "  Running...")
(displayln "")

(define-values (rt-after-go go-result) (run-prompt! rt-after-plan go-prompt))

(displayln "")
(displayln "=== After /go ===")
(define snap2 (gsd-snapshot))
(displayln (format "  mode: ~a" (hash-ref snap2 'mode)))
(displayln (format "  total-waves: ~a" (hash-ref snap2 'total-waves)))
(displayln (format "  completed-waves: ~a" (set-count (hash-ref snap2 'completed-waves))))
(displayln (format "  go-read-budget: ~a" (hash-ref snap2 'go-read-budget)))
(displayln (format "  edit-limit: ~a" (hash-ref snap2 'edit-limit)))
(displayln (format "  read-counts: ~a files" (hash-count (hash-ref snap2 'read-counts))))
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
(define names (remove-duplicates (map event-event (unbox event-log))))
(displayln (format "  Event types: ~a" names))
(displayln (format "  Total events: ~a" (length (unbox event-log))))
(displayln "")
(displayln "=== DONE ===")
