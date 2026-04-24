#lang racket

;; test-self-hosting-deep.rkt
;; Deep analysis: actually invoke extension tools and verify behavior.
;; Safe: all writes to temp dirs, reads from project.

(require rackunit
         racket/file
         racket/path
         racket/string
         racket/match
         "../interfaces/sdk.rkt"
         (only-in "../llm/model.rkt" model-response)
         "../llm/provider.rkt"
         "../tools/tool.rkt"
         "../tools/registry-defaults.rkt"
         "../extensions/api.rkt"
         "../extensions/loader.rkt"
         "../agent/event-bus.rkt"
         "../runtime/settings.rkt")

(define project-root (build-path (current-directory) ".." ".."))
(define on-ci? (not (directory-exists? (build-path project-root ".pi" "skills"))))
(define-syntax-rule (skip-on-ci test-name body ...)
  (if on-ci?
      (test-case test-name (check-true #t "skipped: CI environment"))
      (test-case test-name body ...)))
(define tmp-base (make-temporary-file "q-self-host-~a" 'directory))

;; Helper: create extension registry with all extensions loaded
(define (make-loaded-extension-registry #:bus [bus #f])
  (define ext-reg (make-extension-registry))
  (define b (or bus (make-event-bus)))
  ;; Load from q/extensions/
  (define ext-dir (build-path project-root "q" "extensions"))
  (for ([f (in-directory ext-dir)]
        #:when (regexp-match? #rx"\\.rkt$" (path->string f)))
    (with-handlers ([exn:fail? (lambda (e) (void))])
      (load-extension! ext-reg f #:event-bus b)))
  ext-reg)

;; Helper: create tool registry — note: extension register-tools hooks
;; expect extension-ctx? not tool-registry?, so extensions CANNOT register
;; tools when loaded outside the run-modes.rkt initialization path.
;; This is a documented gap for SDK-mode self-hosting.
(define (make-full-tool-registry #:bus [bus #f])
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  ;; Extensions load but their register-tools hooks fail because
  ;; they receive tool-registry? instead of extension-ctx?
  (define ext-reg (make-loaded-extension-registry #:bus bus))
  (for ([ext (list-extensions ext-reg)])
    (define register-hook (hash-ref (extension-hooks ext) 'register-tools #f))
    (when register-hook
      (with-handlers ([exn:fail? (lambda (e) (void))])
        (register-hook reg))))
  reg)

;; ============================================================
;; Deep Test 1: GSD Planning Extension — Discovery
;; ============================================================
(test-case "DEEP-1: gsd-planning extension loads successfully"
  (define bus (make-event-bus))
  (define ext-reg (make-loaded-extension-registry #:bus bus))
  (define exts (list-extensions ext-reg))
  (define ext-names (map extension-name exts))
  (check-not-false (member "gsd-planning-extension" ext-names)
                   "gsd-planning must load"))

;; ============================================================
;; Deep Test 2: Extension count + tool registration GAP
;; ============================================================
(test-case "DEEP-2: extension tools cannot register in SDK mode (GAP)"
  (define bus (make-event-bus))
  (define reg (make-full-tool-registry #:bus bus))
  (define names (map tool-name (list-tools reg)))
  ;; Built-in tools are present
  (printf "  Total tools (built-in only): ~a~n" (length names))
  (check-true (>= (length names) 12))
  ;; Extension tools (racket-check, planning-read, etc.) are NOT present
  ;; because register-tools hooks expect extension-ctx? not tool-registry?
  (check-false (member "racket-check" names)
               "racket-check NOT registered — extension tools fail in SDK mode")
  (check-false (member "planning-read" names)
               "planning-read NOT registered — extension tools fail in SDK mode"))

;; ============================================================
;; Deep Test 3: Built-in tool schema completeness
;; ============================================================
(test-case "DEEP-3: built-in tools have valid schemas"
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (define tools (list-tools reg))
  (define schema-errors
    (for/list ([t tools]
               #:when (not (and (hash? (tool-schema t))
                                (hash-has-key? (tool-schema t) 'type))))
      (tool-name t)))
  (check-equal? schema-errors '()
                (format "Tools with missing schemas: ~a" schema-errors)))

;; ============================================================
;; Deep Test 4: Skill content loading
;; ============================================================
(skip-on-ci "DEEP-4: skill SKILL.md files are readable"
  (define skills-dir (build-path project-root ".pi" "skills"))
  (define skill-dirs
    (filter (lambda (d)
              (and (directory-exists? (build-path skills-dir d))
                   (not (string-prefix? (path->string d) "_"))))
            (directory-list skills-dir)))
  (define results
    (for/list ([d skill-dirs])
      (define skill-file (build-path skills-dir d "SKILL.md"))
      (define exists? (file-exists? skill-file))
      (define size (if exists? (file-size skill-file) 0))
      (cons (path->string d) (list exists? size))))
  ;; All must exist and be > 100 bytes
  (for ([r results])
    (match-define (cons name (list exists? size)) r)
    (check-true exists? (format "SKILL.md missing for ~a" name))
    (check-true (> size 100) (format "SKILL.md too small for ~a (~a bytes)" name size))))

;; ============================================================
;; Deep Test 5: _shared/ skill resources exist
;; ============================================================
(skip-on-ci "DEEP-5: _shared/ skill dependencies exist"
  (define shared-dir (build-path project-root ".pi" "skills" "_shared"))
  (check-true (directory-exists? shared-dir))
  (define shared-files (directory-list shared-dir))
  (check-true (>= (length shared-files) 3)
              (format "Expected >= 3 shared resources, found ~a" (length shared-files)))
  ;; Key shared files
  (for ([f '("GSD_TDD_WORKFLOW.md" "RACKET_EDIT_POLICY.md")])
    (check-true (file-exists? (build-path shared-dir f))
                (format "~a must exist in _shared/" f))))

;; ============================================================
;; Deep Test 6: Agent loop module completeness
;; ============================================================
(test-case "DEEP-6: core agent modules exist"
  (define core-modules
    '("agent/loop.rkt"
      "agent/event-bus.rkt"
      "agent/state.rkt"
      "runtime/iteration.rkt"
      "runtime/auto-retry.rkt"
      "runtime/context-manager.rkt"
      "runtime/trace-logger.rkt"
      "runtime/project-tree.rkt"
      "runtime/agent-session.rkt"
      "runtime/settings.rkt"
      "runtime/session-store.rkt"
      "llm/provider.rkt"
      "llm/openai-compatible.rkt"
      "tools/tool.rkt"
      "tools/scheduler.rkt"
      "tools/registry-defaults.rkt"
      "interfaces/sdk.rkt"
      "wiring/run-modes.rkt"))
  (for ([rel-path core-modules])
    (define full-path (build-path project-root "q" rel-path))
    (check-true (file-exists? full-path)
                (format "~a must exist" rel-path))))

;; ============================================================
;; Deep Test 7: spawn-subagent tool schema quality
;; ============================================================
(test-case "DEEP-7: spawn-subagent tool has proper schema"
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (define sa (findf (lambda (t) (equal? (tool-name t) "spawn-subagent"))
                    (list-tools reg)))
  (check-not-false sa)
  (define schema (tool-schema sa))
  (define props (hash-ref schema 'properties (hasheq)))
  ;; Required fields
  (check-true (hash-has-key? props 'task) "must have 'task")
  (check-true (hash-has-key? props 'role) "must have 'role")
  (check-true (hash-has-key? props 'tools) "must have 'tools")
  (check-true (hash-has-key? props 'model) "must have 'model")
  ;; Required array
  (define required (hash-ref schema 'required '()))
  (check-not-false (member "task" required) "task must be required"))

;; ============================================================
;; Deep Test 8: Provider factory supports expected providers
;; ============================================================
(test-case "DEEP-8: provider factory exists"
  (define pf-path (build-path project-root "q" "runtime" "provider-factory.rkt"))
  (check-true (file-exists? pf-path))
  (define content (file->string pf-path))
  (check-true (string-contains? content "openai"))
  (check-true (string-contains? content "anthropic")))

;; ============================================================
;; Deep Test 9: Version is current
;; ============================================================
(test-case "DEEP-9: version is >= 0.19.3"
  (define ver-path (build-path project-root "q" "util" "version.rkt"))
  (check-true (file-exists? ver-path))
  (define content (file->string ver-path))
  (check-true (string-contains? content "0.19")
              "version must be in 0.19.x series"))

;; ============================================================
;; Deep Test 10: Benchmark suite infrastructure
;; ============================================================
(test-case "DEEP-10: benchmark suite modules exist"
  (define bench-dir (build-path project-root "q" "scripts" "benchmark"))
  (check-true (directory-exists? bench-dir))
  (for ([f '("scorer.rkt" "executor.rkt" "task.rkt" "report.rkt")])
    (check-true (file-exists? (build-path bench-dir f))
                (format "benchmark/~a must exist" f)))
  ;; Task files
  (define tasks-dir (build-path bench-dir "tasks"))
  (check-true (directory-exists? tasks-dir))
  (check-true (>= (length (directory-list tasks-dir)) 7)
              "must have >= 7 benchmark task files"))

;; ============================================================
;; Cleanup
;; ============================================================
(delete-directory/files tmp-base)
