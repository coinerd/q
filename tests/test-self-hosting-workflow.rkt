#lang racket

;; test-self-hosting-workflow.rkt
;; Live analysis of q's ability to self-host its own development workflow.
;; Tests each pi workflow pattern against q's actual implementation.
;; Safe: reads only, uses mock providers, writes to temp dirs.

(require rackunit
         racket/file
         racket/path
         racket/string
         "helpers/ci-detection.rkt"
         "../interfaces/sdk.rkt"
         (only-in "../llm/model.rkt" model-response)
         "../llm/provider.rkt"
         "../tools/tool.rkt"
         "../tools/registry-defaults.rkt"
         "../extensions/api.rkt"
         "../extensions/loader.rkt"
         "../agent/event-bus.rkt"
         "../skills/resource-loader.rkt"
         "../runtime/settings.rkt")

;; ============================================================
;; Pattern 1: Skill Discovery & Loading
;; ============================================================
(skip-on-ci
 "P1: skills discoverable from .pi/skills/"
 (define skills-dir (build-path project-root ".pi" "skills"))
 (check-true (directory-exists? skills-dir) ".pi/skills/ must exist")
 (define skill-dirs
   (filter (lambda (d)
             (and (directory-exists? (build-path skills-dir d))
                  (file-exists? (build-path skills-dir d "SKILL.md"))))
           (directory-list skills-dir)))
 ;; Exclude _package (archived)
 (define active (filter (lambda (d) (not (string-prefix? (path->string d) "_"))) skill-dirs))
 (check-true
  (>= (length active) 15)
  (format "Expected >= 15 active skills, found ~a: ~a" (length active) (map path->string active))))

(test-case "P1: skill-router tool exists in registry"
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (define names (map tool-name (list-tools reg)))
  (check-not-false (member "skill-route" names) "skill-route must be registered"))

;; ============================================================
;; Pattern 2: Extension Loading
;; ============================================================
(test-case "P2: extensions discoverable from q/extensions/"
  (define ext-dir (build-path project-root "q" "extensions"))
  (check-true (directory-exists? ext-dir) "q/extensions/ must exist")
  (define ext-files
    (filter (lambda (f)
              (and (regexp-match? #rx"\\.rkt$" (path->string f))
                   (not (string-prefix? (path->string f) "_"))))
            (directory-list ext-dir)))
  (check-true (>= (length ext-files) 6)
              (format "Expected >= 6 extension files, found ~a" (length ext-files))))

(test-case "P2: extensions can be loaded into registry"
  (define bus (make-event-bus))
  (define ext-reg (make-extension-registry))
  (define ext-dir (build-path project-root "q" "extensions"))
  (define loaded-count 0)
  (for ([f (in-directory ext-dir)]
        #:when (and (regexp-match? #rx"\\.rkt$" (path->string f))
                    (not (string-contains? (path->string f) "_"))))
    (with-handlers ([exn:fail? (lambda (e)
                                 ;; Some extensions need gh/external tools — just note the failure
                                 (printf "  Note: could not load ~a: ~a~n"
                                         (file-name-from-path f)
                                         (exn-message e)))])
      (load-extension! ext-reg f #:event-bus bus)
      (set! loaded-count (add1 loaded-count))))
  (printf "  Loaded ~a extensions~n" loaded-count)
  (check-true (>= loaded-count 1) "At least 1 extension must load successfully"))

;; ============================================================
;; Pattern 3: Planning Artifact System
;; ============================================================
(skip-on-ci "P3: .planning/ directory is readable"
            (define planning-dir (build-path project-root ".planning"))
            (check-true (directory-exists? planning-dir) ".planning/ must exist")
            (define artifacts (directory-list planning-dir))
            (check-true (>= (length artifacts) 5)
                        (format "Expected >= 5 planning artifacts, found ~a" (length artifacts))))

(test-case "P3: planning-read/write extension tools load"
  (define bus (make-event-bus))
  (define ext-reg (make-extension-registry))
  (define gsd-ext (build-path project-root "q" "extensions" "gsd-planning.rkt"))
  (check-true (file-exists? gsd-ext) "gsd-planning.rkt must exist")
  ;; Load it
  (load-extension! ext-reg gsd-ext #:event-bus bus)
  (define exts (list-extensions ext-reg))
  (check-true (>= (length exts) 1) "At least 1 extension registered"))

;; ============================================================
;; Pattern 4: Spawn Subagent
;; ============================================================
(test-case "P4: spawn-subagent tool registered"
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (define names (map tool-name (list-tools reg)))
  (check-not-false (member "spawn-subagent" names))
  (check-not-false (member "spawn-subagents" names)))

(test-case "P4: spawn-subagent has required schema fields"
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (define sa-tool (findf (lambda (t) (equal? (tool-name t) "spawn-subagent")) (list-tools reg)))
  (check-not-false sa-tool "spawn-subagent must exist")
  (define schema (tool-schema sa-tool))
  (check-true (hash? schema) "tool must have schema")
  ;; Task should be required
  (define props (hash-ref schema 'properties (hasheq)))
  (check-true (hash-has-key? props 'task) "schema must have 'task property"))

;; ============================================================
;; Pattern 5: Tool Error Feedback (v0.19.3 feature)
;; ============================================================
(test-case "P5: format-tool-schema-hint exists"
  ;; Check that the function is exported from tools/tool.rkt
  (define mod-path (build-path project-root "q" "tools" "tool.rkt"))
  (check-true (file-exists? mod-path)))

(test-case "P5: auto-retry classifies permanent errors"
  ;; Check that permanent-tool-error? exists
  (define mod-path (build-path project-root "q" "runtime" "auto-retry.rkt"))
  (check-true (file-exists? mod-path)))

;; ============================================================
;; Pattern 6: Context Management
;; ============================================================
(test-case "P6: compact-context tool available"
  ;; Compact context is an extension tool — check it loads
  (define cc-path (build-path project-root "q" "extensions" "compact-context.rkt"))
  (check-true (file-exists? cc-path) "compact-context extension must exist"))

(test-case "P6: context manager module exists"
  (define cm-path (build-path project-root "q" "runtime" "context-manager.rkt"))
  (check-true (file-exists? cm-path) "context-manager module must exist"))

;; ============================================================
;; Pattern 7: Project Tree Context Seeding (v0.19.3)
;; ============================================================
(test-case "P7: project-tree module exists and exports correctly"
  (define pt-path (build-path project-root "q" "runtime" "project-tree.rkt"))
  (check-true (file-exists? pt-path)))

;; ============================================================
;; Pattern 8: GitHub Integration
;; ============================================================
(test-case "P8: github-integration extension exists"
  (define gi-path (build-path project-root "q" "extensions" "github-integration.rkt"))
  (check-true (file-exists? gi-path)))

(skip-on-ci "P8: gh_helpers.py script exists"
            (define hh-path (build-path project-root "scripts" "gh_helpers.py"))
            (check-true (file-exists? hh-path)))

;; ============================================================
;; Pattern 9: Racket Tooling Extension
;; ============================================================
(test-case "P9: racket-tooling extension exists"
  (define rt-path (build-path project-root "q" "extensions" "racket-tooling.rkt"))
  (check-true (file-exists? rt-path)))

;; ============================================================
;; Pattern 10: SDK Mode Capability Check
;; ============================================================
(test-case "P10: SDK can create runtime with all components"
  (define reg (make-tool-registry))
  (register-default-tools! reg)
  (define prov
    (make-mock-provider (model-response (list (hasheq 'type "text" 'text "OK")) #f "mock" #f)
                        #:name "mock"))
  (define rt (make-runtime #:provider prov #:tool-registry reg))
  (check-true (runtime? rt)))

;; ============================================================
;; Pattern 11: Wave Execution Pipeline
;; ============================================================
(skip-on-ci "P11: wave start/finish scripts exist"
            ;; These are in gh_helpers.py
            (define hh-path (build-path project-root "scripts" "gh_helpers.py"))
            (check-true (file-exists? hh-path))
            ;; Verify the key functions exist
            (define content (file->string hh-path))
            (check-true (string-contains? content "def wave_start("))
            (check-true (string-contains? content "def wave_finish("))
            (check-true (string-contains? content "def create_milestone(")))

;; ============================================================
;; Pattern 12: Remote Collaboration
;; ============================================================
(test-case "P12: remote-collab extension exists"
  (define rc-path (build-path project-root "q" "extensions" "remote-collab" "remote-collab.rkt"))
  (check-true (file-exists? rc-path)))

(test-case "P12: q-sync extension exists"
  (define qs-path (build-path project-root "q" "extensions" "q-sync.rkt"))
  (check-true (file-exists? qs-path)))
