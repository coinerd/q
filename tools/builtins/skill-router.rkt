#lang racket/base

;; tools/builtins/skill-router.rkt — skill discovery and routing tool
;;
;; Provides:
;;   tool-skill-route — handler for the "skill-route" tool
;;
;; Actions:
;;   list   — list all discovered skills with name + description
;;   match  — search skills by query text matching name or description
;;   load   — return full content of a named skill
;;
;; Scans .q/skills/ and .pi/skills/ directories from the working directory.
;; This is a read-only discovery tool — no modification.

(require "../../util/error/error-helpers.rkt"
         racket/runtime-path)
(require racket/contract
         racket/file
         racket/string
         racket/list
         json
         "../../skills/resource-loader.rkt"
         "../../skills/frontmatter.rkt"
         "../../util/config-paths.rkt"
         (only-in "../../util/error/error-sanitizer.rkt" sanitize-error-message)
         "../../tools/tool.rkt")

(provide (contract-out [tool-skill-route (->* (hash?) (any/c) any/c)]))

;; Stable module-path indexes for workflow support modules.
;; These resolve relative to this source module at runtime, not relative to
;; the process current-directory, fixing B-1 (broken skill-route workflow).
(define-runtime-module-path-index mas-workflow-module "../../skills/mas-workflow.rkt")
(define-runtime-module-path-index workflow-executor-module "../../skills/workflow-executor.rkt")

;; ============================================================
;; Skill discovery helpers
;; ============================================================

;; Parse a SKILL.md file into a hash with name, description, content.
;; Reuses the exported parse-skill from resource-loader.
(define (discover-skills [project-dir (current-directory)])
  (define dirs (project-config-dirs project-dir))
  (apply append
         (for/list ([dir (in-list dirs)]
                    #:when (directory-exists? dir))
           (define skills-dir (build-path dir "skills"))
           (cond
             [(not (directory-exists? skills-dir)) '()]
             [else
              (with-safe-fallback
               '()
               (filter
                values
                (for/list ([entry (in-list (directory-list skills-dir))]
                           #:when (directory-exists? (build-path skills-dir entry)))
                  (define skill-name (path->string entry))
                  (define skill-file (build-path skills-dir entry "SKILL.md"))
                  (define content (try-read-file skill-file))
                  (and content
                       (hash-set (parse-skill skill-name content) 'raw-content content)))))]))))

;; ============================================================
;; Tool handler
;; ============================================================

(define (tool-skill-route args [exec-ctx #f])
  (define action (hash-ref args 'action "list"))
  (with-handlers ([exn:fail? (lambda (e)
                               (make-error-result (sanitize-error-message
                                                   (format "skill-route error: ~a"
                                                           (exn-message e)))))])
    (cond
      ;; list: return all skills with name + description + type
      [(string=? action "list")
       (define skills (discover-skills))
       (define summaries
         (for/list ([s (in-list skills)])
           (define raw-content (hash-ref s 'raw-content (hash-ref s 'content "")))
           (define fm (parse-skill-frontmatter-extended raw-content))
           (define skill-type (and fm (hash-ref fm 'type #f)))
           (hasheq 'name
                   (hash-ref s 'name "?")
                   'description
                   (hash-ref s 'description "")
                   'type
                   (or skill-type "standard"))))
       (make-success-result (list (hasheq 'type "text" 'text (jsexpr->string summaries))))]

      ;; match: search skills by query text
      [(string=? action "match")
       (define query (hash-ref args 'query ""))
       (when (string=? query "")
         (raise (exn:fail "match requires 'query'" (current-continuation-marks))))
       (define skills (discover-skills))
       (define query-low (string-downcase query))
       (define matched
         (filter (lambda (s)
                   (define name (string-downcase (hash-ref s 'name "")))
                   (define desc (string-downcase (hash-ref s 'description "")))
                   (or (string-contains? name query-low) (string-contains? desc query-low)))
                 skills))
       (define results
         (for/list ([s (in-list matched)])
           (hasheq 'name
                   (hash-ref s 'name "?")
                   'description
                   (hash-ref s 'description "")
                   'matched
                   #t)))
       (make-success-result (list (hasheq 'type
                                          "text"
                                          'text
                                          (format "Found ~a matching skills:\n~a"
                                                  (length results)
                                                  (jsexpr->string results)))))]

      ;; load: return full content of a named skill
      [(string=? action "load")
       (define name (hash-ref args 'name ""))
       (when (string=? name "")
         (raise (exn:fail "load requires 'name'" (current-continuation-marks))))
       (define skills (discover-skills))
       (define found (findf (lambda (s) (string=? (hash-ref s 'name "") name)) skills))
       (cond
         [found (make-success-result (list (hasheq 'type "text" 'text (hash-ref found 'content ""))))]
         [else (make-error-result (format "Skill not found: ~a" name))])]

      ;; recommend: return top-3 skills with confidence scores
      [(string=? action "recommend")
       (define query (hash-ref args 'query ""))
       (when (string=? query "")
         (raise (exn:fail "recommend requires 'query'" (current-continuation-marks))))
       (define skills (discover-skills))
       (define query-low (string-downcase query))
       (define query-words (string-split query-low))
       (define scored
         (sort (for/list ([s (in-list skills)])
                 (define name (string-downcase (hash-ref s 'name "")))
                 (define desc (string-downcase (hash-ref s 'description "")))
                 (define name-match (string-contains? name query-low))
                 (define desc-match (string-contains? desc query-low))
                 (define word-score
                   (/ (for/sum ([w (in-list query-words)])
                               (cond
                                 [(string-contains? name w) 2]
                                 [(string-contains? desc w) 1]
                                 [else 0]))
                      (max 1 (length query-words))))
                 (define confidence
                   (cond
                     [name-match (min 1.0 (+ 0.7 (* 0.3 word-score)))]
                     [desc-match (min 0.9 (+ 0.4 (* 0.3 word-score)))]
                     [(> word-score 0) (min 0.6 word-score)]
                     [else 0.0]))
                 (cons s confidence))
               >
               #:key cdr))
       (define non-zero (filter (lambda (p) (> (cdr p) 0)) scored))
       (define top-3 (take non-zero (min 3 (length non-zero))))
       (define recommendations
         (for/list ([p (in-list top-3)])
           (hasheq 'name
                   (hash-ref (car p) 'name "?")
                   'description
                   (hash-ref (car p) 'description "")
                   'confidence
                   (exact->inexact (cdr p)))))
       (make-success-result (list (hasheq 'type
                                          "text"
                                          'text
                                          (format "Top ~a recommendations:\n~a"
                                                  (length recommendations)
                                                  (jsexpr->string recommendations)))))]

      ;; context: return skill content plus _shared/ dependencies
      [(string=? action "context")
       (define ctx-name (hash-ref args 'name ""))
       (when (string=? ctx-name "")
         (raise (exn:fail "context requires 'name'" (current-continuation-marks))))
       (define ctx-skills (discover-skills))
       (define ctx-found (findf (lambda (s) (string=? (hash-ref s 'name "") ctx-name)) ctx-skills))
       (cond
         [ctx-found
          ;; Try to load _shared/ dependencies from skill directory
          (define shared-content
            (with-handlers ([exn:fail? (lambda (_) "")])
              (define dirs (project-config-dirs (current-directory)))
              (string-join
               (for/list ([dir (in-list dirs)]
                          #:when (directory-exists? dir))
                 (define skill-dir (build-path dir "skills" ctx-name "_shared"))
                 (cond
                   [(not (directory-exists? skill-dir)) ""]
                   [else
                    (string-join (for/list ([f (in-list (directory-list skill-dir))]
                                            #:when (and (file-exists? (build-path skill-dir f))
                                                        (regexp-match? #rx"\\.md$" (path->string f))))
                                   (define fc (try-read-file (build-path skill-dir f)))
                                   (or fc ""))
                                 "\n\n")]))
               "\n\n")))
          (define full-content
            (string-append (format "# ~a\n\n~a\n\n~a"
                                   (hash-ref ctx-found 'name "?")
                                   (hash-ref ctx-found 'description "")
                                   (hash-ref ctx-found 'content ""))
                           (if (string=? shared-content "")
                               ""
                               (format "\n\n---\nShared Dependencies:\n~a" shared-content))))
          (make-success-result (list (hasheq 'type "text" 'text full-content)))]
         [else (make-error-result (format "Skill not found: ~a" ctx-name))])]

      ;; workflow: execute a mas-workflow skill
      ;; Safety: the router itself is read-only; execution is delegated to the
      ;; workflow-executor, which runs each step through a child-safe subagent
      ;; whose tool set is filtered by the step's declared capabilities.
      ;; Uses dynamic-require to avoid circular dependency:
      ;; skill-router → workflow-executor → spawn-subagent → skill-router
      [(string=? action "workflow")
       (define wf-name (hash-ref args 'name ""))
       (when (string=? wf-name "")
         (raise (exn:fail "workflow requires 'name'" (current-continuation-marks))))
       (define wf-skills (discover-skills))
       (define wf-found (findf (lambda (s) (string=? (hash-ref s 'name "") wf-name)) wf-skills))
       (cond
         [(not wf-found) (make-error-result (format "Skill not found: ~a" wf-name))]
         [else
          (define raw-content (hash-ref wf-found 'raw-content (hash-ref wf-found 'content "")))
          (define fm (parse-skill-frontmatter-extended raw-content))
          (define wf-type (and fm (hash-ref fm 'type #f)))
          (cond
            [(not (and fm (equal? wf-type "mas-workflow")))
             (make-error-result (format "Skill '~a' is not a mas-workflow skill" wf-name))]
            [else
             ;; Lazily load workflow modules to break dependency cycle.
             ;; Module-path indexes are defined at the top level so resolution
             ;; is stable relative to this source module.
             (define parse-mas-workflow-fn (dynamic-require mas-workflow-module 'parse-mas-workflow))
             (define execute-workflow-fn (dynamic-require workflow-executor-module 'execute-workflow))
             (define variables (hash-ref args 'variables (hasheq)))
             (define-values (wf wf-err)
               (parse-mas-workflow-fn wf-name (hash-ref wf-found 'description "") fm))
             (cond
               [wf-err (make-error-result wf-err)]
               [else
                (define wf-result (execute-workflow-fn wf variables exec-ctx))
                (define wf-content (tool-result-content wf-result))
                (define wf-is-error (tool-result-is-error? wf-result))
                (define wf-msg
                  (if wf-is-error
                      (if (list? wf-content)
                          (hash-ref (car wf-content) 'text "")
                          (format "~a" wf-content))
                      (format "Workflow '~a' completed successfully.\n~a"
                              wf-name
                              (jsexpr->string wf-content))))
                (if wf-is-error
                    (make-error-result wf-msg)
                    (make-success-result (list (hasheq 'type "text" 'text wf-msg))))])])])]

      [else
       (make-error-result
        (format "Unknown action: ~a. Valid: list, match, load, recommend, context, workflow"
                action))])))
