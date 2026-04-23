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

(require racket/file
         racket/string
         racket/list
         json
         "../../skills/resource-loader.rkt"
         "../../util/config-paths.rkt"
         "../../tools/tool.rkt")

(provide tool-skill-route)

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
              (with-handlers ([exn:fail? (lambda (_) '())])
                (filter
                 values
                 (for/list ([entry (in-list (directory-list skills-dir))]
                            #:when (directory-exists? (build-path skills-dir entry)))
                   (define skill-name (path->string entry))
                   (define skill-file (build-path skills-dir entry "SKILL.md"))
                   (define content (try-read-file skill-file))
                   (and content (parse-skill skill-name content)))))]))))

;; ============================================================
;; Tool handler
;; ============================================================

(define (tool-skill-route args [exec-ctx #f])
  (define action (hash-ref args 'action "list"))
  (with-handlers ([exn:fail? (lambda (e)
                               (make-error-result (format "skill-route error: ~a" (exn-message e))))])
    (cond
      ;; list: return all skills with name + description
      [(string=? action "list")
       (define skills (discover-skills))
       (define summaries
         (for/list ([s (in-list skills)])
           (hasheq 'name (hash-ref s 'name "?")
                   'description (hash-ref s 'description ""))))
       (make-success-result
        (list (hasheq 'type "text"
                       'text (jsexpr->string summaries))))]

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
                   (or (string-contains? name query-low)
                       (string-contains? desc query-low)))
                 skills))
       (define results
         (for/list ([s (in-list matched)])
           (hasheq 'name (hash-ref s 'name "?")
                   'description (hash-ref s 'description "")
                   'matched #t)))
       (make-success-result
        (list (hasheq 'type "text"
                       'text (format "Found ~a matching skills:\n~a"
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
         [found
          (make-success-result
           (list (hasheq 'type "text"
                          'text (hash-ref found 'content ""))))]
         [else
          (make-error-result (format "Skill not found: ~a" name))])]

      [else
       (make-error-result
        (format "Unknown action: ~a. Valid: list, match, load" action))])))
