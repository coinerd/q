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
         (only-in "../../util/error-sanitizer.rkt" sanitize-error-message)
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
                (filter values
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
                               (make-error-result (sanitize-error-message
                                                   (format "skill-route error: ~a" (exn-message e)))))])
    (cond
      ;; list: return all skills with name + description
      [(string=? action "list")
       (define skills (discover-skills))
       (define summaries
         (for/list ([s (in-list skills)])
           (hasheq 'name (hash-ref s 'name "?") 'description (hash-ref s 'description ""))))
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

      [else
       (make-error-result (format "Unknown action: ~a. Valid: list, match, load, recommend, context"
                                  action))])))
