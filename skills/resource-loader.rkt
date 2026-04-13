#lang racket/base

;; skills/resource-loader.rkt — resource types, loading, parsing, and merging
;;
;; Split from skills/types.rkt (Issue #205, QUAL-09).
;; Contains:
;;   resource / resource-set structs, empty-resource-set
;;   File reading helpers (try-read-file, try-read-json, split-at-blank-line)
;;   Loading functions (load-instructions, load-skills, load-templates, load-config,
;;                      load-resources-from-dir, load-global-resources, load-project-resources)
;;   Parsing (parse-skill)
;;   Merging (merge-skill-lists, deep-merge-hash, merge-resources)

(require racket/file
         racket/port
         racket/string
         racket/hash
         racket/list
         json
         "../util/config-paths.rkt"
         (only-in "../util/hook-types.rkt"
                  hook-result? hook-result-action hook-result-payload))

(provide
 ;; Structs
 (struct-out resource)
 (struct-out resource-set)
 empty-resource-set

 ;; Resource loading
 load-global-resources
 load-project-resources
 merge-resources

 ;; File reading
 try-read-file)

;; ============================================================
;; Structs
;; ============================================================

;; A single discovered resource
(struct resource (kind name source content) #:transparent)
;; kind    : symbol — 'system-instruction | 'skill | 'prompt-template | 'config
;; name    : string — identifier
;; source  : symbol — 'global | 'project
;; content : any    — string for instructions/skills/templates, hash for config

;; A loaded collection of resources, organized by kind
(struct resource-set (instructions skills templates config) #:transparent)
;; instructions : (listof string)
;; skills       : (listof hash) — each with 'name, 'description, 'content
;; templates    : (hashof string string) — filename → content
;; config       : hash — parsed JSON config

(define (empty-resource-set)
  (resource-set '() '() (hash) (hash)))

;; ============================================================
;; Internal helpers
;; ============================================================

;; Safely read a file as a string, returning #f on failure or binary content
(define (try-read-file path)
  (with-handlers ([exn:fail:filesystem? (λ (_) #f)]
                  [exn:fail? (λ (_) #f)])
    (define bs (file->bytes path))
    ;; Skip files that contain null bytes (likely binary)
    (if (for/or ([b (in-bytes bs)])
          (= b 0))
        #f
        (bytes->string/utf-8 bs))))

;; Safely parse a JSON file, returning #f on failure
(define (try-read-json path)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (call-with-input-file path
      (λ (in)
        (define result (read-json in))
        (if (hash? result) result #f)))))

;; Note: project-config-dirs is imported from ../util/config-paths.rkt

;; Load instructions from a directory (looks for instructions.md)
(define (load-instructions dir)
  (define instr-path (build-path dir "instructions.md"))
  (define content (try-read-file instr-path))
  (if (and content (non-empty-string? (string-trim content)))
      (list (string-trim content))
      '()))

;; Split a list of strings at the first blank line
(define (split-at-blank-line lines)
  (let loop ([remaining lines] [acc '()])
    (cond
      [(null? remaining) (values (reverse acc) '())]
      [(string=? (string-trim (car remaining)) "")
       (values (reverse acc) (cdr remaining))]
      [else (loop (cdr remaining) (cons (car remaining) acc))])))

;; Parse a SKILL.md file into a hash with name, description, content
;; Format:
;;   # Skill-Name
;;
;;   Description text
;;
;;   Full content/instructions
(define (parse-skill name raw-content)
  (define lines (string-split raw-content "\n"))
  (cond
    [(null? lines)
     (hash 'name name 'description "" 'content "")]
    [else
     ;; Skip the first line if it starts with "# "
     (define after-header
       (if (string-prefix? (string-trim (car lines)) "# ")
           (cdr lines)
           lines))
     ;; Skip any leading blank lines after header
     (define rest-lines (dropf after-header
                               (λ (l) (string=? (string-trim l) ""))))
     (cond
       [(null? rest-lines)
        (hasheq 'name name 'description "" 'content "")]
       [else
        ;; Find first blank line — everything before is description,
        ;; everything after is content
        (define-values (desc-part content-part)
          (split-at-blank-line rest-lines))
        (define description (string-trim (string-join desc-part "\n")))
        (define content (string-trim (string-join content-part "\n")))
        (hasheq 'name name
              'description (if (non-empty-string? description) description "")
              'content content)])]))

;; Load skills from a skills/ subdirectory
;; Each skill is a subdirectory containing SKILL.md
(define (load-skills dir)
  (define skills-dir (build-path dir "skills"))
  (cond
    [(not (directory-exists? skills-dir)) '()]
    [else
     (define entries
       (with-handlers ([exn:fail? (λ (_) '())])
         (directory-list skills-dir)))
     (filter
      values
      (for/list ([entry (in-list entries)]
                 #:when (directory-exists? (build-path skills-dir entry)))
        (define skill-name (path->string entry))
        (define skill-file (build-path skills-dir entry "SKILL.md"))
        (define content (try-read-file skill-file))
        (and content (parse-skill skill-name content))))]))

;; Load templates from a templates/ subdirectory
(define (load-templates dir)
  (define tpl-dir (build-path dir "templates"))
  (cond
    [(not (directory-exists? tpl-dir)) (hash)]
    [else
     (define entries
       (with-handlers ([exn:fail? (λ (_) '())])
         (directory-list tpl-dir)))
     (for*/hash ([entry (in-list entries)]
                 #:when (file-exists? (build-path tpl-dir entry))
                 [content (in-value (try-read-file (build-path tpl-dir entry)))]
                 #:when content)
       (values (path->string entry) content))]))

;; Load config from config.json
(define (load-config dir)
  (define cfg-path (build-path dir "config.json"))
  (or (try-read-json cfg-path) (hash)))

;; Load all resources from a given directory (the .q or .pi dir itself)
(define (load-resources-from-dir dir #:hook-dispatcher [hook-dispatcher #f])
  ;; Dispatch 'resources-discover hook — extensions can block or amend resource discovery
  (define discover-payload (hasheq 'base-dir (path->string dir)))
  (define discover-result
    (and hook-dispatcher (hook-dispatcher 'resources-discover discover-payload)))
  (cond
    [(and (hook-result? discover-result)
          (eq? (hook-result-action discover-result) 'block))
     ;; Block: return empty resource set
     (empty-resource-set)]
    [(and (hook-result? discover-result)
          (eq? (hook-result-action discover-result) 'amend)
          (resource-set? (hook-result-payload discover-result)))
     ;; Amend: extension provided a custom resource-set
     (hook-result-payload discover-result)]
    [else
     ;; Normal: scan directory
     (resource-set (load-instructions dir)
                   (load-skills dir)
                   (load-templates dir)
                   (load-config dir))]))

;; ============================================================
;; Public API
;; ============================================================

(define (load-global-resources [base-dir (find-system-path 'home-dir)]
                            #:hook-dispatcher [hook-dispatcher #f])
  ;; Scan <base-dir>/.q/ for global resources.
  (define q-dir (car (project-config-dirs base-dir)))
  (cond
    [(directory-exists? q-dir)
     (load-resources-from-dir q-dir #:hook-dispatcher hook-dispatcher)]
    [else (empty-resource-set)]))

(define (load-project-resources [project-dir (current-directory)]
                             #:hook-dispatcher [hook-dispatcher #f])
  ;; Scan <project>/.q/ first, then fall back to <project>/.pi/
  (define dirs (project-config-dirs project-dir))
  (cond
    [(and (pair? dirs) (directory-exists? (car dirs)))
     (load-resources-from-dir (car dirs) #:hook-dispatcher hook-dispatcher)]
    [(and (>= (length dirs) 2) (directory-exists? (cadr dirs)))
     (load-resources-from-dir (cadr dirs) #:hook-dispatcher hook-dispatcher)]
    [else (empty-resource-set)]))

;; ============================================================
;; Merging
;; ============================================================

;; Merge two skill lists; project skills override global by name
(define (merge-skill-lists global-skills project-skills)
  (define proj-by-name (for/hash ([s (in-list project-skills)])
                         (values (hash-ref s 'name) s)))
  ;; Keep global skills that are not overridden by project
  (define kept-global
    (filter (λ (s) (not (hash-has-key? proj-by-name (hash-ref s 'name))))
            global-skills))
  (append kept-global project-skills))

;; Deep merge two hashes; right side wins on conflicts
(define (deep-merge-hash left right)
  (for/fold ([acc left])
            ([(k v) (in-hash right)])
    (define left-v (hash-ref acc k #f))
    (cond
      [(and left-v (hash? left-v) (hash? v))
       (hash-set acc k (deep-merge-hash left-v v))]
      [else
       (hash-set acc k v)])))

(define (merge-resources global-rs project-rs)
  ;; Merge global + project resources with project taking precedence.
  ;;
  ;; Instructions: global first, then project (both included)
  ;; Skills: project overrides global by name
  ;; Templates: project overrides global by key
  ;; Config: deep merge, project wins on conflicts
  (resource-set
   ;; Instructions: concatenate global then project
   (append (resource-set-instructions global-rs)
           (resource-set-instructions project-rs))
   ;; Skills: project overrides global by name
   (merge-skill-lists (resource-set-skills global-rs)
                      (resource-set-skills project-rs))
   ;; Templates: project overrides global by key
   (hash-union (resource-set-templates global-rs)
               (resource-set-templates project-rs)
               #:combine (λ (global-val project-val) project-val))
   ;; Config: deep merge with project precedence
   (deep-merge-hash (resource-set-config global-rs)
                    (resource-set-config project-rs))))
