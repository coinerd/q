#lang racket/base

;; q/runtime/project-tree.rkt — Shallow project file tree generation
;;
;; Generates a compact file tree listing of the project directory
;; for injection into the system prompt, so the LLM can skip
;; initial exploration iterations.
;;
;; v0.19.3 Wave 3: Context Seeding with Project File Tree (C4)

(require racket/list
         racket/match
         racket/string
         racket/set
         racket/path)

(provide generate-project-tree
         project-tree->string
         DEFAULT_TREE_MAX_DEPTH
         DEFAULT_TREE_MAX_ENTRIES
         IGNORED-DIRS
         IGNORED-EXTENSIONS)

;; ============================================================
;; Configuration
;; ============================================================

(define DEFAULT_TREE_MAX_DEPTH 3)
(define DEFAULT_TREE_MAX_ENTRIES 200)

(define IGNORED-DIRS
  (set ".git" "node_modules" "__pycache__" ".racket" "compiled"
       "dist" "build" ".next" ".cache" "target" "vendor" ".tox"
       ".mypy_cache" ".pytest_cache" "egg-info" ".eggs"))

(define IGNORED-EXTENSIONS
  (set ".zo" ".dep" ".elc" ".pyc" ".o" ".so" ".class" ".jar"))

;; ============================================================
;; Tree generation
;; ============================================================

;; generate-project-tree : path? #:max-depth integer? #:max-entries integer? -> (listof string?)
;; Returns a sorted list of relative file paths within the project directory.
;; Respects max-depth and max-entry limits, ignores common noise directories.
(define (generate-project-tree project-dir
                                #:max-depth [max-depth DEFAULT_TREE_MAX_DEPTH]
                                #:max-entries [max-entries DEFAULT_TREE_MAX_ENTRIES])
  (define entries
    (let loop ([dir project-dir] [depth 0])
      (if (> depth max-depth)
          '()
          (with-handlers ([exn:fail:filesystem? (lambda (e) '())])
            (define children (directory-list dir))
            (append*
             (for/list ([child (in-list children)])
               (define full-path (build-path dir child))
               (define name (path->string child))
               (cond
                 [(and (directory-exists? full-path)
                       (set-member? IGNORED-DIRS name))
                  '()]
                 [(directory-exists? full-path)
                  (loop full-path (add1 depth))]
                 [else
                  ;; Skip ignored extensions
                  (define ext (path-get-extension child))
                  (if (and ext (set-member? IGNORED-EXTENSIONS (bytes->string/utf-8 ext)))
                      '()
                      (list (path->string (find-relative-path project-dir full-path))))])))))))
  ;; Sort and truncate
  (take (sort entries string<?) (min (length entries) max-entries)))

;; project-tree->string : path? #:max-depth integer? #:max-entries integer? -> string?
;; Generates a formatted file tree string suitable for system prompt injection.
(define (project-tree->string project-dir
                               #:max-depth [max-depth DEFAULT_TREE_MAX_DEPTH]
                               #:max-entries [max-entries DEFAULT_TREE_MAX_ENTRIES])
  (define entries (generate-project-tree project-dir
                                          #:max-depth max-depth
                                          #:max-entries max-entries))
  (define tree-lines
    (for/list ([entry (in-list entries)])
      (define parts (string-split entry "/"))
      (define depth (sub1 (length parts)))
      (define indent (make-string (* depth 2) #\space))
      (define fname (last parts))
      (format "~a~a" indent fname)))
  (if (null? tree-lines)
      ""
      (format "Project file tree:\n~a" (string-join tree-lines "\n"))))
