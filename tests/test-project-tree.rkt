#lang racket

;;; tests/test-project-tree.rkt — Tests for runtime/project-tree.rkt
;;; v0.19.3 Wave 3: Context Seeding with Project File Tree

(require rackunit
         racket/port
         racket/file
         "../runtime/project-tree.rkt")

;; ============================================================
;; Helpers: create temp project trees
;; ============================================================

(define (make-temp-project-tree entries)
  (define tmp-dir (make-temporary-file "q-tree-test-~a" 'directory))
  (for ([entry (in-list entries)])
    (define full-path (build-path tmp-dir entry))
    (make-directory* (path-only full-path))
    (if (string-suffix? entry "/")
        (make-directory* full-path)
        (call-with-output-file full-path void)))
  tmp-dir)

(define (cleanup tmp-dir)
  (with-handlers ([exn:fail? void])
    (delete-directory/files tmp-dir)))

;; ============================================================
;; Tests
;; ============================================================

(test-case "generate-project-tree: flat directory"
  (define tmp (make-temp-project-tree '("foo.rkt" "bar.rkt" "README.md")))
  (define tree (generate-project-tree tmp))
  (check-equal? (sort tree string<?) '("README.md" "bar.rkt" "foo.rkt"))
  (cleanup tmp))

(test-case "generate-project-tree: nested directories"
  (define tmp (make-temp-project-tree '("a.rkt" "src/b.rkt" "src/sub/c.rkt")))
  (define tree (generate-project-tree tmp))
  (check-not-false (member "a.rkt" tree))
  (check-not-false (member "src/b.rkt" tree))
  (check-not-false (member "src/sub/c.rkt" tree))
  (cleanup tmp))

(test-case "generate-project-tree: ignores .git and node_modules"
  (define tmp (make-temp-project-tree '("foo.rkt" ".git/config" "node_modules/bar.js")))
  (define tree (generate-project-tree tmp))
  (check-equal? tree '("foo.rkt"))
  (cleanup tmp))

(test-case "generate-project-tree: respects max-depth"
  (define tmp (make-temp-project-tree '("a.rkt" "src/b.rkt" "src/sub/c.rkt" "src/sub/deep/d.rkt")))
  (define tree (generate-project-tree tmp #:max-depth 2))
  ;; depth 2 means: root, src, src/sub — src/sub/deep is depth 3
  (check-not-false (member "a.rkt" tree))
  (check-not-false (member "src/b.rkt" tree))
  (check-not-false (member "src/sub/c.rkt" tree))
  (check-false (member "src/sub/deep/d.rkt" tree))
  (cleanup tmp))

(test-case "generate-project-tree: respects max-entries"
  (define entries (for/list ([i (in-range 50)]) (format "file~a.rkt" i)))
  (define tmp (make-temp-project-tree entries))
  (define tree (generate-project-tree tmp #:max-entries 10))
  (check-equal? (length tree) 10)
  (cleanup tmp))

(test-case "generate-project-tree: ignores .zo files"
  (define tmp (make-temp-project-tree '("foo.rkt" "compiled/foo_rkt.zo" "compiled/foo_rkt.dep")))
  (define tree (generate-project-tree tmp))
  (check-equal? tree '("foo.rkt"))
  (cleanup tmp))

(test-case "generate-project-tree: empty directory"
  (define tmp (make-temporary-file "q-tree-empty-~a" 'directory))
  (define tree (generate-project-tree tmp))
  (check-equal? tree '())
  (delete-directory tmp))

(test-case "project-tree->string: produces formatted output"
  (define tmp (make-temp-project-tree '("a.rkt" "src/b.rkt")))
  (define str (project-tree->string tmp))
  (check-true (string-contains? str "Project file tree:"))
  (check-true (string-contains? str "a.rkt"))
  (check-true (string-contains? str "b.rkt"))
  (check-false (string-contains? str "src/"))  ;; only shows filenames with indent
  (cleanup tmp))

(test-case "project-tree->string: empty dir returns empty string"
  (define tmp (make-temporary-file "q-tree-empty2-~a" 'directory))
  (define str (project-tree->string tmp))
  (check-equal? str "")
  (delete-directory tmp))
