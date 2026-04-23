#lang racket/base

;; tests/test-extension-catalog.rkt — Tests for runtime/extension-catalog
;;
;; Wave 2 of v0.17.6: Extension catalog module tests.

(require rackunit
         racket/file
         racket/path
         "../runtime/extension-catalog.rkt"
         "../extensions/api.rkt"
         "../agent/event-bus.rkt")

;; ============================================================
;; known-extensions-dir
;; ============================================================

(test-case "known-extensions-dir returns a path"
  (define dir (known-extensions-dir))
  (check-true (path? dir))
  (check-equal? (path->string (file-name-from-path dir)) "extensions"))

(test-case "known-extensions-dir falls back when config unset"
  ;; ~/.q/config.json doesn't exist or has no extensions.source_dir
  (define dir (known-extensions-dir))
  (check-true (path? dir)))

;; ============================================================
;; list-known-extensions
;; ============================================================

(test-case "list-known-extensions returns non-empty list"
  (define exts (list-known-extensions))
  (check-true (list? exts))
  (check-true (> (length exts) 0))
  ;; Each entry should be an ext-info struct
  (for ([e (in-list exts)])
    (check-true (ext-info? e))
    (check-true (string? (ext-info-name e)))))

(test-case "list-known-extensions includes known extensions"
  (define exts (list-known-extensions))
  (define names (map ext-info-name exts))
  ;; These should be present in the source tree
  (check-not-false (member "gsd-planning" names) "gsd-planning extension found")
  (check-not-false (member "session-export" names) "session-export extension found")
  (check-not-false (member "github-integration" names) "github-integration extension found"))

(test-case "list-known-extensions excludes infrastructure modules"
  (define exts (list-known-extensions))
  (define names (map ext-info-name exts))
  ;; These are framework files, not extensions
  (check-false (member "api" names) "api.rkt is not an extension")
  (check-false (member "loader" names) "loader.rkt is not an extension")
  (check-false (member "context" names) "context.rkt is not an extension")
  (check-false (member "hooks" names) "hooks.rkt is not an extension"))

(test-case "list-known-extensions includes subdirectory extensions"
  (define exts (list-known-extensions))
  (define names (map ext-info-name exts))
  ;; remote-collab lives in extensions/remote-collab/remote-collab.rkt
  (check-not-false (member "remote-collab" names) "remote-collab subdirectory extension found"))

;; ============================================================
;; list-active-extensions
;; ============================================================

(test-case "list-active-extensions returns empty for empty registry"
  (define reg (make-extension-registry))
  (define exts (list-active-extensions reg))
  (check-equal? exts '()))

;; ============================================================
;; activate-extension! and deactivate-extension!
;; ============================================================

(test-case "activate-extension! creates symlink"
  (define tmp-dir (make-temporary-file "q-activate-test-~a" 'directory))
  (define target-dir (build-path tmp-dir "extensions"))
  (make-directory* target-dir)
  ;; Activate a known extension
  (activate-extension! "gsd-planning" target-dir)
  ;; Check symlink was created
  (define link-path (build-path target-dir "gsd-planning.rkt"))
  (check-true (or (link-exists? link-path) (file-exists? link-path)))
  ;; Cleanup
  (delete-directory/files tmp-dir))

(test-case "activate-extension! is idempotent"
  (define tmp-dir (make-temporary-file "q-activate-idem-~a" 'directory))
  (define target-dir (build-path tmp-dir "extensions"))
  (make-directory* target-dir)
  ;; Activate twice — no error
  (activate-extension! "gsd-planning" target-dir)
  (activate-extension! "gsd-planning" target-dir)
  ;; Still just one file
  (check-equal? (length (directory-list target-dir)) 1)
  ;; Cleanup
  (delete-directory/files tmp-dir))

(test-case "activate-extension! handles subdirectory extension"
  (define tmp-dir (make-temporary-file "q-activate-sub-~a" 'directory))
  (define target-dir (build-path tmp-dir "extensions"))
  (make-directory* target-dir)
  ;; remote-collab is a subdirectory extension
  (activate-extension! "remote-collab" target-dir)
  (define link-path (build-path target-dir "remote-collab.rkt"))
  (check-true (or (link-exists? link-path) (file-exists? link-path)))
  ;; Cleanup
  (delete-directory/files tmp-dir))

(test-case "activate-extension! errors on unknown extension"
  (define tmp-dir (make-temporary-file "q-activate-unk-~a" 'directory))
  (define target-dir (build-path tmp-dir "extensions"))
  (make-directory* target-dir)
  (check-exn exn:fail? (lambda () (activate-extension! "nonexistent-extension" target-dir)))
  ;; Cleanup
  (delete-directory/files tmp-dir))

(test-case "deactivate-extension! removes symlink"
  (define tmp-dir (make-temporary-file "q-deact-test-~a" 'directory))
  (define target-dir (build-path tmp-dir "extensions"))
  (make-directory* target-dir)
  ;; Activate then deactivate
  (activate-extension! "gsd-planning" target-dir)
  (define link-path (build-path target-dir "gsd-planning.rkt"))
  (check-true (or (link-exists? link-path) (file-exists? link-path)))
  (deactivate-extension! "gsd-planning" target-dir)
  (check-false (or (link-exists? link-path) (file-exists? link-path)))
  ;; Cleanup
  (delete-directory/files tmp-dir))

(test-case "deactivate-extension! is idempotent"
  (define tmp-dir (make-temporary-file "q-deact-idem-~a" 'directory))
  (define target-dir (build-path tmp-dir "extensions"))
  (make-directory* target-dir)
  ;; Deactivate something that was never activated — no error
  (deactivate-extension! "gsd-planning" target-dir)
  (deactivate-extension! "gsd-planning" target-dir)
  ;; Cleanup
  (delete-directory/files tmp-dir))
