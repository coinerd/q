#lang racket

(require rackunit
         "../util/config-paths.rkt")

;; ============================================================
;; project-config-dirs
;; ============================================================

(test-case "project-config-dirs returns .q then .pi"
  (define dirs (project-config-dirs "/tmp/myproject"))
  (check-equal? (length dirs) 2)
  (check-equal? (car dirs) (build-path "/tmp/myproject" ".q"))
  (check-equal? (cadr dirs) (build-path "/tmp/myproject" ".pi")))

(test-case "project-config-dirs works with relative path"
  (define dirs (project-config-dirs "."))
  (check-equal? (length dirs) 2)
  (check-true (path? (car dirs))))

;; ============================================================
;; global-config-dir
;; ============================================================

(test-case "global-config-dir returns ~/.q"
  (define dir (global-config-dir))
  (check-true (path? dir))
  (check-true (string? (path->string dir)))
  (check-true (string-contains? (path->string dir) ".q")))
