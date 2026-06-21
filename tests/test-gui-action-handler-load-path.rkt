#lang racket/base

(require rackunit
         "../gui/main.rkt")

(module+ test
  (test-case "GUI action handler dynamic load is independent of current-directory"
    (define original-cwd (current-directory))
    (parameterize ([current-directory (find-system-path 'home-dir)])
      (check-not-equal? (current-directory) original-cwd)
      (check-true (procedure? (load-gui-action-handler-factory))))))
