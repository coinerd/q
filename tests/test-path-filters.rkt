#lang racket

(require rackunit
         rackunit/text-ui
         racket/path
         "../util/path-filters.rkt")

(define path-filters-suite
  (test-suite
   "path-filters tests"

   (test-case "hidden-name? detects dot-prefixed names"
     (check-true (hidden-name? ".git"))
     (check-true (hidden-name? ".bashrc"))
     (check-false (hidden-name? "README.md"))
     (check-false (hidden-name? "")))

   (test-case "vcs-dir? recognizes known VCS/noise dirs"
     (check-not-false (vcs-dir? ".git"))
     (check-not-false (vcs-dir? "node_modules"))
     (check-false (vcs-dir? "src")))

   (test-case "should-skip-entry? skips VCS dirs always"
     (check-not-false (should-skip-entry? ".git"))
     (check-not-false (should-skip-entry? "node_modules")))

   (test-case "should-skip-entry? skips hidden unless show-hidden"
     (check-true (should-skip-entry? ".env"))
     (check-false (should-skip-entry? ".env" #:show-hidden? #t)))

   (test-case "should-skip-entry? does not skip regular names"
     (check-false (should-skip-entry? "src"))
     (check-false (should-skip-entry? "main.rkt")))

   (test-case "path-component-hidden? detects hidden path segment"
     (check-true (path-component-hidden? (string->path "/home/user/.config/app")))
     (check-false (path-component-hidden? (string->path "/home/user/config/app"))))

   (test-case "skip-dirs is a non-empty list"
     (check-true (and (list? skip-dirs) (pair? skip-dirs))))))

(run-tests path-filters-suite)
