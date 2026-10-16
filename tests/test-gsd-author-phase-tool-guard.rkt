#lang racket
;; @covers extensions/gsd-planning/execution-policy.rkt
;; @speed fast
;; @suite extensions
;; @boundary unit
(require rackunit rackunit/text-ui racket/file racket/runtime-path
         "../extensions/gsd-planning/execution-policy.rkt"
         (only-in "../extensions/hooks.rkt" hook-result-action))

(define-runtime-path policy-path "../extensions/gsd-planning/execution-policy.rkt")
(define phase-parameter
  (dynamic-require policy-path
                   'current-gsd-author-phase (lambda () #f)))
(define tests
  (test-suite
   "author phase admission"
   (test-case "trusted phase parameter is exported"
     (check-true (parameter? phase-parameter)))
   (test-case "both phases admit exactly their frozen paths"
     (check-true (parameter? phase-parameter))
     (when (parameter? phase-parameter)
       (define repo (make-temporary-file "author~a" 'directory))
       (define outside (make-temporary-file "outside~a" 'directory))
       (dynamic-wind
        void
        (lambda ()
          (make-directory (build-path repo "tests"))
          (make-directory (build-path repo ".git"))
          (make-directory (build-path repo ".planning"))
          (display-to-file "test" (build-path repo "tests/a.rkt"))
          (display-to-file "impl" (build-path repo "a.rkt"))
          (make-file-or-directory-link outside (build-path repo "link"))
          (make-file-or-directory-link (build-path repo "a.rkt") (build-path repo "alias"))
          (for ([phase '(test-author implementation)])
            (define scope
              (hasheq 'phase phase 'repo repo
                      'files '("a.rkt" "new.rkt" "tests/a.rkt" "tests/new.rkt")
                      'tests '("tests/a.rkt" "tests/new.rkt")))
            (define (action tool [path #f] [s scope])
              (parameterize ([phase-parameter s] [current-directory repo])
                (hook-result-action
                 (gsd-tool-guard (hasheq 'tool-name tool 'tool-arguments (hasheq 'path path))))))
            (for ([tool '("read" "grep" "find" "ls" "glob")])
              (check-eq? (action tool) 'pass))
            (for ([tool '("bash" "python" "subagent" "spawn_subagents" "custom-write" "planning-write" #f)])
              (check-eq? (action tool) 'block))
            (for* ([tool '("write" "edit")]
                   [p '("a.rkt" "new.rkt" "tests/a.rkt" "tests/new.rkt")]
                   [absolute? '(#f #t)])
              (check-eq? (action tool (if absolute? (build-path repo p) p))
                         (if (eq? (regexp-match? #rx"^tests/" p) (eq? phase 'test-author)) 'pass 'block)))
            (for ([p (list "../escape" (build-path outside "x") "tests/../../escape"
                           ".git/config" ".planning/PLAN.md" "link/x" "alias" #f)])
              (check-eq? (action "write" p) 'block))
            (for ([p '("link/x" "alias" ".git/config" ".planning/PLAN.md" "../escape")])
              (check-eq? (action "write" p (hasheq 'phase phase 'repo repo 'files (list p) 'tests (list p))) 'block))
            (for ([bad (list (hasheq) (hasheq 'phase 'other 'repo repo 'files '() 'tests '())
                             (hasheq 'phase phase 'repo "relative" 'files '() 'tests '())
                             (hasheq 'phase phase 'repo repo 'files '(3) 'tests '()))])
              (check-eq? (action "read" #f bad) 'block))))
        (lambda () (delete-directory/files repo) (delete-directory/files outside)))))))
(module+ main (exit (if (zero? (run-tests tests)) 0 1)))
