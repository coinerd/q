#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-github-milestone-ops.rkt — tests for milestone/board handler sub-module

(require rackunit
         rackunit/text-ui
         json
         (only-in "../extensions/github/handlers/milestone-ops.rkt" milestone-create-from-spec))

(define milestone-ops-tests
  (test-suite "GitHub Milestone Ops"

    (test-case "milestone-create-from-spec: missing file returns error"
      (define result (milestone-create-from-spec "/nonexistent/path/spec.json" #f))
      (check-not-false result))

    (test-case "milestone-create-from-spec: dry run reads valid spec"
      (define tmp-file (make-temporary-file "spec-~a.json"))
      (call-with-output-file
       tmp-file
       (lambda (out)
         (write-json (hasheq 'milestones (list (hasheq 'title "v1.0" 'description "First release")))
                     out))
       #:exists 'truncate)
      (define result (milestone-create-from-spec tmp-file #t))
      (check-not-false result)
      (delete-file tmp-file))))

(module+ main
  (run-tests milestone-ops-tests))
