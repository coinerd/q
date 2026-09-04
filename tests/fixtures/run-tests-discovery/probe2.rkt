#lang racket
(require "../../../scripts/run-tests/classify.rkt")
(define tests (path->string (simplify-path (build-path (current-directory) "tests"))))
(define (probe name fstr)
  (printf "~a\n  support?: ~s\n  meta: ~s\n"
          name
          (support-test-module? fstr)
          (with-handlers ([exn:fail? (lambda (e) (exn-message e))])
            (get-file-metadata fstr))))
(probe "alpha" (string-append tests "/alpha-heuristic-test.rkt"))
(probe "zulu-not" (string-append tests "/zulu-not-test.rkt"))
(probe "nested-eta" (string-append tests "/nested/eta-nested-test.rkt"))
(probe "helpers" (string-append tests "/helpers/event-simulator.rkt"))
