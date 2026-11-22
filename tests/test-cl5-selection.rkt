#lang racket

;; @speed fast  ;; @suite default
;; @boundary unit

;; tests/test-cl5-selection.rkt — CL5 W2 (#9982) deterministic selection experiment.
;;
;; Covers scripts/cl5/selection-cases.rkt and the pure parts of
;; scripts/cl5/selection-experiment.rkt:
;;   1. Every registered case is one exact, single-occurrence substitution
;;   2. Cases and their mutated files appear in the frozen CL0 corpus document
;;   3. Applying and restoring a case is byte-identical
;;   4. Classification follows the corpus vocabulary (detected/missed/true-negative/
;;      false-positive) and is symmetric where it must be
;;   5. The selector plan parser reads the shipped tool's explain output

(require rackunit
         racket/file
         racket/list
         racket/runtime-path
         racket/string
         (file "../scripts/cl5/selection-cases.rkt")
         (file "../scripts/cl5/selection-experiment.rkt"))

(define-runtime-path corpus "../docs/reports/CL0-SELECTIVE-VERIFICATION-CORPUS-003e425d.md")
(define-runtime-path repo-root "..")

(module+ test
  (test-case "every case is one single-occurrence substitution in its file"
    (for ([case (in-list selection-cases)])
      (define path (build-path repo-root (hash-ref case 'file)))
      (check-true (file-exists? path) (format "~a file exists" (hash-ref case 'id)))
      (define text (file->string path))
      (define-values (from to) (values (first (case-injection case)) (second (case-injection case))))
      (check-equal? (length (regexp-match* (regexp-quote from) text))
                    1
                    (format "~a source occurs exactly once" (hash-ref case 'id)))
      (check-not-equal? from to)
      (check-not-false (member (hash-ref case 'file) (hash-ref case 'changed)))))

  (test-case "cases are the frozen corpus cases, not invented ones"
    (define text (file->string (simplify-path corpus)))
    (for ([case (in-list selection-cases)])
      (define id (symbol->string (hash-ref case 'id)))
      (check-true (string-contains? text id) (format "corpus names ~a" id))
      (check-true (string-contains? text (hash-ref case 'file))
                  (format "corpus names the mutated file for ~a" id))))

  (test-case "apply then restore is byte-identical"
    (define case (selection-case 'D5))
    (define path (build-path repo-root (hash-ref case 'file)))
    (define before (file->string path))
    (define original (apply-case! repo-root case))
    (dynamic-wind void
                  (lambda ()
                    (check-equal? original before)
                    (define mutated (file->string path))
                    (check-not-equal? mutated before)
                    (check-true (string-contains? mutated "silent fallback")))
                  (lambda () (restore-case! repo-root case original)))
    (check-equal? (file->string path) before))

  (test-case "classification follows the corpus vocabulary"
    (check-equal? (classify-selection 1 1) 'detected)
    (check-equal? (classify-selection 0 1) 'missed)
    (check-equal? (classify-selection 0 0) 'true-negative)
    (check-equal? (classify-selection 1 0) 'false-positive))

  (test-case "timeouts are unavailable, never detection evidence"
    (check-equal? (classify-run 0 #f 0 #t) 'unavailable)
    (check-equal? (classify-run 0 #t 1 #f) 'unavailable)
    (check-equal? (classify-run 1 #f 1 #t) 'unavailable)
    (check-equal? (classify-run 0 #f 1 #f) 'missed)
    (check-equal? (classify-run 1 #f 1 #f) 'detected)
    (check-equal? (classify-run 0 #f 0 #f) 'true-negative))

  (test-case "relative executables resolve inside the worktree"
    (check-equal? (resolve-command-path "./qt" "/tmp/wt") "/tmp/wt/qt")
    (check-equal? (resolve-command-path "./qt" "/tmp/wt/") "/tmp/wt/qt")
    (check-false (string-prefix? (resolve-command-path "git" "/tmp/wt") "/tmp/wt/git"))
    (check-equal? (resolve-command-path "/bin/echo" "/tmp/wt") "/bin/echo"))

  (test-case "explain parser reads included files and escalations"
    (define text
      (string-append "changed files: 1\n"
                     "changed escalations: 1 (the whole lane is included, fail open)\n"
                     "  escalation: runner-helper-change scripts/x.rkt: affects every test\n"
                     "  included: tests/test-a.rkt (owner)\n"
                     "  included: tests/test-b.rkt (impact closure)\n"))
    (define plan (parse-explain-output text))
    (check-equal? (hash-ref plan 'selected) '("tests/test-a.rkt" "tests/test-b.rkt"))
    (check-equal? (length (hash-ref plan 'escalations)) 1)
    (check-equal? (parse-explain-output "changed files: 0\n") (hash 'selected '() 'escalations '()))))
