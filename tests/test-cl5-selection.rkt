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

  (test-case "run-command waits for process exit, not just reader EOF"
    ;; The child closes stdout immediately but keeps running and later exits 7:
    ;; an EOF-triggered implementation would report the command as finished.
    (define-values (exit-code _elapsed _out _err timed-out?)
      (run-command (list "sh" "-c" "exec 1>&-; sleep 2; exit 7") #:cwd "/tmp" #:timeout-seconds 30))
    (check-equal? exit-code 7)
    (check-false timed-out?))

  (test-case "a hung command times out and is killed"
    (define-values (exit-code _elapsed _out _err timed-out?)
      (run-command (list "sh" "-c" "sleep 30") #:cwd "/tmp" #:timeout-seconds 1))
    (check-true timed-out?)
    (check-true (not (equal? exit-code 0))))

  (test-case "the defect state refreshes admission once and restores the tree"
    ;; A minimal tree holding the real module text so the substitution applies.
    (define tree (make-temporary-file "cl5-adm-~a" 'directory))
    (define module-rel "extensions/gsd/wave-status.rkt")
    (define source (file->string (build-path repo-root module-rel)))
    (define calls '())
    (define (record! kind detail) (set! calls (cons (list kind detail) calls)) #t)
    (dynamic-wind
     void
     (lambda ()
       (make-directory* (build-path tree "extensions" "gsd"))
       (call-with-output-file (build-path tree module-rel)
                              (lambda (out) (display source out)))
       (parameterize ([current-admission-refresh (lambda (wt) (record! 'refresh (path->string wt)))]
                      [current-derived-restore (lambda (wt) (record! 'restore (path->string wt)))]
                      [current-case-runner
                       (lambda (wt case #:label label)
                         (record! 'run (list label (file->string (build-path wt module-rel)))) )])
         (define case (selection-case 'D5))
         (define original (apply-case! tree case))
         ((current-admission-refresh) tree)
         ((current-case-runner) tree case #:label 'defect)
         (restore-case! tree case original)
         ((current-derived-restore) tree))
       (define kinds (map car calls))
       (check-equal? (length (filter (lambda (kind) (eq? kind 'refresh)) kinds)) 1)
       (check-equal? (length (filter (lambda (kind) (eq? kind 'restore)) kinds)) 1)
       ;; The refresh ran while the injected text was present.
       (define run-entry (for/first ([c (in-list calls)] #:when (eq? (car c) 'run)) c))
       (check-true (string-contains? (second (second run-entry)) "silent fallback"))
       (check-equal? (file->string (build-path tree module-rel)) source))
     (lambda () (delete-directory/files tree))))

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
    (check-equal? (parse-explain-output "changed files: 0\n") (hash 'selected '() 'escalations '())))
  (test-case "selected-failures surfaces only failure lines, bounded"
    (define text
      (string-append "qt: quick passed: 3 files\n"
                     "raco test: (submodule) test\n"
                     "FAILURE\n"
                     "name: check-equal?\n"
                     "raco test: failed\n"))
    (define failures (selected-failures text))
    (check-equal? (length failures) 2)
    (check-not-false (member "FAILURE" failures))
    (check-equal? (length (selected-failures (apply string-append (make-list 60 "failed\n")))) 20))

  (test-case "expected-scope-check reports a missing documented owner test"
    (define case (hash 'expect-tests (list "tests/test-gsd-wave-status.rkt")))
    (define covered (expected-scope-check case (list "tests/test-gsd-wave-status.rkt") '()))
    (check-true (hash-ref covered 'covered?))
    (check-equal? (hash-ref covered 'not-selected) '())
    (check-false (hash-ref covered 'escalation-reported?))
    (define missed (expected-scope-check case (list "tests/test-other.rkt") '()))
    (check-false (hash-ref missed 'covered?))
    (check-equal? (hash-ref missed 'not-selected) (list "tests/test-gsd-wave-status.rkt"))
    (check-false (hash-ref (expected-scope-check (hash) '() '()) 'covered?)))

  (test-case "derived files restore from the pinned source, index included"
    ;; Regression for the D5 provenance defect: `git checkout -- <file>`
    ;; restores from the INDEX, so a staged refresh survived and the committed
    ;; tree carried two files. The restore must reset index AND worktree.
    (define repo (make-temporary-file "cl5-restore-~a" 'directory))
    (dynamic-wind
     void
     (lambda ()
       (make-directory* (build-path repo "tests"))
       (for ([rel (in-list (list "tests/.quick-admission.rktd" "tests/.coverage-manifest.json"))])
         (call-with-output-file (build-path repo rel)
                                (lambda (out) (display "original\n" out))))
       (define (git . args)
         (define-values (exit-code o e)
           (let-values ([(proc out in err)
                         (parameterize ([current-directory repo])
                           (apply subprocess #f #f #f (or (find-executable-path "git") "git") args))])
             (close-output-port in)
             (define out-str (port->string out))
             (define err-str (port->string err))
             (subprocess-wait proc)
             (values (subprocess-status proc) out-str err-str)))
         (unless (equal? exit-code 0)
           (error 'git "git ~a failed: ~a ~a" args o e))
         (string-trim o))
       (git "-c" "user.email=t@example.invalid" "-c" "user.name=t" "init" "-q")
       (git "-c" "user.email=t@example.invalid" "-c" "user.name=t" "add" "-A")
       (git "-c" "user.email=t@example.invalid" "-c" "user.name=t" "commit" "-q" "-m" "base")
       ;; Refresh (modify) and stage, exactly like the admission refresh does.
       (for ([rel (in-list (list "tests/.quick-admission.rktd" "tests/.coverage-manifest.json"))])
         (call-with-output-file (build-path repo rel)
                                (lambda (out) (display "refreshed\n" out))
                                #:exists 'truncate))
       (git "add" "-A")
       (restore-derived-files! repo)
       (check-equal? (git "status" "--porcelain") "")
       (check-equal? (file->string (build-path repo "tests/.quick-admission.rktd")) "original\n")
       (check-equal? (git "diff" "--cached" "--name-only") ""))
     (lambda () (delete-directory/files repo))))
)
