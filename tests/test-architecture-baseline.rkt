#lang racket

;; @speed fast  ;; @suite default
;; @boundary integration
(require racket/file
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         rackunit
         "../scripts/architecture-baseline-helpers.rkt")

(define-runtime-path architecture-cli "../scripts/architecture-baseline.rkt")

(define (field entry
               key)
  (second (assoc key entry)))

(define (module-paths snapshot)
  (map (lambda (module)
         (field module
                'path))
       (field snapshot
              'modules)))

(define (checked-system* executable . arguments)
  (define stdout (open-output-string))
  (define stderr (open-output-string))
  (define ok?
    (parameterize ([current-output-port stdout]
                   [current-error-port stderr])
      (apply system* executable arguments)))
  (unless ok?
    (error 'checked-system*
           "command failed: ~a\n~a"
           (string-join arguments " ")
           (get-output-string stderr))))

(define (captured-system* executable . arguments)
  (define stdout (open-output-string))
  (define stderr (open-output-string))
  (define ok?
    (parameterize ([current-output-port stdout]
                   [current-error-port stderr])
      (apply system* executable arguments)))
  (unless ok?
    (error 'captured-system*
           "command failed: ~a\n~a"
           (string-join arguments " ")
           (get-output-string stderr)))
  (get-output-string stdout))

(module+ test
  (test-case "canonical rendering is byte-identical and hash-order independent"
    (define first-hash (make-hash))
    (define second-hash (make-hash))
    (hash-set! first-hash 'beta 2)
    (hash-set! first-hash 'alpha 1)
    (hash-set! second-hash 'alpha 1)
    (hash-set! second-hash 'beta 2)
    (let ([first-render (canonical-datum->string first-hash)])
      (check-equal? first-render (canonical-datum->string first-hash))
      (check-equal? first-render (canonical-datum->string second-hash))
      (check-not-exn (lambda () (call-with-input-string first-render read)))))
  (test-case "snapshot ordering and rendering are stable"
    (define a-source
      (string-append "#lang racket/base\n" "(require \"b.rkt\")\n" "(provide alpha beta)\n"))
    (define b-source "#lang racket/base\n(provide b)\n")
    (define test-source
      (string-append "#lang racket\n"
                     "(require rackunit \"../a.rkt\")\n"
                     "(module+ test (test-case \"works\" (check-equal? 1 1)))\n"))
    (define findings '(((id "PI-2") (status "RESOLVED")) ((id "PI-1") (status "OPEN"))))
    (define snapshot-one
      (build-architecture-snapshot
       "012345"
       (list (cons "tests/test-a.rkt" test-source) (cons "b.rkt" b-source) (cons "a.rkt" a-source))
       '()
       #:last 20
       #:findings findings))
    (define snapshot-two
      (build-architecture-snapshot
       "012345"
       (list (cons "a.rkt" a-source) (cons "tests/test-a.rkt" test-source) (cons "b.rkt" b-source))
       '()
       #:last 20
       #:findings (reverse findings)))
    (define a-module
      (first (field snapshot-one
                    'modules)))
    (define b-module
      (second (field snapshot-one
                     'modules)))
    (check-equal? (module-paths snapshot-one) '("a.rkt" "b.rkt" "tests/test-a.rkt"))
    (check-equal? (canonical-datum->string snapshot-one) (canonical-datum->string snapshot-two))
    (check-equal? (architecture-snapshot->markdown snapshot-one)
                  (architecture-snapshot->markdown snapshot-two))
    (check-equal? (field a-module
                         'provide-specs)
                  2)
    (check-equal? (field a-module
                         'dependency-fan-out)
                  1)
    (check-equal? (field b-module
                         'dependency-fan-in)
                  1)
    (check-equal? (field (first (field snapshot-one
                                       'test-inventory))
                         'checks)
                  1))
  (test-case "Part-I finding status vocabulary is enforced"
    (for ([status (in-list finding-statuses)])
      (check-true (valid-finding-status? status))
      (check-equal? (validate-finding-statuses `(((id "PI") (status ,status))))
                    `(((id "PI") (status ,status)))))
    (check-false (valid-finding-status? "CLOSED"))
    (check-exn exn:fail:contract?
               (lambda () (validate-finding-statuses '(((id "PI") (status "CLOSED")))))))
  (test-case "co-change excludes release-only commits and exact moves"
    (define log-text
      (string-append "@@@commit\t1\tregular one\n"
                     "M\ta.rkt\nM\tb.rkt\n\n"
                     "@@@commit\t2\trelease\n"
                     "M\tinfo.rkt\nM\treleases/version.rkt\n\n"
                     "@@@commit\t3\texact rename\n"
                     "R100\ta.rkt\trenamed-a.rkt\nM\tb.rkt\n\n"
                     "@@@commit\t4\texact copy\n"
                     "C100\tc.rkt\tc-copy.rkt\nM\tb.rkt\n\n"
                     "@@@commit\t5\tregular two\n"
                     "M\tb.rkt\nM\ta.rkt\nM\ta.rkt\n\n"
                     "@@@commit\t6\ttie pair\n"
                     "M\tc.rkt\nM\ta.rkt\n"))
    (define commits (parse-git-log log-text))
    (define pairs (count-co-changes commits))
    (define rendered (canonical-datum->string pairs))
    (check-equal? (length commits) 6)
    (check-equal? pairs
                  '(((path-a "a.rkt") (path-b "b.rkt") (count 2)) ((path-a "a.rkt") (path-b "c.rkt")
                                                                                    (count 1))))
    (check-false (regexp-match? #rx"renamed-a|c-copy|releases/version" rendered)))
  (test-case "path pairs use deterministic lexical tie-breaks"
    (define commits
      (list (git-commit "1" "" (list (git-change "M" '("z.rkt")) (git-change "M" '("a.rkt"))))
            (git-commit "2" "" (list (git-change "M" '("m.rkt")) (git-change "M" '("a.rkt"))))))
    (check-equal? (count-co-changes commits)
                  '(((path-a "a.rkt") (path-b "m.rkt") (count 1)) ((path-a "a.rkt") (path-b "z.rkt")
                                                                                    (count 1)))))
  (test-case "CLI reads only the pinned commit and is reproducible"
    (define git (find-executable-path "git"))
    (define racket (find-executable-path "racket"))
    (define repository (make-temporary-file "architecture-baseline-~a" 'directory))
    (check-not-false git)
    (check-not-false racket)
    (dynamic-wind
     void
     (lambda ()
       (parameterize ([current-directory repository])
         (checked-system* git "init" "--quiet")
         (make-directory "tests")
         (call-with-output-file "a.rkt"
                                (lambda (output) (display "#lang racket/base\n(provide a)\n" output)))
         (call-with-output-file
          "tests/test-a.rkt"
          (lambda (output)
            (display "#lang racket\n(require rackunit \"../a.rkt\")\n(check-true #t)\n" output)))
         (checked-system* git "add" "--" "a.rkt" "tests/test-a.rkt")
         (checked-system* git
                          "-c"
                          "user.name=Baseline Test"
                          "-c"
                          "user.email=baseline@example.invalid"
                          "-c"
                          "commit.gpgsign=false"
                          "commit"
                          "--quiet"
                          "-m"
                          "initial")
         (let ([revision (string-trim (captured-system* git "rev-parse" "HEAD"))])
           ;; Neither this untracked module nor this tracked-file edit may leak in.
           (call-with-output-file "untracked.rkt"
                                  (lambda (output) (display "#lang racket/base\n" output)))
           (call-with-output-file
            "a.rkt"
            (lambda (output) (display "#lang racket/base\n(provide changed)\nchanged\n" output))
            #:exists 'truncate/replace)
           (checked-system* racket
                            (path->string architecture-cli)
                            "--revision"
                            revision
                            "--raw"
                            "one.rktd"
                            "--markdown"
                            "one.md"
                            "--last"
                            "10")
           (checked-system* racket
                            (path->string architecture-cli)
                            "--revision"
                            revision
                            "--raw"
                            "two.rktd"
                            "--markdown"
                            "two.md"
                            "--last"
                            "10")
           (check-equal? (file->bytes "one.rktd") (file->bytes "two.rktd"))
           (check-equal? (file->bytes "one.md") (file->bytes "two.md"))
           (let* ([raw (file->string "one.rktd")]
                  [snapshot (call-with-input-string raw
                                                    (lambda (input)
                                                      (define datum (read input))
                                                      (check-true (eof-object? (read input)))
                                                      datum))])
             (check-equal? (field snapshot
                                  'revision)
                           revision)
             (check-equal? (module-paths snapshot) '("a.rkt" "tests/test-a.rkt"))
             (check-equal? (field (first (field snapshot
                                                'modules))
                                  'loc)
                           2)
             (check-false (regexp-match? #px"(?i:timestamp)" raw))
             (check-false (regexp-match? #px"(?i:timestamp)" (file->string "one.md")))))))
     (lambda () (delete-directory/files repository))))
  (test-case "policy exceptions, budgets, and default findings are represented"
    (define policy
      '((known-exceptions (runtime . ((layer-adapters.rkt (rationale . "adapter")
                                                          (owner . "runtime")
                                                          (permanent-waiver . #t)
                                                          (waiver-justification .
                                                                                "composition root"))))
                          (extensions . ((context.rkt (rationale . "stale session type rationale")
                                                      (owner . "extensions")
                                                      (revisit-by . "2026-10-01")))))
        (composition-roots . (("runtime/a.rkt" (fan-out . 2) (rationale . "root"))))
        (hotspot-budget (risk-notes . (("runtime/settings-query.rkt"
                                        (risk . "Settings query module with 226 provides")
                                        (owner . "runtime")))))))
    (define exceptions (dependency-policy-exceptions policy))
    (check-equal? (length exceptions) 2)
    (check-equal? (field (first exceptions)
                         'layer)
                  "extensions")
    (check-equal? (field (second exceptions)
                         'layer)
                  "runtime")
    (check-equal? (dependency-policy-composition-roots policy)
                  '(((path "runtime/a.rkt") (recorded-fan-out 2))))
    (check-equal? (dependency-policy-provide-risks policy)
                  '(((path "runtime/settings-query.rkt") (recorded-provides 226))))
    (check-equal? (sort (remove-duplicates (map (lambda (finding)
                                                  (field finding
                                                         'status))
                                                default-part-i-findings))
                        string<?)
                  (sort finding-statuses string<?)))
  (test-case "release subjects and release surfaces do not contaminate co-change"
    (define commits
      (list (git-commit "1"
                        "release: v1"
                        (list (git-change "M" '("runtime/a.rkt"))
                              (git-change "M" '("runtime/b.rkt"))))
            (git-commit "2"
                        "feature"
                        (list (git-change "M" '("CHANGELOG.md"))
                              (git-change "M" '("util/version.rkt"))
                              (git-change "M" '("runtime/a.rkt"))
                              (git-change "M" '("runtime/b.rkt"))))))
    (check-equal? (count-co-changes commits)
                  '(((path-a "runtime/a.rkt") (path-b "runtime/b.rkt") (count 1)))))
  (test-case "require/typed dependencies contribute to fan-in and fan-out"
    (define typed-source "#lang racket/base\n(require/typed \"b.rkt\" [b Integer])\n(provide a)\n")
    (define snapshot
      (build-architecture-snapshot "abc"
                                   (list (cons "a.rkt" typed-source)
                                         (cons "b.rkt" "#lang racket/base\n(provide b)\n"))
                                   '()))
    (define modules
      (field snapshot
             'modules))
    (check-equal? (field (first modules)
                         'dependency-fan-out)
                  1)
    (check-equal? (field (second modules)
                         'dependency-fan-in)
                  1)))
