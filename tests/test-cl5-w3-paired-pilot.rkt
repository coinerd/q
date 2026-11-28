#lang racket

(require json
         rackunit
         racket/file
         racket/list
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         "../scripts/cl5/contract-escalation.rkt"
         "../scripts/cl5/paired-pilot-selection.rkt")

(define-runtime-path repo-root "..")
(define workload-path (build-path repo-root "docs" "reports" "CL5-W3-PAIRED-PILOT-WORKLOAD.json"))
(define design-path (build-path repo-root "docs" "reports" "CL5-W3-PAIRED-PILOT-DESIGN.md"))
(define workflow-path (build-path repo-root ".github" "workflows" "cl5-w3-paired-pilot.yml"))
(define workload (call-with-input-file workload-path read-json))
(define design (file->string design-path))
(define workflow (file->string workflow-path))

(define (string-index haystack needle)
  (let loop ([i 0])
    (cond
      [(> i (- (string-length haystack) (string-length needle))) #f]
      [(string=? (substring haystack i (+ i (string-length needle))) needle) i]
      [else (loop (add1 i))])))

(test-case "paired workload freezes one real decision and the reduced-surface projection"
  (check-equal? (hash-ref workload 'schema) "cl5-w3-paired-pilot/1")
  (check-equal? (hash-ref workload 'pinned_test_tree_sha) "ecb92a5e846a48ee710d9e5b9bed9ca0b0631d5b")
  (define c (hash-ref workload 'case))
  (check-equal? (hash-ref c 'changed_files) '("extensions/gsd/wave-status.rkt"))
  (check-equal? (hash-ref c 'expected_verdict) "escalate")
  (check-equal? (hash-ref c 'expected_escalations) '("no-consumer-detector"))
  (define comprehensive (hash-ref workload 'comprehensive_reduced_files))
  (define selected (hash-ref workload 'selected_projected_files))
  (check-equal? (length comprehensive) 8)
  (check-equal? (length selected) 2)
  (check-true (andmap (lambda (f) (and (member f comprehensive) #t)) selected))
  (check-equal? (hash-ref (hash-ref workload 'schedule) 'order)
                '("selected-comprehensive" "comprehensive-selected" "selected-comprehensive"))
  (check-equal? (hash-ref (hash-ref workload 'command) 'jobs) 4))

(test-case "contract decision digest and projection are re-derivable"
  (define decision
    (evaluate-contract-changes repo-root (hash-ref (hash-ref workload 'case) 'changed_files)))
  (check-equal? (decision-digest decision) (hash-ref (hash-ref workload 'case) 'decision_sha256))
  (check-equal? (project-selection (hash-ref decision 'required)
                                   (hash-ref workload 'comprehensive_reduced_files))
                (hash-ref workload 'selected_projected_files)))

(test-case "pre-registration rejects cross-run timing equivalence and campaign claims"
  (for ([needle (in-list '("methodological pilot only" "production-equivalent"
                                                       "No threshold is applied to `S`"
                                                       "Not authorized: P2-F retry"
                                                       "40 % claim"))])
    (check-true (string-contains? design needle) needle))
  (check-false (string-contains? design "A4 comparability")))

(test-case "deployed workflow validates host before running tests and has no repeat"
  (define validate (string-index workflow "Validate measured host before measurement"))
  (define materialise (string-index workflow "Materialise the frozen reduced surface"))
  (define runner (string-index workflow "racket scripts/run-tests/runner.rkt"))
  (check-not-false validate)
  (check-not-false materialise)
  (check-not-false runner)
  (check-true (< validate materialise runner))
  (check-true (string-contains? workflow "on:\n  workflow_dispatch:"))
  (check-false (regexp-match? #px"(?m:^  (push|pull_request):)" workflow))
  (check-true (string-contains? workflow "--jobs 4"))
  (for ([needle (in-list '("selected warm0" "comprehensive warm0"
                                            "selected pair1"
                                            "comprehensive pair1"
                                            "comprehensive pair2"
                                            "selected pair2"
                                            "selected pair3"
                                            "comprehensive pair3"))])
    (check-true (string-contains? workflow needle) needle))
  (check-false (regexp-match? #px"(?i:retry)" workflow)))

(test-case "local preflight executes the deployed shell function with lightweight stubs"
  (define lines (string-split workflow "\n"))
  (define start-index
    (for/first ([line (in-list lines)]
                [i (in-naturals)]
                #:when (regexp-match? #px"^          run_arm\\(\\) \\{" line))
      i))
  (define end-index
    (for/first ([line (in-list lines)]
                [i (in-naturals)]
                #:when (and start-index (> i start-index) (string=? line "          }")))
      i))
  (check-not-false start-index "deployed run_arm definition")
  (check-not-false end-index "deployed run_arm closing brace")
  (define shell-function
    (string-join (for/list ([i (in-range start-index (add1 end-index))])
                   (list-ref lines i))
                 "\n"))
  (define calls
    (for/list ([line (in-list (drop lines (add1 end-index)))]
               #:when (regexp-match? #px"^          run_arm [a-z]+ [a-z0-9]+$" line))
      (string-trim line)))
  (define expected-calls
    '("run_arm selected warm0" "run_arm comprehensive warm0"
                               "run_arm selected pair1"
                               "run_arm comprehensive pair1"
                               "run_arm comprehensive pair2"
                               "run_arm selected pair2"
                               "run_arm selected pair3"
                               "run_arm comprehensive pair3"))
  (check-equal? calls expected-calls)
  (define expected-invocations
    '("selected-warm0" "comprehensive-warm0"
                       "selected-pair1"
                       "comprehensive-pair1"
                       "comprehensive-pair2"
                       "selected-pair2"
                       "selected-pair3"
                       "comprehensive-pair3"))
  (define stub-racket
    (string-join (list "#!/usr/bin/env bash"
                       "set -euo pipefail"
                       "case \"${1:-}\" in"
                       "  */paired-pilot-selection.rkt)"
                       "    while [ \"$#\" -gt 0 ]; do"
                       "      case \"$1\" in"
                       "        --out) printf '{}\\n' > \"$2\"; shift 2;;"
                       "        *) shift;;"
                       "      esac"
                       "    done"
                       "    ;;"
                       "  */runner.rkt)"
                       "    while [ \"$#\" -gt 0 ]; do"
                       "      case \"$1\" in"
                       "        --json-out) printf '{}\\n' > \"$2\"; shift 2;;"
                       "        *) shift;;"
                       "      esac"
                       "    done"
                       "    ;;"
                       "  *) printf 'unexpected racket invocation: %s\\n' \"$*\" >&2; exit 2;;"
                       "esac")
                 "\n"))
  (define stub-date (string-join '("#!/usr/bin/env bash" "printf '1000\\n'") "\n"))
  (define stub-cat
    (string-join '("#!/usr/bin/env bash" "printf '0.00 0.00 0.00 1/100 12345\\n'") "\n"))
  (define bash-path (find-executable-path "bash"))
  (check-not-false bash-path)
  (define (write-stub! bin name content)
    (define p (build-path bin name))
    (call-with-output-file p #:exists 'replace (lambda (o) (display content o)))
    (check-true (system (format "chmod +x ~a" (path->string p))) "stub chmod"))
  (define (prepare-root! root)
    (make-directory* (build-path root "cl5-w3-paired"))
    (define bin (build-path root "bin"))
    (make-directory* bin)
    (write-stub! bin "racket" stub-racket)
    (write-stub! bin "date" stub-date)
    (write-stub! bin "cat" stub-cat))
  (define (run-shell! root script)
    (define env (environment-variables-copy (current-environment-variables)))
    (environment-variables-set!
     env
     #"PATH"
     (string->bytes/utf-8
      (string-append (path->string (build-path root "bin")) ":" (or (getenv "PATH") ""))))
    (define-values (proc out in err)
      (parameterize ([current-environment-variables env])
        (subprocess #f #f #f bash-path "-c" script)))
    (close-output-port in)
    (define stdout (port->string out))
    (define stderr (port->string err))
    (subprocess-wait proc)
    (values (subprocess-status proc) stdout stderr))
  (define root (make-temporary-directory "cl5-paired-preflight-~a"))
  (dynamic-wind
   void
   (lambda ()
     (prepare-root! root)
     (define script
       (string-join
        (list
         "set -euo pipefail"
         (format "cd ~a" (path->string root))
         "for arm in selected comprehensive; do printf 'tests/fake-%s.rkt\\n' \"$arm\" > cl5-w3-paired/files-$arm.txt; done"
         shell-function
         (string-join calls "\n")
         "printf 'load_after=%s\\n' \"$(cat /proc/loadavg)\" >> cl5-w3-paired/schedule.txt")
        "\n"))
     (define-values (rc _stdout stderr) (run-shell! root script))
     (check-equal? rc 0 (format "preflight failed: ~a" stderr))
     (define schedule-path (build-path root "cl5-w3-paired" "schedule.txt"))
     (check-true (file-exists? schedule-path))
     (define schedule-lines (string-split (file->string schedule-path) "\n"))
     (define invocations (filter (lambda (l) (string-prefix? l "invocation=")) schedule-lines))
     (define results (filter (lambda (l) (string-prefix? l "result=")) schedule-lines))
     (check-equal? (length invocations) 8)
     (check-equal? (length results) 8)
     (check-equal? (for/list ([line (in-list invocations)])
                     (cadr (regexp-match #px"^invocation=([^ ]+)" line)))
                   expected-invocations)
     (check-true (andmap (lambda (l) (string-contains? l "rc=0")) results))
     (check-equal? (length (filter (lambda (l) (string-prefix? l "load_before=")) schedule-lines)) 8)
     (check-equal? (length (filter (lambda (l) (string-prefix? l "load_after=")) schedule-lines)) 1)
     (define artifacts (directory-list (build-path root "cl5-w3-paired")))
     (check-equal? (length (filter (lambda (p)
                                     (regexp-match? #px"^selection-.*\\.json$" (path->string p)))
                                   artifacts))
                   4)
     (check-equal? (length (filter (lambda (p) (regexp-match? #px"^run-.*\\.json$" (path->string p)))
                                   artifacts))
                   8))
   (lambda () (delete-directory/files root)))
  ;; Negative control: the historical third-parameter bug must fail under `set -u`.
  (define broken-function
    (string-replace shell-function
                    "local arm=\"$1\" label=\"$2\""
                    "local arm=\"$1\" label=\"$2\" out=\"$3\""))
  (check-not-false (regexp-match? #px"out=\"\\$3\"" broken-function))
  (define bad-root (make-temporary-directory "cl5-paired-preflight-bad-~a"))
  (dynamic-wind
   void
   (lambda ()
     (prepare-root! bad-root)
     (define script
       (string-join (list "set -euo pipefail"
                          (format "cd ~a" (path->string bad-root))
                          "printf 'tests/fake-selected.rkt\\n' > cl5-w3-paired/files-selected.txt"
                          broken-function
                          "run_arm selected warm0")
                    "\n"))
     (define-values (rc _stdout stderr) (run-shell! bad-root script))
     (check-false (= rc 0) "broken shell preflight must fail")
     (check-true (string-contains? stderr "unbound variable") stderr))
   (lambda () (delete-directory/files bad-root))))
