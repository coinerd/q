#lang racket

;; @speed fast
;; @suite workflows
;; @boundary unit
;; @isolation temp-dir
;; @mutates temp-dir
;; @covers scripts/ci/observed-environment.rkt
;; @covers .github/workflows/ci.yml

;; Red-first contract for CL5 production CI observation. The recorder must
;; derive every host fact from the running process/environment, and the
;; production workflow must attach those facts without altering the claim
;; fields that proof-bundle reuse validates.

(require json
         racket/file
         racket/list
         racket/runtime-path
         racket/string
         rackunit
         "../scripts/ci/observed-environment.rkt")

(define-runtime-path ci-workflow "../.github/workflows/ci.yml")
(define ci-text (file->string ci-workflow))

(define (put! path text)
  (make-directory* (path-only path))
  (call-with-output-file path (lambda (out) (display text out)) #:exists 'truncate))

(define (env-from pairs)
  (define env (make-environment-variables))
  (for ([pair (in-list pairs)])
    (environment-variables-set! env
                                (string->bytes/utf-8 (car pair))
                                (string->bytes/utf-8 (cdr pair))))
  env)

(define complete-env
  (env-from '(("CL5_OBSERVED_RUNS_ON_LABEL" . "ubuntu-latest")
              ("ImageOS" . "ubuntu24")
              ("ImageVersion" . "20260901.1.0")
              ("RUNNER_OS" . "Linux")
              ("RUNNER_ARCH" . "X64")
              ("RUNNER_NAME" . "runnervm-observed")
              ("GITHUB_SHA" . "0123456789abcdef0123456789abcdef01234567")
              ("CL5_OBSERVED_WORKFLOW_SHA" . "fedcba9876543210fedcba9876543210fedcba98")
              ("CL5_OBSERVED_WORKFLOW_BLOB_SHA" . "aaaabbbbccccddddeeeeffff0000111122223333"))))

(define (env-without name)
  (env-from (filter (lambda (pair) (not (string=? (car pair) name)))
                    '(("CL5_OBSERVED_RUNS_ON_LABEL" . "ubuntu-latest")
                      ("ImageOS" . "ubuntu24")
                      ("ImageVersion" . "20260901.1.0")
                      ("RUNNER_OS" . "Linux")
                      ("RUNNER_ARCH" . "X64")
                      ("RUNNER_NAME" . "runnervm-observed")
                      ("GITHUB_SHA" . "0123456789abcdef0123456789abcdef01234567")
                      ("CL5_OBSERVED_WORKFLOW_SHA" . "fedcba9876543210fedcba9876543210fedcba98")
                      ("CL5_OBSERVED_WORKFLOW_BLOB_SHA"
                       . "aaaabbbbccccddddeeeeffff0000111122223333")))))

(define (fake-root!)
  (define root (make-temporary-file "q-observed-env-~a" 'directory))
  (put! (build-path root "etc/os-release")
        "NAME=\"Ubuntu\"\nVERSION_ID=\"26.04.1\"\nPRETTY_NAME=\"Ubuntu 26.04.1 LTS\"\nID=ubuntu\n")
  (put! (build-path root "proc/sys/kernel/osrelease") "7.0.0-1020-azure\n")
  (put! (build-path root "proc/version")
        "Linux version 7.0.0-1020-azure (buildd@lcy02) #21-Ubuntu SMP PREEMPT\n")
  (put! (build-path root "proc/cpuinfo")
        "processor : 0\nmodel name : AMD EPYC 9V74 80-Core Processor\n")
  (put! (build-path root "proc/meminfo") "MemTotal:       16123456 kB\nMemFree:        1234567 kB\n")
  (put! (build-path root "racket") "fake racket executable for digesting\n")
  root)

(define (observe-in root [env complete-env])
  (parameterize ([current-environment-variables env])
    (observed-environment #:root root
                          #:tree-sha "aaaabbbbccccddddeeeeffff0000111122223333"
                          #:racket-executable (build-path root "racket"))))

(test-case "os-release parsing strips quotes and ignores other lines"
  (define parsed
    (parse-os-release "ID=ubuntu\nPRETTY_NAME=\"Ubuntu 26.04.1 LTS\"\n# comment\nOTHER=a=b\n"))
  (check-equal? (hash-ref parsed "ID") "ubuntu")
  (check-equal? (hash-ref parsed "PRETTY_NAME") "Ubuntu 26.04.1 LTS")
  (check-equal? (hash-ref parsed "OTHER") "a=b"))

(test-case "observation records every required production host fact"
  (define root (fake-root!))
  (dynamic-wind
   void
   (lambda ()
     (define obs (observe-in root))
     (check-equal? (hash-ref obs 'schema) observed-environment-schema)
     (check-equal? (hash-ref (hash-ref obs 'os_release) 'version_id) "26.04.1")
     (check-equal? (hash-ref (hash-ref obs 'os_release) 'pretty_name) "Ubuntu 26.04.1 LTS")
     (check-equal? (hash-ref (hash-ref obs 'kernel) 'release) "7.0.0-1020-azure")
     (check-true (string-prefix? (hash-ref (hash-ref obs 'kernel) 'version) "Linux version"))
     (define runner (hash-ref obs 'runner))
     (check-equal? (hash-ref runner 'declared_runs_on_label) "ubuntu-latest")
     (check-equal? (hash-ref runner 'image_os) "ubuntu24")
     (check-equal? (hash-ref runner 'image_version) "20260901.1.0")
     (check-equal? (hash-ref (hash-ref obs 'cpu) 'model) "AMD EPYC 9V74 80-Core Processor")
     (check-true (positive? (hash-ref (hash-ref obs 'cpu) 'available_cores)))
     (check-equal? (hash-ref (hash-ref obs 'memory) 'total_kb) 16123456)
     (check-true (regexp-match? #px"^sha256:[0-9a-f]{64}$"
                                (hash-ref (hash-ref obs 'racket) 'executable_sha256)))
     (define revision (hash-ref obs 'revision))
     (check-equal? (hash-ref revision 'commit_sha) "0123456789abcdef0123456789abcdef01234567")
     (check-equal? (hash-ref revision 'tree_sha) "aaaabbbbccccddddeeeeffff0000111122223333")
     (check-equal? (hash-ref revision 'workflow_sha) "fedcba9876543210fedcba9876543210fedcba98")
     (check-equal? (hash-ref revision 'workflow_blob_sha) "aaaabbbbccccddddeeeeffff0000111122223333"))
   (lambda () (delete-directory/files root #:must-exist? #f))))

(test-case "a missing runner image field fails closed"
  (define root (fake-root!))
  (dynamic-wind void
                (lambda ()
                  (check-exn exn:fail?
                             (lambda () (observe-in root (env-without "ImageVersion")))
                             "missing ImageVersion must fail closed"))
                (lambda () (delete-directory/files root #:must-exist? #f))))

(test-case "a malformed workflow revision fails closed"
  (define root (fake-root!))
  (dynamic-wind
   void
   (lambda ()
     (define bad-env
       (env-from '(("CL5_OBSERVED_RUNS_ON_LABEL" . "ubuntu-latest")
                   ("ImageOS" . "ubuntu24")
                   ("ImageVersion" . "20260901.1.0")
                   ("RUNNER_OS" . "Linux")
                   ("RUNNER_ARCH" . "X64")
                   ("RUNNER_NAME" . "runnervm-observed")
                   ("GITHUB_SHA" . "0123456789abcdef0123456789abcdef01234567")
                   ("CL5_OBSERVED_WORKFLOW_SHA" . "not-a-sha")
                   ("CL5_OBSERVED_WORKFLOW_BLOB_SHA" . "aaaabbbbccccddddeeeeffff0000111122223333"))))
     (check-exn exn:fail?
                (lambda () (observe-in root bad-env))
                "a malformed workflow SHA must fail closed"))
   (lambda () (delete-directory/files root #:must-exist? #f))))

(define (job-block text job)
  (define lines (string-split text "\n"))
  (define prefix (format "  ~a:" job))
  (define start
    (for/first ([line (in-list lines)]
                [index (in-naturals)]
                #:when (string=? line prefix))
      index))
  (check-not-false start (format "no ~a job in ci.yml" job))
  (define end
    (for/first ([line (in-list (drop lines (add1 start)))]
                [index (in-naturals (add1 start))]
                #:when (regexp-match? #px"^  [A-Za-z0-9_-]+:$" line))
      index))
  (string-join (take (drop lines start) (- (or end (length lines)) start)) "\n"))

(test-case "production workflow records observations without changing reuse inputs"
  (define block (job-block ci-text "test-aggregate"))
  (define runs-on (regexp-match #px"(?m:^    runs-on: +([^ ]+) *$)" block))
  (define label (regexp-match #px"(?m:CL5_OBSERVED_RUNS_ON_LABEL: +([^ ]+) *$)" block))
  (check-not-false runs-on "test-aggregate has no runs-on label")
  (check-not-false label "the observed runs-on label is not declared")
  (check-equal? (cadr label) (cadr runs-on))
  (check-true (string-contains? block "racket scripts/ci/observed-environment.rkt"))
  (check-true (string-contains? block "--out artifacts-run/observed-environment.json"))
  (check-true (string-contains? block "CL5_OBSERVED_WORKFLOW_SHA: ${{ github.workflow_sha }}"))
  (check-true (string-contains? block "export CL5_OBSERVED_WORKFLOW_BLOB_SHA=\"$WF_REVISION\""))
  (check-true (string-contains? ci-text "\"os_version\": \"24.04\""))
  (check-true (string-contains? ci-text "\"runner_image\": \"ubuntu-24.04\""))
  (check-true (string-contains? ci-text "observed_environment"))
  (check-true (string-contains? ci-text "proof-bundle/observed-environment.json")))
