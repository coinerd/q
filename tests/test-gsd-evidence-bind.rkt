#lang racket

;; @speed fast
;; @suite default
;; @boundary unit

;; End-to-end tests for scripts/gsd-evidence-bind.rkt's CLI surface — the
;; contract CI and the delivery coordinator depend on: decided verdicts on
;; stdout with exit 0, fail-closed nonzero exits for tool failures, and the
;; subcommand set (digest / verify / bind / record-commit).

(require racket/file
         racket/list
         racket/port
         racket/string
         rackunit)

(require (file "../tests/helpers/w2-mini-git-repo.rkt"))

(require racket/file
         racket/list
         racket/port
         racket/runtime-path
         racket/string
         rackunit)

(require (file "../tests/helpers/w2-mini-git-repo.rkt"))

(define-runtime-path tool-path "../scripts/gsd-evidence-bind.rkt")
(define repo-root (simplify-path (build-path tool-path 'up 'up)))
(define racket-exe (path->string (or (find-executable-path "racket") "racket")))

(define (run-tool args #:cwd [cwd (current-directory)])
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-environment-variables (hermetic-git-env)]
                   [current-directory cwd]
                   [current-output-port out]
                   [current-error-port err])
      (apply system*/exit-code racket-exe tool-path args)))
  (values code (string-trim (get-output-string out)) (get-output-string err)))

(define-values (repo base head)
  (parameterize ([current-environment-variables (hermetic-git-env)])
    (define r (make-mini-repo! "clie2e"))
    (define b (mini-commit-file! r "src/one.rkt" "one\n" "base"))
    (define h (mini-commit-file! r "src/two.rkt" "two\n" "wave"))
    (values r b h)))

(define expected-digest
  (let ()
    (local-require (file "../scripts/gsd-evidence-bind.rkt"))
    (excluded-evidence-digest repo base head)))

(test-case "CLI digest: prints the computed digest, exit 0"
  (define-values (code out err) (run-tool (list "digest" "--repo" repo "--base" base "--head" head)))
  (check-equal? code 0)
  (check-equal? out expected-digest))

(test-case "CLI digest: bad usage exits nonzero"
  (define-values (code out _err) (run-tool (list "digest" "--repo" repo)))
  (check-not-equal? code 0)
  (check-true (string-contains? out "Usage")))

(test-case "CLI verify: exit 0 for both decided verdicts"
  (define root (make-temporary-file "q-w2-cli-verify-~a" 'directory))
  (define good (build-path root "good.rktd"))
  (display-to-file (format "#hasheq((content-digest . ~s))\n" expected-digest) good #:exists 'replace)
  (define-values (code0 out0 _e0)
    (run-tool (list "verify" "--repo" repo "--base" base "--head" head "--evidence" good)))
  (check-equal? code0 0)
  (check-true (string-prefix? out0 "digest-ok"))
  (define bad (build-path root "bad.rktd"))
  (display-to-file (format "#hasheq((content-digest . ~s))\n" (make-string 64 #\f))
                   bad
                   #:exists 'replace)
  (define-values (code1 out1 _e1)
    (run-tool (list "verify" "--repo" repo "--base" base "--head" head "--evidence" bad)))
  ;; decided verdicts are exit 0 with the typed refusal on stdout
  (check-equal? code1 0)
  (check-true (string-prefix? out1 "digest-mismatch")))

(test-case "CLI bind: writes the computed digest into the record"
  (define root (make-temporary-file "q-w2-cli-bind-~a" 'directory))
  (define target (build-path root "evidence.rktd"))
  (display-to-file "#hasheq((schema-version . 2) (status . \"ready-for-merge\"))\n"
                   target
                   #:exists 'replace)
  (define-values (code out _err)
    (run-tool (list "bind" "--repo" repo "--base" base "--head" head "--evidence" target)))
  (check-equal? code 0)
  (check-true (string-prefix? out "bound ") (format "bind stdout: ~a" out))
  (define datum (call-with-input-file target read))
  (check-equal? (hash-ref datum 'content-digest) expected-digest))

(test-case "CLI record-commit: pure and impure verdicts, exit 0 for decided verdicts"
  (parameterize ([current-environment-variables (hermetic-git-env)])
    (define mixed (make-mini-repo! "climixed"))
    (define b (mini-commit-file! mixed "src/one.rkt" "one\n" "base"))
    (make-directory* (build-path mixed "docs/reports/gsd-wave-evidence"))
    (display-to-file "#hasheq((schema-version . 2))\n"
                     (build-path mixed "docs/reports/gsd-wave-evidence/v9.9.9-w2.rktd")
                     #:exists 'replace)
    (display-to-file "drift\n" (build-path mixed "README.md") #:exists 'replace)
    (mini-git! mixed "add" "-A" ".")
    (mini-git! mixed "commit" "-q" "-m" "mixed")
    (define tip (mini-git! mixed "rev-parse" "HEAD"))
    (define-values (code out _err)
      (run-tool (list "record-commit" "--repo" mixed "--base" b "--head" tip)))
    (check-equal? code 0)
    (check-true (string-prefix? out "impure-record-commit"))
    (check-true (string-contains? out "README.md")))
  (parameterize ([current-environment-variables (hermetic-git-env)])
    (define pure (make-mini-repo! "clipure"))
    (define b (mini-commit-file! pure "src/one.rkt" "one\n" "base"))
    (mini-commit-file! pure
                       "docs/reports/gsd-wave-evidence/v9.9.9-w2.rktd"
                       "#hasheq((schema-version . 2))\n"
                       "evidence only")
    (define tip (mini-git! pure "rev-parse" "HEAD"))
    (define-values (code out _err)
      (run-tool (list "record-commit" "--repo" pure "--base" b "--head" tip)))
    (check-equal? code 0)
    (check-equal? out "pure")))

(test-case "CLI digest against the real checkout: the W1 publication digest is EMPTY_SHA"
  (define-values (code out _err)
    (run-tool (list "digest"
                    "--repo"
                    repo-root
                    "--base"
                    "43f89413ac29610196b9931340653b3cbde2149c"
                    "--head"
                    "69647f1ce5d36798e790b1d62a3b79c62b8b011f")))
  (check-equal? code 0)
  (check-equal? out "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
