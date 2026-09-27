#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers scripts/gsd-delivery.py
;; tests/test-delivery-failure-classification.rkt — this campaign's W3 (register F5/F8):
;; every delivery-subprocess failure is a typed, diagnosable refusal carrying
;; the command, exit code and class — never a bare "exit 128".
;;
;; The classifier lives in the Python controller; these tests drive it through
;; the import path the controller itself uses, plus one CLI-boundary probe
;; proving failures surface as typed delivery-pending JSON (exit 2), never an
;; uncaught traceback.
(require rackunit
         racket/file
         racket/runtime-path
         racket/string
         racket/port
         racket/path
         racket/system
         (only-in (file "../util/version.rkt") q-version))

(define-runtime-path controller "../scripts/gsd-delivery.py")

(define (classify args cmd exit stderr)
  (define quoted-args
    (string-join (for/list ([a (in-list args)])
                   (format "~s" a))
                 " "))
  (define program
    (string-append "import importlib.util as u\n"
                   "spec = u.spec_from_file_location('gsd_delivery', r'"
                   (path->string controller)
                   "')\n"
                   "m = u.module_from_spec(spec)\n"
                   "spec.loader.exec_module(m)\n"
                   "print(m.classify_failure(["
                   quoted-args
                   "], "
                   (format "~s" cmd)
                   ", "
                   (number->string exit)
                   ", "
                   (format "~s" stderr)
                   "))\n"))
  (define tmp (make-temporary-file "classify-~a.py"))
  (dynamic-wind (lambda () (display-to-file program tmp #:exists 'truncate))
                (lambda ()
                  (string-trim (with-output-to-string
                                (lambda ()
                                  (void (system (format "python3 ~a" (path->string tmp))))))))
                (lambda () (delete-file tmp))))

(module+ test
  (test-case "F8: a missing remote ref classifies as remote-ref-missing naming ref and remedy"
    (define reason
      (classify (list "git" "-C" "/repo" "fetch" "origin" "refs/heads/campaign/v1.00.30-w4")
                "git"
                128
                "fatal: couldn't find remote ref refs/heads/campaign/v1.00.30-w4"))
    (check-true (string-prefix? reason "remote-ref-missing:")
                (format "typed class expected, got: ~a" reason))
    (check-true (string-contains? reason "refs/heads/campaign/v1.00.30-w4")
                "the exact unpublished ref is named")
    (check-true (string-contains? reason "push the verified head first") "the remedy is named")
    (check-true (string-contains? reason "(git, exit 128)") "command and exit code are carried"))

  (test-case "F8: the ref is recovered from the arguments when stderr omits it"
    (define reason
      (classify (list "git"
                      "-C"
                      "/repo"
                      "fetch"
                      "origin"
                      (format "refs/heads/campaign/v~a-w3:refs/remotes/origin/campaign/v~a-w3"
                              q-version
                              q-version))
                "git"
                128
                "fatal: couldn't find remote ref"))
    (check-true (string-prefix? reason "remote-ref-missing:")
                (format "typed class expected, got: ~a" reason))
    (check-true (string-contains? reason (format "refs/heads/campaign/v~a-w3" q-version))
                "the refspec's source ref is named"))

  (test-case "F8: a non-fast-forward push classifies as non-fast-forward"
    (define reason
      (classify (list "git" "-C" "/repo" "push" "origin" "refs/heads/campaign/w3")
                "git"
                1
                " ! [rejected]        campaign/w3 -> campaign/w3 (non-fast-forward)"))
    (check-true (string-prefix? reason "non-fast-forward:")
                (format "typed class expected, got: ~a" reason))
    (check-true (string-contains? reason "(git, exit 1)") "command and exit code are carried"))

  (test-case "F8: an unknown revision classifies as unknown-ref"
    (define reason
      (classify (list "git" "-C" "/repo" "rev-parse" "refs/remotes/origin/ghost")
                "git"
                128
                "fatal: ambiguous argument 'refs/remotes/origin/ghost': unknown revision"))
    (check-true (string-prefix? reason "unknown-ref:")
                (format "typed class expected, got: ~a" reason))
    (check-true (string-contains? reason "(git, exit 128)") "command and exit code are carried"))

  (test-case "F8: authentication failures keep their dedicated credential-free message"
    (define reason
      (classify (list "gh" "api" "repos/example/q")
                "gh"
                1
                "gh: To get started with GitHub, gh auth login"))
    (check-true (string-prefix? reason "GitHub authentication failed")
                (format "auth message expected, got: ~a" reason))
    (check-false (string-contains? reason "exit")
                 "the auth refusal never leaks the raw subprocess shape"))

  (test-case "F8: unclassified failures keep the honest generic reason with exit code"
    (define reason
      (classify (list "git" "-C" "/repo" "status")
                "git"
                128
                "fatal: not a git repository (or any of the parent directories)"))
    (check-true (string-prefix? reason "delivery command failed")
                (format "generic fallback expected, got: ~a" reason))
    (check-true (string-contains? reason "(git, exit 128)") "command and exit code are carried"))

  (test-case "the CLI boundary reports failures as typed delivery-pending, never a traceback"
    (define out
      (with-output-to-string (lambda ()
                               (void (system (format "python3 ~s status --repo ~s --plan ~s --wave 0"
                                                     (path->string controller)
                                                     (path->string (path-only controller))
                                                     (make-string 64 #\a)))))))
    (check-true (string-contains? out "\"status\": \"delivery-pending\"")
                (format "typed JSON expected on stdout, got: ~a" out))
    (check-true (string-contains? out "\"reason\"") "a reason field is always present")
    (check-false (string-contains? out "Traceback") "failures never surface as raw tracebacks")))
