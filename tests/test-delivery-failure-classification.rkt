#lang racket/base
;; @speed fast
;; @suite extensions
;; @covers scripts/gsd-delivery-git.rkt
;; tests/test-delivery-failure-classification.rkt — this campaign's W3 (register F5/F8):
;; every delivery-subprocess failure is a typed, diagnosable refusal carrying
;; the command, exit code and class — never a bare "exit 128".
;;
;; Phase B: the classifier lives in scripts/gsd-delivery-git.rkt (classify-failure)
;; and the Python controller it was ported from is gone. Each case asserts THREE
;; things: the Racket classifier still reproduces the EXACT reason recorded from
;; the Python original at its final revision
;; (tests/fixtures/gsd-delivery-goldens.json), the recorded reason carries the
;; documented class/remedy, and the CLI boundary surfaces failures as typed
;; delivery-pending JSON (exit 2), never an uncaught traceback.
(require rackunit
         json
         racket/file
         racket/runtime-path
         racket/string
         racket/port
         racket/path
         racket/system
         (file "../scripts/gsd-delivery-git.rkt")
         (file "helpers/gsd-delivery-goldens.rkt")
         (only-in (file "../util/version.rkt") q-version))

(define-runtime-path status-controller "../scripts/gsd-delivery-status.rkt")
(define racket-executable
  (let ([found (find-executable-path "racket")])
    (if found (path->string found) "racket")))

(define classification-group "gsd-delivery-failure-classification")

;; The classifier's answer for one recorded case. The golden carries the Python
;; original's exact reason; the Racket classifier must reproduce it byte for
;; byte, and the caller's assertions run against that recorded text.
(define (classify label args cmd exit stderr)
  (define reason
    (hash-ref (golden-outcome classification-group
                              '()
                              label
                              (hasheq 'args args 'cmd cmd 'exit exit 'stderr stderr))
              'value))
  (check-equal? (classify-failure cmd exit stderr args) reason label)
  reason)

(module+ test
  (test-case "F8: a missing remote ref classifies as remote-ref-missing naming ref and remedy"
    (define reason
      (classify "remote-ref-missing-names-ref"
                (list "git" "-C" "/repo" "fetch" "origin" "refs/heads/campaign/v1.00.30-w4")
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
      (classify "remote-ref-missing-from-args"
                (list "git"
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
      (classify "non-fast-forward"
                (list "git" "-C" "/repo" "push" "origin" "refs/heads/campaign/w3")
                "git"
                1
                " ! [rejected]        campaign/w3 -> campaign/w3 (non-fast-forward)"))
    (check-true (string-prefix? reason "non-fast-forward:")
                (format "typed class expected, got: ~a" reason))
    (check-true (string-contains? reason "(git, exit 1)") "command and exit code are carried"))

  (test-case "F8: an unknown revision classifies as unknown-ref"
    (define reason
      (classify "unknown-ref"
                (list "git" "-C" "/repo" "rev-parse" "refs/remotes/origin/ghost")
                "git"
                128
                "fatal: ambiguous argument 'refs/remotes/origin/ghost': unknown revision"))
    (check-true (string-prefix? reason "unknown-ref:")
                (format "typed class expected, got: ~a" reason))
    (check-true (string-contains? reason "(git, exit 128)") "command and exit code are carried"))

  (test-case "F8: authentication failures keep their dedicated credential-free message"
    (define reason
      (classify "auth-failure"
                (list "gh" "api" "repos/example/q")
                "gh"
                1
                "gh: To get started with GitHub, gh auth login"))
    (check-true (string-prefix? reason "GitHub authentication failed")
                (format "auth message expected, got: ~a" reason))
    (check-false (string-contains? reason "exit")
                 "the auth refusal never leaks the raw subprocess shape"))

  (test-case "F8: unclassified failures keep the honest generic reason with exit code"
    (define reason
      (classify "unclassified-generic"
                (list "git" "-C" "/repo" "status")
                "git"
                128
                "fatal: not a git repository (or any of the parent directories)"))
    (check-true (string-prefix? reason "delivery command failed")
                (format "generic fallback expected, got: ~a" reason))
    (check-true (string-contains? reason "(git, exit 128)") "command and exit code are carried"))

  (test-case "the CLI boundary reports failures as typed delivery-pending, never a traceback"
    ;; Phase B: the native status action owns this boundary. The invariant is
    ;; unchanged — a failing action surfaces typed delivery-pending JSON with a
    ;; reason on stdout, never an uncaught traceback.
    (define out
      (with-output-to-string
       (lambda ()
         (void (system (format "~a ~s status --repo ~s --campaign-root ~s --plan ~s --wave 0"
                               racket-executable
                               (path->string status-controller)
                               (path->string (path-only status-controller))
                               (path->string (path-only status-controller))
                               (make-string 64 #\a)))))))
    (check-false (string-contains? out "Traceback") "failures never surface as raw tracebacks")
    (define payload (string->jsexpr (string-trim out)))
    (check-equal? (hash-ref payload 'status) "delivery-pending")
    (check-true (and (hash-ref payload 'reason #f) (string? (hash-ref payload 'reason)))
                "a reason field is always present")))
