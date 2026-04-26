#lang racket

;; tests/test-gsd-bash-detect.rkt — Bash file-reading detection tests
;;
;; Wave 3b: Verify bash file-read detection works correctly.

(require rackunit
         "../extensions/gsd/bash-detect.rkt")

;; ============================================================
;; Detected commands
;; ============================================================

(test-case "sed -n detected"
  (define-values (detected? detail) (detect-file-read-bash "sed -n '10,20p' foo.rkt"))
  (check-true detected?)
  (check-true (string? detail)))

(test-case "cat detected"
  (define-values (detected? _) (detect-file-read-bash "cat bar.rkt"))
  (check-true detected?))

(test-case "head detected"
  (define-values (detected? _) (detect-file-read-bash "head -20 foo.rkt"))
  (check-true detected?))

(test-case "tail detected"
  (define-values (detected? _) (detect-file-read-bash "tail -5 foo.rkt"))
  (check-true detected?))

(test-case "awk detected"
  (define-values (detected? _) (detect-file-read-bash "awk '{print $1}' data.txt"))
  (check-true detected?))

(test-case "python open detected"
  (define-values (detected? _) (detect-file-read-bash "python3 -c 'open(\"x.rkt\").read()'"))
  (check-true detected?))

(test-case "perl -ne detected"
  (define-values (detected? _) (detect-file-read-bash "perl -ne 'print if /foo/' file.txt"))
  (check-true detected?))

(test-case "grep with $ detected"
  (define-values (detected? _) (detect-file-read-bash "grep 'pattern$' file.txt"))
  (check-true detected?))

;; ============================================================
;; Safe commands (not detected)
;; ============================================================

(test-case "git status NOT detected"
  (define-values (detected? _) (detect-file-read-bash "git status"))
  (check-false detected?))

(test-case "ls NOT detected"
  (define-values (detected? _) (detect-file-read-bash "ls -la"))
  (check-false detected?))

(test-case "find NOT detected"
  (define-values (detected? _) (detect-file-read-bash "find . -name '*.rkt'"))
  (check-false detected?))

(test-case "mkdir NOT detected"
  (define-values (detected? _) (detect-file-read-bash "mkdir -p src/dir"))
  (check-false detected?))

(test-case "echo NOT detected"
  (define-values (detected? _) (detect-file-read-bash "echo 'hello'"))
  (check-false detected?))

(test-case "raco test NOT detected"
  (define-values (detected? _) (detect-file-read-bash "raco test tests/"))
  (check-false detected?))

(test-case "make NOT detected"
  (define-values (detected? _) (detect-file-read-bash "make build"))
  (check-false detected?))

;; ============================================================
;; Edge cases
;; ============================================================

(test-case "empty string returns #f"
  (define-values (detected? _) (detect-file-read-bash ""))
  (check-false detected?))

(test-case "non-string returns #f"
  (define-values (detected? _) (detect-file-read-bash 42))
  (check-false detected?))

(test-case "whitespace-only returns #f"
  (define-values (detected? _) (detect-file-read-bash "   "))
  (check-false detected?))
