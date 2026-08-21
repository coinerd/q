#lang racket/base

(require racket/format
         racket/list
         racket/match
         racket/port
         racket/string
         racket/system)

(define lock-path "ci/racket-package-lock.rktd")

(define (fail fmt . args)
  (apply eprintf (string-append "racket package lock verification failed: " fmt "\n") args)
  (exit 1))

(define lock
  (with-handlers ([exn:fail? (lambda (e) (fail "cannot read ~a: ~a" lock-path (exn-message e)))])
    (call-with-input-file lock-path read)))

(unless (hash? lock)
  (fail "~a must contain a hash" lock-path))

(define expected-version (hash-ref lock 'racket-version #f))
(define expected-packages (hash-ref lock 'packages #f))

(unless (and (string? expected-version) (hash? expected-packages))
  (fail "~a must define racket-version and a packages hash" lock-path))

(unless (string-prefix? (version) expected-version)
  (fail "Racket ~a does not match locked version ~a" (version) expected-version))

(define raco-path
  (or (find-executable-path "raco")
      (fail "cannot locate raco on PATH")))

(define-values (proc stdout stdin stderr)
  (subprocess #f #f #f raco-path "pkg" "show"
              "--scope" "user" "--all" "--long" "--full-checksum"))
(close-output-port stdin)
(define output (port->string stdout))
(define errors (port->string stderr))
(subprocess-wait proc)
(unless (zero? (subprocess-status proc))
  (fail "raco pkg show failed: ~a" (string-trim errors)))

;; `raco pkg show --long --full-checksum` emits one package per line. Auto
;; dependencies have a trailing `*` in the package column; q itself has `#f`
;; because it is deliberately a checkout link and is verified separately.
(define package-rx #px"^([^[:space:]\\*]+)\\*?[[:space:]]+([0-9a-f]{40}|#f)[[:space:]]+")
(define actual
  (for/fold ([table (hash)]) ([line (in-list (string-split output "\n"))])
    (match (regexp-match package-rx line)
      [(list _ name checksum) (hash-set table name checksum)]
      [_ table])))

(for ([(name checksum) (in-hash expected-packages)])
  (define actual-checksum (hash-ref actual name #f))
  (unless actual-checksum
    (fail "locked package ~a is absent from the user package store" name))
  (unless (string=? actual-checksum checksum)
    (fail "locked package ~a has checksum ~a, expected ~a" name actual-checksum checksum)))

(unless (hash-has-key? actual "q")
  (fail "q is absent from the user package store"))

(printf "Racket package lock verified: ~a external packages for Racket ~a\n"
        (hash-count expected-packages)
        expected-version)
