#lang racket/base

;; q/scripts/run-tests/profiles.rkt — Environment profile skip policy
;;
;; Profiles make environment-dependent skips explicit and reportable. Skips are
;; represented as SKIPPED_BY_PROFILE results and must never be counted as PASS.

(require racket/list
         racket/string
         (only-in "parse.rkt" make-test-file-result))

(provide known-profiles
         profile-unavailable-requirements
         profile-skips-test?
         skipped-requirements
         make-skipped-result
         skipped-result-exit-code)

(define known-profiles '(local vps ci headless full))

(define skipped-result-exit-code 5)

(define (normalize-requirement req)
  (cond
    [(symbol? req) (symbol->string req)]
    [(string? req) (string-downcase (string-trim req))]
    [else (format "~a" req)]))

(define (profile-unavailable-requirements profile)
  (case profile
    ;; Developer workstation: terminal/subprocess/filesystem/git are assumed;
    ;; live provider credentials are intentionally not assumed.
    [(local) '("provider-key")]
    ;; VPS is headless: browser/terminal are unavailable; provider credentials
    ;; may exist and are therefore not skipped by policy.
    [(vps) '("browser" "terminal")]
    ;; CI must be deterministic and non-interactive by default.
    [(ci) '("provider-key" "browser" "terminal" "network")]
    ;; Headless means no interactive/UI/provider dependencies.
    [(headless) '("provider-key" "browser" "terminal")]
    ;; Full is opt-in to run everything possible.
    [(full) '()]
    [else '()]))

(define (skipped-requirements profile requirements)
  (define unavailable (profile-unavailable-requirements profile))
  (filter (lambda (req)
            (define normalized (normalize-requirement req))
            (and (not (string=? normalized "none")) (member normalized unavailable)))
          requirements))

(define (profile-skips-test? profile requirements)
  (pair? (skipped-requirements profile requirements)))

(define (make-skipped-result path profile requirements)
  (define skipped (skipped-requirements profile requirements))
  (define reason
    (format "SKIPPED_BY_PROFILE profile=~a requires=~a" profile (string-join skipped ",")))
  (make-test-file-result path
                         skipped-result-exit-code
                         (string->bytes/utf-8 (string-append reason "\n"))
                         #""
                         0
                         0
                         0
                         0))
