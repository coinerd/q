#lang racket

;; @speed fast
;; @suite default
;; @boundary unit

;; test-release-entry-current.rkt — W7 release-entry contract for the
;; canonical release (version from util/version.rkt, never a literal).
;;
;; Pins, derived from the canonical version surface (never from a hardcoded
;; literal, so the test survives future bumps):
;;   1. version surfaces agree: util/version.rkt == info.rkt == README badge;
;;   2. the changelog release entry for the canonical version exists, carries
;;      the "Released <date>." line, and references its milestone;
;;   3. the release-language lint (lint-release-notes) passes for the entry;
;;   4. the entry names the milestone's activated and non-activated scheduler
;;      states explicitly (activated / not activated language is present).

(require rackunit
         rackunit/text-ui
         racket/file
         racket/runtime-path
         racket/string)

(define-runtime-path version-path "../util/version.rkt")
(define-runtime-path info-path "../info.rkt")
(define-runtime-path readme-path "../README.md")
(define-runtime-path changelog-path "../CHANGELOG.md")
(define-runtime-path notes-lint-path "../scripts/lint-release-notes.rkt")
(define-runtime-path surface-path "../scripts/version-surface.rkt")

(define (surface-ref sym)
  (dynamic-require surface-path sym))

(define canonical-version
  (or ((surface-ref 'parse-q-version-from-content) (file->string version-path))
      (raise-user-error 'test-release-entry-current "cannot parse q-version from util/version.rkt")))

(define changelog-text (file->string changelog-path))

(define (entry-block-for text version)
  (define extract
    (dynamic-require (string->path (path->string notes-lint-path)) 'extract-version-block))
  (extract text version))

(define release-entry-suite
  (test-suite "release-entry-current"
    (test-case "version surfaces agree: version.rkt, info.rkt, README badge"
      (check-equal? ((surface-ref 'parse-info-version-from-content) (file->string info-path))
                    canonical-version
                    "info.rkt version must match the canonical version")
      (check-true (string-contains? (file->string readme-path)
                                    (format "version-~a-blue" canonical-version))
                  "README badge must display the canonical version"))
    (test-case "changelog release entry for the canonical version exists"
      (check-true (string-contains? changelog-text (format "## v~a — " canonical-version))
                  "CHANGELOG.md must carry a release heading for the canonical version"))
    (test-case "release entry carries Released line and milestone reference"
      (define block (entry-block-for changelog-text canonical-version))
      (check-not-false block)
      (when block
        (check-true (regexp-match? #px"Released [0-9]{4}-[0-9]{2}-[0-9]{2}\\." block)
                    "release entry must carry the 'Released YYYY-MM-DD.' line")
        (check-true (regexp-match? #px"(?i:milestone) #[0-9]+" block)
                    "release entry must reference its milestone")))
    (test-case "release-language lint passes for the canonical entry"
      (define lint-changelog
        (dynamic-require (string->path (path->string notes-lint-path)) 'lint-changelog))
      (define violations (lint-changelog changelog-path canonical-version))
      (check-equal? violations '()))
    (test-case "entry names activated and non-activated states explicitly"
      (define block (entry-block-for changelog-text canonical-version))
      (check-not-false block)
      (when block
        (check-true (regexp-match? #rx"(?i:not activated)" block)
                    "release entry must name non-activated scheduler states")
        (check-true (regexp-match? #rx"(?i:activated)" block)
                    "release entry must name activated states")))))

(module+ main
  (define result (run-tests release-entry-suite))
  (exit (if (= 0 result) 0 1)))
