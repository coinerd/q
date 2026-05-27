#!/usr/bin/env racket
#lang racket/base

;; lint-release-readiness.rkt — CI release hygiene gate
;;
;; Verifies that all pre-release conditions are met:
;;   1. Version sync: util/version.rkt, info.rkt, README.md agree
;;   2. CHANGELOG entry: CHANGELOG.md has an entry for the current version
;;   3. Git clean: no uncommitted changes
;;   4. On main branch
;;   5. (strict mode) Tag does not already exist
;;   6. (strict mode) Gate evidence: recent passing results for required suites
;;
;; Exit codes:
;;   0 — all checks pass, ready to release
;;   1 — one or more checks failed
;;
;; Usage:
;;   racket scripts/lint-release-readiness.rkt           # dev mode
;;   racket scripts/lint-release-readiness.rkt --strict   # release mode (gate evidence + tag check)

(provide get-canonical-version
         get-info-version
         check-version-sync
         check-tag-unique
         check-changelog-entry
         check-git-clean
         check-main-branch
         check-gate-evidence
         required-gate-suites
         parse-argv)

(require racket/file
         racket/string
         racket/port
         racket/system
         racket/list
         racket/path)

;; ---------------------------------------------------------------------------
;; Version extraction
;; ---------------------------------------------------------------------------

(define (read-version-from-file path pattern)
  (and (file-exists? path)
       (let* ([content (file->string path)]
              [m (regexp-match pattern content)])
         (and m (cadr m)))))

(define (get-canonical-version)
  (or (read-version-from-file "util/version.rkt" #rx"define q-version \"([^\"]+)\"")
      (error "Cannot read version from util/version.rkt")))

(define (get-info-version)
  (read-version-from-file "info.rkt" #rx"define version \"([^\"]+)\""))

;; ---------------------------------------------------------------------------
;; Check 1: Version sync
;; ---------------------------------------------------------------------------

(define (check-version-sync)
  (define canonical (get-canonical-version))
  (define info-ver (get-info-version))
  (cond
    [(equal? canonical info-ver)
     (printf "  [PASS] version sync: ~a~n" canonical)
     #t]
    [else
     (printf "  [FAIL] version drift: util/version.rkt=~a, info.rkt=~a~n" canonical info-ver)
     #f]))

;; ---------------------------------------------------------------------------
;; Check 2: Tag uniqueness
;; ---------------------------------------------------------------------------

(define (check-tag-unique)
  (define ver (get-canonical-version))
  (define tag (format "v~a" ver))
  (define existing
    (with-input-from-string (with-output-to-string (lambda () (system "git tag -l")))
                            (lambda ()
                              (for/or ([line (in-lines)])
                                (equal? (string-trim line) tag)))))
  (cond
    [(not existing)
     (printf "  [PASS] tag ~a does not exist yet~n" tag)
     #t]
    [else
     (printf "  [FAIL] tag ~a already exists — bump version first~n" tag)
     #f]))

;; ---------------------------------------------------------------------------
;; Check 3: CHANGELOG entry
;; ---------------------------------------------------------------------------

(define (check-changelog-entry)
  (define ver (get-canonical-version))
  (cond
    [(not (file-exists? "CHANGELOG.md"))
     (printf "  [FAIL] CHANGELOG.md not found~n")
     #f]
    [else
     (define content (file->string "CHANGELOG.md"))
     (define bare-pattern (regexp (format "## ~a" (regexp-quote ver))))
     (define v-prefix-pattern (regexp (format "## v~a" (regexp-quote ver))))
     (cond
       [(or (regexp-match? bare-pattern content) (regexp-match? v-prefix-pattern content))
        (printf "  [PASS] CHANGELOG has entry for ~a~n" ver)
        #t]
       [else
        (printf "  [FAIL] CHANGELOG missing entry for ~a~n" ver)
        #f])]))

;; ---------------------------------------------------------------------------
;; Check 4: Git clean (no uncommitted changes)
;; ---------------------------------------------------------------------------

(define (check-git-clean)
  (define status (string-trim (with-output-to-string (lambda () (system "git status --porcelain")))))
  (cond
    [(equal? status "")
     (printf "  [PASS] git working tree clean~n")
     #t]
    [else
     (define n (length (string-split status "\n")))
     (printf "  [FAIL] git has ~a uncommitted change(s)~n" n)
     #f]))

;; ---------------------------------------------------------------------------
;; Check 5: On main branch
;; ---------------------------------------------------------------------------

(define (check-main-branch)
  (define branch
    (string-trim (with-output-to-string (lambda () (system "git rev-parse --abbrev-ref HEAD")))))
  (cond
    [(equal? branch "main")
     (printf "  [PASS] on main branch~n")
     #t]
    [else
     (printf "  [FAIL] on branch '~a' — must be on main~n" branch)
     #f]))

;; ---------------------------------------------------------------------------
;; Check 6: Gate evidence (required suites have recent passing results)
;; ---------------------------------------------------------------------------

(define required-gate-suites '("fast" "tui" "arch" "workflows"))

(define (gate-evidence-dir)
  (build-path ".gate-evidence"))

(define (check-gate-evidence)
  (define evid-dir (gate-evidence-dir))
  (cond
    [(not (directory-exists? evid-dir))
     (printf "  [FAIL] no .gate-evidence/ directory — run test suites with --record-gate-evidence~n")
     #f]
    [else
     (define ver (get-canonical-version))
     (define results
       (for/list ([suite (in-list required-gate-suites)])
         (define evidence-file (build-path evid-dir (format "~a.passed" suite)))
         (cond
           [(not (file-exists? evidence-file))
            (printf "  [FAIL] gate evidence missing: ~a~n" suite)
            #f]
           [else
            (define content (file->string evidence-file))
            (define parts (string-split content " "))
            (define ev-version (and (>= (length parts) 1) (car parts)))
            (define ev-time (and (>= (length parts) 2) (string->number (cadr parts))))
            (define now (current-seconds))
            (define max-age 7200) ;; 2 hours
            (cond
              [(not (equal? ev-version ver))
               (printf "  [FAIL] gate evidence for ~a is stale (version ~a, expected ~a)~n"
                       suite
                       ev-version
                       ver)
               #f]
              [(and ev-time (> (- now ev-time) max-age))
               (printf "  [FAIL] gate evidence for ~a is too old (~a seconds)~n"
                       suite
                       (- now ev-time))
               #f]
              [else
               (printf "  [PASS] gate evidence: ~a (v~a)~n" suite ev-version)
               #t])])))
     (andmap values results)]))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (parse-argv argv)
  (define strict? (member "--strict" argv))
  (define context-idx (index-of argv "--context"))
  (define context
    (if context-idx
        (let ([next (and (< context-idx (sub1 (length argv))) (list-ref argv (add1 context-idx)))])
          (if (member next '("pre-tag" "tag-publish"))
              (string->symbol next)
              #f))
        #f))
  (values strict? context))

(define (main)
  (define argv (vector->list (current-command-line-arguments)))
  (define-values (strict? context) (parse-argv argv))

  (unless (file-exists? "util/version.rkt")
    (displayln "ERROR: Run from q/ project root (util/version.rkt not found)")
    (exit 1))

  (when (and strict? context (not (member context '(pre-tag tag-publish))))
    (displayln "ERROR: --context must be 'pre-tag' or 'tag-publish'")
    (exit 1))

  (define ver (get-canonical-version))
  (define context-label
    (cond
      [(not strict?) ""]
      [context (format "[~a] " context)]
      [else "[STRICT] "]))
  (printf "~n── Release Readiness Check (v~a) ~a──~n" ver context-label)

  (define base-results
    (list (check-version-sync) (check-changelog-entry) (check-git-clean) (check-main-branch)))

  ;; Tag check: only in pre-tag context (not tag-publish, not dev)
  ;; tag-publish context: tag already exists by design
  (define tag-result
    (cond
      [(and strict? (or (not context) (eq? context 'pre-tag))) (check-tag-unique)]
      [else #t]))

  ;; Gate evidence: in strict mode only
  (define gate-result
    (if strict?
        (check-gate-evidence)
        #t))

  (define results
    (if strict?
        (append base-results (list tag-result gate-result))
        base-results))

  (define passed (count values results))
  (define failed (- (length results) passed))

  (printf "~n── Summary: ~a/~a checks passed ~a──~n"
          passed
          (length results)
          (if (positive? failed) "❌ " "✅ "))

  (exit (if (positive? failed) 1 0)))

;; Only auto-run when executed directly as a script
(define invoked-directly?
  (let ([run-file (find-system-path 'run-file)])
    (and (path? run-file)
         (let ([base (file-name-from-path run-file)])
           (and base (equal? (path->string base) "lint-release-readiness.rkt"))))))
(when invoked-directly?
  (main))
