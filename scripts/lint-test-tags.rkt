#lang racket/base

;; lint-test-tags.rkt — Lint test files for @suite and @speed metadata tags.
;;
;; Usage:
;;   racket scripts/lint-test-tags.rkt           # check all test files
;;   racket scripts/lint-test-tags.rkt --fix     # auto-tag untagged files
;;
;; Exits 0 if all files tagged, 1 if any untagged.

(require racket/string
         racket/path
         racket/file
         racket/port
         racket/list)

;; ---------------------------------------------------------------------------
;; Heuristics (mirrors classify.rkt)
;; ---------------------------------------------------------------------------

(define slow-patterns
  '("sandbox" "subprocess" "integration" "benchmark" "workflow-" "e2e-"
              "ci_local" "metrics-readme" "bump-version" "examples-compile"
              "pre-commit" "racket-tooling" "run-tests" "audit-script"
              "test-doctor" "check-deps" "self-hosting" "tui-terminal" "sync-readme"))

(define (detect-speed-tag f)
  (define base (path->string (file-name-from-path f)))
  (cond
    [(for/or ([p (in-list slow-patterns)])
       (string-contains? base p))
     "slow"]
    [(string-contains? f "/workflows/")
     "slow"]
    [else "fast"]))

(define (detect-suite-tag f)
  (cond
    [(string-contains? f "/tui/") "tui"]
    [(string-contains? f "/interfaces/tui") "tui"]
    [(string-prefix? (path->string (file-name-from-path f)) "test-tui-") "tui"]
    [(string-contains? f "/workflows/") "workflows"]
    [(or (string-contains? f "security")
         (string-contains? f "permission")
         (string-contains? f "sandbox")
         (string-contains? f "safe-mode"))
     "security"]
    [(or (string-contains? f "arch-")
         (string-contains? f "boundary")
         (string-contains? f "fitness")
         (string-contains? f "hotspot"))
     "arch"]
    [(or (string-contains? f "runtime")
         (string-contains? f "session")
         (string-contains? f "compaction")
         (string-contains? f "iteration")
         (string-contains? f "turn-")
         (string-contains? f "tool-coord"))
     "runtime"]
    [(or (string-contains? f "extensions/")
         (string-contains? f "gsd-")
         (string-contains? f "define-extension")
         (string-contains? f "wave-executor")
         (string-contains? f "hook-"))
     "extensions"]
    [else #f]))

;; ---------------------------------------------------------------------------
;; Tag checking
;; ---------------------------------------------------------------------------

(define (read-first-n-lines path n)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (call-with-input-file path
      (lambda (port)
        (for/list ([_ (in-range n)]
                   #:break (eof-object? (peek-byte port)))
          (read-line port))))))

(define (has-tag? lines tag-name)
  (define rx (regexp (format ";+[ \\t]*@~a[ \\t]+" tag-name)))
  (for/or ([line (in-list lines)])
    (and (string? line) (regexp-match? rx line))))

;; ---------------------------------------------------------------------------
;; Auto-tagging
;; ---------------------------------------------------------------------------

(define (add-tags-to-file! f speed suite)
  (with-handlers ([exn:fail? (lambda (e) (void))])
    (define lines (file->lines f))
    ;; Find the line after the header comments (after #lang line and comment block)
    ;; Insert tags after #lang line
    (define insert-idx
      (for/first ([line (in-list lines)]
                  [i (in-naturals)]
                  #:when (string-prefix? (string-trim line) "#lang"))
        (add1 i)))
    (define idx (or insert-idx 1))
    (define tag-line
      (format ";; @speed ~a~a" speed
              (if suite (format "  ;; @suite ~a" suite) "")))
    (define new-lines
      (append (take lines idx)
              (list "" tag-line)
              (drop lines idx)))
    (display-lines-to-file new-lines f #:exists 'replace)))

;; ---------------------------------------------------------------------------
;; Collect test files
;; ---------------------------------------------------------------------------

(define (collect-test-files)
  (define root (find-system-path 'orig-dir))
  (define tests-dir (build-path root "tests"))
  (if (directory-exists? tests-dir)
      (for/list ([f (in-directory tests-dir)]
                 #:when (and (file-exists? f)
                             (string-suffix? (path->string f) ".rkt")
                             (not (string-contains? (path->string f) "/compiled/"))))
        f)
      '()))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (main args)
  (define fix-mode? (and (pair? args) (equal? (car args) "--fix")))
  (define files (collect-test-files))

  (define results
    (for/list ([f (in-list files)])
      (define lines (read-first-n-lines f 5))
      (define has-speed? (has-tag? lines "speed"))
      (define has-suite? (has-tag? lines "suite"))
      (list f has-speed? has-suite?)))

  (define untagged
    (filter (lambda (r) (or (not (cadr r)) (not (caddr r)))) results))

  (cond
    [(null? untagged)
     (printf "All ~a test files have @speed and @suite tags.\n" (length files))
     (exit 0)]
    [fix-mode?
     (for ([r (in-list untagged)])
       (define f (car r))
       (define fstr (path->string f))
       (define speed (detect-speed-tag fstr))
       (define suite (detect-suite-tag fstr))
       (add-tags-to-file! f speed suite)
       (printf "Tagged: ~a (@speed ~a~a)\n"
               (path->string (file-name-from-path f))
               speed
               (if suite (format " @suite ~a" suite) "")))
     (printf "Tagged ~a files.\n" (length untagged))
     (exit 0)]
    [else
     (printf "Untagged files (~a):\n" (length untagged))
     (for ([r (in-list untagged)])
       (define f (car r))
       (printf "  ~a  speed=~a suite=~a\n"
               (path->string f)
               (if (cadr r) "yes" "MISSING")
               (if (caddr r) "yes" "MISSING")))
     (exit 1)]))

(module+ main
  (main (vector->list (current-command-line-arguments))))
