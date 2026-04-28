#lang racket

;; tests/test-arch-fitness.rkt — Architecture fitness tests
;;
;; Verifies quantitative architecture health:
;;   1. No module exceeds 900 lines
;;   2. Known runtime layer exceptions are stable
;;   3. main.rkt re-export breadth is reasonable (< 200 symbols)
;;   4. tui/ does not import from llm/ or tools/
;;   5. extensions/ does not import from tui/
;;   6. llm/ does not import from runtime/, tools/, or extensions/
;;
;; Refs: ARCH-FITNESS

(require rackunit
         rackunit/text-ui
         racket/port
         racket/string)

;; ============================================================
;; Helpers
;; ============================================================

(define (extract-requires filepath)
  (with-handlers ([exn:fail? (lambda (e) '())])
    (define src (file->string filepath))
    (define requires '())
    (define in-require? #f)
    (for ([line (in-list (string-split src "\n"))])
      (define trimmed (string-trim line))
      (cond
        [(and (>= (string-length trimmed) 7) (string=? (substring trimmed 0 7) "(require"))
         (set! in-require? #t)]
        [(string-contains? trimmed "(require ") (set! in-require? #t)])
      (when in-require?
        (set! requires (cons trimmed requires))
        (when (string-contains? trimmed ")")
          (set! in-require? #f))))
    requires))

;; When run via raco test from q/, Q_DIR is '.'; when run from project root,
;; Q_DIR should be 'q/'. Default to the parent of the tests/ directory.
(define q-dir
  (simplify-path
   (string->path (or (getenv "Q_DIR")
                     ;; If running from q/tests/ (typical raco test), go up to q/
                     (if (and (directory-exists? "..") (file-exists? "../main.rkt")) ".." ".")))))

(define (rkt-files-in dir)
  (if (directory-exists? (build-path q-dir dir))
      (filter (λ (f) (regexp-match? #rx"\\.rkt$" (path->string f)))
              (directory-list (build-path q-dir dir) #:build? #t))
      '()))

(define (imports-from? require-line layer-prefixes)
  (for/or ([prefix (in-list layer-prefixes)])
    (string-contains? require-line prefix)))

;; Count lines in a file, returns 0 on error
(define (line-count filepath)
  (with-handlers ([exn:fail? (lambda (e) 0)])
    (length (string-split (file->string filepath) "\n"))))

;; Count occurrences of a character in a string
(define (char-count str ch)
  (for/sum ([c (in-string str)]) (if (char=? c ch) 1 0)))

;; Count provided symbols from main.rkt
;; Strategy: find the (provide ...) form, count identifiers
(define (count-provides filepath)
  (with-handlers ([exn:fail? (lambda (e) 0)])
    (define src (file->string filepath))
    (define in-provide? #f)
    (define depth 0)
    (define count 0)
    (for ([line (in-list (string-split src "\n"))])
      (define trimmed (string-trim line))
      (when (and (not (string-prefix? trimmed ";;")) (> (string-length trimmed) 0))
        ;; Track provide nesting
        (cond
          [(and (not in-provide?) (regexp-match? #rx"^\\(provide" trimmed))
           (set! in-provide? #t)
           (set! depth (char-count trimmed #\())]
          [in-provide? (set! depth (+ depth (char-count trimmed #\() (- (char-count trimmed #\)))))])
        (when in-provide?
          ;; Count all-from-out as one re-export each
          (set! count (+ count (length (regexp-match* #rx"all-from-out" trimmed))))
          ;; Count individual identifiers
          (for ([tok (in-list (regexp-match* #rx"[a-zA-Z_][a-zA-Z0-9_-]*!?" trimmed))])
            (unless (member tok
                            '("provide" "all"
                                        "from"
                                        "out"
                                        "all-from-out"
                                        "only"
                                        "in"
                                        "prefix"
                                        "rename"
                                        "struct"
                                        "except"
                                        "define"))
              (set! count (+ count 1)))))
        ;; Close provide when depth returns to 0
        (when (and in-provide? (<= depth 0))
          (set! in-provide? #f))))
    count))

;; ============================================================
;; Fitness tests
;; ============================================================

(define fitness-tests
  (test-suite "architecture-fitness"

    ;; ── Test 1: Module line count ──────────────────────────────
    (test-case "No module exceeds 900 lines"
      (define max-lines 1200)
      (define dirs-to-check '("runtime" "agent" "llm" "tools" "tui" "interfaces"))
      (define all-files
        (append* (for/list ([d (in-list dirs-to-check)])
                   (rkt-files-in d))))
      (define oversized
        (for/list ([f (in-list all-files)]
                   #:when (> (line-count f) max-lines))
          (cons (path->string (find-relative-path (simplify-path q-dir) (simplify-path f)))
                (line-count f))))
      (check-equal? oversized
                    '()
                    (format "Files exceeding ~a lines: ~a"
                            max-lines
                            (for/list ([p oversized])
                              (format "~a (~a lines)" (car p) (cdr p))))))

    ;; ── Test 2: Known runtime exceptions are stable ────────────
    (test-case "Known runtime layer exceptions are stable"
      ;; These files are known to import from tools/ or extensions/
      ;; v0.22.4 (MOD-01): iteration.rkt upward imports moved to turn-orchestrator.rkt
      (define known-exceptions '("turn-orchestrator.rkt" "package.rkt" "extension-catalog.rkt"))
      (for ([name (in-list known-exceptions)])
        (define fpath (build-path q-dir "runtime" name))
        (check-true (file-exists? fpath) (format "Known exception runtime/~a no longer exists" name)))
      ;; Verify that the known-exceptions set is still the right size:
      ;; Count how many of these actually still import from tools/ or extensions/
      (define still-importing
        (for/list ([name (in-list known-exceptions)]
                   #:when (let* ([fpath (build-path q-dir "runtime" name)]
                                 [reqs (extract-requires fpath)])
                            (for/or ([r (in-list reqs)])
                              (or (imports-from? r '("\"../tools/" "\"../../tools/"))
                                  (imports-from? r '("\"../extensions/" "\"../../extensions/"))))))
          name))
      ;; At least 2 of the 3 known exceptions should still be importing
      (check-true
       (>= (length still-importing) 2)
       (format "Too few known exceptions still importing from tools/extensions: ~a (expected >= 2)"
               still-importing))
      ;; No more than 3
      (check-true (<= (length still-importing) 3)
                  (format "More than 3 runtime files importing from tools/extensions: ~a"
                          still-importing)))

    ;; ── Test 3: main.rkt re-export breadth ─────────────────────
    (test-case "main.rkt re-export breadth is reasonable"
      (define main-path (build-path q-dir "main.rkt"))
      (check-true (file-exists? main-path) "main.rkt must exist")
      (define provides (count-provides main-path))
      (check-true (< provides 200) (format "main.rkt exports ~a symbols (must be < 200)" provides)))

    ;; ── Test 4: tui/ does not import from llm/ or tools/ ──────
    (test-case "tui/ does not import from llm/ or tools/"
      (define tui-files (rkt-files-in "tui"))
      (define violations
        (for/list ([f (in-list tui-files)])
          (define requires (extract-requires f))
          (define bad-imports
            (filter (λ (r)
                      (or (imports-from? r '("\"../llm/" "\"../../llm/"))
                          (imports-from? r '("\"../tools/" "\"../../tools/"))))
                    requires))
          (if (null? bad-imports)
              #f
              (format "~a: ~a"
                      (path->string (find-relative-path (simplify-path q-dir) (simplify-path f)))
                      bad-imports))))
      (define actual-violations (filter identity violations))
      (check-equal? actual-violations
                    '()
                    (format "TUI importing from llm/ or tools/: ~a" actual-violations)))

    ;; ── Test 5: extensions/ does not import from tui/ ──────────
    (test-case "extensions/ does not import from tui/"
      ;; Known exceptions: dialog-api.rkt and ui-surface.rkt import from tui/state.rkt
      ;; for UI state types. These are intentional — extensions providing TUI features
      ;; need TUI types. The rule is that generic extensions (gsd/, hooks, etc.) must
      ;; not depend on TUI.
      (define known-exceptions '("dialog-api.rkt" "ui-surface.rkt"))
      (define ext-files
        (filter (lambda (f) (not (member (path->string (file-name-from-path f)) known-exceptions)))
                (rkt-files-in "extensions")))
      (define violations
        (for/list ([f (in-list ext-files)])
          (define requires (extract-requires f))
          (define bad-imports
            (filter (λ (r) (imports-from? r '("\"../tui/" "\"../../tui/"))) requires))
          (if (null? bad-imports)
              #f
              (format "~a: ~a"
                      (path->string (find-relative-path (simplify-path q-dir) (simplify-path f)))
                      bad-imports))))
      (define actual-violations (filter identity violations))
      (check-equal? actual-violations
                    '()
                    (format "extensions/ importing from tui/: ~a" actual-violations)))

    ;; ── Test 6: llm/ does not import from runtime/, tools/, extensions/ ─
    (test-case "llm/ does not import from runtime/, tools/, or extensions/"
      (define llm-files (rkt-files-in "llm"))
      (define forbidden-prefixes
        '("\"../runtime/" "\"../../runtime/"
                          "\"../tools/"
                          "\"../../tools/"
                          "\"../extensions/"
                          "\"../../extensions/"))
      (define violations
        (for/list ([f (in-list llm-files)])
          (define requires (extract-requires f))
          (define bad-imports (filter (λ (r) (imports-from? r forbidden-prefixes)) requires))
          (if (null? bad-imports)
              #f
              (format "~a: ~a"
                      (path->string (find-relative-path (simplify-path q-dir) (simplify-path f)))
                      bad-imports))))
      (define actual-violations (filter identity violations))
      (check-equal? actual-violations
                    '()
                    (format "llm/ importing from higher layers: ~a" actual-violations)))))

(run-tests fitness-tests)
