#lang racket

;; tests/test-arch-fitness.rkt — Architecture fitness tests
;;
;; Verifies quantitative architecture health:
;;   1. No module exceeds 900 lines (with known-large tracked items)
;;   2. Known runtime layer exceptions are stable
;;   3. main.rkt re-export breadth is reasonable (< 200 symbols)
;;   4. tui/ does not import from llm/ or tools/
;;   5. extensions/ does not import from tui/
;;   6. llm/ does not import from runtime/, tools/, extensions/
;;
;; Refs: ARCH-FITNESS

(require rackunit
         rackunit/text-ui
         racket/port
         racket/string)

;; ============================================================
;; Helpers (read-based S-expression parser)
;; ============================================================

;; Extract all require-spec sub-forms from a source file.
;; Returns a list of require-spec items (strings, symbols, or lists like
;; (only-in "../tools/tool.rkt" ...)).
;; Uses Racket's `read` for robust multi-line require parsing.
(define (extract-requires filepath)
  (with-handlers ([exn:fail? (lambda (e) '())])
    (define src (file->string filepath))
    ;; Skip #lang line, then read top-level forms
    (define lines (string-split src "\n"))
    (define rest
      (string-join (if (string-prefix? (car lines) "#lang")
                       (cdr lines)
                       lines)
                   "\n"))
    (define forms (port->list read (open-input-string rest)))
    (append* (for/list ([form forms])
               (cond
                 [(and (pair? form) (eq? (car form) 'require))
                  ;; Single require form — could be one spec or sub-forms
                  (if (and (pair? (cdr form)) (pair? (cadr form)))
                      ;; (require x y z) — multiple specs in parens
                      (cdr form)
                      ;; (require x) — single spec
                      (cdr form))]
                 ;; Skip submodule forms
                 [(and (pair? form) (eq? (car form) 'module+)) '()]
                 [else '()])))))

;; Extract all string paths from a require-spec.
;; Handles: strings, (only-in "path" ...), (prefix-in "pref" "path"), etc.
(define (require-spec->paths spec)
  (cond
    [(string? spec) (list spec)]
    [(symbol? spec) '()] ; e.g. racket/contract
    [(pair? spec)
     (case (car spec)
       [(only-in prefix-in rename-in except-in)
        ;; Second argument is usually the path
        (if (and (pair? (cdr spec)) (string? (cadr spec)))
            (list (cadr spec))
            '())]
       [else (append* (map require-spec->paths (cdr spec)))])]
    [else '()]))

;; Check if any require spec imports from any of the given layer prefixes.
;; Works on structured require specs (not raw lines).
(define (imports-from? req-specs layer-prefixes)
  (for*/or ([spec (in-list req-specs)]
            [path (in-list (require-spec->paths spec))])
    (for/or ([prefix (in-list layer-prefixes)])
      (string-contains? path prefix))))

(define q-dir
  (simplify-path
   (string->path (or (getenv "Q_DIR")
                     (if (and (directory-exists? "..") (file-exists? "../main.rkt")) ".." ".")))))

(define (rkt-files-in dir)
  (if (directory-exists? (build-path q-dir dir))
      (filter (λ (f) (regexp-match? #rx"\\.rkt$" (path->string f)))
              (directory-list (build-path q-dir dir) #:build? #t))
      '()))

(define (line-count filepath)
  (with-handlers ([exn:fail? (lambda (e) 0)])
    (length (string-split (file->string filepath) "\n"))))

(define (char-count str ch)
  (for/sum ([c (in-string str)]) (if (char=? c ch) 1 0)))

(define (count-provides filepath)
  (with-handlers ([exn:fail? (lambda (e) 0)])
    (define src (file->string filepath))
    (define in-provide? #f)
    (define depth 0)
    (define count 0)
    (for ([line (in-list (string-split src "\n"))])
      (define trimmed (string-trim line))
      (when (and (not (string-prefix? trimmed ";;")) (> (string-length trimmed) 0))
        (cond
          [(and (not in-provide?) (regexp-match? #rx"^\\(provide" trimmed))
           (set! in-provide? #t)
           (set! depth (char-count trimmed #\())]
          [in-provide? (set! depth (+ depth (char-count trimmed #\() (- (char-count trimmed #\)))))])
        (when in-provide?
          (set! count (+ count (length (regexp-match* #rx"all-from-out" trimmed))))
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
      (define max-lines 900)
      ;; Known-large modules tracked for future splitting (v0.23.0 target):
      ;;   tui/state.rkt (~994 lines), extensions/racket-tooling.rkt (~922 lines)
      (define known-large '(("tui/state.rkt" . 1000) ("extensions/racket-tooling.rkt" . 950)))
      (define dirs-to-check '("runtime" "agent" "llm" "tools" "tui" "interfaces"))
      (define all-files
        (append* (for/list ([d (in-list dirs-to-check)])
                   (rkt-files-in d))))
      (define oversized
        (for/list ([f (in-list all-files)]
                   #:when (let ([lc (line-count f)])
                            (and (> lc max-lines)
                                 ;; Not in known-large list
                                 (not (assoc (path->string (find-relative-path (simplify-path q-dir)
                                                                               (simplify-path f)))
                                             known-large)))))
          (cons (path->string (find-relative-path (simplify-path q-dir) (simplify-path f)))
                (line-count f))))
      (check-equal? oversized
                    '()
                    (format "Files exceeding ~a lines (excluding known-large): ~a"
                            max-lines
                            (for/list ([p oversized])
                              (format "~a (~a lines)" (car p) (cdr p))))))

    ;; ── Test 2: Known runtime exceptions are stable ────────────
    (test-case "Known runtime layer exceptions are stable"
      ;; v0.22.4 (MOD-01): iteration.rkt -> turn-orchestrator.rkt
      (define known-exceptions '("turn-orchestrator.rkt" "package.rkt" "extension-catalog.rkt"))
      (for ([name (in-list known-exceptions)])
        (define fpath (build-path q-dir "runtime" name))
        (check-true (file-exists? fpath) (format "Known exception runtime/~a no longer exists" name)))
      (define still-importing
        (for/list ([name (in-list known-exceptions)]
                   #:when (let* ([fpath (build-path q-dir "runtime" name)]
                                 [reqs (extract-requires fpath)])
                            (imports-from?
                             reqs
                             '("../tools/" "../../tools/" "../extensions/" "../../extensions/"))))
          name))
      (check-true
       (>= (length still-importing) 2)
       (format "Too few known exceptions still importing from tools/extensions: ~a (expected >= 2)"
               still-importing))
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
          (define reqs (extract-requires f))
          (if (imports-from? reqs '("../llm/" "../../llm/" "../tools/" "../../tools/"))
              (format "~a imports from llm/ or tools/"
                      (path->string (find-relative-path (simplify-path q-dir) (simplify-path f))))
              #f)))
      (define actual-violations (filter identity violations))
      (check-equal? actual-violations
                    '()
                    (format "TUI importing from llm/ or tools/: ~a" actual-violations)))

    ;; ── Test 5: extensions/ does not import from tui/ ──────────
    (test-case "extensions/ does not import from tui/"
      ;; Known exceptions: dialog-api.rkt and ui-surface.rkt import from tui/state.rkt
      (define known-exceptions '("dialog-api.rkt" "ui-surface.rkt"))
      (define ext-files
        (filter (lambda (f) (not (member (path->string (file-name-from-path f)) known-exceptions)))
                (rkt-files-in "extensions")))
      (define violations
        (for/list ([f (in-list ext-files)])
          (define reqs (extract-requires f))
          (if (imports-from? reqs '("../tui/" "../../tui/"))
              (format "~a imports from tui/"
                      (path->string (find-relative-path (simplify-path q-dir) (simplify-path f))))
              #f)))
      (define actual-violations (filter identity violations))
      (check-equal? actual-violations
                    '()
                    (format "extensions/ importing from tui/: ~a" actual-violations)))

    ;; ── Test 6: llm/ does not import from runtime/, tools/, extensions/ ─
    (test-case "llm/ does not import from runtime/, tools/, or extensions/"
      (define llm-files (rkt-files-in "llm"))
      (define forbidden-prefixes
        '("../runtime/" "../../runtime/"
                        "../tools/"
                        "../../tools/"
                        "../extensions/"
                        "../../extensions/"))
      (define violations
        (for/list ([f (in-list llm-files)])
          (define reqs (extract-requires f))
          (if (imports-from? reqs forbidden-prefixes)
              (format "~a imports from higher layers"
                      (path->string (find-relative-path (simplify-path q-dir) (simplify-path f))))
              #f)))
      (define actual-violations (filter identity violations))
      (check-equal? actual-violations
                    '()
                    (format "llm/ importing from higher layers: ~a" actual-violations)))))

(run-tests fitness-tests)
