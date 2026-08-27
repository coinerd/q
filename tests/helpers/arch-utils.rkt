#lang racket

;; @speed fast  ;; @suite arch

;; BOUNDARY: integration

;; tests/helpers/arch-utils.rkt — Shared architecture test utilities
;;
;; Provides read-based S-expression require extraction and layer
;; boundary checking for architecture fitness/boundary tests.
;; Extracted from test-arch-fitness.rkt and test-arch-boundaries.rkt
;; to eliminate duplication.
;;
;; Refs: FIT-04

(require racket/port
         racket/string
         racket/runtime-path)

(provide extract-requires
         require-spec->paths
         imports-from?
         rkt-files-in
         line-count
         char-count
         count-provides
         q-dir
         rkt-files-in-recursive
         extract-parameters)

;; ============================================================
;; Read-based S-expression require parser
;; ============================================================

;; Extract all require-spec sub-forms from a source file.
;; Returns a flat list of require-spec items (strings, symbols, or
;; lists like (only-in "../tools/tool.rkt" ...)).
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
                 [(and (pair? form)
                       (memq (car form)
                             '(require require/typed
                                       require/typed/contract)))
                  ;; (require x y z) / (require/typed path spec ...) — cdr gives the specs
                  (if (and (pair? (cdr form)) (pair? (cadr form)))
                      ;; (require (only-in ...) x y) — flattened
                      (cdr form)
                      (cdr form))]
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
       [(require/typed require/typed/contract)
        ;; (require/typed "path" spec ...) — module path is the second element
        (if (and (pair? (cdr spec)) (string? (cadr spec)))
            (list (cadr spec))
            '())]
       [else (append* (map require-spec->paths (cdr spec)))])]
    [else '()]))

;; Check if any require spec imports from any of the given layer prefixes.
(define (imports-from? req-specs layer-prefixes)
  (for*/or ([spec (in-list req-specs)]
            [path (in-list (require-spec->paths spec))])
    (for/or ([prefix (in-list layer-prefixes)])
      (string-contains? path prefix))))

;; Extract parameter names defined via (make-parameter ...) in a source file.
;; Returns a list of symbols. Handles define, define/typed, and define/contract.
;; Walks all top-level forms recursively; only parameters bound by a define
;; whose value is a (make-parameter ...) form are returned.
(define (extract-parameters filepath)
  (with-handlers ([exn:fail? (lambda (e) '())])
    (define src (file->string filepath))
    (define lines (string-split src "\n"))
    (define rest
      (string-join (if (and (pair? lines) (string-prefix? (car lines) "#lang"))
                       (cdr lines)
                       lines)
                   "\n"))
    (define forms (port->list read (open-input-string rest)))
    (define params '())
    ;; Robust list traversal that tolerates improper (dotted) lists.
    (define (walk-list l)
      (let loop ([l l])
        (when (pair? l)
          (walk (car l))
          (loop (cdr l)))))
    (define (walk form)
      (cond
        [(pair? form)
         (define op (car form))
         (cond
           [(and (memq op '(define define/typed define/contract)) (pair? (cdr form)))
            (define name (cadr form))
            (define value-form
              (cond
                [(pair? name) #f] ; function definition
                [else
                 (cond
                   [(and (pair? (cddr form)) (memq op '(define/typed define/contract)))
                    (caddr (cdr form))]
                   [else (caddr form)])]))
            (when (and value-form (pair? value-form) (eq? (car value-form) 'make-parameter))
              (set! params (cons name params)))]
           [else (void)])
         (walk-list form)]
        [else (void)]))
    (for ([f (in-list forms)])
      (walk f))
    params))

;; ============================================================
;; File system helpers
;; ============================================================

;; BUG-0033: resolve the repo root relative to THIS FILE (tests/helpers/)
;; so arch tests pass from any cwd. Q_DIR remains an explicit override for
;; CI environments that relocate the checkout.
(define-runtime-path repo-root/default "../..")
(define q-dir (simplify-path (string->path (or (getenv "Q_DIR") (path->string repo-root/default)))))

(define (rkt-files-in dir)
  (if (directory-exists? (build-path q-dir dir))
      (filter (λ (f) (regexp-match? #rx"\\.rkt$" (path->string f)))
              (directory-list (build-path q-dir dir) #:build? #t))
      '()))

;; Recursive variant — includes files in subdirectories.
(define (rkt-files-in-recursive dir)
  (define base (build-path q-dir dir))
  (if (directory-exists? base)
      (let loop ([path base])
        (append* (for/list ([f (in-list (directory-list path #:build? #t))])
                   (cond
                     [(directory-exists? f) (loop f)]
                     [(regexp-match? #rx"\\.rkt$" (path->string f)) (list f)]
                     [else '()]))))
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
