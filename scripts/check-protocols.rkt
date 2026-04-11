#lang racket

;; check-protocols.rkt — Protocol/contract consistency checker
;; Reads protocols.rktd and verifies that all callers access return values
;; using the declared convention (struct vs list).
;;
;; Strategy:
;;   For each declared protocol function (e.g., hook-dispatcher → struct),
;;   find every .rkt file that calls it. In those files, identify result
;;   variables (bound via define/let/etc.) and check whether they are
;;   accessed with struct accessors or list accessors (car/cadr).
;;
;; Usage: racket scripts/check-protocols.rkt
;; Exit 0 = all consistent, 1 = mismatches found

(require racket/port)

;;; ---------------------------------------------------------------------------
;;; Configuration
;;; ---------------------------------------------------------------------------

(define project-root (simplify-path (path->complete-path ".")))
(define protocols-file (build-path project-root "scripts" "protocols.rktd"))

;;; ---------------------------------------------------------------------------
;;; Load protocol registry
;;; ---------------------------------------------------------------------------

(define (load-protocols)
  (call-with-input-file protocols-file
    (lambda (in)
      (define form (read in))
      (unless (list? form)
        (error 'load-protocols "expected alist, got ~a" form))
      (for/list ([entry (in-list form)])
        (unless (and (pair? entry) (symbol? (car entry)) (symbol? (cdr entry)))
          (error 'load-protocols "bad entry: ~a" entry))
        entry))))

;;; ---------------------------------------------------------------------------
;;; File discovery
;;; ---------------------------------------------------------------------------

(define (find-rkt-files root)
  (for/list ([f (in-directory root)]
             #:when (and (file-exists? f)
                         (let ([ext (filename-extension f)])
                           (and ext (bytes=? ext #"rkt")))
                         (not (regexp-match? #px"/benchmarks/" (path->string f)))
                         ;; Exclude this script itself
                         (not (regexp-match? #px"check-protocols\\.rkt$" (path->string f)))))
    f))

;;; ---------------------------------------------------------------------------
;;; Detection helpers
;;; ---------------------------------------------------------------------------

(define (file->lines path)
  (call-with-input-file path port->lines))

;; Extract result variable names from lines that call `func-name` inside a define.
;; Matches: (define <var> ... (func-name ...))
(define (find-result-vars lines func-name)
  (define func-pat (pregexp (format "\\(~a " (regexp-quote (symbol->string func-name)))))
  (define define-pat #px"\\(define\\s+([a-zA-Z_][a-zA-Z0-9_-]*)")
  (define vars '())
  (define len (length lines))

  (for ([i (in-range len)])
    (define line (list-ref lines i))
    (define define-m (regexp-match define-pat line))
    (when define-m
      ;; Collect the define body — may span multiple lines.
      (define join-end (min (+ i 8) len))
      (define body-text
        (string-join (for/list ([j (in-range i join-end)]) (list-ref lines j)) " "))
      (when (regexp-match? func-pat body-text)
        (set! vars (cons (cadr define-m) vars)))))
  (reverse vars))

;; Find lines where a result variable is accessed with car/cadr (list access)
(define (find-list-accesses lines var-name)
  (define var-str (symbol->string var-name))
  (define pat (pregexp (format "\\b(car|cadr|caddr|cadddr|cdr|cddr)\\s+~a\\b"
                          (regexp-quote var-str))))
  (for/list ([i (in-range (length lines))]
             #:when (regexp-match? pat (list-ref lines i)))
    (+ i 1)))

;; Find lines where a result variable is accessed with struct accessors
(define (find-struct-accesses lines var-name)
  (define var-str (symbol->string var-name))
  (define pat (pregexp (format "hook-result-(action|payload)\\s+~a\\b"
                          (regexp-quote var-str))))
  (for/list ([i (in-range (length lines))]
             #:when (regexp-match? pat (list-ref lines i)))
    (+ i 1)))

;; Check for inline access on the call itself:
;;   (car (hook-dispatcher ...))          — list
;;   (hook-result-action (hook-dispatcher ...)) — struct
(define (find-inline-accesses lines func-name)
  (define func-str (symbol->string func-name))
  (define list-inline (pregexp (format "\\b(car|cadr|caddr|cadddr)\\s+\\(~a " (regexp-quote func-str))))
  (define struct-inline (pregexp (format "hook-result-(action|payload)\\s+\\(~a " (regexp-quote func-str))))
  (define list-lines '())
  (define struct-lines '())
  (for ([i (in-range (length lines))])
    (define line (list-ref lines i))
    (when (regexp-match? list-inline line)
      (set! list-lines (cons (+ i 1) list-lines)))
    (when (regexp-match? struct-inline line)
      (set! struct-lines (cons (+ i 1) struct-lines))))
  (values (reverse list-lines) (reverse struct-lines)))

;;; ---------------------------------------------------------------------------
;;; Core scanning
;;; ---------------------------------------------------------------------------

(define (scan-protocol func-name expected-type files)
  (define findings '())
  (define func-str (symbol->string func-name))
  (define call-pat (pregexp (format "\\(~a " (regexp-quote func-str))))

  (for ([path (in-list files)])
    (define lines (file->lines path))
    (define rel-path (path->string (find-relative-path project-root path #:more-than-root? #t)))

    (define calls-function?
      (for/or ([line (in-list lines)])
        (regexp-match? call-pat line)))

    (when calls-function?
      ;; Phase 1: Find result variables bound to hook-dispatcher results
      (define result-vars (find-result-vars lines func-name))

      ;; Phase 2: For each result variable, check access patterns
      (define file-list-lines '())
      (define file-struct-lines '())

      (for ([var (in-list result-vars)])
        (set! file-list-lines (append file-list-lines (find-list-accesses lines (string->symbol var))))
        (set! file-struct-lines (append file-struct-lines (find-struct-accesses lines (string->symbol var)))))

      ;; Phase 3: Check inline accesses
      (define-values (inline-list inline-struct) (find-inline-accesses lines func-name))
      (set! file-list-lines (append file-list-lines inline-list))
      (set! file-struct-lines (append file-struct-lines inline-struct))

      ;; Phase 4: Report mismatches
      (define has-list? (pair? file-list-lines))
      (define has-struct? (pair? file-struct-lines))

      (when (and has-list? (eq? expected-type 'struct))
        (for ([ln (in-list (sort (remove-duplicates file-list-lines) <))])
          (set! findings
                (cons (list 'list-access rel-path ln func-name expected-type) findings))))

      (when (and has-struct? (eq? expected-type 'list))
        (for ([ln (in-list (sort (remove-duplicates file-struct-lines) <))])
          (set! findings
                (cons (list 'struct-access rel-path ln func-name expected-type) findings))))))

  (reverse findings))

;;; ---------------------------------------------------------------------------
;;; Reporting
;;; ---------------------------------------------------------------------------

(define (report-finding finding)
  (match-define (list access-type rel-path line-num func-name expected) finding)
  (define access-label (if (eq? access-type 'list-access) "list" "struct"))
  (printf "[WARN] ~a:~a uses ~a access for ~a (expected ~a per protocols.rktd)~n"
          rel-path line-num access-label func-name expected))

(define (report-summary count)
  (if (zero? count)
      (displayln "All protocol accesses consistent.")
      (printf "Found ~a protocol inconsistenc~a~n"
              count (if (= count 1) "y" "ies"))))

;;; ---------------------------------------------------------------------------
;;; Main
;;; ---------------------------------------------------------------------------

(define (main)
  (unless (file-exists? protocols-file)
    (displayln "ERROR: scripts/protocols.rktd not found")
    (exit 2))

  (define protocols (load-protocols))
  (when (null? protocols)
    (displayln "No protocols defined in protocols.rktd")
    (exit 0))

  (printf "Checking ~a protocol(s) against ~a~n"
          (length protocols) (path->string project-root))

  (define files (find-rkt-files project-root))
  (printf "Scanning ~a .rkt files~n" (length files))

  (define all-findings '())
  (for ([proto (in-list protocols)])
    (define func-name (car proto))
    (define expected-type (cdr proto))
    (printf "  ~a → expects ~a~n" func-name expected-type)
    (define findings (scan-protocol func-name expected-type files))
    (set! all-findings (append all-findings findings)))

  (newline)
  (for ([f (in-list all-findings)])
    (report-finding f))
  (newline)
  (report-summary (length all-findings))

  (exit (if (null? all-findings) 0 1)))

(main)
