#lang racket

;; q/scripts/contract-metrics.rkt — Count any/c in contract forms across source tree
;;
;; Usage: racket q/scripts/contract-metrics.rkt [--json] [--summary]
;;
;; Uses the Racket reader to parse files into S-expressions, then walks
;; the syntax tree to count `any/c` ONLY inside actual contract forms
;; (contract-out, define/contract, ->, ->*, etc.), not in comments or strings.

(require racket/string
         racket/file
         racket/port)

;; ── File discovery ──────────────────────────────────────────

(define src-dirs
  '("extensions" "agent"
                 "runtime"
                 "tui"
                 "tools"
                 "util"
                 "interfaces"
                 "cli"
                 "llm"
                 "sandbox"
                 "skills"
                 "wiring"))

(define (under-src-dir? p)
  (define str (path->string p))
  (define clean
    (if (string-prefix? str "./")
        (substring str 2)
        str))
  (ormap (lambda (d) (string-prefix? clean (string-append d "/"))) src-dirs))

(define (find-src-files [root "."])
  (for/list ([f (in-directory root)]
             #:when (and (path-has-extension? f #".rkt") (under-src-dir? f)))
    (path->string f)))

;; ── Reader-based parsing ────────────────────────────────────

;; Strip #lang line(s) from content before reading, since the reader
;; cannot handle #lang directives.
(define (strip-lang-lines content)
  (define lines (string-split content "\n" #:trim? #f))
  (string-join (for/list ([line (in-list lines)]
                          #:unless (regexp-match? #rx"^#lang" line))
                 line)
               "\n"))

;; Read all top-level forms from a string, returning list of datums.
(define (read-all-forms content)
  (with-handlers ([exn:fail:read? (lambda (_) '())])
    (define in (open-input-string (strip-lang-lines content)))
    (port-count-lines! in)
    (let loop ([acc '()])
      (define form (read in))
      (if (eof-object? form)
          (reverse acc)
          (loop (cons form acc))))))

;; ── Contract form detection ─────────────────────────────────

;; Symbols that introduce contract forms where any/c should be counted.
(define contract-heads '(contract-out define/contract define/contract* -> ->* ->i ->d case-> proc->))

(define (contract-head? d)
  (and (symbol? d) (memq d contract-heads) #t))

;; ── Counting any/c in contract forms ────────────────────────

;; Count any/c symbols in a datum tree when already inside a contract context.
(define (count-anyc-raw tree)
  (cond
    [(eq? tree 'any/c) 1]
    [(and (pair? tree) (list? tree)) (for/sum ([sub (in-list tree)]) (count-anyc-raw sub))]
    [else 0]))

;; Process a contract-out body: each child is (id contract-expr ...)
;; where the contract expressions (everything after id) contain any/c.
(define (count-anyc-in-contract-out-entries entries)
  (for/sum ([entry (in-list entries)])
           (cond
             [(and (list? entry) (>= (length entry) 2))
              ;; Skip the first element (identifier), count any/c in rest
              (for/sum ([sub (in-list (cdr entry))]) (count-anyc-raw sub))]
             ;; Single element — probably a rename or struct-out, skip
             [(and (list? entry) (= (length entry) 1)) 0]
             [else 0])))

;; Walk a top-level form, counting any/c inside contract contexts.
(define (count-anyc-in-form form)
  (cond
    [(not (list? form)) 0]
    [(null? form) 0]
    ;; (provide (contract-out ...)) or (provide/contract-out ...)
    [(eq? (car form) 'provide)
     (for/sum ([sub (in-list (cdr form))])
              (cond
                [(and (list? sub) (not (null? sub)) (eq? (car sub) 'contract-out))
                 (count-anyc-in-contract-out-entries (cdr sub))]
                [else 0]))]
    ;; (define/contract (name args) contract body ...)
    ;; The third element (index 2) is the contract expression
    [(and (eq? (car form) 'define/contract) (>= (length form) 3)) (count-anyc-raw (list-ref form 2))]
    ;; Other top-level forms with contract heads
    [(contract-head? (car form)) (count-anyc-raw form)]
    [else 0]))

;; ── Public API ──────────────────────────────────────────────

;; Count any/c in contract forms within file content string.
(define (count-anyc-in-file content)
  (define forms (read-all-forms content))
  (for/sum ([form (in-list forms)]) (count-anyc-in-form form)))

;; Helper: count any/c in a contract-form string.
(define (anyc-in-contract-form? str)
  (with-handlers ([exn:fail? (lambda (_) 0)])
    (define form (read (open-input-string str)))
    (if (and (pair? form) (contract-head? (car form)))
        (count-anyc-raw form)
        0)))

;; ── Main ────────────────────────────────────────────────────

(define (run-metrics #:root [root "."] #:json? [json? #f] #:summary? [summary? #f])
  (define files (find-src-files root))
  (define results
    (sort (for/list ([f (in-list files)])
            (define content
              (with-handlers ([exn:fail? (lambda (_) "")])
                (file->string f)))
            (define n (count-anyc-in-file content))
            (cons f n))
          >
          #:key cdr))
  (define total (apply + (map cdr results)))
  (define files-with-anyc (filter (lambda (p) (> (cdr p) 0)) results))

  (cond
    [json? (displayln (format "{ \"total\": ~a, \"files\": ~a }" total (length files-with-anyc)))]
    [summary?
     (printf "Total any/c in contract-out: ~a~n" total)
     (printf "Files with any/c: ~a~n" (length files-with-anyc))
     (printf "Total source files scanned: ~a~n" (length files))]
    [else
     (printf "~n── Contract Metrics: any/c in contract forms ──~n~n")
     (printf "Total: ~a any/c across ~a files~n~n" total (length files-with-anyc))
     (for ([p (in-list files-with-anyc)])
       (printf "  ~a  ~a~n" (cdr p) (car p)))
     (printf "~n── Total: ~a any/c ──~n" total)])

  total)

;; ── CLI ─────────────────────────────────────────────────────

(module+ main
  (define json-mode #f)
  (define summary-mode #f)

  (command-line #:program "contract-metrics"
                #:once-each [("--json") "Output JSON" (set! json-mode #t)]
                [("--summary") "Summary only" (set! summary-mode #t)]
                #:args ()
                (run-metrics #:root "." #:json? json-mode #:summary? summary-mode)))

;; ── Testable exports ────────────────────────────────────────

(provide count-anyc-in-file
         anyc-in-contract-form?
         run-metrics)
