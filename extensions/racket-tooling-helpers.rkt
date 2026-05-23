#lang racket/base

;; extensions/racket-tooling-helpers.rkt — Low-level helpers for Racket tooling
;;
;; Raco commands, file I/O, s-expression parsing, form boundary detection,
;; pattern matching, and template substitution.
;; Split from racket-tooling.rkt (v0.22.6 W3).

(require racket/contract
         racket/port
         racket/string
         racket/file
         racket/list)

;; Raco command helpers
(provide (contract-out [run-raco (->* ((listof any/c)) (any/c) (values any/c any/c any/c))]
                       [find-raco (-> (or/c path? #f))]
                       [run-command (->* (any/c (listof any/c)) (any/c) (values any/c any/c any/c))]
                       [raco-fmt (-> any/c (values any/c any/c any/c))]
                       [raco-make (-> any/c (values any/c any/c any/c))]
                       [raco-test (-> any/c (values any/c any/c any/c))]
                       [raco-expand (-> any/c (values any/c any/c any/c))]
                       ;; File I/O helpers
                       [read-file-string (-> any/c string?)]
                       [write-file-string! (-> any/c any/c any/c)]
                       [backup-file (-> any/c any/c)]
                       [restore-backup! (-> any/c any/c any/c)]
                       ;; S-expression helpers
                       [read-all-forms (-> string? (listof any/c))]
                       [form->string (-> any/c string?)]
                       [find-form-boundaries (-> string? (listof any/c))]
                       [find-form-end
                        (-> (listof string?) exact-nonnegative-integer? (values any/c any/c))]
                       ;; Pattern matching and template substitution
                       [pattern-matches? (-> any/c any/c boolean?)]
                       [apply-template (-> any/c any/c any/c any/c)]
                       [collect-bindings (-> any/c any/c (listof any/c))]
                       [subst-bindings (-> (listof any/c) any/c any/c)]))

;; ============================================================
;; Raco command helpers
;; ============================================================

(define (run-raco args [cwd #f])
  (define raco-bin (find-raco))
  (unless raco-bin
    (error 'run-raco "raco not found on PATH"))
  (run-command raco-bin args cwd))

(define (find-raco)
  (let loop ([dirs (string-split (or (getenv "PATH") "") ":")])
    (cond
      [(null? dirs) #f]
      [else
       (define candidate (build-path (car dirs) "raco"))
       (if (file-exists? candidate)
           candidate
           (loop (cdr dirs)))])))

(define (run-command cmd args [cwd #f])
  (define cmd-path
    (if (string? cmd)
        cmd
        (path->string cmd)))
  (define arg-strings
    (map (lambda (a)
           (if (string? a)
               a
               (format "~a" a)))
         args))
  (parameterize ([current-directory (or cwd (current-directory))])
    (define-values (sp stdout-in stdin-out stderr-in)
      (apply subprocess #f #f #f (find-executable-path cmd-path) arg-strings))
    (define stdout-bytes (port->bytes stdout-in))
    (define stderr-bytes (port->bytes stderr-in))
    (close-input-port stdout-in)
    (close-input-port stderr-in)
    (when (output-port? stdin-out)
      (close-output-port stdin-out))
    (subprocess-wait sp)
    (define exit-code (subprocess-status sp))
    (values exit-code (bytes->string/utf-8 stdout-bytes) (bytes->string/utf-8 stderr-bytes))))

(define (raco-fmt path)
  (run-raco `("fmt" "-i" ,path)))

(define (raco-make path)
  (run-raco `("make" ,path)))

(define (raco-test path)
  (run-raco `("test" ,path)))

(define (raco-expand path)
  (run-raco `("expand" ,path)))

;; ============================================================
;; File I/O helpers
;; ============================================================

(define (read-file-string path)
  (call-with-input-file path port->string))

(define (write-file-string! path content)
  (call-with-output-file path (lambda (out) (display content out)) #:exists 'replace))

(define (backup-file path)
  (define bak (string-append path ".bak"))
  (copy-file path bak #t)
  bak)

(define (restore-backup! bak-path orig-path)
  (when (file-exists? bak-path)
    (copy-file bak-path orig-path #t)
    (delete-file bak-path)))

;; ============================================================
;; S-expression helpers
;; ============================================================

(define (read-all-forms str)
  (define lines (string-split str "\n" #:trim? #f))
  (define stripped
    (string-join (filter (lambda (l) (not (regexp-match? #rx"^#lang" (string-trim l)))) lines) "\n"))
  (define inp (open-input-string stripped))
  (let loop ([forms '()])
    (define form (read inp))
    (if (eof-object? form)
        (reverse forms)
        (loop (cons form forms)))))

(define (form->string form)
  (define out (open-output-string))
  (write form out)
  (get-output-string out))

(define (find-form-boundaries content)
  (define lines (string-split content "\n" #:trim? #f))
  (define inp (open-input-string content))
  (port-count-lines! inp)
  (let loop ([boundaries '()])
    (define start-pos (port-next-location inp))
    (define form (read inp))
    (cond
      [(eof-object? form) (reverse boundaries)]
      [else
       (define end-pos (port-next-location inp))
       (define start-line (and start-pos (car start-pos)))
       (define end-line (and end-pos (car end-pos)))
       (loop (cons (cons (or start-line 1) (or end-line start-line)) boundaries))])))

;; ============================================================
;; Pattern matching and template substitution
;; ============================================================

(define (pattern-matches? pattern source)
  (cond
    [(and (symbol? pattern)
          (let ([s (symbol->string pattern)]) (and (> (string-length s) 2) (string-prefix? s "@@"))))
     #t]
    [(and (symbol? pattern) (symbol? source)) (eq? pattern source)]
    [(and (pair? pattern) (pair? source))
     (and (= (length pattern) (length source)) (andmap pattern-matches? pattern source))]
    [(and (null? pattern) (null? source)) #t]
    [(and (string? pattern) (string? source)) (string=? pattern source)]
    [(and (number? pattern) (number? source)) (= pattern source)]
    [else #f]))

(define (apply-template pattern template source)
  (define bindings (collect-bindings pattern source))
  (subst-bindings bindings template))

(define (collect-bindings pattern source)
  (cond
    [(and (symbol? pattern)
          (let ([s (symbol->string pattern)]) (and (> (string-length s) 2) (string-prefix? s "@@"))))
     (list (cons pattern source))]
    [(and (list? pattern) (list? source)) (apply append (map collect-bindings pattern source))]
    [else '()]))

(define (subst-bindings bindings form)
  (cond
    [(and (symbol? form) (assoc form bindings))
     =>
     cdr]
    [(list? form) (map (lambda (f) (subst-bindings bindings f)) form)]
    [else form]))

;; ============================================================
;; Helper: find form end by paren tracking
;; ============================================================

(define (find-form-end lines start-idx)
  (let loop ([i start-idx]
             [col 0]
             [in-string #f]
             [escape-next #f]
             [cur-depth 0])
    (cond
      [(>= i (length lines)) (values i cur-depth)]
      [else
       (define line (list-ref lines i))
       (define chars (string->list line))
       (let char-loop ([cs (if (= i start-idx)
                               (drop chars col)
                               chars)]
                       [d cur-depth]
                       [in-str in-string]
                       [esc escape-next])
         (cond
           [(null? cs)
            (if (zero? d)
                (values (+ i 1) d)
                (loop (+ i 1) 0 in-str esc d))]
           [else
            (define c (car cs))
            (cond
              [in-str
               (cond
                 [esc (char-loop (cdr cs) d in-str #f)]
                 [(char=? c #\\) (char-loop (cdr cs) d in-str #t)]
                 [(char=? c #\") (char-loop (cdr cs) d #f #f)]
                 [else (char-loop (cdr cs) d in-str #f)])]
              [else
               (cond
                 [(char=? c #\;)
                  (if (zero? d)
                      (values (+ i 1) 0)
                      (loop (+ i 1) 0 #f #f d))]
                 [(char=? c #\") (char-loop (cdr cs) d #t #f)]
                 [(char=? c #\() (char-loop (cdr cs) (+ d 1) #f #f)]
                 [(char=? c #\))
                  (define new-d (- d 1))
                  (if (and (= i start-idx) (= new-d 0))
                      (values (+ i 1) 0)
                      (char-loop (cdr cs) new-d #f #f))]
                 [(char=? c #\[) (char-loop (cdr cs) (+ d 1) #f #f)]
                 [(char=? c #\])
                  (define new-d (- d 1))
                  (if (and (= i start-idx) (= new-d 0))
                      (values (+ i 1) 0)
                      (char-loop (cdr cs) new-d #f #f))]
                 [else (char-loop (cdr cs) d #f #f)])])]))])))
