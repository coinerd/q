#lang racket

;; scripts/lint-deprecation-deadlines.rkt — Lint expired version-gated TODOs
;;
;; Scans all .rkt files for TODO(#vX.Y.Z) patterns and checks whether
;; the target version is <= current version (expired).
;;
;; Usage:
;;   racket scripts/lint-deprecation-deadlines.rkt          # Check, print report
;;   racket scripts/lint-deprecation-deadlines.rkt --ci      # Exit 1 on any expired TODO

(require racket/string
         racket/path
         racket/format
         racket/port)

;; --- Version parsing ---

(define (parse-version s)
  ;; "0.25.0" -> (list 0 25 0)
  (map string->number (string-split s ".")))

(define (version<=? a b)
  ;; Compare two version lists lexicographically
  (cond
    [(and (null? a) (null? b)) #t]
    [(null? a) #t]
    [(null? b) #f]
    [(< (car a) (car b)) #t]
    [(> (car a) (car b)) #f]
    [else (version<=? (cdr a) (cdr b))]))

;; --- Read current version from util/version.rkt ---

(define (read-current-version)
  (define version-file (build-path (current-directory) "up" "util" "version.rkt"))
  (define alt-version-file (build-path (current-directory) "util" "version.rkt"))
  (define p
    (cond
      [(file-exists? version-file) version-file]
      [(file-exists? alt-version-file) alt-version-file]
      [else (error 'lint-deprecation-deadlines "Cannot find util/version.rkt")]))
  (define content (file->string p))
  (define m (regexp-match #px"q-version\\s+\"([0-9]+\\.[0-9]+\\.[0-9]+)\"" content))
  (unless m
    (error 'lint-deprecation-deadlines "Cannot parse version from ~a" p))
  (parse-version (cadr m)))

;; --- Scan for TODO patterns ---

(define todo-pattern #px"TODO\\(#v([0-9]+\\.[0-9]+\\.[0-9]+)\\)")

(define (scan-file path current-version)
  (define lines (file->lines path))
  (for/fold ([expired '()])
            ([line (in-list lines)]
             [lineno (in-naturals 1)])
    (define matches (regexp-match* todo-pattern line))
    (for/fold ([acc expired]) ([m (in-list matches)])
      ;; m is like "TODO(#vX.Y.Z)"
      (define ver-match (regexp-match #px"#v([0-9]+\\.[0-9]+\\.[0-9]+)" m))
      (if ver-match
          (let ([target-ver (parse-version (cadr ver-match))])
            (if (version<=? target-ver current-version)
                (cons (list path lineno m (string-trim line)) acc)
                acc))
          acc))))

(define (find-rkt-files)
  (define q-dir (build-path (current-directory)))
  (for/list ([f (in-directory q-dir)]
             #:when (and (file-exists? f)
                         (let ([ext (path-get-extension f)])
                           (and ext
                                (member (string->bytes/utf-8 ".rkt")
                                        (list (string->bytes/utf-8 ".rkt")
                                              (string->bytes/utf-8 ".rktl")))))))
    f))

(define (scan-all-files current-version)
  ;; Find .rkt files, skip compiled dirs
  (define q-dir (build-path (current-directory)))
  (define all-expired
    (for/fold ([expired '()])
              ([f (in-directory q-dir)]
               #:when (and (file-exists? f)
                           (regexp-match? #rx"\\.rktl?$" (path->string f))
                           (not (regexp-match? #rx"/compiled/" (path->string f)))))
      (append (scan-file f current-version) expired)))
  all-expired)

;; --- Main ---

(define ci-mode? (member "--ci" (vector->list (current-command-line-arguments))))

(define current-ver (read-current-version))

(define expired (scan-all-files current-ver))

(cond
  [(null? expired)
   (printf "✓ No expired version-gated TODOs found (current version: ~a)\n"
           (string-join (map number->string current-ver) "."))]
  [else
   (printf "⚠ Found ~a expired version-gated TODO(s):\n\n" (length expired))
   (for ([e (in-list expired)])
     (match-define (list path lineno ver-str line-text) e)
     (printf "  ~a:~a: ~a\n    ~a\n\n"
             path
             lineno
             ver-str
             (substring line-text 0 (min (string-length line-text) 100))))
   (when ci-mode?
     (exit 1))])
