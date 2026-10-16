#lang racket/base
;; Compare worker runner dependencies with installed owner code, never JSON.
(require racket/file
         racket/list
         racket/path
         racket/port
         racket/promise
         racket/runtime-path
         racket/string
         racket/system
         "review-proof-format.rkt")
(provide regression-command-covered?)
(define-runtime-path installed "../..")
(define mini
  "#lang racket/base\n(for ([test (in-vector (current-command-line-arguments))])\n  (dynamic-require `(submod ,(string->path test) test) #f))\n")
(define fixed-mini
  "#lang racket/base\n(require racket/runtime-path)\n(define-runtime-path test \"../tests/probe.rkt\")\n(dynamic-require `(submod ,test test) #f)\n")
(define (git-bytes repo . args)
  (define out (open-output-bytes))
  (and (parameterize ([current-output-port out]
                      [current-error-port (open-output-nowhere)])
         (apply system* (find-executable-path "git") "-C" repo args))
       (get-output-bytes out)))
;; Parse only installed syntax: candidate readers/macros never run here.
(define (trusted-closure)
  (define seen (make-hash))
  (define (visit path)
    (define full (simplify-path (path->complete-path path installed)))
    (define relative (path->string (find-relative-path (simplify-path installed) full)))
    (unless (and (not (string-prefix? relative "..")) (file-exists? full) (not (link-exists? full)))
      (error 'evidence-runner-coverage "unapproved installed dependency"))
    (unless (hash-ref seen relative #f)
      (hash-set! seen relative (file->bytes full))
      (define stx
        (parameterize ([read-accept-reader #t])
          (call-with-input-file full (lambda (in) (read-syntax full in)))))
      (define (dependency s)
        (cond
          [(string? s)
           (when (string-suffix? s ".rkt")
             (visit (build-path (path-only full) s)))]
          [(pair? s) (for-each dependency s)]))
      (define (walk s)
        (when (and (pair? s) (list? s))
          (when (eq? (car s) 'require)
            (for-each dependency (cdr s)))
          (when (and (eq? (car s) 'define-runtime-path)
                     (= (length s) 3)
                     (string? (caddr s))
                     (string-suffix? (caddr s) ".rkt"))
            (dependency (caddr s)))
          (for-each walk s)))
      (walk (syntax->datum stx))))
  (visit (build-path installed "scripts/run-tests.rkt"))
  seen)
(define closure
  (delay
    (trusted-closure)))
(define (regular-blob? repo head path)
  (define entry (git-bytes repo "ls-tree" head "--" path))
  (and entry (regexp-match? #rx#"^100644 blob " entry)))
(define (regression-command-covered? repo head command test-path)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define stages (owned-chain-stages command))
    (and
     stages
     (let ([focused (filter (lambda (s) (member test-path (string-split s))) stages)])
       (and (= (length focused) 1)
            (let ([tokens (string-split (car focused))])
              (cond
                [(equal? tokens (list "raco" "test" test-path)) #t]
                [(and (>= (length tokens) 3)
                      (equal? (take tokens 2) '("racket" "scripts/run-tests.rkt")))
                 (define bytes (git-bytes repo "show" (string-append head ":scripts/run-tests.rkt")))
                 (and bytes
                      (regular-blob? repo head "scripts/run-tests.rkt")
                      (or (and (bytes=? bytes (string->bytes/utf-8 mini))
                               (equal? (drop tokens 2) (list test-path)))
                          (and (bytes=? bytes (string->bytes/utf-8 fixed-mini))
                               (equal? test-path "tests/probe.rkt")
                               (equal? (drop tokens 2) (list test-path)))
                          (for/and ([(path expected) (in-hash (force closure))])
                            (define actual (git-bytes repo "show" (string-append head ":" path)))
                            (and actual (regular-blob? repo head path) (bytes=? actual expected)))))]
                [else #f])))))))
