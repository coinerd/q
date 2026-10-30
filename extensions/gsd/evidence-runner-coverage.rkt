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
(provide regression-command-covered?
         regression-command-coverage-refusal)
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
;; Reasoned outcome of the SAME pure coverage authentication: #f means the
;; focused runner stage is covered; otherwise an immutable refusal naming a
;; stable code, the failing phase, the first offending installed path when
;; known, and bounded detail. The accepted grammar is unchanged; only the
;; previously boolean refusal becomes nameable (W0: the live first failure
;; was an installed scripts/run-tests/inventory.rkt drift).
(define (coverage-refusal code phase path detail)
  (hasheq 'admitted? #f 'code code 'phase phase 'path path 'detail detail))
(define (regression-command-coverage-refusal repo head command test-path)
  (with-handlers ([exn:fail? (lambda (_)
                               (coverage-refusal
                                "runner-closure-unavailable"
                                "runner-closure"
                                #f
                                "installed trusted runner dependency could not be authenticated"))])
    (let/ec refuse
      (define (refusal code phase path detail)
        (refuse (coverage-refusal code phase path detail)))
      (define stages (owned-chain-stages command))
      (unless stages
        (refusal "unapproved-normalized-command"
                 "command"
                 #f
                 "normalized command has no owned verify stages"))
      (define focused (filter (lambda (s) (member test-path (string-split s))) stages))
      (unless (= (length focused) 1)
        (refusal "test-not-focused"
                 "command"
                 test-path
                 "the probe module must appear in exactly one focused runner stage"))
      (define tokens (string-split (car focused)))
      (cond
        [(equal? tokens (list "raco" "test" test-path)) #f]
        [(and (>= (length tokens) 3) (equal? (take tokens 2) '("racket" "scripts/run-tests.rkt")))
         (define bytes (git-bytes repo "show" (string-append head ":scripts/run-tests.rkt")))
         (unless (and bytes (regular-blob? repo head "scripts/run-tests.rkt"))
           (refusal "runner-not-regular-blob"
                    "runner-closure"
                    "scripts/run-tests.rkt"
                    "candidate head runner is absent or not a regular blob"))
         (cond
           [(and (bytes=? bytes (string->bytes/utf-8 mini)) (equal? (drop tokens 2) (list test-path)))
            #f]
           [(and (bytes=? bytes (string->bytes/utf-8 fixed-mini))
                 (equal? test-path "tests/probe.rkt")
                 (equal? (drop tokens 2) (list test-path)))
            #f]
           [else
            (let ()
              (define trusted (force closure))
              (define mismatch
                (for/or ([path (in-list (sort (hash-keys trusted) string<?))])
                  (define actual (git-bytes repo "show" (string-append head ":" path)))
                  (and (not (and actual
                                 (regular-blob? repo head path)
                                 (bytes=? actual (hash-ref trusted path))))
                       path)))
              (and mismatch
                   (coverage-refusal
                    "runner-closure-mismatch"
                    "runner-closure"
                    mismatch
                    "installed trusted runner file differs from the candidate head blob")))])]
        [else
         (refusal "unapproved-runner"
                  "command"
                  #f
                  "focused stage is not an owned raco test or run-tests runner invocation")]))))
(define (regression-command-covered? repo head command test-path)
  (not (regression-command-coverage-refusal repo head command test-path)))
