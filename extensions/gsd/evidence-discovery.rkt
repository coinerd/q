#lang racket/base
;; Static, fail-closed discovery. The caller owns checkout/context freshness.
(require racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         racket/system
         "review-proof-format.rkt"
         "evidence-repair-format.rkt"
         "evidence-runner-coverage.rkt"
         "../../util/json/checksum.rkt"
         "../../scripts/gsd-evidence-probe-runner.rkt")
(provide discover-evidence-probe-spec)
(define checks '(check-equal? check-eq? check-eqv? check-true check-false check-not-false check-=))
(define (discover-evidence-probe-spec repo context normalized-command)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (let/ec reject
      (define (git . args)
        (define out (open-output-string))
        (and (parameterize ([current-directory repo]
                            [current-output-port out]
                            [current-error-port (open-output-nowhere)])
               (apply system* (find-executable-path "git") args))
             (string-trim (get-output-string out))))
      (unless (and (valid-evidence-context? context)
                   (equal? (git "status" "--porcelain" "--untracked-files=all") ""))
        (reject #f))
      (define head (hash-ref context 'head))
      (define stages (owned-chain-stages normalized-command))
      (unless stages
        (reject #f))
      (define paths
        (append-map (lambda (stage)
                      (define tokens (string-split stage))
                      (if (and (>= (length tokens) 3)
                               (or (equal? (take tokens 2) '("racket" "scripts/run-tests.rkt"))
                                   (equal? (take tokens 2) '("raco" "test")))
                               (andmap (lambda (p)
                                         (and (regexp-match? #px"^tests/[A-Za-z0-9_/-]+[.]rkt$" p)
                                              (not (member ".." (string-split p "/")))))
                                       (drop tokens 2)))
                          (drop tokens 2)
                          '()))
                    stages))
      (unless (pair? paths)
        (reject #f))
      (define (production-difference? base)
        (define diff (git "diff" "--name-only" base head "--"))
        (and diff
             (for/or ([p (in-list (string-split diff "\n"))])
               (and (string-suffix? p ".rkt") (not (string-prefix? p "tests/"))))))
      (define (eligible? base)
        (and base
             (regexp-match? #px"^[0-9a-f]{40}$" base)
             (not (equal? base head))
             (git "merge-base" "--is-ancestor" base head)
             (production-difference? base)))
      (define baseline
        (or (for/or ([ref '("origin/main" "main")])
              (define b (git "merge-base" head ref))
              (and (eligible? b) b))
            (for/or ([n (in-range 1 21)])
              (define b (git "rev-parse" "--verify" (format "~a~~~a" head n)))
              (and (eligible? b) b))))
      (unless baseline
        (reject #f))
      (define (candidate p)
        (with-handlers ([exn:fail? (lambda (_) #f)])
          (let/ec fail
            (unless (regression-command-covered? repo head normalized-command p)
              (fail #f))
            (define file (build-path repo p))
            (unless (and (file-exists? file)
                         (not (link-exists? file))
                         (for/and ([part (in-list (drop-right (string-split p "/") 1))]
                                   [n (in-naturals 1)])
                           (not (link-exists?
                                 (apply build-path repo (take (string-split p "/") n))))))
              (fail #f))
            (validate-regression-module! file #:bindings? #f)
            (define entry (git "ls-tree" head "--" p))
            (unless (and entry (regexp-match? #px"^100644 blob [0-9a-f]{40}\t" entry))
              (fail #f))
            (define forms (read-regression-forms file))
            (unless (ormap (lambda (s) (equal? (syntax->datum s) '(provide evidence-suite))) forms)
              (fail #f))
            (define defs
              (filter (lambda (s)
                        (define xs (syntax->list s))
                        (and xs
                             (= (length xs) 3)
                             (eq? (syntax-e (car xs)) 'define)
                             (eq? (syntax-e (cadr xs)) 'evidence-suite)))
                      forms))
            (unless (= (length defs) 1)
              (fail #f))
            (define targets '())
            (define names '())
            (define sites '())
            (define (suite s trail)
              (define xs (syntax->list s))
              (unless (and xs
                           (>= (length xs) 2)
                           (eq? (syntax-e (car xs)) 'test-suite)
                           (string? (syntax-e (cadr xs))))
                (fail #f))
              (define path (append trail (list (syntax-e (cadr xs)))))
              (for ([child (in-list (cddr xs))])
                (define cs (syntax->list child))
                (unless (and cs (pair? cs))
                  (fail #f))
                (if (eq? (syntax-e (car cs)) 'test-suite)
                    (suite child path)
                    (let ()
                      (unless (and (= (length cs) 3)
                                   (eq? (syntax-e (car cs)) 'test-case)
                                   (string? (syntax-e (cadr cs))))
                        (fail #f))
                      (define name (syntax-e (cadr cs)))
                      (when (member name names)
                        (fail #f))
                      (set! names (cons name names))
                      (define check (caddr cs))
                      (define parts (syntax->list check))
                      (unless (and parts (pair? parts) (memq (syntax-e (car parts)) checks))
                        (fail #f))
                      (set! sites (cons (syntax-position check) sites))
                      (set! targets
                            (append targets
                                    (list (hasheq 'test-path
                                                  p
                                                  'suite-path
                                                  path
                                                  'test-case-name
                                                  name
                                                  'check-name
                                                  (symbol->string (syntax-e (car parts)))
                                                  'check-location
                                                  (list (path->string (file-name-from-path file))
                                                        (syntax-line check)
                                                        (syntax-column check)
                                                        (syntax-position check)
                                                        (syntax-span check))))))))))
            (suite (caddr (syntax->list (car defs))) '())
            (define all '())
            (define (walk s)
              (define xs (syntax->list s))
              (when xs
                (when (and (pair? xs)
                           (symbol? (syntax-e (car xs)))
                           (regexp-match? #rx"^check-" (symbol->string (syntax-e (car xs)))))
                  (set! all (cons (syntax-position s) all)))
                (for-each walk xs)))
            (for-each walk forms)
            (unless (and (pair? targets) (equal? (sort sites <) (sort all <)))
              (fail #f))
            (hasheq 'kind
                    "probe-spec/v1"
                    'context
                    context
                    'baseline-head
                    baseline
                    'baseline-tree
                    (git "rev-parse" (string-append baseline "^{tree}"))
                    'focused-normalized-command
                    (findf (lambda (s) (member p (string-split s))) stages)
                    'test-paths
                    (list p)
                    'test-files
                    (list (hasheq 'path p 'sha256 (sha256-file file)))
                    'harness-files
                    '()
                    'suite-export
                    "evidence-suite"
                    'target
                    (car targets)
                    'runtime
                    (hasheq 'racket-version (version) 'platform (symbol->string (system-type 'os)))
                    'timeout-sec
                    30))))
      (for/or ([p (in-list (remove-duplicates paths))])
        (candidate p)))))
