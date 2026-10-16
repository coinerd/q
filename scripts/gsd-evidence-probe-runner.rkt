#lang racket/base
;; Adapter output is telemetry, never registration authority. The owner must
;; capture it outside the checkout and authenticate all inputs and job facts.
(require json
         racket/list
         racket/port
         compiler/cm
         rackunit/private/base
         rackunit/private/result
         rackunit/private/check-info
         racket/path
         racket/file)
(provide collect-evidence-suite
         validate-regression-module!
         read-regression-forms)
;; Consume only the two fixed built-in language headers. Disallow nested
;; #reader/#lang and custom readtables BEFORE parsing any candidate datum.
(define (read-regression-forms path)
  (call-with-input-file path
                        (lambda (in)
                          (port-count-lines! in)
                          (unless (member (read-line in) '("#lang racket/base" "#lang racket"))
                            (error 'evidence-suite "unsupported regression language"))
                          (parameterize ([read-accept-reader #f]
                                         [read-accept-lang #f]
                                         [read-accept-compiled #f]
                                         [current-readtable #f])
                            (let loop ()
                              (define form (read-syntax path in))
                              (if (eof-object? form)
                                  '()
                                  (cons form (loop))))))))
;; Deliberately small grammar: imported production bindings must be variables,
;; and expressions cannot invoke macros or override core/RackUnit operators.
(define (validate-regression-module! path #:bindings? [bindings? #t])
  (define (refuse)
    (error 'evidence-suite "unsupported regression module"))
  (define forms (map syntax->datum (read-regression-forms path)))
  (define imported '())
  (define ns (make-base-namespace))
  (define (reserved? name)
    (or (memq name '(evidence-suite test-suite test-case run-tests))
        (regexp-match? #rx"^(check-|#%)" (symbol->string name))
        (parameterize ([current-namespace ns])
          (namespace-variable-value name #t (lambda () #f)))
        (parameterize ([current-namespace ns])
          (identifier-binding (namespace-symbol->identifier name)))))
  (define requires 0)
  (define provides 0)
  (define definitions '())
  (define tests 0)
  (for ([form (in-list forms)])
    (cond
      [(and (list? form) (pair? form) (eq? (car form) 'require))
       (for ([mod (in-list (cdr form))])
         (cond
           [(eq? mod 'rackunit) (set! requires (add1 requires))]
           [(and (string? mod) (relative-path? mod) (regexp-match? #rx"[.]rkt$" mod))
            (define file (simplify-path (build-path (path-only path) mod)))
            ;; Compilation and binding authentication happen only inside the
            ;; bounded owned probe process, never controller discovery.
            (when bindings?
              (define code
                (parameterize ([current-namespace (make-base-namespace)]
                               [current-load-relative-directory (path-only file)]
                               [read-accept-reader #t])
                  (compile (call-with-input-file file (lambda (in) (read-syntax file in))))))
              (define-values (vars macros) (module-compiled-exports code))
              (unless (null? macros)
                (refuse))
              (for ([phase (in-list vars)])
                (unless (equal? (car phase) 0)
                  (refuse))
                (for ([entry (in-list (cdr phase))])
                  (define name (car entry))
                  (when (or (reserved? name) (memq name imported))
                    (refuse))
                  (set! imported (cons name imported)))))]
           [else (refuse)]))]
      [(equal? form '(provide evidence-suite)) (set! provides (add1 provides))]
      [(and (list? form) (= (length form) 3) (equal? (take form 2) '(define evidence-suite)))
       (set! definitions (cons (caddr form) definitions))]
      [(equal? form
               '(module+ test
                  (require rackunit/text-ui)
                  (exit (run-tests evidence-suite))))
       (set! tests (add1 tests))]
      [else (refuse)]))
  (unless (and (= requires 1) (= provides 1) (= tests 1) (= (length definitions) 1))
    (refuse))
  (define (expr? e)
    (or (number? e)
        (string? e)
        (boolean? e)
        (char? e)
        (and (symbol? e) (or (not bindings?) (memq e imported)))
        (and (list? e)
             (pair? e)
             (or (and (not bindings?) (symbol? (car e))) (memq (car e) imported))
             (andmap expr? (cdr e)))))
  (define (suite? s)
    (and
     (list? s)
     (>= (length s) 3)
     (eq? (car s) 'test-suite)
     (string? (cadr s))
     (for/and ([child (in-list (cddr s))])
       (or
        (suite? child)
        (and
         (list? child)
         (= (length child) 3)
         (eq? (car child) 'test-case)
         (string? (cadr child))
         (let ([check (caddr child)])
           (and
            (list? check)
            (pair? check)
            (memq (car check)
                  '(check-equal? check-eq? check-eqv? check-true check-false check-not-false check-=))
            (= (length (cdr check))
               (case (car check)
                 [(check-true check-false check-not-false) 1]
                 [(check-=) 3]
                 [else 2]))
            (andmap expr? (cdr check)))))))))
  (unless (suite? (car definitions))
    (refuse))
  (void))
;; RackUnit test-begin deliberately overrides current-check-around. Restrict
;; accepted cases to a single unconditional literal check; derive success
;; identity from the authenticated syntax and confirm RED against exception info.
(define (literal-checks path)
  (define forms (read-regression-forms path))
  (define found (make-hash))
  (define check-sites '())
  (define all-check-sites '())
  (define (walk s)
    (define xs (syntax->list s))
    (when xs
      (when (and (pair? xs)
                 (symbol? (syntax-e (car xs)))
                 (regexp-match? #rx"^check-" (symbol->string (syntax-e (car xs)))))
        (set! all-check-sites (cons (syntax-position s) all-check-sites)))
      (when (and (>= (length xs) 2) (eq? (syntax-e (car xs)) 'test-case))
        (define name (syntax-e (cadr xs)))
        (unless (and (string? name) (= (length xs) 3))
          (error 'evidence-suite "case must contain one unconditional check"))
        (define check (caddr xs))
        (define parts (syntax->list check))
        (unless (and parts
                     (memq (syntax-e (car parts))
                           '(check-equal? check-eq?
                                          check-eqv?
                                          check-true
                                          check-false
                                          check-not-false
                                          check-=)))
          (error 'evidence-suite "unsupported literal check"))
        (when (hash-has-key? found name)
          (error 'evidence-suite "duplicate case name"))
        (set! check-sites (cons (syntax-position check) check-sites))
        (hash-set! found
                   name
                   (list (hasheq 'name
                                 (symbol->string (syntax-e (car parts)))
                                 'location
                                 (list (path->string (file-name-from-path path))
                                       (syntax-line check)
                                       (syntax-column check)
                                       (syntax-position check)
                                       (syntax-span check))))))
      (for-each walk xs)))
  (for-each walk forms)
  (unless (equal? (sort check-sites <) (sort all-check-sites <))
    (error 'evidence-suite "hidden or nested check form unsupported"))
  found)
;; The reporting thread must survive target aborts, self-kill and custodian
;; shutdown. This is protocol supervision, NOT a sandbox for hostile project
;; code, native code or same-user filesystem/memory inspection.
(define (collect-evidence-suite path)
  (define cache (make-temporary-file "probe-compiled-~a" 'directory))
  (define custodian (make-custodian))
  (define results (make-channel))
  (dynamic-wind void
                (lambda ()
                  (define worker
                    (parameterize ([current-custodian custodian])
                      (thread (lambda ()
                                (channel-put results (collect-evidence-suite/worker path cache))))))
                  (define result (sync results (thread-dead-evt worker)))
                  (unless (hash? result)
                    (error 'evidence-suite "target worker terminated without a completed report"))
                  result)
                (lambda ()
                  (custodian-shutdown-all custodian)
                  (delete-directory/files cache))))
(define (collect-evidence-suite/worker path cache)
  (define compiled? #f)
  (define loaded? #f)
  (define completed? #f)
  (define cases '())
  (define diagnostic #f)
  (with-handlers ([(lambda (_) #t) (lambda (e)
                                     (set! diagnostic
                                           (if (exn? e)
                                               (exn-message e)
                                               (format "~s" e))))])
    (parameterize ([current-output-port (open-output-nowhere)]
                   [current-error-port (open-output-nowhere)]
                   [exit-handler (lambda (_) (error 'evidence-suite "target attempted process exit"))]
                   [current-compiled-file-roots (list cache)])
      (validate-regression-module! path)
      (managed-compile-zo path)
      (set! compiled? #t)
      (define literals (literal-checks path))
      (define suite (dynamic-require path 'evidence-suite))
      (set! loaded? #t)
      (unless (rackunit-test-suite? suite)
        (error 'evidence-suite "unsupported exported suite"))
      (define suite-path '())
      (fold-test-results
       (lambda (result seed)
         (define failure? (test-failure? result))
         (define failure-check
           (if failure?
               (let* ([stack (exn:test:check-stack (test-failure-result result))]
                      [loc (findf (lambda (i) (eq? (check-info-name i) 'location)) stack)]
                      [name (findf (lambda (i) (eq? (check-info-name i) 'name)) stack)]
                      [datum (and loc (location-info-value (check-info-value loc)))])
                 (hasheq 'name
                         (and name (symbol->string (check-info-value name)))
                         'location
                         (and datum (cons (path->string (file-name-from-path path)) (cdr datum)))))
               'null))
         (define info
           (if failure?
               (for/list ([i (in-list (exn:test:check-stack (test-failure-result result)))])
                 (hasheq 'name
                         (symbol->string (check-info-name i))
                         'value
                         (format "~s" (check-info-value i))))
               '()))
         (set! cases
               (cons (hasheq 'test-case-name
                             (test-result-test-case-name result)
                             'suite-path
                             suite-path
                             'status
                             (cond
                               [failure? "failure"]
                               [(test-error? result) "error"]
                               [else "success"])
                             'failure-check
                             failure-check
                             'check-count
                             (length (hash-ref literals (test-result-test-case-name result) '()))
                             'checks
                             (hash-ref literals (test-result-test-case-name result) '())
                             'check-info
                             info)
                     cases))
         seed)
       #f
       suite
       #:fdown (lambda (name seed)
                 (set! suite-path (append suite-path (list name)))
                 seed)
       #:fup (lambda (_ seed)
               (set! suite-path (drop-right suite-path 1))
               seed))
      (unless (= (hash-count literals) (length cases))
        (error 'evidence-suite "static cases differ from exported suite"))
      (set! completed? #t)))
  (define ordered (reverse cases))
  (define (count-status s)
    (count (lambda (c) (equal? s (hash-ref c 'status))) ordered))
  (hasheq 'kind
          "structured-probe-result/v1"
          'compile-ok
          compiled?
          'load-ok
          loaded?
          'suite-completed
          completed?
          'successes
          (count-status "success")
          'failures
          (count-status "failure")
          'errors
          (count-status "error")
          'cases
          ordered
          'diagnostic
          (or diagnostic 'null)))
(module+ main
  (unless (= (vector-length (current-command-line-arguments)) 1)
    (error 'evidence-probe-runner "expected exactly one absolute module path"))
  (define path (string->path (vector-ref (current-command-line-arguments) 0)))
  (unless (absolute-path? path)
    (error 'evidence-probe-runner "absolute path required"))
  (write-json (collect-evidence-suite path))
  (newline))
