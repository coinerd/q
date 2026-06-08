#lang racket/base

;; Classification of test files into suites (slow, fast, tui, security, etc.)
;; Metadata parsing from file headers (@suite, @speed, @mutates tags).

(require racket/string
         racket/path
         racket/file)

(provide get-file-metadata
         clear-metadata-cache!
         slow-file?
         tui-file?
         mutating-file?
         mutating-patterns
         security-file?
         file-has-suite-tag?
         smoke-excluded?
         support-test-module?
         arch-file?
         runtime-file?
         extensions-file?
         workflows-file?
         collect-test-files
         normalize-test-path
         resolve-base-dir
         q-root-candidate?
         base-dir)

;; Metadata parser integration (v0.83.4)
(define metadata-cache (make-hash))

(define (clear-metadata-cache!)
  (hash-clear! metadata-cache))

(define (get-file-metadata f)
  "Parse metadata from file header, cached per file."
  (hash-ref!
   metadata-cache
   f
   (lambda ()
     (define full-path
       (if (absolute-path? f)
           f
           (build-path base-dir f)))
     (if (file-exists? full-path)
         (let ([speed #f]
               [suite #f]
               [mutates #f]
               [boundary #f]
               [isolation #f]
               [timeout #f])
           (with-handlers ([exn:fail? (lambda (_) (void))])
             (call-with-input-file
              full-path
              (lambda (port)
                (for ([_ (in-range 30)]
                      #:break (eof-object? (peek-byte port)))
                  (define line (read-line port))
                  (when (string? line)
                    (cond
                      [(regexp-match? #rx";+[ \t]*@speed[ \t]+fast" line) (set! speed 'fast)]
                      [(regexp-match? #rx";+[ \t]*@speed[ \t]+slow" line) (set! speed 'slow)]
                      [(regexp-match? #rx";+[ \t]*@speed[ \t]+perf" line) (set! speed 'perf)]
                      [(regexp-match #rx";+[ \t]*@suite[ \t]+(.+)$" line)
                       =>
                       (lambda (m) (set! suite (string-trim (cadr m))))]
                      [(regexp-match #rx";+[ \t]*@mutates[ \t]+(.+)$" line)
                       =>
                       (lambda (m) (set! mutates (string-trim (cadr m))))]
                      [(regexp-match #rx";+[ \t]*@boundary[ \t]+(.+)$" line)
                       =>
                       (lambda (m) (set! boundary (string-trim (cadr m))))]
                      [(regexp-match #rx";+[ \t]*@isolation[ \t]+(.+)$" line)
                       =>
                       (lambda (m) (set! isolation (string-trim (cadr m))))]
                      [(regexp-match #rx";+[ \t]*@timeout[ \t]+([0-9]+)" line)
                       =>
                       (lambda (m) (set! timeout (string->number (cadr m))))]))))))
           (hash 'speed
                 speed
                 'suite
                 suite
                 'mutates
                 mutates
                 'boundary
                 boundary
                 'isolation
                 isolation
                 'timeout
                 timeout))
         (hash)))))

(define slow-patterns
  '("sandbox" "subprocess"
              "integration"
              "benchmark"
              "workflow-"
              "e2e-"
              "ci_local"
              "metrics-readme"
              "bump-version"
              "examples-compile"
              "pre-commit"
              "racket-tooling"
              "run-tests"
              "audit-script"
              "test-doctor"
              "check-deps"
              "self-hosting"
              "tui-terminal"
              "sync-readme"))

(define path-slow-patterns '("/workflows/"))

(define (slow-file? f)
  (define meta (get-file-metadata f))
  (define meta-speed (hash-ref meta 'speed #f))
  (cond
    [(eq? meta-speed 'fast) #f]
    [(eq? meta-speed 'slow) #t]
    [(eq? meta-speed 'perf) #t]
    [else
     (define base (file-name-from-path f))
     (or (for/or ([p (in-list slow-patterns)])
           (and base (string-contains? (path->string base) p)))
         (for/or ([p (in-list path-slow-patterns)])
           (string-contains? f p)))]))

(define (tui-file? f)
  (define meta (get-file-metadata f))
  (define meta-suite (hash-ref meta 'suite #f))
  (if (equal? meta-suite "tui")
      #t
      (let ([base (file-name-from-path f)])
        (or (string-contains? f "/tui/")
            (string-contains? f "/interfaces/tui.rkt")
            (and base (string-prefix? (path->string base) "test-tui-"))))))

(define mutating-patterns
  '("ci_local" "pre-commit"
               "check-deps"
               "sync-version"
               "sync-readme"
               "bump-version"
               "metrics-readme"
               "self-hosting"))

(define (mutating-file? f)
  (define meta (get-file-metadata f))
  (define meta-mutates (hash-ref meta 'mutates #f))
  (define meta-isolation (hash-ref meta 'isolation #f))
  (cond
    [(equal? meta-mutates "none") #f]
    [(equal? meta-isolation "process") #t]
    [else
     (let ([base (file-name-from-path f)])
       (for/or ([p (in-list mutating-patterns)])
         (and base (string-contains? (path->string base) p))))]))

(define (security-file? f)
  (define base (path->string (file-name-from-path f)))
  (or (string-contains? base "security")
      (string-contains? base "permission")
      (string-contains? base "safe-mode")
      (string-contains? base "sandbox")
      (string-contains? base "tool-bash")
      ;; Tag-based: files with ;; @suite security in first 10 lines
      (file-has-suite-tag? f "security")))

(define (file-has-suite-tag? f tag)
  (define meta (get-file-metadata f))
  (equal? (hash-ref meta 'suite #f) tag))

(define (smoke-excluded? f)
  (or (slow-file? f) (string-contains? f "/workflows/") (string-contains? f "/interfaces/")))

(define support-test-module-names
  '("event-simulator.rkt" "mock-tui-session.rkt" "state-assertions.rkt" "workflow-harness.rkt"))

(define (support-test-module? f)
  (define s
    (if (path? f)
        (path->string f)
        f))
  (define base (file-name-from-path s))
  (or (string-contains? s "/helpers/")
      (and (string-contains? s "/fixtures/")
           (or (not base) (not (string-prefix? (path->string base) "test-"))))
      (and base (member (path->string base) support-test-module-names) #t)))

(define (q-root-candidate? p)
  (and (directory-exists? (build-path p "tests"))
       (file-exists? (build-path p "scripts" "run-tests.rkt"))))

(define (resolve-base-dir orig)
  (define parent (simplify-path (build-path orig "..")))
  (define candidates
    (list (simplify-path (build-path orig "q")) (simplify-path (build-path parent "q")) orig parent))
  (or (for/first ([candidate (in-list candidates)]
                  #:when (q-root-candidate? candidate))
        candidate)
      orig))

(define base-dir (resolve-base-dir (find-system-path 'orig-dir)))

(define (normalize-test-path f)
  (define s
    (if (path? f)
        (path->string f)
        f))
  (cond
    [(absolute-path? s) s]
    [(string-prefix? s "q/tests/") (substring s 2)]
    [(string-prefix? s "./q/tests/") (substring s 4)]
    [else s]))

(define (arch-file? f)
  (or (string-contains? f "arch-")
      (string-contains? f "boundary")
      (string-contains? f "fitness")
      (string-contains? f "hotspot")
      (file-has-suite-tag? f "arch")))

(define (runtime-file? f)
  (or (string-contains? f "runtime")
      (string-contains? f "session")
      (string-contains? f "compaction")
      (string-contains? f "iteration")
      (string-contains? f "turn-")
      (string-contains? f "tool-coord")
      (file-has-suite-tag? f "runtime")))

(define (extensions-file? f)
  (or (string-contains? f "extensions/")
      (string-contains? f "gsd-")
      (string-contains? f "define-extension")
      (string-contains? f "wave-executor")
      (string-contains? f "hook-")
      (file-has-suite-tag? f "extensions")))

(define (workflows-file? f)
  (and (string-contains? f "/workflows/")
       (or (not (string-contains? f "/fixtures/"))
           (string-prefix? (path->string (file-name-from-path f)) "test-"))))

(define (collect-test-files suite #:extra-files [extra-files #f])
  (cond
    [(pair? extra-files) (map normalize-test-path extra-files)]
    [else
     (define all-files
       (for/list ([f (in-directory (build-path base-dir "tests"))]
                  #:when (and (file-exists? f)
                              (let ([s (path->string f)])
                                (and (string-suffix? s ".rkt")
                                     (not (string-contains? s "/compiled/"))
                                     (not (support-test-module? s))))))
         (path->string (find-relative-path base-dir f))))
     (case suite
       [(all) all-files]
       [(fast) (filter (lambda (f) (not (slow-file? f))) all-files)]
       [(slow) (filter slow-file? all-files)]
       [(tui) (filter tui-file? all-files)]
       [(smoke) (filter (lambda (f) (not (smoke-excluded? f))) all-files)]
       [(security) (filter security-file? all-files)]
       [(arch) (filter arch-file? all-files)]
       [(runtime) (filter runtime-file? all-files)]
       [(extensions) (filter extensions-file? all-files)]
       [(workflows) (filter workflows-file? all-files)]
       [else '("tests/")])]))
