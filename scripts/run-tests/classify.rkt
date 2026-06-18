#lang racket/base

;; q/scripts/run-tests/classify.rkt — File classification and discovery
;;
;; Metadata parsing, suite classifiers, file collection, and path utilities.
;; Extracted from run-tests.rkt (v0.96.16, AX1-2).
;; STABILITY: internal (test runner infrastructure)

(require racket/string
         racket/path
         racket/match
         racket/file
         racket/system)
(require (only-in "../../util/error/error-helpers.rkt" with-safe-fallback))

;; Metadata
(provide get-file-metadata
         clear-metadata-cache!
         ;; Classifiers
         slow-file?
         tui-file?
         mutating-file?
         mutating-patterns
         security-file?
         arch-file?
         runtime-file?
         extensions-file?
         workflows-file?
         smoke-excluded?
         file-has-suite-tag?
         support-test-module?
         file-has-rackunit-tests?
         ;; Patterns (exposed for testing)
         slow-patterns
         ;; File collection
         collect-test-files
         ;; Path utilities
         base-dir
         normalize-test-path
         ;; Repo surface restore
         repo-surface-files
         restore-repo-surfaces!
         ;; Bytecode cleanup
         clean-stale-bytecode!)

;; ============================================================
;; Metadata parser
;; ============================================================

(define metadata-cache (make-hash))

(define (clear-metadata-cache!)
  (hash-clear! metadata-cache))

(define (metadata-tokens raw)
  (filter (lambda (s) (not (string=? s ""))) (regexp-split #rx"[ ,\t]+" (string-trim raw))))

(define (metadata-bool raw default)
  (define normalized (string-downcase (string-trim raw)))
  (cond
    [(string=? normalized "") default]
    [(member normalized '("true" "yes" "1" "on")) #t]
    [(member normalized '("false" "no" "0" "off")) #f]
    [else default]))

(define (metadata-line-match line tag)
  (define pattern (pregexp (format "@~a(?:[[:space:]]+([^;]*))?" (regexp-quote tag))))
  (define m (regexp-match pattern line))
  (and m (list line (string-trim (or (cadr m) "")))))

(define (get-file-metadata f)
  (hash-ref!
   metadata-cache
   f
   (lambda ()
     (define full-path
       (if (absolute-path? f)
           f
           (build-path base-dir f)))
     (cond
       [(not (file-exists? full-path)) (hash)]
       [else
        (define speed #f)
        (define suite #f)
        (define suites '())
        (define requires '())
        (define not-test? #f)
        (define mutates #f)
        (define boundary #f)
        (define isolation #f)
        (define timeout #f)
        (with-handlers ([exn:fail? (lambda (_) (void))])
          (call-with-input-file full-path
                                (lambda (port)
                                  (for ([_ (in-range 50)]
                                        #:break (eof-object? (peek-byte port)))
                                    (define line (read-line port))
                                    (when (string? line)
                                      (define speed-match (metadata-line-match line "speed"))
                                      (when speed-match
                                        (define toks (metadata-tokens (cadr speed-match)))
                                        (when (pair? toks)
                                          (set! speed (string->symbol (car toks)))))
                                      (define suite-match (metadata-line-match line "suite"))
                                      (when suite-match
                                        (set! suites (metadata-tokens (cadr suite-match)))
                                        (set! suite (and (pair? suites) (car suites))))
                                      (define requires-match (metadata-line-match line "requires"))
                                      (when requires-match
                                        (set! requires (metadata-tokens (cadr requires-match))))
                                      (define not-test-match (metadata-line-match line "not-test"))
                                      (when not-test-match
                                        (set! not-test? (metadata-bool (cadr not-test-match) #t)))
                                      (define mutates-match (metadata-line-match line "mutates"))
                                      (when mutates-match
                                        (set! mutates (string-trim (cadr mutates-match))))
                                      (define boundary-match (metadata-line-match line "boundary"))
                                      (when boundary-match
                                        (set! boundary (string-trim (cadr boundary-match))))
                                      (define isolation-match (metadata-line-match line "isolation"))
                                      (when isolation-match
                                        (set! isolation (string-trim (cadr isolation-match))))
                                      (define timeout-match
                                        (regexp-match #rx";+[ \\t]*@timeout[ \\t]+([0-9]+)" line))
                                      (when timeout-match
                                        (set! timeout (string->number (cadr timeout-match)))))))))
        (hash 'speed
              speed
              'suite
              suite
              'suites
              suites
              'requires
              requires
              'not-test?
              not-test?
              'mutates
              mutates
              'boundary
              boundary
              'isolation
              isolation
              'timeout
              timeout)]))))

;; ============================================================
;; Patterns
;; ============================================================

(define slow-patterns
  '("sandbox" "subprocess"
              "integration"
              "benchmark"
              "workflow-"
              "e2e-"
              "ci-local"
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

(define mutating-patterns
  '("ci-local" "pre-commit"
               "check-deps"
               "sync-version"
               "sync-readme"
               "bump-version"
               "metrics-readme"
               "self-hosting"))

(define support-test-module-names
  '("event-simulator.rkt" "mock-tui-session.rkt" "state-assertions.rkt" "workflow-harness.rkt"))

;; ============================================================
;; Classifiers
;; ============================================================

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
      (file-has-suite-tag? f "security")))

(define (file-has-suite-tag? f tag)
  (define meta (get-file-metadata f))
  (define suites (hash-ref meta 'suites '()))
  (or (member tag suites) #f))

(define (smoke-excluded? f)
  (or (slow-file? f) (string-contains? f "/workflows/") (string-contains? f "/interfaces/")))

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

(define (file-has-rackunit-tests? path)
  (and (file-exists? path)
       (let* ([content (file->string path)]
              [has-rackunit-forms? (or (regexp-match? #rx"\\(test-case" content)
                                       (regexp-match? #rx"\\(check-" content))]
              [module-plus-test? (regexp-match? #px"\\(module\\+\\s+test\\b" content)]
              [uses-rackunit-text-ui? (and has-rackunit-forms?
                                           (regexp-match? #rx"\\(run-tests" content))]
              [needs-raco-discovery?
               (and has-rackunit-forms? (not module-plus-test?) (not uses-rackunit-text-ui?))])
         (or module-plus-test? needs-raco-discovery?))))

;; ============================================================
;; Path resolution
;; ============================================================

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

;; ============================================================
;; File collection
;; ============================================================

(define (collect-test-files suite #:extra-files [extra-files #f])
  (cond
    [(pair? extra-files) (map normalize-test-path extra-files)]
    [else
     (define all-files
       (for/list ([f (in-directory (build-path base-dir "tests"))]
                  #:when (and (file-exists? f)
                              (let* ([s (path->string f)]
                                     [rel (path->string (find-relative-path base-dir f))])
                                (and (string-suffix? s ".rkt")
                                     (not (string-contains? s "/compiled/"))
                                     (not (support-test-module? s))
                                     (not (hash-ref (get-file-metadata rel) 'not-test? #f))))))
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

;; ============================================================
;; Repo surface restore
;; ============================================================

(define repo-surface-files '("info.rkt" "README.md" "CHANGELOG.md"))

(define (restore-repo-surfaces! root)
  (for ([surface (in-list repo-surface-files)])
    (define path (build-path root surface))
    (when (file-exists? path)
      (define git-restore (format "cd ~a && git checkout -- ~a 2>/dev/null" root surface))
      (system git-restore))))

;; ============================================================
;; Bytecode cleanup
;; ============================================================

(define (compiled-zo-source-candidates compiled-dir zo)
  (define parent (path-only compiled-dir))
  (define base-path (file-name-from-path zo))
  (define base
    (if base-path
        (path->string base-path)
        ""))
  (define stem (regexp-replace #rx"\\.zo$" base ""))
  (filter values
          (list (and (regexp-match? #rx"_rkt$" stem)
                     (build-path parent (regexp-replace #rx"_rkt$" stem ".rkt")))
                (and (regexp-match? #rx"_rktl$" stem)
                     (build-path parent (regexp-replace #rx"_rktl$" stem ".rktl")))
                (and (regexp-match? #rx"_scrbl$" stem)
                     (build-path parent (regexp-replace #rx"_scrbl$" stem ".scrbl")))
                (path-replace-extension (path-replace-suffix zo "") #".rkt")
                (path-replace-extension (path-replace-suffix zo "") #".rktl"))))

(define (stale-compiled-zo? compiled-dir zo)
  (and (file-exists? zo)
       (string-suffix? (path->string zo) ".zo")
       (let* ([candidates (compiled-zo-source-candidates compiled-dir zo)]
              [existing-sources (filter file-exists? candidates)])
         (or (null? existing-sources)
             (for/or ([src (in-list existing-sources)])
               (> (file-or-directory-modify-seconds src) (file-or-directory-modify-seconds zo)))))))

(define (clean-stale-bytecode! root)
  (define cleaned 0)
  (for ([d (in-directory root)])
    (when (and (directory-exists? d) (equal? (path->string (file-name-from-path d)) "compiled"))
      (define zo-files (directory-list d #:build? #t))
      (define stale?
        (for/or ([zo (in-list zo-files)])
          (stale-compiled-zo? d zo)))
      (when stale?
        (delete-directory/files d)
        (set! cleaned (add1 cleaned)))))
  cleaned)
