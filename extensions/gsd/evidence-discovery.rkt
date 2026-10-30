#lang racket/base
;; Static, fail-closed discovery. The caller owns checkout/context freshness.
;; The internal discovery outcome is REASONED and immutable: every refusal
;; names a stable code, the failing phase, the offending path when known and
;; bounded safe detail — never a raw exception dump. The public
;; spec-or-#f wrapper (discover-evidence-probe-spec) is preserved unchanged
;; for existing callers.
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
(provide discover-evidence-probe-spec
         discover-evidence-probe-outcome
         static-probe-admission)
(define checks '(check-equal? check-eq? check-eqv? check-true check-false check-not-false check-=))
(define unselected (gensym 'unselected))

;; ---- reasoned refusal helpers -------------------------------------------
;; One shared immutable refusal shape: admitted?/code/phase/path/detail.
;; `detail` is bounded and single-line, built from our own grammar facts; it
;; never embeds an exception dump, log body or secret.
(define (bounded-detail s)
  (and s
       (let ([one (string-join (string-split (format "~a" s)) " ")])
         (string->immutable-string (if (> (string-length one) 200)
                                       (string-append (substring one 0 200) "…")
                                       one)))))
(define (refusal code phase path detail)
  (hasheq 'admitted?
          #f
          'code
          code
          'phase
          phase
          'path
          (and path (string->immutable-string path))
          'detail
          (bounded-detail detail)))

;; Read-only git probe shared by both static parsers: trimmed stdout on
;; success, #f on any failure. Never raises; callers decide the named refusal.
(define (git-out repo . args)
  (define out (open-output-string))
  (and (parameterize ([current-directory repo]
                      [current-output-port out]
                      [current-error-port (open-output-nowhere)])
         (apply system* (find-executable-path "git") args))
       (string-trim (get-output-string out))))

;; ---- reusable static test/helper/runner admissibility -------------------
;; The exact per-candidate checks discovery has always applied, factored out
;; of the discovery path so the SAME pure parser gives the SAME reason at an
;; early check (before candidate sealing/approval or expensive C1) and at a
;; fresh staged-E re-check before R1. Read-only: no candidate code is loaded
;; or expanded, and admission is advisory data — never proof, approval or a
;; substitute for freshness/binding authentication.
(define (static-probe-admission repo head command test-path harness-files)
  ;; Detail discipline: candidate-grammar failures carry the owned parser's
  ;; single-line message about the candidate's own bytes; this catch-all is
  ;; for ARBITRARY internal failures, so its detail stays stable text — never
  ;; a raw exception body that could embed environment paths.
  (with-handlers ([exn:fail?
                   (lambda (_)
                     (refusal "discovery-error" "internal" #f "internal static admission failure"))])
    (let/ec reject
      (define (need ok code phase path detail)
        (unless ok
          (reject (refusal code phase path detail))))
      (define (git . args)
        (apply git-out repo args))
      (define (phase-of from)
        (if (equal? from test-path) "test-module" "helper-module"))
      ;; 1. frozen runner/coverage authentication (same one parser).
      (define coverage (regression-command-coverage-refusal repo head command test-path))
      (when coverage
        (reject coverage))
      ;; 2. the candidate test module is a regular committed literal path.
      (need (and (string? test-path)
                 (regexp-match? #px"^tests/[A-Za-z0-9_/-]+[.]rkt$" test-path)
                 (not (member ".." (string-split test-path "/"))))
            "unsupported-test-path"
            "test-path"
            test-path
            "candidate test path is not a literal tests/*.rkt module")
      (define file (build-path repo test-path))
      (need
       (and (file-exists? file)
            (not (link-exists? file))
            (for/and ([part (in-list (drop-right (string-split test-path "/") 1))]
                      [n (in-naturals 1)])
              (not (link-exists? (apply build-path repo (take (string-split test-path "/") n))))))
       "test-not-regular-blob"
       "test-path"
       test-path
       "candidate test module is absent or reached through a link")
      (need (regexp-match? #px"^100644 blob [0-9a-f]{40}\t" (git "ls-tree" head "--" test-path))
            "test-not-regular-blob"
            "test-path"
            test-path
            "candidate test module is not a regular committed blob at head")
      ;; 3. bounded literal read: no custom reader, no nested #lang.
      (define stx-forms
        (with-handlers
            ([exn:fail?
              (lambda (_)
                (reject (refusal
                         "invalid-test-module"
                         "test-module"
                         test-path
                         "candidate test module could not be read as supported literal syntax")))])
          (read-regression-forms file)))
      (define forms (map syntax->datum stx-forms))
      ;; 4. approved harness manifest. Selection/overlay data, NOT
      ;; registration authority; the supplemental owner authenticates its
      ;; private approved manifest before staging.
      (define approved (make-hash))
      (need (and (list? harness-files) (<= (length harness-files) 8))
            "invalid-harness-manifest"
            "helper-manifest"
            #f
            "harness manifest must be a list of at most eight entries")
      (for ([entry (in-list harness-files)])
        (need (and (hash? entry) (equal? (sort (hash-keys entry) symbol<?) '(path sha256)))
              "invalid-harness-manifest"
              "helper-manifest"
              #f
              "harness manifest entry must have exactly path and sha256")
        (define p (hash-ref entry 'path))
        (define sha (hash-ref entry 'sha256))
        (need (and (string? p)
                   (regexp-match? #px"^[A-Za-z0-9_/-]+[.]rkt$" p)
                   (string-prefix? p "tests/helpers/")
                   (andmap (lambda (s) (not (member s '("" "." ".."))))
                           (string-split p "/" #:trim? #f)))
              "invalid-harness-path"
              "helper-manifest"
              p
              "harness path must be a literal tests/helpers/*.rkt file")
        (need (and (string? sha) (regexp-match? #px"^[0-9a-f]{64}$" sha))
              "invalid-harness-digest"
              "helper-manifest"
              p
              "harness digest must be a 64-hex sha256")
        (need (not (hash-has-key? approved p))
              "duplicate-harness-entry"
              "helper-manifest"
              p
              "duplicate harness manifest path")
        (hash-set! approved p #t)
        (define entry*
          (and (file-exists? (build-path repo p))
               (let ([blob (git "hash-object" "--no-filters" "--" p)])
                 (and blob (format "100644 blob ~a\t~a" blob p)))))
        (need (and entry*
                   (for/and ([n (in-range 1 (add1 (length (string-split p "/"))))])
                     (not (link-exists? (apply build-path repo (take (string-split p "/") n)))))
                   (equal? (git "ls-tree" head "--" p) entry*))
              "harness-not-regular-blob"
              "helper-manifest"
              p
              "approved helper is absent, linked or not a regular committed blob")
        (need (<= (file-size (build-path repo p)) 131072)
              "harness-too-large"
              "helper-manifest"
              p
              "approved helper exceeds the 128 KiB static bound")
        (need (equal? sha (sha256-file (build-path repo p)))
              "harness-digest-mismatch"
              "helper-manifest"
              p
              "approved helper bytes differ from the manifest digest"))
      ;; 5. closure walk over the test module and every approved helper.
      ;; Resolve before classifying, but inspect UNSIMPLIFIED components too:
      ;; a symlink/.. sequence must never hide a symlinked path component.
      (define seen (make-hash))
      (define nodes 0)
      (define (resolve from mod)
        (need (and (string? mod) (relative-path? mod) (string-suffix? mod ".rkt"))
              "unsafe-require-path"
              (phase-of from)
              from
              "require must be a literal relative .rkt path")
        (define raw (build-path repo (or (path-only (string->path from)) (string->path ".")) mod))
        (define current (simplify-path (path->complete-path repo) #f))
        (for ([part (in-list (append (explode-path (or (path-only (string->path from))
                                                       (string->path ".")))
                                     (explode-path (string->path mod))))])
          (set! current (build-path current part))
          (need (not (link-exists? current))
                "unsafe-require-path"
                (phase-of from)
                from
                "require traverses a linked path component")
          (set! current (simplify-path current #f))
          (need (string-prefix? (path->string (path->directory-path current))
                                (path->string (path->directory-path
                                               (simplify-path (path->complete-path repo) #f))))
                "unsafe-require-path"
                (phase-of from)
                from
                "require escapes the repository"))
        (define p
          (path->string (find-relative-path (simplify-path (path->complete-path repo) #f)
                                            (simplify-path raw #f))))
        (need (and (regexp-match? #px"^[A-Za-z0-9_/-]+[.]rkt$" p)
                   (file-exists? (build-path repo p))
                   (for/and ([n (in-range 1 (add1 (length (string-split p "/"))))])
                     (not (link-exists? (apply build-path repo (take (string-split p "/") n)))))
                   (let ([entry (git "ls-tree" head "--" p)]
                         [blob (git "hash-object" "--no-filters" "--" p)])
                     (and entry blob (equal? entry (format "100644 blob ~a\t~a" blob p)))))
              "dependency-not-regular-blob"
              (phase-of from)
              p
              "imported module is absent, linked or not a regular committed blob")
        p)
      (define (dependency from mod)
        (define p (resolve from mod))
        (when (string-prefix? p "tests/")
          (need (hash-has-key? approved p)
                "helper-dependency-unapproved"
                (phase-of from)
                p
                "imported tests/ module is not in the approved harness manifest")
          (visit p)))
      (define (visit p)
        (unless (hash-has-key? seen p)
          (hash-set! seen p #t)
          (define helper-forms
            (with-handlers ([exn:fail?
                             (lambda (_)
                               (reject
                                (refusal
                                 "invalid-helper-module"
                                 "helper-module"
                                 p
                                 "approved helper could not be read as supported literal syntax")))])
              (map syntax->datum (read-regression-forms (build-path repo p)))))
          (scan p helper-forms #t)))
      (define (scan from module-forms helper?)
        (define runtime-paths (make-hasheq))
        (when helper?
          (for ([f (in-list module-forms)])
            (need (and (list? f)
                       (pair? f)
                       (memq (car f)
                             '(require provide
                                       define
                                       define-runtime-path)))
                  "unsupported-helper-form"
                  "helper-module"
                  from
                  (format "unsupported helper top-level form: ~a"
                          (if (and (pair? f) (symbol? (car f)))
                              (car f)
                              "non-list form")))
            (when (eq? (car f) 'define-runtime-path)
              (need (and (= (length f) 3) (symbol? (cadr f)) (string? (caddr f)))
                    "unsupported-helper-form"
                    "helper-module"
                    from
                    "malformed define-runtime-path")
              (need (not (hash-has-key? runtime-paths (cadr f)))
                    "unsupported-helper-form"
                    "helper-module"
                    from
                    "duplicate define-runtime-path name")
              (hash-set! runtime-paths (cadr f) (caddr f))
              (resolve from (caddr f)))))
        (define (walk f)
          (set! nodes (add1 nodes))
          (need (<= nodes 20000)
                "module-too-complex"
                (phase-of from)
                from
                "module form tree exceeds the bounded static walk")
          (when (pair? f)
            (need (list? f)
                  "unsupported-form"
                  (phase-of from)
                  from
                  "improper list in module form tree")
            (case (car f)
              [(require)
               (for ([mod (in-list (cdr f))])
                 (cond
                   [(string? mod) (dependency from mod)]
                   [(and (symbol? mod)
                         (or (memq mod '(rackunit rackunit/text-ui))
                             (regexp-match? #px"^racket/[A-Za-z0-9/-]+$" (symbol->string mod))))
                    (void)]
                   [(symbol? mod)
                    (need #f
                          "unsupported-require"
                          (phase-of from)
                          from
                          (format "unsupported module require: ~a" mod))]
                   [else
                    (need #f
                          "unsupported-require"
                          (phase-of from)
                          from
                          (format "unsupported import form: ~a"
                                  (if (and (pair? mod) (symbol? (car mod)))
                                      (car mod)
                                      "structured")))]))]
              [(dynamic-require)
               (if helper?
                   (need (<= 3 (length f) 4)
                         "unsupported-import"
                         "helper-module"
                         from
                         "dynamic-require must name one module and a mode")
                   (need #f
                         "unsupported-import"
                         "test-module"
                         from
                         "dynamic-require is unsupported in the test module"))
               (define mod (cadr f))
               (dependency
                from
                (if (string? mod)
                    mod
                    (hash-ref
                     runtime-paths
                     mod
                     (lambda ()
                       (need #f
                             "unsupported-import"
                             "helper-module"
                             from
                             "dynamic-require target is not a declared define-runtime-path")))))]
              [(local-require #%require define-syntax define-syntax-rule module module*)
               (need #f
                     "unsupported-form"
                     (phase-of from)
                     from
                     (format "unsupported form: ~a" (car f)))])
            (for-each walk f)))
        (for-each walk module-forms))
      (scan test-path forms #f)
      (define unused
        (for/first ([p (in-list (sort (hash-keys approved) string<?))]
                    #:unless (hash-has-key? seen p))
          p))
      (need (and (= (hash-count approved) (hash-count seen)) (not unused))
            "unused-approved-helper"
            "helper-manifest"
            unused
            "approved helper is never imported by the candidate")
      ;; 6. suite export, literal suite grammar and direct-check grammar.
      (need (ormap (lambda (s) (equal? (syntax->datum s) '(provide evidence-suite))) stx-forms)
            "missing-suite-export"
            "suite-grammar"
            test-path
            "candidate must export evidence-suite")
      (define defs
        (filter (lambda (s)
                  (define xs (syntax->list s))
                  (and xs
                       (= (length xs) 3)
                       (eq? (syntax-e (car xs)) 'define)
                       (eq? (syntax-e (cadr xs)) 'evidence-suite)))
                stx-forms))
      (need (= (length defs) 1)
            "missing-suite-export"
            "suite-grammar"
            test-path
            "candidate must define evidence-suite exactly once")
      (define targets '())
      (define names '())
      (define sites '())
      (define (suite s trail)
        (define xs (syntax->list s))
        (need (and xs
                   (>= (length xs) 2)
                   (eq? (syntax-e (car xs)) 'test-suite)
                   (string? (syntax-e (cadr xs))))
              "invalid-suite-grammar"
              "suite-grammar"
              test-path
              "evidence-suite must be literal test-suite forms")
        (define path (append trail (list (syntax-e (cadr xs)))))
        (for ([child (in-list (cddr xs))])
          (define cs (syntax->list child))
          (need (and cs (pair? cs))
                "invalid-suite-grammar"
                "suite-grammar"
                test-path
                "suite children must be literal forms")
          (if (eq? (syntax-e (car cs)) 'test-suite)
              (suite child path)
              (let ()
                (need (and (= (length cs) 3)
                           (eq? (syntax-e (car cs)) 'test-case)
                           (string? (syntax-e (cadr cs))))
                      "invalid-suite-grammar"
                      "suite-grammar"
                      test-path
                      "each test-case must have one literal name and one direct check")
                (define name (syntax-e (cadr cs)))
                (need (not (member name names))
                      "invalid-suite-grammar"
                      "suite-grammar"
                      test-path
                      (format "duplicate test-case name: ~a" name))
                (set! names (cons name names))
                (define check (caddr cs))
                (define parts (syntax->list check))
                (need (and parts (pair? parts) (memq (syntax-e (car parts)) checks))
                      "invalid-suite-grammar"
                      "suite-grammar"
                      test-path
                      "test-case must contain one direct literal check")
                (set! sites (cons (syntax-position check) sites))
                (set! targets
                      (append targets
                              (list (hasheq 'test-path
                                            test-path
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
      (define (walk-checks s)
        (define xs (syntax->list s))
        (when xs
          (when (and (pair? xs)
                     (symbol? (syntax-e (car xs)))
                     (regexp-match? #rx"^check-" (symbol->string (syntax-e (car xs)))))
            (set! all (cons (syntax-position s) all)))
          (for-each walk-checks xs)))
      (for-each walk-checks stx-forms)
      (need (and (pair? targets) (equal? (sort sites <) (sort all <)))
            "hidden-check-form"
            "suite-grammar"
            test-path
            "hidden or nested check forms are unsupported")
      ;; 7. structural module validation (language header, exact module+ test
      ;; runner, export counts). Constant runner messages are safe details.
      (with-handlers ([exn:fail?
                       (lambda (_)
                         (reject
                          (refusal
                           "invalid-test-module"
                           "test-module"
                           test-path
                           "candidate module does not satisfy the supported regression grammar")))])
        (validate-regression-module! file #:bindings? #f))
      (hasheq 'admitted?
              #t
              'stage
              (findf (lambda (s) (member test-path (string-split s))) (owned-chain-stages command))
              'target
              (car targets)))))

;; ---- reasoned discovery outcome -----------------------------------------
(define (discover-evidence-probe-outcome repo
                                         context
                                         normalized-command
                                         #:harness-files [harness-files '()]
                                         #:baseline-head [baseline-head unselected]
                                         #:test-path [test-path unselected])
  ;; Same detail discipline as static-probe-admission above.
  (with-handlers ([exn:fail?
                   (lambda (_)
                     (hash-set (refusal "discovery-error" "internal" #f "internal discovery failure")
                               'status
                               "refused"))])
    (let/ec reject
      (define (refuse-with r)
        (reject (hash-set r 'status "refused")))
      (define (need ok code phase path detail)
        (unless ok
          (refuse-with (refusal code phase path detail))))
      (define (git . args)
        (apply git-out repo args))
      (need (valid-evidence-context? context)
            "invalid-context"
            "context"
            #f
            "the evidence context is not a valid canonical context")
      (need (equal? (git "status" "--porcelain" "--untracked-files=all") "")
            "checkout-dirty"
            "context"
            #f
            "the execution checkout is not clean")
      (define head (hash-ref context 'head))
      (need (equal? head (git "rev-parse" "HEAD"))
            "head-moved"
            "context"
            #f
            "the context head is not the checked-out HEAD")
      (define stages (owned-chain-stages normalized-command))
      (need stages
            "unsupported-command"
            "command"
            #f
            "the normalized command has no owned verify stages")
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
      (need (pair? paths)
            "no-probe-module"
            "command"
            #f
            "the normalized command yields no literal tests/*.rkt probe module")
      (define (production-difference? base)
        (define diff (git "diff" "--name-only" base head "--"))
        (and diff
             (for/or ([p (in-list (string-split diff "\n"))])
               (and (string-suffix? p ".rkt") (not (string-prefix? p "tests/"))))))
      (define (baseline-refusal base)
        (cond
          [(not (and (string? base) (regexp-match? #px"^[0-9a-f]{40}$" base))) "invalid-baseline-pin"]
          [(equal? base head) "baseline-not-ancestor"]
          [(not (git "merge-base" "--is-ancestor" base head)) "baseline-not-ancestor"]
          [(not (production-difference? base)) "no-production-delta"]
          [else #f]))
      (define baseline
        (if (not (eq? baseline-head unselected))
            (let ([r (baseline-refusal baseline-head)])
              (need
               (not r)
               r
               "baseline"
               baseline-head
               (case r
                 [("invalid-baseline-pin") "pinned baseline must be a 40-hex commit id"]
                 [("baseline-not-ancestor")
                  "pinned baseline is not a strict ancestor of the candidate head"]
                 [else
                  "pinned baseline has no production .rkt difference against the candidate head"]))
              baseline-head)
            (or (for/or ([ref '("origin/main" "main")])
                  (define b (git "merge-base" head ref))
                  (and b (not (baseline-refusal b)) b))
                (for/or ([n (in-range 1 21)])
                  (define b (git "rev-parse" "--verify" (format "~a~~~a" head n)))
                  (and b (not (baseline-refusal b)) b))
                (refuse-with
                 (refusal "no-eligible-baseline"
                          "baseline"
                          #f
                          "no production-delta ancestor of head is an eligible baseline")))))
      (define first-refusal (box #f))
      (define (spec-from p admission)
        (hasheq 'kind
                "probe-spec/v1"
                'context
                context
                'baseline-head
                baseline
                'baseline-tree
                (git "rev-parse" (string-append baseline "^{tree}"))
                'focused-normalized-command
                (hash-ref admission 'stage)
                'test-paths
                (list p)
                'test-files
                (list (hasheq 'path p 'sha256 (sha256-file (build-path repo p))))
                'harness-files
                harness-files
                'suite-export
                "evidence-suite"
                'target
                (hash-ref admission 'target)
                'runtime
                (hasheq 'racket-version (version) 'platform (symbol->string (system-type 'os)))
                ;; A cold first compilation of a #lang racket regression
                ;; module routinely costs tens of seconds; 300 seconds is
                ;; the plan's initial per-job ceiling (schema cap 600).
                'timeout-sec
                300))
      (cond
        [(not (eq? test-path unselected))
         (need (member test-path paths)
               "test-not-in-command"
               "command"
               test-path
               "the selected probe module is not named by the normalized command")
         (define admission
           (static-probe-admission repo head normalized-command test-path harness-files))
         (unless (hash-ref admission 'admitted?)
           (refuse-with admission))
         (hasheq 'status "admitted" 'spec (spec-from test-path admission))]
        [else
         (define spec
           (for/or ([p (in-list (remove-duplicates paths))])
             (define admission (static-probe-admission repo head normalized-command p harness-files))
             (if (hash-ref admission 'admitted?)
                 (spec-from p admission)
                 (begin
                   (unless (unbox first-refusal)
                     (set-box! first-refusal admission))
                   #f))))
         (unless spec
           (refuse-with (or (unbox first-refusal)
                            (refusal "no-admissible-candidate"
                                     "candidate"
                                     #f
                                     "no candidate probe module passed static admissibility"))))
         (hasheq 'status "admitted" 'spec spec)]))))

;; Legacy public wrapper: spec or #f, byte-compatible with the previous
;; boolean-only behavior for every existing caller.
(define (discover-evidence-probe-spec repo
                                      context
                                      normalized-command
                                      #:harness-files [harness-files '()]
                                      #:baseline-head [baseline-head unselected]
                                      #:test-path [test-path unselected])
  (define outcome
    (discover-evidence-probe-outcome repo
                                     context
                                     normalized-command
                                     #:harness-files harness-files
                                     #:baseline-head baseline-head
                                     #:test-path test-path))
  (and (equal? (hash-ref outcome 'status #f) "admitted") (hash-ref outcome 'spec)))
