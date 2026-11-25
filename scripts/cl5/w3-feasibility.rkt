#lang racket/base

;; w3-feasibility.rkt — CL5 W3.2-A: can the frozen historical corpus support the
;; W3a measurement at all? (evaluation only; runs no trial)
;;
;; The frozen protocol (docs/reports/CL5-W3-DESIGN.md) measures, per case, the
;; shipped selector's plan and the cost of running it, against a comprehensive
;; surface. Both come from the repository's own tools at the case tree:
;; `./qt` plus the selection engine (`scripts/testing/quick-audit.rkt`,
;; `scripts/testing/full-plan.rkt`). The corpus population rule only required a
;; successful comprehensive CI run, which says nothing about whether those tools
;; exist at that tree.
;;
;; This audit answers, deterministically and without running anything:
;;   * how many cases have the selection engine at their tree;
;;   * how many of those are version-bump-only (protocol: reported separately);
;;   * whether the pre-registered per-stratum minimums can be met;
;;   * which cases are measurable at all (the candidate pilot/campaign set).
;;
;; It reads the frozen corpus, never rewrites it, and emits JSON. A conclusion
;; that the population is unusable is a finding for review, not a licence to
;; change the corpus.

(require json
         racket/cmdline
         racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         racket/system
         "ci-evidence.rkt"
         "selection-experiment.rkt")

(provide classify-case
         feasible?
         measurable-cases
         strata-summary
         version-bump-only?
         audit-corpus
         engine-paths)

;; The tools the protocol's measurement needs at the case tree. All three, because
;; the facade alone cannot enumerate a plan.
(define engine-paths (list "qt" "scripts/testing/quick-audit.rkt" "scripts/testing/full-plan.rkt"))

(define manifest-path "tests/.coverage-manifest.json")
(define admission-path "tests/.quick-admission.rktd")

;; Protocol: these production modules are derived version metadata, not behaviour.
;; A case whose production changes are a subset of them is "version-bump-only" and
;; is reported as its own bucket (CL5-W3-CORPUS-REPORT.md, 147 of 928 cases).
(define version-modules (list "info.rkt" "util/version.rkt"))

(define (version-bump-only? case)
  (define modules (hash-ref case 'production-modules '()))
  (and (pair? modules) (andmap (lambda (module) (and (member module version-modules) #t)) modules)))

;; A batched existence probe: one `git cat-file --batch-check` process for all
;; (sha, path) pairs, instead of thousands of `rev-parse` calls.
(define (presence-map repo-root queries)
  (define git (or (find-executable-path "git") "git"))
  (define-values (proc out in err)
    (parameterize ([current-directory repo-root])
      (subprocess #f #f #f git "cat-file" "--batch-check")))
  (define out-box (box ""))
  (define reader (thread (lambda () (set-box! out-box (port->string out)))))
  (with-handlers ([exn:fail? (lambda (e)
                               (raise-arguments-error
                                'presence-map
                                "git could not read the repository (not a checkout?)"
                                "repo-root"
                                (path->string (path->complete-path repo-root))
                                "detail"
                                (exn-message e)))])
    (for ([query (in-list queries)])
      (fprintf in "~a:~a\n" (car query) (cdr query)))
    (close-output-port in))
  (subprocess-wait proc)
  (thread-wait reader)
  (define lines (string-split (unbox out-box) "\n" #:trim? #t))
  (for/hash ([query (in-list queries)]
             [line (in-list lines)])
    (values (cons (car query) (cdr query)) (not (regexp-match? #rx"^.* missing$" line)))))

(define (case-key case path)
  (cons (hash-ref case 'sha) path))

(define (classify-case case presence)
  (define engine
    (for/list ([path (in-list engine-paths)]
               #:unless (hash-ref presence (case-key case path) #f))
      path))
  (define sha (hash-ref case 'sha))
  (hash 'sha
        sha
        'stratum
        (hash-ref case 'stratum)
        'subject
        (hash-ref case 'subject "")
        'production-modules
        (hash-ref case 'production-modules '())
        'cone-keys
        (hash-ref case 'cone-keys '())
        'version-bump-only?
        (version-bump-only? case)
        'engine-present?
        (null? engine)
        'engine-missing
        engine
        'manifest-present?
        (hash-ref presence (cons sha manifest-path) #f)
        'admission-record-present?
        (hash-ref presence (cons sha admission-path) #f)))

(define (strata-summary classified)
  (for/hash ([stratum (in-list (list "S1" "S2" "S3"))])
    (define group (filter (lambda (c) (equal? (hash-ref c 'stratum) stratum)) classified))
    (define measurable
      (filter (lambda (c) (and (hash-ref c 'engine-present?) (not (hash-ref c 'version-bump-only?))))
              group))
    (values (string->symbol stratum)
            (hash 'total
                  (length group)
                  'version-bump-only
                  (length (filter (lambda (c) (hash-ref c 'version-bump-only?)) group))
                  'engine-present
                  (length (filter (lambda (c) (hash-ref c 'engine-present?)) group))
                  'measurable
                  (length measurable)
                  'measurable-cones
                  (length (remove-duplicates (append* (map (lambda (c) (hash-ref c 'cone-keys))
                                                           measurable))))))))

(define (measurable-cases classified)
  (sort (for/list ([case (in-list classified)]
                   #:when (and (hash-ref case 'engine-present?)
                               (not (hash-ref case 'version-bump-only?))))
          (hash 'sha
                (hash-ref case 'sha)
                'stratum
                (hash-ref case 'stratum)
                'subject
                (hash-ref case 'subject)
                'cones
                (hash-ref case 'cone-keys)
                'manifest-present?
                (hash-ref case 'manifest-present?)))
        string<?
        #:key (lambda (c) (hash-ref c 'sha))))

;; The protocol's population gate: S2 and S3 each need at least 20 measurable cases.
(define (feasible? summary #:minimum [minimum 20])
  (and (>= (hash-ref (hash-ref summary 'S2) 'measurable) minimum)
       (>= (hash-ref (hash-ref summary 'S3) 'measurable) minimum)))

;; Optional: does the case's own commit have a completed successful CI run? The
;; corpus population rule assumed it; the audit checks the measurable subset.
(define (ci-rows repo-root measurable)
  (for/list ([case (in-list measurable)])
    (define sha (hash-ref case 'sha))
    (define runs
      (with-handlers ([exn:fail? (lambda (_) '())])
        (hash-ref (gh-json repo-root "GET" (format "actions/runs?head_sha=~a&per_page=10" sha))
                  'workflow_runs
                  '())))
    (define ci
      (for/first ([run (in-list runs)]
                  #:when (equal? (hash-ref run 'name "") "CI"))
        run))
    (hash 'sha
          sha
          'ci-run-id
          (and ci (hash-ref ci 'id))
          'ci-conclusion
          (and ci (hash-ref ci 'conclusion))
          'ci-event
          (and ci (hash-ref ci 'event))
          'eligible?
          (and ci (equal? (hash-ref ci 'conclusion #f) "success")))))

;; audit-corpus : corpus-json repo-root [--ci] -> record
(define (audit-corpus corpus
                      repo-root
                      #:ci [ci? #f]
                      #:workspace [workspace repo-root]
                      #:presence [presence-override #f])
  (define cases (hash-ref corpus 'cases))
  (define presence
    (or presence-override
        (presence-map repo-root
                      (append* (for/list ([case (in-list cases)])
                                 (map (lambda (path) (case-key case path))
                                      (append engine-paths (list manifest-path admission-path))))))))
  (define classified (map (lambda (case) (classify-case case presence)) cases))
  (define summary (strata-summary classified))
  (define measurable (measurable-cases classified))
  (define feasible (feasible? summary))
  (define minimums (hash 'S2 20 'S3 20))
  (hash 'experiment
        "CL5 W3.2-A feasibility audit (#9982)"
        'protocol
        "docs/reports/CL5-W3-DESIGN.md"
        'corpus
        "docs/reports/CL5-W3-CORPUS-431b491d.json"
        'corpus-sha
        (hash-ref corpus 'revision)
        'generated-by
        "scripts/cl5/w3-feasibility.rkt"
        'engine-paths
        engine-paths
        'preregistered-minimums
        minimums
        'strata
        (for/hash ([(stratum row) (in-hash summary)])
          (values stratum row))
        'totals
        (hash 'cases
              (length cases)
              'engine-present
              (length (filter (lambda (c) (hash-ref c 'engine-present?)) classified))
              'version-bump-only
              (length (filter (lambda (c) (hash-ref c 'version-bump-only?)) classified))
              'measurable
              (length measurable)
              'missing-coverage-manifest
              (length (filter (lambda (c) (not (hash-ref c 'manifest-present?))) classified)))
        'ci-eligible
        (if ci?
            (length (filter (lambda (row) (hash-ref row 'eligible? #f))
                            (ci-rows workspace measurable)))
            'not-checked)
        'population-feasible?
        feasible
        'reasons
        (cond
          [feasible '()]
          [else
           (list (format "S2 measurable ~a < pre-registered minimum ~a"
                         (hash-ref (hash-ref summary 'S2) 'measurable)
                         (hash-ref minimums 'S2))
                 (format "S3 measurable ~a < pre-registered minimum ~a"
                         (hash-ref (hash-ref summary 'S3) 'measurable)
                         (hash-ref minimums 'S3)))])
        'measurable-cases
        measurable
        'ci
        (if ci?
            (ci-rows workspace measurable)
            'not-checked)
        'cases
        classified))

(module+ main
  (define corpus-file #f)
  (define out-file #f)
  (define repo-root #f)
  (define workspace #f)
  (define ci? #f)
  (command-line
   #:program "w3-feasibility.rkt"
   #:once-each ["--corpus" path "frozen corpus JSON" (set! corpus-file path)]
   ["--out" path "audit JSON to write" (set! out-file path)]
   ["--repo-root" path "q checkout whose history is probed (default: cwd)" (set! repo-root path)]
   ["--workspace"
    path
    "workspace root containing scripts/gh_helpers.py (for --ci)"
    (set! workspace path)]
   ["--ci" "look up each measurable case's own CI run (network)" (set! ci? #t)]
   #:args ()
   (unless (and corpus-file out-file)
     (raise-user-error 'w3-feasibility "--corpus and --out are required"))
   (define corpus (with-input-from-file corpus-file (lambda () (read-json))))
   (define record
     (audit-corpus corpus
                   (or repo-root (current-directory))
                   #:ci ci?
                   #:workspace (or workspace repo-root (current-directory))))
   (define out-path (string->path out-file))
   (define tmp (string->path (string-append (path->string out-path) ".tmp")))
   (call-with-output-file tmp
                          (lambda (out) (write-string (jsexpr->string (->jsexpr record)) out))
                          #:exists 'truncate/replace)
   (rename-file-or-directory tmp out-path #t)
   (printf "wrote ~a: measurable=~a feasible=~a\n"
           out-file
           (hash-ref (hash-ref record 'totals) 'measurable)
           (hash-ref record 'population-feasible?))))
