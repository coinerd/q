#lang racket/base

;; w3-corpus.rkt — CL5 Wave 3.0: historical corpus extraction (evaluation only).
;;
;; Turns `git log` into a frozen, stratified case manifest for the W3 protocol
;; (`docs/reports/CL5-W3-DESIGN.md`). Nothing here touches selection, CI or the
;; scheduler: it reads history and emits JSON.
;;
;; The corpus is EFFICIENCY-ONLY by design. A merged commit's tree already passed
;; comprehensive CI, so replaying it cannot produce a `detected` case except
;; through flakiness, which the protocol quarantines as `unavailable`. Safety
;; evidence comes from the mutant corpus (W3.3).
;;
;; Exclusion rules are applied in a fixed order and every excluded commit is
;; COUNTED, never silently dropped:
;;
;;   merge-commit        more than one parent
;;   revert-commit       subject is a revert
;;   non-production      no changed file in the production universe
;;   formatting-only     production files with exactly zero changed lines
;;                       (binary changes are CASES, not exclusions)
;;
;; Strata (protocol): one changed production module = S1, two or more modules
;; sharing one contract cone = S2, two or more cones = S3. The cone key is the
;; module's declared owner detector when it has one, else the module itself.

(require json
         racket/list
         racket/set
         racket/match
         racket/path
         racket/port
         racket/string
         racket/system
         "../testing/changed-selection.rkt"
         "ownership-audit.rkt"
         "contract-escalation.rkt")

(provide cone-key
         classify-commit
         parse-git-log
         extract-cases
         corpus-manifest
         write-manifest!
         git-commits
         default-strata-minimum)

;; Protocol W3.0 gate: at least this many cases in each of S2 and S3.
(define default-strata-minimum 20)

;; ---- cone attribution -------------------------------------------------------------

;; A module belongs to the cone of its declared owner detector. Modules with no
;; declared detector are their own cone: their cone is exactly themselves, so
;; two such modules never collapse into one "shared" stratum by accident.
;; A module with a declared owner detector belongs to that owner's cone.
;;
;; Otherwise its cone is the shared contract it consumes: the highest-fan-in
;; transitive dependency with at least `contract-fan-in` consumers. That is the
;; CL5 notion (a shared module plus its consumers) and it is graph-derived, so
;; it does not depend on the coverage manifest being complete. Modules that
;; consume no such contract are their own cone, which keeps two unrelated
;; modules from being grouped by accident.
(define contract-fan-in 3)

(define (cone-key module
                  covers-inverted
                  #:dependents [dependents (hash)]
                  #:requires [requires (hash)])
  (define tests (hash-ref covers-inverted module '()))
  (cond
    [(pair? tests) (string-append "owner:" (car (sort tests string<?)))]
    [else
     (define gov (governing-contract module dependents #:requires requires))
     (if gov (string-append "gov:" gov) (string-append "self:" module))]))

(define (governing-contract module dependents #:requires [requires (hash)])
  (define requires-map requires)
  (define seen (mutable-set))
  (define queue (list module))
  (define ancestors '())
  (let loop ()
    (unless (null? queue)
      (define m (car queue))
      (set! queue (cdr queue))
      (unless (set-member? seen m)
        (set-add! seen m)
        (for ([dep (in-list (hash-ref requires-map m '()))])
          (set! ancestors (cons dep ancestors))
          (set! queue (append queue (list dep))))
        (loop))))
  (define candidates
    (sort (for/list ([a (in-list (remove-duplicates ancestors))]
                     #:when (>= (length (hash-ref dependents a '())) contract-fan-in))
            a)
          string<?))
  (and (pair? candidates)
       (argmax (lambda (a) (length (hash-ref dependents a '()))) candidates)))

;; ---- classification ---------------------------------------------------------------

;; commit : hash with 'sha 'parents 'subject 'files
;;   files : (listof (hash 'path 'added 'deleted))  ; added/deleted are
;;           exact non-negative integers, or 'binary
;; ctx    : hash with 'production (set of paths) 'covers (module -> tests)
;;   -> (hash 'kind 'case|'excluded 'reason symbol? 'detail ...)
(define (classify-commit commit ctx)
  (define parents (hash-ref commit 'parents '()))
  (define subject (hash-ref commit 'subject ""))
  (define files (hash-ref commit 'files '()))
  (define production (hash-ref ctx 'production (hash)))
  (define covers (hash-ref ctx 'covers (hash)))
  (define dependents (hash-ref ctx 'dependents (hash)))
  (define requires (hash-ref ctx 'requires (hash)))
  (define production-files
    (for/list ([f (in-list files)]
               #:when (hash-ref production (hash-ref f 'path) #f))
      f))
  (define (binary? f)
    (or (not (exact-nonnegative-integer? (hash-ref f 'added 'binary)))
        (not (exact-nonnegative-integer? (hash-ref f 'deleted 'binary)))))
  (define has-binary?
    (for/or ([f (in-list production-files)])
      (binary? f)))
  (define exact-lines
    (for/sum ([f (in-list production-files)])
             (if (binary? f)
                 0
                 (+ (hash-ref f 'added) (hash-ref f 'deleted)))))
  (cond
    [(> (length parents) 1) (excluded commit 'merge-commit (format "~a parents" (length parents)))]
    [(regexp-match? #rx"(?i:^revert)" subject) (excluded commit 'revert-commit subject)]
    [(null? production-files)
     (excluded commit
               'non-production
               (format "~a changed file(s), none in the production universe" (length files)))]
    [(and (not has-binary?) (zero? exact-lines))
     (excluded commit
               'formatting-only
               (format "~a production file(s), zero changed lines" (length production-files)))]
    [else
     (define modules (remove-duplicates (map (lambda (f) (hash-ref f 'path)) production-files)))
     (define keys
       (remove-duplicates (map (lambda (m) (cone-key m covers #:dependents dependents #:requires requires))
                               modules)))
     (define stratum
       (cond
         [(= 1 (length modules)) 'S1]
         [(= 1 (length keys)) 'S2]
         [else 'S3]))
     (hash 'kind
           'case
           'sha
           (hash-ref commit 'sha)
           'subject
           subject
           'parents
           parents
           'stratum
           stratum
           'production-modules
           (sort modules string<?)
           'cone-keys
           (sort keys string<?)
           'production-changes
           (for/list ([f (in-list (sort production-files
                                        (lambda (a b)
                                          (string<? (hash-ref a 'path) (hash-ref b 'path)))))])
             (hasheq 'path
                     (hash-ref f 'path)
                     'added
                     (json-count (hash-ref f 'added 'binary))
                     'deleted
                     (json-count (hash-ref f 'deleted 'binary))))
           'ignored-files
           (- (length files) (length production-files)))]))

(define (json-count v)
  (if (exact-nonnegative-integer? v) v "binary"))

(define (excluded commit reason detail)
  (hasheq 'kind
          'excluded
          'sha
          (hash-ref commit 'sha)
          'subject
          (hash-ref commit 'subject "")
          'reason
          reason
          'detail
          detail))

;; ---- git adapter -------------------------------------------------------------------

;; Parse `git log --format=%x00%H%x1f%P%x1f%s --numstat` output. Records are
;; NUL-separated; the header fields are unit-separated; numstat lines are
;; "<added>\t<deleted>\t<path>" with "-" for binary.
(define (parse-git-log text)
  (for/list ([chunk (in-list (string-split text "\u0000"))]
             #:unless (string=? (string-trim chunk) ""))
    (define lines (string-split chunk "\n" #:trim? #f))
    (define header (car lines))
    (define parts (string-split header "\u001f"))
    (when (< (length parts) 3)
      (error 'parse-git-log "malformed header: ~e" header))
    (define numstat
      (for/list ([line (in-list (cdr lines))]
                 #:unless (string=? (string-trim line) ""))
        (match (string-split line "\t")
          [(list added deleted path)
           (hasheq 'path
                   path
                   'added
                   (if (equal? added "-")
                       'binary
                       (string->number added))
                   'deleted
                   (if (equal? deleted "-")
                       'binary
                       (string->number deleted)))]
          [(list path) (hasheq 'path path 'added 'binary 'deleted 'binary)]
          [_ #f])))
    (hasheq 'sha
            (car parts)
            'parents
            (filter (lambda (s) (not (string=? s ""))) (string-split (cadr parts) " "))
            'subject
            (string-join (cddr parts) "\u001f")
            'files
            (filter values numstat))))

(define (git-commits root #:limit [limit 500] #:revision [revision "HEAD"])
  ;; subprocess does not PATH-search a bare name, and system* connects the child
  ;; to our ports instead of capturing. Read stdout to EOF first, then wait, so a
  ;; large log cannot fill the pipe and deadlock.
  (define git (or (find-executable-path "git") (error 'git-commits "git not found")))
  (define-values (proc stdout stdin stderr)
    (subprocess #f
                #f
                #f
                git
                "-C"
                (path->string root)
                "log"
                (format "-n~a" limit)
                "--numstat"
                "--format=%x00%H%x1f%P%x1f%s"
                revision))
  (define out (port->string stdout))
  (define err (port->string stderr))
  (subprocess-wait proc)
  (unless (zero? (subprocess-status proc))
    (error 'git-commits "git log failed for ~a: ~a" revision (string-trim err)))
  (parse-git-log out))

(define (extract-cases commits ctx)
  (define classified (map (lambda (c) (classify-commit c ctx)) commits))
  (define cases (filter (lambda (r) (eq? (hash-ref r 'kind) 'case)) classified))
  (define excluded-rows (filter (lambda (r) (eq? (hash-ref r 'kind) 'excluded)) classified))
  (hasheq 'cases
          cases
          'excluded
          excluded-rows
          'counts
          (hasheq 'total (length commits) 'cases (length cases) 'excluded (length excluded-rows))))

;; ---- manifest ----------------------------------------------------------------------

(define (stratum-summary cases)
  (for/fold ([acc (hasheq 'S1 0 'S2 0 'S3 0)]) ([c (in-list cases)])
    (hash-update acc (hash-ref c 'stratum) add1)))

(define (corpus-manifest revision cases excluded-rows #:limit [limit 500])
  (define counts
    (for/fold ([acc (hash)]) ([r (in-list excluded-rows)])
      (hash-update acc (hash-ref r 'reason) add1 0)))
  (hasheq 'protocol
          "docs/reports/CL5-W3-DESIGN.md"
          'purpose
          "efficiency-and-escalation-only (W3a); safety evidence comes from the mutant corpus"
          'revision
          revision
          'window
          (hasheq 'limit limit)
          'generated-by
          "scripts/cl5/w3-corpus.rkt"
          'cone-definition
          (string-append "owner detector when declared; else the highest-fan-in transitive "
                         "dependency with at least " (number->string contract-fan-in)
                         " consumers; else the module itself")
          'strata
          (stratum-summary cases)
          'excluded-counts
          (for/hasheq ([(k v) (in-hash counts)])
            (values k v))
          'cases
          (for/list ([c (in-list
                         (sort cases (lambda (a b) (string<? (hash-ref a 'sha) (hash-ref b 'sha)))))])
            (hasheq 'sha
                    (hash-ref c 'sha)
                    'subject
                    (hash-ref c 'subject)
                    'stratum
                    (symbol->string (hash-ref c 'stratum))
                    'production-modules
                    (hash-ref c 'production-modules)
                    'cone-keys
                    (hash-ref c 'cone-keys)
                    'production-changes
                    (hash-ref c 'production-changes)))))

(define (write-manifest! path manifest)
  (call-with-output-file path
                         (lambda (out)
                           (write-json manifest out)
                           (newline out))
                         #:exists 'replace))

;; ---- CLI ---------------------------------------------------------------------------

(module+ main
  (require racket/cmdline)
  (define limit (make-parameter 500))
  (define out (make-parameter #f))
  (define require-min (make-parameter #f))
  (define revision (make-parameter "HEAD"))
  (command-line #:program "w3-corpus"
                #:once-each [("-n" "--limit") v "Commits to scan" (limit (string->number v))]
                [("-o" "--out") v "Manifest path" (out v)]
                [("-r" "--revision") v "Revision to scan" (revision v)]
                [("--require-min")
                 v
                 "Exit 1 when S2 or S3 is below this many cases"
                 (require-min (string->number v))]
                #:args ()
                (define root (current-directory))
                (define-values (covers _sources status) (load-coverage-manifest root))
                (define graph (repo-inputs root))
                (define ctx
                  (hasheq 'production
                          (for/hash ([p (in-list (production-files root))])
                            (values p #t))
                          'covers
                          (invert-covers covers)
                          'manifest-status
                          status
                          'dependents
                          (hash-ref graph 'dependents)
                          'requires
                          (hash-ref graph 'requires)))
                (define commits (git-commits root #:limit (limit) #:revision (revision)))
                (define result (extract-cases commits ctx))
                (define manifest
                  (corpus-manifest (revision)
                                   (hash-ref result 'cases)
                                   (hash-ref result 'excluded)
                                   #:limit (limit)))
                (define strata (hash-ref manifest 'strata))
                (printf "scanned ~a commit(s)\n" (length commits))
                (printf "cases: ~a  S1=~a S2=~a S3=~a\n"
                        (length (hash-ref result 'cases))
                        (hash-ref strata 'S1)
                        (hash-ref strata 'S2)
                        (hash-ref strata 'S3))
                (printf "excluded: ~a\n" (hash-ref manifest 'excluded-counts))
                (printf "coverage manifest: ~a\n" status)
                (when (out)
                  (write-manifest! (out) manifest)
                  (printf "wrote ~a\n" (out)))
                (when (require-min)
                  (define min (require-min))
                  (when (or (< (hash-ref strata 'S2) min) (< (hash-ref strata 'S3) min))
                    (eprintf "FAIL: S2=~a or S3=~a below required ~a\n"
                             (hash-ref strata 'S2)
                             (hash-ref strata 'S3)
                             min)
                    (exit 1)))))
