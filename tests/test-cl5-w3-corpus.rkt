#lang racket

;; Tests for scripts/cl5/w3-corpus.rkt (CL5 W3.0 corpus extraction).
;; Pure classification only: no git, no network, no file writes.

(require rackunit
         json
         racket/list
         "../scripts/cl5/w3-corpus.rkt")

(define production
  (for/hash ([p (in-list '("agent/a.rkt" "agent/b.rkt" "llm/c.rkt" "tools/d.rkt" "util/e.rkt"))])
    (values p #t)))

(define covers (hash "agent/a.rkt" (list "tests/test-a.rkt") "llm/c.rkt" (list "tests/test-a.rkt")))

(define ctx (hasheq 'production production 'covers covers))

(define (commit #:sha [sha "1111111111111111111111111111111111111111"]
                #:parents [parents (list "2222222222222222222222222222222222222222")]
                #:subject [subject "feat: change"]
                #:files [files '()])
  (hasheq 'sha sha 'parents parents 'subject subject 'files files))

(define (file path #:added [added 1] #:deleted [deleted 0])
  (hasheq 'path path 'added added 'deleted deleted))

;; ---- exclusions -------------------------------------------------------------------

(test-case "merge commits are excluded and counted"
  (define r
    (classify-commit (commit #:parents (list "a" "b") #:files (list (file "agent/a.rkt"))) ctx))
  (check-eq? (hash-ref r 'kind) 'excluded)
  (check-eq? (hash-ref r 'reason) 'merge-commit))

(test-case "revert commits are excluded"
  (define r
    (classify-commit (commit #:subject "Revert \"feat: thing\"" #:files (list (file "agent/a.rkt")))
                     ctx))
  (check-eq? (hash-ref r 'reason) 'revert-commit))

(test-case "documentation- and test-only commits are classified as non-production"
  (for ([path (in-list '("docs/reports/X.md" "tests/test-x.rkt" "README.md" "artifacts/x.json"))])
    (define r (classify-commit (commit #:files (list (file path))) ctx))
    (check-eq? (hash-ref r 'reason) 'non-production)))

(test-case "a whitespace-only production change is formatting-only"
  (define r (classify-commit (commit #:files (list (file "agent/a.rkt" #:added 0 #:deleted 0))) ctx))
  (check-eq? (hash-ref r 'reason) 'formatting-only))

(test-case "a binary production change is a case, not a formatting exclusion"
  (define bin (hasheq 'path "agent/a.rkt" 'added 'binary 'deleted 'binary))
  (define r (classify-commit (commit #:files (list bin)) ctx))
  (check-eq? (hash-ref r 'kind) 'case))

;; ---- strata -----------------------------------------------------------------------

(test-case "one production module is S1"
  (define r (classify-commit (commit #:files (list (file "agent/a.rkt"))) ctx))
  (check-eq? (hash-ref r 'kind) 'case)
  (check-eq? (hash-ref r 'stratum) 'S1)
  (check-equal? (hash-ref r 'production-modules) '("agent/a.rkt")))

(test-case "two modules sharing an owner detector are S2"
  (define r (classify-commit (commit #:files (list (file "agent/a.rkt") (file "llm/c.rkt"))) ctx))
  (check-eq? (hash-ref r 'stratum) 'S2)
  (check-equal? (hash-ref r 'cone-keys) '("owner:tests/test-a.rkt")))

(test-case "two modules with different owners are S3"
  (define r (classify-commit (commit #:files (list (file "agent/a.rkt") (file "tools/d.rkt"))) ctx))
  (check-eq? (hash-ref r 'stratum) 'S3)
  (check-equal? (hash-ref r 'cone-keys) '("owner:tests/test-a.rkt" "self:tools/d.rkt")))

(test-case "ignored non-production files are counted but do not affect the stratum"
  (define r (classify-commit (commit #:files (list (file "agent/a.rkt") (file "docs/x.md"))) ctx))
  (check-eq? (hash-ref r 'stratum) 'S1)
  (check-equal? (hash-ref r 'ignored-files) 1))

;; ---- cone attribution --------------------------------------------------------------

(test-case "a declared owner detector outranks the dependency graph"
  (define key (cone-key "agent/a.rkt" covers))
  (check-equal? key "owner:tests/test-a.rkt"))

(test-case "modules consuming the same shared contract share a cone"
  (define dependents (hash "util/e.rkt" (list "agent/a.rkt" "agent/b.rkt" "tools/d.rkt")))
  (define requires
    (hash "agent/a.rkt"
          (list "util/e.rkt")
          "agent/b.rkt"
          (list "agent/a.rkt" "util/e.rkt")
          "tools/d.rkt"
          (list "util/e.rkt")))
  (check-equal? (cone-key "agent/a.rkt" (hash) #:dependents dependents #:requires requires)
                "gov:util/e.rkt")
  (check-equal? (cone-key "agent/b.rkt" (hash) #:dependents dependents #:requires requires)
                "gov:util/e.rkt")
  ;; and they therefore land in the same stratum
  (define r
    (classify-commit
     (commit #:files (list (file "agent/a.rkt") (file "agent/b.rkt")))
     (hasheq 'production production 'covers (hash) 'dependents dependents 'requires requires)))
  (check-eq? (hash-ref r 'stratum) 'S2))

(test-case "a module with no shared contract is its own cone"
  (check-equal? (cone-key "agent/a.rkt" (hash) #:dependents (hash) #:requires (hash))
                "self:agent/a.rkt"))

(test-case "the highest-fan-in governing contract wins"
  (define dependents
    (hash "util/e.rkt"
          (list "agent/a.rkt" "agent/b.rkt" "tools/d.rkt")
          "llm/c.rkt"
          (list "agent/a.rkt")))
  (define requires (hash "agent/a.rkt" (list "util/e.rkt" "llm/c.rkt")))
  (check-equal? (cone-key "agent/a.rkt" (hash) #:dependents dependents #:requires requires)
                "gov:util/e.rkt"))

;; ---- git parsing -------------------------------------------------------------------

(define sample-log
  (string-append "\u0000aaaa\u001fpppp\u001fsubject one\n"
                 "\n"
                 "3\t1\tagent/a.rkt\n"
                 "-\t-\tartifacts/blob.bin\n"
                 "\u0000bbbb\u001fccccc\u001fsubject two\n"
                 "\n"
                 "0\t0\tdocs/x.md\n"))

(test-case "git log parsing keeps parents, subjects, and binary markers"
  (define parsed (parse-git-log sample-log))
  (check-equal? (length parsed) 2)
  (check-equal? (hash-ref (first parsed) 'sha) "aaaa")
  (check-equal? (hash-ref (first parsed) 'parents) '("pppp"))
  (check-equal? (hash-ref (first parsed) 'subject) "subject one")
  (check-equal? (map (lambda (f) (hash-ref f 'path)) (hash-ref (first parsed) 'files))
                '("agent/a.rkt" "artifacts/blob.bin"))
  (check-eq? (hash-ref (second (hash-ref (first parsed) 'files)) 'added) 'binary)
  (check-equal? (hash-ref (second parsed) 'subject) "subject two"))

;; ---- manifest ----------------------------------------------------------------------

(test-case "the manifest counts every exclusion reason and every stratum"
  (define cases
    (list (classify-commit (commit #:sha "c1" #:files (list (file "agent/a.rkt"))) ctx)
          (classify-commit (commit #:sha "c2"
                                   #:files (list (file "agent/a.rkt") (file "tools/d.rkt")))
                           ctx)))
  (define excluded
    (list (classify-commit (commit #:sha "c3" #:parents (list "a" "b")) ctx)
          (classify-commit (commit #:sha "c4" #:files (list (file "docs/x.md"))) ctx)))
  (define m (corpus-manifest "revision" cases excluded))
  (check-equal? (hash-ref m 'strata) (hasheq 'S1 1 'S2 0 'S3 1))
  (check-equal? (hash-ref m 'excluded-counts) (hasheq 'merge-commit 1 'non-production 1))
  (check-equal? (hash-ref m 'revision) "revision")
  (check-equal? (length (hash-ref m 'cases)) 2)
  ;; cases are sorted by sha and carry the stratum as a string for JSON
  (check-equal? (map (lambda (c) (hash-ref c 'sha)) (hash-ref m 'cases)) '("c1" "c2"))
  (check-equal? (hash-ref (second (hash-ref m 'cases)) 'stratum) "S3"))

(test-case "the manifest is JSON-serialisable with symbol keys only"
  (define m
    (corpus-manifest "r"
                     (list (classify-commit (commit #:files (list (file "agent/a.rkt"))) ctx))
                     '()))
  (define text (with-output-to-string (lambda () (write-json m))))
  (check-true (> (string-length text) 10))
  (check-true (and (regexp-match #rx"\"revision\"" text) #t)))
