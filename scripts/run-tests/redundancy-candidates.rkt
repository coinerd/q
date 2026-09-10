#lang racket/base

;; q/scripts/run-tests/redundancy-candidates.rkt — W5 semantic-redundancy
;; detector (v1.00.28).
;;
;; Pairs test-cases that are candidates for consolidation using STATIC signals
;; only (the W0 census is provenance/enrichment, never a required input):
;;   - shared @covers identity (file-level `;; @covers` headers),
;;   - shared fixture-builder usage (`(make-…)` calls),
;;   - stimulus shape (non-assertion setup calls before the first `check-`),
;;   - assertion sets (`(check-…)` calls).
;;
;; Policy (consolidation preference order, see wave contract):
;;   1 share-setup           — shared fixture builders, different covers
;;   2 merge-complementary   — same stimulus, differing assertions
;;   3 keep-both             — differing stimulus (detection-power ambiguity;
;;                             MUST NOT resolve to delete)
;;   4 delete-with-adequacy  — same stimulus AND identical assertions
;;                             (deletion requires the adequacy artifact)
;;
;; Determinism: groups, members and evidence links are sorted; identical
;; inputs yield identical candidates JSON.
;; STABILITY: internal — CLI (`racket scripts/run-tests/redundancy-candidates.rkt`),
;; contract-tested by tests/test-redundancy-candidates.rkt.

(require racket/file
         racket/list
         racket/path
         racket/string)

(provide parse-test-file
         detect-redundancy-groups
         candidates-json
         preference-rank)

;; ---------------------------------------------------------------------------
;; Parsing
;; ---------------------------------------------------------------------------

;; Paths in records/evidence are reported relative to the scanned test root
;; (the parent of the file's directory) so evidence links read `tests/x.rkt#name`.
(define (relative-test-path path)
  ;; "…/<any-root>/tests/a.rkt" -> "tests/a.rkt"; falls back to the whole
  ;; path when there is no enclosing directory element.
  (define p (simple-form-path path))
  (define e (explode-path p))
  (if (>= (length e) 2)
      (path->string (apply build-path (list-tail e (- (length e) 2))))
      (path->string p)))

(define (tokens px s)
  (regexp-match* px s))

(define (strip-paren t)
  (substring t 1))

(define (sorted-unique xs)
  (sort (remove-duplicates xs) string<?))

(define (all-equal? xs)
  (or (null? xs) (null? (cdr xs)) (andmap (lambda (x) (equal? x (car xs))) (cdr xs))))

;; Balanced-paren slice of every top-level `(test-case …)` form. String
;; literals containing unbalanced parens are outside the heuristic scope
;; (documented simplification — the census complements this view).
(define (test-case-blocks src)
  (define starts (regexp-match-positions* #rx"\\(test-case[ \n]" src))
  (for/list ([m (in-list starts)])
    (define i (car m))
    (define n (string-length src))
    (let loop ([j i]
               [depth 0])
      (cond
        [(>= j n) (substring src i n)]
        [(zero? depth) (loop (add1 j) 1)]
        [else
         (define c (string-ref src j))
         (cond
           [(char=? c #\() (loop (add1 j) (add1 depth))]
           [(char=? c #\))
            (if (zero? (sub1 depth))
                (substring src i (add1 j))
                (loop (add1 j) (sub1 depth)))]
           [else (loop (add1 j) depth)])]))))

;; parse-test-file : path-string? -> (listof hash?)
;; One record per test-case: path, name, covers, fixtures, stimulus,
;; assertions.
(define (parse-test-file path)
  (define src (file->string path))
  (define rel (relative-test-path path))
  (define covers (regexp-match* #px";;\\s*@covers\\s+(\\S+)" src #:match-select cadr))
  (for/list ([b (in-list (test-case-blocks src))])
    (define assert-pos (regexp-match-positions #rx"\\(check-" b))
    (define before-assert
      (if assert-pos
          (substring b 0 (caar assert-pos))
          b))
    (hasheq 'path
            rel
            'name
            (let ([m (regexp-match #px"\"([^\"]+)\"" b)])
              (if m
                  (cadr m)
                  "?"))
            'covers
            covers
            'fixtures
            (sorted-unique (map strip-paren (tokens #px"\\(make-[A-Za-z0-9!?_-]+" b)))
            'stimulus
            (sorted-unique (filter (lambda (t) (not (string=? t "test-case")))
                                   (map strip-paren
                                        (tokens #rx"\\([A-Za-z0-9!?_<>=/.:+*-]+" before-assert))))
            'assertions
            (sorted-unique (map strip-paren (tokens #px"\\((check-[A-Za-z!?-]+)" b))))))

;; ---------------------------------------------------------------------------
;; Grouping + policy
;; ---------------------------------------------------------------------------

(define (record<? a b)
  (define pa (hash-ref a 'path))
  (define pb (hash-ref b 'path))
  (if (string=? pa pb)
      (string<? (hash-ref a 'name) (hash-ref b 'name))
      (string<? pa pb)))

(define (group<? a b)
  (string<? (hash-ref a 'evidence) (hash-ref b 'evidence)))

(define (evidence-line members)
  (string-join (for/list ([r (in-list members)])
                 (format "~a#~a" (hash-ref r 'path) (hash-ref r 'name)))
               "; "))

(define (same-covers-groups records)
  (define by-covers (make-hash))
  (for ([r (in-list records)])
    (define covers (hash-ref r 'covers))
    (unless (null? covers)
      (hash-update! by-covers (string-join covers " ") (lambda (old) (cons r old)) '())))
  (sort (for/list ([(key members) (in-hash by-covers)]
                   #:when (>= (length members) 2))
          (define members* (sort members record<?))
          (define stim-eq (all-equal? (map (lambda (r) (hash-ref r 'stimulus)) members*)))
          (define assert-diff (not (all-equal? (map (lambda (r) (hash-ref r 'assertions)) members*))))
          (define preference
            (cond
              [(and stim-eq assert-diff) "merge-complementary"]
              [(and stim-eq (not assert-diff)) "delete-with-adequacy"]
              [else "keep-both"]))
          (hasheq 'covers
                  (hash-ref (car members*) 'covers)
                  'members
                  members*
                  'stimulus_equal
                  stim-eq
                  'assertions_differ
                  assert-diff
                  'preference
                  preference
                  'requires_adequacy
                  (string=? preference "delete-with-adequacy")
                  'evidence
                  (evidence-line members*)))
        group<?))

(define (shared-fixture-groups records)
  (define by-fixture (make-hash))
  (for ([r (in-list records)])
    (define fx (hash-ref r 'fixtures))
    (unless (null? fx)
      (hash-update! by-fixture fx (lambda (old) (cons r old)) '())))
  (sort (for/list ([(fx members) (in-hash by-fixture)]
                   #:when
                   (>= (length (remove-duplicates (map (lambda (r) (hash-ref r 'path)) members))) 2))
          (define members* (sort members record<?))
          (hasheq 'fixture_key
                  fx
                  'covers
                  (remove-duplicates (map (lambda (r) (hash-ref r 'covers)) members*))
                  'members
                  members*
                  'preference
                  "share-setup"
                  'requires_adequacy
                  #f
                  'evidence
                  (evidence-line members*)))
        group<?))

;; detect-redundancy-groups : (listof (listof hash?)) -> hash?
;; → (hasheq 'same_covers (listof group) 'shared_fixtures (listof group))
(define (detect-redundancy-groups record-lists)
  (define records (append* record-lists))
  (hasheq 'same_covers (same-covers-groups records) 'shared_fixtures (shared-fixture-groups records)))

;; preference-rank : string? -> exact-positive-integer?
(define (preference-rank pref)
  (cond
    [(string=? pref "share-setup") 1]
    [(string=? pref "merge-complementary") 2]
    [(string=? pref "keep-both") 3]
    [(string=? pref "delete-with-adequacy") 4]
    [else (raise-argument-error 'preference-rank "known consolidation preference" pref)]))

;; ---------------------------------------------------------------------------
;; Emission
;; ---------------------------------------------------------------------------

;; candidates-json : hash? string? -> hash?
;; Deterministic jsexpr artifact; every group carries evidence links
;; (path#test-case).
(define (candidates-json groups pkg)
  (define same (hash-ref groups 'same_covers))
  (define shared (hash-ref groups 'shared_fixtures))
  (define distinct-cases
    (remove-duplicates (for*/list ([g (in-list (append same shared))]
                                   [r (in-list (hash-ref g 'members))])
                         (cons (hash-ref r 'path) (hash-ref r 'name)))))
  (hasheq 'schema
          "redundancy-candidates/1"
          'package
          pkg
          'same_covers_groups
          same
          'shared_fixture_groups
          shared
          'policy
          (hasheq 'share_setup 1 'merge_complementary 2 'keep_both 3 'delete_with_adequacy 4)
          'counts
          (hasheq 'test_cases
                  (length distinct-cases)
                  'same_covers_groups
                  (length same)
                  'shared_fixture_groups
                  (length shared))))

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(module+ main
  (require racket/cmdline
           racket/format
           json)
  (define root (make-parameter "tests"))
  (define out (make-parameter #f))
  (define census (make-parameter #f))
  (define pkg (make-parameter "unversioned"))
  (command-line #:program "redundancy-candidates"
                #:once-each
                [("--root") r "directory to scan for test-*.rkt files (default tests)" (root r)]
                [("--out") o "write the candidates JSON artifact here" (out o)]
                [("--census") c "optional W0 runtime-census JSON (provenance only)" (census c)]
                [("--package") p "package label embedded in the artifact" (pkg p)])
  (define files
    (sort (for/list ([p (in-directory (root))]
                     #:when (let ([s (path->string p)])
                              (and (string-suffix? s ".rkt")
                                   (regexp-match? #rx"(^|/)test-[^/]*\\.rkt$" s))))
            p)
          string<?
          #:key path->string))
  (define record-lists (map parse-test-file files))
  (define groups (detect-redundancy-groups record-lists))
  (define jsexpr (candidates-json groups (pkg)))
  (define jsexpr*
    (if (census)
        (hash-set jsexpr 'generated_from (hasheq 'root (root) 'census (census)))
        jsexpr))
  (cond
    [(out)
     (with-output-to-file (out)
                          (lambda ()
                            (write-json jsexpr*)
                            (newline))
                          #:exists 'replace)
     (displayln
      (format
       "redundancy-candidates: ~a test-case(s) scanned, ~a same-covers group(s), ~a shared-fixture group(s) → ~a"
       (hash-ref (hash-ref jsexpr* 'counts) 'test_cases)
       (hash-ref (hash-ref jsexpr* 'counts) 'same_covers_groups)
       (hash-ref (hash-ref jsexpr* 'counts) 'shared_fixture_groups)
       (~a (out))))]
    [else
     (write-json jsexpr*)
     (newline)]))
