#lang racket/base

;; q/scripts/impact-selector/evaluate.rkt — static, explanation-only
;; change-impact selector evaluator (v1.00.29 W7: selector governance and
;; bounded shadow design; spec PLAN-v1.00.29-PROOF-GRAPH-REDUCTION.md §6 W5,
;; §10 selector threat model, §11.3 no-silent-fallback).
;;
;; WHAT THIS TOOL IS
;;
;; A deterministic, offline decision-and-explanation engine. Inputs:
;;   (1) a diff manifest (file list + change kind, schema q.impact-diff/1);
;;   (2) the W0 proof graph (artifacts/proof-graph/v1.00.29-w0/graph.json),
;;       optionally accompanied by two sibling companions in the same
;;       directory: claims.json (claim inventory, per the graph's
;;       claims_ref) and selector-mapping.json (schema
;;       q.selector-mapping/1: coverage entries, area rules, and
;;       uncertain-source markers).
;; Output: one decision record (schema q.impact-decision/1) with
;;   decision = "broad" | "selected" | "fallback:broad" plus the §11.3
;;   explanation fields (reason, source SHA, consumer, affected claim set,
;;   zero-tests-run statement, elapsed-ms).
;;
;; WHAT THIS TOOL IS NOT
;;
;; It is NOT a test runner and is NOT wired into any CI workflow (hard
;; prohibition through W7: .github/workflows must contain no test-impact
;; execution job, new or renamed). There is NO test-executing code path:
;; no subprocess, no system call, no thread, no sleep, no network. The
;; evaluation phase executes ZERO tests; the record states tests-run = 0
;; by construction. Decisions are shadow/replay recommendations only —
;; they never gate, filter, or shrink required proof.
;;
;; FAILURE DIRECTION (§10)
;;
;; Every uncertainty class broadens: unknown mapping, dynamic require,
;; macro-generated dependency, reader/generated source, missing
;; @covers/mapping metadata, runner/helper/fixture/workflow/config
;; changes, deleted/renamed source, malformed graph, multi-area change,
;; security- and platform-sensitive paths, huge diff, budget exceedance.
;; Exceedance yields fallback:broad — NEVER "select less".
;;
;; DETERMINISM
;;
;; Same inputs yield byte-identical canonical output except the single
;; measured field elapsed-ms (never decision-relevant unless the budget
;; is actually exceeded). Dependencies: racket/base + racket/contract +
;; json only. Named contracts are bound at column 0 so the textual
;; check-deps scanner never mistakes a combinator for an external
;; package (W5 lesson).

(require json
         racket/contract)

;; ---------------------------------------------------------------------------
;; Named contracts (column 0 — check-deps false-positive discipline)
;; ---------------------------------------------------------------------------

(define jsexpr-hash-contract (hash/c symbol? jsexpr?))
(define path-or-false-contract (or/c path-string? #f))

(provide (contract-out
          (decision-schema string?)
          (diff-schema string?)
          (mapping-schema string?)
          (consumer-placeholder string?)
          (default-budget-ms exact-nonnegative-integer?)
          (hard-timeout-ms exact-nonnegative-integer?)
          (huge-diff-file-limit exact-nonnegative-integer?)
          (broadening-reasons (listof string?))
          (sha256-hex (-> bytes? string?))
          (canonical-json (-> jsexpr? string?))
          (read-file-bytes (-> path-string? bytes?))
          (read-json-file (-> path-string? jsexpr?))
          (load-diff-manifest (-> path-string? jsexpr?))
          (load-selector-graph (-> path-string? jsexpr-hash-contract))
          (classify-change (-> string? string? jsexpr-hash-contract jsexpr?))
          (evaluate-impact (->* (path-string? path-string? string? path-or-false-contract)
                                (#:budget-ms exact-nonnegative-integer?)
                                jsexpr?))
          (parse-argv (-> (listof string?) jsexpr-hash-contract))))

;; ---------------------------------------------------------------------------
;; Constants
;; ---------------------------------------------------------------------------

(define decision-schema "q.impact-decision/1")
(define diff-schema "q.impact-diff/1")
(define mapping-schema "q.selector-mapping/1")

;; §11.3 consumer field: the evaluator has NO consumer this wave. The
;; placeholder is recorded verbatim in every explanation record so a
;; future reader can never mistake a replay decision for a gated one.
(define consumer-placeholder
  "unwired-shadow-evaluator (no CI consumer; W7 hard prohibition: no test-impact job in .github/workflows)")

(define default-budget-ms 60000)
(define hard-timeout-ms 120000)
(define huge-diff-file-limit 40)

;; Docs-only suffixes: a changed file with one of these suffixes can never
;; create or invalidate a test-selection obligation; it is skipped from
;; production-area counting. A fully docs-only diff still evaluates to
;; decision "broad" — the broad tier is never skipped.
(define doc-suffixes (list ".md" ".markdown" ".rst" ".txt"))

;; Platform-sensitive areas: their proofs must never be narrowed.
(define platform-sensitive-areas (list "gui" "browser" "interfaces" "tui"))

;; Security-sensitive path prefixes: never narrow the strict-security
;; proofs; a hit broadens before any other production rule applies.
(define security-sensitive-prefixes
  (list "security/" "sandbox/" "scripts/lint-security.rkt" "scripts/lint-credential-policy.rkt"))

;; ---------------------------------------------------------------------------
;; Small string helpers (racket/base only — no racket/string dependency)
;; ---------------------------------------------------------------------------

(define (starts-with? s prefix)
  (and (<= (string-length prefix) (string-length s))
       (equal? (substring s 0 (string-length prefix)) prefix)))

(define (ends-with? s suffix)
  (define n (string-length s))
  (define m (string-length suffix))
  (and (<= m n) (equal? (substring s (- n m) n) suffix)))

(define (contains-substring? s needle)
  (let loop ([i 0])
    (cond
      [(> i (- (string-length s) (string-length needle))) #f]
      [(equal? (substring s i (+ i (string-length needle))) needle) #t]
      [else (loop (add1 i))])))

;; Stable insertion sort by key (deterministic; lists here are small).
(define (sort-by-key less? xs key)
  (let loop ([in xs]
             [out '()])
    (if (null? in)
        out
        (loop (cdr in) (insert-by less? key (key (car in)) (car in) out)))))

(define (insert-by less? key k v out)
  (cond
    [(null? out) (list v)]
    [(less? k (key (car out))) (cons v out)]
    [else (cons (car out) (insert-by less? key k v (cdr out)))]))

(define (string-list-unique xs)
  (define sorted (sort-by-key string<? xs (lambda (x) x)))
  (let loop ([in sorted]
             [out '()])
    (cond
      [(null? in) (reverse out)]
      [(null? out) (loop (cdr in) (list (car in)))]
      [(equal? (car in) (car out)) (loop (cdr in) out)]
      [else (loop (cdr in) (cons (car in) out))])))

(define (hash-keys-sorted h)
  (string-list-unique (for/list ([k (in-hash-keys h)])
                        (symbol->string k))))

(define (last-path-segment rel)
  (define n (string-length rel))
  (let loop ([i (- n 1)])
    (cond
      [(< i 0) rel]
      [(equal? (string-ref rel i) #\/) (substring rel (+ i 1) n)]
      [else (loop (- i 1))])))

;; ---------------------------------------------------------------------------
;; File I/O (ports only — no racket/file dependency)
;; ---------------------------------------------------------------------------

(define (read-file-bytes path)
  (call-with-input-file* path
                         (lambda (in)
                           (let loop ([acc '()])
                             (define b (read-byte in))
                             (if (eof-object? b)
                                 (list->bytes (reverse acc))
                                 (loop (cons b acc)))))))

(define (read-json-file path)
  (call-with-input-file* path read-json))

;; ---------------------------------------------------------------------------
;; Canonical JSON (sorted keys, deterministic)
;; ---------------------------------------------------------------------------

(define (canonical-json v)
  (cond
    [(hash? v)
     (string-append
      "{"
      (let loop ([ks (hash-keys-sorted v)]
                 [first? #t]
                 [acc ""])
        (if (null? ks)
            acc
            (loop (cdr ks)
                  #f
                  (string-append acc
                                 (if first? "" ",")
                                 (canonical-json (car ks))
                                 ":"
                                 (canonical-json (hash-ref v (string->symbol (car ks))))))))
      "}")]
    [(list? v)
     (string-append
      "["
      (let loop ([xs v]
                 [first? #t]
                 [acc ""])
        (if (null? xs)
            acc
            (loop (cdr xs) #f (string-append acc (if first? "" ",") (canonical-json (car xs))))))
      "]")]
    [(string? v)
     (string-append "\""
                    (let ([n (string-length v)])
                      (let loop ([i 0]
                                 [acc ""])
                        (if (>= i n)
                            acc
                            (let ([c (string-ref v i)])
                              (loop (add1 i)
                                    (cond
                                      [(equal? c #\") (string-append acc "\\\"")]
                                      [(equal? c #\\) (string-append acc "\\\\")]
                                      [(equal? c #\newline) (string-append acc "\\n")]
                                      [(equal? c #\return) (string-append acc "\\r")]
                                      [(equal? c #\tab) (string-append acc "\\t")]
                                      [else
                                       (define code (char->integer c))
                                       (if (< code 32)
                                           (string-append acc "\\u" (hex4 code))
                                           (string-append acc (string c)))]))))))
                    "\"")]
    [(boolean? v) (if v "true" "false")]
    [(null? v) "null"]
    [(exact-integer? v) (number->string v)]
    [(real? v) (format "~a" v)]
    [(symbol? v) (canonical-json (symbol->string v))]
    [else (canonical-json (format "~a" v))]))

(define (hex-digit n)
  (string-ref "0123456789abcdef" n))

(define (hex4 n)
  (string (hex-digit (arithmetic-shift n -12))
          (hex-digit (bitwise-and (arithmetic-shift n -8) 15))
          (hex-digit (bitwise-and (arithmetic-shift n -4) 15))
          (hex-digit (bitwise-and n 15))))

(define (hex8 n)
  (string (hex-digit (arithmetic-shift n -28))
          (hex-digit (bitwise-and (arithmetic-shift n -24) 15))
          (hex-digit (bitwise-and (arithmetic-shift n -20) 15))
          (hex-digit (bitwise-and (arithmetic-shift n -16) 15))
          (hex-digit (bitwise-and (arithmetic-shift n -12) 15))
          (hex-digit (bitwise-and (arithmetic-shift n -8) 15))
          (hex-digit (bitwise-and (arithmetic-shift n -4) 15))
          (hex-digit (bitwise-and n 15))))

;; ---------------------------------------------------------------------------
;; SHA-256 (FIPS 180-4, exact integer arithmetic, small inputs only)
;; ---------------------------------------------------------------------------

(define sha256-k
  (list->vector '(#x428a2f98 #x71374491
                             #xb5c0fbcf
                             #xe9b5dba5
                             #x3956c25b
                             #x59f111f1
                             #x923f82a4
                             #xab1c5ed5
                             #xd807aa98
                             #x12835b01
                             #x243185be
                             #x550c7dc3
                             #x72be5d74
                             #x80deb1fe
                             #x9bdc06a7
                             #xc19bf174
                             #xe49b69c1
                             #xefbe4786
                             #x0fc19dc6
                             #x240ca1cc
                             #x2de92c6f
                             #x4a7484aa
                             #x5cb0a9dc
                             #x76f988da
                             #x983e5152
                             #xa831c66d
                             #xb00327c8
                             #xbf597fc7
                             #xc6e00bf3
                             #xd5a79147
                             #x06ca6351
                             #x14292967
                             #x27b70a85
                             #x2e1b2138
                             #x4d2c6dfc
                             #x53380d13
                             #x650a7354
                             #x766a0abb
                             #x81c2c92e
                             #x92722c85
                             #xa2bfe8a1
                             #xa81a664b
                             #xc24b8b70
                             #xc76c51a3
                             #xd192e819
                             #xd6990624
                             #xf40e3585
                             #x106aa070
                             #x19a4c116
                             #x1e376c08
                             #x2748774c
                             #x34b0bcb5
                             #x391c0cb3
                             #x4ed8aa4a
                             #x5b9cca4f
                             #x682e6ff3
                             #x748f82ee
                             #x78a5636f
                             #x84c87814
                             #x8cc70208
                             #x90befffa
                             #xa4506ceb
                             #xbef9a3f7
                             #xc67178f2)))

(define sha256-h0
  (list->vector
   '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))

(define (m32 x)
  (bitwise-and x #xffffffff))

(define (rotr32 x n)
  (bitwise-ior (arithmetic-shift x (- n)) (arithmetic-shift x (- 32 n))))

;; Producers of replay artifacts and tests share this one implementation.
(define (sha256-hex bstr)
  (define msg-len (bytes-length bstr))
  (define bit-len (* 8 msg-len))
  (define zeros (modulo (- 55 msg-len) 64))
  (define total (+ msg-len 1 zeros 8))
  (define buf (make-bytes total 0))
  (bytes-copy! buf 0 bstr)
  (bytes-set! buf msg-len #x80)
  (for ([i (in-range 8)])
    (bytes-set! buf (- total 1 i) (bitwise-and (arithmetic-shift bit-len (* -8 i)) #xff)))
  (define h (list->vector (vector->list sha256-h0)))
  (for ([off (in-range 0 total 64)])
    (define w (make-vector 64 0))
    (for ([i (in-range 16)])
      (define j (+ off (* 4 i)))
      (vector-set! w
                   i
                   (bitwise-ior (arithmetic-shift (bytes-ref buf j) 24)
                                (arithmetic-shift (bytes-ref buf (+ j 1)) 16)
                                (arithmetic-shift (bytes-ref buf (+ j 2)) 8)
                                (bytes-ref buf (+ j 3)))))
    (for ([i (in-range 16 64)])
      (define w15 (vector-ref w (- i 15)))
      (define s0 (bitwise-xor (rotr32 w15 7) (rotr32 w15 18) (arithmetic-shift w15 -3)))
      (define w2 (vector-ref w (- i 2)))
      (define s1 (bitwise-xor (rotr32 w2 17) (rotr32 w2 19) (arithmetic-shift w2 -10)))
      (vector-set! w i (m32 (+ (vector-ref w (- i 16)) s0 (vector-ref w (- i 7)) s1))))
    (define-values (a b c d e f g hh)
      (for/fold ([a (vector-ref h 0)]
                 [b (vector-ref h 1)]
                 [c (vector-ref h 2)]
                 [d (vector-ref h 3)]
                 [e (vector-ref h 4)]
                 [f (vector-ref h 5)]
                 [g (vector-ref h 6)]
                 [hh (vector-ref h 7)])
                ([i (in-range 64)])
        (define S1 (bitwise-xor (rotr32 e 6) (rotr32 e 11) (rotr32 e 25)))
        (define ch (bitwise-xor (bitwise-and e f) (bitwise-and (bitwise-not (m32 e)) g)))
        (define temp1 (m32 (+ hh S1 ch (vector-ref sha256-k i) (vector-ref w i))))
        (define S0 (bitwise-xor (rotr32 a 2) (rotr32 a 13) (rotr32 a 22)))
        (define maj (bitwise-xor (bitwise-and a b) (bitwise-and a c) (bitwise-and b c)))
        (define temp2 (m32 (+ S0 maj)))
        (values (m32 (+ temp1 temp2)) a b c (m32 (+ d temp1)) e f g)))
    (vector-set! h 0 (m32 (+ (vector-ref h 0) a)))
    (vector-set! h 1 (m32 (+ (vector-ref h 1) b)))
    (vector-set! h 2 (m32 (+ (vector-ref h 2) c)))
    (vector-set! h 3 (m32 (+ (vector-ref h 3) d)))
    (vector-set! h 4 (m32 (+ (vector-ref h 4) e)))
    (vector-set! h 5 (m32 (+ (vector-ref h 5) f)))
    (vector-set! h 6 (m32 (+ (vector-ref h 6) g)))
    (vector-set! h 7 (m32 (+ (vector-ref h 7) hh))))
  (apply string-append
         (for/list ([i (in-range 8)])
           (hex8 (vector-ref h i)))))

;; ---------------------------------------------------------------------------
;; Input loading and validation (fail-closed to named broadening reasons)
;; ---------------------------------------------------------------------------

(define (load-diff-manifest path)
  (define raw (read-json-file path))
  (when (not (hash? raw))
    (error 'load-diff-manifest "diff manifest is not a JSON object"))
  (when (not (equal? (hash-ref raw 'schema #f) diff-schema))
    (error 'load-diff-manifest "diff manifest schema mismatch (expected ~a)" diff-schema))
  (define changes (hash-ref raw 'changes #f))
  (when (not (list? changes))
    (error 'load-diff-manifest "diff manifest has no changes list"))
  (for ([c changes])
    (when (not (and (hash? c)
                    (string? (hash-ref c 'path #f))
                    (member (hash-ref c 'change #f) '("modified" "added" "deleted" "renamed"))))
      (error 'load-diff-manifest "diff manifest change entry malformed")))
  raw)

(define (load-selector-graph path)
  (define raw (read-json-file path))
  (when (not (hash? raw))
    (error 'load-selector-graph "graph is not a JSON object"))
  raw)

;; ---------------------------------------------------------------------------
;; Classification (§10 classes -> named reasons; first deterministic hit wins)
;; ---------------------------------------------------------------------------

;; Every reason that forces the decision to the broad tier. A reason is
;; "broadening" iff it appears in this list; selection is only possible
;; when no change produces one of these.
(define broadening-reasons
  (list "empty-diff"
        "workflow-change"
        "config-or-dependency-metadata"
        "runner-change"
        "runner-helper-change"
        "helper-change"
        "fixture-change"
        "dynamic-require"
        "macro-generated-dependency"
        "generated-source"
        "missing-mapping-metadata"
        "deleted-or-renamed-source"
        "multi-area-change"
        "security-sensitive-path"
        "platform-sensitive-path"
        "unknown-path"
        "huge-diff"
        "graph-parse-failure"
        "missing-claims-metadata"
        "invalid-diff-manifest"
        "budget-exceeded"))

(define (path-area rel rules)
  (let loop ([rs rules])
    (cond
      [(null? rs) #f]
      [(starts-with? rel (format "~a" (hash-ref (car rs) 'prefix)))
       (format "~a" (hash-ref (car rs) 'area))]
      [else (loop (cdr rs))])))

(define (doc-file? rel)
  (let loop ([sfx doc-suffixes])
    (cond
      [(null? sfx) #f]
      [(ends-with? rel (car sfx)) #t]
      [else (loop (cdr sfx))])))

(define (workflow-file? rel)
  (starts-with? rel ".github/workflows/"))

(define (security-sensitive? rel)
  (let loop ([ps security-sensitive-prefixes])
    (cond
      [(null? ps) #f]
      [(starts-with? rel (car ps)) #t]
      [else (loop (cdr ps))])))

(define (fixture-file? rel)
  (or (contains-substring? rel "/fixtures/")
      (contains-substring? rel "/golden/")
      (contains-substring? rel "/testdata/")))

(define (test-file? rel)
  (or (starts-with? rel "tests/test-") (contains-substring? rel "/test-")))

(define (under-tests? rel)
  (starts-with? rel "tests/"))

(define (generated-file? rel)
  (or (contains-substring? rel "/generated/") (contains-substring? rel "/gen/")))

(define (config-metadata-file? rel)
  (or (equal? rel "info.rkt")
      (equal? (last-path-segment rel) "info.rkt")
      (ends-with? rel ".rktd")
      (starts-with? rel ".github/")
      (starts-with? rel "config/")
      (starts-with? rel "pkg/")))

(define (runner-file? rel)
  (or (equal? rel "scripts/run-tests.rkt") (starts-with? rel "scripts/run-tests/")))

(define (source-suffix? rel)
  (or (ends-with? rel ".rkt") (ends-with? rel ".rktl") (ends-with? rel ".ss")))

;; classify-change : path change-kind mapping -> per-file decision hash.
;; Rules run in a FIXED order; the first hit wins (documented in the
;; governance report so replay classification is reproducible).
;; Outcome is one of "selected-contribution", "broaden", "docs-no-impact".
(define (classify-change rel change mapping)
  (define rules (hash-ref mapping 'area_rules '()))
  (define uncertain (hash-ref mapping 'uncertain_sources (hash)))
  (define area (path-area rel rules))
  (define (result outcome reason area* class [extra (hash)])
    (for/fold ([h (hash 'path
                        rel
                        'change
                        change
                        'outcome
                        outcome
                        'reason
                        reason
                        'area
                        (or area* "none")
                        'reason-class
                        class)])
              ([k (in-hash-keys extra)])
      (hash-set h k (hash-ref extra k))))
  (cond
    [(member change '("deleted" "renamed"))
     (result "broaden" "deleted-or-renamed-source" area "s10-deleted-or-renamed-source")]
    [(doc-file? rel) (result "docs-no-impact" "docs-file" area "not-a-threat-class")]
    [(workflow-file? rel) (result "broaden" "workflow-change" area "s10-changed-workflow")]
    [(security-sensitive? rel)
     (result "broaden" "security-sensitive-path" area "s10-security-sensitive-path")]
    [(config-metadata-file? rel)
     (result "broaden"
             "config-or-dependency-metadata"
             area
             "s10-changed-package-dependency-metadata")]
    [(runner-file? rel) (result "broaden" "runner-change" area "s10-changed-runner")]
    [(starts-with? rel "scripts/")
     (result "broaden" "runner-helper-change" area "s10-changed-runner")]
    [(equal? area #f) (result "broaden" "unknown-path" area "s10-unknown-path")]
    [(and (under-tests? rel) (fixture-file? rel))
     (result "broaden" "fixture-change" area "s10-changed-fixture-helper")]
    [(and (under-tests? rel) (not (test-file? rel)))
     ;; A shared test helper: statically unknowable which other test files
     ;; require it, so it always broadens (s10 changed test helper).
     (result "broaden" "helper-change" area "s10-changed-test-helper")]
    [(platform-sensitive-area? area)
     (result "broaden" "platform-sensitive-path" area "s10-platform-sensitive-path")]
    [(generated-file? rel) (result "broaden" "generated-source" area "s10-reader-generated-source")]
    [(hash-ref uncertain (string->symbol rel) #f)
     =>
     (lambda (marker)
       (cond
         [(equal? marker "dynamic-require")
          (result "broaden" "dynamic-require" area "s10-dynamic-require")]
         [(equal? marker "macro-generated")
          (result "broaden" "macro-generated-dependency" area "s10-macro-generated-dependency")]
         [else (result "broaden" "generated-source" area "s10-reader-generated-source")]))]
    ;; The changed test file itself joins the selection set: running a
    ;; changed test is the minimal correct obligation (never narrower).
    [(test-file? rel) (result "selected-contribution" "changed-test-file" area "selectable")]
    [(source-suffix? rel)
     (define covering
       (let loop ([es (hash-ref mapping 'coverage '())])
         (cond
           [(null? es) '()]
           [(member rel (hash-ref (car es) 'covers '()))
            (cons (hash-ref (car es) 'test) (loop (cdr es)))]
           [else (loop (cdr es))])))
     (cond
       [(null? covering) (result "broaden" "missing-mapping-metadata" area "s10-missing-covers")]
       [else
        (result "selected-contribution"
                "mapped-source"
                area
                "selectable"
                (hash 'covered-by (string-list-unique covering)))])]
    [else (result "broaden" "missing-mapping-metadata" area "s10-missing-covers")]))

(define (platform-sensitive-area? area)
  (and (member area platform-sensitive-areas) #t))

;; ---------------------------------------------------------------------------
;; Claim-set derivation. §11.3: every explanation carries the affected
;; claim set. A graph whose claims inventory is unavailable (no sibling
;; claims.json, or no parseable claims) broadens with
;; missing-claims-metadata — job-granularity stand-ins are not accepted.
;; ---------------------------------------------------------------------------

(define (claims-from-json claims-json)
  (for/list ([c (hash-ref claims-json 'claims '())]
             #:when (and (hash? c) (string? (hash-ref c 'claim_id #f))))
    c))

(define (claims-loaded? claims-json)
  (pair? (claims-from-json claims-json)))

(define (test-bearing-claim? c)
  (define pt (format "~a" (hash-ref c 'proof_type "")))
  (or (starts-with? pt "test") (starts-with? pt "suite")))

(define (graph-workflow-jobs graph-raw wf-id)
  (string-list-unique (for/list ([j (hash-ref (hash-ref graph-raw 'nodes (hash)) 'jobs '())]
                                 #:when (equal? (hash-ref j 'workflow #f) wf-id))
                        (format "~a" (hash-ref j 'id "")))))

(define (affected-claims-for-workflow claims-json graph-raw wf-id)
  (define wf-jobs (graph-workflow-jobs graph-raw wf-id))
  (string-list-unique (for/list ([c (claims-from-json claims-json)]
                                 #:when (ormap (lambda (p) (member (format "~a" p) wf-jobs))
                                               (hash-ref c 'produced_by '())))
                        (hash-ref c 'claim_id))))

(define (affected-claims-test-surface claims-json)
  (string-list-unique (for/list ([c (claims-from-json claims-json)]
                                 #:when (test-bearing-claim? c))
                        (hash-ref c 'claim_id))))

(define (all-claim-ids claims-json)
  (string-list-unique (for/list ([c (claims-from-json claims-json)])
                        (hash-ref c 'claim_id))))

(define (workflow-id-for-file graph-raw rel)
  (let loop ([wfs (hash-ref (hash-ref graph-raw 'nodes (hash)) 'workflows '())])
    (cond
      [(null? wfs) #f]
      [(equal? (hash-ref (car wfs) 'file #f) rel) (format "~a" (hash-ref (car wfs) 'id))]
      [else (loop (cdr wfs))])))

;; ---------------------------------------------------------------------------
;; Record assembly (canonical field order comes from the JSON key sort)
;; ---------------------------------------------------------------------------

(define (finalize-record decision
                         reason
                         reason-class
                         affected
                         graph-digest
                         source-sha
                         selected-tests
                         budget-ok
                         budget
                         elapsed
                         fallback-chain)
  (hash 'schema
        decision-schema
        'decision
        decision
        'reason
        reason
        'reason-class
        reason-class
        'source-sha
        source-sha
        'graph-sha256
        graph-digest
        'consumer
        consumer-placeholder
        'affected-claim-set
        affected
        'tests-run
        0
        'zero-test-execution
        #t
        'selected
        (if (null? selected-tests)
            '()
            (hash 'tests
                  selected-tests
                  'suites
                  (if (equal? decision "selected")
                      (list "fast")
                      '())))
        'budget-ms
        budget
        'budget-hard-timeout-ms
        hard-timeout-ms
        'budget-ok
        budget-ok
        'elapsed-ms
        (inexact->exact (floor (max 0 elapsed)))
        'fallback-chain
        fallback-chain))

(define (per-file->fallback-entry r)
  (hash 'path
        (hash-ref r 'path)
        'change
        (hash-ref r 'change)
        'outcome
        (hash-ref r 'outcome)
        'reason
        (hash-ref r 'reason)))

;; ---------------------------------------------------------------------------
;; Core evaluation. Stages run under a budget checkpoint discipline; an
;; exceeded budget escapes to a fallback:broad record via an escape
;; continuation (no threads, no timers, no sleeps).
;; ---------------------------------------------------------------------------

(define (evaluate-impact diff-path
                         graph-path
                         source-sha
                         out-path
                         #:budget-ms [budget-arg default-budget-ms])
  (define t0 (current-inexact-milliseconds))
  (define budget (min budget-arg hard-timeout-ms))
  (define graph-bytes (read-file-bytes graph-path))
  (define graph-digest (sha256-hex graph-bytes))
  (call-with-current-continuation
   (lambda (escape)
     (define (elapsed)
       (- (current-inexact-milliseconds) t0))
     (define (check-budget! stage)
       (when (> (elapsed) budget)
         (escape (finalize-record
                  "fallback:broad"
                  "budget-exceeded"
                  "s10-timeout-resource-budget-exceedance"
                  '()
                  graph-digest
                  source-sha
                  '()
                  #f
                  budget
                  (elapsed)
                  (list (hash 'stage stage 'elapsed-ms (inexact->exact (floor (elapsed)))))))))
     (define (escape-reason reason reason-class detail)
       (escape (finalize-record "fallback:broad"
                                reason
                                reason-class
                                '()
                                graph-digest
                                source-sha
                                '()
                                #t
                                budget
                                (elapsed)
                                (if detail
                                    (list (hash 'detail detail))
                                    '()))))
     (check-budget! "load-diff")
     (define diff-raw
       (with-handlers ([exn:fail? (lambda (e)
                                    (escape-reason "invalid-diff-manifest"
                                                   "s10-malformed-input"
                                                   (exn-message e)))])
         (load-diff-manifest diff-path)))
     (check-budget! "load-graph")
     (define graph-raw
       (with-handlers ([exn:fail? (lambda (e)
                                    (escape-reason "graph-parse-failure"
                                                   "s10-malformed-dependency-graph"
                                                   (exn-message e)))])
         (load-selector-graph graph-path)))
     (check-budget! "companions")
     (define-values (graph-dir _gname _gdir?) (split-path graph-path))
     (define claims-path (build-path graph-dir "claims.json"))
     (define claims-json
       (if (file-exists? claims-path)
           (with-handlers ([exn:fail? (lambda (_) (hash))])
             (read-json-file claims-path))
           (hash)))
     (define mapping-path (build-path graph-dir "selector-mapping.json"))
     (define mapping
       (if (file-exists? mapping-path)
           (with-handlers ([exn:fail? (lambda (_) (hash))])
             (read-json-file mapping-path))
           (hash)))
     (check-budget! "classify")
     (define changes
       (sort-by-key string<?
                    (hash-ref diff-raw 'changes '())
                    (lambda (c) (format "~a" (hash-ref c 'path "")))))
     (cond
       [(null? changes)
        (finalize-record "broad"
                         "empty-diff"
                         "s10-empty-diff"
                         (if (claims-loaded? claims-json)
                             (all-claim-ids claims-json)
                             '())
                         graph-digest
                         source-sha
                         '()
                         #t
                         budget
                         (elapsed)
                         '())]
       [(not (claims-loaded? claims-json))
        (finalize-record
         "fallback:broad"
         "missing-claims-metadata"
         "s10-missing-metadata"
         '()
         graph-digest
         source-sha
         '()
         #t
         budget
         (elapsed)
         (list (hash 'detail "graph claims inventory unavailable; affected claim set unknowable")))]
       [else
        (define per-file
          (for/list ([c changes])
            (classify-change (format "~a" (hash-ref c 'path))
                             (format "~a" (hash-ref c 'change))
                             mapping)))
        (define non-docs
          (filter (lambda (r) (not (equal? (hash-ref r 'outcome) "docs-no-impact"))) per-file))
        (define broadenings (filter (lambda (r) (equal? (hash-ref r 'outcome) "broaden")) non-docs))
        (define areas
          (string-list-unique (for/list ([r non-docs]
                                         #:when (not (equal? (hash-ref r 'area) "tests"))
                                         #:when (not (equal? (hash-ref r 'area) "none")))
                                (hash-ref r 'area))))
        (define affected
          (cond
            [(ormap (lambda (r) (equal? (hash-ref r 'reason) "workflow-change")) per-file)
             (apply append
                    (for/list ([r per-file]
                               #:when (equal? (hash-ref r 'reason) "workflow-change"))
                      (define wf-id (workflow-id-for-file graph-raw (hash-ref r 'path)))
                      (if wf-id
                          (affected-claims-for-workflow claims-json graph-raw wf-id)
                          '())))]
            [(ormap (lambda (r) (equal? (hash-ref r 'outcome) "selected-contribution")) per-file)
             (affected-claims-test-surface claims-json)]
            [else (all-claim-ids claims-json)]))
        (cond
          [(null? non-docs)
           (finalize-record "broad"
                            "docs-only-change"
                            "not-a-threat-class"
                            affected
                            graph-digest
                            source-sha
                            '()
                            #t
                            budget
                            (elapsed)
                            '())]
          [(> (length non-docs) huge-diff-file-limit)
           (finalize-record "fallback:broad"
                            "huge-diff"
                            "s10-huge-diff"
                            affected
                            graph-digest
                            source-sha
                            '()
                            #t
                            budget
                            (elapsed)
                            '())]
          [(> (length areas) 1)
           (finalize-record "fallback:broad"
                            "multi-area-change"
                            "s10-multi-area-change"
                            affected
                            graph-digest
                            source-sha
                            '()
                            #t
                            budget
                            (elapsed)
                            '())]
          [(pair? broadenings)
           (define first-broaden (car broadenings))
           (finalize-record "fallback:broad"
                            (hash-ref first-broaden 'reason)
                            (hash-ref first-broaden 'reason-class)
                            affected
                            graph-digest
                            source-sha
                            '()
                            #t
                            budget
                            (elapsed)
                            (map per-file->fallback-entry per-file))]
          [else
           (define selected-tests
             (string-list-unique
              (apply append
                     (for/list ([r per-file])
                       (cond
                         [(equal? (hash-ref r 'reason) "changed-test-file") (list (hash-ref r 'path))]
                         [(hash-has-key? r 'covered-by) (hash-ref r 'covered-by)]
                         [else '()])))))
           (finalize-record "selected"
                            "mapped-single-area"
                            "selectable"
                            affected
                            graph-digest
                            source-sha
                            selected-tests
                            #t
                            budget
                            (elapsed)
                            '())])]))))

;; ---------------------------------------------------------------------------
;; CLI (module+ main). Bad invocation -> exit 2 (usage error). Bad data ->
;; fallback:broad decision record: the tool never crashes into silence; it
;; fails open into a broad recommendation with a named reason.
;; ---------------------------------------------------------------------------

(define (parse-argv argv)
  (let loop ([xs argv]
             [acc (hash)])
    (cond
      [(null? xs) acc]
      [(null? (cdr xs)) (error 'parse-argv "missing value for flag ~a" (car xs))]
      [else
       (define flag (car xs))
       (define key
         (cond
           [(equal? flag "--diff-manifest") 'diff-manifest]
           [(equal? flag "--graph") 'graph]
           [(equal? flag "--source-sha") 'source-sha]
           [(equal? flag "--out") 'out]
           [(equal? flag "--budget-ms") 'budget-ms]
           [else (error 'parse-argv "unknown flag ~a" flag)]))
       (loop (cddr xs) (hash-set acc key (cadr xs)))])))

(define (write-decision-record! record out-path)
  (call-with-output-file* out-path
                          #:exists 'truncate/replace
                          (lambda (o)
                            (display (canonical-json record) o)
                            (newline o))))

(module+ main
  (with-handlers
      ([exn:fail?
        (lambda (e)
          (eprintf
           "usage: racket scripts/impact-selector/evaluate.rkt --diff-manifest F --graph F --source-sha S --out F [--budget-ms N]~n")
          (eprintf "error: ~a~n" (exn-message e))
          (exit 2))])
    (define opts (parse-argv (vector->list (current-command-line-arguments))))
    (define diff (hash-ref opts 'diff-manifest))
    (define graph (hash-ref opts 'graph))
    (define sha (hash-ref opts 'source-sha "unknown"))
    (define out (hash-ref opts 'out))
    (define budget
      (let ([n (string->number (hash-ref opts 'budget-ms "60000"))]) (or n default-budget-ms)))
    (define record (evaluate-impact diff graph sha out #:budget-ms budget))
    (write-decision-record! record out)
    (eprintf "q.impact-decision/1: ~a (~a) tests-run=~a elapsed=~ams~n"
             (hash-ref record 'decision)
             (hash-ref record 'reason)
             (hash-ref record 'tests-run)
             (hash-ref record 'elapsed-ms))))
