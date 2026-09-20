#lang racket/base

;; @speed fast
;; @suite extensions
;; @boundary integration

;; This test deliberately declares NO @covers: it exercises no production module.
;; It freezes the v1.00.31 contract artifacts (failure register, red-fixture
;; reproductions, contract document) and cross-checks them against each other and
;; against the committed raw plan excerpt. Claiming @covers here would create a
;; false impact link in tests/.coverage-manifest.json.

;; Register harness for the frozen v1.00.31 wave-delivery integrity register.
;;
;; W0 deliverable (task 3): a harness that runs each register row's fixture in
;; `expect-refused` mode and reports per-row status, so W6 can re-run it against
;; injected defects.
;;
;; Status vocabulary (deliberately has no "ok"):
;;   guard-status   'unguarded | 'guarded-pass | 'guarded-fail
;;   fixture-status 'reproduced | 'not-reproduced | 'refused | 'not-refused | 'skipped
;;
;; A row with no registered guard reports `unguarded` and is NEVER reported as
;; passing; while unguarded, the row's fixture is checked for reproduction
;; liveness (the defect must still be observable). W1-W5 register one guard per
;; mode with `register-guard!`; from then on the row is exercised through the
;; guard instead, which is what W6 replays against injected defects.
;;
;; This wave changes no production behaviour: no guard is registered here and no
;; green claim is made about any guard.

(require rackunit
         racket/file
         racket/hash
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         racket/system
         json
         (file "../util/json/checksum.rkt"))

(define-runtime-path q-root "..")
(define-runtime-path register-path
                     "../artifacts/wave-delivery-integrity/v1.00.31-w0/failure-register.json")
(define-runtime-path reproduction-path
                     "../artifacts/wave-delivery-integrity/v1.00.31-w0/w4-reproduction.json")
(define-runtime-path
 raw-register-path
 "../artifacts/wave-delivery-integrity/v1.00.31-w0/raw/plan-failure-mode-register.txt")
(define-runtime-path sums-path "../artifacts/wave-delivery-integrity/v1.00.31-w0/SHA256SUMS")
(define-runtime-path contract-doc-path "../docs/reports/WAVE-DELIVERY-INTEGRITY-CONTRACT-v1.00.31.md")

(define (read-json-file p)
  (with-input-from-file p (lambda () (read-json))))

(define register (read-json-file register-path))
(define reproduction (read-json-file reproduction-path))
(define register-rows (hash-ref register 'rows))
(define reproduction-rows (hash-ref reproduction 'reproductions))

(define (row-ref r k [default #f])
  (hash-ref r k default))

;; the four contract columns of a register row, used to prove verbatim agreement.
(define (row-fields r)
  (list (row-ref r 'mode "")
        (row-ref r 'structural-fix "")
        (row-ref r 'owning-wave "")
        (row-ref r 'refusal "")))

(define (repro-for mode)
  (for/first ([r (in-list reproduction-rows)]
              #:when (equal? (row-ref r 'mode) mode))
    r))

;; ============================================================
;; Guard registry (populated by W1-W5; empty at W0)
;; ============================================================

(define guards (make-hash))

;; register-guard! : string? (-> (or/c 'refused 'not-refused)) -> void?
;; A guard receives the reproduction row and reports whether the injected defect
;; was refused. W6 calls the harness in expect-refused mode to replay them.
(define (register-guard! mode proc)
  (hash-set! guards mode proc))
(provide register-guard!)

;; ============================================================
;; Executable fixture: F1 declared producer invocation
;; ============================================================

(define (run-capture argv)
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-output-port out]
                   [current-error-port err])
      (with-handlers ([exn:fail? (lambda (e) 127)])
        (apply system*/exit-code argv))))
  (values code (get-output-string out) (get-output-string err)))

;; The exact shape the W4 branch declared in
;; .github/actions/prepare-racket-environment/action.yml:146-149.
(define (f1-declared-invocation-reproduced?)
  (define script (path->string (build-path q-root "scripts/ci/compiled-root.rkt")))
  (define racket-exe (or (find-executable-path "racket") "racket"))
  (define-values (code _out err)
    (run-capture (list racket-exe
                       script
                       "build"
                       "--out"
                       "/tmp/q-w0-repro/compiled-root"
                       "--checkout"
                       (path->string (current-directory))
                       "--trusted-label"
                       "q-trusted-producer")))
  (and (not (zero? code)) (regexp-match? #rx"unknown switch: --out" err)))

;; ============================================================
;; Reproduction-liveness checks for unguarded rows
;; ============================================================

;; Reproduction fixtures F2-F10 are RECORDED inputs, not recomputations: they
;; reference an unmerged W4 branch (ce13d038) and live GitHub API responses that
;; do not exist in the checked-out tree, so re-deriving them in CI is impossible
;; by design. F1 is the exception and is executed live below. W6 replays all of
;; them as injections through the guards W1-W5 register; the raw inputs are kept
;; under artifacts/wave-delivery-integrity/v1.00.31-w0/raw/ for offline replay.
(define (hex64? s)
  (and (string? s) (regexp-match? #px"^[0-9a-f]{64}$" s)))

(define (f2-reproduced? r)
  (define computed (row-ref r 'computed))
  (define authored (row-ref r 'authored))
  (and (hex64? computed) (hex64? authored) (not (equal? computed authored))))

(define (f3-reproduced? r)
  (not (equal? (row-ref r 'recorded-implementation-sha) (row-ref r 'actual-head))))

(define (f4-reproduced? r)
  (define paths (row-ref r 'non-evidence-paths '()))
  (and (pair? paths) (member "README.md" paths)))

(define (f5-reproduced? r)
  (equal? 0 (row-ref r 'matching-refs -1)))

(define (f6-reproduced? r)
  (and (null? (row-ref r 'tool-form-result '())) (pair? (row-ref r 'api-form-result '()))))

(define (f7-reproduced? r)
  (and (pair? (row-ref r 'stale-head-pins '()))
       (for/or ([t (in-list (row-ref r 'timing-disagreement '()))])
         (not (row-ref t 'agree #t)))))

(define (f8-reproduced? r)
  (string-contains? (row-ref r 'surfaced-to-operator "") "git, exit 128"))

(define (f9-reproduced? r)
  (define pre (row-ref r 'observed-pre-correction #f))
  (and (hash? pre)
       (string-contains? (row-ref pre 'plan-index-row "") "[DONE] W4")
       (equal? "Status: DONE" (row-ref pre 'wave-doc-header))
       (equal? "done" (row-ref pre 'campaign-record-wave-4))
       (equal? "delivery-pending" (row-ref pre 'delivery-journal))))

(define (f10-reproduced? r)
  (eq? #t (row-ref (row-ref r 'observed #f) 'ledger-leads-record)))

(define liveness-checks
  (hash "F1"
        (lambda (_r) (f1-declared-invocation-reproduced?))
        "F2"
        f2-reproduced?
        "F3"
        f3-reproduced?
        "F4"
        f4-reproduced?
        "F5"
        f5-reproduced?
        "F6"
        f6-reproduced?
        "F7"
        f7-reproduced?
        "F8"
        f8-reproduced?
        "F9"
        f9-reproduced?
        "F10"
        f10-reproduced?))

;; ============================================================
;; Harness
;; ============================================================

(struct row-result (mode owning-wave guard-status fixture-status) #:transparent)

;; run-register : -> (listof row-result)
;;
;; The harness semantics ARE expect-refused mode, so there is no separate mode
;; switch to get wrong: a row with a registered guard is evaluated through that
;; guard and the guard must report 'refused (any other outcome is a failure); a
;; row with no registered guard reports 'unguarded together with its
;; reproduction-liveness result. W6 therefore replays a wave merely by
;; registering the wave's guards with `register-guard!` and calling this.
(define (run-register)
  (for/list ([row (in-list register-rows)])
    (define id (row-ref row 'id))
    (define repro (repro-for id))
    (define guard (hash-ref guards id #f))
    (cond
      [guard
       (define outcome (guard repro))
       (row-result id
                   (row-ref row 'owning-wave)
                   (if (eq? outcome 'refused) 'guarded-pass 'guarded-fail)
                   outcome)]
      [else
       (define check (hash-ref liveness-checks id #f))
       (define reproduced? (and check (check repro)))
       (row-result id
                   (row-ref row 'owning-wave)
                   'unguarded
                   (if reproduced? 'reproduced 'not-reproduced))])))

(provide run-register
         row-result
         row-result-mode
         row-result-owning-wave
         row-result-guard-status
         row-result-fixture-status
         register-rows
         reproduction-rows
         repro-for)

;; ============================================================
;; Tests
;; ============================================================

(define results (run-register))

(define (statuses)
  (map row-result-guard-status results))

(test-case "register is the frozen F1-F10 set"
  (check-equal? (map (lambda (r) (row-ref r 'id)) register-rows)
                '("F1" "F2" "F3" "F4" "F5" "F6" "F7" "F8" "F9" "F10"))
  (check-true (eq? #t (hash-ref register 'frozen)))
  (check-equal? (hash-ref register 'row-count) (length register-rows))
  ;; the plan digest is recorded provenance for the (repo-external) plan file;
  ;; the in-repo verbatim copy is raw/plan-failure-mode-register.txt, which the
  ;; next test checks the register against. Assert the digest is well formed.
  (check-true (and (regexp-match? #px"^[0-9a-f]{64}$" (row-ref register 'plan-sha256 "")) #t)
              "plan digest is a recorded SHA-256"))

(test-case "every register row carries its contract fields"
  (for ([r (in-list register-rows)])
    (define id (row-ref r 'id))
    (check-true (and (string? (row-ref r 'mode "")) (non-empty-string? (row-ref r 'mode "")))
                (format "~a mode" id))
    (check-true (non-empty-string? (row-ref r 'structural-fix "")) (format "~a structural-fix" id))
    (check-true (non-empty-string? (row-ref r 'refusal "")) (format "~a refusal" id))
    (check-true (and (member (row-ref r 'owning-wave) '("W1" "W2" "W3" "W4" "W5")) #t)
                (format "~a owning-wave" id))))

(test-case "register matches the raw plan excerpt verbatim"
  (define raw (file->string raw-register-path))
  (define raw-lines (string-split raw "\n"))
  (for ([r (in-list register-rows)])
    (define id (row-ref r 'id))
    ;; a mode may appear in both the incident table and the register table; the
    ;; register line is the one carrying the refusal contract.
    (define candidates
      (for/list ([l (in-list raw-lines)]
                 #:when (string-prefix? l (format "| ~a |" id)))
        l))
    (check-true (and (pair? candidates) #t) (format "~a present in raw plan excerpt" id))
    ;; every contract column must survive verbatim - mode, guard, owning wave
    ;; and refusal - not merely the refusal text.
    (check-true (and (for/or ([l (in-list candidates)])
                       (andmap (lambda (v) (string-contains? l v)) (row-fields r)))
                     #t)
                (format "~a (mode, guard, wave, refusal) verbatim from the plan" id)))
  ;; converse: the excerpt may not invent a mode the register does not freeze.
  (define raw-ids
    (for/list ([l (in-list raw-lines)]
               #:when (regexp-match? #px"^\\| (F[0-9]+) \\|" l))
      (second (regexp-match #px"^\\| (F[0-9]+) \\|" l))))
  (check-equal? (sort (remove-duplicates raw-ids) string<?)
                (sort (map (lambda (r) (row-ref r 'id)) register-rows) string<?)
                "raw plan excerpt contains exactly the frozen modes, none invented")
  ;; and the observed-evidence narrative must survive too, not just the four
  ;; contract columns: whatever the register carries as `observed` / evidence
  ;; gap must be readable verbatim from the excerpt, so neither can drift.
  (for ([r (in-list register-rows)])
    (define id (row-ref r 'id))
    (define observed (row-ref r 'observed ""))
    (define gap (row-ref r 'evidence-gap ""))
    (when (or (non-empty-string? observed) (non-empty-string? gap))
      (define line
        (for/or ([l (in-list raw-lines)]
                 #:when (string-prefix? l (format "| ~a |" id)))
          (and (or (not (non-empty-string? observed)) (string-contains? l observed))
               (or (not (non-empty-string? gap)) (string-contains? l gap))
               l)))
      (check-true (and line #t) (format "~a observed/evidence-gap verbatim from the plan" id)))))

(test-case "contract document agrees with the register"
  (define doc (file->string contract-doc-path))
  (for ([r (in-list register-rows)])
    (define id (row-ref r 'id))
    (define line
      (for/first ([l (in-list (string-split doc "\n"))]
                  #:when (string-prefix? l (format "| **~a**" id)))
        l))
    (check-true (and line #t) (format "~a row in contract doc" id))
    (check-true (and line (andmap (lambda (v) (string-contains? line v)) (row-fields r)) #t)
                (format "~a (mode, guard, wave, refusal) in contract doc" id)))
  ;; converse: the contract document may not invent a row the register does not
  ;; freeze (a fabricated F-row in the table would otherwise pass unnoticed).
  (define doc-ids
    (for/list ([l (in-list (string-split doc "\n"))]
               #:when (regexp-match? #px"^\\| \\*\\*(F[0-9]+)\\*\\*" l))
      (second (regexp-match #px"^\\| \\*\\*(F[0-9]+)\\*\\*" l))))
  (check-equal? (sort (remove-duplicates doc-ids) string<?)
                (sort (map (lambda (r) (row-ref r 'id)) register-rows) string<?)
                "contract document has exactly the frozen modes, none invented"))

(test-case "every register row has exactly one reproduction"
  (for ([r (in-list register-rows)])
    (define id (row-ref r 'id))
    (check-equal? (length (for/list ([x (in-list reproduction-rows)]
                                     #:when (equal? (row-ref x 'mode) id))
                            x))
                  1
                  (format "~a reproductions" id))
    (define repro (repro-for id))
    (check-equal? (row-ref repro 'expect) "refused" (format "~a expect" id))
    (check-true
     (and (member (row-ref repro 'guard-status) '("unguarded" "guarded-pass" "guarded-fail")) #t)
     (format "~a guard-status" id))))

(test-case "harness reports every row and never reports an unguarded row as passing"
  (check-equal? (length results) (length register-rows))
  (for ([res (in-list results)])
    (define id (row-result-mode res))
    ;; every reported status comes from the declared vocabulary, which contains
    ;; no passing value at all; an unguarded row is therefore unrepresentable as
    ;; a pass.
    (check-true (and (member (row-result-guard-status res) '(unguarded guarded-pass guarded-fail)) #t)
                (format "~a guard-status in vocabulary" id))
    (check-true (and (member (row-result-fixture-status res)
                             '(reproduced not-reproduced refused not-refused skipped))
                     #t)
                (format "~a fixture-status in vocabulary" id))
    (check-false (hash-has-key? guards id) (format "~a has no guard in W0" id))))

(test-case "harness expect-refused mode discriminates (self-test of the W6 mechanism)"
  ;; W0's deliverable is the mechanism W6 replays. Prove it actually
  ;; discriminates by registering a stub guard for one row and observing both
  ;; outcomes, then restore the empty registry.
  (dynamic-wind void
                (lambda ()
                  (register-guard! "F1" (lambda (_r) 'refused))
                  (define passed
                    (for/first ([x (in-list (run-register))]
                                #:when (equal? (row-result-mode x) "F1"))
                      x))
                  (check-eq? (row-result-guard-status passed) 'guarded-pass)
                  (register-guard! "F1" (lambda (_r) 'not-refused))
                  (define failed
                    (for/first ([x (in-list (run-register))]
                                #:when (equal? (row-result-mode x) "F1"))
                      x))
                  (check-eq? (row-result-guard-status failed) 'guarded-fail))
                (lambda () (hash-remove! guards "F1")))
  (check-equal? (hash-count guards) 0))

(test-case "unguarded rows still reproduce their defect on the unfixed tree"
  (for ([res (in-list results)]
        #:when (eq? (row-result-guard-status res) 'unguarded))
    (check-eq? (row-result-fixture-status res)
               'reproduced
               (format "~a fixture reproduces" (row-result-mode res)))))

(test-case "W0 registers no guard (no fix in this wave)"
  (check-equal? (hash-count guards) 0)
  (check-equal? (remove-duplicates (statuses)) '(unguarded)))

(test-case "SHA256SUMS covers every artifact input and matches its content"
  (define sums
    (for/list ([l (in-list (string-split (file->string sums-path) "\n"))]
               #:when (non-empty-string? (string-trim l)))
      (define m (regexp-match #px"^([0-9a-f]{64})  (.+)$" l))
      (check-true (and m #t) (format "well-formed SHA256SUMS line: ~a" l))
      (cons (second m) (third m))))
  (check-true (>= (length sums) 4))
  (for ([entry (in-list sums)])
    (define path (build-path q-root (cdr entry)))
    (check-true (file-exists? path) (format "~a exists" (cdr entry)))
    (check-equal? (sha256-file path)
                  (car entry)
                  (format "~a digest matches SHA256SUMS" (cdr entry)))))
