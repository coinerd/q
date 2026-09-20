#lang racket/base
;; @covers extensions/gsd/wave-docs.rkt
;; @covers extensions/gsd/wave-completion.rkt

;; @speed fast
;; @suite extensions
;; @boundary integration

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

;; run-register : [#:mode 'expect-refused] -> (listof row-result)
(define (run-register #:mode [mode 'expect-refused])
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
  (check-equal? (hash-ref register 'row-count) (length register-rows)))

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
    (check-true (and (for/or ([l (in-list candidates)])
                       (string-contains? l (row-ref r 'refusal "")))
                     #t)
                (format "~a refusal verbatim from the plan" id))))

(test-case "contract document agrees with the register"
  (define doc (file->string contract-doc-path))
  (for ([r (in-list register-rows)])
    (define id (row-ref r 'id))
    (define line
      (for/first ([l (in-list (string-split doc "\n"))]
                  #:when (string-prefix? l (format "| **~a**" id)))
        l))
    (check-true (and line #t) (format "~a row in contract doc" id))
    (check-true (and line (string-contains? line (row-ref r 'refusal "")))
                (format "~a refusal in contract doc" id))
    (check-true (and line (string-contains? line (row-ref r 'owning-wave "")))
                (format "~a owning wave in contract doc" id))))

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
    (check-false (eq? (row-result-guard-status res) 'ok) (format "~a is not 'ok" id))
    (cond
      [(eq? (row-result-guard-status res) 'unguarded)
       ;; consistency: unguarded <=> no guard registered
       (check-false (hash-has-key? guards id) (format "~a has no guard" id))
       (check-true (and (member (row-result-fixture-status res) '(reproduced not-reproduced)) #t)
                   (format "~a fixture status" id))]
      [else (check-true (hash-has-key? guards id) (format "~a has a guard" id))])))

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
