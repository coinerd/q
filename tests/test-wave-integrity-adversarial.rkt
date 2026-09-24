#lang racket/base

;; @speed slow
;; @suite workflows
;; @timeout 900
;; @boundary integration

;; v1.00.31 W6 — adversarial permanence rehearsal.
;;
;; Replays the frozen register harness in expect-refused mode over injected
;; fixtures: every register row F1-F13 must be refused by its shipped guard, a
;; clean synthetic wave must be accepted by every guard (so the guards are not
;; merely refusing everything), and a guard that is missing must be reported
;; `guard-missing` instead of skipped (so a guard cannot be neutered by
;; omission). The committed matrix must regenerate byte-identically and must stay
;; bound to the W0 register digest, so editing the register invalidates the
;; verdict instead of silently inheriting a stale PERMANENT.

(require racket/file
         racket/list
         racket/path
         racket/runtime-path
         racket/set
         racket/string
         rackunit
         json
         (file "../util/json/checksum.rkt")
         (file "../scripts/ci/inject-wave-defect.rkt"))

(define-runtime-path q-root-rel "..")
(define q-root (simplify-path q-root-rel))
(define matrix-path
  (build-path q-root "artifacts/wave-delivery-integrity/v1.00.31-w6/injection-matrix.json"))
(define sums-path (build-path q-root "artifacts/wave-delivery-integrity/v1.00.31-w6/SHA256SUMS"))
(define register-path
  (build-path q-root "artifacts/wave-delivery-integrity/v1.00.31-w0/failure-register.json"))
(define reproduction-path
  (build-path q-root "artifacts/wave-delivery-integrity/v1.00.31-w0/w4-reproduction.json"))

(define (read-json-file p)
  (with-input-from-file p (lambda () (read-json))))

(define register (read-json-file register-path))

(define (row-hash matrix id)
  (for/first ([r (in-list (hash-ref matrix 'rows))]
              #:when (equal? (hash-ref r 'id) id))
    r))

(define (outcome matrix id)
  (hash-ref (row-hash matrix id) 'outcome))

(define rehearsal (run-rehearsal q-root))

(test-case "every register row F1-F13 is refused by its shipped guard"
  (check-equal? (map (lambda (r) (hash-ref r 'id)) (hash-ref rehearsal 'rows))
                '("F1" "F2" "F3" "F4" "F5" "F6" "F7" "F8" "F9" "F10" "F11" "F12" "F13"))
  (for ([r (in-list (hash-ref register 'rows))])
    (define id (hash-ref r 'id))
    (check-equal? (outcome rehearsal id)
                  "refused"
                  (format "~a injected defect refused (observed: ~a)"
                          id
                          (hash-ref (row-hash rehearsal id) 'reason))))
  (check-equal? (hash-ref rehearsal 'failing-rows) '()))

(test-case "the clean synthetic wave is accepted by every guard"
  (check-equal? (hash-ref (hash-ref (hash-ref rehearsal 'controls) 'clean-synthetic-wave) 'outcome)
                "accepted")
  (for ([c (in-list
            (hash-ref (hash-ref (hash-ref rehearsal 'controls) 'clean-synthetic-wave) 'per-row '()))])
    (check-equal?
     (hash-ref c 'outcome)
     "accepted"
     (format "clean control accepted by ~a (observed: ~a)" (hash-ref c 'id) (hash-ref c 'reason)))))

(test-case "a missing guard is detected instead of skipped"
  (check-equal? (hash-ref (hash-ref (hash-ref rehearsal 'controls) 'missing-guard) 'outcome)
                "detected")
  ;; Fail closed per row: with the guard files absent from the rehearsal root,
  ;; every row reports guard-missing and nothing is reported as refused/ok.
  (define empty-root (make-temporary-file "q-w6-missing-~a" 'directory))
  (define outcomes (row-outcomes empty-root))
  (delete-directory/files empty-root #:must-exist? #f)
  (check-equal? (sort (remove-duplicates (for/list ([o (in-list outcomes)])
                                           (hash-ref o 'outcome)))
                      string<?)
                '("guard-missing"))
  (check-equal? (length outcomes) 13))

(test-case "the verdict is PERMANENT only with all rows refused and both controls"
  (check-equal? (hash-ref rehearsal 'verdict) "PERMANENT")
  ;; The verdict's inputs are exactly the three conditions, so a single failing
  ;; row or control would flip it: assert the composition, not just the label.
  (check-equal? (hash-ref rehearsal 'refused-rows)
                (map (lambda (r) (hash-ref r 'id)) (hash-ref register 'rows)))
  (check-equal? (hash-ref rehearsal 'failing-rows) '()))

(test-case "the matrix is bound to the frozen register and reproduces byte-identically"
  (define committed (read-json-file matrix-path))
  (check-equal? (canonical-json rehearsal)
                (file->string matrix-path)
                "a fresh rehearsal reproduces the committed matrix byte-identically")
  (check-equal? (hash-ref (hash-ref committed 'register) 'sha256)
                (sha256-file register-path)
                "the recorded register digest is the current register digest")
  (check-equal? (hash-ref (hash-ref committed 'register) 'reproduction-sha256)
                (sha256-file reproduction-path)
                "the recorded reproduction digest is the current reproduction digest")
  (check-equal? (hash-ref (hash-ref committed 'register) 'row-count)
                (length (hash-ref register 'rows)))
  (check-equal? (hash-ref (hash-ref committed 'register) 'row-count) 13)
  ;; Invalidation: a register edit changes the digest, so the recorded binding
  ;; stops matching and the verdict can no longer be inherited silently.
  (define mutated (make-temporary-file "q-w6-register-~a.json"))
  (dynamic-wind
   (lambda ()
     (define data (read-json-file register-path))
     (with-output-to-file mutated
                          (lambda ()
                            (write-json (hash-set data 'row-count (add1 (hash-ref data 'row-count))))
                            (newline))
                          #:exists 'truncate))
   (lambda ()
     (check-not-equal? (sha256-file mutated)
                       (hash-ref (hash-ref committed 'register) 'sha256)
                       "a mutated register no longer matches the recorded digest"))
   (lambda () (delete-file mutated)))
  (check-true (and (regexp-match? #px"^[0-9a-f]{40}$" (hash-ref committed 'rehearsal-head "")) #t)
              "the rehearsal head is a full commit SHA"))

(test-case "the matrix is bound by SHA256SUMS and stays canonical"
  (define sums
    (for/list ([l (in-list (string-split (file->string sums-path) "\n"))]
               #:when (non-empty-string? (string-trim l)))
      (define m (regexp-match #px"^([0-9a-f]{64})  (.+)$" l))
      (check-true (and m #t) (format "well-formed SHA256SUMS line: ~a" l))
      (cons (second m) (third m))))
  (check-true (>= (length sums) 2))
  (for ([entry (in-list sums)])
    (define path (build-path q-root (cdr entry)))
    (check-true (file-exists? path) (format "~a exists" (cdr entry)))
    (check-equal? (sha256-file path) (car entry) (format "~a digest matches SHA256SUMS" (cdr entry))))
  ;; Coverage: every non-raw file under the artifact directory is recorded.
  (define recorded
    (for/set ([entry (in-list sums)])
      (cdr entry)))
  (define (raw-segment? file)
    (and (member "raw" (map path->string (explode-path file))) #t))
  (for ([file (in-directory (build-path q-root "artifacts/wave-delivery-integrity/v1.00.31-w6"))]
        #:when (and (file-exists? file)
                    (not (directory-exists? file))
                    (not (raw-segment? file))
                    (not (string-suffix? (path->string file) "SHA256SUMS"))))
    (define rel (find-relative-path q-root (simplify-path file)))
    (check-true (set-member? recorded (path->string rel))
                (format "~a is covered by SHA256SUMS" (path->string rel))))
  ;; The matrix must be the canonical (Python-compatible) rendering.
  (check-equal? (canonical-json (read-json-file matrix-path))
                (file->string matrix-path)
                "the committed matrix is already in canonical form"))
