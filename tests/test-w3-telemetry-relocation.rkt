#lang racket/base

;; v<q-version> W3 pins: the report-only shard-plan-report job is relocated off the
;; PR critical path. The job (name, artifact contract, report content, same-SHA
;; provenance) moves byte-for-byte into .github/workflows/shard-plan-telemetry.yml,
;; triggered post-workflow (workflow_run: CI completed + workflow_dispatch).
;; These pins cover what the W0/W1/W2 pins in test-ci-runtime-contract.rkt do not:
;;   1. the W3 dag-checkpoint exists, is checksum-bound, and declares itself a
;;      topology checkpoint (not a cohort);
;;   2. the required-check policy_pin is byte-identical to the W2 pin (W3 must
;;      not remove or add any required check — the report was never required);
;;   3. the ci.yml seam: no `shard-plan-report:` job definition remains and
;;      test-aggregate is the workflow tail (needs: [test, test-platform]);
;;   4. the telemetry workflow carries the unchanged artifact contract and the
;;      unchanged same-SHA provenance, with exactly one tolerant download step;
;;   5. the same-SHA timing shape: the PR workflow's critical path ends 289s
;;      earlier (286s report + 3s scheduling gap), with the report's measured
;;      duration unchanged — the win is placement, not a faster report.

(require racket/file
         (only-in "../util/version.rkt" q-version)
         racket/list
         racket/path
         racket/string
         racket/format
         json
         rackunit)

(define this-dir
  (simple-form-path (path-only (resolved-module-path-name (variable-reference->resolved-module-path
                                                           (#%variable-reference))))))
(define project-root (simplify-path (build-path this-dir "..")))

;; Pure-Racket SHA-256 (lowercase hex) — same verified implementation as
;; test-ci-runtime-contract.rkt (no collect dependencies beyond racket/base).
(define (sha256-hex path)
  (define (m32 x)
    (bitwise-and x #xffffffff))
  (define (rotr x n)
    (bitwise-ior (arithmetic-shift (m32 x) (- n)) (arithmetic-shift (m32 x) (- 32 n))))
  (define (word bs i)
    (+ (* (bytes-ref bs i) 16777216)
       (* (bytes-ref bs (+ i 1)) 65536)
       (* (bytes-ref bs (+ i 2)) 256)
       (bytes-ref bs (+ i 3))))
  (define k
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
  (define data (file->bytes path))
  (define len (bytes-length data))
  (define padded
    (let* ([zeros (let loop ([n 0])
                    (if (= 56 (modulo (+ len 1 n) 64))
                        n
                        (loop (add1 n))))]
           [total (+ len 1 zeros 8)]
           [bs (make-bytes total 0)])
      (bytes-copy! bs 0 data)
      (bytes-set! bs len #x80)
      (define bitlen (* 8 len))
      (for ([i (in-range 8)])
        (bytes-set! bs (- total 1 i) (bitwise-and (arithmetic-shift bitlen (* -8 i)) #xff)))
      bs))
  (define h
    (vector #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))
  (define w (make-vector 64 0))
  (for ([off (in-range 0 (bytes-length padded) 64)])
    (for ([i (in-range 16)])
      (vector-set! w i (word padded (+ off (* 4 i)))))
    (for ([i (in-range 16 64)])
      (define s0
        (bitwise-xor (rotr (vector-ref w (- i 15)) 7)
                     (rotr (vector-ref w (- i 15)) 18)
                     (arithmetic-shift (vector-ref w (- i 15)) -3)))
      (define s1
        (bitwise-xor (rotr (vector-ref w (- i 2)) 17)
                     (rotr (vector-ref w (- i 2)) 19)
                     (arithmetic-shift (vector-ref w (- i 2)) -10)))
      (vector-set! w i (m32 (+ (vector-ref w (- i 16)) s0 (vector-ref w (- i 7)) s1))))
    (let loop ([a (vector-ref h 0)]
               [b (vector-ref h 1)]
               [c (vector-ref h 2)]
               [d (vector-ref h 3)]
               [e (vector-ref h 4)]
               [f (vector-ref h 5)]
               [g (vector-ref h 6)]
               [hh (vector-ref h 7)]
               [i 0])
      (if (= i 64)
          (begin
            (for ([i2 (in-range 8)])
              (vector-set! h i2 (m32 (+ (vector-ref h i2) (list-ref (list a b c d e f g hh) i2))))))
          (let* ([S1 (bitwise-xor (rotr e 6) (rotr e 11) (rotr e 25))]
                 [ch (bitwise-xor (bitwise-and e f) (bitwise-and (bitwise-not e) g))]
                 [t1 (m32 (+ hh S1 ch (vector-ref k i) (vector-ref w i)))]
                 [S0 (bitwise-xor (rotr a 2) (rotr a 13) (rotr a 22))]
                 [maj (bitwise-xor (bitwise-and a b) (bitwise-and a c) (bitwise-and b c))]
                 [t2 (m32 (+ S0 maj))])
            (loop (m32 (+ t1 t2)) a b c (m32 (+ d t1)) e f g (add1 i))))))
  (define (hex8 n)
    (define s (number->string n 16))
    (string-append (make-string (- 8 (string-length s)) #\0) s))
  (string-append* (for/list ([i (in-range 8)])
                    (hex8 (vector-ref h i)))))

(define (rd . parts)
  (apply build-path project-root parts))

(define ci-yml (file->string (rd ".github" "workflows" "ci.yml")))
(define cp3 (rd "artifacts" "ci-topology" (format "v~a-w3" q-version) "dag-checkpoint.json"))
(define cp2 (rd "artifacts" "ci-topology" (format "v~a-w2" q-version) "dag-checkpoint.json"))
(define telemetry-yml-path (rd ".github" "workflows" "shard-plan-telemetry.yml"))

;; ---- 1. checkpoint exists, checksum-bound, topology-checkpoint-not-cohort ----
(test-case "W3: dag-checkpoint exists, is checksum-bound, and is a topology checkpoint"
  (check-true (file-exists? cp3) "W3 dag-checkpoint.json must exist")
  (check-equal? (sha256-hex cp3)
                "bffe410ac755bddec09642c6fa23b363896bde95b3a5503ab8b58f40f2db12ab"
                "W3 dag-checkpoint.json must stay byte-for-byte the recorded checkpoint")
  (define j (call-with-input-file cp3 read-json))
  (check-equal? (hash-ref j 'wave) (format "v~a-w3" q-version))
  (check-equal? (hash-ref (hash-ref j 'timing_checkpoint) 'label)
                "topology checkpoint — not a cohort")
  (check-equal? (hash-ref (hash-ref j 'timing_checkpoint) 'measurement_kind)
                "derived-shape-projection"))

;; ---- 2. required-check policy unchanged from W2 ----
(test-case "W3: policy_pin byte-identical to the W2 pin (no required check added or removed)"
  (define j3 (call-with-input-file cp3 read-json))
  (define j2 (call-with-input-file cp2 read-json))
  (check-equal? (sort (map ~a (hash-ref j3 'policy_pin)) string<?)
                (sort (map ~a (hash-ref j2 'policy_pin)) string<?)
                "the relocation is report-only: required checks must be unchanged")
  (check-false (member "shard-plan-report" (map ~a (hash-ref j3 'policy_pin)))
               "the report was never a required check and must not become one"))

;; ---- 3. ci.yml seam: job gone, test-aggregate is the tail ----
(test-case "W3: no shard-plan-report job in ci.yml; test-aggregate is the workflow tail"
  (check-equal? (length (regexp-match* #px"^  shard-plan-report:" ci-yml))
                0
                "ci.yml must not define a shard-plan-report job")
  (check-true (string-contains? ci-yml "shard-plan-telemetry.yml")
              "ci.yml keeps a pointer comment to the telemetry workflow")
  (define lines (string-split ci-yml "\n"))
  (check-true (regexp-match? #px"(?m:^\\s*test-aggregate:)" ci-yml)
              "ci.yml must still define test-aggregate")
  (define idx (index-of lines (λ (l) (regexp-match? #px"^\\s*test-aggregate:" l))))
  (when idx
    (define block (string-join (take (drop lines idx) 10) "\n"))
    (check-true (string-contains? block "needs: [test, test-platform]")
                "test-aggregate must need exactly [test, test-platform] (new workflow tail)")))

;; ---- 4. telemetry workflow: unchanged artifact contract, one tolerant download ----
(test-case "W3: telemetry workflow carries the unchanged artifact contract"
  (check-true (file-exists? telemetry-yml-path) "shard-plan-telemetry.yml must exist")
  (define j3 (call-with-input-file cp3 read-json))
  (define contract (hash-ref j3 'telemetry_relocation_contract))
  (check-equal? (sha256-hex telemetry-yml-path)
                (hash-ref contract 'telemetry_workflow_sha256)
                "telemetry workflow must stay byte-for-byte the recorded relocation")
  (define t (file->string telemetry-yml-path))
  (for ([s (list "name: shard-plan-report"
                 "shard-plan-report.log"
                 "retention-days: 7"
                 "if-no-files-found: ignore"
                 "workflow_run"
                 "workflow_dispatch"
                 "gh run download"
                 "FAST_SHARD_COUNT"
                 "workflow_run.head_sha")]
        #:when #t)
    (check-true (string-contains? t s) (format "telemetry yml must contain ~s" s)))
  ;; tolerance boundary: only the artifact download may be tolerant; the
  ;; plan-build/summary/upload steps stay loud so a planner failure fails the
  ;; telemetry workflow (never silently).
  (check-equal? (length (regexp-match* #px"continue-on-error: true" t))
                1
                "exactly the artifact download step may use continue-on-error: true"))

;; ---- 5. same-SHA timing shape ----
(test-case "W3: same-SHA timing shape — critical path ends 289s earlier, report duration unchanged"
  (define tc (hash-ref (call-with-input-file cp3 read-json) 'timing_checkpoint))
  (define before (hash-ref tc 'before))
  (define after (hash-ref tc 'after))
  (check-equal? (hash-ref before 'head_sha)
                (hash-ref after 'anchor_sha)
                "before/after must be anchored to the SAME head SHA")
  (check-equal? (hash-ref after 'anchor_sha) "c6ee39e43025cc49202a27084aed459221dac43c")
  (check-equal? (hash-ref tc 'delta_seconds_earlier) 289)
  (check-equal? (hash-ref after 'workflow_end)
                "2026-09-06T18:35:44Z"
                "after: the PR workflow ends with test-aggregate")
  (check-equal? (hash-ref before 'workflow_end)
                "2026-09-06T18:40:39Z"
                "before: the PR workflow ended with the report tail")
  (define (report-duration j)
    (for/first ([job (in-list (hash-ref j 'jobs))]
                #:when (string-contains? (~a (hash-ref job 'name)) "shard-plan-report"))
      (hash-ref job 'duration_seconds)))
  (check-equal? (report-duration before) 286)
  (check-equal? (report-duration after)
                286
                "the report's measured duration is unchanged — the win is placement"))
