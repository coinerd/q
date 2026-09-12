#lang racket/base

;; q/scripts/proof-bundle/bundle-writer.rkt — canonical q.proof-bundle/1 writer
;; (v1.00.29 W5: proof-graph reduction, spec §5.2–§5.16)
;;
;; A proof bundle is evidence, not a cached exit code (spec §5.1). The writer:
;;   - emits canonicalized JSON (sorted keys, deterministic formatting, no
;;     whitespace) so byte-identical input always yields byte-identical output;
;;   - content-addresses the bundle: bundle_id = "sha256:<hex>" over the
;;     canonical JSON of the bundle with the bundle_id field set to the empty
;;     string (spec §5.2);
;;   - knows every required §5.2–§5.16 field and fails closed on an incomplete
;;     bundle (a producer never writes an unprovable bundle);
;;   - depends only on racket/base plus the json, racket/contract and
;;     racket/file libraries — no network, no threads, no sleeps (the json
;;     collection is required under its installed name `json`).
;;
;; SHA-256 is implemented here (FIPS 180-4, exact integer arithmetic) instead
;; of being required from a sibling module, so that the canonical digest
;; primitive ships with the canonical writer: consumers and producers MUST
;; agree byte-for-byte on how bundle_id is derived, and the wave contract
;; pins this module's dependency set. Digests cover small inputs only
;; (canonical JSON documents), so a pure-Racket implementation is fast enough.
;;
;; STABILITY: prototype (v1.00.29 W5) — this is the W9 gate; never remove a
;; recomputation before its replacement evidence can be validated fail-closed.

(require json
         racket/contract
         racket/file
         racket/string)

(provide (contract-out (proof-bundle-schema string?)
                       (required-bundle-fields (listof string?))
                       (required-section-fields (hash/c string? (listof string?)))
                       (canonical-json (-> jsexpr? string?))
                       (canonical-json-bytes (-> jsexpr? bytes?))
                       (sha256-bytes (-> bytes? bytes?))
                       (sha256-hex (-> bytes? string?))
                       (sha256-string (-> string? string?))
                       (proof-bundle-id (-> jsexpr? string?))
                       (complete-bundle-errors (-> jsexpr? (listof string?)))
                       (finalize-proof-bundle (-> jsexpr? jsexpr?))
                       (proof-bundle->string (-> jsexpr? string?))
                       (write-proof-bundle! (->* (jsexpr?) (#:path (or/c path-string? #f)) jsexpr?))))

;; ---------------------------------------------------------------------------
;; Schema identity and required-field metadata (spec §5.2–§5.16)
;; ---------------------------------------------------------------------------

(define proof-bundle-schema "q.proof-bundle/1")

;; The 17 required top-level fields (spec §5.2 and the q.proof-bundle/1 JSON
;; schema "required" list).
(define required-bundle-fields
  (list "schema"
        "bundle_id"
        "created_at"
        "producer"
        "subject"
        "claims"
        "selection"
        "command"
        "environment"
        "policy"
        "prepared_environment"
        "result"
        "artifacts"
        "provenance"
        "retention"
        "authorization"
        "compatibility"))

;; Required sub-fields per section, from spec §5.3–§5.16. The producer-side
;; completeness gate and the consumer-side fail-closed checks both derive from
;; this single table so the two can never drift apart.
(define required-section-fields
  (hash
   "producer"
   (list "repository"
         "workflow_path"
         "workflow_revision_sha"
         "run_id"
         "run_attempt"
         "job_id"
         "job_name"
         "event"
         "ref"
         "trust_tier")
   "subject"
   (list "subject_mode" "commit_sha" "tree_sha" "dirty")
   "selection"
   (list "inventory_digest"
         "metadata_manifest_digest"
         "covers_manifest_digest"
         "selected_manifest_digest"
         "selected_count"
         "selection_mode"
         "selector_revision_sha"
         "explanation_artifact_digest")
   "command"
   (list "argv" "cwd_contract" "flags_digest" "runner_revision_sha")
   "environment"
   (list "os"
         "os_version"
         "runner_image"
         "architecture"
         "racket_version"
         "racket_executable_digest"
         "racket_installer_digest"
         "package_lock_digest"
         "resolved_package_set_digest"
         "locale"
         "timezone")
   "policy"
   (list "test_profile"
         "security_profile"
         "sandbox_profile"
         "feature_flags_digest"
         "relevant_environment_digest")
   "prepared_environment"
   (list "mode" "artifact_digest" "build_recipe_digest" "verification_status" "compiled_cache_digest")
   "result"
   (list "status"
         "started_at"
         "completed_at"
         "elapsed_ms"
         "tests_passed"
         "tests_failed"
         "tests_skipped"
         "allowed_skip_reasons"
         "timeouts"
         "cancelled"
         "retry_count"
         "result_summary_digest")
   "provenance"
   (list "attestation_format"
         "attestation_digest"
         "producer_identity"
         "source_repository"
         "source_workflow_revision_sha"
         "verification_method"
         "verification_key_or_identity")
   "retention"
   (list "policy_id" "created_at" "retain_until" "immutable" "store" "retention_verified_at")
   "authorization"
   (list "allowed_consumers" "denied_consumers" "release_reusable")
   "compatibility"
   (list "compatibility_policy_version"
         "environment_class"
         "satisfies_classes"
         "subject_scope"
         "notes")))

;; Artifacts are a list; each element requires these keys (spec §5.12).
(define required-artifact-fields
  (list "name" "media_type" "digest" "size_bytes" "store" "store_object_id"))

;; Each claim requires these keys (spec §5.5). `gate_class` is the repo's W0
;; claim-metadata extension (required-vs-observational status; §9 case 19:
;; an observational proof is never reusable by a required gate). Only `pass`
;; claims are reusable.
(define required-claim-fields
  (list "claim_id" "claim_version" "proof_class" "required_environment_class" "result" "gate_class"))

;; ---------------------------------------------------------------------------
;; Dependency-free SHA-256 (FIPS 180-4)
;; ---------------------------------------------------------------------------

(define K
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

(define H0
  (list->vector
   '(#x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19)))

(define (m32 x)
  (bitwise-and x #xffffffff))

(define (rotr32 x n)
  (bitwise-ior (arithmetic-shift x (- n)) (arithmetic-shift x (- 32 n))))

(define (sha256-bytes bstr)
  (define msg-len (bytes-length bstr))
  (define bit-len (* 8 msg-len))
  ;; Pad: message || 0x80 || zeros || 64-bit big-endian bit length.
  (define zeros (modulo (- 55 msg-len) 64))
  (define total (+ msg-len 1 zeros 8))
  (define buf (make-bytes total 0))
  (bytes-copy! buf 0 bstr)
  (bytes-set! buf msg-len #x80)
  (for ([i (in-range 8)])
    (bytes-set! buf (- total 1 i) (bitwise-and (arithmetic-shift bit-len (* -8 i)) #xff)))
  (define h (list->vector (vector->list H0)))
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
        (define temp1 (m32 (+ hh S1 ch (vector-ref K i) (vector-ref w i))))
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
  (define out (make-bytes 32 0))
  (for ([i (in-range 8)])
    (define word (vector-ref h i))
    (for ([j (in-range 4)])
      (bytes-set! out (+ (* 4 i) j) (bitwise-and (arithmetic-shift word (* -8 (- 3 j))) #xff))))
  out)

(define hex-chars "0123456789abcdef")

(define (bytes->hex-string bstr)
  (define out (make-string (* 2 (bytes-length bstr))))
  (for ([i (in-range (bytes-length bstr))])
    (define byte (bytes-ref bstr i))
    (string-set! out (* 2 i) (string-ref hex-chars (arithmetic-shift byte -4)))
    (string-set! out (add1 (* 2 i)) (string-ref hex-chars (bitwise-and byte #x0f))))
  out)

(define (sha256-hex bstr)
  (bytes->hex-string (sha256-bytes bstr)))

(define (sha256-string s)
  (sha256-hex (string->bytes/utf-8 s)))

;; ---------------------------------------------------------------------------
;; Canonical JSON (sorted keys, deterministic formatting)
;; ---------------------------------------------------------------------------

(define (canonical-key k)
  (cond
    [(string? k) k]
    [(symbol? k) (symbol->string k)]
    [else (error 'canonical-json "unsupported object key: ~s" k)]))

(define (json-escape s)
  (define out (open-output-string))
  (display "\"" out)
  (for ([ch (in-string s)])
    (cond
      [(char=? ch #\") (display "\\\"" out)]
      [(char=? ch #\\) (display "\\\\" out)]
      [(char=? ch #\newline) (display "\\n" out)]
      [(char=? ch #\return) (display "\\r" out)]
      [(char=? ch #\tab) (display "\\t" out)]
      [(char=? ch #\u08) (display "\\b" out)]
      [(char=? ch #\u0C) (display "\\f" out)]
      [(char<? ch #\u20)
       (display (string-append "\\u00" (substring (bytes->hex-string (char->bytes ch)) 0 2)) out)]
      [else (write-char ch out)]))
  (display "\"" out)
  (get-output-string out))

(define (char->bytes ch)
  (string->bytes/utf-8 (string ch)))

(define (canonical-json-number n)
  (cond
    [(exact-integer? n) (number->string n)]
    [(real? n) (number->string n)]
    [else (error 'canonical-json "unsupported JSON number: ~s" n)]))

(define (canonical-json v)
  (cond
    [(hash? v)
     (define entries
       (for/list ([k (in-hash-keys v)])
         (cons (canonical-key k) k)))
     (define sorted (sort entries string<? #:key car))
     (string-append
      "{"
      (string-join (for/list ([e sorted])
                     (string-append (json-escape (car e)) ":" (canonical-json (hash-ref v (cdr e)))))
                   ",")
      "}")]
    [(list? v) (string-append "[" (string-join (map canonical-json v) ",") "]")]
    [(string? v) (json-escape v)]
    [(boolean? v) (if v "true" "false")]
    [(number? v) (canonical-json-number v)]
    [(symbol? v) (error 'canonical-json "unsupported JSON symbol value: ~s" v)]
    [(eq? v 'null) (error 'canonical-json "unsupported JSON null value: ~s" v)]
    [else (error 'canonical-json "unsupported JSON value: ~s" v)]))

(define (canonical-json-bytes v)
  (string->bytes/utf-8 (canonical-json v)))

;; ---------------------------------------------------------------------------
;; bundle_id: content address over the canonical form with bundle_id empty
;; ---------------------------------------------------------------------------

(define bundle-id-prefix "sha256:")

(define (proof-bundle-id spec)
  (define pre-image
    (if (hash? spec)
        (hash-set spec 'bundle_id "")
        spec))
  (string-append bundle-id-prefix (sha256-string (canonical-json pre-image))))

(define (bundle-id-shape-ok? s)
  (and (string? s)
       (string-prefix? s bundle-id-prefix)
       (= 71 (string-length s))
       (for/and ([ch (in-string (substring s 7))])
         (or (char<=? #\0 ch #\9) (char<=? #\a ch #\f)))))

;; ---------------------------------------------------------------------------
;; Producer-side completeness gate (fail-closed; spec §5.2–§5.16)
;; ---------------------------------------------------------------------------

(define (section-of spec key-string)
  (hash-ref spec (string->symbol key-string) #f))

(define (complete-bundle-errors spec)
  (define errors '())
  (define (add! e)
    (set! errors (cons e errors)))
  (unless (hash? spec)
    (add! "bundle-not-an-object"))
  (when (hash? spec)
    (define present
      (for/list ([k (in-hash-keys spec)])
        (canonical-key k)))
    (for ([f required-bundle-fields])
      (unless (member f present)
        (add! (string-append "missing-field:" f))))
    (when (and (member "schema" present)
               (not (string=? proof-bundle-schema (hash-ref spec 'schema ""))))
      (add! (string-append "schema-mismatch:expected-" proof-bundle-schema)))
    ;; An empty bundle_id is the pre-finalization state (finalize-proof-bundle
    ;; computes and installs the content address); anything else non-empty must
    ;; already be well-formed.
    (define stated-id (hash-ref spec 'bundle_id ""))
    (when (and (member "bundle_id" present)
               (not (string=? stated-id ""))
               (not (bundle-id-shape-ok? stated-id)))
      (add! "malformed-bundle-id"))
    (for ([pair (in-hash-pairs required-section-fields)])
      (define section-name (car pair))
      (define section (section-of spec section-name))
      (cond
        [(not (hash? section)) (add! (string-append "malformed-section:" section-name))]
        [else
         (define keys
           (for/list ([k (in-hash-keys section)])
             (canonical-key k)))
         (for ([sub (cdr pair)])
           (unless (member sub keys)
             (add! (string-append "missing-field:" section-name "." sub))))]))
    ;; claims: non-empty list, per-claim required keys, only `pass` claims.
    (define claims (hash-ref spec 'claims #f))
    (cond
      [(not (and (list? claims) (pair? claims))) (add! "malformed-section:claims")]
      [else
       (for ([claim claims]
             [i (in-naturals)])
         (cond
           [(not (hash? claim)) (add! (format "malformed-claim:~a" i))]
           [else
            (define keys
              (for/list ([k (in-hash-keys claim)])
                (canonical-key k)))
            (for ([sub required-claim-fields])
              (unless (member sub keys)
                (add! (format "missing-field:claims.~a.~a" i sub))))
            (unless (equal? (hash-ref claim 'result #f) "pass")
              (add! (format "claim-not-pass:~a" i)))]))])
    ;; artifacts: list, per-artifact required keys.
    (define artifacts (hash-ref spec 'artifacts #f))
    (cond
      [(not (list? artifacts)) (add! "malformed-section:artifacts")]
      [else
       (for ([art artifacts]
             [i (in-naturals)])
         (cond
           [(not (hash? art)) (add! (format "malformed-artifact:~a" i))]
           [else
            (define keys
              (for/list ([k (in-hash-keys art)])
                (canonical-key k)))
            (for ([sub required-artifact-fields])
              (unless (member sub keys)
                (add! (format "missing-field:artifacts.~a.~a" i sub))))]))]))
  (reverse errors))

;; ---------------------------------------------------------------------------
;; Finalization and writing
;; ---------------------------------------------------------------------------

;; Computes bundle_id and returns the finalized spec. Does not write.
(define (finalize-proof-bundle spec)
  (define errors (complete-bundle-errors spec))
  (unless (null? errors)
    (error 'finalize-proof-bundle
           "refusing to finalize an incomplete proof bundle: ~a"
           (string-join errors ", ")))
  (hash-set spec 'bundle_id (proof-bundle-id spec)))

(define (proof-bundle->string spec)
  (string-append (canonical-json spec) "\n"))

;; Canonical writer. With #:path writes the canonical bytes (single line plus
;; trailing newline) and creates parent directories; always returns the
;; finalized spec (with bundle_id set) so callers can inspect the address.
(define (write-proof-bundle! spec #:path [path #f])
  (define finalized (finalize-proof-bundle spec))
  (when path
    (define-values (base _name _dir?)
      (split-path (if (complete-path? path)
                      (path->complete-path path)
                      path)))
    (when base
      (make-directory* base))
    (call-with-output-file path
                           (lambda (out) (display (proof-bundle->string finalized) out))
                           #:exists 'replace))
  finalized)
