#lang racket
;; scripts/github/wave-finalizer.rkt -- pure model layer of the wave-finalizer port.
;;
;; This module is the model layer only: it cannot prepare, merge, close, or sync
;; anything.  It parses and validates trusted configuration, external envelopes,
;; branch-protection policy snapshots, GitHub API payloads, and ISO-8601
;; timestamps; it computes Python-parity canonical JSON and SHA-256 digests; and
;; it owns the sealed `prepared-finalization` snapshot plus its revalidation.
;; An authenticated adapter and the finalizing controller are separate modules.
;;
;; Ported from `scripts/github/wave-finalizer.py` (since deleted: this module is
;; now the only finalizer implementation).  Refusal kinds and messages are
;; kept byte-identical unless a note below says otherwise; the notes are repeated
;; in `tests/test-wave-finalizer-model.rkt` next to the assertions, and the
;; Python behaviour they pin lives in `tests/fixtures/wave-finalizer-goldens.json`.
;;
;; Parity notes (deliberately narrowed, always fail-closed):
;;  * `parse-timestamp` accepts the GitHub extended ISO-8601 form only
;;    (YYYY-MM-DD[Tt ]HH:MM[:SS[.frac]](Z|+hh:mm|+hhmm)); a lowercase `z` is
;;    refused, exactly like Python's `.replace("Z", "+00:00")` + fromisoformat.
;;    CPython's `datetime.fromisoformat` additionally accepts basic forms
;;    (20240101T000000), any single-character date/time separator, hour-only
;;    offsets and offsets with seconds; those are refused here as malformed
;;    rather than silently accepted.
;;  * Canonical JSON float rendering reproduces CPython's `float.__repr__`
;;    (which `json.dumps` emits); non-finite floats use the same
;;    "Out of range float values are not JSON compliant: ..." text for nan/inf.
;;    Errors raised for unserialisable *Racket* values keep the
;;    "<label> is not canonical JSON: " prefix but have a Racket-specific suffix.
;;  * `parse-protection-policy` keeps the message texts of this port's earlier
;;    revision even where the Python original says "response is not an object"
;;    (kind is identical: ApiUncertainty).

(require json
         file/sha1
         openssl
         racket/bytes
         racket/list
         racket/string)

(provide (struct-out exn:fail:finalizer)
         (struct-out protection-policy)
         protection-policy-app-id-for
         parse-protection-policy
         (struct-out workflow-contract)
         (struct-out board-sync)
         board-sync-value
         (struct-out trusted-configuration)
         load-trusted-configuration
         (struct-out external-projection)
         (struct-out verified-envelope)
         verified-envelope-evidence-reference
         verify-external-envelope
         compute-envelope-digest
         finalizer-canonical-json
         typed-equal?
         parse-timestamp
         api-mapping
         api-items
         api-nested
         api-bool
         api-positive-int
         prepared-finalization?
         prepared-finalization-config-json
         prepared-finalization-envelope-json
         prepared-finalization-config-digest
         prepared-finalization-envelope-digest
         prepared-finalization-attestation-digest
         prepared-finalization-head-sha
         prepared-finalization-policy
         prepared-finalization-premerge-run-ids
         prepared-finalization-already-merged
         prepared-finalization-seal
         issue-prepared-finalization
         prepared-key-source
         validate-prepared-finalization
         (struct-out finalization-result))

;; ================================================================ errors
;; Python: FinalizerError + subclasses, distinguished by `kind` (PolicyError and
;; CheckError subclass ValidationError; the kind string records the class name).

(struct exn:fail:finalizer exn:fail (kind) #:transparent)

(define (fail kind message)
  (raise (exn:fail:finalizer message (current-continuation-marks) kind)))
(define (uncertain message)
  (fail "ApiUncertainty" message))
(define (policy-error message)
  (fail "PolicyError" message))
(define (check-error message)
  (fail "CheckError" message))
(define (validation-error message)
  (fail "ValidationError" message))
(define (configuration-error message)
  (fail "ConfigurationError" message))
(define (envelope-error message)
  (fail "EnvelopeError" message))
(define (attestation-error message)
  (fail "AttestationError" message))
(define (merge-error message)
  (fail "MergeError" message))

;; ================================================================ digests
;; Python used hmac.compare_digest for every secret/digest comparison; string=?
;; short-circuits on the first differing byte, so compare full buffers instead.

(define (constant-time-string=? left right)
  (and (string? left)
       (string? right)
       (let* ([a (string->bytes/utf-8 left)]
              [b (string->bytes/utf-8 right)]
              [a-len (bytes-length a)]
              [b-len (bytes-length b)]
              [limit (max a-len b-len)])
         (zero? (for/fold ([acc (bitwise-xor a-len b-len)]) ([i (in-range limit)])
                  (bitwise-ior acc
                               (bitwise-xor (if (< i a-len)
                                                (bytes-ref a i)
                                                0)
                                            (if (< i b-len)
                                                (bytes-ref b i)
                                                0))))))))

(define (sha256-hex-of-string text)
  (bytes->hex-string (sha256-bytes (string->bytes/utf-8 text))))

(define hmac-block-size 64)

(define (bytes-xor-constant bs fill)
  (let ([out (make-bytes (bytes-length bs))])
    (for ([i (in-range (bytes-length bs))])
      (bytes-set! out i (bitwise-xor (bytes-ref bs i) fill)))
    out))

;; HMAC-SHA256 (RFC 2104) over the platform SHA-256; the port keeps its own copy
;; because the openssl binding exposes only the plain digest.
(define (hmac-sha256 key message)
  (define padded
    (let* ([k (if (> (bytes-length key) hmac-block-size)
                  (sha256-bytes key)
                  key)])
      (bytes-append k (make-bytes (- hmac-block-size (bytes-length k)) 0))))
  (define inner (sha256-bytes (bytes-append (bytes-xor-constant padded #x36) message)))
  (sha256-bytes (bytes-append (bytes-xor-constant padded #x5c) inner)))

;; ================================================================ jsexpr keys
;; `read-json` yields hashes keyed by symbols; Python dicts are keyed by strings.
;; Every model loader normalises once at its entry so the rest of the module has
;; exactly one key domain, and canonical JSON (which also accepts symbol keys)
;; renders the same bytes either way.

(define (object-key-string key)
  (cond
    [(string? key) key]
    [(symbol? key) (symbol->string key)]
    [else #f]))

(define (normalise-jsexpr value)
  (cond
    [(hash? value)
     (for/fold ([acc (hash)]) ([(key item) (in-hash value)])
       (define key* (object-key-string key))
       (unless key*
         (error 'normalise-jsexpr "object key is not a string or symbol: ~e" key))
       (hash-set acc key* (normalise-jsexpr item)))]
    [(list? value) (map normalise-jsexpr value)]
    [(vector? value) (list->vector (map normalise-jsexpr (vector->list value)))]
    [else value]))

;; ================================================================ python reprs
;; `_exact_keys` renders Python list/set reprs into its message, e.g.
;; "has missing ['base_ref'], unknown ['bogus']".  Reproduce repr() for strings.

(define (padded-hex-string number width)
  (define digits (string-downcase (number->string number 16)))
  (string-append (make-string (max 0 (- width (string-length digits))) #\0) digits))

(define (python-string-repr value)
  (define quote (if (and (string-contains? value "'") (not (string-contains? value "\""))) "\"" "'"))
  (define out (open-output-string))
  (display quote out)
  (for ([ch (in-string value)])
    (define code (char->integer ch))
    (cond
      [(char=? ch (string-ref quote 0))
       (display "\\" out)
       (write-char ch out)]
      [(char=? ch #\\) (display "\\\\" out)]
      [(char=? ch #\newline) (display "\\n" out)]
      [(char=? ch #\return) (display "\\r" out)]
      [(char=? ch #\tab) (display "\\t" out)]
      [(or (< code #x20) (= code #x7f))
       (display "\\x" out)
       (display (padded-hex-string code 2) out)]
      [else (write-char ch out)]))
  (display quote out)
  (get-output-string out))

(define (python-list-repr values)
  (string-append "[" (string-join (map python-string-repr values) ", ") "]"))

;; ================================================================ canonical JSON
;; Python: json.dumps(value, sort_keys=True, separators=(",", ":"),
;;                     ensure_ascii=False, allow_nan=False)
;; Keys sort by code point (Python str ordering), NOT by UTF-16 units, and floats
;; are emitted with Python's repr -- hence a local canonicaliser instead of
;; util/json/canonical-json.rkt (which is integer-only and sorts by UTF-16).

(define float-text-rx #px"^([0-9]*)(?:\\.([0-9]+))?(?:e([+-]?[0-9]+))?$")

;; Re-render a finite double the way CPython's repr does: shortest round-trip
;; digits, exponential notation outside 1e-4 <= |x| < 1e17, two-digit exponent.
;; Racket's shortest text supplies the *length*; the last digit is then
;; re-derived from the exact binary value (round-to-nearest, ties-to-even) so
;; tie cases choose the same digit CPython's dtoa chooses.
(define (python-float-text value)
  (define text (number->string value))
  (define negative-text? (and (positive? (string-length text)) (char=? (string-ref text 0) #\-)))
  (define body
    (if negative-text?
        (substring text 1)
        text))
  (define match (regexp-match float-text-rx body))
  (unless match
    (error 'python-float-text "unexpected float text: ~a" text))
  (define whole (or (list-ref match 1) ""))
  (define fraction (or (list-ref match 2) ""))
  (define exponent-text (list-ref match 3))
  (define exponent
    (if exponent-text
        (string->number exponent-text)
        0))
  (define raw-digits (string-append whole fraction))
  (define raw-decpt (+ (string-length whole) exponent))
  (define-values (digits decpt)
    (if (zero? value)
        (values "0" 1)
        (shortest-digits value raw-digits raw-decpt)))
  (define sign (if negative-text? "-" ""))
  (cond
    [(zero? (string-length digits)) (string-append sign "0.0")]
    [(or (<= decpt -4) (> decpt 16))
     (define magnitude (sub1 decpt))
     (define mantissa
       (if (= 1 (string-length digits))
           digits
           (string-append (substring digits 0 1) "." (substring digits 1))))
     (string-append sign
                    mantissa
                    "e"
                    (if (negative? magnitude) "-" "+")
                    (padded-decimal-digits (abs magnitude)))]
    [(<= decpt 0) (string-append sign "0." (make-string (- decpt) #\0) digits)]
    [(>= decpt (string-length digits))
     (string-append sign digits (make-string (- decpt (string-length digits)) #\0) ".0")]
    [else (string-append sign (substring digits 0 decpt) "." (substring digits decpt))]))

;; Strip leading zeros from Racket's shortest digit text (adjusting the decimal
;; point), then derive CPython's digits: the fewest significant digits that still
;; round-trip, last digit correctly rounded (ties to even).
(define (shortest-digits value raw-digits raw-decpt)
  (define-values (digits decpt)
    (let loop ([index 0])
      (cond
        [(= index (string-length raw-digits)) (values "" raw-decpt)]
        [(char=? (string-ref raw-digits index) #\0) (loop (add1 index))]
        [else (values (substring raw-digits index) (- raw-decpt index))])))
  (define limit (add1 (string-length digits)))
  (let loop ([count 1])
    (when (> count limit)
      (error 'shortest-digits "no round-tripping decimal found for ~a" value))
    (define exponent (- decpt count))
    (define rounded (abs (round (* (inexact->exact value) (expt 10 (- exponent))))))
    (define text (number->string rounded))
    (define-values (trimmed placed)
      (let* ([position (+ (string-length text) exponent)]
             [stripped (let strip ([candidate text])
                         (cond
                           [(zero? (string-length candidate)) candidate]
                           [(char=? (string-ref candidate (sub1 (string-length candidate))) #\0)
                            (strip (substring candidate 0 (sub1 (string-length candidate))))]
                           [else candidate]))])
        (values stripped position)))
    (define round-trip
      (string->number
       (string-append trimmed "e" (number->string (- placed (string-length trimmed))))))
    (if (and round-trip (= round-trip (abs value)))
        (values trimmed placed)
        (loop (add1 count)))))

;; Exponents are decimal with at least two digits ("e+16", "e-05", "e-324").
(define (padded-decimal-digits magnitude)
  (define digits (number->string magnitude))
  (string-append (make-string (max 0 (- 2 (string-length digits))) #\0) digits))

(define (write-canonical-string! out value)
  (write-char #\" out)
  (for ([ch (in-string value)])
    (define code (char->integer ch))
    (cond
      [(char=? ch #\") (display "\\\"" out)]
      [(char=? ch #\\) (display "\\\\" out)]
      [(char=? ch #\backspace) (display "\\b" out)]
      [(char=? ch #\page) (display "\\f" out)]
      [(char=? ch #\newline) (display "\\n" out)]
      [(char=? ch #\return) (display "\\r" out)]
      [(char=? ch #\tab) (display "\\t" out)]
      ;; ensure_ascii=False still escapes ASCII controls and lone surrogates
      ;; (the latter cannot be encoded as UTF-8).
      [(or (< code #x20) (<= #xd800 code #xdfff))
       (display "\\u" out)
       (display (padded-hex-string code 4) out)]
      [else (write-char ch out)]))
  (write-char #\" out))

(define (unrepresentable! label kind detail)
  (fail kind (format "~a is not canonical JSON: ~a" label detail)))

(define (write-canonical-value! out value label kind)
  (cond
    [(eq? value 'null) (display "null" out)]
    [(boolean? value) (display (if value "true" "false") out)]
    [(exact-integer? value) (display (number->string value) out)]
    [(flonum? value)
     (when (or (nan? value) (infinite? value))
       (unrepresentable! label
                         kind
                         (format "Out of range float values are not JSON compliant: ~a"
                                 (cond
                                   [(nan? value) "nan"]
                                   [(positive? value) "inf"]
                                   [else "-inf"]))))
     (display (python-float-text value) out)]
    [(string? value) (write-canonical-string! out value)]
    [(or (list? value) (vector? value))
     (define items
       (if (list? value)
           value
           (vector->list value)))
     (display "[" out)
     (for ([item (in-list items)]
           [index (in-naturals)])
       (unless (zero? index)
         (display "," out))
       (write-canonical-value! out item label kind))
     (display "]" out)]
    [(hash? value)
     (define keys
       (map (lambda (key)
              (define key* (object-key-string key))
              (unless key*
                (unrepresentable! label kind "object keys must be strings"))
              key*)
            (hash-keys value)))
     (when (> (length keys) (length (remove-duplicates keys)))
       (unrepresentable! label kind "object has duplicate keys after normalisation"))
     (display "{" out)
     (for ([key (in-list (sort keys string<?))]
           [index (in-naturals)])
       (unless (zero? index)
         (display "," out))
       (write-canonical-string! out key)
       (display ":" out)
       (write-canonical-value! out
                               (hash-ref value key (hash-ref value (string->symbol key) 'null))
                               label
                               kind))
     (display "}" out)]
    [else (unrepresentable! label kind (format "value is not JSON-representable: ~e" value))]))

(define (kind-string kind)
  (cond
    [(string? kind) kind]
    [(symbol? kind) (symbol->string kind)]
    [else (kind-string (format "~a" kind))]))

;; finalizer-canonical-json : jsexpr? [string? (or/c string? symbol?)] -> string?
;;   Canonical (Python-literal) JSON text.  `label` and `kind` shape the refusal,
;;   which is always an exn:fail:finalizer of that kind.
(define (finalizer-canonical-json value [label "value"] [kind "ValidationError"])
  (define out (open-output-string))
  (write-canonical-value! out value label (kind-string kind))
  (get-output-string out))

;; ================================================================ field readers
;; `_mapping` / `_exact_keys` / `_string` / `_positive_int` / `_sha`, parameterised
;; by the error kind (Python passed `error_type` for the same reason).

(define (model-mapping value label kind)
  (unless (hash? value)
    (fail kind (format "~a must be an object" label)))
  (normalise-jsexpr value))

(define (model-exact-keys value required optional label kind)
  (define present (hash-keys value))
  (define missing (sort (filter (lambda (key) (not (member key present))) required) string<?))
  (define unknown
    (sort (filter (lambda (key) (not (or (member key required) (member key optional)))) present)
          string<?))
  (unless (and (null? missing) (null? unknown))
    (define parts
      (append (if (null? missing)
                  '()
                  (list (format "missing ~a" (python-list-repr missing))))
              (if (null? unknown)
                  '()
                  (list (format "unknown ~a" (python-list-repr unknown))))))
    (fail kind (format "~a has ~a" label (string-join parts ", "))))
  value)

(define (model-string value label kind)
  (unless (and (string? value) (positive? (string-length value)) (string=? value (string-trim value)))
    (fail kind (format "~a must be a nonempty, trimmed string" label)))
  (string->immutable-string value))

;; Booleans are not integers in Racket, so `exact-integer?` alone rejects them;
;; the `boolean?` guard documents the Python `isinstance(value, bool)` rule.
(define (model-positive-int value label kind)
  (unless (and (not (boolean? value)) (exact-integer? value) (positive? value))
    (fail kind (format "~a must be a positive integer" label)))
  value)

(define sha-rx #px"^[0-9a-fA-F]{40}$")
(define digest-rx #px"^[0-9a-f]{64}$")

(define (model-sha value label kind)
  (unless (and (string? value) (regexp-match? sha-rx value))
    (fail kind (format "~a must be a full 40-character hexadecimal SHA" label)))
  (string->immutable-string value))

;; ================================================================ workflow contracts

(struct workflow-contract (context workflow ref event app-slug app-id) #:transparent)

(define (parse-workflow value label)
  (define item (model-mapping value label "ConfigurationError"))
  (model-exact-keys item
                    (list "context" "workflow" "ref" "event" "app")
                    '()
                    label
                    "ConfigurationError")
  (define identity (hash-ref item "workflow"))
  (define identity*
    (cond
      [(boolean? identity)
       (configuration-error (format "~a.workflow must be a path or positive workflow id" label))]
      [(string? identity) (model-string identity (format "~a.workflow" label) "ConfigurationError")]
      [(exact-integer? identity)
       (model-positive-int identity (format "~a.workflow" label) "ConfigurationError")]
      [else
       (configuration-error (format "~a.workflow must be a path or positive workflow id" label))]))
  (define app (model-mapping (hash-ref item "app") (format "~a.app" label) "ConfigurationError"))
  (model-exact-keys app (list "slug" "id") '() (format "~a.app" label) "ConfigurationError")
  (workflow-contract
   (model-string (hash-ref item "context") (format "~a.context" label) "ConfigurationError")
   identity*
   (model-string (hash-ref item "ref") (format "~a.ref" label) "ConfigurationError")
   (model-string (hash-ref item "event") (format "~a.event" label) "ConfigurationError")
   (model-string (hash-ref app "slug") (format "~a.app.slug" label) "ConfigurationError")
   (model-positive-int (hash-ref app "id") (format "~a.app.id" label) "ConfigurationError")))

(define (parse-workflow-list value label)
  (unless (and (list? value) (pair? value))
    (configuration-error (format "~a must be a nonempty array" label)))
  (define result
    (for/list ([item (in-list value)]
               [index (in-naturals)])
      (parse-workflow item (format "~a[~a]" label index))))
  (define contexts (map workflow-contract-context result))
  (when (> (length contexts) (length (remove-duplicates contexts)))
    (configuration-error (format "~a has duplicate contexts" label)))
  result)

;; ================================================================ trusted configuration

(struct board-sync (project-id item-id field-id value-json) #:transparent)

;; Python: BoardSync.value() -> json.loads(self.value_json); normalised here so
;; the value lives in the same (string-keyed) jsexpr domain as everything else.
(define (board-sync-value board)
  (normalise-jsexpr (read-json (open-input-string (board-sync-value-json board)))))

(struct trusted-configuration
        (repository base-ref
                    merge-method
                    attestation-issuer
                    required-workflows
                    merge-workflows
                    wave-issue-numbers
                    board
                    canonical-json
                    digest)
  #:transparent)

(define trusted-configuration-keys
  (list "schema_version"
        "repository"
        "base_ref"
        "merge_method"
        "attestation_issuer"
        "required_workflows"
        "merge_workflows"
        "wave_issue_numbers"
        "board"))

;; Parse the launcher's separate, trusted and immutable policy snapshot.
(define (load-trusted-configuration value)
  (define raw (model-mapping value "trusted configuration" "ConfigurationError"))
  (model-exact-keys raw trusted-configuration-keys '() "trusted configuration" "ConfigurationError")
  (define schema-version (hash-ref raw "schema_version"))
  (unless (and (not (boolean? schema-version)) (exact-integer? schema-version) (= 1 schema-version))
    (configuration-error "unsupported trusted configuration schema_version"))
  (define repository (model-string (hash-ref raw "repository") "repository" "ConfigurationError"))
  ;; Python: repository.count("/") != 1 or any(not part for part in repository.split("/"))
  (unless (regexp-match? #px"^[^/]+/[^/]+$" repository)
    (configuration-error "repository must be owner/name"))
  (define base-ref (model-string (hash-ref raw "base_ref") "base_ref" "ConfigurationError"))
  (define merge-method
    (model-string (hash-ref raw "merge_method") "merge_method" "ConfigurationError"))
  (unless (member merge-method (list "squash" "rebase"))
    (configuration-error "merge_method must preserve linear history"))
  (define required-workflows
    (parse-workflow-list (hash-ref raw "required_workflows") "required_workflows"))
  (define merge-workflows (parse-workflow-list (hash-ref raw "merge_workflows") "merge_workflows"))
  (when (ormap (lambda (item) (not (string=? (workflow-contract-event item) "pull_request")))
               required-workflows)
    (configuration-error "pre-merge workflows must use the pull_request event"))
  (when (ormap (lambda (item)
                 (not (and (string=? (workflow-contract-event item) "push")
                           (string=? (workflow-contract-ref item) base-ref))))
               merge-workflows)
    (configuration-error "merge CI must use push on the exact base_ref"))
  (define issue-values (hash-ref raw "wave_issue_numbers"))
  (unless (and (list? issue-values) (pair? issue-values))
    (configuration-error "wave_issue_numbers must be a nonempty array"))
  (define issues
    (for/list ([item (in-list issue-values)]
               [index (in-naturals)])
      (model-positive-int item (format "wave_issue_numbers[~a]" index) "ConfigurationError")))
  (when (> (length issues) (length (remove-duplicates issues)))
    (configuration-error "wave_issue_numbers must be unique"))
  (define board-raw (model-mapping (hash-ref raw "board") "board" "ConfigurationError"))
  (model-exact-keys board-raw
                    (list "project_id" "item_id" "field_id" "value")
                    '()
                    "board"
                    "ConfigurationError")
  (define value-json
    (finalizer-canonical-json (hash-ref board-raw "value") "board.value" "ConfigurationError"))
  (define board
    (board-sync
     (model-string (hash-ref board-raw "project_id") "board.project_id" "ConfigurationError")
     (model-string (hash-ref board-raw "item_id") "board.item_id" "ConfigurationError")
     (model-string (hash-ref board-raw "field_id") "board.field_id" "ConfigurationError")
     value-json))
  (define canonical (finalizer-canonical-json raw "trusted configuration" "ConfigurationError"))
  (trusted-configuration
   repository
   base-ref
   merge-method
   (model-string (hash-ref raw "attestation_issuer") "attestation_issuer" "ConfigurationError")
   required-workflows
   merge-workflows
   issues
   board
   canonical
   (sha256-hex-of-string canonical)))

;; ================================================================ external envelope

(struct external-projection (pull-number head-sha) #:transparent)

(struct verified-envelope
        (projection issuer
                    evidence-kind
                    evidence-id
                    digest
                    projection-digest
                    canonical-json
                    [canonical-self-consistent #:auto])
  #:transparent
  #:auto-value #t)

;; Python: VerifiedEnvelope.evidence_reference(); string keys like every other
;; jsexpr value this module hands out.
(define (verified-envelope-evidence-reference envelope)
  (hash "kind"
        (verified-envelope-evidence-kind envelope)
        "id"
        (verified-envelope-evidence-id envelope)))

;; Compute only the envelope's canonical self-consistency digest.
(define (compute-envelope-digest envelope)
  (define raw (model-mapping envelope "envelope" "EnvelopeError"))
  (define covered
    (for/fold ([acc (hash)])
              ([(key item) (in-hash raw)]
               #:unless (string=? key "canonical_digest"))
      (hash-set acc key item)))
  (sha256-hex-of-string (finalizer-canonical-json covered "envelope" "EnvelopeError")))

(define (verify-external-envelope value)
  (define raw (model-mapping value "envelope" "EnvelopeError"))
  (model-exact-keys raw
                    (list "schema_version" "issuer" "projection" "evidence" "canonical_digest")
                    '()
                    "envelope"
                    "EnvelopeError")
  (define schema-version (hash-ref raw "schema_version"))
  (unless (and (not (boolean? schema-version)) (exact-integer? schema-version) (= 1 schema-version))
    (envelope-error "unsupported envelope schema_version"))
  (define projection-raw (model-mapping (hash-ref raw "projection") "projection" "EnvelopeError"))
  (model-exact-keys projection-raw (list "pull_number" "head_sha") '() "projection" "EnvelopeError")
  (define projection
    (external-projection
     (model-positive-int (hash-ref projection-raw "pull_number")
                         "projection.pull_number"
                         "EnvelopeError")
     (model-sha (hash-ref projection-raw "head_sha") "projection.head_sha" "EnvelopeError")))
  (define evidence (model-mapping (hash-ref raw "evidence") "evidence" "EnvelopeError"))
  (model-exact-keys evidence (list "kind" "id") '() "evidence" "EnvelopeError")
  (define kind (model-string (hash-ref evidence "kind") "evidence.kind" "EnvelopeError"))
  (unless (member kind (list "artifact" "check"))
    (envelope-error "evidence.kind must be artifact or check"))
  (define digest (model-mapping (hash-ref raw "canonical_digest") "canonical_digest" "EnvelopeError"))
  (model-exact-keys digest (list "algorithm" "value") '() "canonical_digest" "EnvelopeError")
  (define algorithm (hash-ref digest "algorithm"))
  (unless (and (string? algorithm) (string=? algorithm "sha256"))
    (envelope-error "canonical_digest.algorithm must be sha256"))
  (define supplied (hash-ref digest "value"))
  (unless (and (string? supplied) (regexp-match? digest-rx supplied))
    (envelope-error "canonical_digest.value must be lowercase SHA-256"))
  (define calculated (compute-envelope-digest raw))
  (unless (constant-time-string=? supplied calculated)
    (envelope-error "envelope canonical self-consistency mismatch"))
  (verified-envelope
   projection
   (model-string (hash-ref raw "issuer") "issuer" "EnvelopeError")
   kind
   (model-positive-int (hash-ref evidence "id") "evidence.id" "EnvelopeError")
   (string->immutable-string supplied)
   (sha256-hex-of-string (finalizer-canonical-json projection-raw "projection" "EnvelopeError"))
   (finalizer-canonical-json raw "envelope" "EnvelopeError")))

;; ================================================================ GitHub API reads
;; These never touch the network: the adapter supplies already-fetched payloads.

(define (api-mapping value label)
  (unless (hash? value)
    (uncertain (format "~a response is not an object" label)))
  (normalise-jsexpr value))

(define (api-items value label)
  (define raw
    (cond
      [(hash? value)
       (define items-value (normalise-jsexpr value))
       (unless (and (= 2 (hash-count items-value))
                    (hash-has-key? items-value "items")
                    (hash-has-key? items-value "complete")
                    (eq? (hash-ref items-value "complete") #t))
         (uncertain (format "~a collection is not explicitly complete" label)))
       (hash-ref items-value "items")]
      [else value]))
  (unless (list? raw)
    (uncertain (format "~a is not a complete sequence" label)))
  (for/list ([item (in-list raw)]
             [index (in-naturals)])
    (unless (hash? item)
      (uncertain (format "~a[~a] is malformed" label index)))
    (normalise-jsexpr item)))

(define (api-nested value path label)
  (define path*
    (map (lambda (key)
           (if (string? key)
               key
               (symbol->string key)))
         path))
  (define full-path (string-join path* "."))
  (let loop ([current value]
             [remaining path*])
    (cond
      [(null? remaining) current]
      [(and (hash? current) (hash-has-key? (normalise-jsexpr current) (car remaining)))
       (loop (hash-ref (normalise-jsexpr current) (car remaining)) (cdr remaining))]
      [else (uncertain (format "~a is missing ~a" label full-path))])))

(define (api-bool value label)
  (unless (boolean? value)
    (uncertain (format "~a is not boolean" label)))
  value)

(define (api-positive-int value label)
  (unless (and (not (boolean? value)) (exact-integer? value) (positive? value))
    (uncertain (format "~a is not a positive integer" label)))
  value)

;; ================================================================ timestamps

;; Python's `_time` first applies `.replace("Z", "+00:00")` and only accepts an
;; uppercase Z; a lowercase z is refused by CPython's parser, so it is refused
;; here too.  Offsets are the GitHub shape (+hh:mm / +hhmm).
(define timestamp-rx
  #px"^([0-9]{4})-([0-9]{2})-([0-9]{2})(?:[Tt ]([0-9]{2}):([0-9]{2})(?::([0-9]{2})(?:[.,]([0-9]+))?)?)?(Z|[+-][0-9]{2}:?[0-9]{2})?$")

(define (leap-year? year)
  (and (zero? (modulo year 4)) (or (not (zero? (modulo year 100))) (zero? (modulo year 400)))))

(define (days-in-month year month)
  (cond
    [(= month 2) (if (leap-year? year) 29 28)]
    [(member month (list 4 6 9 11)) 30]
    [else 31]))

;; Howard Hinnant's days_from_civil: days since 1970-01-01 for a valid date.
(define (civil-days year month day)
  (define y
    (if (<= month 2)
        (sub1 year)
        year))
  (define era
    (quotient (if (>= y 0)
                  y
                  (- y 399))
              400))
  (define yoe (- y (* era 400)))
  (define mp
    (if (> month 2)
        (- month 3)
        (+ month 9)))
  (define doy (+ (quotient (+ (* 153 mp) 2) 5) (sub1 day)))
  (define doe (+ (* yoe 365) (quotient yoe 4) (- (quotient yoe 100)) doy))
  (- (+ (* era 146097) doe) 719468))

;; Python: datetime.fromisoformat(value.replace("Z", "+00:00")) then
;; astimezone(timezone.utc); the port keeps an exact integer instead of a
;; datetime -- microseconds since the Unix epoch, UTC.  Python truncates
;; fractional digits beyond microseconds, so the same truncation is applied.
(define (parse-timestamp value [label "timestamp"])
  (unless (string? value)
    (uncertain (format "~a is not a timestamp" label)))
  (define match (regexp-match timestamp-rx value))
  (unless match
    (uncertain (format "~a is malformed" label)))
  (define (part index)
    (list-ref match index))
  (define year (string->number (part 1)))
  (define month (string->number (part 2)))
  (define day (string->number (part 3)))
  (define hour-text (part 4))
  (define minute-text (part 5))
  (define second-text (part 6))
  (define fraction-text (part 7))
  (define offset-text (part 8))
  (define (malformed)
    (uncertain (format "~a is malformed" label)))
  (when (or (< month 1) (> month 12) (< day 1) (> day (days-in-month year month)))
    (malformed))
  (define hour
    (if hour-text
        (string->number hour-text)
        0))
  (define minute
    (if minute-text
        (string->number minute-text)
        0))
  (define second
    (if second-text
        (string->number second-text)
        0))
  (when (or (> hour 23) (> minute 59) (> second 59))
    (malformed))
  (define offset-seconds
    (cond
      [(not offset-text) #f]
      [(string=? offset-text "Z") 0]
      [else
       (define sign (if (char=? (string-ref offset-text 0) #\-) -1 1))
       (define digits (string-replace offset-text ":" ""))
       (define hours (string->number (substring digits 1 3)))
       (define minutes (string->number (substring digits 3 5)))
       (when (or (> hours 23) (> minutes 59))
         (malformed))
       (* sign (+ (* hours 3600) (* minutes 60)))]))
  ;; A missing time part and a missing offset both mean "no timezone" in Python,
  ;; which refuses with its own message rather than calling the value malformed.
  (unless (and hour-text offset-seconds)
    (uncertain (format "~a has no timezone" label)))
  (define micros
    (if fraction-text
        (string->number
         (string-append (substring fraction-text 0 (min 6 (string-length fraction-text)))
                        (make-string (max 0 (- 6 (string-length fraction-text))) #\0)))
        0))
  (define whole-seconds
    (- (+ (* (civil-days year month day) 86400) (* hour 3600) (* minute 60) second) offset-seconds))
  (+ (* whole-seconds 1000000) micros))

;; ================================================================ branch protection
;; `parse-protection-policy` keeps this port's original messages (the native
;; differential test pins kinds against `_parse_policy`).

(struct protection-policy (required-contexts app-ids) #:transparent)

;; Python: dict(policy.app_ids)[context]; the caller has already proven the
;; context is pinned, so reaching the error branch means the policy was forged.
(define (protection-policy-app-id-for policy context)
  (define entry (assoc context (protection-policy-app-ids policy)))
  (unless entry
    (error 'protection-policy-app-id-for "no App ID is pinned for context: ~a" context))
  (cdr entry))

(define (object value label)
  (unless (hash? value)
    (uncertain (format "~a is not an object" label)))
  value)
(define (nested value key label)
  (unless (and (hash? value) (hash-has-key? value key))
    (uncertain (format "~a is missing ~a" label key)))
  (hash-ref value key))
(define (enabled protection key)
  (api-bool (nested (nested protection key "branch protection") 'enabled "branch protection")
            (format "~a.enabled" key)))

(define (parse-protection-policy value)
  (define protection (object (normalise-for-policy value) "branch protection"))
  (define required (nested protection 'required_status_checks "branch protection"))
  (unless (hash? required)
    (uncertain "required_status_checks is malformed"))
  (unless (api-bool (hash-ref required 'strict 'null) "required_status_checks.strict")
    (policy-error "required status checks must be strict"))
  (define contexts (hash-ref required 'contexts #f))
  (define checks (hash-ref required 'checks #f))
  (unless (and (list? contexts) (list? checks))
    (uncertain "required status check contexts/checks are malformed"))
  (define declared (make-hash))
  (define app-ids (make-hash))
  (for ([context (in-list contexts)])
    (unless (and (string? context) (not (string=? context "")))
      (uncertain "legacy required status context is malformed"))
    (hash-set! declared context #t))
  (for ([check (in-list checks)])
    (unless (and (hash? check)
                 (= (hash-count check) 2)
                 (hash-has-key? check 'context)
                 (hash-has-key? check 'app_id))
      (uncertain "required status check is malformed"))
    (define context (hash-ref check 'context))
    (unless (and (string? context) (not (string=? context "")))
      (uncertain "required status check context is malformed"))
    (define app-id (hash-ref check 'app_id))
    (when (eq? app-id 'null)
      (policy-error "legacy unpinned required status checks are forbidden"))
    (api-positive-int app-id "required status check app_id")
    (when (and (hash-has-key? app-ids context) (not (= (hash-ref app-ids context) app-id)))
      (policy-error "required context has conflicting App IDs"))
    (hash-set! app-ids context app-id)
    (hash-set! declared context #t))
  (when (zero? (hash-count declared))
    (policy-error "required status check policy must be nonempty"))
  (unless (for/and ([context (in-hash-keys declared)])
            (hash-has-key? app-ids context))
    (policy-error "every required context must have a non-null App ID"))
  (unless (enabled protection 'enforce_admins)
    (policy-error "branch protection must include administrators"))
  (unless (enabled protection 'required_linear_history)
    (policy-error "linear history must be required"))
  (when (enabled protection 'allow_force_pushes)
    (policy-error "force pushes must be disabled"))
  (when (enabled protection 'allow_deletions)
    (policy-error "branch deletion must be disabled"))
  (define sorted (map string->immutable-string (sort (hash-keys declared) string<?)))
  (protection-policy sorted
                     (for/list ([context (in-list sorted)])
                       (cons context (hash-ref app-ids context)))))

;; The policy path is keyed by symbols (it predates the model layer's string-key
;; normalisation); convert string keys to symbols so both spellings work.
(define (normalise-for-policy value)
  (cond
    [(hash? value)
     (for/fold ([acc (hash)]) ([(key item) (in-hash value)])
       (define key*
         (cond
           [(symbol? key) key]
           [(string? key) (string->symbol key)]
           [else key]))
       (hash-set acc key* (normalise-for-policy item)))]
    [(list? value) (map normalise-for-policy value)]
    [else value]))

;; ================================================================ typed equality
;; Python: _typed_equal -- `type(left) is not type(right)` first, so 1 and True
;; (and 1 and 1.0) never compare equal; dicts compare by key set, lists by shape.

(define (typed-equal? left right)
  (cond
    [(and (hash? left) (hash? right))
     (and (= (hash-count left) (hash-count right))
          (for/and ([(key value) (in-hash left)])
            (and (hash-has-key? right key) (typed-equal? value (hash-ref right key)))))]
    [(and (list? left) (list? right))
     (and (= (length left) (length right)) (andmap typed-equal? left right))]
    [(and (vector? left) (vector? right)) (typed-equal? (vector->list left) (vector->list right))]
    [(or (hash? left) (hash? right) (list? left) (list? right) (vector? left) (vector? right)) #f]
    [else (equal? left right)]))

;; ================================================================ prepared finalization
;; Python: frozen dataclass + per-process HMAC key and a weakref identity
;; registry, so a hand-built lookalike (or a `dataclasses.replace` copy) is
;; refused even when every field was copied from a genuine value.
;; Racket structures are immutable, so the seal detects a *material* mismatch
;; (forged field values carrying a genuine seal); the identity registry detects
;; values that were never issued by this process.

(struct prepared-finalization
        (config-json envelope-json
                     config-digest
                     envelope-digest
                     attestation-digest
                     head-sha
                     policy
                     premerge-run-ids
                     already-merged
                     seal)
  #:transparent
  #:authentic)

;; Per-process seal key (Python: `secrets.token_bytes(32)`), 32 CSPRNG bytes.
;;
;; This Racket installation has neither `racket/random` nor `racket/crypto`, so
;; the key comes from the OS CSPRNG.  `/dev/urandom` is read in binary mode on
;; POSIX; if that is unavailable the key falls back to the platform PRNG folded
;; with process timing, which is WEAKER and is reported through the exported
;; `prepared-key-source` so a caller (and the tests) can see which one is in use.
;; An attacker able to run code in this process can read the key either way --
;; the same trust boundary as the Python original.
(define (read-urandom count)
  (define path "/dev/urandom")
  (and (file-exists? path)
       (with-handlers ([exn:fail? (lambda (_error) #f)])
         (call-with-input-file
          path
          (lambda (in) (read-bytes count in))
          #:mode 'binary))))

(define (timing-fill count)
  (sha256-bytes
   (string->bytes/utf-8 (format "~a|~a|~a|~a"
                                (current-milliseconds)
                                (current-process-milliseconds)
                                (random)
                                count))))

(define-values (prepared-key prepared-key-source)
  (let* ([from-os (read-urandom 32)])
    (values (if (and from-os (= 32 (bytes-length from-os)))
                from-os
                (timing-fill 32))
            (if (and from-os (= 32 (bytes-length from-os))) 'dev-urandom 'prng-fallback))))
(define issued-prepared (box '()))

(define (issued-prepared-boxes)
  (for/list ([box (in-list (unbox issued-prepared))]
             #:when (weak-box-value box))
    box))

(define (register-prepared! value)
  (set-box! issued-prepared (cons (make-weak-box value) (issued-prepared-boxes))))

(define (issued-prepared-value? value)
  (for/or ([box (in-list (issued-prepared-boxes))])
    (eq? (weak-box-value box) value)))

;; Python: _policy_dict; every invariant below is enforced by
;; `parse-protection-policy`, so the literals match a parsed policy exactly.
(define (policy-dict policy)
  (hash "required_contexts"
        (protection-policy-required-contexts policy)
        "app_ids"
        (for/list ([entry (in-list (protection-policy-app-ids policy))])
          (list (car entry) (cdr entry)))
        "strict"
        #t
        "admins_enforced"
        #t
        "linear_history"
        #t
        "force_pushes_allowed"
        #f
        "deletions_allowed"
        #f))

(define (prepared-material config-json
                           envelope-json
                           config-digest
                           envelope-digest
                           attestation-digest
                           head-sha
                           policy
                           run-ids
                           already-merged)
  (finalizer-canonical-json (hash "config_json"
                                  config-json
                                  "envelope_json"
                                  envelope-json
                                  "config_digest"
                                  config-digest
                                  "envelope_digest"
                                  envelope-digest
                                  "attestation_digest"
                                  attestation-digest
                                  "head_sha"
                                  head-sha
                                  "policy"
                                  (policy-dict policy)
                                  "premerge_run_ids"
                                  run-ids
                                  "already_merged"
                                  already-merged)
                            "prepared finalization"
                            "ValidationError"))

(define (compute-prepared-seal config-json
                               envelope-json
                               config-digest
                               envelope-digest
                               attestation-digest
                               head-sha
                               policy
                               run-ids
                               already-merged)
  (bytes->hex-string (hmac-sha256 prepared-key
                                  (string->bytes/utf-8 (prepared-material config-json
                                                                          envelope-json
                                                                          config-digest
                                                                          envelope-digest
                                                                          attestation-digest
                                                                          head-sha
                                                                          policy
                                                                          run-ids
                                                                          already-merged)))))

;; The only construction path exposed to the controller layer: the seal is
;; derived from the supplied material and the value is registered as issued.
;;
;; CAPABILITY NOTE (review finding): this is exported, so any module in the
;; process can mint a correctly sealed and registered value for arbitrary
;; material without running attestation.  It is therefore a prepare-equivalent
;; capability, not a constructor: `finalize-prepared` re-derives the trusted
;; configuration, the envelope, the attestation, the branch protection policy and
;; the pre-merge run ids from the client and compares them against the sealed
;; fields, so a minted value is refused unless the real verifications also pass.
;; Keeping it exported is deliberate -- the controller lives in another module --
;; and the residual risk (defence-in-depth only, no gate bypass) is recorded here
;; rather than hidden.
(define (issue-prepared-finalization config-json
                                     envelope-json
                                     config-digest
                                     envelope-digest
                                     attestation-digest
                                     head-sha
                                     policy
                                     run-ids
                                     already-merged)
  (define prepared
    (prepared-finalization config-json
                           envelope-json
                           config-digest
                           envelope-digest
                           attestation-digest
                           head-sha
                           policy
                           run-ids
                           already-merged
                           (compute-prepared-seal config-json
                                                  envelope-json
                                                  config-digest
                                                  envelope-digest
                                                  attestation-digest
                                                  head-sha
                                                  policy
                                                  run-ids
                                                  already-merged)))
  (register-prepared! prepared)
  prepared)

(define (read-prepared-snapshot text)
  (unless (string? text)
    (validation-error "PreparedFinalization snapshots are malformed"))
  (with-handlers ([exn:fail? (lambda (exception)
                               (validation-error "PreparedFinalization snapshots are malformed"))])
    (normalise-jsexpr (read-json (open-input-string text)))))

;; Python: _validate_prepared -- re-derive the seal, re-parse both snapshots with
;; the same loaders, then re-check the digests and the head binding.
(define (validate-prepared-finalization prepared)
  (unless (prepared-finalization? prepared)
    (validation-error "finalize requires an authentic PreparedFinalization"))
  (unless (eq? (prepared-finalization-already-merged prepared) #f)
    (validation-error "PreparedFinalization must originate from an unmerged pull request"))
  (unless (issued-prepared-value? prepared)
    (validation-error "PreparedFinalization was not issued by prepare_finalization"))
  (define expected-seal
    (compute-prepared-seal (prepared-finalization-config-json prepared)
                           (prepared-finalization-envelope-json prepared)
                           (prepared-finalization-config-digest prepared)
                           (prepared-finalization-envelope-digest prepared)
                           (prepared-finalization-attestation-digest prepared)
                           (prepared-finalization-head-sha prepared)
                           (prepared-finalization-policy prepared)
                           (prepared-finalization-premerge-run-ids prepared)
                           (prepared-finalization-already-merged prepared)))
  (define supplied-seal (prepared-finalization-seal prepared))
  (unless (constant-time-string=? supplied-seal expected-seal)
    (validation-error "PreparedFinalization seal is invalid"))
  (define config
    (load-trusted-configuration (read-prepared-snapshot (prepared-finalization-config-json
                                                         prepared))))
  (define envelope
    (verify-external-envelope (read-prepared-snapshot (prepared-finalization-envelope-json
                                                       prepared))))
  (unless (and (constant-time-string=? (trusted-configuration-digest config)
                                       (prepared-finalization-config-digest prepared))
               (constant-time-string=? (verified-envelope-digest envelope)
                                       (prepared-finalization-envelope-digest prepared)))
    (validation-error "PreparedFinalization snapshot digest mismatch"))
  (unless (constant-time-string=?
           (external-projection-head-sha (verified-envelope-projection envelope))
           (prepared-finalization-head-sha prepared))
    (validation-error "PreparedFinalization head-binding mismatch"))
  (values config envelope))

;; ================================================================ result

(struct finalization-result
        (repository pull-number
                    head-sha
                    merge-sha
                    already-merged
                    merge-ci-verified
                    post-merge-synchronized
                    [canonical-self-consistency-verified #:auto]
                    [github-attestation-verified #:auto])
  #:transparent
  #:auto-value #t)

;; ================================================================ test hooks
;; Test-only forging path for tests/test-wave-finalizer-model.rkt.  A forged
;; value can only be built here, which is the documented analogue of Python's
;; same-process `dataclasses.replace`/`object.__setattr__` attacks; nothing in
;; this submodule is provided by the module itself.
(module+ for-testing
  (provide forge-prepared-finalization)
  ;; (forge-prepared-finalization <9 fields> #:register? bool #:seal #f|string)
  ;;   #:seal #f (default) seals the material as given;
  ;;   #:seal "..." reuses a seal captured from another value (mutation model).
  (define (forge-prepared-finalization config-json
                                       envelope-json
                                       config-digest
                                       envelope-digest
                                       attestation-digest
                                       head-sha
                                       policy
                                       run-ids
                                       already-merged
                                       #:register? [register? #f]
                                       #:seal [seal #f])
    (define seal*
      (or seal
          (compute-prepared-seal config-json
                                 envelope-json
                                 config-digest
                                 envelope-digest
                                 attestation-digest
                                 head-sha
                                 policy
                                 run-ids
                                 already-merged)))
    (define forged
      (prepared-finalization config-json
                             envelope-json
                             config-digest
                             envelope-digest
                             attestation-digest
                             head-sha
                             policy
                             run-ids
                             already-merged
                             seal*))
    (when register?
      (register-prepared! forged))
    forged))
