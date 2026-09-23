#lang racket/base
;; @covers scripts/ci/verify-artifact-provenance.rkt
;; @speed fast  ;; @suite workflows

;; tests/test-artifact-provenance.rkt — v1.00.31 W5 (#9728, F7).
;;
;; Red-first contract for the artifact provenance lint:
;;   - the F7 red fixture (rollback-drill prose timings vs structured
;;     eager-fallback-ms, stale recorded head) is refused with typed
;;     provenance-drift naming both sources;
;;   - canonical JSON, SHA256SUMS binding, and ancestor checks fail closed;
;;   - the real tree passes with the current wave in strict mode;
;;   - regenerating the W5 artifacts reproduces them byte-identically.

(require rackunit
         racket/file
         racket/string
         racket/system
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         rackunit/text-ui)

(define-runtime-path repo-root-rel "../")
(define repo-root (simplify-path repo-root-rel))
(define lint-path (build-path repo-root "scripts" "ci" "verify-artifact-provenance.rkt"))
(define gen-path
  (build-path repo-root "artifacts" "wave-delivery-integrity" "v1.00.31-w5" "raw" "matrix-gen.py"))

(define (run-lint! root #:current-wave [current-wave #f] #:wave-tip [wave-tip #f])
  (define out (open-output-string))
  (define args (list (path->string lint-path) "--root" (path->string root)))
  (when current-wave
    (set! args (append args (list "--current-wave" current-wave))))
  (when wave-tip
    (set! args (append args (list "--wave-tip" wave-tip))))
  (define exit-code
    (parameterize ([current-output-port out]
                   [current-directory root])
      (apply system*/exit-code (find-executable-path "racket") args)))
  (values exit-code (get-output-string out)))

(define (write-text! path text)
  (make-directory* (path-only path))
  (call-with-output-file path (lambda (out) (display text out)) #:exists 'truncate))

(require (file "../scripts/run-tests/sha256.rkt"))

(define (bytes->hex-string bs)
  (apply string-append
         (for/list ([b (in-bytes bs)])
           (let ([s (number->string b 16)])
             (if (= (string-length s) 1)
                 (string-append "0" s)
                 s)))))

(define (git*! root . args)
  (parameterize ([current-directory root])
    (apply system*/exit-code (find-executable-path "git") args)))

(define (git-rev-parse! root arg)
  (define out (open-output-string))
  (parameterize ([current-output-port out]
                 [current-directory root])
    (void (apply system*/exit-code (find-executable-path "git") (list "rev-parse" arg))))
  (string-trim (get-output-string out)))

;; Build a minimal real git repository so recorded heads can resolve and be
;; checked for ancestry.
(define (make-fixture-repo!)
  (define dir (make-temporary-file "artifact-prov-~a" 'directory))
  (git*! dir "init" "-q")
  (git*! dir "config" "user.email" "test@example.com")
  (git*! dir "config" "user.name" "Test")
  dir)

(define (commit-all! root)
  (git*! root "add" "-A")
  (git*! root "commit" "-q" "--no-gpg-sign" "-m" "fixture")
  (git-rev-parse! root "HEAD"))

(define artifact-provenance-suite
  (test-suite "artifact-provenance"

    (test-case "F7 red fixture: stale head and timing disagreement are refused"
      (define dir (make-fixture-repo!))
      (write-text! (build-path dir "artifacts" "f7-repro" "v9.99.99-w0" "rollback-drill.json")
                   (string-append
                    "{\n"
                    " \"schema\": \"prepared-env-rollback-drill@1\",\n"
                    " \"steps\": [\n"
                    "  {\n"
                    "   \"observed\": \"fallback resolved in 260 ms on the first attempt\"\n"
                    "  }\n"
                    " ],\n"
                    " \"timing\": {\n"
                    "  \"eager-fallback-ms\": [\n"
                    "   247,\n"
                    "   256\n"
                    "  ]\n"
                    " },\n"
                    " \"recorded-head\": \"0000000000000000000000000000000000000000\"\n"
                    "}\n"))
      (write-text! (build-path dir "artifacts" "f7-repro" "v9.99.99-w0" "SHA256SUMS") "")
      (define-values (code output) (run-lint! dir #:current-wave "v9.99.99-w0"))
      (check-equal? code 2 "the lint refuses the F7-shaped artifact")
      (check-true (string-contains? output "provenance-drift") "typed provenance-drift")
      (check-true (string-contains? output "prose value 260 ms disagrees with structured timing")
                  "names the prose/structured disagreement")
      (check-true (string-contains? output "timing.*-ms") "names the structured source")
      (check-true (string-contains? output "steps/0/observed") "names the prose source")
      (check-true (string-contains? output "does not resolve to a commit")
                  "refuses an unresolvable recorded head")
      (delete-directory/files dir))

    (test-case "F7: stale but resolvable recorded head is refused by ancestry"
      (define dir (make-fixture-repo!))
      (write-text! (build-path dir "seed.txt") "seed\n")
      (void (commit-all! dir))
      ;; Build an orphaned sibling: commit B on detached HEAD, then move the
      ;; branch line back to its parent and commit C. B resolves as a commit
      ;; but is not an ancestor of C.
      (write-text! (build-path dir "second.txt") "second\n")
      (define old-head (commit-all! dir))
      (git*! dir "reset" "-q" "--hard" "HEAD~1")
      (write-text! (build-path dir "third.txt") "third\n")
      (define head (commit-all! dir))
      ;; old-head resolves but is not an ancestor of HEAD.
      (define adir (build-path dir "artifacts" "f7-repro" "v9.99.99-w0"))
      (write-text! (build-path adir "matrix.json")
                   (string-append "{\n \"recorded-head\": \"" old-head "\"\n}\n"))
      (write-text! (build-path adir "SHA256SUMS") "")
      (define-values (code output) (run-lint! dir #:current-wave "v9.99.99-w0"))
      (check-equal? code 2)
      (check-true (string-contains? output "is not an ancestor of the wave tip")
                  "stale provenance head refused")
      (delete-directory/files dir))

    (test-case "R1: provenance semantics come from the last path segment"
      ;; A provenance-named leaf nested under a non-provenance ancestor must
      ;; still be checked (the recorded head is unresolvable -> refusal).
      (define dir (make-fixture-repo!))
      (define adir (build-path dir "artifacts" "prov" "v9.99.99-w0"))
      (write-text! (build-path adir "matrix.json")
                   (string-append
                    "{\n"
                    " \"provenance\": {\n"
                    "  \"recorded-head\": \"0000000000000000000000000000000000000000\"\n"
                    " }\n"
                    "}\n"))
      (write-text! (build-path adir "SHA256SUMS")
                   "deadbeef  artifacts/prov/v9.99.99-w0/matrix.json\n")
      (define-values (code output) (run-lint! dir #:current-wave "v9.99.99-w0"))
      (check-equal? code 2)
      (check-true (string-contains? output "recorded-head") "nested provenance field is checked")
      (check-true (string-contains? output "does not resolve to a commit"))
      (delete-directory/files dir))

    (test-case "SHA256SUMS tampering is refused on the current wave"
      (define dir (make-fixture-repo!))
      (define adir (build-path dir "artifacts" "prov" "v9.99.99-w0"))
      (write-text! (build-path adir "data.json") "{\n \"k\": \"v\"\n}\n")
      (write-text! (build-path adir "SHA256SUMS") "deadbeef  artifacts/prov/v9.99.99-w0/data.json\n")
      (define-values (code output) (run-lint! dir #:current-wave "v9.99.99-w0"))
      (check-equal? code 2)
      (check-true (string-contains? output "does not match the committed bytes")
                  "tampered binding refused")
      (delete-directory/files dir))

    (test-case "R3: recorded tree/head pairs are verified against the commit"
      (define dir (make-fixture-repo!))
      (write-text! (build-path dir "seed.txt") "seed\n")
      (define head (commit-all! dir))
      (define tree (git-rev-parse! dir "HEAD^{tree}"))
      (define adir (build-path dir "artifacts" "prov" "v9.99.99-w0"))
      ;; Pair 1 matches the real commit; pair 2 names a wrong tree.
      (write-text! (build-path adir "matrix.json")
                   (string-append "{\n"
                                  " \"subject\": {\n"
                                  "  \"head\": \""
                                  head
                                  "\",\n"
                                  "  \"tree\": \""
                                  tree
                                  "\"\n"
                                  " },\n"
                                  " \"subject-bad\": {\n"
                                  "  \"head\": \""
                                  head
                                  "\",\n"
                                  "  \"tree\": \"1111111111111111111111111111111111111111\"\n"
                                  " }\n"
                                  "}\n"))
      (write-text! (build-path adir "SHA256SUMS") "")
      (define-values (code output) (run-lint! dir #:current-wave "v9.99.99-w0"))
      (check-equal? code 2)
      (check-true (string-contains? output "does not match the actual tree of")
                  "wrong tree refused, naming head and tree")
      (check-equal? (length (regexp-match* #rx"does not match the actual tree" output))
                    1
                    "only the mismatched pair is flagged")
      (delete-directory/files dir))

    (test-case "R4: current-wave SHA256SUMS must be byte-identical canonical"
      ;; Every digest is individually correct, but the binding is not the
      ;; canonical regeneration (unsorted order): refused on the current
      ;; wave.
      (define dir (make-fixture-repo!))
      (define adir (build-path dir "artifacts" "prov" "v9.99.99-w0"))
      (write-text! (build-path adir "b.json") "{\n \"b\": 1\n}\n")
      (write-text! (build-path adir "a.json") "{\n \"a\": 2\n}\n")
      (define b-hex (bytes->hex-string (sha256-bytes (string->bytes/utf-8 "{\n \"b\": 1\n}\n"))))
      (define a-hex (bytes->hex-string (sha256-bytes (string->bytes/utf-8 "{\n \"a\": 2\n}\n"))))
      ;; b.json listed before a.json: valid digests, wrong canonical order.
      (write-text! (build-path adir "SHA256SUMS")
                   (string-append b-hex
                                  "  artifacts/prov/v9.99.99-w0/b.json\n"
                                  a-hex
                                  "  artifacts/prov/v9.99.99-w0/a.json\n"))
      (define-values (code output) (run-lint! dir #:current-wave "v9.99.99-w0"))
      (check-equal? code 2)
      (check-true (string-contains? output "SHA256SUMS is not canonical")
                  "non-canonical binding refused on the current wave")
      (delete-directory/files dir))

    (test-case "R6: nested non-raw JSON artifacts are checked recursively"
      ;; A bound nested artifact under a subdir cannot bypass the stale-head
      ;; check (raw/ stays exempt).
      (define dir (make-fixture-repo!))
      (define adir (build-path dir "artifacts" "prov" "v9.99.99-w0"))
      (write-text! (build-path adir "evidence" "nested.json")
                   (string-append
                    "{\n \"recorded-head\": \"0000000000000000000000000000000000000000\"\n}\n"))
      (write-text! (build-path adir "SHA256SUMS")
                   "00aa  artifacts/prov/v9.99.99-w0/evidence/nested.json\n")
      (define-values (code output) (run-lint! dir #:current-wave "v9.99.99-w0"))
      (check-equal? code 2)
      (check-true (string-contains? output "does not resolve to a commit")
                  "nested provenance field is refused")
      (delete-directory/files dir))

    (test-case "R7: timing drift cannot hide in nested non-raw JSON"
      (define dir (make-fixture-repo!))
      (define adir (build-path dir "artifacts" "prov" "v9.99.99-w0"))
      (write-text! (build-path adir "evidence" "rollback-drill.json")
                   (string-append "{\n"
                                  " \"timing\": {\n"
                                  "  \"eager-fallback-ms\": [\n"
                                  "   247\n"
                                  "  ]\n"
                                  " },\n"
                                  " \"steps\": [\n"
                                  "  {\n"
                                  "   \"observed\": \"fallback resolved in 260 ms\"\n"
                                  "  }\n"
                                  " ]\n"
                                  "}\n"))
      (write-text! (build-path adir "SHA256SUMS")
                   "00aa  artifacts/prov/v9.99.99-w0/evidence/rollback-drill.json\n")
      (define-values (code output) (run-lint! dir #:current-wave "v9.99.99-w0"))
      (check-equal? code 2)
      (check-true (string-contains? output "prose value 260 ms disagrees")
                  "nested timing drift is refused")
      (delete-directory/files dir))

    (test-case "R9: a tree field is validated as a tree, not as a commit head"
      (define dir (make-fixture-repo!))
      (write-text! (build-path dir "seed.txt") "seed\n")
      (define sha (commit-all! dir))
      (define tree (git-rev-parse! dir "HEAD^{tree}"))
      (define json-p (build-path dir "artifacts" "prov" "v9.99.99-w0" "pair.json"))
      (write-text!
       json-p
       (string-append "{\n" " \"head\": \"" sha "\",\n" " \"tree\": \"" tree "\"\n" "}\n"))
      (write-text! (build-path dir "artifacts" "prov" "v9.99.99-w0" "SHA256SUMS")
                   (string-append (sha256-hex (port->bytes (open-input-file json-p)))
                                  "  artifacts/prov/v9.99.99-w0/pair.json\n"))
      (define-values (code output) (run-lint! dir #:current-wave "v9.99.99-w0"))
      (check-equal? code 0 "a valid head/tree pair is not refused")
      (check-false (string-contains? output "does not resolve to a commit")
                   "the tree value is not walked as a commit identity")
      (delete-directory/files dir))

    (test-case "R9: report timing agreement uses the union across artifacts"
      (define dir (make-fixture-repo!))
      (define (wrel! rel text)
        (write-text! (build-path dir rel) text))
      (wrel!
       "artifacts/prov/v9.99.99-w0/a.json"
       (string-append "{\n" " \"timing\": {\n" "  \"a-ms\": [\n" "   10\n" "  ]\n" " }\n" "}\n"))
      (wrel!
       "artifacts/prov/v9.99.99-w0/b.json"
       (string-append "{\n" " \"timing\": {\n" "  \"b-ms\": [\n" "   20\n" "  ]\n" " }\n" "}\n"))
      (wrel! "docs/reports/r.md" "- metrics 10 ms\n")
      (define entries
        '("artifacts/prov/v9.99.99-w0/a.json" "artifacts/prov/v9.99.99-w0/b.json"
                                              "docs/reports/r.md"))
      (write-text!
       (build-path dir "artifacts" "prov" "v9.99.99-w0" "SHA256SUMS")
       (string-append
        (string-join
         (for/list ([rel (in-list entries)])
           (string-append (sha256-hex (port->bytes (open-input-file (build-path dir rel)))) "  " rel))
         "\n")
        "\n"))
      (define-values (code output) (run-lint! dir #:current-wave "v9.99.99-w0"))
      (check-equal? code 0 "a report citing an earlier artifact's timing is accepted")
      (check-false (string-contains? output "bound report prose value")
                   "the union retains every artifact's structured timings")
      (delete-directory/files dir))

    (test-case "R9: canonical JSON uses Python-compatible escapes"
      (define dir (make-fixture-repo!))
      (define json-p (build-path dir "artifacts" "prov" "v9.99.99-w0" "uni.json"))
      (write-text! json-p (string-append "{\n" " \"note\": \"\\u00e9\\u20ac\"\n" "}\n"))
      (write-text! (build-path dir "artifacts" "prov" "v9.99.99-w0" "SHA256SUMS")
                   (string-append (sha256-hex (port->bytes (open-input-file json-p)))
                                  "  artifacts/prov/v9.99.99-w0/uni.json\n"))
      (define-values (code output) (run-lint! dir #:current-wave "v9.99.99-w0"))
      (check-equal? code 0 "a Python-escaped non-ASCII artifact is canonical")
      (check-false (string-contains? output "not in canonical form")
                   "non-ASCII escapes are byte-compatible with the generator")
      (delete-directory/files dir))

    (test-case "R8: a recorded sha256 must match the bytes of its named file"
      (define dir (make-fixture-repo!))
      (define raw-p (build-path dir "artifacts" "prov" "v9.99.99-w0" "raw" "x.txt"))
      (write-text! raw-p "hello\n")
      (define json-p (build-path dir "artifacts" "prov" "v9.99.99-w0" "matrix.json"))
      (write-text! json-p
                   (string-append "{\n"
                                  " \"artifacts\": [\n"
                                  "  {\n"
                                  "   \"bytes\": 6,\n"
                                  "   \"path\": \"artifacts/prov/v9.99.99-w0/raw/x.txt\",\n"
                                  "   \"sha256\": \""
                                  (make-string 64 #\0)
                                  "\"\n"
                                  "  }\n"
                                  " ]\n"
                                  "}\n"))
      (write-text! (build-path dir "artifacts" "prov" "v9.99.99-w0" "SHA256SUMS")
                   (string-append (sha256-hex (port->bytes (open-input-file json-p)))
                                  "  artifacts/prov/v9.99.99-w0/matrix.json\n"
                                  (sha256-hex (port->bytes (open-input-file raw-p)))
                                  "  artifacts/prov/v9.99.99-w0/raw/x.txt\n"))
      (define-values (code output) (run-lint! dir #:current-wave "v9.99.99-w0"))
      (check-equal? code 2)
      (check-true (string-contains? output "recorded sha256") "names the recorded digest field")
      (check-true (string-contains? output "does not match the bytes of")
                  "refuses the digest/file mismatch")
      (delete-directory/files dir))

    (test-case "R8: bound report prose ms must agree with structured timing"
      (define dir (make-fixture-repo!))
      (define json-p (build-path dir "artifacts" "prov" "v9.99.99-w0" "probe.json"))
      (write-text!
       json-p
       (string-append "{\n" " \"timing\": {\n" "  \"probe-ms\": [\n" "   1\n" "  ]\n" " }\n" "}\n"))
      (define md-p (build-path dir "docs" "reports" "rep.md"))
      (write-text! md-p "- metrics run 999 ms\n\n- red-first fixture run 888 ms\n")
      (write-text! (build-path dir "artifacts" "prov" "v9.99.99-w0" "SHA256SUMS")
                   (string-append (sha256-hex (port->bytes (open-input-file json-p)))
                                  "  artifacts/prov/v9.99.99-w0/probe.json\n"
                                  (sha256-hex (port->bytes (open-input-file md-p)))
                                  "  docs/reports/rep.md\n"))
      (define-values (code output) (run-lint! dir #:current-wave "v9.99.99-w0"))
      (check-equal? code 2)
      (check-true (string-contains? output "bound report prose value 999 ms")
                  "refuses the unmarked report value")
      (check-false (string-contains? output "888") "the red-fixture-labeled block is exempt")
      (delete-directory/files dir))

    (test-case "R5: only *-ms timing keys legitimize prose ms values"
      ;; A non-^-ms numeric field (attempt-count 260) must not make prose
      ;; "260 ms" agree; the typed refusal must still fire.
      (define dir (make-fixture-repo!))
      (define adir (build-path dir "artifacts" "prov" "v9.99.99-w0"))
      (write-text! (build-path adir "matrix.json")
                   (string-append "{\n"
                                  " \"timing\": {\n"
                                  "  \"eager-fallback-ms\": [\n"
                                  "   247\n"
                                  "  ],\n"
                                  "  \"attempt-count\": 260\n"
                                  " },\n"
                                  " \"steps\": [\n"
                                  "  {\n"
                                  "   \"observed\": \"fallback resolved in 260 ms\"\n"
                                  "  }\n"
                                  " ]\n"
                                  "}\n"))
      (write-text! (build-path adir "SHA256SUMS") "")
      (define-values (code output) (run-lint! dir #:current-wave "v9.99.99-w0"))
      (check-equal? code 2)
      (check-true (string-contains? output "prose value 260 ms disagrees")
                  "non-^-ms timing fields do not legitimize prose values")
      (delete-directory/files dir))

    (test-case "R5: malformed current-wave SUMS yields typed drift, not a crash"
      (define dir (make-fixture-repo!))
      (define adir (build-path dir "artifacts" "prov" "v9.99.99-w0"))
      (write-text! (build-path adir "data.json") "{\n \"k\": \"v\"\n}\n")
      ;; Malformed line (no double-space separator) plus a missing target:
      ;; the lint must exit 2 with typed drift, never raise.
      (write-text! (build-path adir "SHA256SUMS")
                   (string-append "garbage-line-without-separator\n"
                                  "00aa  artifacts/prov/v9.99.99-w0/missing.json\n"))
      (define-values (code output) (run-lint! dir #:current-wave "v9.99.99-w0"))
      (check-equal? code 2)
      (check-true (string-contains? output "provenance-drift") "typed refusal")
      (check-true (string-contains? output "malformed") "malformed lines named")
      (delete-directory/files dir))

    (test-case "current wave JSON must be canonical"
      (define dir (make-fixture-repo!))
      (define adir (build-path dir "artifacts" "prov" "v9.99.99-w0"))
      ;; Valid JSON, but keys unsorted: not the canonical form.
      (write-text! (build-path adir "matrix.json") "{\n \"zeta\": 1,\n \"alpha\": 2\n}\n")
      (write-text! (build-path adir "SHA256SUMS") "")
      (define-values (code output) (run-lint! dir #:current-wave "v9.99.99-w0"))
      (check-equal? code 2)
      (check-true (string-contains? output "not in canonical form")
                  "non-canonical JSON refused on the current wave")
      (delete-directory/files dir))

    (test-case "integration: the real tree passes with the current wave strict"
      (define-values (code output) (run-lint! repo-root #:current-wave "v1.00.31-w5"))
      (check-equal? code 0)
      (check-false (string-contains? output "provenance-drift:") "no drift on the real tree")
      (check-true (string-contains? output "artifact-provenance ok")))

    (test-case "determinism: regenerating W5 artifacts reproduces bytes"
      (define matrix-path
        (build-path repo-root
                    "artifacts"
                    "wave-delivery-integrity"
                    "v1.00.31-w5"
                    "provenance-matrix.json"))
      (define sums-path
        (build-path repo-root "artifacts" "wave-delivery-integrity" "v1.00.31-w5" "SHA256SUMS"))
      (define before-matrix (file->string matrix-path))
      (define before-sums (file->string sums-path))
      ;; Same inputs must produce the same bytes. R2: the recorded head is
      ;; a pinned committed observation, so regeneration must reproduce the
      ;; COMMITTED bytes verbatim, not just a self-consistent second run.
      (system*/exit-code (find-executable-path "python3") (path->string gen-path))
      (check-equal? (file->string matrix-path)
                    before-matrix
                    "matrix regeneration reproduces the committed bytes")
      (check-equal? (file->string sums-path)
                    before-sums
                    "SHA256SUMS regeneration reproduces the committed bytes")
      (system*/exit-code (find-executable-path "python3") (path->string gen-path))
      (check-equal? (file->string matrix-path) before-matrix "matrix regeneration is deterministic")
      (check-equal? (file->string sums-path)
                    before-sums
                    "SHA256SUMS regeneration is deterministic"))))
(void (run-tests artifact-provenance-suite))
