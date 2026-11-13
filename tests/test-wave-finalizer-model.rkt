#lang racket
;; @suite all
;; @boundary unit
;; @speed fast
;; @covers scripts/github/wave-finalizer.rkt
;;
;; Model-layer parity tests for the wave-finalizer port.  Canonical JSON, digests,
;; refusal messages and ISO-8601 parsing are pinned differentially against the
;; Python original (`scripts/github/wave-finalizer.py`, imported through
;; `test_wave_finalizer.wf`): python3 is required and a missing interpreter fails
;; these tests instead of skipping them.  Each differential test sends all of its
;; cases in one python process to keep the suite fast.
;;
;; Forged `prepared-finalization` values are built through the module's
;; `for-testing` submodule (`forge-prepared-finalization`), the only place the
;; sealed constructor is reachable.  Racket structures are immutable, so a field
;; "mutation" is modelled by a registered struct carrying a genuine seal over
;; different material, and a lookalike by a struct that was never registered as
;; issued; both are documented where they are exercised.

(require rackunit
         json
         racket/list
         racket/runtime-path
         racket/string
         "../scripts/github/wave-finalizer.rkt")

(define-runtime-path github-dir "../scripts/github")
(define-runtime-path finalizer-path "../scripts/github/wave-finalizer.rkt")
(define python3 (find-executable-path "python3"))

;; ---------------------------------------------------------------- python bridge

(define (to-jsexpr value)
  ;; write-json only accepts symbol-keyed objects; the fixtures use both spellings.
  (cond
    [(hash? value)
     (for/fold ([acc (hash)]) ([(key item) (in-hash value)])
       (hash-set acc
                 (if (string? key)
                     (string->symbol key)
                     key)
                 (to-jsexpr item)))]
    [(list? value) (map to-jsexpr value)]
    [(vector? value) (list->vector (map to-jsexpr (vector->list value)))]
    [else value]))

(define (python-run code payload)
  (unless python3
    (error 'test-wave-finalizer-model "python3 is required for wave-finalizer parity tests"))
  (parameterize ([current-directory github-dir])
    (define-values (proc out in err) (subprocess #f #f #f python3 "-c" code))
    (write-json (to-jsexpr payload) in)
    (close-output-port in)
    (define out-text (port->string out))
    (define err-text (port->string err))
    (subprocess-wait proc)
    (check-equal? (subprocess-status proc) 0 (string-append err-text out-text))
    (read-json (open-input-string out-text))))

(define py-prelude "import json,sys,test_wave_finalizer as t\nwf=t.wf\n")

;; Each snippet reads a JSON array on stdin and prints a JSON array of results.
(define py-canonical-many
  (string-append py-prelude
                 "out=[]\n"
                 "for v in json.load(sys.stdin):\n"
                 " try:\n"
                 "  out.append({'value':wf._canonical_json(v,'lbl',wf.EnvelopeError)})\n"
                 " except wf.FinalizerError as exc:\n"
                 "  out.append({'error':type(exc).__name__,'message':str(exc)})\n"
                 "print(json.dumps(out))\n"))

(define py-config-many
  (string-append
   py-prelude
   "out=[]\n"
   "for v in json.load(sys.stdin):\n"
   " try:\n"
   "  c=wf.load_trusted_configuration(v)\n"
   "  out.append({'canonical':c.canonical_json,'digest':c.digest,'board':c.board.value()})\n"
   " except wf.FinalizerError as exc:\n"
   "  out.append({'error':type(exc).__name__,'message':str(exc)})\n"
   "print(json.dumps(out))\n"))

(define py-envelope-many
  (string-append py-prelude
                 "out=[]\n"
                 "for v in json.load(sys.stdin):\n"
                 " try:\n"
                 "  e=wf.verify_external_envelope(v)\n"
                 "  out.append({'digest':e.digest,'projection_digest':e.projection_digest,"
                 "'canonical':e.canonical_json,'evidence':e.evidence_reference()})\n"
                 " except wf.FinalizerError as exc:\n"
                 "  out.append({'error':type(exc).__name__,'message':str(exc)})\n"
                 "print(json.dumps(out))\n"))

(define py-envelope-digest-many
  (string-append
   py-prelude
   "print(json.dumps([wf.compute_envelope_digest(v) for v in json.load(sys.stdin)]))\n"))

(define py-time-many
  (string-append py-prelude
                 "out=[]\n"
                 "for v in json.load(sys.stdin):\n"
                 " try:\n"
                 "  d=wf._time(v,'created_at')\n"
                 "  out.append({'micros':(d.toordinal()-719163)*86400000000"
                 "+d.hour*3600000000+d.minute*60000000+d.second*1000000+d.microsecond})\n"
                 " except wf.FinalizerError as exc:\n"
                 "  out.append({'error':type(exc).__name__,'message':str(exc)})\n"
                 "print(json.dumps(out))\n"))

(define py-exact-keys-many
  (string-append py-prelude
                 "out=[]\n"
                 "for p in json.load(sys.stdin):\n"
                 " try:\n"
                 "  wf._exact_keys(p['value'],set(p['required']),set(),p['label'],wf.EnvelopeError)\n"
                 "  out.append({'ok':True})\n"
                 " except wf.FinalizerError as exc:\n"
                 "  out.append({'error':type(exc).__name__,'message':str(exc)})\n"
                 "print(json.dumps(out))\n"))

;; ---------------------------------------------------------------- helpers

(define (outcome thunk)
  (with-handlers
      ([exn:fail:finalizer?
        (lambda (exception)
          (hasheq 'error (exn:fail:finalizer-kind exception) 'message (exn-message exception)))])
    (hasheq 'ok (thunk))))

(define (refusal thunk)
  (define result (outcome thunk))
  (unless (hash-has-key? result 'error)
    (error 'test-wave-finalizer-model "expected a finalizer refusal, got ~e" (hash-ref result 'ok)))
  result)

(define (check-refusal thunk kind message)
  (define result (refusal thunk))
  (check-equal? (hash-ref result 'error) kind "refusal kind")
  (check-equal? (hash-ref result 'message) message "refusal message"))

(define (check-parity ours theirs label)
  (check-equal? (hash-ref ours 'message) (hash-ref theirs 'message) label)
  (check-equal? (hash-ref ours 'error) (hash-ref theirs 'error) label))

(define sample-sha (string->immutable-string (make-string 40 #\a)))
(define sample-merge-sha (make-string 40 #\b))
(define sample-attestation (make-string 64 #\c))

(define (sample-config)
  (hash 'schema_version
        1
        'repository
        "owner/repo"
        'base_ref
        "main"
        'merge_method
        "squash"
        'attestation_issuer
        "q-attestor"
        'required_workflows
        (list (hash 'context
                    "ci"
                    'workflow
                    ".github/workflows/ci.yml"
                    'ref
                    "feature"
                    'event
                    "pull_request"
                    'app
                    (hash 'slug "github-actions" 'id 15368)))
        'merge_workflows
        (list (hash 'context
                    "merge-ci"
                    'workflow
                    12345
                    'ref
                    "main"
                    'event
                    "push"
                    'app
                    (hash 'slug "github-actions" 'id 15368)))
        'wave_issue_numbers
        (list 7 8)
        'board
        (hash 'project_id
              "PVT_1"
              'item_id
              "PVTI_2"
              'field_id
              "PVTF_3"
              'value
              (hash "status" "Done" 'count 2 'ratio 0.5))))

(define expected-config-canonical
  (string-append "{\"attestation_issuer\":\"q-attestor\",\"base_ref\":\"main\",\"board\":"
                 "{\"field_id\":\"PVTF_3\",\"item_id\":\"PVTI_2\",\"project_id\":\"PVT_1\","
                 "\"value\":{\"count\":2,\"ratio\":0.5,\"status\":\"Done\"}},"
                 "\"merge_method\":\"squash\",\"merge_workflows\":[{\"app\":"
                 "{\"id\":15368,\"slug\":\"github-actions\"},\"context\":\"merge-ci\","
                 "\"event\":\"push\",\"ref\":\"main\",\"workflow\":12345}],"
                 "\"repository\":\"owner/repo\",\"required_workflows\":[{\"app\":"
                 "{\"id\":15368,\"slug\":\"github-actions\"},\"context\":\"ci\","
                 "\"event\":\"pull_request\",\"ref\":\"feature\","
                 "\"workflow\":\".github/workflows/ci.yml\"}],\"schema_version\":1,"
                 "\"wave_issue_numbers\":[7,8]}"))

(define expected-config-digest "23be576d06348681dae7727952880d091578dc76bb0b0dbed53b67c1a9d3b960")

;; An envelope is self-consistent only when its canonical digest covers exactly
;; the other keys; the digest is computed with the module under test (the digest
;; itself is pinned against Python by the "envelope digest matches python" case)
;; so the envelope fixtures stay readable literals.
(define (sample-envelope #:head-sha [head-sha sample-sha] #:pull-number [pull-number 42])
  (define body
    (hash 'schema_version
          1
          'issuer
          "q-attestor"
          'projection
          (hash 'pull_number pull-number 'head_sha head-sha)
          'evidence
          (hash 'kind "check" 'id 9)))
  (hash-set body 'canonical_digest (hash 'algorithm "sha256" 'value (compute-envelope-digest body))))

;; Same envelope with fields replaced and the digest recomputed over them, so a
;; later refusal is about the changed field and not about self-consistency.
(define (envelope-with replacements)
  (define body
    (for/fold ([acc (hash-remove (sample-envelope) 'canonical_digest)])
              ([(key value) (in-hash replacements)])
      (hash-set acc key value)))
  (hash-set body 'canonical_digest (hash 'algorithm "sha256" 'value (compute-envelope-digest body))))

(define sample-policy
  (parse-protection-policy (hash 'required_status_checks
                                 (hash 'strict
                                       #t
                                       'contexts
                                       (list "legacy")
                                       'checks
                                       (list (hash 'context "ci" 'app_id 15368)
                                             (hash 'context "legacy" 'app_id 7)))
                                 'enforce_admins
                                 (hash 'enabled #t)
                                 'required_linear_history
                                 (hash 'enabled #t)
                                 'allow_force_pushes
                                 (hash 'enabled #f)
                                 'allow_deletions
                                 (hash 'enabled #f))))

(define (prepared-fields config envelope)
  (list (trusted-configuration-canonical-json config)
        (verified-envelope-canonical-json envelope)
        (trusted-configuration-digest config)
        (verified-envelope-digest envelope)
        sample-attestation
        (external-projection-head-sha (verified-envelope-projection envelope))
        sample-policy
        (list 11 12)
        #f))

(define (trusted-sample)
  (load-trusted-configuration (sample-config)))
(define (verified-sample)
  (verify-external-envelope (sample-envelope)))

(define (issue-sample-prepared)
  (apply issue-prepared-finalization (prepared-fields (trusted-sample) (verified-sample))))

(define forge-prepared-finalization
  (dynamic-require `(submod ,finalizer-path for-testing) 'forge-prepared-finalization))

;; ---------------------------------------------------------------- canonical JSON

(module+ test
  (test-case "canonical json sorts keys and uses python separators"
    (check-equal? (finalizer-canonical-json (hash "b" 1 "a" (list 1 'null #t "x")))
                  "{\"a\":[1,null,true,\"x\"],\"b\":1}")
    (check-equal? (finalizer-canonical-json (hash 'b 1 "a" 2)) "{\"a\":2,\"b\":1}"))

  (test-case "canonical json matches python for representative documents"
    (define documents
      (list (hash "b" 1 "a" (list 1 2.5 'null #t "x\u00e9\n"))
            (hash "k" (hash "z" 1 "A" 2 "a" 3))
            (hash "empty" (list) "nested" (hash "x" (hash "y" 'null)))
            (list "tab\there" "quote\"q" "back\\slash" "\u007f" "\u2028")
            (list "x\u00e9\n" "\u0000\u001f" "\u007f")))
    (for ([document (in-list documents)]
          [theirs (in-list (python-run py-canonical-many documents))])
      (check-equal? (finalizer-canonical-json document)
                    (hash-ref theirs 'value)
                    (format "~s" document))))

  (test-case "canonical json keeps non-ASCII raw and escapes controls like python"
    (check-equal? (finalizer-canonical-json "\u2028") "\"\u2028\"")
    (check-equal? (finalizer-canonical-json "\u0000\u001f\u000b") "\"\\u0000\\u001f\\u000b\"")
    (check-equal? (finalizer-canonical-json "\u007f") "\"\u007f\""))

  (test-case "canonical json renders floats exactly like python repr"
    (define values
      (list 1.0
            1e16
            1e15
            1e-5
            1e-4
            0.1
            1e20
            1e300
            1.5e-7
            1234.5678
            5e-324
            -0.0
            3.141592653589793
            123456789012345678.0
            0.5))
    (define documents
      (for/list ([value (in-list values)])
        (hash "v" value)))
    (for ([document (in-list documents)]
          [theirs (in-list (python-run py-canonical-many documents))])
      (check-equal? (finalizer-canonical-json document)
                    (hash-ref theirs 'value)
                    (format "~s" document))))

  (test-case "canonical json refuses non-finite floats with python's message"
    (check-refusal
     (lambda () (finalizer-canonical-json (hash "a" +nan.0) "container" "EnvelopeError"))
     "EnvelopeError"
     "container is not canonical JSON: Out of range float values are not JSON compliant: nan")
    (check-refusal
     (lambda () (finalizer-canonical-json (hash "a" +inf.0) "envelope" "EnvelopeError"))
     "EnvelopeError"
     "envelope is not canonical JSON: Out of range float values are not JSON compliant: inf")
    (check-refusal
     (lambda () (finalizer-canonical-json (hash "a" -inf.0) "envelope" "EnvelopeError"))
     "EnvelopeError"
     "envelope is not canonical JSON: Out of range float values are not JSON compliant: -inf")
    ;; python raises ValueError through the same prefix and the same suffix text
    (define python-message
      (hash-ref (python-run (string-append
                             py-prelude
                             "v=json.load(sys.stdin)\n"
                             "try:\n"
                             " print(json.dumps({'value':wf._canonical_json({'a':float('nan')},"
                             "'envelope',wf.EnvelopeError)}))\n"
                             "except wf.FinalizerError as exc:\n"
                             " print(json.dumps({'error':type(exc).__name__,'message':str(exc)}))\n")
                            (hash 'nan #t))
                'message))
    (check-equal?
     python-message
     "envelope is not canonical JSON: Out of range float values are not JSON compliant: nan"))

  (test-case "canonical json refuses values outside the JSON model"
    (define procedure-message
      (hash-ref
       (refusal (lambda ()
                  (finalizer-canonical-json (hash "a" (lambda () 1)) "label" "EnvelopeError")))
       'message))
    (check-true (regexp-match?
                 #rx"^label is not canonical JSON: value is not JSON-representable: #<procedure"
                 procedure-message))
    (check-refusal (lambda () (finalizer-canonical-json (hash "a" 1/3) "label" "EnvelopeError"))
                   "EnvelopeError"
                   "label is not canonical JSON: value is not JSON-representable: 1/3")))

;; ---------------------------------------------------------------- exact keys

(module+ test
  (test-case "exact key message renders python list reprs"
    (check-refusal (lambda () (load-trusted-configuration (hash-set (sample-config) 'extra 1)))
                   "ConfigurationError"
                   "trusted configuration has unknown ['extra']")
    (check-refusal (lambda ()
                     (load-trusted-configuration (for/fold ([acc (hash)])
                                                           ([(key value) (in-hash (sample-config))]
                                                            #:unless (eq? key 'board))
                                                   (hash-set acc key value))))
                   "ConfigurationError"
                   "trusted configuration has missing ['board']")
    (check-refusal (lambda () (verify-external-envelope (hash-remove (sample-envelope) 'issuer)))
                   "EnvelopeError"
                   "envelope has missing ['issuer']")
    (check-refusal
     (lambda () (verify-external-envelope (hash-set (sample-envelope) (string->symbol "bad'key") 1)))
     "EnvelopeError"
     "envelope has unknown [\"bad'key\"]"))

  (test-case "exact key messages match python for both sides"
    (define required (list "schema_version" "issuer" "projection" "evidence" "canonical_digest"))
    (define payloads
      (list (hash "schema_version" 1 "zz" 1 "bb" 1)
            (hash "issuer" "q-attestor")
            (hash "schema_version" 1 "projection" 1)))
    (define theirs
      (python-run py-exact-keys-many
                  (for/list ([value (in-list payloads)])
                    (hash 'value value 'required required 'label "envelope"))))
    (for ([value (in-list payloads)]
          [expected (in-list theirs)])
      (check-parity (refusal (lambda () (verify-external-envelope value)))
                    expected
                    (format "~s" value))))

  (test-case "nested workflow labels appear in key messages"
    (define broken
      (hash-set (sample-config)
                'required_workflows
                (list (hash 'context "ci" 'workflow 1 'ref "feature" 'event "pull_request"))))
    (check-refusal (lambda () (load-trusted-configuration broken))
                   "ConfigurationError"
                   "required_workflows[0] has missing ['app']")))

;; ---------------------------------------------------------------- field readers

(module+ test
  (test-case "sha accepts upper and lower hex and refuses anything shorter"
    (define lower (string->immutable-string (make-string 40 #\a)))
    (define upper (string->immutable-string (string-append (make-string 39 #\A) "0")))
    (check-true (verified-envelope? (verify-external-envelope (sample-envelope #:head-sha lower))))
    (check-equal? (external-projection-head-sha
                   (verified-envelope-projection
                    (verify-external-envelope
                     (envelope-with (hash 'projection (hash 'pull_number 42 'head_sha upper))))))
                  upper)
    (check-refusal (lambda ()
                     (verify-external-envelope (sample-envelope #:head-sha (substring lower 0 39))))
                   "EnvelopeError"
                   "projection.head_sha must be a full 40-character hexadecimal SHA")
    (check-refusal
     (lambda ()
       (verify-external-envelope
        (envelope-with
         (hash 'projection
               (hash 'pull_number 42 'head_sha (string-append (substring lower 0 39) "z"))))))
     "EnvelopeError"
     "projection.head_sha must be a full 40-character hexadecimal SHA"))

  (test-case "positive ints refuse booleans, zeros and non-integers"
    (for ([value (in-list (list #t 0 1.5))])
      (check-refusal (lambda () (verify-external-envelope (sample-envelope #:pull-number value)))
                     "EnvelopeError"
                     "projection.pull_number must be a positive integer")))

  (test-case "strings must be nonempty and trimmed"
    (for ([value (in-list (list "" " q" "q "))])
      (check-refusal (lambda () (verify-external-envelope (envelope-with (hash 'issuer value))))
                     "EnvelopeError"
                     "issuer must be a nonempty, trimmed string"))
    (check-refusal (lambda () (load-trusted-configuration '("not" "an" "object")))
                   "ConfigurationError"
                   "trusted configuration must be an object"))

  (test-case "api readers refuse uncertainty instead of guessing"
    (check-refusal (lambda () (api-mapping '((a . 1)) "pull request"))
                   "ApiUncertainty"
                   "pull request response is not an object")
    (check-refusal (lambda () (api-items (hash 'items 5 'complete #t) "workflow runs"))
                   "ApiUncertainty"
                   "workflow runs is not a complete sequence")
    (check-refusal (lambda () (api-items (hash 'items (list) 'complete #f) "workflow runs"))
                   "ApiUncertainty"
                   "workflow runs collection is not explicitly complete")
    (check-refusal (lambda () (api-items (list 1) "check runs"))
                   "ApiUncertainty"
                   "check runs[0] is malformed")
    (check-equal? (api-items (list (hash 'id 1)) "check runs") (list (hash "id" 1)))
    (check-refusal (lambda ()
                     (api-nested (hash 'head (hash 'sha sample-sha)) '(head ref repo) "pull request"))
                   "ApiUncertainty"
                   "pull request is missing head.ref.repo")
    (check-equal? (api-nested (hash 'head (hash 'ref "feature")) '(head ref) "pull request")
                  "feature")
    (check-refusal (lambda () (api-bool 1 "pull request.merged"))
                   "ApiUncertainty"
                   "pull request.merged is not boolean")
    (check-refusal (lambda () (api-positive-int #t "workflow run id"))
                   "ApiUncertainty"
                   "workflow run id is not a positive integer")))

;; ---------------------------------------------------------------- trusted configuration

(module+ test
  (test-case "trusted configuration canonical json and digest match python"
    (define config (trusted-sample))
    (define python-side (car (python-run py-config-many (list (sample-config)))))
    (check-equal? (trusted-configuration-canonical-json config) expected-config-canonical)
    (check-equal? (trusted-configuration-digest config) expected-config-digest)
    (check-equal? (trusted-configuration-canonical-json config) (hash-ref python-side 'canonical))
    (check-equal? (trusted-configuration-digest config) (hash-ref python-side 'digest)))

  (test-case "symbol and string keys normalise to the same configuration"
    (define string-keyed
      (hash "schema_version"
            1
            "repository"
            "owner/repo"
            "base_ref"
            "main"
            "merge_method"
            "squash"
            "attestation_issuer"
            "q-attestor"
            "required_workflows"
            (list (hash "context"
                        "ci"
                        "workflow"
                        ".github/workflows/ci.yml"
                        "ref"
                        "feature"
                        "event"
                        "pull_request"
                        "app"
                        (hash "slug" "github-actions" "id" 15368)))
            "merge_workflows"
            (list (hash "context"
                        "merge-ci"
                        "workflow"
                        12345
                        "ref"
                        "main"
                        "event"
                        "push"
                        "app"
                        (hash "slug" "github-actions" "id" 15368)))
            "wave_issue_numbers"
            (list 7 8)
            "board"
            (hash "project_id"
                  "PVT_1"
                  "item_id"
                  "PVTI_2"
                  "field_id"
                  "PVTF_3"
                  "value"
                  (hash "status" "Done" "count" 2 "ratio" 0.5))))
    (check-equal? (trusted-configuration-digest (load-trusted-configuration string-keyed))
                  expected-config-digest))

  (test-case "parsed configuration accessors expose the trusted snapshot"
    (define config (trusted-sample))
    (check-equal? (trusted-configuration-repository config) "owner/repo")
    (check-equal? (trusted-configuration-base-ref config) "main")
    (check-equal? (trusted-configuration-merge-method config) "squash")
    (check-equal? (trusted-configuration-attestation-issuer config) "q-attestor")
    (check-equal? (trusted-configuration-wave-issue-numbers config) '(7 8))
    (check-equal? (map workflow-contract-context (trusted-configuration-required-workflows config))
                  '("ci"))
    (check-equal? (map workflow-contract-app-id (trusted-configuration-merge-workflows config))
                  '(15368))
    (check-equal? (workflow-contract-workflow (car (trusted-configuration-merge-workflows config)))
                  12345)
    (check-equal? (workflow-contract-ref (car (trusted-configuration-merge-workflows config))) "main")
    (check-equal? (workflow-contract-app-slug (car (trusted-configuration-required-workflows config)))
                  "github-actions"))

  (test-case "board value is canonicalised and reparses"
    (define board (trusted-configuration-board (trusted-sample)))
    (check-equal? (board-sync-value-json board) "{\"count\":2,\"ratio\":0.5,\"status\":\"Done\"}")
    (check-equal? (board-sync-value board) (hash "count" 2 "ratio" 0.5 "status" "Done"))
    (check-equal? (board-sync-project-id board) "PVT_1")
    (check-equal? (hash-ref (car (python-run py-config-many (list (sample-config)))) 'board)
                  (read-json (open-input-string (board-sync-value-json board)))))

  (test-case "trusted configuration refusals match python"
    (define (variant path value)
      (define (update node path)
        (cond
          [(null? path) value]
          [(hash? node) (hash-set node (car path) (update (hash-ref node (car path)) (cdr path)))]
          [(list? node)
           (for/list ([item (in-list node)]
                      [index (in-naturals)])
             (if (= index (car path))
                 (update item (cdr path))
                 item))]
          [else (error 'variant "cannot descend into ~e" node)]))
      (update (sample-config) path))
    (define (workflow sub-path value)
      (variant (cons 'required_workflows (cons 0 sub-path)) value))
    (define variants
      (list (variant '() (hash-remove (sample-config) 'merge_workflows))
            (variant '() 42)
            (variant '(schema_version) #t)
            (variant '(schema_version) 2)
            (variant '(repository) "owner")
            (variant '(repository) "owner/")
            (variant '(repository) "/repo")
            (variant '(repository) "a/b/c")
            (variant '(merge_method) "merge")
            (variant '(base_ref) "")
            (variant '(board) (hash 'project_id "PVT_1" 'item_id "PVTI_2" 'field_id "PVTF_3"))
            (variant '(board) (hash-set (hash-ref (sample-config) 'board) 'extra 1))
            (variant '(wave_issue_numbers) (list))
            (variant '(wave_issue_numbers) (list 7 7))
            (variant '(wave_issue_numbers) (list 7 #t))
            (variant '(wave_issue_numbers) (list 7 0))
            (variant '(required_workflows) (list))
            (variant '(required_workflows) 'null)
            (variant '(required_workflows) (list (hash-ref (sample-config) 'required_workflows)))
            (workflow '(workflow) #t)
            (workflow '(workflow) 0)
            (workflow '(workflow) " padded")
            (workflow '(context) "")
            (workflow '(event) "push")
            (workflow '(app) (hash 'slug "github-actions"))
            (workflow '(app) (hash 'slug "github-actions" 'id #t))
            (variant '(merge_workflows 0 ref) "develop")
            (variant '(merge_workflows 0 event) "pull_request")))
    (define theirs (python-run py-config-many variants))
    (for ([broken (in-list variants)]
          [expected (in-list theirs)])
      (check-parity (refusal (lambda () (load-trusted-configuration broken)))
                    expected
                    (format "~e" broken)))))

;; ---------------------------------------------------------------- envelope

(module+ test
  (test-case "envelope digest matches python"
    (define bodies
      (list (hash-remove (sample-envelope) 'canonical_digest)
            (hash-remove (sample-envelope #:pull-number 7) 'canonical_digest)
            (hash 'schema_version 1 'issuer "other")))
    (define theirs (python-run py-envelope-digest-many bodies))
    (for ([body (in-list bodies)]
          [expected (in-list theirs)])
      (check-equal? (compute-envelope-digest body) expected))
    (check-equal? (compute-envelope-digest (sample-envelope))
                  "7957cb9d5657ef35ebbfa59f1246912bac0b6c6ebc74b3c26765519d4b3c40ba"))

  (test-case "verified envelope matches python"
    (define envelope (verified-sample))
    (define python-side (car (python-run py-envelope-many (list (sample-envelope)))))
    (check-equal? (verified-envelope-digest envelope) (hash-ref python-side 'digest))
    (check-equal? (verified-envelope-projection-digest envelope)
                  (hash-ref python-side 'projection_digest))
    (check-equal? (verified-envelope-canonical-json envelope) (hash-ref python-side 'canonical))
    (check-equal? (read-json (open-input-string (finalizer-canonical-json
                                                 (verified-envelope-evidence-reference envelope))))
                  (hash-ref python-side 'evidence))
    (check-equal? (verified-envelope-projection-digest envelope)
                  "4e1f22db668a58a1a6a0da284d8d1145787161109cb7c4c811b683d2182a7c08")
    (check-true (verified-envelope-canonical-self-consistent envelope))
    (check-equal? (verified-envelope-issuer envelope) "q-attestor")
    (check-equal? (verified-envelope-evidence-kind envelope) "check")
    (check-equal? (verified-envelope-evidence-id envelope) 9)
    (check-equal? (external-projection-pull-number (verified-envelope-projection envelope)) 42))

  (test-case "envelope self-consistency mismatch is refused"
    ;; the digest is kept from the untampered envelope while the projection changed
    (check-refusal
     (lambda ()
       (verify-external-envelope
        (hash-set (sample-envelope) 'projection (hash 'pull_number 43 'head_sha sample-sha))))
     "EnvelopeError"
     "envelope canonical self-consistency mismatch"))

  (test-case "envelope digest and evidence fields are pinned"
    (define envelope (sample-envelope))
    (check-refusal
     (lambda ()
       (verify-external-envelope (hash-set (sample-envelope)
                                           'canonical_digest
                                           (hash 'algorithm "sha512" 'value (make-string 64 #\d)))))
     "EnvelopeError"
     "canonical_digest.algorithm must be sha256")
    (check-refusal (lambda ()
                     (verify-external-envelope
                      (hash-set (sample-envelope)
                                'canonical_digest
                                (hash 'algorithm
                                      "sha256"
                                      'value
                                      (string-upcase (compute-envelope-digest (sample-envelope)))))))
                   "EnvelopeError"
                   "canonical_digest.value must be lowercase SHA-256")
    (check-refusal (lambda ()
                     (verify-external-envelope (envelope-with (hash 'evidence
                                                                    (hash 'kind "comment" 'id 9)))))
                   "EnvelopeError"
                   "evidence.kind must be artifact or check")
    (check-refusal (lambda () (verify-external-envelope (envelope-with (hash 'schema_version 2))))
                   "EnvelopeError"
                   "unsupported envelope schema_version"))

  (test-case "envelope refusals match python"
    (define envelope (sample-envelope))
    (define variants
      (list (hash-remove envelope 'issuer)
            (hash-set envelope 'unknown_key 1)
            'null
            (hash-set envelope 'schema_version #t)
            (hash-set envelope 'projection (hash 'pull_number 42))
            (hash-set envelope 'projection (hash 'pull_number #t 'head_sha sample-sha))
            (hash-set envelope 'projection (hash 'pull_number 42 'head_sha "short"))
            (hash-set envelope 'evidence (hash 'kind "artifact"))
            (hash-set envelope 'evidence (hash 'kind "artifact" 'id 0))
            (hash-set envelope 'canonical_digest (hash 'algorithm "sha256"))
            (hash-set envelope 'canonical_digest (hash 'algorithm "sha256" 'value "XYZ"))))
    (define theirs (python-run py-envelope-many variants))
    (for ([broken (in-list variants)]
          [expected (in-list theirs)])
      (check-parity (refusal (lambda () (verify-external-envelope broken)))
                    expected
                    (format "~e" broken)))))

;; ---------------------------------------------------------------- timestamps

(module+ test
  (test-case "timestamp parsing matches python fromisoformat"
    (define values
      (list "2024-01-01T00:00:00Z"
            "2024-01-01T00:00:00.1234567Z"
            "2024-01-01T00:00:00+02:30"
            "2024-01-01T00:00:00.5-05:00"
            "2024-01-01 00:00:00Z"
            "2024-01-01T00:00:00,5Z"
            "2024-01-01T00:00:00+0230"
            "2024-01-01t00:00:00Z"
            "1969-07-20T20:17:40Z"
            "0001-01-01T00:00:00Z"
            "2024-02-29T12:00:00Z"
            "2024-12-31T23:59:59Z"))
    (define theirs (python-run py-time-many values))
    (for ([value (in-list values)]
          [expected (in-list theirs)])
      (check-equal? (parse-timestamp value "created_at") (hash-ref expected 'micros) value)))

  (test-case "timestamp refusals match python"
    (define values
      (list "2024-01-01T00:00"
            "2024-01-01"
            "2024-01-01T00:00:00"
            "not-a-time"
            "2024-02-30T00:00:00Z"
            "2023-02-29T12:00:00Z"
            "2024-01-01T24:00:00Z"
            "2024-01-01T00:00:60Z"
            "2024-01-01T00:00:00+24:00"
            "2024-01-01T00:00:00z"))
    (define theirs (python-run py-time-many values))
    (for ([value (in-list values)]
          [expected (in-list theirs)])
      (check-parity (refusal (lambda () (parse-timestamp value "created_at"))) expected value))
    (check-refusal (lambda () (parse-timestamp 1767225600 "created_at"))
                   "ApiUncertainty"
                   "created_at is not a timestamp")
    (check-refusal (lambda () (parse-timestamp "nope" "created_at"))
                   "ApiUncertainty"
                   "created_at is malformed")
    (check-refusal (lambda () (parse-timestamp "2024-01-01T00:00:00" "created_at"))
                   "ApiUncertainty"
                   "created_at has no timezone"))

  (test-case "timestamp parsing refuses forms only python's flexible parser accepts"
    ;; Documented narrowing: CPython's fromisoformat also accepts basic-format
    ;; timestamps, arbitrary date/time separators and offsets with seconds.  The
    ;; port refuses them as malformed (fail-closed) instead of parsing them.
    (define values
      (list "20240101T000000Z"
            "2024-01-01X00:00:00Z"
            "2024-01-01T00:00:00+02:30:15"
            "2024-01-01T00:00:00+02"
            "2024-01-01T000000Z"))
    (define theirs (python-run py-time-many values))
    (for ([value (in-list values)]
          [expected (in-list theirs)])
      (check-refusal (lambda () (parse-timestamp value "created_at"))
                     "ApiUncertainty"
                     "created_at is malformed")
      (check-true (hash-has-key? expected 'micros) value))))

;; ---------------------------------------------------------------- typed equality

(module+ test
  (test-case "typed equal refuses type-crossing equality"
    (check-false (typed-equal? 1 #t))
    (check-false (typed-equal? #t 1))
    (check-false (typed-equal? 1 1.0))
    (check-true (typed-equal? 1 1))
    (check-true (typed-equal? 'null 'null))
    (check-false (typed-equal? 'null #f))
    (check-true (typed-equal? (hash "a" 1 "b" (list 2 3)) (hash "b" (list 2 3) "a" 1)))
    (check-false (typed-equal? (hash "a" 1) (hash "a" 1 "b" 2)))
    (check-false (typed-equal? (hash "a" #t) (hash "a" 1)))
    (check-false (typed-equal? (list 1 2) (list 1 2 3)))
    (check-false (typed-equal? (hash "a" 1) (list 1)))
    (check-false (typed-equal? (vector 1 2) (list 1 2)))
    (check-true (typed-equal? (vector 1 2) (vector 1 2)))))

;; ---------------------------------------------------------------- policy accessors

(module+ test
  (test-case "policy accessors expose pinned contexts and app ids"
    (check-equal? (protection-policy-required-contexts sample-policy) '("ci" "legacy"))
    (check-equal? (protection-policy-app-id-for sample-policy "ci") 15368)
    (check-equal? (protection-policy-app-id-for sample-policy "legacy") 7)
    (check-exn exn:fail? (lambda () (protection-policy-app-id-for sample-policy "other"))))

  (test-case "policy refusals keep this port's messages"
    (define (protection strict contexts checks)
      (hash 'required_status_checks
            (hash 'strict strict 'contexts contexts 'checks checks)
            'enforce_admins
            (hash 'enabled #t)
            'required_linear_history
            (hash 'enabled #t)
            'allow_force_pushes
            (hash 'enabled #f)
            'allow_deletions
            (hash 'enabled #f)))
    (check-refusal
     (lambda ()
       (parse-protection-policy (protection #f (list) (list (hash 'context "ci" 'app_id 1)))))
     "PolicyError"
     "required status checks must be strict")
    (check-refusal (lambda () (parse-protection-policy (protection #t (list "legacy") (list))))
                   "PolicyError"
                   "every required context must have a non-null App ID")
    (check-refusal (lambda () (parse-protection-policy (protection #t (list) (list))))
                   "PolicyError"
                   "required status check policy must be nonempty")
    (check-refusal
     (lambda ()
       (parse-protection-policy (protection #t (list) (list (hash 'context "ci" 'app_id 'null)))))
     "PolicyError"
     "legacy unpinned required status checks are forbidden")
    (check-refusal (lambda () (parse-protection-policy 42))
                   "ApiUncertainty"
                   "branch protection is not an object")))

;; ---------------------------------------------------------------- prepared finalization

(module+ test
  (test-case "issued prepared finalization revalidates its snapshots"
    (define prepared (issue-sample-prepared))
    (define-values (config envelope) (validate-prepared-finalization prepared))
    (check-equal? (trusted-configuration-digest config)
                  (prepared-finalization-config-digest prepared))
    (check-equal? (verified-envelope-digest envelope)
                  (prepared-finalization-envelope-digest prepared))
    (check-equal? (external-projection-head-sha (verified-envelope-projection envelope))
                  (prepared-finalization-head-sha prepared))
    (check-equal? (prepared-finalization-attestation-digest prepared) sample-attestation)
    (check-equal? (prepared-finalization-premerge-run-ids prepared) '(11 12))
    (check-equal? (prepared-finalization-envelope-json prepared)
                  (verified-envelope-canonical-json (verified-sample)))
    (check-false (prepared-finalization-already-merged prepared))
    (check-true (and (string? (prepared-finalization-seal prepared))
                     (= 64 (string-length (prepared-finalization-seal prepared))))))

  (test-case "finalize requires an authentic prepared finalization"
    (check-refusal (lambda () (validate-prepared-finalization 'not-a-prepared-finalization))
                   "ValidationError"
                   "finalize requires an authentic PreparedFinalization")
    (check-refusal (lambda () (validate-prepared-finalization #f))
                   "ValidationError"
                   "finalize requires an authentic PreparedFinalization")
    ;; the constructor is not exported, so a lookalike cannot be built at all
    (check-false (dynamic-require finalizer-path 'prepared-finalization (lambda () #f))))

  (test-case "prepared finalization refuses an already merged snapshot"
    (define fields (prepared-fields (trusted-sample) (verified-sample)))
    (define forged
      (apply forge-prepared-finalization (append (take fields 8) (list #t)) #:register? #t))
    (check-refusal (lambda () (validate-prepared-finalization forged))
                   "ValidationError"
                   "PreparedFinalization must originate from an unmerged pull request"))

  (test-case "prepared finalization refuses a lookalike that was never issued"
    ;; Documented forging model: the struct is built by the module's for-testing
    ;; hook with the material as given (so its seal is internally consistent) but
    ;; is not registered as issued by this process.
    (define forged
      (apply forge-prepared-finalization (prepared-fields (trusted-sample) (verified-sample))))
    (check-refusal (lambda () (validate-prepared-finalization forged))
                   "ValidationError"
                   "PreparedFinalization was not issued by prepare_finalization"))

  (test-case "prepared finalization refuses a mutated field carrying a genuine seal"
    ;; Documented mutation model: Racket structures are immutable, so a "mutated"
    ;; snapshot is a registered struct whose fields changed while the seal still
    ;; covers the original material -- exactly the tamper the seal must detect.
    (define prepared (issue-sample-prepared))
    (define fields (prepared-fields (trusted-sample) (verified-sample)))
    (define mutated
      (apply forge-prepared-finalization
             (list (list-ref fields 0)
                   (list-ref fields 1)
                   (list-ref fields 2)
                   (list-ref fields 3)
                   (list-ref fields 4)
                   (string-append (make-string 39 #\f) "f")
                   (list-ref fields 6)
                   (list-ref fields 7)
                   (list-ref fields 8))
             #:register? #t
             #:seal (prepared-finalization-seal prepared)))
    (check-refusal (lambda () (validate-prepared-finalization mutated))
                   "ValidationError"
                   "PreparedFinalization seal is invalid"))

  (test-case "prepared finalization refuses a snapshot digest mismatch"
    (define fields (prepared-fields (trusted-sample) (verified-sample)))
    (define other-config
      (load-trusted-configuration (hash-set (sample-config) 'repository "owner/other")))
    (define forged
      (apply forge-prepared-finalization
             (list (trusted-configuration-canonical-json other-config)
                   (list-ref fields 1)
                   (list-ref fields 2)
                   (list-ref fields 3)
                   (list-ref fields 4)
                   (list-ref fields 5)
                   (list-ref fields 6)
                   (list-ref fields 7)
                   (list-ref fields 8))
             #:register? #t))
    (check-refusal (lambda () (validate-prepared-finalization forged))
                   "ValidationError"
                   "PreparedFinalization snapshot digest mismatch"))

  (test-case "prepared finalization refuses a head binding mismatch"
    (define prepared (issue-sample-prepared))
    (define fields (prepared-fields (trusted-sample) (verified-sample)))
    (define other-envelope
      (verify-external-envelope (sample-envelope #:head-sha (string-append (make-string 39 #\d)
                                                                           "d"))))
    (define forged
      (apply forge-prepared-finalization
             (list (list-ref fields 0)
                   (verified-envelope-canonical-json other-envelope)
                   (list-ref fields 2)
                   (verified-envelope-digest other-envelope)
                   (list-ref fields 4)
                   (list-ref fields 5)
                   (list-ref fields 6)
                   (list-ref fields 7)
                   (list-ref fields 8))
             #:register? #t))
    (check-refusal (lambda () (validate-prepared-finalization forged))
                   "ValidationError"
                   "PreparedFinalization head-binding mismatch")
    (check-equal? (prepared-finalization-head-sha prepared) sample-sha))

  (test-case "prepared finalization refuses malformed snapshots"
    (define fields (prepared-fields (trusted-sample) (verified-sample)))
    (define forged
      (apply forge-prepared-finalization
             (list "{not json"
                   (list-ref fields 1)
                   (list-ref fields 2)
                   (list-ref fields 3)
                   (list-ref fields 4)
                   (list-ref fields 5)
                   (list-ref fields 6)
                   (list-ref fields 7)
                   (list-ref fields 8))
             #:register? #t))
    (check-refusal (lambda () (validate-prepared-finalization forged))
                   "ValidationError"
                   "PreparedFinalization snapshots are malformed"))

  (test-case "finalization result pins the verification flags"
    (define result (finalization-result "owner/repo" 42 sample-sha sample-merge-sha #f #t #t))
    (check-true (finalization-result-canonical-self-consistency-verified result))
    (check-true (finalization-result-github-attestation-verified result))
    (check-equal? (finalization-result-merge-sha result) sample-merge-sha)
    (check-equal? (finalization-result-repository result) "owner/repo")
    (check-false (finalization-result-already-merged result))))
