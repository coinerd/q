#lang racket/base
;; tests/helpers/gsd-delivery-goldens.rkt — recorded oracle answers for the
;; delivery parity suites (Phase B: the Python controller is gone).
;;
;; scripts/gsd-delivery.py was the differential oracle for five Racket suites.
;; Before it is deleted, every parity case's Python outcome (success payload or
;; refusal message) is captured verbatim into
;; tests/fixtures/gsd-delivery-goldens.json, with the case text it was captured
;; for and the provenance of the capture (source path, the git blob revision of
;; that source, and the capture method). The suites then read the recorded
;; answer instead of importing the module.
;;
;; Two invariants make the recording honest:
;;
;;   1. IDENTITY — a row may only be read for the EXACT case that produced it.
;;      `golden-outcome` recomputes the case text from the live input and
;;      requires textual equality with the recorded row; a stale or copied row
;;      fails loudly instead of answering a different case. The group's label
;;      set is also pinned by every suite (`golden-labels`), so a row (or a
;;      parity case) that disappears or is renamed fails loudly too.
;;
;;   2. PROVENANCE — the group names the source module it was captured from,
;;      its 40-hex git blob revision, and how it was captured. A row is only
;;      meaningful against that revision.
;;
;; Dynamic fixture values (git object names, temp repository paths) can never be
;; baked into a checked-in answer, so each group declares tokens: full values
;; become `{{name}}` and their 12-hex prefixes `{{name-12}}` at capture time,
;; and are substituted back from the live fixtures at read time. A token left
;; unresolved is a hard error, never a silent literal.

(require json
         racket/file
         racket/list
         racket/promise
         racket/runtime-path
         racket/string)

(provide gsd-delivery-goldens
         golden-outcome
         golden-labels
         golden-case-count
         canonical-case-text
         tokenize-value
         detokenize-value)

(define-runtime-path goldens-file "../fixtures/gsd-delivery-goldens.json")

;; ============================================================
;; Loud failures
;; ============================================================

(define (goldens-fail where fmt . args)
  (error 'gsd-delivery-goldens "~a" (string-append where ": " (apply format fmt args))))

(define (require-non-empty-string where value what)
  (unless (and (string? value) (not (string=? value "")))
    (goldens-fail where "~a must be a non-empty string, got ~s" what value))
  value)

(define (require-string-list where value what)
  (unless (and (list? value) (andmap string? value))
    (goldens-fail where "~a must be a list of strings, got ~s" what value))
  value)

;; ============================================================
;; Document validation
;; ============================================================

(define (validate-entry! group label entry)
  (define where (format "group ~a case ~a" group label))
  (unless (hash? entry)
    (goldens-fail where "the recorded row must be an object, got ~s" entry))
  (define recorded-label (require-non-empty-string where (hash-ref entry 'label #f) "label"))
  (unless (string=? recorded-label label)
    (goldens-fail where "the row is labelled ~a" recorded-label))
  (require-non-empty-string where (hash-ref entry 'case #f) "case text")
  (define outcome (hash-ref entry 'outcome #f))
  (unless (hash? outcome)
    (goldens-fail where "outcome must be an object with 'ok, got ~s" outcome))
  (unless (boolean? (hash-ref outcome 'ok #f))
    (goldens-fail where "outcome 'ok must be a JSON boolean, got ~s"
                  (hash-ref outcome 'ok #f)))
  (cond
    [(hash-ref outcome 'ok)
     (unless (hash-has-key? outcome 'value)
       (goldens-fail where "a successful outcome must carry 'value"))]
    [else
     (require-non-empty-string where (hash-ref outcome 'reason #f) "refusal reason")
     (unless (eq? (hash-ref outcome 'reason) (string-trim (hash-ref outcome 'reason)))
       (goldens-fail where "the recorded refusal reason carries surrounding whitespace"))]))

(define (validate-group! index group)
  (define where (format "group #~a" index))
  (unless (hash? group)
    (goldens-fail where "each group must be an object, got ~s" group))
  (define name (require-non-empty-string where (hash-ref group 'name #f) "name"))
  (require-non-empty-string (format "group ~a" name) (hash-ref group 'oracle #f) "oracle")
  (require-string-list (format "group ~a" name) (hash-ref group 'tokens #f) "tokens")
  (unless (and (list? (hash-ref group 'cases #f)) (pair? (hash-ref group 'cases)))
    (goldens-fail "group ~a" name "cases must be a non-empty list"))
  (define labels
    (for/list ([entry (in-list (hash-ref group 'cases))])
      (require-non-empty-string (format "group ~a" name) (and (hash? entry) (hash-ref entry 'label #f)) "label")))
  (define duplicates
    (for/list ([label (in-list labels)]
               #:when (> (count (lambda (other) (string=? other label)) labels) 1))
      label))
  (unless (null? duplicates)
    (goldens-fail "group ~a" name "duplicate labels: ~a" (string-join (remove-duplicates duplicates) ", ")))
  (for ([label (in-list labels)] [entry (in-list (hash-ref group 'cases))])
    (validate-entry! name label entry))
  name)

(define (validate-document! document)
  (unless (hash? document)
    (goldens-fail "fixtures" "the golden document must be an object, got ~s" document))
  (unless (equal? (hash-ref document 'schema #f) "gsd-delivery-goldens/1")
    (goldens-fail "fixtures" "schema must be gsd-delivery-goldens/1, got ~s"
                  (hash-ref document 'schema #f)))
  (unless (equal? (hash-ref document 'source #f) "scripts/gsd-delivery.py")
    (goldens-fail "fixtures" "source must name the oracle this file records, got ~s"
                  (hash-ref document 'source #f)))
  (define revision (require-non-empty-string "fixtures" (hash-ref document 'source_revision #f)
                                             "source_revision"))
  (unless (regexp-match? #px"^[0-9a-f]{40}$" revision)
    (goldens-fail "fixtures" "source_revision must be a 40-hex git blob revision, got ~s" revision))
  (require-non-empty-string "fixtures" (hash-ref document 'captured_with #f) "captured_with")
  (define groups (hash-ref document 'groups #f))
  (unless (and (list? groups) (pair? groups))
    (goldens-fail "fixtures" "groups must be a non-empty list, got ~s" groups))
  (define names (for/list ([group (in-list groups)] [index (in-naturals)]) (validate-group! index group)))
  (define duplicate (findf (lambda (name) (> (count (lambda (other) (string=? other name)) names) 1)) names))
  (when duplicate
    (goldens-fail "fixtures" "duplicate group name: ~a" duplicate))
  document)

(define cached-document
  (delay
    (unless (file-exists? goldens-file)
      (goldens-fail "fixtures"
                    "the recorded oracle answers are missing at ~a; the parity suites cannot run without them"
                    (path->string goldens-file)))
    (define document
      (with-handlers ([exn:fail?
                       (lambda (error)
                         (goldens-fail "fixtures" "~a is not readable JSON: ~a"
                                       (path->string goldens-file) (exn-message error)))])
        (call-with-input-file goldens-file read-json)))
    (validate-document! document)))

;; ============================================================
;; Group access
;; ============================================================

;; A validated group as a hash:
;;   'name 'oracle 'tokens 'cases (capture order) 'by-label 'labels (sorted).
(define (gsd-delivery-goldens name)
  (define document (force cached-document))
  (define group
    (or (findf (lambda (candidate) (equal? (hash-ref candidate 'name) name))
               (hash-ref document 'groups))
        (goldens-fail "fixtures"
                      "no golden group named ~a (present: ~a)"
                      name
                      (string-join (for/list ([g (in-list (hash-ref document 'groups))])
                                     (hash-ref g 'name))
                                   ", "))))
  (define cases (hash-ref group 'cases))
  (hasheq 'name
          name
          'oracle
          (hash-ref group 'oracle)
          'tokens
          (hash-ref group 'tokens)
          'cases
          cases
          'by-label
          (for/hash ([entry (in-list cases)])
            (values (hash-ref entry 'label) entry))
          'labels
          (sort (for/list ([entry (in-list cases)]) (hash-ref entry 'label)) string<?)))

;; The pinned label set: a parity table must carry EXACTLY these labels.
(define (golden-labels name)
  (hash-ref (gsd-delivery-goldens name) 'labels))

(define (golden-case-count name)
  (length (golden-labels name)))

;; ============================================================
;; Case text: a stable, tokenized rendering of the oracle input
;; ============================================================

(define (string-key key)
  (cond
    [(symbol? key) (symbol->string key)]
    [(string? key) key]
    [else (format "~a" key)]))

(define (sorted-hash-pairs value)
  (sort (for/list ([(key item) (in-hash value)])
          (cons (string-key key) item))
        string<?
        #:key car))

(define (render-atom value)
  (cond
    [(string? value) (let ([out (open-output-string)]) (write value out) (get-output-string out))]
    [(eq? value 'null) "null"]
    [(eq? value #t) "true"]
    [(eq? value #f) "false"]
    [(void? value) "void"]
    [else (let ([out (open-output-string)]) (write value out) (get-output-string out))]))

;; A stable, structurally unambiguous rendering of a case's oracle input: keys
;; are sorted, strings are escaped, so two different inputs can never render to
;; the same case text.
(define (canonical-case-text value)
  (cond
    [(hash? value)
     (string-append "{"
                    (string-join (for/list ([pair (in-list (sorted-hash-pairs value))])
                                   (format "~a ~a"
                                           (render-atom (string->symbol (car pair)))
                                           (canonical-case-text (cdr pair))))
                                 "\n ")
                    "}")]
    [(list? value) (string-append "[" (string-join (map canonical-case-text value) " ") "]")]
    [(pair? value)
     (string-append "("
                    (canonical-case-text (car value))
                    " . "
                    (canonical-case-text (cdr value))
                    ")")]
    [else (render-atom value)]))

;; ============================================================
;; Tokens for dynamic fixture values
;; ============================================================

(define (map-strings value transform)
  (cond
    [(hash? value)
     (for/hash ([(key item) (in-hash value)])
       (values key (map-strings item transform)))]
    [(list? value) (map (lambda (item) (map-strings item transform)) value)]
    [(pair? value)
     (cons (map-strings (car value) transform) (map-strings (cdr value) transform))]
    [(string? value) (transform value)]
    [else value]))

(define (token-name token)
  (car token))

(define (token-value token)
  (cdr token))

(define (token-ordered tokens)
  (sort tokens > #:key (lambda (token) (string-length (token-value token)))))

;; `tokens` is a list of (name . value) pairs. Every full value becomes
;; {{name}} and its 12-hex prefix {{name-12}}; longest value first so a value
;; that contains another value still tokenizes completely.
(define (tokenize-value value tokens)
  (define ordered (token-ordered tokens))
  (map-strings
   value
   (lambda (text)
     (for/fold ([result text]) ([token (in-list ordered)])
       (define full (token-value token))
       (define named (string-replace result full (format "{{~a}}" (token-name token))))
       (if (> (string-length full) 12)
           (string-replace named (substring full 0 12) (format "{{~a-12}}" (token-name token)))
           named)))))

(define (detokenize-value value tokens)
  (define ordered (sort tokens string<? #:key token-name))
  (map-strings
   value
   (lambda (text)
     (define resolved
       (for/fold ([result text]) ([token (in-list ordered)])
         (define full (token-value token))
         (string-replace
          (string-replace result
                          (format "{{~a-12}}" (token-name token))
                          (substring full 0 (min 12 (string-length full))))
          (format "{{~a}}" (token-name token))
          full)))
     (when (regexp-match? #rx"[{][{]" resolved)
       (goldens-fail "tokens"
                     "the recorded answer still carries an unresolved token: ~s (supplied: ~a)"
                     resolved
                     (string-join (map token-name tokens) ", ")))
     resolved)))

;; ============================================================
;; The oracle read
;; ============================================================

;; `string->jsexpr` builds eq?-keyed hashes, but `read-json` builds equal?-keyed
;; ones, and `equal?` on hash tables is flavour-sensitive. The recorded value is
;; normalized to the `string->jsexpr` shape so a parity comparison is a pure
;; content comparison (the suites most often canonicalize first anyway).
(define (normalize-jsexpr value)
  (cond
    [(hash? value) (for/hasheq ([(key item) (in-hash value)]) (values key (normalize-jsexpr item)))]
    [(list? value) (map normalize-jsexpr value)]
    [else value]))

;; `spec` is the exact input handed to the oracle; `tokens` supplies the live
;; values for the group's dynamic placeholders. Fails loudly when the label is
;; absent, or when the recorded row was captured for a different case.
(define (golden-outcome group tokens label spec)
  (define loaded (gsd-delivery-goldens group))
  (define text (canonical-case-text (tokenize-value spec tokens)))
  (define entry
    (or (hash-ref (hash-ref loaded 'by-label) label #f)
        (goldens-fail (format "group ~a" group)
                      "no recorded oracle answer for case ~a; the parity table and the goldens disagree (labels: ~a)"
                      label
                      (string-join (hash-ref loaded 'labels) ", "))))
  (unless (equal? (hash-ref entry 'case) text)
    (goldens-fail (format "group ~a case ~a" group label)
                  (string-append
                   "STALE GOLDEN: the recorded answer was captured for a different case.\n"
                   "  recorded: ~a\n  driven:   ~a\n"
                   "Re-capture the golden against the oracle revision it names; "
                   "never read a stale row ")
                  (hash-ref entry 'case)
                  text))
  (normalize-jsexpr (detokenize-value (hash-ref entry 'outcome) tokens)))
