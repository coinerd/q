#lang racket/base

;; Canonical milestone truth is intentionally a pure, narrow boundary:
;; strict JSON in, deterministic projection bytes/digest, and one derived result.
;;
;; Canonicalization uses the restricted integer-only JCS profile implemented
;; below: JSON scalars plus safe integers only, UTF-16 object-key ordering, and
;; canonical UTF-8 emission.  This is deliberately not a claim of unrestricted
;; RFC 8785 support (notably, floating-point JSON numbers are rejected).

(require racket/cmdline
         racket/list
         racket/port
         racket/string
         (only-in file/sha1 bytes->hex-string)
         openssl)

(provide release-mechanics-values
         substantive-acceptance-values
         strict-json-read
         strict-json-read-file
         canonical-json-bytes
         canonical-json-sha256
         validate-milestone-document
         (struct-out milestone-truth)
         milestone-truth->jsexpr
         milestone-truth-gate-passed?
         evaluate-milestone-truth
         evaluate-milestone-file)

(define release-mechanics-values '("planned" "built" "smoked" "draft-verified" "published"))
(define substantive-acceptance-values '("planned" "in-progress" "rejected" "accepted"))

(define MAX-SAFE-INTEGER 9007199254740991)
(define digest-pattern #px"^[0-9a-f]{64}$")

;; Strict restricted-JSON reader --------------------------------------------

(define (strict-json-read source)
  (unless (string? source)
    (raise-argument-error 'strict-json-read "string?" source))
  (define length* (string-length source))
  (define position 0)
  (define (at-end?)
    (= position length*))
  (define (peek)
    (and (not (at-end?)) (string-ref source position)))
  (define (take!)
    (when (at-end?)
      (error 'strict-json-read "unexpected end of input at offset ~a" position))
    (define char (string-ref source position))
    (set! position (add1 position))
    char)
  (define (json-space? char)
    (and char (memv char '(#\space #\tab #\newline #\return))))
  (define (skip-space!)
    (let loop ()
      (when (json-space? (peek))
        (set! position (add1 position))
        (loop))))
  (define (expect! expected)
    (define actual (take!))
    (unless (char=? actual expected)
      (error 'strict-json-read "expected ~s at offset ~a, got ~s" expected (sub1 position) actual)))
  (define (read-hex4!)
    (when (> (+ position 4) length*)
      (error 'strict-json-read "incomplete Unicode escape at offset ~a" position))
    (define text (substring source position (+ position 4)))
    (unless (regexp-match? #px"^[0-9A-Fa-f]{4}$" text)
      (error 'strict-json-read "invalid Unicode escape \\u~a at offset ~a" text position))
    (set! position (+ position 4))
    (string->number text 16))
  (define (write-codepoint! output codepoint)
    (when (or (> codepoint #x10ffff) (<= #xd800 codepoint #xdfff))
      (error 'strict-json-read "invalid Unicode scalar U+~x" codepoint))
    (write-char (integer->char codepoint) output))
  (define (parse-string!)
    (expect! #\")
    (define output (open-output-string))
    (let loop ()
      (when (at-end?)
        (error 'strict-json-read "unterminated string"))
      (define char (take!))
      (cond
        [(char=? char #\") (get-output-string output)]
        [(char=? char #\\)
         (when (at-end?)
           (error 'strict-json-read "unterminated escape"))
         (define escaped (take!))
         (case escaped
           [(#\") (write-char #\" output)]
           [(#\\) (write-char #\\ output)]
           [(#\/) (write-char #\/ output)]
           [(#\b) (write-char #\backspace output)]
           [(#\f) (write-char #\page output)]
           [(#\n) (write-char #\newline output)]
           [(#\r) (write-char #\return output)]
           [(#\t) (write-char #\tab output)]
           [(#\u)
            (define first (read-hex4!))
            (cond
              [(<= #xd800 first #xdbff)
               (unless (and (<= (+ position 2) length*)
                            (char=? (string-ref source position) #\\)
                            (char=? (string-ref source (add1 position)) #\u))
                 (error 'strict-json-read "high surrogate is not followed by a low surrogate"))
               (set! position (+ position 2))
               (define second (read-hex4!))
               (unless (<= #xdc00 second #xdfff)
                 (error 'strict-json-read "high surrogate is not followed by a low surrogate"))
               (write-codepoint! output (+ #x10000 (* (- first #xd800) #x400) (- second #xdc00)))]
              [(<= #xdc00 first #xdfff) (error 'strict-json-read "unpaired low surrogate")]
              [else (write-codepoint! output first)])]
           [else
            (error 'strict-json-read
                   "invalid string escape \\~a at offset ~a"
                   escaped
                   (sub1 position))])
         (loop)]
        [(< (char->integer char) #x20)
         (error 'strict-json-read "unescaped control character in string")]
        [else
         (write-codepoint! output (char->integer char))
         (loop)])))
  (define (consume-literal! text value)
    (define end (+ position (string-length text)))
    (unless (and (<= end length*) (string=? (substring source position end) text))
      (error 'strict-json-read "expected ~a at offset ~a" text position))
    (set! position end)
    value)
  (define (parse-integer!)
    (define start position)
    (when (eqv? (peek) #\-)
      (set! position (add1 position)))
    (when (at-end?)
      (error 'strict-json-read "incomplete number at offset ~a" start))
    (cond
      [(eqv? (peek) #\0) (set! position (add1 position))]
      [(and (char-numeric? (peek)) (not (eqv? (peek) #\0)))
       (let loop ()
         (when (and (peek) (char-numeric? (peek)))
           (set! position (add1 position))
           (loop)))]
      [else (error 'strict-json-read "invalid number at offset ~a" start)])
    (when (and (peek) (memv (peek) '(#\. #\e #\E)))
      (error 'strict-json-read "only integers are allowed at offset ~a" start))
    (define value (string->number (substring source start position)))
    (unless (and (exact-integer? value) (<= (- MAX-SAFE-INTEGER) value MAX-SAFE-INTEGER))
      (error 'strict-json-read "integer is outside the safe range at offset ~a" start))
    value)
  (define parse-value! #f)
  (define (parse-array!)
    (expect! #\[)
    (skip-space!)
    (cond
      [(eqv? (peek) #\])
       (take!)
       '()]
      [else
       (let loop ([values '()])
         (define value (parse-value!))
         (skip-space!)
         (case (peek)
           [(#\,)
            (take!)
            (skip-space!)
            (loop (cons value values))]
           [(#\])
            (take!)
            (reverse (cons value values))]
           [else (error 'strict-json-read "expected comma or ] at offset ~a" position)]))]))
  (define (parse-object!)
    (expect! #\{)
    (skip-space!)
    (define entries (make-hash))
    (cond
      [(eqv? (peek) #\})
       (take!)
       (hash)]
      [else
       (let loop ()
         (unless (eqv? (peek) #\")
           (error 'strict-json-read "object key must be a string at offset ~a" position))
         (define key (parse-string!))
         (when (hash-has-key? entries key)
           (error 'strict-json-read "duplicate object key ~s" key))
         (skip-space!)
         (expect! #\:)
         (skip-space!)
         (hash-set! entries key (parse-value!))
         (skip-space!)
         (case (peek)
           [(#\,)
            (take!)
            (skip-space!)
            (loop)]
           [(#\})
            (take!)
            (make-immutable-hash (hash->list entries))]
           [else (error 'strict-json-read "expected comma or } at offset ~a" position)]))]))
  (set! parse-value!
        (lambda ()
          (skip-space!)
          (case (peek)
            [(#\") (parse-string!)]
            [(#\{) (parse-object!)]
            [(#\[) (parse-array!)]
            [(#\t) (consume-literal! "true" #t)]
            [(#\f) (consume-literal! "false" #f)]
            [(#\n) (consume-literal! "null" 'null)]
            [(#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (parse-integer!)]
            [else (error 'strict-json-read "expected a JSON value at offset ~a" position)])))
  (define result (parse-value!))
  (skip-space!)
  (unless (at-end?)
    (error 'strict-json-read "trailing input at offset ~a" position))
  result)

(define (strict-json-read-file path)
  (call-with-input-file path (lambda (input) (strict-json-read (port->string input)))))

;; Canonical restricted JSON -------------------------------------------------

(define (safe-integer? value)
  (and (exact-integer? value) (<= (- MAX-SAFE-INTEGER) value MAX-SAFE-INTEGER)))

(define (string->utf16-units value)
  (append-map (lambda (char)
                (define codepoint (char->integer char))
                (cond
                  [(<= #xd800 codepoint #xdfff)
                   (error 'canonical-json-bytes "string contains an invalid Unicode surrogate")]
                  [(<= codepoint #xffff) (list codepoint)]
                  [else
                   (define offset (- codepoint #x10000))
                   (list (+ #xd800 (quotient offset #x400)) (+ #xdc00 (remainder offset #x400)))]))
              (string->list value)))

(define (utf16-string<? left right)
  (let loop ([left-units (string->utf16-units left)]
             [right-units (string->utf16-units right)])
    (cond
      [(null? left-units) (pair? right-units)]
      [(null? right-units) #f]
      [(< (car left-units) (car right-units)) #t]
      [(> (car left-units) (car right-units)) #f]
      [else (loop (cdr left-units) (cdr right-units))])))

(define (canonical-json-bytes value)
  (define output (open-output-string))
  (define (write-string! value*)
    (write-char #\" output)
    (for ([char (in-string value*)])
      (define codepoint (char->integer char))
      (cond
        [(char=? char #\") (display "\\\"" output)]
        [(char=? char #\\) (display "\\\\" output)]
        [(char=? char #\backspace) (display "\\b" output)]
        [(char=? char #\page) (display "\\f" output)]
        [(char=? char #\newline) (display "\\n" output)]
        [(char=? char #\return) (display "\\r" output)]
        [(char=? char #\tab) (display "\\t" output)]
        [(< codepoint #x20)
         (define hex (number->string codepoint 16))
         (display "\\u" output)
         (display (make-string (- 4 (string-length hex)) #\0) output)
         (display hex output)]
        [(<= #xd800 codepoint #xdfff)
         (error 'canonical-json-bytes "string contains an invalid Unicode surrogate")]
        [else (write-char char output)]))
    (write-char #\" output))
  (define (write-value! current)
    (cond
      [(eq? current 'null) (display "null" output)]
      [(boolean? current) (display (if current "true" "false") output)]
      [(string? current) (write-string! current)]
      [(safe-integer? current) (display current output)]
      [(number? current) (error 'canonical-json-bytes "number is not a safe integer: ~e" current)]
      [(list? current)
       (write-char #\[ output)
       (for ([element (in-list current)]
             [index (in-naturals)])
         (unless (zero? index)
           (write-char #\, output))
         (write-value! element))
       (write-char #\] output)]
      [(hash? current)
       (unless (andmap string? (hash-keys current))
         (error 'canonical-json-bytes "object keys must all be strings"))
       (write-char #\{ output)
       (for ([key (in-list (sort (hash-keys current) utf16-string<?))]
             [index (in-naturals)])
         (unless (zero? index)
           (write-char #\, output))
         (write-string! key)
         (write-char #\: output)
         (write-value! (hash-ref current key)))
       (write-char #\} output)]
      [else (error 'canonical-json-bytes "value is outside the restricted JSON model: ~e" current)]))
  (write-value! value)
  (string->bytes/utf-8 (get-output-string output)))

(define (canonical-json-sha256 value)
  (bytes->hex-string (sha256-bytes (open-input-bytes (canonical-json-bytes value)))))

;; Closed milestone schema ---------------------------------------------------

(define top-level-keys
  '("acceptance-critical-remaining-items" "milestone"
                                          "release-evidence"
                                          "release-mechanics"
                                          "review"
                                          "schema-version"
                                          "substantive-acceptance"
                                          "validation"
                                          "work-items"))

(define (require-exact-keys who value expected)
  (unless (hash? value)
    (error who "expected an object, got ~e" value))
  (define actual (sort (hash-keys value) string<?))
  (define wanted (sort expected string<?))
  (unless (equal? actual wanted)
    (error who "object keys must be exactly ~e; got ~e" wanted actual)))

(define (non-empty-string? value)
  (and (string? value) (not (string=? (string-trim value) ""))))

(define (member-string? value choices)
  (and (string? value) (member value choices) #t))

(define release-stage-names '("build" "exact-artifact-smoke" "draft-verification" "publication"))

(define (digest? value)
  (and (string? value) (regexp-match? digest-pattern value) #t))

(define (evidence-or-null? value)
  (or (eq? value 'null) (digest? value)))

(define (validate-milestone-document document)
  ;; This also enforces the restricted in-memory JSON model before schema checks.
  (canonical-json-bytes document)
  (require-exact-keys 'validate-milestone-document document top-level-keys)
  (unless (equal? (hash-ref document "schema-version") 1)
    (error 'validate-milestone-document "schema-version must be 1"))
  (unless (and (non-empty-string? (hash-ref document "milestone"))
               (regexp-match? #px"^v[0-9]+\\.[0-9]+\\.[0-9]+$" (hash-ref document "milestone")))
    (error 'validate-milestone-document "milestone must be a v-prefixed semantic version"))
  (unless (member-string? (hash-ref document "release-mechanics") release-mechanics-values)
    (error 'validate-milestone-document "release-mechanics is not a locked value"))
  (unless (member-string? (hash-ref document "substantive-acceptance") substantive-acceptance-values)
    (error 'validate-milestone-document "substantive-acceptance is not a locked value"))

  ;; A release-evidence array is an ordered completed prefix.  Consequently a
  ;; later stage can never appear before, or without, an earlier stage.
  (define release-evidence (hash-ref document "release-evidence"))
  (unless (and (list? release-evidence) (<= (length release-evidence) 4))
    (error 'validate-milestone-document "release-evidence must contain at most four stages"))
  (for ([entry (in-list release-evidence)]
        [expected-stage (in-list release-stage-names)])
    (require-exact-keys 'validate-milestone-document entry '("evidence-digest" "stage"))
    (unless (string=? (hash-ref entry "stage") expected-stage)
      (error 'validate-milestone-document
             "release-evidence must be the gap-free ordered stage prefix; expected ~a"
             expected-stage))
    (unless (digest? (hash-ref entry "evidence-digest"))
      (error 'validate-milestone-document "completed release stage requires a 64-hex digest")))

  (define work-items (hash-ref document "work-items"))
  (unless (and (list? work-items) (= (length work-items) 11))
    (error 'validate-milestone-document "work-items must contain exactly W0 through W10"))
  (for ([item (in-list work-items)]
        [index (in-range 11)])
    (require-exact-keys 'validate-milestone-document item '("criteria" "id" "status"))
    (unless (equal? (hash-ref item "id") (format "W~a" index))
      (error 'validate-milestone-document "work-items must be ordered exactly W0 through W10"))
    (unless (member-string? (hash-ref item "status") '("planned" "in-progress" "complete"))
      (error 'validate-milestone-document "work-item status is invalid for ~a" (hash-ref item "id")))
    (define criteria (hash-ref item "criteria"))
    (unless (and (list? criteria) (pair? criteria))
      (error 'validate-milestone-document "~a criteria must be non-empty" (hash-ref item "id")))
    (define criterion-ids '())
    (for ([criterion (in-list criteria)])
      (require-exact-keys 'validate-milestone-document criterion '("evidence-digest" "id" "met"))
      (unless (non-empty-string? (hash-ref criterion "id"))
        (error 'validate-milestone-document "criterion id must be a non-empty string"))
      (unless (boolean? (hash-ref criterion "met"))
        (error 'validate-milestone-document "criterion met must be boolean"))
      (unless (evidence-or-null? (hash-ref criterion "evidence-digest"))
        (error 'validate-milestone-document "criterion evidence-digest must be null or 64-hex"))
      (when (and (hash-ref criterion "met") (not (digest? (hash-ref criterion "evidence-digest"))))
        (error 'validate-milestone-document "met criterion requires a 64-hex evidence digest"))
      (set! criterion-ids (cons (hash-ref criterion "id") criterion-ids)))
    (unless (= (length criterion-ids) (length (remove-duplicates criterion-ids string=?)))
      (error 'validate-milestone-document
             "criterion ids must be unique within ~a"
             (hash-ref item "id")))
    (when (and (string=? (hash-ref item "status") "complete")
               (ormap (lambda (criterion) (not (hash-ref criterion "met"))) criteria))
      (error 'validate-milestone-document "complete work item requires every criterion met"))
    (when (and (string=? (hash-ref item "status") "planned")
               (ormap (lambda (criterion) (hash-ref criterion "met")) criteria))
      (error 'validate-milestone-document "planned work item cannot have met criteria")))

  (define remaining (hash-ref document "acceptance-critical-remaining-items"))
  (unless (and (list? remaining) (andmap non-empty-string? remaining))
    (error 'validate-milestone-document
           "acceptance-critical-remaining-items must be an array of non-empty strings"))
  (unless (= (length remaining) (length (remove-duplicates remaining string=?)))
    (error 'validate-milestone-document "acceptance-critical remaining items must be unique"))

  (define review (hash-ref document "review"))
  (require-exact-keys 'validate-milestone-document review '("evidence-digest" "independent" "status"))
  (unless (member-string? (hash-ref review "status") '("pending" "approved" "rejected"))
    (error 'validate-milestone-document "review status is invalid"))
  (unless (boolean? (hash-ref review "independent"))
    (error 'validate-milestone-document "review independent must be boolean"))
  (unless (evidence-or-null? (hash-ref review "evidence-digest"))
    (error 'validate-milestone-document "review evidence-digest must be null or 64-hex"))
  (when (and (string=? (hash-ref review "status") "pending")
             (not (eq? (hash-ref review "evidence-digest") 'null)))
    (error 'validate-milestone-document "pending review evidence-digest must be null"))
  (when (and (not (string=? (hash-ref review "status") "pending"))
             (not (digest? (hash-ref review "evidence-digest"))))
    (error 'validate-milestone-document "completed review requires a 64-hex evidence digest"))

  (define validation (hash-ref document "validation"))
  (require-exact-keys 'validate-milestone-document validation '("evidence-digest" "status"))
  (unless (member-string? (hash-ref validation "status") '("pending" "passed" "failed"))
    (error 'validate-milestone-document "validation status is invalid"))
  (unless (evidence-or-null? (hash-ref validation "evidence-digest"))
    (error 'validate-milestone-document "validation evidence-digest must be null or 64-hex"))
  (when (and (string=? (hash-ref validation "status") "pending")
             (not (eq? (hash-ref validation "evidence-digest") 'null)))
    (error 'validate-milestone-document "pending validation evidence-digest must be null"))
  (when (and (not (string=? (hash-ref validation "status") "pending"))
             (not (digest? (hash-ref validation "evidence-digest"))))
    (error 'validate-milestone-document "completed validation requires a 64-hex evidence digest"))
  #t)

;; Pure truth evaluator ------------------------------------------------------

(struct milestone-truth
        (milestone declared-release-mechanics
                   derived-release-mechanics
                   effective-release-mechanics
                   declared-substantive-acceptance
                   derived-substantive-acceptance
                   effective-substantive-acceptance
                   accepted?
                   reasons
                   projection-digest
                   digest-matches?)
  #:transparent)

(define (derived-release-mechanics document)
  (list-ref release-mechanics-values (length (hash-ref document "release-evidence"))))

(define (evaluate-milestone-truth document external-projection-digest)
  (validate-milestone-document document)
  (define canonical-digest (canonical-json-sha256 document))
  (define digest-matches?
    (and (digest? external-projection-digest) (string=? canonical-digest external-projection-digest)))
  (define declared-release (hash-ref document "release-mechanics"))
  (define derived-release (derived-release-mechanics document))
  (define release-consistent? (string=? declared-release derived-release))
  (define work-items (hash-ref document "work-items"))
  (define statuses (map (lambda (item) (hash-ref item "status")) work-items))
  (define all-complete? (andmap (lambda (status) (string=? status "complete")) statuses))
  (define all-planned? (andmap (lambda (status) (string=? status "planned")) statuses))
  (define all-criteria-met?
    (for*/and ([item (in-list work-items)]
               [criterion (in-list (hash-ref item "criteria"))])
      (hash-ref criterion "met")))
  (define remaining (hash-ref document "acceptance-critical-remaining-items"))
  (define review (hash-ref document "review"))
  (define validation (hash-ref document "validation"))
  (define review-approved? (string=? (hash-ref review "status") "approved"))
  (define independent? (hash-ref review "independent"))
  (define review-evidenced? (digest? (hash-ref review "evidence-digest")))
  (define validation-passed? (string=? (hash-ref validation "status") "passed"))
  (define validation-evidenced? (digest? (hash-ref validation "evidence-digest")))
  (define acceptance-gates-pass?
    (and all-complete?
         all-criteria-met?
         (null? remaining)
         review-approved?
         independent?
         review-evidenced?
         validation-passed?
         validation-evidenced?
         digest-matches?))
  (define derived-substantive
    (cond
      [acceptance-gates-pass? "accepted"]
      [(or (string=? (hash-ref review "status") "rejected")
           (string=? (hash-ref validation "status") "failed")
           (not digest-matches?)
           all-complete?)
       "rejected"]
      [(and all-planned?
            (string=? (hash-ref review "status") "pending")
            (string=? (hash-ref validation "status") "pending"))
       "planned"]
      [else "in-progress"]))
  (define declared-substantive (hash-ref document "substantive-acceptance"))
  (define substantive-consistent? (string=? declared-substantive derived-substantive))
  (define effective-substantive
    (if (and release-consistent? substantive-consistent?) derived-substantive "rejected"))
  (define reasons
    (append (if all-complete?
                '()
                '("work records W0-W10 are not all complete"))
            (if all-criteria-met?
                '()
                '("not all criteria are met"))
            (if (null? remaining)
                '()
                '("acceptance-critical remaining items are not empty"))
            (if review-approved?
                '()
                '("review is not approved"))
            (if independent?
                '()
                '("review is not independent"))
            (if review-evidenced?
                '()
                '("review does not have a 64-hex evidence digest"))
            (if validation-passed?
                '()
                '("validation has not passed"))
            (if validation-evidenced?
                '()
                '("validation does not have a 64-hex evidence digest"))
            (if digest-matches?
                '()
                '("projection digest does not match canonical projection"))
            (if release-consistent?
                '()
                (list (format "declared release-mechanics ~a differs from derived ~a"
                              declared-release
                              derived-release)))
            (if substantive-consistent?
                '()
                (list (format "declared substantive-acceptance ~a differs from derived ~a"
                              declared-substantive
                              derived-substantive)))))
  (milestone-truth (hash-ref document "milestone")
                   declared-release
                   derived-release
                   (if release-consistent? derived-release "inconsistent")
                   declared-substantive
                   derived-substantive
                   effective-substantive
                   (string=? effective-substantive "accepted")
                   reasons
                   canonical-digest
                   digest-matches?))

(define (evaluate-milestone-file path external-projection-digest)
  ;; The expected digest is intentionally a separate argument and is never read
  ;; from, or synthesized out of, the milestone file by this boundary.
  (evaluate-milestone-truth (strict-json-read-file path) external-projection-digest))

(define (milestone-truth-gate-passed? truth)
  ;; The standalone command is an acceptance gate by default. Keep every
  ;; condition explicit so a synthetic or stale result cannot pass merely by
  ;; setting accepted? while declarations disagree with their derivations.
  (and (milestone-truth? truth)
       (string? (milestone-truth-milestone truth))
       (regexp-match? #px"^v[0-9]+\\.[0-9]+\\.[0-9]+$" (milestone-truth-milestone truth))
       (digest? (milestone-truth-projection-digest truth))
       (eq? (milestone-truth-digest-matches? truth) #t)
       (equal? (milestone-truth-declared-release-mechanics truth)
               (milestone-truth-derived-release-mechanics truth))
       (equal? (milestone-truth-effective-release-mechanics truth)
               (milestone-truth-derived-release-mechanics truth))
       (equal? (milestone-truth-declared-substantive-acceptance truth)
               (milestone-truth-derived-substantive-acceptance truth))
       (equal? (milestone-truth-effective-substantive-acceptance truth)
               (milestone-truth-derived-substantive-acceptance truth))
       (equal? (milestone-truth-effective-substantive-acceptance truth) "accepted")
       (eq? (milestone-truth-accepted? truth) #t)))

(define (milestone-truth->jsexpr truth)
  (hash "milestone"
        (milestone-truth-milestone truth)
        "release-mechanics"
        (hash "declared"
              (milestone-truth-declared-release-mechanics truth)
              "derived"
              (milestone-truth-derived-release-mechanics truth)
              "effective"
              (milestone-truth-effective-release-mechanics truth))
        "substantive-acceptance"
        (hash "declared"
              (milestone-truth-declared-substantive-acceptance truth)
              "derived"
              (milestone-truth-derived-substantive-acceptance truth)
              "effective"
              (milestone-truth-effective-substantive-acceptance truth))
        "accepted"
        (milestone-truth-accepted? truth)
        "reasons"
        (milestone-truth-reasons truth)
        "projection-digest"
        (milestone-truth-projection-digest truth)
        "digest-matches"
        (milestone-truth-digest-matches? truth)))

(module+ main
  (define supplied-digest #f)
  (define report-only? #f)
  (command-line
   #:program "gsd-milestone-truth.rkt"
   #:usage-help
   "Default gate: exits nonzero unless digest-bound effective acceptance is accepted and declarations are consistent."
   "Use --report-only only to inspect valid in-progress or rejected history."
   #:once-each [("-d" "--digest")
                digest
                "independently supplied canonical projection SHA-256 (required)"
                (set! supplied-digest digest)]
   [("--report-only")
    "inspect valid in-progress/rejected history without gate exit semantics"
    (set! report-only? #t)]
   #:args (json-file)
   (unless supplied-digest
     (raise-user-error 'gsd-milestone-truth.rkt "--digest is required"))
   (let ([truth (evaluate-milestone-file json-file supplied-digest)])
     (write-bytes (canonical-json-bytes (milestone-truth->jsexpr truth)))
     (newline)
     (unless (or report-only? (milestone-truth-gate-passed? truth))
       (exit 1)))))
