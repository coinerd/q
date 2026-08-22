#lang racket/base

;; @speed fast  ;; @suite provider

;; Test-only architecture checker for v0.99.91 Path-B provider ownership.
;; It deliberately does not parse or normalize provider wire events.

(require racket/file
         racket/list
         racket/path
         racket/port
         racket/string)

(provide (struct-out source-unit)
         (struct-out protocol-marker)
         (struct-out provider-protocol)
         (struct-out neutral-helper)
         (struct-out provider-locality-policy)
         (struct-out locality-violation)
         load-provider-locality-policy
         production-llm-source-units
         check-provider-locality-policy
         check-provider-locality-policy-units
         check-provider-change-locality
         locality-violation->string)

(struct source-unit (path source) #:transparent)
(struct protocol-marker (value context) #:transparent)
(struct provider-protocol (name owners markers) #:transparent)
(struct neutral-helper
        (module primitives evidence
          )
  #:transparent)
(struct provider-locality-policy (version protocols generic-streaming-modules neutral-helpers)
  #:transparent)
(struct locality-violation (path provider marker reason allowed-owners) #:transparent)
(struct source-analysis (forms facts error) #:transparent)

(define frozen-neutral-helper-spec
  '(("llm/http-helpers.rkt" (make-provider-http-request check-provider-status! translate-stop-reason)
                            (C1 C5 C6))
    ("llm/stream.rkt" (stream-sse-events parse-sse-line
                                         parse-sse-lines
                                         sse-done?
                                         close-port-after-stream
                                         accumulate-tool-call-deltas
                                         call-with-request-timeout
                                         read-line/timeout
                                         read-response-body/timeout
                                         current-http-request-timeout)
                      (C2 C3 C7 C8))
    ("llm/provider-errors.rkt" (provider-error classify-http-status raise-provider-error) (C4))))

(define (section datum name)
  (define found (assoc name datum))
  (unless found
    (error 'load-provider-locality-policy "missing policy section: ~a" name))
  (cdr found))

(define (entry-field entry name)
  (define found (assoc name (cdr entry)))
  (unless found
    (error 'load-provider-locality-policy "missing ~a in ~a" name (car entry)))
  (cdr found))

(define (load-provider-locality-policy path)
  (define input (open-input-file path))
  (define datum
    (with-handlers ([exn:fail:read? (lambda (e)
                                      (close-input-port input)
                                      (raise e))])
      (read input)))
  (define trailing (read input))
  (close-input-port input)
  (unless (and (pair? datum) (eof-object? trailing))
    (error 'load-provider-locality-policy
           "policy must contain exactly one complete datum, got ~s + ~s"
           datum
           trailing))
  (define protocols
    (for/list ([entry (in-list (section datum 'provider-protocols))])
      (provider-protocol (car entry)
                         (entry-field entry 'owners)
                         (for/list ([marker (in-list (entry-field entry 'markers))])
                           (protocol-marker (car marker) (cdr marker))))))
  (define neutral-helpers
    (for/list ([entry (in-list (section datum 'neutral-helpers))])
      (neutral-helper (car entry) (entry-field entry 'primitives) (entry-field entry 'evidence))))
  (provider-locality-policy (section datum 'version)
                            protocols
                            (section datum 'generic-streaming-modules)
                            neutral-helpers))

(define (source-text-without-lang source)
  (define lines (string-split source "\n" #:trim? #f))
  (string-join (if (and (pair? lines) (string-prefix? (car lines) "#lang"))
                   (cdr lines)
                   lines)
               "\n"))

(define (analyze-source source)
  (define read-result
    (with-handlers ([exn:fail:read? values])
      (port->list read (open-input-string (source-text-without-lang source)))))
  (if (exn:fail:read? read-result)
      (source-analysis '() '() (exn-message read-result))
      (let ([facts '()])
        (define (add-fact! value context)
          (set! facts (cons (cons value context) facts)))
        (define (quoted-key value)
          (cond
            [(and (pair? value) (eq? (car value) 'quote) (pair? (cdr value)) (symbol? (cadr value)))
             (symbol->string (cadr value))]
            [(symbol? value) (symbol->string value)]
            [(string? value) value]
            [else #f]))
        (define (hash-constructor? value)
          (and (pair? value)
               (symbol? (car value))
               (memq (car value) '(hash hasheq hasheqv hash* hasheq* hasheqv*))))
        (define (alist-builder? value)
          (and (pair? value)
               (symbol? (car value))
               (pair? (cdr value))
               (memq (car value)
                     '(make-hash make-hasheq
                                 make-hasheqv
                                 make-immutable-hash
                                 make-immutable-hasheq
                                 make-immutable-hasheqv
                                 make-hash-table
                                 make-weak-hasheq))))
        (define (walk value)
          (cond
            [(pair? value)
             (when (and (symbol? (car value))
                        (memq (car value)
                              '(hash-ref hash-ref!
                                         hash-has-key?
                                         hash-set
                                         hash-set!
                                         hash-update
                                         hash-remove
                                         hash-remove!
                                         dict-ref))
                        (pair? (cdr value))
                        (pair? (cddr value)))
               (define key (quoted-key (caddr value)))
               (when key
                 (add-fact! key 'hash-key)))
             (cond
               [(hash-constructor? value)
                ;; (hash k1 v1 k2 v2 ...) / (hash* k1 v1 ...) — keys at odd cdr positions.
                (define args (cdr value))
                (let loop ([remaining args])
                  (when (and (pair? remaining) (pair? (cdr remaining)))
                    (define key (quoted-key (car remaining)))
                    (when key
                      (add-fact! key 'hash-key))
                    (loop (cddr remaining))))
                (for-each walk args)]
               [(alist-builder? value)
                ;; (make-hasheq '((k . v) ...)) — keyed by quoted association pairs.
                (define alist-value (cadr value))
                (define alist
                  (cond
                    [(and (pair? alist-value)
                          (eq? (car alist-value) 'quote)
                          (pair? (cdr alist-value)))
                     (cadr alist-value)]
                    [else alist-value]))
                (for ([pair (in-list (if (pair? alist)
                                         alist
                                         '()))]
                      #:when (pair? pair))
                  (define key (quoted-key (car pair)))
                  (when key
                    (add-fact! key 'hash-key)))
                (for-each walk (cdr value))]
               [else
                (walk (car value))
                (walk (cdr value))])]
            [(vector? value) (for-each walk (vector->list value))]
            [(box? value) (walk (unbox value))]
            [(hash? value)
             ;; Literal #hash/#hasheq tables: every key is a wire key in scope.
             (for ([(key item) (in-hash value)])
               (define literal-key (quoted-key key))
               (when literal-key
                 (add-fact! literal-key 'hash-key))
               (walk key)
               (walk item))]
            [(string? value) (add-fact! value 'string-literal)]
            [(bytes? value) (add-fact! (bytes->string/utf-8 value #\uFFFD) 'encoded-literal)]
            [(regexp? value)
             (define pattern (object-name value))
             (add-fact! (if (bytes? pattern)
                            (bytes->string/utf-8 pattern #\uFFFD)
                            pattern)
                        'encoded-literal)]
            [else (void)]))
        (for-each walk read-result)
        (source-analysis read-result (remove-duplicates facts equal?) #f))))

(define (production-llm-source-units repo-root)
  (define resolved-root (simplify-path repo-root))
  (define llm-root (build-path resolved-root "llm"))
  (sort (for/list ([path (in-directory llm-root)]
                   #:when (and (file-exists? path) (regexp-match? #rx"[.]rkt$" (path->string path))))
          (source-unit (path->string (find-relative-path resolved-root path)) (file->string path)))
        string<?
        #:key source-unit-path))

(define (marker-observed? marker facts)
  (for/or ([fact (in-list facts)])
    (and (string=? (protocol-marker-value marker) (car fact))
         (or (eq? (protocol-marker-context marker) (cdr fact)) (eq? 'encoded-literal (cdr fact))))))

(define (check-provider-change-locality policy units)
  (sort
   (append*
    (for/list ([unit (in-list units)])
      (define path (source-unit-path unit))
      (define analysis (analyze-source (source-unit-source unit)))
      (if (source-analysis-error analysis)
          (list (locality-violation path
                                    'source-reader
                                    (source-analysis-error analysis)
                                    'source-read-error
                                    '()))
          (append* (for/list ([protocol (in-list (provider-locality-policy-protocols policy))])
                     (for/list ([marker (in-list (provider-protocol-markers protocol))]
                                #:when (and (marker-observed? marker (source-analysis-facts analysis))
                                            (not (member path (provider-protocol-owners protocol)))))
                       (locality-violation
                        path
                        (provider-protocol-name protocol)
                        (protocol-marker-value marker)
                        (if (member path (provider-locality-policy-generic-streaming-modules policy))
                            'generic-stream-protocol
                            'wrong-provider-owner)
                        (provider-protocol-owners protocol))))))))
   string<?
   #:key (lambda (violation)
           (format "~a|~a|~a"
                   (locality-violation-path violation)
                   (locality-violation-provider violation)
                   (locality-violation-marker violation)))))

(define (top-level-definition-names forms)
  (remove-duplicates (for/list ([form (in-list forms)]
                                #:when (and (pair? form)
                                            (memq (car form)
                                                  '(define define/contract
                                                     define-runtime-path
                                                     struct))
                                            (pair? (cdr form)))
                                #:do [(define binding (cadr form))]
                                #:when (or (symbol? binding)
                                           (and (pair? binding) (symbol? (car binding)))))
                       (if (symbol? binding)
                           binding
                           (car binding)))))

(define (struct-fields forms name)
  (for/or ([form (in-list forms)]
           #:when
           (and (pair? form) (eq? (car form) 'struct) (pair? (cdr form)) (eq? (cadr form) name)))
    (caddr form)))

;; Interpret (provide ...) spec forms semantically enough to decide whether a
;; named primitive is actually exported. Handles direct symbols, all-defined-out,
;; contract-out, struct-out, except-out, rename-out, and prefix-out.
(define (provide-exports forms)
  (define defined-names (top-level-definition-names forms))
  (define (resolve spec)
    (cond
      [(symbol? spec) (list spec)]
      [(pair? spec)
       (case (car spec)
         [(all-defined-out) defined-names]
         [(all-from-out) '()]
         [(contract-out)
          (for/list ([item (in-list (cdr spec))]
                     #:when (and (pair? item) (symbol? (car item))))
            (car item))]
         [(struct-out)
          (define name (cadr spec))
          (define fields (struct-fields forms name))
          (append (list name (string->symbol (format "~a?" name)))
                  (for/list ([field (in-list (if (pair? fields)
                                                 fields
                                                 '()))])
                    (string->symbol (format "~a-~a" name field))))]
         [(except-out) (remove* (cddr spec) (resolve (cadr spec)))]
         [(rename-out)
          (for/list ([entry (in-list (cdr spec))]
                     #:when (and (pair? entry) (pair? (cdr entry))))
            (cadr entry))]
         [(prefix-out)
          (define prefix (cadr spec))
          (map (lambda (name) (string->symbol (format "~a~a" prefix name))) (resolve (caddr spec)))]
         [else '()])]
      [else '()]))
  (remove-duplicates (append* (for/list ([form (in-list forms)]
                                         #:when (and (pair? form) (eq? (car form) 'provide)))
                                (append* (for/list ([spec (in-list (cdr form))])
                                           (resolve spec)))))
                     eq?))

(define (neutral-helper-spec helpers)
  (for/list ([helper (in-list helpers)])
    (list (neutral-helper-module helper)
          (neutral-helper-primitives helper)
          (neutral-helper-evidence helper))))

(define (check-provider-locality-policy-units policy units)
  (define units-by-path
    (for/hash ([unit (in-list units)])
      (values (source-unit-path unit) unit)))
  (define analyses-by-path
    (for/hash ([unit (in-list units)])
      (values (source-unit-path unit) (analyze-source (source-unit-source unit)))))
  (define protocols (provider-locality-policy-protocols policy))
  (define all-markers (append-map provider-protocol-markers protocols))
  (append
   (if (= (provider-locality-policy-version policy) 1)
       '()
       (list (list 'unsupported-version (provider-locality-policy-version policy))))
   (if (equal? (neutral-helper-spec (provider-locality-policy-neutral-helpers policy))
               frozen-neutral-helper-spec)
       '()
       (list (list 'neutral-helper-allowlist-drift
                   (neutral-helper-spec (provider-locality-policy-neutral-helpers policy)))))
   (for/list ([(path analysis) (in-hash analyses-by-path)]
              #:when (source-analysis-error analysis))
     (list 'unreadable-source path (source-analysis-error analysis)))
   (for/list ([marker (in-list (remove-duplicates all-markers))]
              #:when
              (> (count (lambda (candidate)
                          (string=? (protocol-marker-value candidate) (protocol-marker-value marker)))
                        all-markers)
                 1))
     (list 'duplicate-marker (protocol-marker-value marker)))
   (for/list ([marker (in-list all-markers)]
              #:unless (memq (protocol-marker-context marker) '(hash-key string-literal)))
     (list 'unsupported-marker-context
           (protocol-marker-value marker)
           (protocol-marker-context marker)))
   (append* (for/list ([protocol (in-list protocols)])
              (append (for/list ([owner (in-list (provider-protocol-owners protocol))]
                                 #:unless (hash-has-key? units-by-path owner))
                        (list 'missing-owner (provider-protocol-name protocol) owner))
                      (for/list ([marker (in-list (provider-protocol-markers protocol))]
                                 #:unless
                                 (for/or ([owner (in-list (provider-protocol-owners protocol))])
                                   (define analysis (hash-ref analyses-by-path owner #f))
                                   (and analysis
                                        (not (source-analysis-error analysis))
                                        (marker-observed? marker (source-analysis-facts analysis)))))
                        (list 'unobserved-marker
                              (provider-protocol-name protocol)
                              (protocol-marker-value marker))))))
   (for/list ([path (in-list (provider-locality-policy-generic-streaming-modules policy))]
              #:unless (hash-has-key? units-by-path path))
     (list 'missing-generic-streaming-module path))
   (append*
    (for/list ([helper (in-list (provider-locality-policy-neutral-helpers policy))])
      (define module (neutral-helper-module helper))
      (define analysis (hash-ref analyses-by-path module #f))
      (cond
        [(not analysis) (list (list 'missing-neutral-helper-module module))]
        [(source-analysis-error analysis) '()]
        [else
         (define definitions (top-level-definition-names (source-analysis-forms analysis)))
         (define exports (provide-exports (source-analysis-forms analysis)))
         (append*
          (for/list ([primitive (in-list (neutral-helper-primitives helper))])
            (define definition-paths
              (for/list ([(path candidate-analysis) (in-hash analyses-by-path)]
                         #:when (and (not (source-analysis-error candidate-analysis))
                                     (member primitive
                                             (top-level-definition-names
                                              (source-analysis-forms candidate-analysis)))))
                path))
            (append (if (equal? definition-paths (list module))
                        '()
                        (list (list 'neutral-primitive-ownership primitive module definition-paths)))
                    (if (member primitive definitions)
                        '()
                        (list (list 'missing-neutral-primitive-definition module primitive)))
                    (if (member primitive exports)
                        '()
                        (list (list 'missing-neutral-primitive-export module primitive))))))])))))

(define (check-provider-locality-policy policy repo-root)
  (check-provider-locality-policy-units policy (production-llm-source-units repo-root)))

(define (locality-violation->string violation)
  (if (eq? (locality-violation-reason violation) 'source-read-error)
      (format "Cannot verify provider change locality in ~a: Racket reader failed: ~a"
              (locality-violation-path violation)
              (locality-violation-marker violation))
      (format
       "Provider protocol marker ~s for ~a appears in ~a (~a); move it to an allowed ownership path: ~a"
       (locality-violation-marker violation)
       (locality-violation-provider violation)
       (locality-violation-path violation)
       (locality-violation-reason violation)
       (string-join (locality-violation-allowed-owners violation) ", "))))
