#lang racket/base

;; scripts/semantic-navigator.rkt — CL2 (#9979), developer-only semantic navigator.
;;
;; Reads CURRENT repository facts only: tracked Racket sources, the CL1 derived
;; subsystem manifest/maps, the @covers manifest and qt's own admission/selection
;; machinery. It produces bounded navigational evidence (where an owner lives,
;; which modules import it, which tests mention coverage) with explicit
;; uncertainty for dynamic edges, macros and missing/stale metadata.
;;
;; NOT a behavioral verifier and NOT an authority for PR verification: a
;; navigational fact is not a verified contract. `context` reuses qt's
;; changed-selection as a REPRODUCTION aid; required CI contexts remain
;; authoritative. No LLM and no trusted-runtime code is involved.

(require racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/path
         racket/port
         racket/string
         racket/system
         openssl/sha1
         json
         "architecture-baseline-helpers.rkt"
         "testing/full-plan.rkt"
         (only-in "testing/quick-audit.rkt" admitted-owners))

(provide navigator-root
         invalidate-index-cache!
         build-index
         map-module-paths
         bundle-paths
         symbol-query
         module-query
         focused-context
         benchmark-report)

(define navigator-root (make-parameter #f))
(define index-cache (make-hash))
(define (invalidate-index-cache!)
  (hash-clear! index-cache))

(define verification-note "navigational evidence only; not behavioral verification")
(define context-note
  "focused reproduction only, not PR verification; required CI contexts remain authoritative")

;; ---------------------------------------------------------------- filesystem --

(define (resolve-root root)
  (define candidate (or root (navigator-root)))
  (if candidate
      (path->complete-path candidate)
      (let loop ([dir (current-directory)])
        (if (and (file-exists? (build-path dir "qt"))
                 (file-exists? (build-path dir "scripts" "subsystem-manifest.rkt")))
            dir
            (let ([parent (path-only dir)])
              (if (and parent (not (equal? parent dir)))
                  (loop parent)
                  (raise-user-error 'semantic-navigator "cannot locate repository root")))))))

(define (git-output root args)
  (define exe (find-executable-path "git"))
  (unless exe
    (raise-user-error 'semantic-navigator "git is required for the tracked-source index"))
  (parameterize ([current-directory root]
                 [current-error-port (open-output-string)])
    (with-output-to-string (lambda () (apply system* exe args)))))

(define (tracked-rkt-paths root)
  (sort (filter (lambda (s) (string-suffix? s ".rkt"))
                (string-split (git-output root '("ls-files" "--" "*.rkt")) "\n"))
        string<?))

(define (read-datums source)
  (define without-lang (regexp-replace #px"^#lang[^\n]*(?:\n|$)" source ""))
  (with-handlers ([exn:fail:read? (lambda (_) '())])
    (call-with-input-string without-lang
                            (lambda (in)
                              (let loop ([acc '()])
                                (define datum (read in))
                                (if (eof-object? datum)
                                    (reverse acc)
                                    (loop (cons datum acc))))))))

(define (source-text root path)
  (call-with-input-file (build-path root (string->path path)) port->string))

(define (sha1-fingerprint root rel)
  (if (file-exists? (build-path root (string->path rel)))
      (call-with-input-file (build-path root (string->path rel)) sha1)
      "missing"))

(define (manifest-path)
  "docs/architecture/subsystem-manifest.rktd")
(define (coverage-path)
  "tests/.coverage-manifest.json")
(define (admission-path)
  "tests/.quick-admission.rktd")
(define (maps-dir)
  "docs/architecture/subsystem-maps")

(define (map-files root)
  (define dir (build-path root (string->path (maps-dir))))
  (if (directory-exists? dir)
      (sort (for/list ([p (in-list (directory-list dir))]
                       #:when (and (path-has-extension? p #".md")
                                   (not (equal? (path->string p) "README.md"))))
              (string-append (maps-dir) "/" (path->string p)))
            string<?)
      '()))

(define (load-manifest root)
  (define path (build-path root (string->path (manifest-path))))
  (if (file-exists? path)
      (call-with-input-file path read)
      '()))

;; Metadata records use two shapes: proper (key value ...) assoc entries and
;; improper (key . value) pairs. Normalize both (same rule as CL1's manifest).
(define (entry-ref entry key [default #f])
  (define found (assoc key entry))
  (if (not found)
      default
      (let ([rest (cdr found)])
        (cond
          [(null? rest) default]
          [(and (pair? rest) (null? (cdr rest))) (car rest)]
          [else rest]))))

(define (manifest-subsystems datum)
  (or (entry-ref datum 'subsystems) '()))

(define (manifest-owner-status datum test)
  (for*/first ([subsystem (in-list (manifest-subsystems datum))]
               [owner (in-list (or (entry-ref subsystem 'owner-tests) '()))]
               #:when (equal? (entry-ref owner 'test) test))
    (hash 'test
          test
          'status
          (or (entry-ref owner 'status) 'unknown)
          'capability
          (or (entry-ref owner 'capability) #f))))

(define (load-coverage-entries root)
  (define coverage-file (build-path root (string->path (coverage-path))))
  (unless (file-exists? coverage-file)
    '())
  (define data
    (with-handlers ([exn:fail? (lambda (_) (hash))])
      (call-with-input-file coverage-file read-json)))
  (if (hash? data)
      (hash-ref data 'entries '())
      '()))

(define (hash-merge a b)
  (for/fold ([result a]) ([(k v) (in-hash b)])
    (hash-set result k v)))

(define (coverage-tests root path)
  (define manifest (load-manifest root))
  (for/list ([entry (in-list (load-coverage-entries root))]
             #:when (member path (hash-ref entry 'covers '())))
    (define test (hash-ref entry 'test ""))
    (hash-merge (hash 'test test) (or (manifest-owner-status manifest test) (hash)))))

;; ------------------------------------------------------------ static index --

(define (definition-record datum module)
  (and (pair? datum)
       (let ([head (car datum)]
             [target (and (pair? (cdr datum)) (cadr datum))])
         (cond
           [(memq head
                  '(define define/contract
                     define+provide
                     define-syntax
                     define-syntax-rule
                     define-values))
            (define name
              (cond
                [(symbol? target) target]
                [(pair? target) (car target)]
                [else #f]))
            (and (symbol? name)
                 (hash 'name
                       (symbol->string name)
                       'module
                       module
                       'kind
                       (symbol->string head)
                       'line
                       #f))]
           [(memq head '(struct define-struct))
            (and (symbol? target)
                 (hash 'name
                       (symbol->string target)
                       'module
                       module
                       'kind
                       (symbol->string head)
                       'line
                       #f))]
           [else #f]))))

(define (top-level-forms datum)
  (if (and (pair? datum) (eq? (car datum) 'begin))
      (apply append (map top-level-forms (cdr datum)))
      (list datum)))

(define (provide-specs spec)
  ;; Explicitly exported names; forms whose exports need expansion are returned
  ;; as #f and reported as uncertainty rather than guessed.
  (cond
    [(symbol? spec) (list (symbol->string spec))]
    [(not (pair? spec)) '()]
    [else
     (case (car spec)
       [(contract-out)
        (for/list ([p (in-list (cdr spec))])
          (and (pair? p) (symbol? (car p)) (symbol->string (car p))))]
       [(rename-out)
        (for/list ([p (in-list (cdr spec))])
          (and (pair? p) (>= (length p) 2) (symbol? (cadr p)) (symbol->string (cadr p))))]
       [(struct-out)
        (if (and (pair? (cdr spec)) (symbol? (cadr spec)))
            (list (symbol->string (cadr spec)))
            '())]
       [(except-out)
        (define excluded (filter symbol? (cddr spec)))
        (filter values
                (remove* (map symbol->string excluded)
                         (provide-specs (and (pair? (cdr spec)) (cadr spec)))))]
       [(combine-out protect-out) (apply append (map provide-specs (cdr spec)))]
       [(all-defined-out all-from-out prefix-out) '(#f)]
       [else (list #f)])]))

(define (line-of-function source)
  (define lines (string-split source "\n"))
  (lambda (name)
    (define rx
      (pregexp (string-append "(^|[^A-Za-z0-9_\\-])"
                              (regexp-quote (symbol->string name))
                              "([^A-Za-z0-9_\\-]|$)")))
    (for/first ([line (in-list lines)]
                [n (in-naturals 1)]
                #:when (regexp-match? rx line))
      n)))

(define (dynamic-require-modules path source)
  (define (walk value)
    (cond
      [(and (pair? value)
            (eq? (car value) 'dynamic-require)
            (pair? (cdr value))
            (string? (cadr value))
            (string-suffix? (cadr value) ".rkt"))
       (filter values (list (normal-require-path path (cadr value))))]
      [(pair? value) (append (walk (car value)) (walk (cdr value)))]
      [else '()]))
  (sort (remove-duplicates (apply append (map walk (read-datums source))) string=?) string<?))

(define (module-record root path)
  (define source (source-text root path))
  (define forms (apply append (map top-level-forms (read-datums source))))
  (define definitions (filter values (map (lambda (d) (definition-record d path)) forms)))
  (define explicit-exports
    (for*/list ([d (in-list forms)]
                #:when (and (pair? d) (eq? (car d) 'provide))
                [spec (in-list (cdr d))]
                [name (in-list (provide-specs spec))]
                #:when name)
      (hash 'name
            name
            'module
            path
            'form
            (if (symbol? spec)
                (symbol->string spec)
                (symbol->string (car spec))))))
  (define unresolved-exports
    (for*/or ([d (in-list forms)]
              #:when (and (pair? d) (eq? (car d) 'provide))
              [spec (in-list (cdr d))]
              #:when (equal? (provide-specs spec) '(#f)))
      #t))
  (define all-defined?
    (for*/or ([d (in-list forms)]
              #:when (and (pair? d) (eq? (car d) 'provide))
              [spec (in-list (cdr d))]
              #:when (and (pair? spec) (eq? (car spec) 'all-defined-out)))
      #t))
  (define exports
    (sort (append explicit-exports
                  (if all-defined?
                      (for/list ([d (in-list definitions)])
                        (hash 'name (hash-ref d 'name) 'module path 'form "all-defined-out"))
                      '()))
          string<?
          #:key (lambda (e) (string-append (hash-ref e 'name) ":" (hash-ref e 'module)))))
  (hash 'path
        path
        'definitions
        definitions
        'exports
        exports
        'requires
        (collected-module-path-strings source)
        'dynamic-requires
        (dynamic-require-modules path source)
        'unresolved-exports
        (and unresolved-exports #t)))

(define (hash->immutable-hash h)
  (for/fold ([result (hash)]) ([(k v) (in-hash h)])
    (hash-set result k v)))

(define (build-index #:root [root #f])
  (define r (resolve-root root))
  (or (hash-ref index-cache r #f)
      (let* ([paths (tracked-rkt-paths r)]
             [_ (unless (pair? paths)
                  (raise-user-error 'semantic-navigator
                                    "no tracked Racket sources under ~a; refusing an empty index"
                                    r))]
             [records (for/hash ([p (in-list paths)])
                        (values p (module-record r p)))]
             [deps (for/hash ([p (in-list paths)])
                     (values
                      p
                      (sort (for*/list ([req (in-list (hash-ref (hash-ref records p) 'requires))]
                                        #:do [(define normalized (normal-require-path p req))]
                                        #:when (and normalized (hash-has-key? records normalized)))
                              normalized)
                            string<?)))]
             [callers (for/fold ([acc (hash)]) ([p (in-list paths)])
                        (for/fold ([acc acc]) ([dep (in-list (hash-ref deps p))])
                          (hash-update acc dep (lambda (xs) (cons p xs)) '())))])
        (define idx
          (hash 'root
                r
                'paths
                paths
                'records
                records
                'deps
                (hash->immutable-hash deps)
                'callers
                (hash->immutable-hash (for/hash ([(k v) (in-hash callers)])
                                        (values k (sort v string<?))))
                'maps
                (map-files r)))
        (hash-set! index-cache r idx)
        idx)))

;; --------------------------------------------------------- shared evidence --

(define (map-excerpts root index path)
  (for*/list ([rel (in-list (hash-ref index 'maps))]
              #:when (file-exists? (build-path root (string->path rel)))
              [line (in-list (string-split (file->string (build-path root (string->path rel))) "\n"))]
              #:when (string-contains? line path))
    (hash 'map rel 'text (string-trim line))))

(define (map-module-paths root index path)
  (define rx #px"`([^`]+\\.rkt)`")
  (remove-duplicates
   (apply append
          (for*/list ([rel (in-list (hash-ref index 'maps))]
                      #:when (file-exists? (build-path root (string->path rel)))
                      [line (in-list (string-split (file->string (build-path root (string->path rel)))
                                                   "\n"))]
                      #:when (string-contains? line path))
            (filter values (regexp-match* rx line #:match-select cadr))))
   string=?))

(define (freshness-info root index)
  (define manifest (load-manifest root))
  (define availability (or (entry-ref manifest 'input-availability) '()))
  (define unavailable
    (for/list ([entry (in-list availability)]
               #:when (string-contains? (format "~a" (cdr entry)) "unavailable"))
      (car entry)))
  (define missing-maps
    (for/list ([rel (in-list (hash-ref index 'maps))]
               #:unless (file-exists? (build-path root (string->path rel))))
      rel))
  (hash 'stale?
        (and (or (pair? unavailable)
                 (pair? missing-maps)
                 (null? (hash-ref index 'maps))
                 (not (file-exists? (build-path root (string->path (manifest-path))))))
             #t)
        'unavailable-inputs
        (sort (map symbol->string unavailable) string<?)
        'missing-maps
        missing-maps
        'revision
        (let ([head (string-trim (git-output root '("rev-parse" "HEAD")))])
          (if (regexp-match? #px"^[0-9a-f]{40}$" head) head "unavailable: no committed HEAD"))
        'fingerprints
        (for/list ([rel (in-list (sort (append (list (manifest-path) (coverage-path) (admission-path))
                                               (hash-ref index 'maps))
                                       string<?))])
          (hash 'path rel 'sha1 (sha1-fingerprint root rel)))
        'byte-regeneration-check
        "tests/test-subsystem-maps.rkt (not executed by the navigator)"))

(define (textual-reference? root path name)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (define rx
      (pregexp (string-append "(^|[^A-Za-z0-9_\\-])"
                              (regexp-quote (symbol->string name))
                              "([^A-Za-z0-9_\\-]|$)")))
    (regexp-match? rx (source-text root path))))

(define (caller-records index path)
  (for/list ([caller (in-list (hash-ref (hash-ref index 'callers) path '()))])
    (hash 'module caller 'kind 'static-require)))

(define (take-n xs n)
  (let loop ([xs xs]
             [n n]
             [acc '()])
    (cond
      [(zero? n) (reverse acc)]
      [(null? xs) (reverse acc)]
      [else (loop (cdr xs) (sub1 n) (cons (car xs) acc))])))

;; ------------------------------------------------------------------ queries --

(define (module-query path #:root [root #f])
  (define idx (build-index #:root root))
  (define r (hash-ref idx 'root))
  (define record (hash-ref (hash-ref idx 'records) path #f))
  (unless record
    (raise-user-error 'semantic-navigator "no tracked module: ~a" path))
  (define owners (coverage-tests r path))
  (define excerpts (map-excerpts r idx path))
  (define dynamic (hash-ref record 'dynamic-requires))
  (define freshness (freshness-info r idx))
  (define uncertainty
    (append (if (pair? dynamic)
                '(dynamic-require)
                '())
            (if (null? owners)
                '(no-owner-tests)
                '())
            (if (null? excerpts)
                '(no-curated-map)
                '())
            (if (hash-ref record 'unresolved-exports)
                '(unresolved-provide-form)
                '())
            (if (hash-ref freshness 'stale?)
                '(stale-map-metadata)
                '())))
  (hash 'module
        path
        'subsystem
        (car (string-split path "/"))
        'definitions
        (hash-ref record 'definitions)
        'exports
        (hash-ref record 'exports)
        'requires
        (hash-ref record 'requires)
        'callers
        (caller-records idx path)
        'dynamic-requires
        (for/list ([d (in-list dynamic)])
          (hash 'module d 'status 'unresolved))
        'owner-tests
        owners
        'map-excerpts
        excerpts
        'freshness
        freshness
        'uncertainty
        (remove-duplicates uncertainty)
        'verification-note
        verification-note))

(define (symbol-query name #:root [root #f] #:limit [limit 20])
  (define idx (build-index #:root root))
  (define r (hash-ref idx 'root))
  (define definitions*
    (for*/list ([p (in-list (hash-ref idx 'paths))]
                [d (in-list (hash-ref (hash-ref (hash-ref idx 'records) p) 'definitions))]
                #:when (equal? (hash-ref d 'name) (symbol->string name)))
      d))
  (define exports
    (for*/list ([p (in-list (hash-ref idx 'paths))]
                [e (in-list (hash-ref (hash-ref (hash-ref idx 'records) p) 'exports))]
                #:when (equal? (hash-ref e 'name) (symbol->string name)))
      e))
  (define (attach-line d)
    (define source (source-text r (hash-ref d 'module)))
    (hash-set d 'line ((line-of-function source) (string->symbol (hash-ref d 'name)))))
  (define definitions (map attach-line definitions*))
  (define defining-modules
    (sort (remove-duplicates (map (lambda (d) (hash-ref d 'module)) (append definitions exports)))
          string<?))
  (define textual-callers
    (take-n
     (sort (for*/list ([m (in-list defining-modules)]
                       [c (in-list (caller-records idx m))]
                       #:when (textual-reference? r (hash-ref c 'module) name))
             (hash 'module (hash-ref c 'module) 'imports m 'kind 'textual-reference-not-call-graph))
           string<?
           #:key (lambda (c) (hash-ref c 'module)))
     limit))
  (define excerpts
    (apply append
           (for/list ([m (in-list defining-modules)])
             (map-excerpts r idx m))))
  (define uncertainty
    (append (if (> (length defining-modules) 1)
                '(ambiguous-symbol)
                '())
            (if (null? defining-modules)
                '(no-static-definition)
                '())
            (if (>= (length textual-callers) limit)
                '(caller-list-truncated)
                '())
            (if (for/or ([m (in-list defining-modules)])
                  (pair? (hash-ref (hash-ref (hash-ref idx 'records) m) 'dynamic-requires)))
                '(dynamic-require)
                '())))
  (hash 'symbol
        (symbol->string name)
        'definitions
        definitions
        'exports
        exports
        'probable-importers
        textual-callers
        'map-excerpts
        excerpts
        'freshness
        (freshness-info r idx)
        'uncertainty
        (remove-duplicates uncertainty)
        'verification-note
        verification-note))

;; ----------------------------------------------------------------- context --

(define (focused-context changed #:root [root #f] #:revision [revision #f])
  (define idx (build-index #:root root))
  (define r (hash-ref idx 'root))
  (define changes (sort (remove-duplicates changed) string<?))
  (define report (changed-selection r (admitted-owners) changes))
  (define included (change-report-included report))
  (define entries (load-coverage-entries r))
  (define manifest (load-manifest r))
  (define owner-tests
    (for/list ([test (in-list included)])
      (hash-merge (hash 'test test) (or (manifest-owner-status manifest test) (hash)))))
  (define covered-sources
    (take-n (sort (remove-duplicates (for*/list ([test (in-list included)]
                                                 [entry (in-list entries)]
                                                 [cover (in-list (hash-ref entry 'covers '()))]
                                                 #:when (equal? (hash-ref entry 'test #f) test))
                                       cover))
                  string<?)
            40))
  (define map-paths
    (take-n (sort (remove-duplicates (append (apply append
                                                    (for/list ([p (in-list changes)])
                                                      (map-module-paths r idx p)))
                                             (apply append
                                                    (for/list ([p (in-list included)])
                                                      (map-module-paths r idx p)))))
                  string<?)
            30))
  (define subsystems
    (sort (remove-duplicates (append (map (lambda (p) (car (string-split p "/"))) changes)
                                     (for*/list ([sub (in-list (manifest-subsystems manifest))]
                                                 [owner (in-list (or (entry-ref sub 'owner-tests)
                                                                     '()))]
                                                 #:when (member (entry-ref owner 'test) included))
                                       (car (string-split (or (entry-ref owner 'test) "") "/")))))
          string<?))
  (define uncertainty
    (append (for/list ([e (in-list (change-report-escalations report))])
              (string-append "escalation: " e))
            (if (pair? (change-report-unattributable report))
                (list (format "~a owners had no @covers attribution (included fail open)"
                              (length (change-report-unattributable report))))
                '())))
  (hash 'changed
        changes
        'selected-owners
        included
        'excluded-owners
        (change-report-excluded report)
        'owner-tests
        owner-tests
        'covered-sources
        covered-sources
        'map-paths
        map-paths
        'subsystems
        subsystems
        'provenance
        (for/list ([rel (in-list (sort (append (list (manifest-path) (coverage-path) (admission-path))
                                               (hash-ref idx 'maps))
                                       string<?))])
          (hash 'path rel 'sha1 (sha1-fingerprint r rel)))
        'freshness
        (freshness-info r idx)
        'revision
        (or revision
            (let ([head (string-trim (git-output r '("rev-parse" "HEAD")))])
              (if (regexp-match? #px"^[0-9a-f]{40}$" head)
                  head
                  "unavailable: no committed HEAD")))
        'uncertainty
        uncertainty
        'verification-note
        context-note))

;; --------------------------------------------------------------- benchmark --

(define (bundle-paths result)
  (sort
   (remove-duplicates (append (for/list ([entry (in-list (hash-ref result 'definitions '()))])
                                (hash-ref entry 'module))
                              (for/list ([entry (in-list (hash-ref result 'exports '()))])
                                (hash-ref entry 'module))
                              (for/list ([entry (in-list (hash-ref result 'callers '()))])
                                (hash-ref entry 'module))
                              (for/list ([entry (in-list (hash-ref result 'dynamic-requires '()))])
                                (hash-ref entry 'module))
                              (for/list ([entry (in-list (hash-ref result 'owner-tests '()))])
                                (hash-ref entry 'test))
                              (for/list ([entry (in-list (hash-ref result 'map-excerpts '()))])
                                (hash-ref entry 'map))
                              (for/list ([entry (in-list (hash-ref result 'map-module-paths '()))])
                                entry)
                              (if (hash-has-key? result 'map-paths)
                                  (hash-ref result 'map-paths)
                                  '())))
   string<?))

(define (task-result task expected bundle)
  (define missing (filter (lambda (p) (not (member p bundle))) expected))
  (hash 'task
        task
        'expected
        expected
        'missing
        missing
        'contained
        (null? missing)
        'bundle-size
        (length bundle)
        'evidence
        "mechanical containment of frozen CL0 facts only"))

(define (benchmark-report #:root [root #f])
  (define idx (build-index #:root root))
  (define r (hash-ref idx 'root))
  (define (enrich path)
    (define m (module-query path #:root r))
    (define subsystem (car (string-split path "/")))
    ;; CL1 maps group capabilities under top-level subsystems: collect every
    ;; module path named on map lines for this subsystem, not only the matched
    ;; line, so durable-state writers are reachable from the decision owner.
    (define extras (take-n (sort (map-module-paths r idx subsystem) string<?) 80))
    (hash-set m 'map-module-paths extras))
  (define t1 (enrich "extensions/gsd/campaign-decision.rkt"))
  (define t3 (enrich "agent/state.rkt"))
  (define t4a (enrich "llm/provider-errors.rkt"))
  (define t4b (enrich "runtime/provider-retry.rkt"))
  (hash 'corpus
        "CL0-AGENT-TASK-BENCHMARK-003e425d.md"
        'tasks
        (list (task-result
               "T1"
               '("extensions/gsd/campaign-decision.rkt" "extensions/gsd/go-orchestrator.rkt"
                                                        "extensions/gsd/campaign-state.rkt"
                                                        "extensions/gsd/campaign-repository.rkt")
               (bundle-paths t1))
              (task-result "T3"
                           '("agent/state.rkt" "agent/effect-executor.rkt"
                                               "agent/event-emitter.rkt"
                                               "agent/iteration/main-loop.rkt"
                                               "agent/loop-dispatch.rkt"
                                               "agent/loop-phases.rkt"
                                               "agent/loop-stream.rkt"
                                               "agent/loop.rkt"
                                               "agent/stream-runner.rkt"
                                               "extensions/gsd/author-execution.rkt"
                                               "extensions/gsd/command-handlers.rkt"
                                               "extensions/gsd/go-orchestrator.rkt"
                                               "extensions/gsd/stall-policy.rkt"
                                               "runtime/iteration/step-executor.rkt")
                           (bundle-paths t3))
              (task-result "T4"
                           '("llm/provider-errors.rkt" "runtime/provider-retry.rkt"
                                                       "tests/test-retry-error-classification.rkt")
                           (append (bundle-paths t4a) (bundle-paths t4b))))
        'agent-trial-measurement
        #f
        'limitation
        (string-append
         "PENDING: token counts, file reads, wall-clock time, owner-location time and "
         "first-edit correctness require fresh-context agent trials under the frozen CL0 "
         "protocol; containment is not a trial measurement, not a correctness oracle and "
         "not evidence of safe isolated verification.")))

;; ---------------------------------------------------------------------- CLI --

(define (canonical-write datum out)
  (cond
    [(hash? datum)
     (display "(hash" out)
     (for ([key (in-list (sort (hash-keys datum)
                               (lambda (a b) (string<? (format "~a" a) (format "~a" b)))))])
       (display " '" out)
       (display key out)
       (display " " out)
       (canonical-write (hash-ref datum key) out))
     (display ")" out)]
    [(pair? datum)
     (display "(list" out)
     (for ([item (in-list datum)])
       (display " " out)
       (canonical-write item out))
     (display ")" out)]
    [(vector? datum) (canonical-write (vector->list datum) out)]
    [else (write datum out)]))

(define (render datum machine?)
  (if machine?
      (canonical-write datum (current-output-port))
      (for ([(key value) (in-hash datum)])
        (printf "~a: " key)
        (cond
          [(and (list? value)
                (for/and ([v (in-list value)])
                  (hash? v)))
           (printf "~a entries\n" (length value))
           (for ([v (in-list value)])
             (printf
              "  - ~a\n"
              (or (hash-ref v 'module #f) (hash-ref v 'test #f) (hash-ref v 'map #f)
                  (hash-ref v 'task #f) (hash-ref v 'path #f) (hash-ref v 'symbol #f)
                  "item")))]
          [(list? value) (printf "~a\n" (string-join (map ~a value) ", "))]
          [(hash? value) (printf "~a keys\n" (hash-count value))]
          [else (printf "~a\n" value)])))
  (newline))

(define (parse-range root text)
  (define parts (string-split text ".."))
  (if (= (length parts) 2)
      (let-values ([(changed label outside) (commits-changed (first parts) (second parts) root)])
        (values changed label outside))
      (let-values ([(changed label outside) (working-tree-changed text root)])
        (values changed label outside))))

(define (usage)
  (displayln "Usage: racket scripts/semantic-navigator.rkt COMMAND [OPTIONS]")
  (displayln
   "  symbol NAME [--machine]             static definitions/exports, importers, uncertainty")
  (displayln
   "  module PATH [--machine]             ownership, contracts, callers, dynamic edges, tests")
  (displayln "  context PATH ... | --changed RANGE  focused qt-selection bundle (reproduction only)")
  (displayln "  benchmark [--machine]               CL0 frozen-fact containment (NOT an agent trial)")
  (displayln "  help                                This message")
  (exit 0))

(module+ main
  (define args (vector->list (current-command-line-arguments)))
  (define machine? (and (member "--machine" args) #t))
  (define positional (filter (lambda (a) (not (string-prefix? a "--"))) args))
  (define command
    (if (null? positional)
        "help"
        (car positional)))
  (case (string->symbol command)
    [(help) (usage)]
    [(symbol)
     (unless (= (length positional) 2)
       (raise-user-error 'semantic-navigator "symbol expects exactly one NAME"))
     (render (symbol-query (string->symbol (second positional))) machine?)]
    [(module)
     (unless (= (length positional) 2)
       (raise-user-error 'semantic-navigator "module expects exactly one PATH"))
     (render (module-query (second positional)) machine?)]
    [(context)
     (define changed
       (if (member "--changed" args)
           (let-values ([(changed label outside) (parse-range (resolve-root #f)
                                                              (second (member "--changed" args)))])
             (void label outside)
             changed)
           (cdr positional)))
     (when (null? changed)
       (raise-user-error 'semantic-navigator "context needs at least one path or --changed RANGE"))
     (render (focused-context changed) machine?)]
    [(benchmark) (render (benchmark-report) machine?)]
    [else (raise-user-error 'semantic-navigator "unknown command: ~a" command)]))
