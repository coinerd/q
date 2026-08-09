#lang racket/base

;; Pure rendering and shell-free Git collection for the architecture baseline.

(require racket/file
         racket/list
         racket/path
         racket/port
         racket/string
         racket/system)

(provide finding-statuses
         valid-finding-status?
         validate-finding-statuses
         canonical-datum->string
         architecture-snapshot->markdown
         parse-git-log
         count-co-changes
         build-architecture-snapshot
         collect-architecture-snapshot
         (struct-out git-change)
         (struct-out git-commit)
         default-part-i-findings
         dependency-policy-exceptions
         dependency-policy-composition-roots
         dependency-policy-provide-risks)

(define finding-statuses '("OPEN" "PARTIALLY_RESOLVED" "RESOLVED" "STALE_INCORRECT"))

;; Stable Part-I reconciliation used by generated W0 evidence.
(define default-part-i-findings
  '(((id "F01_GSD_DOMAIN_EFFECT_SEPARATION")
     (finding "GSD domain logic and external effects remain difficult to separate")
     (status "OPEN")
     (evidence ("extensions/gsd/" "docs/architecture/dependency-policy.rktd")))
    ((id "F02_EXTENSION_CONTEXT_SESSION_TYPE")
     (finding "Extension Context imports Runtime session types")
     (status "STALE_INCORRECT")
     (evidence ("extensions/context.rkt" "util/extension/extension-types.rkt")))
    ((id "F03_EXTENSION_RUNTIME_SERVICE_COUPLING")
     (finding "Extension Context still imports the concrete Provider Registry service")
     (status "PARTIALLY_RESOLVED")
     (evidence ("extensions/context.rkt" "runtime/provider/provider-registry.rkt")))
    ((id "F04_EXTENSION_BOUNDARY_EXCEPTIONS")
     (finding "Extension Runtime and TUI boundary exceptions remain fragile")
     (status "OPEN")
     (evidence ("docs/architecture/dependency-policy.rktd")))
    ((id "F05_PROVIDER_CO_CHANGE_DUPLICATION")
     (finding
      "Provider transport has shared SSE primitives but adapter-specific normalization; no second evidence justifies a further shared base")
     (status "PARTIALLY_RESOLVED")
     (evidence ("llm/stream.rkt" "llm/openai-compatible.rkt"
                                 "llm/gemini.rkt"
                                 "docs/architecture/dependency-policy.rktd")))
    ((id "F06_PROVIDER_PROTOCOL_LEAKAGE")
     (finding "Provider-specific stream parsing leaks into Agent Core")
     (status "RESOLVED")
     (evidence ("llm/stream.rkt" "tests/test-provider-transport-architecture.rkt")))
    ((id "F07_AGENT_ITERATION_RUNTIME_BOUNDARY")
     (finding "Agent Iteration imports Runtime implementation modules")
     (status "RESOLVED")
     (evidence ("docs/architecture/dependency-policy.rktd" "tests/test-arch-fitness.rkt")))
    ((id "F08_SESSION_LIFECYCLE_RESPONSIBILITIES")
     (finding "Session Lifecycle concentrates orchestration and preparation responsibilities")
     (status "OPEN")
     (evidence ("runtime/session/session-lifecycle.rkt")))
    ((id "F09_SETTINGS_QUERY_SURFACE")
     (finding
      "Settings Query has a high managed surface; policy records 226 provides but source exports count 53 — semantics differ, decide before v0.99.92")
     (status "OPEN")
     (evidence ("runtime/settings-query.rkt" "docs/architecture/dependency-policy.rktd"
                                             "tests/test-architecture-baseline.rkt")))
    ((id "F10_HIDDEN_PARAMETER_STATE")
     (finding "Parameters contain unclassified hidden cross-turn session state")
     (status "RESOLVED")
     (evidence ("docs/architecture/parameter-inventory.rktd" "tests/test-arch-parameters.rkt")))
    ((id "F11_CONTEXT_ASSEMBLY_COMPLEXITY")
     (finding "Context Assembly hidden state is resolved but cognitive complexity remains")
     (status "PARTIALLY_RESOLVED")
     (evidence ("runtime/context-assembly/" "tests/test-rollback-session-ownership.rkt")))
    ((id "F12_DOCUMENTATION_ONLY_ENFORCEMENT")
     (finding "Architecture boundaries rely only on documentation")
     (status "RESOLVED")
     (evidence ("tests/test-arch-fitness.rkt" "docs/architecture/dependency-policy.rktd")))))

(struct git-change (status paths) #:transparent)
(struct git-commit (revision subject changes) #:transparent)

(define absent (gensym 'absent))

(define (datum-key->string value)
  (with-output-to-string (lambda () (write value))))

;; Hash iteration order is deliberately erased. Lists retain their semantic order.
(define (canonicalize value)
  (cond
    [(hash? value)
     (list 'hash
           (for/list ([key (in-list (sort (hash-keys value) string<? #:key datum-key->string))])
             (list (canonicalize key) (canonicalize (hash-ref value key)))))]
    [(pair? value) (cons (canonicalize (car value)) (canonicalize (cdr value)))]
    [(vector? value) (list->vector (map canonicalize (vector->list value)))]
    [else value]))

(define (canonical-datum->string datum)
  (string-append (with-output-to-string (lambda () (write (canonicalize datum)))) "\n"))

(define (entry-ref entry key [default absent])
  (define answer
    (cond
      [(hash? entry) (hash-ref entry key absent)]
      [(list? entry)
       (define found
         (for/first ([item (in-list entry)]
                     #:when (and (list? item) (= (length item) 2) (equal? (first item) key)))
           (second item)))
       (if found found absent)]
      [else absent]))
  (cond
    [(not (eq? answer absent)) answer]
    [(not (eq? default absent)) default]
    [else (error 'entry-ref "missing field ~s in ~s" key entry)]))

(define (dependency-policy-exceptions policy)
  (define known-section (assoc 'known-exceptions policy))
  (define entries
    (if known-section
        (for*/list ([layer-entry (in-list (cdr known-section))]
                    [exception (in-list (cdr layer-entry))])
          (define attributes (cdr exception))
          (define revisit (assoc 'revisit-by attributes))
          (define permanent (assoc 'permanent-waiver attributes))
          `((layer ,(symbol->string (car layer-entry)))
            (file ,(symbol->string (car exception)))
            (rationale ,(cdr (assoc 'rationale attributes)))
            (owner ,(cdr (assoc 'owner attributes)))
            (lifecycle ,(if revisit "DATED" "PERMANENT"))
            (revisit-by ,(and revisit (cdr revisit)))
            (permanent-waiver ,(and permanent (cdr permanent)))))
        '()))
  (sort entries
        string<?
        #:key (lambda (entry) (string-append (entry-ref entry 'layer) "/" (entry-ref entry 'file)))))

(define (dependency-policy-composition-roots policy)
  (define section (assoc 'composition-roots policy))
  (for/list ([root (in-list (if section
                                (cdr section)
                                '()))]
             #:when (pair? root))
    `((path ,(if (symbol? (car root))
                 (symbol->string (car root))
                 (car root)))
      (recorded-fan-out ,(cdr (assoc 'fan-out (cdr root)))))))

(define (dependency-policy-provide-risks policy)
  (define section (assoc 'hotspot-budget policy))
  (define notes (and section (assoc 'risk-notes (cdr section))))
  (for/list ([note (in-list (if notes
                                (cdr notes)
                                '()))]
             #:when (pair? note))
    (define text (cdr (assoc 'risk (cdr note))))
    (define provides (regexp-match #px"([0-9]+) provides" (or text "")))
    `((path ,(if (symbol? (car note))
                 (symbol->string (car note))
                 (car note)))
      (recorded-provides ,(if provides
                              (string->number (second provides))
                              0)))))

(define (valid-finding-status? status)
  (and (string? status) (if (member status finding-statuses) #t #f)))

(define (validate-finding-statuses findings)
  (for ([finding (in-list findings)])
    (define status (entry-ref finding 'status))
    (unless (valid-finding-status? status)
      (raise-arguments-error 'validate-finding-statuses
                             "invalid Part-I finding status"
                             "status"
                             status
                             "allowed"
                             finding-statuses)))
  findings)

(define (run-git root . arguments)
  (define git (find-executable-path "git"))
  (define stdout (open-output-string))
  (define stderr (open-output-string))
  (define ok?
    (begin
      (unless git
        (error 'architecture-baseline "git executable not found"))
      (parameterize ([current-directory root]
                     [current-output-port stdout]
                     [current-error-port stderr])
        (apply system* git arguments))))
  (unless ok?
    (error 'architecture-baseline
           "git failed (~a): ~a"
           (string-join arguments " ")
           (string-trim (get-output-string stderr))))
  (get-output-string stdout))

(define (resolve-revision root revision)
  (string-trim (run-git root "rev-parse" "--verify" (string-append revision "^{commit}"))))

(define (tracked-rkt-paths root revision)
  (sort (filter (lambda (path) (string-suffix? path ".rkt"))
                (filter non-empty-string?
                        (string-split (run-git root "ls-tree" "-r" "--name-only" revision "--")
                                      "\n")))
        string<?))

(define (revision-file-content root revision path)
  ;; REV:path must be one argv element; no command is interpreted by a shell.
  (run-git root "show" (string-append revision ":" path)))

;; Read every blob through one long-lived Git process. Spawning `git show` once
;; per module makes a full repository baseline prohibitively slow.
(define (revision-file-sources root revision paths)
  (define git
    (or (find-executable-path "git") (error 'architecture-baseline "git executable not found")))
  (define-values (process output input error-output)
    (subprocess #f #f #f git "-C" (path->string root) "cat-file" "--batch"))
  ;; Git may block while writing blob output, so feed requests concurrently
  ;; instead of filling both sides of the subprocess pipe serially.
  (define writer
    (thread (lambda ()
              (for ([path (in-list paths)])
                (fprintf input "~a:~a\n" revision path))
              (close-output-port input))))
  (define sources
    (for/list ([path (in-list paths)])
      (define header (read-line output 'any))
      (define match (and (string? header) (regexp-match #px"^[0-9a-f]+ blob ([0-9]+)$" header)))
      (unless match
        (error 'architecture-baseline "unexpected git cat-file header for ~a: ~s" path header))
      (define size (string->number (second match)))
      (define content (read-bytes size output))
      (unless (and (bytes? content) (= (bytes-length content) size))
        (error 'architecture-baseline "short git blob read for ~a" path))
      (read-byte output)
      (cons path (bytes->string/utf-8 content))))
  (thread-wait writer)
  (define error-text (port->string error-output))
  (close-input-port output)
  (close-input-port error-output)
  (subprocess-wait process)
  (unless (zero? (subprocess-status process))
    (error 'architecture-baseline "git cat-file failed: ~a" (string-trim error-text)))
  sources)

(define (source-loc source)
  (cond
    [(zero? (string-length source)) 0]
    [else (+ (length (regexp-match* #rx"\n" source)) (if (string-suffix? source "\n") 0 1))]))

(define (source-datums source)
  (define without-lang (regexp-replace #px"^#lang[^\n]*(?:\n|$)" source ""))
  (with-handlers ([exn:fail:read? (lambda (_error) '())])
    (call-with-input-string without-lang
                            (lambda (input)
                              (let loop ([result '()])
                                (define value (read input))
                                (if (eof-object? value)
                                    (reverse result)
                                    (loop (cons value result))))))))

(define (provide-spec-count source)
  (define (spec-count spec)
    (cond
      [(symbol? spec) 1]
      [(not (pair? spec)) 0]
      [else
       (case (car spec)
         [(contract-out rename-out) (length (cdr spec))]
         [(combine-out protect-out) (for/sum ([nested (in-list (cdr spec))]) (spec-count nested))]
         [(prefix-out)
          (if (>= (length spec) 3)
              (spec-count (third spec))
              1)]
         ;; These forms export a computed set that cannot be enumerated from
         ;; syntax alone; count the public export form rather than its tokens.
         [(all-defined-out all-from-out struct-out except-out) 1]
         [else 1])]))
  (for/sum ([datum (in-list (source-datums source))] #:when (and (pair? datum)
                                                                 (eq? (car datum) 'provide)))
           (for/sum ([spec (in-list (cdr datum))]) (spec-count spec))))

(define (require-path-strings source)
  (define result '())
  (define (collect-module-paths value)
    (cond
      [(and (string? value) (string-suffix? value ".rkt")) (set! result (cons value result))]
      [(pair? value)
       (collect-module-paths (car value))
       (collect-module-paths (cdr value))]
      [(vector? value)
       (for ([item (in-vector value)])
         (collect-module-paths item))]
      [else (void)]))
  (define (walk value)
    (cond
      [(and (pair? value)
            (memq (car value)
                  '(require require/typed
                            require/typed/contract)))
       (collect-module-paths (cdr value))]
      [(pair? value)
       (walk (car value))
       (walk (cdr value))]
      [(vector? value)
       (for ([item (in-vector value)])
         (walk item))]
      [else (void)]))
  (for ([datum (in-list (source-datums source))])
    (walk datum))
  (remove-duplicates result string=?))

(define (normal-require-path requiring-path required-path)
  (and (relative-path? (string->path required-path))
       (path->string (simplify-path (build-path (or (path-only (string->path requiring-path))
                                                    (string->path "."))
                                                required-path)
                                    #f))))

(define (source-dependencies path source tracked-paths)
  (sort (remove-duplicates (for/list ([required (in-list (require-path-strings source))]
                                      #:do [(define normalized (normal-require-path path required))]
                                      #:when (and normalized
                                                  (member normalized tracked-paths string=?)))
                             normalized)
                           string=?)
        string<?))

(define (test-path? path)
  (or (string-prefix? path "tests/") (string-contains? path "/tests/")))

(define (count-pattern pattern source)
  (length (regexp-match* pattern source)))

(define (count-lines-matching pattern source)
  (for/sum ([line (in-list (string-split source "\n"))]) (if (regexp-match? pattern line) 1 0)))

(define (release-only-path? path)
  (or
   (regexp-match? #px"^(?:CHANGELOG(?:-ARCHIVE)?\\.md|VERSION|info\\.rkt|util/version\\.rkt)$" path)
   (regexp-match? #px"^(?:pkg/|\\.gate-evidence/|\\.release/)" path)
   (regexp-match? #px"^\\.github/workflows/release" path)
   (regexp-match?
    #px"^scripts/(?:gen-release-(?:manifest|notes)|pre-release-check|release-|sync-version|version-(?:guard|surface)|verify-release-bundle)"
    path)
   (regexp-match? #px"^(?:docs/)?releases?/" path)))

(define release-subject-rx
  #px"(?i:^(?:release(?:[: ])|prepare release|bump version|version bump|chore\\(release\\)))")

(define (parse-status-line line)
  (define pieces (string-split line "\t" #:trim? #f))
  (and (pair? pieces)
       (regexp-match? #px"^[A-Z][0-9]*$" (car pieces))
       (>= (length pieces) 2)
       (git-change (car pieces) (cdr pieces))))

;; Input is produced by: git log --format=@@@commit%x09%H%x09%s --name-status ...
(define (parse-git-log text)
  (define commits '())
  (define revision #f)
  (define subject "")
  (define changes '())
  (define (finish!)
    (when revision
      (set! commits (cons (git-commit revision subject (reverse changes)) commits))))
  (for ([line (in-list (string-split text "\n" #:trim? #f))])
    (cond
      [(string-prefix? line "@@@commit\t")
       (finish!)
       (let ([pieces (string-split line "\t" #:trim? #f)])
         (set! revision
               (if (> (length pieces) 1)
                   (second pieces)
                   ""))
         (set! subject
               (if (> (length pieces) 2)
                   (string-join (drop pieces 2) "\t")
                   ""))
         (set! changes '()))]
      [revision
       (define change (parse-status-line line))
       (when change
         (set! changes (cons change changes)))]
      [else (void)]))
  (finish!)
  (reverse commits))

(define (exact-move-or-copy? change)
  (member (git-change-status change) '("R100" "C100")))

(define (commit-all-paths commit)
  (remove-duplicates (append* (map git-change-paths (git-commit-changes commit))) string=?))

(define (release-only-commit? commit)
  (define paths (commit-all-paths commit))
  (or (regexp-match? release-subject-rx (git-commit-subject commit))
      (and (pair? paths) (andmap release-only-path? paths))))

(define (commit-countable-paths commit allowed-paths)
  (sort (remove-duplicates (for*/list ([change (in-list (git-commit-changes commit))]
                                       #:unless (exact-move-or-copy? change)
                                       [path (in-list (git-change-paths change))]
                                       #:when (and (string-suffix? path ".rkt")
                                                   (not (release-only-path? path))
                                                   (or (not allowed-paths)
                                                       (member path allowed-paths string=?))))
                             path)
                           string=?)
        string<?))

(define (pair<? left right)
  (or (string<? (first left) (first right))
      (and (string=? (first left) (first right)) (string<? (second left) (second right)))))

(define (count-co-changes commits #:paths [allowed-paths #f])
  (define counts (make-hash))
  (for ([commit (in-list commits)]
        #:unless (release-only-commit? commit))
    (define paths (commit-countable-paths commit allowed-paths))
    (for* ([index (in-range (length paths))]
           [other-index (in-range (add1 index) (length paths))])
      (define pair (list (list-ref paths index) (list-ref paths other-index)))
      (hash-update! counts pair add1 0)))
  (sort (for/list ([(pair count) (in-hash counts)])
          `((path-a ,(first pair)) (path-b ,(second pair)) (count ,count)))
        (lambda (left right)
          (define left-count (entry-ref left 'count))
          (define right-count (entry-ref right 'count))
          (if (= left-count right-count)
              (pair<? (list (entry-ref left 'path-a) (entry-ref left 'path-b))
                      (list (entry-ref right 'path-a) (entry-ref right 'path-b)))
              (> left-count right-count)))))

(define (change-frequencies commits paths)
  (define frequencies (make-hash))
  (for ([commit (in-list commits)]
        #:unless (release-only-commit? commit))
    (for ([path (in-list (commit-countable-paths commit paths))])
      (hash-update! frequencies path add1 0)))
  frequencies)

(define readme-metric-keys
  '(("Source modules" . source-modules) ("Test files" . test-files)
                                        ("Source lines" . source-lines)
                                        ("Test lines" . test-lines)
                                        ("Test assertions" . checks)))

(define (readme-published-metrics text)
  (for/list ([line (in-list (string-split text "\n"))]
             #:do
             [(define match
                (regexp-match
                 #px"^\\| (Source modules|Test files|Source lines|Test lines|Test assertions) \\| ([0-9]+) \\|"
                 line))]
             #:when match)
    (list (cdr (assoc (second match) readme-metric-keys)) (string->number (third match)))))

(define (build-architecture-snapshot revision
                                     sources
                                     commits
                                     #:last [last-n 200]
                                     #:findings [findings '()]
                                     #:policy-exceptions [policy-exceptions '()]
                                     #:policy-composition-roots [policy-composition-roots '()]
                                     #:policy-provide-risks [policy-provide-risks '()]
                                     #:published-metrics [published-metrics '()])
  (define validated-findings (validate-finding-statuses findings))
  (define sorted-sources (sort sources string<? #:key car))
  (define paths (map car sorted-sources))
  (define dependencies
    (for/hash ([source (in-list sorted-sources)])
      (values (car source) (source-dependencies (car source) (cdr source) paths))))
  (define frequencies (change-frequencies commits paths))
  (define modules
    (for/list ([source (in-list sorted-sources)])
      (define path (car source))
      (define content (cdr source))
      (define loc (source-loc content))
      (define outgoing (hash-ref dependencies path))
      (define incoming
        (sort (for/list ([candidate (in-list paths)]
                         #:when (member path (hash-ref dependencies candidate) string=?))
                candidate)
              string<?))
      (define frequency (hash-ref frequencies path 0))
      `((path ,path) (loc ,loc)
                     (provide-specs ,(provide-spec-count content))
                     (dependency-fan-in ,(length incoming))
                     (dependency-fan-out ,(length outgoing))
                     (dependencies ,outgoing)
                     (changed-commits ,frequency)
                     (hotspot-score ,(* loc frequency)))))
  (define exceptions
    (sort policy-exceptions
          string<?
          #:key (lambda (item) (string-append (entry-ref item 'layer) "/" (entry-ref item 'file)))))
  (define tests
    (for/list ([source (in-list sorted-sources)]
               #:when (test-path? (car source)))
      `((path ,(car source)) (loc ,(source-loc (cdr source)))
                             (test-cases ,(count-pattern #px"\\(test-case(?:\\s|\\[)" (cdr source)))
                             (checks ,(count-lines-matching #rx"\\(check-" (cdr source))))))
  (define all-co-change (count-co-changes commits #:paths paths))
  (define co-change-threshold 3)
  (define co-change-candidates
    (filter (lambda (pair) (>= (entry-ref pair 'count) co-change-threshold)) all-co-change))
  (define co-change (take co-change-candidates (min 100 (length co-change-candidates))))
  (define sorted-findings
    (sort validated-findings string<? #:key (lambda (item) (entry-ref item 'id))))
  (define measured-metrics
    `((source-modules ,(- (length modules) (length tests)))
      (test-files ,(length tests))
      (source-lines ,(for/sum ([module (in-list modules)] #:unless
                                                          (test-path? (entry-ref module 'path)))
                              (entry-ref module 'loc)))
      (test-lines ,(for/sum ([test (in-list tests)]) (entry-ref test 'loc)))
      (checks ,(for/sum ([test (in-list tests)]) (entry-ref test 'checks)))))
  (define reconciliation
    (for/list ([published (in-list published-metrics)])
      (define key (first published))
      (define measured (entry-ref measured-metrics key))
      `((metric ,key) (published ,(second published))
                      (measured ,measured)
                      (status ,(if (= measured (second published)) "MATCH" "DRIFT")))))
  (define module-fan-out-by-path
    (for/hash ([module (in-list modules)])
      (values (entry-ref module 'path) (entry-ref module 'dependency-fan-out))))
  (define fan-out-reconciliation
    (for/list ([root (in-list policy-composition-roots)])
      (define path (entry-ref root 'path))
      (define recorded (entry-ref root 'recorded-fan-out))
      (define measured (hash-ref module-fan-out-by-path path 'MISSING))
      `((path ,path) (recorded-fan-out ,recorded)
                     (measured-fan-out ,measured)
                     (status ,(if (equal? recorded measured) "MATCH" "DRIFT")))))
  (define provide-risks
    (for/list ([risk (in-list policy-provide-risks)])
      (define path (entry-ref risk 'path))
      (define module
        (for/first ([module (in-list modules)]
                    #:when (equal? (entry-ref module 'path) path))
          module))
      `((path ,path) (recorded-provides ,(entry-ref risk 'recorded-provides))
                     (measured-provides ,(if module
                                             (entry-ref module 'provide-specs)
                                             0))
                     (status ,(if (and module
                                       (= (entry-ref risk 'recorded-provides)
                                          (entry-ref module 'provide-specs)))
                                  "MATCH"
                                  "DRIFT")))))
  `((schema-version 1)
    (generator
     ((name "scripts/architecture-baseline.rkt")
      (co-change-exclusion-policy-version 1)
      (reproduction-command
       ,(format
         "racket scripts/architecture-baseline.rkt --revision ~a --last ~a --raw OUT.rktd --markdown OUT.md"
         revision
         last-n))))
    (revision ,revision)
    (history-limit ,last-n)
    (history-summary ((raw-non-merge-commits ,(length commits))
                      (excluded-release-commits ,(for/sum ([commit (in-list commits)])
                                                          (if (release-only-commit? commit) 1 0)))
                      (exact-move-copy-records
                       ,(for*/sum ([commit (in-list commits)] [change
                                                               (in-list (git-commit-changes commit))])
                                  (if (exact-move-or-copy? change) 1 0)))))
    (summary
     ((tracked-rkt-files ,(length modules))
      (source-modules ,(- (length modules) (length tests)))
      (test-files ,(length tests))
      (source-lines ,(for/sum ([module (in-list modules)] #:unless
                                                          (test-path? (entry-ref module 'path)))
                              (entry-ref module 'loc)))
      (test-lines ,(for/sum ([test (in-list tests)]) (entry-ref test 'loc)))
      (provide-specs ,(for/sum ([module (in-list modules)]) (entry-ref module 'provide-specs)))
      (dependency-edges ,(for/sum ([module (in-list modules)])
                                  (entry-ref module 'dependency-fan-out)))
      (test-cases ,(for/sum ([test (in-list tests)]) (entry-ref test 'test-cases)))
      (checks ,(for/sum ([test (in-list tests)]) (entry-ref test 'checks)))
      (policy-exceptions ,(length exceptions))
      (co-change-threshold ,co-change-threshold)
      (co-change-pairs-at-threshold ,(length co-change-candidates))
      (co-change-pairs-reported ,(length co-change))))
    (finding-status-vocabulary ,finding-statuses)
    (published-metric-reconciliation ,reconciliation)
    (composition-root-reconciliation ,fan-out-reconciliation)
    (provide-risk-reconciliation ,provide-risks)
    (modules ,modules)
    (co-change ,co-change)
    (policy-exceptions ,exceptions)
    (test-inventory ,tests)
    (part-i-findings ,sorted-findings)))

(define (collect-architecture-snapshot revision
                                       #:root [root (current-directory)]
                                       #:last [last-n 200]
                                       #:findings [findings default-part-i-findings])
  (unless (and (exact-integer? last-n) (positive? last-n))
    (raise-argument-error 'collect-architecture-snapshot "positive exact integer" last-n))
  (define resolved (resolve-revision root revision))
  (define paths (tracked-rkt-paths root resolved))
  (define sources (revision-file-sources root resolved paths))
  (define policy-text
    (with-handlers ([exn:fail? (lambda (_error) #f)])
      (revision-file-content root resolved "docs/architecture/dependency-policy.rktd")))
  (define policy (and policy-text (call-with-input-string policy-text read)))
  (define readme-text
    (with-handlers ([exn:fail? (lambda (_error) #f)])
      (revision-file-content root resolved "README.md")))
  (define published-metrics
    (if readme-text
        (readme-published-metrics readme-text)
        '()))
  (define history
    (run-git root
             "log"
             (format "-n~a" last-n)
             "--no-merges"
             "--format=@@@commit%x09%H%x09%s"
             "--name-status"
             "--find-renames=100%"
             "--find-copies=100%"
             resolved
             "--"))
  (build-architecture-snapshot resolved
                               sources
                               (parse-git-log history)
                               #:last last-n
                               #:findings findings
                               #:policy-exceptions (if policy
                                                       (dependency-policy-exceptions policy)
                                                       '())
                               #:policy-composition-roots
                               (if policy
                                   (dependency-policy-composition-roots policy)
                                   '())
                               #:policy-provide-risks (if policy
                                                          (dependency-policy-provide-risks policy)
                                                          '())
                               #:published-metrics published-metrics))

(define (markdown-cell value)
  (string-replace (string-replace (format "~a" value) "|" "\\|") "\n" " "))

(define (architecture-snapshot->markdown snapshot)
  (define summary (entry-ref snapshot 'summary))
  (define reconciliation (entry-ref snapshot 'published-metric-reconciliation '()))
  (define modules (entry-ref snapshot 'modules))
  (define modules-by-hotspot
    (sort modules
          (lambda (left right)
            (define left-score (entry-ref left 'hotspot-score))
            (define right-score (entry-ref right 'hotspot-score))
            (if (= left-score right-score)
                (string<? (entry-ref left 'path) (entry-ref right 'path))
                (> left-score right-score)))))
  (define shown-modules (take modules-by-hotspot (min 100 (length modules-by-hotspot))))
  (define pairs (entry-ref snapshot 'co-change))
  (define exceptions (entry-ref snapshot 'policy-exceptions))
  (define tests (entry-ref snapshot 'test-inventory))
  (define tests-by-size
    (sort tests
          (lambda (left right)
            (define left-loc (entry-ref left 'loc))
            (define right-loc (entry-ref right 'loc))
            (if (= left-loc right-loc)
                (string<? (entry-ref left 'path) (entry-ref right 'path))
                (> left-loc right-loc)))))
  (define shown-tests (take tests-by-size (min 100 (length tests-by-size))))
  (define findings (entry-ref snapshot 'part-i-findings))
  (define fan-out-reconciliation (entry-ref snapshot 'composition-root-reconciliation '()))
  (define provide-risks (entry-ref snapshot 'provide-risk-reconciliation '()))
  (define out (open-output-string))
  (fprintf out "# Architecture Baseline\n\n")
  (fprintf out
           "Pinned revision: `~a`\n\nHistory limit: ~a commits\n\n"
           (entry-ref snapshot 'revision)
           (entry-ref snapshot 'history-limit))
  (fprintf out "## Summary\n\n")
  (for ([item (in-list summary)])
    (fprintf out "- ~a: ~a\n" (first item) (second item)))
  (fprintf out "\n## Published metric reconciliation\n\n")
  (fprintf out "| Metric | README | Measured | Status |\n|---|---:|---:|---|\n")
  (for ([item (in-list reconciliation)])
    (fprintf out
             "| ~a | ~a | ~a | ~a |\n"
             (entry-ref item 'metric)
             (entry-ref item 'published)
             (entry-ref item 'measured)
             (entry-ref item 'status)))
  (fprintf out "\n### Composition-root fan-out vs policy\n\n")
  (fprintf out "| Path | Policy | Measured | Status |\n|---|---:|---:|---|\n")
  (for ([entry (in-list fan-out-reconciliation)])
    (fprintf out
             "| `~a` | ~a | ~a | ~a |\n"
             (markdown-cell (entry-ref entry 'path))
             (entry-ref entry 'recorded-fan-out)
             (entry-ref entry 'measured-fan-out)
             (entry-ref entry 'status)))
  (fprintf out "\n### Provide-risk notes vs measured provides\n\n")
  (fprintf out "| Path | Recorded | Measured | Status |\n|---|---:|---:|---|\n")
  (for ([entry (in-list provide-risks)])
    (fprintf out
             "| `~a` | ~a | ~a | ~a |\n"
             (markdown-cell (entry-ref entry 'path))
             (entry-ref entry 'recorded-provides)
             (entry-ref entry 'measured-provides)
             (entry-ref entry 'status)))
  (fprintf
   out
   "\nPolicy exceptions are read from the pinned dependency policy; Part-I claims are reconciled below with live code evidence.\n")
  (fprintf out "\n## Top module hotspots\n\n")
  (fprintf out
           "Showing ~a of ~a tracked Racket modules; raw evidence contains all modules.\n\n"
           (length shown-modules)
           (length modules))
  (fprintf out "| Path | LOC | Provides | Fan-in | Fan-out | Changes | Hotspot |\n")
  (fprintf out "|---|---:|---:|---:|---:|---:|---:|\n")
  (for ([module (in-list shown-modules)])
    (fprintf out
             "| `~a` | ~a | ~a | ~a | ~a | ~a | ~a |\n"
             (markdown-cell (entry-ref module 'path))
             (entry-ref module 'loc)
             (entry-ref module 'provide-specs)
             (entry-ref module 'dependency-fan-in)
             (entry-ref module 'dependency-fan-out)
             (entry-ref module 'changed-commits)
             (entry-ref module 'hotspot-score)))
  (fprintf out "\n## Co-change evidence\n\n")
  (fprintf out "Release-only commits and exact R100/C100 moves are excluded.\n\n")
  (fprintf out "| Count | Path A | Path B |\n|---:|---|---|\n")
  (for ([pair (in-list pairs)])
    (fprintf out
             "| ~a | `~a` | `~a` |\n"
             (entry-ref pair 'count)
             (markdown-cell (entry-ref pair 'path-a))
             (markdown-cell (entry-ref pair 'path-b))))
  (fprintf out "\n## Policy exceptions\n\n")
  (fprintf out "| Layer | File | Owner | Lifecycle | Revisit | Rationale |\n")
  (fprintf out "|---|---|---|---|---|---|\n")
  (for ([exception (in-list exceptions)])
    (fprintf out
             "| ~a | `~a` | ~a | ~a | ~a | ~a |\n"
             (markdown-cell (entry-ref exception 'layer))
             (markdown-cell (entry-ref exception 'file))
             (markdown-cell (entry-ref exception 'owner))
             (entry-ref exception 'lifecycle)
             (markdown-cell (or (entry-ref exception 'revisit-by #f) "—"))
             (markdown-cell (entry-ref exception 'rationale))))
  (fprintf out "\n## Largest test files\n\n")
  (fprintf out
           "Showing ~a of ~a tracked test files; raw evidence contains all tests.\n\n"
           (length shown-tests)
           (length tests))
  (fprintf out "| Path | LOC | Test cases | Checks |\n|---|---:|---:|---:|\n")
  (for ([test (in-list shown-tests)])
    (fprintf out
             "| `~a` | ~a | ~a | ~a |\n"
             (markdown-cell (entry-ref test 'path))
             (entry-ref test 'loc)
             (entry-ref test 'test-cases)
             (entry-ref test 'checks)))
  (fprintf out "\n## Part-I finding statuses\n\n")
  (fprintf out "Allowed: ~a\n\n" (string-join finding-statuses ", "))
  (fprintf out "| ID | Finding | Status | Evidence |\n|---|---|---|---|\n")
  (for ([finding (in-list findings)])
    (fprintf out
             "| ~a | ~a | ~a | ~a |\n"
             (markdown-cell (entry-ref finding 'id))
             (markdown-cell (entry-ref finding 'finding "—"))
             (entry-ref finding 'status)
             (markdown-cell (string-join (entry-ref finding 'evidence '()) ", "))))
  (get-output-string out))
