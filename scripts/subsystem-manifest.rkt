#lang racket/base

;; scripts/subsystem-manifest.rkt — derive the subsystem navigational manifest
;; (CL1, issue #9978).
;;
;; NOTE: requires the `json` package (read-json), not `racket/json` — the
;; repo's CI base image ships a minimal Racket without racket/json.rkt (see
;; scripts/run-tests/classify.rkt W2 note).
;;
;; Emits docs/architecture/subsystem-manifest.rktd: a machine-readable map of
;; every top-level subsystem built ONLY from existing metadata —
;;   - the working-tree source graph (static relative requires resolved to
;;     tracked paths, extraction semantics reused from
;;     scripts/architecture-baseline-helpers.rkt),
;;   - tests/.coverage-manifest.json (@covers test -> production files),
;;   - tests/.quick-admission.rktd (audited quick owners + capability status),
;;   - reports/test-ownership-map.json (area -> suite mapping),
;;   - extensions/gsd/responsibility-inventory.rkt (machine-checked GSD
;;     module effects/domains),
;;   - docs/architecture/dependency-policy.rktd (boundary exceptions).
;; No hand-maintained module lists live here or in the emitted manifest;
;; freshness is a byte-identical regeneration check pinned by
;; tests/test-subsystem-maps.rkt. The pinned metric record of record remains
;; docs/reports/architecture-baseline-cl0-003e425d.rktd (CL0); this manifest
;; is a navigational view of the current tree, not a metric authority.
;;
;; Usage:
;;   racket scripts/subsystem-manifest.rkt \
;;     --out docs/architecture/subsystem-manifest.rktd

(require racket/cmdline
         racket/file
         racket/list
         racket/set
         racket/port
         racket/string
         racket/system
         json
         "architecture-baseline-helpers.rkt") ; json pkg: read-json (racket/json is

(provide build-manifest
         manifest->string
         current-git-revision)

(define (entry-ref* entry key [default #f])
  ;; Metadata records use two shapes: proper (key value ...) assoc entries
  ;; (dependency-policy exceptions) and improper (key . value) pairs (quick
  ;; admission record). Normalize both: a single trailing value unwraps to
  ;; the value itself; multiple values keep the list.
  (define found (assoc key entry))
  (if (not found)
      default
      (let ([rest (cdr found)])
        (cond
          [(null? rest) default]
          [(and (pair? rest) (null? (cdr rest))) (car rest)]
          [else rest]))))

;; List-valued metadata field (e.g. the owner closure list): keep every
;; element, wrapping a bare improper value into a one-element list.
(define (entry-ref*-list entry key)
  (define found (assoc key entry))
  (if (not found)
      '()
      (let ([rest (cdr found)])
        (cond
          [(null? rest) '()]
          [(list? rest) rest]
          [else (list rest)]))))

;; ---- source collection ---------------------------------------------------------

;; Bare executable names do not PATH-resolve reliably under parameterized
;; ports in some environments (CL0 lesson): resolve git explicitly and fail
;; loudly when absent — an empty tracked-file list would silently build an
;; empty manifest.
(define git-path (find-executable-path "git"))

(define (require-git)
  (unless git-path
    (raise-user-error 'subsystem-manifest "git executable not found on PATH"))
  git-path)

(define (tracked-rkt-paths/root root)
  (define out (open-output-string))
  (define err (open-output-string))
  (define ok?
    (parameterize ([current-directory root]
                   [current-output-port out]
                   [current-error-port err])
      (system* git-path "ls-files" "*.rkt")))
  (define parsed
    ;; string-split, not in-lines: reading a string port line-wise misbehaves
    ;; in some parameterized contexts observed locally (CL1 debugging note).
    (for/list ([line (in-list (string-split (get-output-string out) "\n"))]
               #:unless (string=? (string-trim line) ""))
      (string-trim line)))
  (if ok?
      (sort (remove-duplicates parsed string=?) string<?)
      '()))

(define (read-source root path)
  (with-handlers ([exn:fail? (lambda (_) "")])
    (call-with-input-file (build-path root (string->path path)) (lambda (in) (port->string in)))))

(define (current-git-revision root)
  (define out (open-output-string))
  (define err (open-output-string))
  (define code
    (parameterize ([current-directory root]
                   [current-output-port out]
                   [current-error-port err])
      (system* (require-git) "rev-parse" "HEAD")))
  (if (and code (non-empty-string? (string-trim (get-output-string out))))
      (string-trim (get-output-string out))
      "unknown"))

;; ---- graph ---------------------------------------------------------------------

;; modules: list of (path source). Returns hash path -> sorted internal deps.
(define (build-graph root modules)
  (define paths (map car modules))
  (for/hash ([record (in-list modules)])
    (define path (car record))
    (define deps
      (remove-duplicates (for/list ([required (in-list (collected-module-path-strings (cdr record)))]
                                    #:do [(define normalized (normal-require-path path required))]
                                    #:when (and normalized (member normalized paths string=?)))
                           normalized)
                         string=?))
    (values path (sort deps string<?))))

(define (subsystem-of path)
  (define parts (string-split path "/"))
  (if (= (length parts) 1)
      "root"
      (first parts)))

;; ---- metadata loaders ------------------------------------------------------------

(define (load-coverage-mappings root)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (call-with-input-file (build-path root "tests" ".coverage-manifest.json")
                          (lambda (in)
                            (define data (read-json in))
                            (for/list ([entry (in-list (hash-ref data 'entries '()))])
                              (cons (hash-ref entry 'test) (hash-ref entry 'covers '())))))))

(define (load-admission-owners root)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (define data (call-with-input-file (build-path root "tests" ".quick-admission.rktd") read))
    (define owners (entry-ref*-list data 'owners))
    (for/list ([owner (in-list owners)])
      (cons (car owner) (cdr owner)))))

(define (load-ownership-areas root)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (call-with-input-file (build-path root "reports" "test-ownership-map.json")
                          (lambda (in)
                            (for/list ([area (in-list (hash-ref (read-json in) 'areas '()))])
                              (cons (hash-ref area 'area)
                                    (hash-ref area 'suite (hash-ref area 'owning_path ""))))))))

(define (load-gsd-inventory root)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (define mod (build-path root "extensions" "gsd" "responsibility-inventory.rkt"))
    (define inventory (dynamic-require mod 'inventory))
    (define entry-module-file (dynamic-require mod 'entry-module-file))
    (define entry-domain (dynamic-require mod 'entry-domain))
    (define entry-effects (dynamic-require mod 'entry-effects))
    (for/list ([e (in-list inventory)])
      (cons (entry-module-file e) (list (entry-domain e) (entry-effects e))))))

(define (load-policy-exception-files root)
  (with-handlers ([exn:fail? (lambda (_) '())])
    (define policy
      (call-with-input-file (build-path root "docs" "architecture" "dependency-policy.rktd") read))
    (for/list ([exception (in-list (dependency-policy-exceptions policy))])
      (cons (entry-ref* exception 'layer) (entry-ref* exception 'file)))))

;; ---- assembly ---------------------------------------------------------------------

(define (production-module-records root)
  (define paths (tracked-rkt-paths/root root))
  (for/list ([path (in-list paths)]
             #:unless (test-path? path))
    (cons path (read-source root path))))

(define (dynamic-require? source)
  (regexp-match? #rx"dynamic-require" source))

(define (build-manifest root)
  (define modules (production-module-records root))
  (define graph (build-graph root modules))
  (define coverage (load-coverage-mappings root))
  (define admission (load-admission-owners root))
  (define ownership (load-ownership-areas root))
  (define gsd (load-gsd-inventory root))
  (define policy-exceptions (load-policy-exception-files root))

  (define subsystem-names
    (sort (remove-duplicates (map (lambda (record) (subsystem-of (car record))) modules) string=?)
          string<?))

  (define admission-attrs
    (for/hash ([record (in-list admission)])
      (values (car record) (cdr record))))

  (define subsystems
    (for/list ([name (in-list subsystem-names)])
      (define members
        (sort (for/list ([record (in-list modules)]
                         #:when (string=? (subsystem-of (car record)) name))
                record)
              string<?
              #:key car))
      (define member-paths (map car members))
      (define member-set (list->set member-paths))
      (define provide-total (for/sum ([record (in-list members)]) (provide-spec-count (cdr record))))
      (define dynamic-modules
        (for/list ([record (in-list members)]
                   #:when (dynamic-require? (cdr record)))
          (car record)))
      ;; audited quick owners whose closure intersects this subsystem
      (define owner-tests
        (for/list ([(test attrs) (in-hash admission-attrs)]
                   #:do [(define closure (entry-ref*-list attrs 'closure))
                         (define hits
                           (sort (for/list ([file (in-list closure)]
                                            #:when (set-member? member-set file))
                                   file)
                                 string<?))]
                   #:when (pair? hits))
          `((test ,test) (status ,(entry-ref* attrs 'status "unknown"))
                         (capability ,(entry-ref* attrs 'capability))
                         (subsystem-files ,hits))))
      ;; @covers mappings targeting production files in this subsystem
      (define coverage-here
        (for/list ([mapping (in-list coverage)]
                   #:do [(define covered
                           (sort (for/list ([file (in-list (cdr mapping))]
                                            #:when (set-member? member-set file))
                                   file)
                                 string<?))]
                   #:when (pair? covered))
          `((test ,(car mapping)) (covers ,covered))))
      ;; outbound edges to other subsystems (aggregated)
      (define outbound-targets
        (sort (remove-duplicates (for*/list ([from (in-list member-paths)]
                                             [to (in-list (hash-ref graph from '()))])
                                   (subsystem-of to))
                                 string=?)
              string<?))
      (define outbound
        (for/list ([target-subsys (in-list outbound-targets)]
                   #:unless (string=? target-subsys name))
          (define count
            (for*/sum ([from (in-list member-paths)] [to (in-list (hash-ref graph from '()))]
                                                     #:when (string=? (subsystem-of to)
                                                                      target-subsys))
                      1))
          `((to ,target-subsys) (edges ,count))))
      `((name ,name)
        (production-modules ,(length members))
        (provide-total ,provide-total)
        (dynamic-require-modules ,dynamic-modules)
        (owner-tests ,owner-tests)
        (coverage-mappings ,coverage-here)
        (outbound-edges ,outbound)
        (gsd-inventory ,(for/list ([module+facts (in-list (sort gsd string<? #:key car))]
                                   #:do [(define module-file (car module+facts))]
                                   #:when (set-member? member-set
                                                       (string-append "extensions/gsd/" module-file)))
                          `((module ,module-file) (domain ,(first (cdr module+facts)))
                                                  (effects ,(second (cdr module+facts))))))
        (policy-exceptions ,(sort (for/list ([layer+file (in-list policy-exceptions)]
                                             #:when (string=? (car layer+file) name))
                                    (cdr layer+file))
                                  string<?))
        (ownership-suite ,(for/first ([area+suite (in-list ownership)]
                                      #:when (string=? (car area+suite) name))
                            (cdr area+suite))))))

  `((schema-version 1)
    (generator
     (name "scripts/subsystem-manifest.rkt")
     (reproduction-command
      "racket scripts/subsystem-manifest.rkt --out docs/architecture/subsystem-manifest.rktd"))
    (revision ,(current-git-revision root))
    (production-modules ,(length modules))
    (subsystems ,subsystems)))

;; ---- deterministic rendering -------------------------------------------------------

(define (canonicalize value)
  (cond
    [(hash? value) (canonicalize (hash->list value))]
    [(set? value) (canonicalize (set->list value))]
    [(pair? value)
     (if (symbol? (car value))
         (cons (car value) (canonicalize (cdr value)))
         (map canonicalize value))]
    [else value]))

(define (manifest->string manifest)
  ;; write (not writeln): no trailing newline, so regeneration and the
  ;; committed artifact are byte-identical without trim conventions.
  (define canonical (canonicalize manifest))
  (with-output-to-string (lambda () (write canonical))))

;; ---- CLI ---------------------------------------------------------------------------

(module+ main
  (define out-path #f)
  (define root (current-directory))
  (command-line #:program "subsystem-manifest.rkt"
                #:once-each [("--out") path "Manifest output path (.rktd)" (set! out-path path)]
                [("--root") path "Repository root (default: current directory)" (set! root path)])
  (unless out-path
    (raise-user-error 'subsystem-manifest "missing required --out PATH"))
  (define manifest (build-manifest root))
  (call-with-output-file out-path
                         (lambda (output) (display (manifest->string manifest) output))
                         #:exists 'truncate/replace))
