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
         manifest->string)

(define (require-input-file root . relative)
  (define path (apply build-path root relative))
  (unless (file-exists? path)
    (raise-user-error 'subsystem-manifest "required input missing: ~a" relative))
  path)

(define (require-datum path what)
  (with-handlers ([exn:fail? (lambda (e)
                               (raise-user-error 'subsystem-manifest
                                                 "required input ~a is unparseable: ~a"
                                                 what
                                                 (exn-message e)))])
    (call-with-input-file path read)))

(define (require-nonempty what value)
  (unless (and (list? value) (pair? value))
    (raise-user-error 'subsystem-manifest "required input ~a is empty or malformed" what))
  value)

;; A tracked module that cannot be read is an inconsistent tree: generation
;; fails instead of recording an empty source (CL1 review).
(define (require-tracked-source root path)
  (with-handlers ([exn:fail? (lambda (e)
                               (raise-user-error 'subsystem-manifest
                                                 "tracked module ~a cannot be read: ~a"
                                                 path
                                                 (exn-message e)))])
    (call-with-input-file (build-path root (string->path path)) (lambda (in) (port->string in)))))

;; Optional inputs (CL1 review): recorded as explicitly unavailable in the
;; manifest's input-availability record instead of being silently omitted.
;; availability is a mutable symbol-keyed hash.
(define (load-optional what loader root availability)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (hash-set! availability what (format "unavailable: ~a" (exn-message e)))
                     #f)])
    (define value (loader root))
    (hash-set! availability what "available")
    value))

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
  ;; Fail-closed (CL1 review): a failed listing or an empty tracked-file set
  ;; would silently build an empty manifest that looks valid.
  (unless ok?
    (raise-user-error 'subsystem-manifest
                      "git ls-files failed: ~a"
                      (string-trim (get-output-string err))))
  (unless (pair? parsed)
    (raise-user-error 'subsystem-manifest "no tracked Racket files under ~a" root))
  (sort (remove-duplicates parsed string=?) string<?))

(define (read-source root path)
  (with-handlers ([exn:fail? (lambda (_) "")])
    (call-with-input-file (build-path root (string->path path)) (lambda (in) (port->string in)))))

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
  (define path (require-input-file root "tests" ".coverage-manifest.json"))
  (define data
    (with-handlers ([exn:fail? (lambda (e)
                                 (raise-user-error 'subsystem-manifest
                                                   "coverage manifest unparseable: ~a"
                                                   (exn-message e)))])
      (call-with-input-file path read-json)))
  (define entries (require-nonempty "coverage-manifest entries" (hash-ref data 'entries #f)))
  (for/list ([entry (in-list entries)])
    (define test (hash-ref entry 'test #f))
    (define covers (hash-ref entry 'covers #f))
    (unless (and (string? test) (pair? covers))
      (raise-user-error 'subsystem-manifest "coverage entry lacks test/covers: ~a" entry))
    (unless (file-exists? (build-path root test))
      (raise-user-error 'subsystem-manifest "coverage test missing on disk: ~a" test))
    (for ([covered (in-list covers)])
      (unless (file-exists? (build-path root covered))
        (raise-user-error 'subsystem-manifest
                          "coverage target missing on disk: ~a (from ~a)"
                          covered
                          test)))
    (cons test covers)))

(define (load-admission-owners root)
  (define path (require-input-file root "tests" ".quick-admission.rktd"))
  (define data (require-datum path "quick-admission record"))
  (define owners (entry-ref*-list data 'owners))
  (unless (and (pair? owners) (andmap pair? owners))
    (raise-user-error 'subsystem-manifest "admission record has no audited owners"))
  (for/list ([owner (in-list owners)])
    (define test (car owner))
    (unless (string? test)
      (raise-user-error 'subsystem-manifest "admission record entry without a test path: ~a" owner))
    (unless (file-exists? (build-path root test))
      (raise-user-error 'subsystem-manifest "admitted owner missing on disk: ~a" test))
    (for ([file (in-list (entry-ref*-list (cdr owner) 'closure))])
      (unless (file-exists? (build-path root file))
        (raise-user-error 'subsystem-manifest
                          "owner closure file missing on disk: ~a (from ~a)"
                          file
                          test)))
    (cons test (cdr owner))))

(define (load-ownership-areas root)
  (define path (require-input-file root "reports" "test-ownership-map.json"))
  (call-with-input-file path
                        (lambda (in)
                          (for/list ([area (in-list (hash-ref (read-json in) 'areas '()))])
                            (cons (hash-ref area 'area)
                                  (hash-ref area 'suite (hash-ref area 'owning_path "")))))))

(define (load-gsd-inventory root)
  (define mod (build-path root "extensions" "gsd" "responsibility-inventory.rkt"))
  (define inventory (dynamic-require mod 'inventory))
  (define entry-module-file (dynamic-require mod 'entry-module-file))
  (define entry-domain (dynamic-require mod 'entry-domain))
  (define entry-effects (dynamic-require mod 'entry-effects))
  (require-nonempty "gsd responsibility inventory" inventory)
  (for/list ([e (in-list inventory)])
    (cons (entry-module-file e) (list (entry-domain e) (entry-effects e)))))

(define (load-policy-exception-files root)
  (define path (require-input-file root "docs" "architecture" "dependency-policy.rktd"))
  (define policy (require-datum path "dependency policy"))
  (unless (assoc 'known-exceptions policy)
    (raise-user-error 'subsystem-manifest "dependency policy lacks a known-exceptions section"))
  (for/list ([exception (in-list (dependency-policy-exceptions policy))])
    (cons (entry-ref* exception 'layer) (entry-ref* exception 'file))))

;; ---- assembly ---------------------------------------------------------------------

(define (production-module-records root)
  (define paths (tracked-rkt-paths/root root))
  (define records
    (for/list ([path (in-list paths)]
               #:unless (test-path? path))
      (cons path (require-tracked-source root path))))
  (unless (pair? records)
    (raise-user-error 'subsystem-manifest "no production modules found under ~a" root))
  records)

(define (dynamic-require? source)
  (regexp-match? #rx"dynamic-require" source))

(define (build-manifest root)
  (define availability (make-hash))
  (define modules (production-module-records root))
  (define graph (build-graph root modules))
  (define coverage (load-coverage-mappings root))
  (define admission (load-admission-owners root))
  (define ownership (load-optional 'ownership-map load-ownership-areas root availability))
  (define gsd (load-optional 'gsd-inventory load-gsd-inventory root availability))
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
      (define gsd-column
        (if (not gsd)
            'unavailable
            (for/list ([module+facts (in-list (sort gsd string<? #:key car))]
                       #:do [(define module-file (car module+facts))]
                       #:when (set-member? member-set (string-append "extensions/gsd/" module-file)))
              (list (list 'module module-file)
                    (list 'domain (first (cdr module+facts)))
                    (list 'effects (second (cdr module+facts)))))))
      (define ownership-column
        (if (not ownership)
            'unavailable
            (for/first ([area+suite (in-list ownership)]
                        #:when (string=? (car area+suite) name))
              (cdr area+suite))))
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
      `((name ,name) (production-modules ,(length members))
                     (provide-total ,provide-total)
                     (dynamic-require-modules ,dynamic-modules)
                     (owner-tests ,owner-tests)
                     (coverage-mappings ,coverage-here)
                     (outbound-edges ,outbound)
                     (gsd-inventory ,gsd-column)
                     (policy-exceptions ,(sort (for/list ([layer+file (in-list policy-exceptions)]
                                                          #:when (string=? (car layer+file) name))
                                                 (cdr layer+file))
                                               string<?))
                     (ownership-suite ,ownership-column))))

  (define input-availability
    (sort (append (for/list ([what (in-list '(coverage-manifest admission-record
                                                                dependency-policy
                                                                tracked-sources))])
                    (cons what "required"))
                  (for/list ([(what status) (in-hash availability)])
                    (cons what status)))
          string<?
          #:key (lambda (pair) (symbol->string (car pair)))))

  `((schema-version 1)
    (generator
     (name "scripts/subsystem-manifest.rkt")
     (reproduction-command
      "racket scripts/subsystem-manifest.rkt --out docs/architecture/subsystem-manifest.rktd"))
    (input-availability ,input-availability)
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
