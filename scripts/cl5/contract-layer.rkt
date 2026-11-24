#lang racket/base

;; contract-layer.rkt — CL5 F3: evaluate the repository's OWN contract layer for
;; a changed-file set, for the W3 evaluation driver only.
;;
;; This module does not reimplement contract logic. It runs the target tree's
;; `scripts/cl5/contract-escalation.rkt` (`evaluate-contract-changes` plus the
;; verified `contract-selection` bridge) in a CHILD process, so:
;;   * the decision is the shipped layer's own decision;
;;   * the code revision is separable from the data/tree revision:
;;       - historical: the layer and the evaluated tree come from one revision;
;;       - counterfactual: the layer comes from a different, explicitly named
;;         revision while the tree/graph/manifest come from the evaluated one.
;;         That is recorded as `counterfactual`, never as a historical run.
;;
;; Fail-closed. Any of the following yields status 'invalid/'unavailable and an
;; instruction of `unavailable` (never an empty or invented selection):
;;   * the child failed, timed out, or emitted non-JSON;
;;   * an unknown verdict or instruction symbol;
;;   * `run-all` (the bridge could not resolve a test list);
;;   * `run-required` with an empty, mismatched, or nonexistent required set;
;;   * `run-comprehensive` that omits a required test or adds a test outside
;;     required ∪ comprehensive;
;;   * a verdict/escalation contradiction;
;;   * a contract module modified relative to its own revision.
;;
;; Evaluation-only: reads files and runs the contract module; changes no
;; production selector, scheduler, admission record or CI policy.

(require file/sha1
         json
         racket/cmdline
         racket/file
         racket/format
         racket/list
         racket/path
         racket/runtime-path
         racket/string
         "selection-experiment.rkt")

(define-runtime-path this-file "contract-layer.rkt")

(provide contract-module-relative-path
         contract-module-info
         evaluate-layer
         layer-absent
         layer-instruction
         layer-record-valid?
         parse-layer-payload
         validate-layer-output
         validate-layer-record)

;; ---- small helpers -----------------------------------------------------------------

(define contract-module-relative-path "scripts/cl5/contract-escalation.rkt")

(define (hex-string bytes)
  (apply string-append
         (for/list ([byte (in-bytes bytes)])
           (~r byte #:min-width 2 #:pad-string "0" #:base 16))))

(define (file-sha1 path)
  (hex-string (sha1-bytes (file->bytes path))))

(define (as-symbol value)
  (if (string? value)
      (string->symbol value)
      value))

(define (truncate-text text)
  (if (> (string-length text) 400)
      (substring text 0 400)
      text))

(define (sorted-unique files)
  (sort (remove-duplicates files) string<?))

;; `hash-union` is not in this Racket's base; merge explicitly (right side wins).
(define (merge-hash base extra)
  (for/fold ([acc base]) ([key (in-list (hash-keys extra))])
    (hash-set acc key (hash-ref extra key))))

;; ---- contract module provenance ----------------------------------------------------

;; Where the code came from, separate from the tree it is evaluated against.
;; `modified?` is a provenance refusal: a dirty layer file is not "the layer at
;; revision R" even if HEAD says so.
(define (contract-module-info worktree)
  (define path (build-path worktree contract-module-relative-path))
  (define head (string-trim (git! worktree "rev-parse" "HEAD")))
  (define status (string-trim (git! worktree "status" "--porcelain")))
  (cond
    [(not (file-exists? path))
     (hash 'exists?
           #f
           'module
           contract-module-relative-path
           'revision
           head
           'blob-sha
           #f
           'file-sha1
           #f
           'dirty?
           (not (equal? status ""))
           'modified?
           #f)]
    [else
     (define blob (string-trim (git! worktree "hash-object" contract-module-relative-path)))
     (define head-blob
       (with-handlers ([exn:fail? (lambda (_) #f)])
         (string-trim (git! worktree "rev-parse" (format "HEAD:~a" contract-module-relative-path)))))
     (hash 'exists?
           #t
           'module
           contract-module-relative-path
           'revision
           head
           'blob-sha
           blob
           'file-sha1
           (file-sha1 path)
           'dirty?
           (not (equal? status ""))
           'modified?
           (and head-blob (not (equal? blob head-blob))))]))

(define (layer-present-at-revision? worktree revision)
  (define-values (code _e _o _err _t)
    (run-command (list "git" "cat-file" "-e" (format "~a:~a" revision contract-module-relative-path))
                 #:cwd worktree
                 #:timeout-seconds 120))
  (equal? code 0))

;; ---- evaluation through the shipped layer ------------------------------------------

;; Absence is a status; `evaluate-layer` derives the state/mode so that a
;; historical tree evaluated with a later layer is `counterfactual`, not absence.
(define (layer-absent reason)
  (hash 'status 'absent 'problems '() 'reasons (list reason) 'decision #f 'selection #f 'required #f))

(define (unavailable reason)
  (hash 'status
        'unavailable
        'problems
        '()
        'reasons
        (list reason)
        'decision
        #f
        'selection
        #f
        'required
        #f))

;; The child is this same file in --evaluate mode: it dynamic-requires the
;; target layer and prints the decision + bridge instruction as JSON. `root` is
;; the tree whose graph/manifest/changed files are read, which is NOT
;; necessarily the tree the code came from (counterfactual).
(define (run-child root module-file changed comprehensive timeout)
  (define tmp (make-temporary-file "cl5-comprehensive-~a.json"))
  (dynamic-wind
   (lambda ()
     (call-with-output-file tmp
                            (lambda (out) (write-json comprehensive out))
                            #:exists 'truncate/replace))
   (lambda ()
     (define-values (code elapsed out err timed-out?)
       (run-command (list "racket"
                          (path->string this-file)
                          "--evaluate"
                          "--root"
                          (path->string (simplify-path (path->complete-path root)))
                          "--contract-module"
                          (path->string (simplify-path (path->complete-path module-file)))
                          "--changed"
                          (string-join changed ",")
                          "--comprehensive-file"
                          (path->string tmp))
                    #:cwd (if (path? root)
                              root
                              (string->path root))
                    #:timeout-seconds timeout))
     (void elapsed)
     (cond
       [timed-out? (unavailable (format "contract layer evaluation timed out after ~a s" timeout))]
       [(not (equal? code 0))
        (unavailable (format "contract layer evaluation failed (exit ~a): ~a"
                             code
                             (truncate-text (if (equal? err "") out err))))]
       [else
        (with-handlers ([exn:fail? (lambda (e)
                                     (unavailable (format "contract layer output is unusable: ~a"
                                                          (truncate-text (exn-message e)))))])
          (parse-layer-payload out))]))
   (lambda ()
     (when (file-exists? tmp)
       (delete-file tmp)))))

(define (parse-layer-payload text)
  (define payload (read-json (open-input-string text)))
  (unless (hash? payload)
    (raise-arguments-error 'parse-layer-payload "output is not a JSON object"))
  payload)

;; ---- validation of what the layer decided ------------------------------------------

(define (valid-verdict? value)
  (memq value '(covered escalate)))

(define (check-path-list value label problem!)
  (cond
    [(eq? value 'unresolved) (void)]
    [(not (list? value)) (problem! (format "~a is not a list" label))]
    [(not (andmap string? value)) (problem! (format "~a is not a list of paths" label))]
    [else (void)]))

;; Pure validation of the child's payload against the layer's own contract.
;; Returns (values problems normalized-selection). `problems` non-empty means the
;; instruction may not be used.
(define (validate-layer-output payload root comprehensive)
  (define problems '())
  (define (problem! message)
    (set! problems (cons message problems)))
  (define error (hash-ref payload 'error #f))
  (when error
    (problem! (format "contract layer raised: ~a" error)))
  (define decision (hash-ref payload 'decision #f))
  (define selection (hash-ref payload 'selection #f))
  (define verdict (and (hash? decision) (as-symbol (hash-ref decision 'verdict 'unrecognized))))
  (define decision-required (and (hash? decision) (hash-ref decision 'required #f)))
  (define escalations
    (if (hash? decision)
        (hash-ref decision 'escalations '())
        '()))
  (cond
    [(not (hash? decision)) (problem! "no decision was returned")]
    [(not (valid-verdict? verdict)) (problem! (format "unknown verdict ~a" verdict))]
    [(not (list? escalations)) (problem! "escalations is not a list")]
    [(and (eq? verdict 'covered) (pair? escalations))
     (problem! "verdict 'covered carries escalations")]
    [(and (eq? verdict 'escalate) (null? escalations))
     (problem! "verdict 'escalate carries no escalation reason")]
    [else (void)])
  (unless (and (list? decision-required) (andmap string? decision-required))
    (problem! "decision required set is not a list of paths"))

  (define instruction
    (and (hash? selection) (as-symbol (hash-ref selection 'instruction 'unrecognized))))
  (cond
    [(not (hash? selection)) (problem! "no selection instruction was returned")]
    [(not (memq instruction '(run-required run-comprehensive run-all)))
     (problem! (format "unknown instruction ~a" (hash-ref selection 'instruction 'missing)))]
    [(eq? instruction 'run-all)
     (problem! "the contract layer could not resolve a test list (run-all)")]
    [else (void)])

  (define (unresolved->symbol value)
    (if (equal? value "unresolved") 'unresolved value))
  (define tests (and (hash? selection) (unresolved->symbol (hash-ref selection 'tests 'missing))))
  (define required
    (and (hash? selection) (unresolved->symbol (hash-ref selection 'required 'missing))))
  (check-path-list tests "selected tests" problem!)
  (check-path-list required "required tests" problem!)

  (when (and (list? required)
             (list? decision-required)
             (not (equal? (sorted-unique required) (sorted-unique decision-required))))
    (problem! "the selection's required set differs from the decision's"))

  (define comprehensive* (sorted-unique comprehensive))
  (define (exists? path)
    (file-exists? (build-path root path)))
  (define (check-exist value label)
    (when (list? value)
      (for ([path (in-list value)]
            #:unless (exists? path))
        (problem! (format "~a names a test that does not exist: ~a" label path)))))

  (define (check-run-required)
    (cond
      [(not (list? required)) (problem! "run-required without a resolved required set")]
      [(null? required) (problem! "run-required with an empty required set")]
      [(not (list? tests)) (problem! "run-required without a resolved test list")]
      [(not (equal? (sorted-unique tests) (sorted-unique required)))
       (problem! "run-required does not run exactly the required set")]
      [(not (eq? verdict 'covered)) (problem! "run-required contradicts an escalated verdict")]
      [(pair? escalations) (problem! "run-required contradicts recorded escalations")]
      [else (void)])
    (check-exist required "required tests"))

  (define (check-run-comprehensive)
    (cond
      [(not (list? required)) (problem! "run-comprehensive without a resolved required set")]
      [(not (list? tests)) (problem! "run-comprehensive without a resolved test list")]
      [(not (eq? verdict 'escalate)) (problem! "run-comprehensive contradicts a covered verdict")]
      [else
       (define required* (sorted-unique required))
       (define tests* (sorted-unique tests))
       (define expected (sorted-unique (append required* comprehensive*)))
       (unless (equal? tests* expected)
         (problem! "run-comprehensive is not required union comprehensive"))])
    (check-exist (if (list? required)
                     required
                     '())
                 "required tests")
    (check-exist (if (list? tests)
                     tests
                     '())
                 "selected tests"))

  (case (if (memq instruction '(run-required run-comprehensive)) instruction 'unrecognized)
    [(run-required) (check-run-required)]
    [(run-comprehensive) (check-run-comprehensive)]
    [else (void)])

  (values (reverse problems)
          (hash 'instruction
                (if (null? problems) instruction 'unavailable)
                'tests
                (if (null? problems) tests 'unresolved)
                'required
                (if (null? problems) required 'unresolved)
                'escalations
                escalations)))

;; ---- the evaluated record block ----------------------------------------------------

;; `evaluated-revision`/`evaluated-tree` identify the tree being evaluated.
;; `contract-worktree`/`contract-revision` identify the code that decided. When
;; they differ (or when the evaluated revision has no layer and a later one is
;; supplied), the block is a counterfactual and says so.
;; JSON gives strings; the in-memory record uses symbols for verdict/code, the
;; same form `contract-decision` produces.
(define (normalize-decision decision)
  (cond
    [(not (hash? decision)) decision]
    [else
     (hash-set (hash-set decision 'verdict (as-symbol (hash-ref decision 'verdict 'unrecognized)))
               'escalations
               (for/list ([e (in-list (hash-ref decision 'escalations '()))])
                 (if (hash? e)
                     (hash-set e 'code (as-symbol (hash-ref e 'code 'unrecognized)))
                     e)))]))

(define (evaluate-layer #:root root
                        #:changed changed
                        #:comprehensive comprehensive
                        #:evaluated-revision revision
                        #:evaluated-tree tree
                        #:contract-worktree [contract-worktree root]
                        #:contract-revision [contract-revision revision]
                        #:timeout [timeout 1800])
  (define info (contract-module-info contract-worktree))
  (define present? (layer-present-at-revision? contract-worktree revision))
  (define counterfactual? (not (equal? contract-revision revision)))
  (define state
    (cond
      [(and present? (not counterfactual?)) 'present]
      [(hash-ref info 'exists?) 'counterfactual]
      [else 'absent-at-revision]))
  (define comprehensive* (sorted-unique comprehensive))
  (define base
    (hash 'state
          state
          'mode
          (if (eq? state 'counterfactual) 'counterfactual 'historical)
          'evaluated-revision
          revision
          'evaluated-tree
          tree
          'contract-worktree
          (path->string (simplify-path (path->complete-path contract-worktree)))
          'contract-revision
          contract-revision
          'contract-module
          contract-module-relative-path
          'contract-blob-sha
          (hash-ref info 'blob-sha)
          'contract-file-sha1
          (hash-ref info 'file-sha1)
          'contract-modified?
          (hash-ref info 'modified?)
          'contract-worktree-dirty?
          (hash-ref info 'dirty?)
          'layer-present-at-evaluated-revision?
          present?
          'counterfactual?
          (eq? state 'counterfactual)
          'changed
          (sorted-unique changed)
          'comprehensive-count
          (length comprehensive*)
          'comprehensive-digest
          (hex-string (sha1-bytes (string->bytes/utf-8
                                   (string-append (string-join comprehensive* "\n") "\n"))))))
  (define evaluation
    (cond
      [(not (hash-ref info 'exists?))
       (layer-absent "no contract layer exists in the contract worktree")]
      [(hash-ref info 'modified?)
       (hash 'status
             'invalid
             'problems
             (list "the contract module is modified relative to its own revision")
             'reasons
             '()
             'decision
             #f
             'selection
             #f
             'required
             #f)]
      [else
       (define payload
         (run-child root
                    (build-path contract-worktree contract-module-relative-path)
                    (sorted-unique changed)
                    comprehensive*
                    timeout))
       (cond
         [(eq? (hash-ref payload 'status #f) 'unavailable) payload]
         [else
          (define-values (problems selection) (validate-layer-output payload root comprehensive*))
          (define decision (normalize-decision (hash-ref payload 'decision #f)))
          (hash 'status
                (if (null? problems) 'evaluated 'invalid)
                'problems
                problems
                'reasons
                '()
                'decision
                decision
                'selection
                selection
                'required
                (and selection (hash-ref selection 'required 'unresolved)))])]))
  (merge-hash base evaluation))

;; The instruction the W3 driver may use. Nothing here defaults to a selection.
(define (layer-instruction layer)
  (case (as-symbol (hash-ref layer 'state 'unrecognized))
    [(absent-at-revision) 'run-required]
    [(present counterfactual)
     (cond
       [(eq? (as-symbol (hash-ref layer 'status 'unavailable)) 'evaluated)
        (define instruction
          (as-symbol (hash-ref (hash-ref layer 'selection) 'instruction 'unavailable)))
        (if (memq instruction '(run-required run-comprehensive)) instruction 'unavailable)]
       [else 'unavailable])]
    [else 'unavailable]))

;; ---- structural re-verification of a recorded block --------------------------------

;; Cannot re-run the layer; checks that the recorded instruction follows from the
;; recorded decision/selection and the recorded comprehensive enumeration.
(define (validate-layer-record layer comprehensive-files)
  (define problems '())
  (define (problem! message)
    (set! problems (cons message problems)))
  (cond
    [(not (hash? layer)) (problem! "contract layer block is not an object")]
    [else
     (define state (as-symbol (hash-ref layer 'state 'unrecognized)))
     (unless (memq state '(present counterfactual absent-at-revision))
       (problem! (format "unknown contract layer state ~a" state)))
     (cond
       [(hash-has-key? layer 'status) (check-modern-layer layer state comprehensive-files problem!)]
       [else (check-legacy-layer layer state problem!)])
     (unless (memq (layer-instruction layer) '(run-required run-comprehensive unavailable))
       (problem! "recorded instruction does not follow from the record"))])
  (reverse problems))

;; Frozen W3.1 shape: only absence was representable.
(define (check-legacy-layer layer state problem!)
  (unless (eq? state 'absent-at-revision)
    (problem! "a present contract layer must carry an evaluation"))
  (unless (eq? (as-symbol (hash-ref layer 'instruction 'unavailable)) 'run-required)
    (problem! "an absent contract layer must not carry an instruction")))

(define (check-modern-layer layer state comprehensive-files problem!)
  (define status (as-symbol (hash-ref layer 'status 'unrecognized)))
  (unless (memq status '(evaluated absent invalid unavailable))
    (problem! (format "unknown contract layer status ~a" status)))
  (check-layer-state layer state status problem!)
  (case status
    [(evaluated) (check-evaluated-layer layer comprehensive-files problem!)]
    [(invalid)
     (unless (pair? (hash-ref layer 'problems '()))
       (problem! "an invalid layer must record its problems"))]
    [else (void)]))

(define (check-layer-state layer state status problem!)
  (case state
    [(present)
     (unless (equal? (hash-ref layer 'contract-revision #f) (hash-ref layer 'evaluated-revision #f))
       (problem! "a historical evaluation must use the evaluated revision's own layer"))
     (unless (hash-ref layer 'layer-present-at-evaluated-revision? #f)
       (problem! "a historical evaluation must record that the layer exists at the revision"))
     (unless (eq? (as-symbol (hash-ref layer 'mode 'unrecognized)) 'historical)
       (problem! "a historical evaluation must be recorded with mode historical"))
     (when (hash-ref layer 'counterfactual? #f)
       (problem! "a historical evaluation must not claim counterfactual?"))]
    [(counterfactual)
     (when (equal? (hash-ref layer 'contract-revision #f) (hash-ref layer 'evaluated-revision #f))
       (problem! "a counterfactual evaluation must name a different contract revision"))
     (unless (eq? (as-symbol (hash-ref layer 'mode 'unrecognized)) 'counterfactual)
       (problem! "a counterfactual evaluation must be labelled as such"))
     (unless (hash-ref layer 'counterfactual? #f)
       (problem! "a counterfactual evaluation must record counterfactual? #t"))]
    [(absent-at-revision)
     (unless (eq? status 'absent)
       (problem! "an absent contract layer must have status 'absent"))
     (when (hash-ref layer 'counterfactual? #f)
       (problem! "an absent contract layer cannot be counterfactual"))]
    [else (void)]))

(define (check-evaluated-layer layer comprehensive-files problem!)
  (define decision (hash-ref layer 'decision #f))
  (define selection (hash-ref layer 'selection #f))
  (if (not (and (hash? decision) (hash? selection)))
      (problem! "an evaluated layer must record its decision and selection")
      (check-evaluated-parts layer selection decision comprehensive-files problem!)))

(define (check-evaluated-parts layer selection decision comprehensive-files problem!)
  (define verdict (as-symbol (hash-ref decision 'verdict 'unrecognized)))
  (define escalations (hash-ref decision 'escalations '()))
  (define instruction (as-symbol (hash-ref selection 'instruction 'unrecognized)))
  (define required (hash-ref selection 'required 'unresolved))
  (define tests (hash-ref selection 'tests 'unresolved))
  (define decision-required (hash-ref decision 'required 'missing))
  (unless (and (list? decision-required) (andmap string? decision-required))
    (problem! "recorded decision required set is not a list of paths"))
  (when (and (list? required)
             (list? decision-required)
             (not (equal? (sorted-unique required) (sorted-unique decision-required))))
    (problem! "recorded selection required set differs from the recorded decision's"))
  (unless (memq verdict '(covered escalate))
    (problem! (format "unknown recorded verdict ~a" verdict)))
  (unless (list? escalations)
    (problem! "recorded escalations is not a list"))
  (cond
    [(eq? instruction 'run-required)
     (unless (eq? verdict 'covered)
       (problem! "run-required recorded against an escalated verdict"))
     (unless (and (pair? required) (pair? tests) (equal? required tests))
       (problem! "run-required must record exactly the required set"))]
    [(eq? instruction 'run-comprehensive)
     (unless (eq? verdict 'escalate)
       (problem! "run-comprehensive recorded against a covered verdict"))
     (when (and (list? required) (list? tests) (list? comprehensive-files))
       (define expected (sorted-unique (append required comprehensive-files)))
       (unless (equal? (sorted-unique tests) expected)
         (problem! "recorded selected set is not required union comprehensive")))]
    [(memq instruction '(run-all unavailable)) (void)]
    [else (problem! (format "unknown recorded instruction ~a" instruction))])
  (unless (eq? (layer-instruction layer) instruction)
    (problem! (format "instruction does not follow from the record (~a)" (layer-instruction layer)))))

(define (layer-record-valid? layer comprehensive-files)
  (null? (validate-layer-record layer comprehensive-files)))

;; ---- child process mode ------------------------------------------------------------

(define (child-main)
  (define root #f)
  (define contract-module #f)
  (define changed "")
  (define comprehensive-file #f)
  (command-line
   #:program "contract-layer.rkt --evaluate"
   #:once-each ["--evaluate" "evaluation mode (internal)" (void)]
   ["--root" path "tree whose graph/manifest/changed files are read" (set! root path)]
   ["--contract-module" path "contract-escalation.rkt to load" (set! contract-module path)]
   ["--changed" files "comma-separated repo-relative changed files" (set! changed files)]
   ["--comprehensive-file"
    path
    "JSON array of the comprehensive enumeration"
    (set! comprehensive-file path)]
   #:args ()
   (unless (and root contract-module comprehensive-file)
     (raise-user-error 'contract-layer
                       "--root, --contract-module and --comprehensive-file are required"))
   (define changed-list
     (if (equal? changed "")
         '()
         (string-split changed ",")))
   (define comprehensive
     (map (lambda (v)
            (if (string? v)
                v
                (format "~a" v)))
          (with-input-from-file comprehensive-file read-json)))
   (define payload
     (with-handlers
         ([exn:fail?
           (lambda (e)
             (hash
              'decision
              #f
              'selection
              (hash
               'instruction
               "run-all"
               'tests
               "unresolved"
               'required
               "unresolved"
               'escalations
               (list (hash 'code "selection-failed" 'module #f 'detail (exn-message e) 'tests '())))
              'error
              (exn-message e)))])
       (define module-path (string->path contract-module))
       (define evaluate (dynamic-require module-path 'evaluate-contract-changes))
       (define bridge (dynamic-require module-path 'contract-selection))
       (define to-json (dynamic-require module-path 'contract-decision->jsexpr))
       (define decision (evaluate (string->path root) changed-list))
       (define selection (bridge decision #:comprehensive comprehensive))
       (hash 'decision
             (to-json decision)
             'selection
             (hash 'instruction
                   (symbol->string (hash-ref selection 'instruction))
                   'tests
                   (let ([v (hash-ref selection 'tests)]) (if (eq? v 'unresolved) "unresolved" v))
                   'required
                   (let ([v (hash-ref selection 'required)]) (if (eq? v 'unresolved) "unresolved" v)))
             'error
             #f)))
   (write-json payload)
   (newline)))

(module+ main
  (child-main))
