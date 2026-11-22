#lang racket/base

;; scripts/cl5/t2-controls.rkt — positive/negative controls for the T2 oracle
;; (CL5 #9982 W1). Runs the REAL t2-verdict against a real checkout: a valid
;; row-pinning test-case must PASS, and a null/irrelevant addition must FAIL.
;;
;; This is a control, not a trial: the patches are fixed fixtures written by
;; this script (never by an agent), the tree is restored afterwards, and no
;; model is involved. It exists so the oracle's verdict is demonstrated on the
;; actual pinned baseline rather than only on synthetic trees.
;;
;; Usage:
;;   racket scripts/cl5/t2-controls.rkt --worktree PATH --revision SHA
;;
;; The worktree may be a disposable checkout at <revision>; the script never
;; leaves it modified (each patch is applied and restored in dynamic-wind) and
;; it refuses a dirty tree.

(require json
         racket/cmdline
         racket/file
         racket/string
         "oracle.rkt")

(provide control-variants
         run-controls
         valid-addition
         null-addition)

(define test-rel "tests/test-gsd-delivery-stop-decision.rkt")

;; A VALID addition: another pinned behavior of the review-infra row, which
;; M-STOP-1 mutates (so the case must fail under the mutation and pass clean).
(define valid-addition
  (string-append
   "(test-case \"review-infra keeps an explicit reason under a preflight fallback\"\n"
   "  (check-equal? (delivery-stop-outcome 'review-infra\n"
   "                                       (hasheq 'reason \"explicit reviewer failure\")\n"
   "                                       #:reason-fallback \"delivery preflight refused\")\n"
   "                (delivery-outcome 'review-infra \"explicit reviewer failure\")))\n"))

;; A NULL addition: true but unrelated to the mutated classification, so it
;; cannot be credited as the detecting regression test.
(define null-addition
  (string-append
   "(test-case \"the outcome constructor keeps its kind\"\n"
   "  (check-equal? (delivery-outcome-kind (delivery-stop-outcome 'blocked (hasheq 'reason \"x\")))\n"
   "                'blocked))\n"))

(define (control-variants)
  (list (hash 'name "valid-row-pinning" 'addition valid-addition 'expected 'pass)
        (hash 'name "null-unrelated" 'addition null-addition 'expected 'fail)))

(define (git-clean? worktree #:timeout-ms [timeout-ms 120000])
  (define-values (exit-code stdout _stderr _timed-out?)
    (run-captured (list "git" "status" "--porcelain")
                  #:cwd worktree
                  #:timeout-ms timeout-ms))
  (and (equal? exit-code 0) (equal? (string-trim stdout) "")))

;; Run one variant: append the fixture to the owner test file, grade, restore.
(define (run-variant worktree revision variant #:timeout-ms [timeout-ms 600000])
  (define test-path (build-path (string->path worktree) test-rel))
  (define original (file->string test-path))
  (define result
    (dynamic-wind (lambda ()
                    (call-with-output-file
                     test-path
                     (lambda (out)
                       (display (string-append original "\n" (hash-ref variant 'addition)) out))
                     #:exists 'truncate/replace))
                  (lambda () (t2-verdict (string->path worktree) #:revision revision #:timeout-ms timeout-ms))
                  (lambda ()
                    (call-with-output-file test-path
                                           (lambda (out) (display original out))
                                           #:exists 'truncate/replace))))
  (hash 'name
        (hash-ref variant 'name)
        'expected
        (hash-ref variant 'expected)
        'verdict
        (hash-ref result 'verdict)
        'pass?
        (eq? (hash-ref result 'verdict) (hash-ref variant 'expected))
        'added-cases
        (hash-ref result 'added-cases)
        'isolation
        (hash-ref result 'added-cases-isolation)))

;; JSON conversion: verdicts and names are symbols; JSON wants strings.
(define (json-ready value)
  (cond
    [(hash? value) (for/hash ([(key val) (in-hash value)]) (values key (json-ready val)))]
    [(symbol? value) (symbol->string value)]
    [(list? value) (map json-ready value)]
    [(path? value) (path->string value)]
    [else value]))

(define (run-controls worktree revision #:timeout-ms [timeout-ms 600000])
  (define worktree-path
    (if (path? worktree) worktree (string->path worktree)))
  (unless (git-clean? worktree-path)
    (raise-arguments-error 'run-controls
                           "control worktree is dirty; refusing to patch a developer tree"
                           "worktree"
                           (path->string worktree-path)))
  (define results
    (for/list ([variant (in-list (control-variants))])
      (run-variant worktree revision variant #:timeout-ms timeout-ms)))
  (hash 'revision
        revision
        'worktree
        (path->string worktree-path)
        'controls
        results
        'all-as-expected?
        (andmap (lambda (r) (hash-ref r 'pass?)) results)))

(module+ main
  (define worktree #f)
  (define revision #f)
  (define timeout-ms 600000)
  (command-line #:program "t2-controls.rkt"
                #:once-each
                ["--worktree" path "checkout to patch (must be clean)" (set! worktree path)]
                ["--revision" sha "full revision SHA the worktree is pinned at" (set! revision sha)]
                ["--timeout-ms" ms "per-grade timeout" (set! timeout-ms (string->number ms))])
  (unless (and worktree revision)
    (raise-user-error 't2-controls "--worktree and --revision are required"))
  (define report (run-controls worktree revision #:timeout-ms timeout-ms))
  (write-json (json-ready report) (current-output-port))
  (newline))
