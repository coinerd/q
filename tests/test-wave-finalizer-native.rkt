#lang racket
;; @suite testing
;; @speed fast
(require rackunit
         racket/runtime-path
         json
         "../scripts/github/wave-finalizer.rkt")

(define-runtime-path github-dir "../scripts/github")
(define python-code
  (string-append "import json,sys,test_wave_finalizer as t\n"
                 "try:\n"
                 " p=t.wf._parse_policy(json.load(sys.stdin))\n"
                 " print(json.dumps({'contexts':p.required_contexts,'app_ids':p.app_ids}))\n"
                 "except t.wf.FinalizerError as e:\n"
                 " print(json.dumps({'error':type(e).__name__}))\n"))
(define (python-policy value)
  (parameterize ([current-directory github-dir])
    (define-values (proc out in err)
      (subprocess #f #f #f (find-executable-path "python3") "-c" python-code))
    (write-json value in)
    (close-output-port in)
    (define result (read-json out))
    (define errors (port->string err))
    (subprocess-wait proc)
    (check-equal? (subprocess-status proc) 0 errors)
    result))
(define (native-policy value)
  (with-handlers ([exn:fail:finalizer? (lambda (e) (hasheq 'error (exn:fail:finalizer-kind e)))])
    (define policy (parse-protection-policy value))
    (hasheq 'contexts
            (protection-policy-required-contexts policy)
            'app_ids
            (map (lambda (entry) (list (car entry) (cdr entry)))
                 (protection-policy-app-ids policy)))))

(define valid
  (hash 'required_status_checks
        (hash 'strict #t 'checks (list (hash 'context "ci" 'app_id 42)) 'contexts '())
        'enforce_admins
        (hash 'enabled #t)
        'required_linear_history
        (hash 'enabled #t)
        'allow_force_pushes
        (hash 'enabled #f)
        'allow_deletions
        (hash 'enabled #f)))
(define required (hash-ref valid 'required_status_checks))
(define (with-required key value)
  (hash-set valid 'required_status_checks (hash-set required key value)))
(define cases
  (list
   valid
   (with-required
    'checks
    (list (hash 'context "z" 'app_id 9) (hash 'context "a" 'app_id 2) (hash 'context "z" 'app_id 9)))
   (hash-set valid
             'required_status_checks
             (hash 'strict
                   #t
                   'contexts
                   '("z" "a" "z")
                   'checks
                   (list (hash 'context "z" 'app_id 9) (hash 'context "a" 'app_id 2))))
   (hash-set valid 'enforce_admins (hash 'enabled #f))
   (hash-set valid 'allow_force_pushes (hash 'enabled #t))
   (hash-set valid 'allow_deletions (hash 'enabled #t))
   (hash-set valid 'required_linear_history (hash 'enabled #f))
   (hash-set valid 'enforce_admins (hash 'enabled 1))
   (hash-remove valid 'required_status_checks)
   (hash-set valid 'required_status_checks 'null)
   (with-required 'strict #f)
   (with-required 'strict 'null)
   (with-required 'contexts 'null)
   (with-required 'checks 'null)
   (with-required 'checks '())
   (with-required 'contexts '("other"))
   (with-required 'contexts '(""))
   (with-required 'contexts '(42))
   (with-required 'checks (list (hash 'context "ci" 'app_id 'null)))
   (with-required 'checks (list (hash 'context "ci" 'app_id #t)))
   (with-required 'checks (list (hash 'context "ci" 'app_id 0)))
   (with-required 'checks (list (hash 'context "" 'app_id 42)))
   (with-required 'checks (list (hash 'context 1 'app_id 42)))
   (with-required 'checks (list (hash 'context "ci")))
   (with-required 'checks (list (hash 'context "ci" 'app_id 42 'extra 1)))
   (with-required 'checks (list (hash 'context "ci" 'app_id 42) (hash 'context "ci" 'app_id 43)))))
(module+ test
  (test-case "parsed context strings are immutable snapshots"
    (define legacy (string-copy "legacy"))
    (define check-context (string-copy "check"))
    (define matching (string-copy "legacy"))
    (define input
      (hash-set valid
                'required_status_checks
                (hash 'strict
                      #t
                      'contexts
                      (list legacy)
                      'checks
                      (list (hash 'context check-context 'app_id 7)
                            (hash 'context matching 'app_id 8)))))
    (define policy (parse-protection-policy input))
    (string-set! legacy 0 #\X)
    (string-set! check-context 0 #\X)
    (string-set! matching 0 #\X)
    (check-equal? (protection-policy-required-contexts policy) '("check" "legacy"))
    (check-equal? (protection-policy-app-ids policy) '(("check" . 7) ("legacy" . 8)))
    (for ([context (in-list (append (protection-policy-required-contexts policy)
                                    (map car (protection-policy-app-ids policy))))])
      (check-true (immutable? context))
      (check-exn exn:fail:contract? (lambda () (string-set! context 0 #\X)))))
  (for ([value (in-list cases)]
        [index (in-naturals)])
    (test-case (format "protection differential ~a" index)
      (check-equal? (native-policy value) (python-policy value)))))
