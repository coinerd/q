#lang racket
;; @suite testing
;; @speed fast
;; Protection-policy parsing goldens for the wave-finalizer port: each case below
;; was executed once against the Python original
;; (`scripts/github/wave-finalizer.py` at revision
;; 0d9789c6b7120c7f9528fc784094e2f8eaf27447, via
;; `scripts/github/test_wave_finalizer.py`; both deleted now) and its observed
;; outcome is the `policy` group of tests/fixtures/wave-finalizer-goldens.json,
;; read through tests/helpers/wave-finalizer-goldens.rkt.  No python3 needed.
(require rackunit
         racket/runtime-path
         json
         "helpers/wave-finalizer-goldens.rkt"
         "../scripts/github/wave-finalizer.rkt")

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
  (define theirs (golden-outcomes 'policy cases))
  (for ([value (in-list cases)]
        [index (in-naturals)]
        [expected (in-list theirs)])
    (test-case (format "protection differential ~a" index)
      (check-equal? (native-policy value) expected))))
