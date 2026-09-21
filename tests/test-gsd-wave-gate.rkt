#lang racket

;; @speed fast
;; @suite default
;; @boundary unit

;; Tests for scripts/gsd-wave-gate.rkt — the schema-2 evidence gate.
;; Covers the W2 hardening: register F12 (sentinel/placeholder evidence is
;; refused structurally, with substantive minimum content) and register F3
;; (evidence/review heads bound to the durable receipt head), both directions,
;; plus historical compatibility (an acceptance-less trio still validates).

(require rackunit
         racket/list
         racket/port
         racket/string)

(define gate-path "../scripts/gsd-wave-gate.rkt")
(require (file "../scripts/gsd-wave-gate.rkt"))

;; 40-hex / 64-hex fixture identities (syntactically valid, semantically inert).
(define impl-sha (make-string 40 #\a))
(define digest-sha (make-string 64 #\b))
(define receipt-head-sha (make-string 40 #\c))
(define other-sha (make-string 40 #\d))

(define (hexpairs pairs)
  (format "#hasheq(~a)"
          (string-join (map (lambda (p) (format "(~a . ~a)" (car p) (cdr p))) pairs) " ")))

(define (escaped s)
  (format "~s" s))

(define policy-names '("lint" "test (0)" "workflows (0)"))

(define review-scope
  "Full read-only verification of the wave implementation, tests, gate evidence and provenance chain")
(define review-report
  "Independent review rounds found blocking defects, fixes landed at their own heads, and the verdict was approved at the exact reviewed head")

(define passed-gate
  "#hasheq((result . \"passed\") (command . \"racket scripts/run-tests.rkt --suite fast\"))")

(define (trio-texts #:impl [impl impl-sha]
                    #:digest [digest digest-sha]
                    #:reviewer [reviewer "independent reviewer (fresh non-author, read-only)"]
                    #:timestamp [timestamp "2026-09-21T08:26:05Z"]
                    #:scope [scope review-scope]
                    #:report [report review-report]
                    #:reviewed [reviewed impl]
                    #:red-command [red-command "raco test tests/test-gsd-wave-gate.rkt"]
                    #:red-failure
                    [red-failure
                     "refusal absent: the malformed digest passed the gate before the fix"]
                    #:remaining [remaining '()])
  (define evidence
    (hexpairs (list (cons 'schema-version '2)
                    (cons 'milestone '896)
                    (cons 'wave "\"W2\"")
                    (cons 'issue '9725)
                    (cons 'status "\"ready-for-merge\"")
                    (cons 'implementation-sha (escaped impl))
                    (cons 'content-digest (escaped digest))
                    (cons 'required-checks "(\"lint\" \"test (0)\" \"workflows (0)\")")
                    (cons 'review-artifact "\"reviews/w2.rktd\"")
                    (cons 'validation-artifact "\"validation/w2.rktd\""))))
  (define review
    (hexpairs (list (cons 'reviewer (escaped reviewer))
                    (cons 'verdict "\"APPROVED\"")
                    (cons 'timestamp (escaped timestamp))
                    (cons 'reviewed-sha (escaped reviewed))
                    (cons 'content-digest (escaped digest))
                    (cons 'scope (escaped scope))
                    (cons 'report (escaped report)))))
  (define validation
    (hexpairs
     (append
      (list (cons 'status "\"current\"")
            (cons 'milestone '896)
            (cons 'wave "\"W2\"")
            (cons 'issue '9725)
            (cons 'branch "\"campaign/v1.00.31-w2\"")
            (cons 'implementation-sha (escaped impl))
            (cons 'content-digest (escaped digest))
            (cons 'red-first
                  (format "#hasheq((command . ~a) (failure . ~a))"
                          (escaped red-command)
                          (escaped red-failure)))
            (cons 'focused-tests passed-gate)
            (cons 'format-compile passed-gate)
            (cons 'lint passed-gate)
            (cons 'fast passed-gate)
            (cons 'review-artifact "\"reviews/w2.rktd\"")
            (cons 'remaining-items
                  (if (null? remaining)
                      "()"
                      (string-append
                       "("
                       (string-join (for/list ([item remaining])
                                      (hexpairs (list (cons 'classification "\"noncritical\"")
                                                      (cons 'owner (escaped (hash-ref item 'owner)))
                                                      (cons 'rationale
                                                            (escaped (hash-ref item 'rationale))))))
                                    " ")
                       ")")))
            (cons 'planning-sync "\"current\"")))))
  (hasheq 'evidence evidence 'review review 'validation validation))

(define (make-fixture-root! texts)
  (define root (make-temporary-file "q-w2-gate-~a" 'directory))
  (define (write-record relative text)
    (define path (build-path root relative))
    (make-directory* (path-only path))
    (display-to-file (string-append text "\n") path #:exists 'replace))
  (write-record "evidence/w2.rktd" (hash-ref texts 'evidence))
  (write-record "reviews/w2.rktd" (hash-ref texts 'review))
  (write-record "validation/w2.rktd" (hash-ref texts 'validation))
  (make-directory* (build-path root "scripts"))
  (with-output-to-file (build-path root "scripts/required-pr-checks.policy")
                       (lambda () (displayln "(\"lint\" \"test (0)\" \"workflows (0)\")"))
                       #:exists 'replace)
  root)

(define (validate-at-root root texts #:digest [digest digest-sha] #:receipt-head [receipt-head #f])
  (validate-wave-evidence (read-single-datum-file (build-path root "evidence/w2.rktd"))
                          #:root root
                          #:actual-content-digest digest
                          #:receipt-head receipt-head))

(require racket/file)
(define (read-single-datum-file path)
  (call-with-input-file path read))

(define baseline (trio-texts))
(define baseline-root (make-fixture-root! baseline))

(test-case "historical compatibility: a complete acceptance-less trio passes"
  (define result (validate-at-root baseline-root baseline))
  (check-true (wave-evidence-result-passed? result)
              (string-join (wave-evidence-result-reasons result) "; ")))

(test-case "F3 head binding: evidence and review heads must equal the receipt head"
  (define control (validate-at-root baseline-root baseline #:receipt-head impl-sha))
  (check-true (wave-evidence-result-passed? control)
              "control: receipt-head equal to implementation-sha passes")
  (check-false (wave-evidence-result-passed?
                (validate-at-root baseline-root (trio-texts) #:receipt-head other-sha))
               "defect: a differing receipt head is refused")
  (define reasons
    (wave-evidence-result-reasons
     (validate-at-root baseline-root (trio-texts) #:receipt-head other-sha)))
  (check-true (and (findf (lambda (r) (string-contains? r "head-binding-mismatch")) reasons) #t)
              (string-join reasons "; "))
  (check-true (and (findf (lambda (r) (string-contains? r other-sha)) reasons) #t)
              "the typed refusal names both SHAs")
  (check-true (and (findf (lambda (r) (string-contains? r impl-sha)) reasons) #t)
              "the typed refusal names the recorded head too"))

(test-case "F3 head binding: reviewed-sha must equal the receipt head"
  (define texts (trio-texts #:reviewed other-sha))
  (check-false (wave-evidence-result-passed?
                (validate-wave-evidence (read-single-datum-file (build-path baseline-root
                                                                            "evidence/w2.rktd"))
                                        #:root baseline-root
                                        #:receipt-head receipt-head-sha))
               "a review bound to a foreign head is refused even before identity checks"))

(define placeholder-values
  (list "PENDING" "pending" "TODO" "TBD" "PLACEHOLDER" "FIXME" "N/A" "n/a" "TBA" "PENDING." "todo!"))

(test-case "F12 sentinel refusal: reviewer identity"
  (for ([p placeholder-values])
    (define texts (trio-texts #:reviewer p))
    (define root (make-fixture-root! texts))
    (define result (validate-at-root root texts))
    (check-false (wave-evidence-result-passed? result) (format "~a must be refused" p))
    (check-true (and (findf (lambda (r) (string-contains? r "placeholder-evidence"))
                            (wave-evidence-result-reasons result))
                     #t)
                (format "~a refusal is typed" p))))

(test-case "F12 sentinel refusal: timestamp, scope, report, red-first"
  (for ([field '(timestamp scope report)])
    (for ([p (list "PENDING" "TODO" "TBD")])
      (define texts
        (trio-texts #:timestamp (if (eq? field 'timestamp) p "2026-09-21T08:26:05Z")
                    #:scope (if (eq? field 'scope) p review-scope)
                    #:report (if (eq? field 'report) p review-report)))
      (define root (make-fixture-root! texts))
      (define result (validate-at-root root texts))
      (check-false (wave-evidence-result-passed? result) (format "~a=~a" field p))
      (check-true (and (findf (lambda (r) (string-contains? r "placeholder-evidence"))
                              (wave-evidence-result-reasons result))
                       #t)
                  (format "~a=~a typed" field p))))
  (for ([p (list "PENDING" "y" "TODO")])
    (define texts (trio-texts #:red-failure p))
    (define root (make-fixture-root! texts))
    (define result (validate-at-root root texts))
    (check-false (wave-evidence-result-passed? result) (format "red-failure=~a" p))
    (check-true (and (or (findf (lambda (r) (string-contains? r "placeholder-evidence"))
                                (wave-evidence-result-reasons result))
                         (findf (lambda (r) (string-contains? r "insufficient-review-content"))
                                (wave-evidence-result-reasons result)))
                     #t)
                (format "red-failure=~a typed" p))))

(test-case "F12 minimum content: narrative fields need substance"
  (for ([p (list "ok" "done" "looks fine to me")])
    (define texts (trio-texts #:scope p))
    (define root (make-fixture-root! texts))
    (define result (validate-at-root root texts))
    (check-false (wave-evidence-result-passed? result) (format "scope=~a must be refused" p))
    (check-true (and (findf (lambda (r) (string-contains? r "insufficient-review-content"))
                            (wave-evidence-result-reasons result))
                     #t)
                (format "scope=~a typed" p)))
  (define short-report (trio-texts #:report "approved"))
  (check-false (wave-evidence-result-passed? (validate-at-root (make-fixture-root! short-report)
                                                               short-report))
               "short report refused")
  (define short-red (trio-texts #:red-failure "failed"))
  (check-false (wave-evidence-result-passed? (validate-at-root (make-fixture-root! short-red)
                                                               short-red))
               "short red-first failure refused"))

(test-case "F12 both directions: the same draft with real content passes"
  (for ([p placeholder-values])
    (define texts (trio-texts #:reviewer p))
    (define root (make-fixture-root! texts))
    (check-false (wave-evidence-result-passed? (validate-at-root root texts))
                 (format "sentinel ~a refused" p)))
  (define real (trio-texts))
  (check-true (wave-evidence-result-passed? (validate-at-root (make-fixture-root! real) real))
              (string-join (wave-evidence-result-reasons (validate-at-root baseline-root baseline))
                           "; ")))

(test-case "F12 remaining-items owner and rationale refuse sentinels"
  (define texts
    (trio-texts #:remaining (list (hasheq 'owner
                                          "PENDING"
                                          'rationale
                                          "deferred to the hardening wave with owner agreement"))))
  (define root (make-fixture-root! texts))
  (define result (validate-at-root root texts))
  (check-false (wave-evidence-result-passed? result))
  (check-true (and (findf (lambda (r) (string-contains? r "placeholder-evidence"))
                          (wave-evidence-result-reasons result))
                   #t)
              (string-join (wave-evidence-result-reasons result) "; ")))

(test-case "CLI: --receipt-head is enforced fail-closed"
  (parameterize ([current-directory baseline-root])
    (define out (open-output-string))
    (define code
      (parameterize ([current-output-port out])
        (main (list "evidence/w2.rktd" "--content-digest" digest-sha "--receipt-head" other-sha))))
    (check-equal? code 1)
    (check-true (string-contains? (get-output-string out) "head-binding-mismatch"))
    (define out2 (open-output-string))
    (define code2
      (parameterize ([current-output-port out2])
        (main (list "evidence/w2.rktd" "--content-digest" digest-sha "--receipt-head" impl-sha))))
    (check-equal? code2 0)
    (check-true (string-contains? (get-output-string out2) "PASS"))))

(test-case "CLI: malformed usage fails closed"
  (check-equal? (parameterize ([current-output-port (open-output-string)])
                  (main (list "x" "--content-digest" "y" "--bogus" "z")))
                1)
  (check-equal? (parameterize ([current-output-port (open-output-string)])
                  (main (list)))
                1))
