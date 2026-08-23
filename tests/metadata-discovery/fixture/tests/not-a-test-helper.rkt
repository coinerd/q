;; @not-test #t
;; Fixture: helper module explicitly marked @not-test. MUST NOT be
;; discovered in any invocation mode (not-test exclusion). Undiscovered also
;; means the repo-wide metadata lint never validates it, so the non-schema
;; tag @not-test cannot trip the enforced lint.
(module helper racket/base
  (provide helper-thing)
  (define (helper-thing) 42))
