#lang racket
;; @speed fast
;; @suite extensions
;; @boundary unit
;; @isolation temp-dir
;; @timeout 300
;; @covers extensions/gsd/supplemental-evidence-journal.rkt
;; @covers extensions/gsd/supplemental-disposition-claim.rkt
;; @covers extensions/gsd/supplemental-evidence-store-core.rkt
;; BUG-0090 regression: cold first use of the supplemental disposition path
;; from three concurrent threads must never re-enter module instantiation.
;; The old lazy dynamic-require seam raced first-use instantiation on worker
;; threads, producing either "cannot re-define a constant" (double body
;; evaluation) or "instantiate-linklet: ... uninitialized" (claim linked
;; against a partially initialized policy). The repair makes the graph static
;; and acyclic: journal -> claim -> supplemental-evidence-store-core, so
;; instantiation happens once, at load, in dependency order. This test pins
;; the repair deterministically: cold instantiation completes, the journal's
;; disposition entry points ARE the claim module's own procedures, the lazy
;; seam and back-edge are gone from the source, and three threads entering
;; the actual disposition route observe only validation outcomes. Negative
;; controls assert the security properties did not move: configured-but-
;; invalid claims raise (never #f) and the storage primitives still refuse
;; malformed records.
(require rackunit
         racket/file
         racket/runtime-path
         "../extensions/gsd/supplemental-evidence-authority.rkt"
         "../extensions/gsd/supplemental-evidence-store.rkt"
         "../extensions/gsd/supplemental-evidence-store-core.rkt"
         "../extensions/gsd/supplemental-disposition-claim.rkt"
         "../extensions/gsd/supplemental-evidence-journal.rkt")
(define-runtime-path journal-src "../extensions/gsd/supplemental-evidence-journal.rkt")
(define-runtime-path claim-src "../extensions/gsd/supplemental-disposition-claim.rkt")
(define-runtime-path core-src "../extensions/gsd/supplemental-evidence-store-core.rkt")
;; Partial-instantiation failure signatures that must never reappear.
(define bad-signatures
  (list #rx"cannot re-define a constant" #rx"not initialized or did not init" #rx"uninitialized"
        #rx"instantiate-linklet"))
(define (instantiation-failure? message)
  (and (string? message) (for/or ([rx (in-list bad-signatures)]) (regexp-match? rx message))))
(define (check-no-instantiation-failures where results)
  (for ([result (in-vector results)])
    (when (and (eq? (car result) 'exn) (instantiation-failure? (cdr result)))
      (fail (format "~a: instantiation failure resurfaced: ~a" where (cdr result))))))
;; Run `proc` in `n` threads released together by one barrier; each outcome is
;; (cons 'ok value) or (cons 'exn message). Deterministic: no sleeps, no retry.
(define (run-in-threads n proc)
  (define results (make-vector n #f))
  (define barrier (make-semaphore))
  (define threads
    (for/list ([i (in-range n)])
      (thread
       (lambda ()
         (semaphore-wait barrier)
         (vector-set! results i (with-handlers ([exn:fail? (lambda (e) (cons 'exn (exn-message e)))])
                                  (cons 'ok (proc i))))))))
  (for ([_ (in-range n)]) (semaphore-post barrier))
  (for ([t (in-list threads)]) (thread-wait t))
  results)
(module+ test
  (test-case "cold instantiation of the acyclic chain completes without re-entry"
    ;; A fresh namespace instantiates journal -> claim -> store-core (plus the
    ;; disposition policy) exactly once, in dependency order, with no lazy
    ;; re-entry. Deliberately single-threaded: the concurrent-require race is
    ;; a Racket loader property, not this repair; the production path no
    ;; longer performs any first-use dynamic-require at all.
    (define ns (make-base-namespace))
    (define outcome
      (with-handlers ([exn:fail? (lambda (e) (cons 'exn (exn-message e)))])
        (parameterize ([current-namespace ns])
          (dynamic-require journal-src #f)
          (dynamic-require claim-src #f)
          (dynamic-require core-src #f)
          (cons 'ok (dynamic-require journal-src 'supplemental-recovery-pointer-claimed?)))))
    (check-equal? (car outcome) 'ok (format "cold instantiation failed: ~a" (cdr outcome)))
    (check-false (instantiation-failure? (cdr outcome)))
    (check-false (module-declared? (list 'submod journal-src 'disposition-store-support))
                 "the lazy disposition-store-support back-edge must stay deleted")
    ;; In the freshly instantiated namespace the journal's disposition entry
    ;; points must be the claim module's own procedures, not wrappers.
    (parameterize ([current-namespace ns])
      (check-eq? (dynamic-require journal-src 'inspect-supplemental-disposition-parent)
                 (dynamic-require claim-src 'inspect-supplemental-disposition-parent))
      (check-eq? (dynamic-require journal-src 'install-supplemental-disposition-claim!)
                 (dynamic-require claim-src 'install-supplemental-disposition-claim!))))

  (test-case "the module graph is acyclic: claim and core never import the journal"
    ;; Structural pin on the compiled import graph (BUG-0090): journal ->
    ;; claim -> core with no back-edge and a leaf core. The old claim ->
    ;; journal `disposition-store-support` submod import is what made the
    ;; lazy seam unavoidable and its first use racy.
    (define (import-files src)
      (dynamic-require src #f)
      (for*/list ([group (in-list (module->imports src))]
                  [mpi (in-list group)]
                  #:when (module-path-index? mpi))
        (define name
          (call-with-values
           (lambda () (module-path-index-resolve mpi))
           (case-lambda
             [(resolved) (and (resolved-module-path? resolved)
                              (resolved-module-path-name resolved))]
             [(path _base) (and (pair? path) (eq? (car path) 'file) (cadr path))]
             [_ #f])))
        (and (path? name) (path->string (simplify-path name)))))
    (define (imports? src suffix)
      (for/or ([file (in-list (import-files src))] #:when file)
        (string-suffix? file suffix)))
    (check-true (imports? journal-src "supplemental-disposition-claim.rkt")
                "journal must statically require the claim module")
    (check-true (imports? journal-src "supplemental-evidence-store-core.rkt")
                "journal must statically require the shared store core")
    (check-false (imports? claim-src "supplemental-evidence-journal.rkt")
                 "claim must not require the journal (no back-edge)")
    (check-true (imports? claim-src "supplemental-evidence-store-core.rkt")
                "claim must require the shared store core")
    (check-false (imports? core-src "supplemental-evidence-journal.rkt")
                 "store core must stay a leaf")
    (check-false (imports? core-src "supplemental-disposition-claim.rkt")
                 "store core must stay a leaf"))

  (test-case "journal disposition entry points ARE the claim module's implementations"
    (check-eq? inspect-supplemental-disposition-parent
               (dynamic-require claim-src 'inspect-supplemental-disposition-parent))
    (check-eq? install-supplemental-disposition-claim!
               (dynamic-require claim-src 'install-supplemental-disposition-claim!)))

  (test-case "three threads entering the disposition route observe only validation refusals"
    ;; The actual journal route (supplemental-recovery-pointer-claimed? ->
    ;; read-disposition-claim!) against an isolated private authority root
    ;; holding one sealed-but-invalid disposition claim. Every thread must
    ;; observe the exact claim validation refusal -- a configured-but-invalid
    ;; claim raises and NEVER becomes #f -- and no thread may observe an
    ;; instantiation failure, because nothing on this path loads modules.
    (define ws (make-temporary-file "bug0090-cold-cache-~a" 'directory))
    (dynamic-wind
     (lambda () #f)
     (lambda ()
       (define root (build-path ws "private"))
       (make-directory root #o700)
       (define claim-file
         (build-path root
                     "supplemental-disposition-aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.json"))
       (atomic-write! claim-file (seal (hasheq 'schema "supplemental-disposition-claim/v1")))
       (define pointer (build-path ws "pointer.json"))
       (define results
         (run-in-threads
          3
          (lambda (i)
            (parameterize ([current-supplemental-authority-root root])
              (supplemental-recovery-pointer-claimed? pointer)))))
       (check-no-instantiation-failures "disposition route race" results)
       (for ([result (in-vector results)])
         (check-equal? (car result) 'exn "invalid claim must refuse, never fall back")
         (when (eq? (car result) 'exn)
           (check-regexp-match #rx"supplemental-disposition-claim: invalid permanent disposition claim"
                               (cdr result)))))
     (lambda () (delete-directory/files ws))))

  (test-case "an empty private authority root scans to #f from three threads"
    (define ws (make-temporary-file "bug0090-cold-cache-~a" 'directory))
    (dynamic-wind
     (lambda () #f)
     (lambda ()
       (define root (build-path ws "private"))
       (make-directory root #o700)
       (define pointer (build-path ws "pointer.json"))
       (define results
         (run-in-threads
          3
          (lambda (i)
            (parameterize ([current-supplemental-authority-root root])
              (supplemental-recovery-pointer-claimed? pointer)))))
       (check-no-instantiation-failures "empty root race" results)
       (for ([result (in-vector results)])
         (check-equal? result '(ok . #f) "empty root must scan as not claimed")))
     (lambda () (delete-directory/files ws))))

  (test-case "negative control: primitives still refuse invalid claims and records"
    (define ws (make-temporary-file "bug0090-cold-cache-~a" 'directory))
    (dynamic-wind
     (lambda () #f)
     (lambda ()
       ;; Sealed but incomplete permanent recovery claim.
       (define bad-recovery (build-path ws "bad-recovery.json"))
       (atomic-write! bad-recovery (seal (hasheq 'schema "supplemental-recovery-claim/v1")))
       (check-exn #rx"supplemental-evidence-journal: invalid permanent recovery claim"
                  (lambda () (read-recovery-claim! bad-recovery)))
       ;; Sealed but incomplete permanent disposition claim (claim module).
       (define bad-disposition (build-path ws "bad-disposition.json"))
       (atomic-write! bad-disposition (seal (hasheq 'schema "supplemental-disposition-claim/v1")))
       (check-exn #rx"supplemental-disposition-claim: invalid permanent disposition claim"
                  (lambda () (read-disposition-claim! bad-disposition)))
       ;; Sealed operation-record validation stays exact.
       (check-exn #rx"supplemental-evidence-journal: invalid operation/base/grant binding"
                  (lambda () (validate-record! (hasheq) 'g 'p 't 1)))
       ;; Noncanonical recovery pointers stay refused.
       (define pointer-file (build-path ws "pointer.json"))
       (call-with-output-file pointer-file void #:exists 'truncate)
       (check-exn exn:fail? (lambda () (read-pointer! pointer-file))))
     (lambda () (delete-directory/files ws)))))
