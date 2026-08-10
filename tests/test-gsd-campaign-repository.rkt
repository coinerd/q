#lang racket/base

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-campaign-repository.rkt — v0.99.90 W1: Campaign repository
;; boundary (#9232).
;;
;; TDD red tests for the .rktd storage boundary:
;;   1. Legacy fixtures load compatibly (backward-compatible loader).
;;   2. Corruption matrix fails closed (exn:fail:campaign-corrupt).
;;   3. Path containment: plan-id is the only input-derived path component;
;;      non-hex/traversal plan-ids are rejected before touching the FS.
;;   4. No-follow: symlinked .rktd targets rejected on read and write.
;;   5. Atomic replace: overwrite works, failures leave no tmp leftovers.
;;   6. Fencing/attempt identity round-trip.
;;   7. load-or-migrate-campaign! composes migrate-seed + load; a corrupted
;;      existing record fails closed instead of silently re-migrating.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         "../extensions/gsd/campaign-state.rkt"
         "../extensions/gsd/campaign-repository.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-test-record n #:plan-id [pid #f])
  (define m
    (make-campaign-manifest 1
                            "Test Campaign"
                            '()
                            (for/list ([i (in-range n)])
                              (make-campaign-wave-descriptor i
                                                             (format "Wave ~a" i)
                                                             (format "waves/W~a-test.md" i)
                                                             (format "hash-~a" i)))
                            "constraints-hash"))
  (make-campaign-record (or pid (campaign-manifest-hash m))
                        m
                        (for/list ([i (in-range n)])
                          (make-campaign-wave i (format "Wave ~a" i) 'pending 0 #f))
                        #f
                        0
                        #f
                        (current-seconds)
                        (current-seconds)))

;; Write a raw datum to the campaigns dir as <plan-id>.rktd (legacy fixture).
(define (write-fixture! dir plan-id datum)
  (define campaigns-dir (build-path dir ".planning" "campaigns"))
  (make-directory* campaigns-dir)
  (call-with-output-file (build-path campaigns-dir (string-append plan-id ".rktd"))
                         (lambda (out) (write datum out))
                         #:exists 'truncate))

;; ============================================================
;; 1. Backward compatibility: legacy fixtures load
;; ============================================================

(define compat-suite
  (test-suite "legacy fixture compatibility"
    (test-case "v1 fixture with full fields loads compatibly"
      (define dir (make-temporary-file "repo-compat-~a" 'directory))
      (define plan-id "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
      (define m
        (make-campaign-manifest 1
                                "Legacy"
                                '()
                                (list (make-campaign-wave-descriptor 0 "W0" "waves/W0.md" "h0"))
                                "constraints"))
      (define real-id (campaign-manifest-hash m))
      (write-fixture!
       dir
       real-id
       (list 'campaign-record
             real-id
             (list 'manifest 1 "Legacy" '() (list (list 0 "W0" "waves/W0.md" "h0")) "constraints")
             (list (list 0 "W0" 'done 2 (list "attempt-2" 7 1234)))
             (list 'cancellation "operator" 99)
             42
             'plan-and-state
             1000
             2000))
      (define rec (load-campaign-record dir real-id))
      (check-not-false rec)
      (check-equal? (campaign-plan-id rec) real-id)
      (check-equal? (campaign-manifest-title (campaign-record-manifest rec)) "Legacy")
      (check-equal? (campaign-fence-token rec) 42)
      (check-equal? (campaign-record-provenance rec) 'plan-and-state)
      (check-equal? (campaign-record-created-at rec) 1000)
      (check-equal? (campaign-record-updated-at rec) 2000)
      (check-true (campaign-cancellation? (campaign-record-cancellation rec)))
      (check-equal? (campaign-cancellation-reason (campaign-record-cancellation rec)) "operator")
      (define w (list-ref (campaign-record-waves rec) 0))
      (check-eq? (campaign-wave-status w) 'done)
      (check-equal? (campaign-wave-attempt-count w) 2)
      (check-not-false (campaign-wave-current-attempt w))
      (check-equal? (campaign-attempt-id (campaign-wave-current-attempt w)) "attempt-2")
      (check-equal? (campaign-attempt-fence-token (campaign-wave-current-attempt w)) 7)
      (check-equal? (campaign-attempt-started-at (campaign-wave-current-attempt w)) 1234)
      (delete-directory/files dir #:must-exist? #f))

    (test-case "persist then load round-trips every field"
      (define dir (make-temporary-file "repo-roundtrip-~a" 'directory))
      (define rec (make-test-record 2))
      (begin-attempt! rec 0 7)
      (set-campaign-fence-token! rec 42)
      (set-campaign-cancellation! rec (make-campaign-cancellation "operator" 12345))
      (persist-campaign! dir rec)
      (define loaded (load-campaign-record dir (campaign-plan-id rec)))
      (check-not-false loaded)
      (check-equal? (campaign-plan-id loaded) (campaign-plan-id rec))
      (check-equal? (campaign-fence-token loaded) 42)
      (check-equal? (campaign-cancellation-reason (campaign-record-cancellation loaded)) "operator")
      (define w (list-ref (campaign-record-waves loaded) 0))
      (check-eq? (campaign-wave-status w) 'in-progress)
      (check-equal? (campaign-wave-attempt-count w) 1)
      (check-equal? (campaign-attempt-fence-token (campaign-wave-current-attempt w)) 7)
      (delete-directory/files dir #:must-exist? #f))

    (test-case "missing file returns #f (not an error)"
      (define dir (make-temporary-file "repo-missing-~a" 'directory))
      (check-false
       (load-campaign-record dir "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"))
      (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; 2. Corruption matrix — fail closed
;; ============================================================

(define (corrupt-check dir plan-id datum)
  (write-fixture! dir plan-id datum)
  (check-exn exn:fail:campaign-corrupt?
             (lambda () (load-campaign-record dir plan-id))
             (format "corrupt fixture ~s fails closed"
                     (if (list? datum)
                         (car datum)
                         datum))))

(define corruption-suite
  (test-suite "corruption fails closed"
    (test-case "non-list datum"
      (define dir (make-temporary-file "repo-corrupt-~a" 'directory))
      (corrupt-check dir "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" 42)
      (delete-directory/files dir #:must-exist? #f))

    (test-case "wrong record tag"
      (define dir (make-temporary-file "repo-corrupt-~a" 'directory))
      (corrupt-check dir
                     "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                     (list 'bogus 1 2 3))
      (delete-directory/files dir #:must-exist? #f))

    (test-case "truncated record (missing fields)"
      (define dir (make-temporary-file "repo-corrupt-~a" 'directory))
      (corrupt-check dir
                     "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                     (list 'campaign-record "pid" 'manifest '()))
      (delete-directory/files dir #:must-exist? #f))

    (test-case "plan-id does not equal manifest hash"
      (define dir (make-temporary-file "repo-corrupt-~a" 'directory))
      (define m (make-campaign-manifest 1 "T" '() '() "c"))
      (define real-id (campaign-manifest-hash m))
      (corrupt-check
       dir
       real-id
       (list 'campaign-record real-id (list 'manifest 1 "EDITED-TITLE" '() '() "c") '() #f 0 #f 0 0))
      (delete-directory/files dir #:must-exist? #f))

    (test-case "non-canonical wave status"
      (define dir (make-temporary-file "repo-corrupt-~a" 'directory))
      (define m
        (make-campaign-manifest 1
                                "T"
                                '()
                                (list (make-campaign-wave-descriptor 0 "W" "w.md" "h"))
                                "c"))
      (define real-id (campaign-manifest-hash m))
      (corrupt-check dir
                     real-id
                     (list 'campaign-record
                           real-id
                           (list 'manifest 1 "T" '() (list (list 0 "W" "w.md" "h")) "c")
                           (list (list 0 "W" 'rework 0 #f))
                           #f
                           0
                           #f
                           0
                           0))
      (delete-directory/files dir #:must-exist? #f))

    (test-case "negative fence token"
      (define dir (make-temporary-file "repo-corrupt-~a" 'directory))
      (define m
        (make-campaign-manifest 1
                                "T"
                                '()
                                (list (make-campaign-wave-descriptor 0 "W" "w.md" "h"))
                                "c"))
      (define real-id (campaign-manifest-hash m))
      (corrupt-check dir
                     real-id
                     (list 'campaign-record
                           real-id
                           (list 'manifest 1 "T" '() (list (list 0 "W" "w.md" "h")) "c")
                           (list (list 0 "W" 'pending 0 #f))
                           #f
                           -1
                           #f
                           0
                           0))
      (delete-directory/files dir #:must-exist? #f))

    (test-case "non-string attempt id"
      (define dir (make-temporary-file "repo-corrupt-~a" 'directory))
      (define m
        (make-campaign-manifest 1
                                "T"
                                '()
                                (list (make-campaign-wave-descriptor 0 "W" "w.md" "h"))
                                "c"))
      (define real-id (campaign-manifest-hash m))
      (corrupt-check dir
                     real-id
                     (list 'campaign-record
                           real-id
                           (list 'manifest 1 "T" '() (list (list 0 "W" "w.md" "h")) "c")
                           (list (list 0 "W" 'in-progress 1 (list 123 0 5)))
                           #f
                           0
                           #f
                           0
                           0))
      (delete-directory/files dir #:must-exist? #f))

    (test-case "schema version newer than current fails closed"
      (define dir (make-temporary-file "repo-corrupt-~a" 'directory))
      (define m (make-campaign-manifest 2 "T" '() '() "c"))
      (define real-id (campaign-manifest-hash m))
      (corrupt-check
       dir
       real-id
       (list 'campaign-record real-id (list 'manifest 2 "T" '() '() "c") '() #f 0 #f 0 0))
      (delete-directory/files dir #:must-exist? #f))

    (test-case "garbage non-s-expression content"
      (define dir (make-temporary-file "repo-corrupt-~a" 'directory))
      (define plan-id "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
      (define campaigns-dir (build-path dir ".planning" "campaigns"))
      (make-directory* campaigns-dir)
      (call-with-output-file (build-path campaigns-dir (string-append plan-id ".rktd"))
                             (lambda (out) (write-bytes #"\xde\xad\xbe\xef not an sexpr (" out))
                             #:exists 'truncate)
      (check-exn exn:fail:campaign-corrupt?
                 (lambda () (load-campaign-record dir plan-id))
                 "garbage file fails closed")
      (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; 3. Path containment
;; ============================================================

(define containment-suite
  (test-suite "path containment"
    (test-case "traversal plan-id rejected on load"
      (define dir (make-temporary-file "repo-containment-~a" 'directory))
      (check-exn exn:fail:campaign-corrupt? (lambda () (load-campaign-record dir "../escape")))
      (check-exn exn:fail:campaign-corrupt? (lambda () (load-campaign-record dir "..%2f..%2fetc")))
      (check-exn exn:fail:campaign-corrupt? (lambda () (load-campaign-record dir "NOT-HEX!")))
      (delete-directory/files dir #:must-exist? #f))

    (test-case "tampered record with traversal plan-id rejected on persist"
      (define dir (make-temporary-file "repo-containment-~a" 'directory))
      (define rec (make-test-record 1 #:plan-id "../escape"))
      (check-exn exn:fail:campaign-corrupt?
                 (lambda () (persist-campaign! dir rec))
                 "persist rejects traversal plan-id before touching the FS")
      (check-false (file-exists? (build-path (build-path dir ".planning") "campaigns" "..%2f.rktd")))
      (check-false (file-exists? (build-path dir ".planning" "campaigns")))
      (delete-directory/files dir #:must-exist? #f))

    (test-case "tampered record with non-hex plan-id rejected on persist"
      (define dir (make-temporary-file "repo-containment-~a" 'directory))
      (define rec (make-test-record 1 #:plan-id "not-a-hash"))
      (check-exn exn:fail:campaign-corrupt? (lambda () (persist-campaign! dir rec)))
      (delete-directory/files dir #:must-exist? #f))

    (test-case "hex-but-mismatched plan-id rejected on persist"
      (define dir (make-temporary-file "repo-containment-~a" 'directory))
      (define rec
        (make-test-record 1
                          #:plan-id
                          "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"))
      (check-exn exn:fail:campaign-corrupt?
                 (lambda () (persist-campaign! dir rec))
                 "plan-id must equal the manifest hash even when hex-valid")
      (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; 4. No-follow: symlink targets
;; ============================================================

(define nofollow-suite
  (test-suite "symlink no-follow"
    (test-case "symlinked .rktd target fails closed on load"
      (define dir (make-temporary-file "repo-symlink-~a" 'directory))
      (define plan-id "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa")
      ;; real content elsewhere; campaigns/<plan-id>.rktd is a symlink to it
      (define outside (build-path dir "outside.rktd"))
      (call-with-output-file outside (lambda (out) (write 42 out)) #:exists 'truncate)
      (define campaigns-dir (build-path dir ".planning" "campaigns"))
      (make-directory* campaigns-dir)
      (define target (build-path campaigns-dir (string-append plan-id ".rktd")))
      (make-file-or-directory-link outside target)
      (check-exn exn:fail:campaign-corrupt?
                 (lambda () (load-campaign-record dir plan-id))
                 "symlinked target rejected (no-follow)")
      (delete-directory/files dir #:must-exist? #f))

    (test-case "persist over symlinked target fails closed"
      (define dir (make-temporary-file "repo-symlink-~a" 'directory))
      (define rec (make-test-record 1))
      (define plan-id (campaign-plan-id rec))
      (define campaigns-dir (build-path dir ".planning" "campaigns"))
      (make-directory* campaigns-dir)
      (define outside (build-path dir "outside.rktd"))
      (call-with-output-file outside (lambda (out) (write 42 out)) #:exists 'truncate)
      (define target (build-path campaigns-dir (string-append plan-id ".rktd")))
      (make-file-or-directory-link outside target)
      (check-exn exn:fail:campaign-corrupt?
                 (lambda () (persist-campaign! dir rec))
                 "persist refuses to write through a symlink")
      (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; 5. Atomic replace
;; ============================================================

(define atomic-suite
  (test-suite "atomic replace"
    (test-case "overwrite persists latest record"
      (define dir (make-temporary-file "repo-atomic-~a" 'directory))
      (define rec (make-test-record 1))
      (persist-campaign! dir rec)
      (set-campaign-wave-status! (list-ref (campaign-record-waves rec) 0) 'done)
      (set-campaign-fence-token! rec 99)
      (persist-campaign! dir rec)
      (define loaded (load-campaign-record dir (campaign-plan-id rec)))
      (check-eq? (campaign-wave-status (list-ref (campaign-record-waves loaded) 0)) 'done)
      (check-equal? (campaign-fence-token loaded) 99)
      (delete-directory/files dir #:must-exist? #f))

    (test-case "failed persist leaves no tmp leftovers"
      (define dir (make-temporary-file "repo-atomic-~a" 'directory))
      (define rec (make-test-record 1))
      (persist-campaign! dir rec)
      ;; Make the target path a directory so rename fails.
      (define campaigns-dir (build-path dir ".planning" "campaigns"))
      (define plan-id (campaign-plan-id rec))
      (delete-file (build-path campaigns-dir (string-append plan-id ".rktd")))
      (make-directory (build-path campaigns-dir (string-append plan-id ".rktd")))
      (check-exn exn:fail? (lambda () (persist-campaign! dir rec)) "persist over a directory fails")
      (define tmp-leftovers
        (for/list ([p (directory-list campaigns-dir)]
                   #:when (and (string? (path->string p)) (string-prefix? (path->string p) ".tmp-")))
          p))
      (check-equal? tmp-leftovers '() "no .tmp- files remain after failed persist")
      (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; 6. load-or-migrate-campaign!
;; ============================================================

(define (seed-plan-dir! dir)
  (make-directory (build-path dir ".planning"))
  (make-directory (build-path dir ".planning" "waves"))
  (call-with-output-file (build-path dir ".planning" "PLAN.md")
                         (lambda (out)
                           (write-string "# Plan: RepoCampaign\n\n## Waves\n" out)
                           (write-string "- [Inbox] W0: Zero → waves/W0-zero.md\n" out))
                         #:exists 'truncate)
  (call-with-output-file (build-path dir ".planning" "STATE.md")
                         (lambda (out)
                           (write-string "| Wave | Title | Status |\n|---|---|---|\n" out)
                           (write-string "| W0 | Zero | NOT STARTED |\n" out))
                         #:exists 'truncate))

(define migrate-suite
  (test-suite "load-or-migrate-campaign!"
    (test-case "fresh dir migrates, persists, and is returned"
      (define dir (make-temporary-file "repo-migrate-~a" 'directory))
      (seed-plan-dir! dir)
      (define rec (load-or-migrate-campaign! dir))
      (check-not-false rec)
      (check-eq? (campaign-record-provenance rec) 'plan-and-state)
      (check-equal? (length (campaign-record-waves rec)) 1)
      (check-true
       (file-exists?
        (build-path dir ".planning" "campaigns" (string-append (campaign-plan-id rec) ".rktd")))
       "migrate persists the seeded record")
      (delete-directory/files dir #:must-exist? #f))

    (test-case "second call returns the persisted record, not a re-migration"
      (define dir (make-temporary-file "repo-migrate-~a" 'directory))
      (seed-plan-dir! dir)
      (define rec1 (load-or-migrate-campaign! dir))
      ;; Progress made after the first call (durable).
      (set-campaign-wave-status! (list-ref (campaign-record-waves rec1) 0) 'done)
      (persist-campaign! dir rec1)
      (define rec2 (load-or-migrate-campaign! dir))
      (check-equal? (campaign-plan-id rec2) (campaign-plan-id rec1))
      (check-eq? (campaign-wave-status (list-ref (campaign-record-waves rec2) 0))
                 'done
                 "returns persisted progress instead of re-seeding pending")
      (delete-directory/files dir #:must-exist? #f))

    (test-case "corrupted existing record fails closed (no silent re-migration)"
      (define dir (make-temporary-file "repo-migrate-~a" 'directory))
      (seed-plan-dir! dir)
      (define rec1 (load-or-migrate-campaign! dir))
      ;; Corrupt the persisted file in place.
      (define campaigns-dir (build-path dir ".planning" "campaigns"))
      (call-with-output-file (build-path campaigns-dir
                                         (string-append (campaign-plan-id rec1) ".rktd"))
                             (lambda (out) (write 'garbage out))
                             #:exists 'truncate)
      (check-exn exn:fail:campaign-corrupt?
                 (lambda () (load-or-migrate-campaign! dir))
                 "corrupted durable record fails closed instead of re-migrating")
      (delete-directory/files dir #:must-exist? #f))))

;; ============================================================
;; Runner
;; ============================================================

(define repository-suite
  (test-suite "gsd-campaign-repository"
    compat-suite
    corruption-suite
    containment-suite
    nofollow-suite
    atomic-suite
    migrate-suite))

(void (run-tests repository-suite))
