#lang racket

;; @speed fast
;; @suite default
;; @boundary unit

;; BOUNDARY: unit

;; Test shard support and platform-cross classifier.
;; CI Acceleration W0 — PR gate ≤ 20 min.

(require rackunit
         rackunit/text-ui
         racket/port
         racket/runtime-path
         racket/string)

(require (only-in "../scripts/run-tests/classify.rkt" collect-test-files platform-file? shard-files)
         (only-in "../scripts/run-tests/cli.rkt" known-suites)
         (only-in "../scripts/run-tests/inventory.rkt" compute-inventory-hash selected-paths-digest))

(define-runtime-path runner-script "../scripts/run-tests.rkt")

(define shard-suite
  (test-suite "Shard and platform tests"

    (test-case "shard 0/1 selects all files"
      (check-equal? (length (shard-files '("a" "b" "c" "d" "e") 0 1)) 5))

    (test-case "shard 0/3 selects correct subset"
      (check-equal? (shard-files '("a" "b" "c" "d" "e" "f") 0 3) '("a" "d")))

    (test-case "shard 1/3 selects correct subset"
      (check-equal? (shard-files '("a" "b" "c" "d" "e" "f") 1 3) '("b" "e")))

    (test-case "shard 2/3 selects correct subset"
      (check-equal? (shard-files '("a" "b" "c" "d" "e" "f") 2 3) '("c" "f")))

    (test-case "three shards union = full set"
      (define files '("a" "b" "c" "d" "e" "f" "g"))
      (define union
        (sort (append (shard-files files 0 3) (shard-files files 1 3) (shard-files files 2 3))
              string<?))
      (check-equal? union (sort files string<?)))

    (test-case "shard 0/1 = identity"
      (check-equal? (shard-files '("x" "y") 0 1) '("x" "y")))

    (test-case "shard-total 0 is rejected"
      (check-exn exn:fail? (lambda () (shard-files '("a") 0 0))))

    (test-case "shard-index >= shard-total is rejected"
      (check-exn exn:fail? (lambda () (shard-files '("a") 3 3))))

    (test-case "empty files with valid shard is empty"
      (check-equal? (shard-files '() 0 3) '()))

    (test-case "subprocess test is platform-file"
      (check-true (platform-file? "tests/test-subprocess.rkt")))

    (test-case "cwd-independence test is platform-file"
      (check-true (platform-file? "tests/test-cwd-independence.rkt")))

    (test-case "version test is platform-file (curated)"
      (check-true (platform-file? "tests/test-version.rkt")))

    (test-case "non-curated test is NOT platform-file"
      (check-false (platform-file? "tests/test-something-not-in-list.rkt")))

    (test-case "platform is a known suite"
      (check-true (and (member 'platform known-suites) #t)))

    (test-case "platform suite collects non-empty file list"
      (check-true (pair? (collect-test-files 'platform))))

    ;; ---------------------------------------------------------------------
    ;; v1.00.24 W3: stable inventory identity for gate evidence.
    ;; equal-hash-code is randomized per Racket process; the selected
    ;; inventory identity must be the SHA-256 selected-path digest so that
    ;; recorded evidence can be re-derived and compared after the fact.
    ;; ---------------------------------------------------------------------

    (test-case "inventory hash is a stable SHA-256 selected-path digest"
      (define files '("tests/a.rkt" "tests/b.rkt" "tests/c.rkt"))
      ;; Full 64-hex SHA-256, not a process-dependent hash code.
      (check-true (regexp-match? #px"^[0-9a-f]{64}$" (compute-inventory-hash files)))
      ;; Same canonical digest as the existing selected-path digest.
      (check-equal? (compute-inventory-hash files) (selected-paths-digest files))
      ;; Canonical over the sorted, de-duplicated path set.
      (check-equal? (compute-inventory-hash files) (compute-inventory-hash (reverse files)))
      (check-equal? (compute-inventory-hash files) (compute-inventory-hash (append files files)))
      (check-not-equal? (compute-inventory-hash files)
                        (compute-inventory-hash '("tests/a.rkt" "tests/b.rkt" "tests/d.rkt"))))

    (test-case "a shard's inventory digest never equals the full-suite digest"
      (define files
        (for/list ([i (in-range 6)])
          (format "tests/shard-fixture-~a.rkt" i)))
      (define full-digest (compute-inventory-hash files))
      (for ([idx (in-range 3)])
        (check-not-equal? (compute-inventory-hash (shard-files files idx 3)) full-digest)))

    (test-case "runner refuses --record-gate-evidence on sharded runs (fail closed)"
      (define racket-bin (find-executable-path "racket"))
      (define-values (sp out in err)
        (subprocess #f
                    #f
                    #f
                    racket-bin
                    runner-script
                    "--suite"
                    "smoke"
                    "--record-gate-evidence"
                    "--shard-index"
                    "0"
                    "--shard-total"
                    "2"))
      (close-output-port in)
      (define done (sync/timeout 600 sp))
      (unless done
        (subprocess-kill sp #t)
        (fail "runner did not exit before the 600s timeout"))
      (define stdout-text (port->string out))
      (define stderr-text (port->string err))
      (close-input-port out)
      (close-input-port err)
      (check-not-equal? (subprocess-status sp) 0)
      (check-true (string-contains? stderr-text "shard"))
      ;; Refusal happens before any run: no RUN-SUMMARY, no PASS record.
      (check-false (string-contains? stdout-text "RUN-SUMMARY")))))

(run-tests shard-suite)
