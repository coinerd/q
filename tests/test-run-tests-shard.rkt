#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: unit

;; Test shard support and platform-cross classifier.
;; CI Acceleration W0 — PR gate ≤ 20 min.

(require rackunit
         rackunit/text-ui)

(require (only-in "../scripts/run-tests/classify.rkt" collect-test-files platform-file? shard-files)
         (only-in "../scripts/run-tests/cli.rkt" known-suites))

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
      (check-true (pair? (collect-test-files 'platform))))))

(run-tests shard-suite)
