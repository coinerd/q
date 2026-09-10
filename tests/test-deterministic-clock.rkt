#lang racket

;; @speed fast
;; @suite default
;; @boundary unit

;; tests/test-deterministic-clock.rkt — W1 deterministic clock seam
;;
;; Two responsibilities:
;;   1. Unit tests for the shared deterministic clock/sleeper helper.
;;   2. The wait-audit lint (red guard): every test file carrying a raw sleep
;;      call must have a wait-audit row with a class and a named disposition;
;;      retained canaries must carry a named behavioral reason; a new
;;      unjustified real sleep in unit-fast turns this test red.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/format
         racket/list
         racket/string
         json
         (only-in "../util/version.rkt" q-version)
         "helpers/deterministic-clock.rkt")

(define-test-suite
 deterministic-clock-suite
 ;; ============================================================
 ;; Deterministic clock/sleeper helper unit tests
 ;; ============================================================
 (test-case "fake-clock: fresh clock starts at start-ms"
   (check-equal? (fake-clock-now (make-fake-clock)) 0)
   (check-equal? (fake-clock-now (make-fake-clock #:start-ms 12345)) 12345))
 (test-case "fake-clock: requested logical delay is recorded exactly"
   (define c (make-fake-clock))
   (fake-clock-sleep! c 250 'backoff)
   (check-equal? (fake-clock-now c) 250 "logical now advances by exactly the requested delay")
   (fake-clock-sleep! c 500 'backoff)
   (check-equal? (fake-clock-now c) 750)
   (check-equal? (fake-clock-total-delay-ms c) 750))
 (test-case "fake-clock: events are labelled, ordered, and timestamped"
   (define c (make-fake-clock))
   (fake-clock-sleep! c 10 'first)
   (fake-clock-sleep! c 20 'second)
   (fake-clock-sleep! c 30 'third)
   (define evs (fake-clock-events c))
   (check-equal? (length evs) 3)
   (check-equal? (vector-ref (first evs) 0) 'first)
   (check-equal? (vector-ref (second evs) 0) 'second)
   (check-equal? (vector-ref (third evs) 0) 'third)
   (check-equal? (vector-ref (first evs) 1) 10)
   (check-equal? (vector-ref (second evs) 1) 20)
   (check-equal? (vector-ref (third evs) 1) 30)
   ;; start-of-event logical timestamps advance monotonically
   (check-true (< (vector-ref (first evs) 2) (vector-ref (second evs) 2) (vector-ref (third evs) 2))))
 (test-case "fake-clock: logical delays cost zero wall time"
   ;; 1000 nominal seconds of logical delay must complete near-instantly.
   (define c (make-fake-clock))
   (define t0 (current-milliseconds))
   (for ([i (in-range 1000)])
     (fake-clock-sleep! c 1000 'loop))
   (define wall-ms (- (current-milliseconds) t0))
   (check-equal? (fake-clock-total-delay-ms c) 1000000 "logical total is exact")
   (check-true (< wall-ms 5000) (format "1000s of logical delay took ~ams of wall time" wall-ms)))
 (test-case "fake-clock: sleeper is a drop-in delay callable"
   ;; Simulates a retry family asserting its backoff sequence through the seam.
   (define c (make-fake-clock))
   (define sleeper (fake-clock-sleeper c 'backoff))
   (for ([d (in-list '(10 20 40))])
     (sleeper d))
   (check-equal? (map (lambda (e) (vector-ref e 1)) (fake-clock-events c)) '(10 20 40))
   (check-equal? (fake-clock-total-delay-ms c) 70)
   (check-true (andmap (lambda (e) (equal? (vector-ref e 0) 'backoff)) (fake-clock-events c))))
 (test-case "fake-clock: advance! models timeout transitions"
   (define c (make-fake-clock #:start-ms 0))
   (fake-clock-advance! c 9999)
   (check-false (>= (fake-clock-now c) 10000) "before the deadline")
   (fake-clock-advance! c 1)
   (check-true (>= (fake-clock-now c) 10000) "deadline reached with zero wall cost"))
 (test-case "fake-clock: reset! clears logical state"
   (define c (make-fake-clock))
   (fake-clock-sleep! c 100 'x)
   (fake-clock-reset! c)
   (check-equal? (fake-clock-now c) 0)
   (check-equal? (fake-clock-events c) '()))
 ;; ============================================================
 ;; Wait-audit lint (W1 red guard)
 ;; ============================================================
 (test-case "wait-audit lint: audit exists and is self-consistent"
   (define audit (call-with-input-file WAIT-AUDIT-PATH read-json))
   (check-true (hash? audit) "wait-audit.json must parse to a JSON object")
   (check-equal? (hash-ref audit 'wave) (format "v~a-w1" q-version))
   (define summary (hash-ref audit 'summary))
   (check-equal? (hash-ref summary 'unjustified-unit-fast-sleeps)
                 0
                 "no unjustified real sleeps may be recorded anywhere, least of all unit-fast")
   (define raw-files (hash-ref audit 'raw-sleep-files))
   (define sleep-files
     (for/list ([f (in-list (walk-rkt-files "tests"))]
                #:when (positive? (count-raw-sleeps f)))
       f))
   (for ([f (in-list sleep-files)])
     (define key (path->string f))
     (define row (hash-ref raw-files (string->symbol key) #f))
     (check-true (hash? row) (format "wait-audit: missing row for ~a" key))
     (when (hash? row)
       (define occs (hash-ref row 'occurrences))
       (check-true (<= (count-raw-sleeps f) (length occs))
                   (format "wait-audit: ~a carries more live sleeps than recorded rows (stale audit?)"
                           key))
       (for ([o (in-list occs)])
         (check-true (non-empty-string? (~a (hash-ref o 'class "")))
                     (format "wait-audit: ~a occurrence without a class" key))
         (check-true (and (member (hash-ref o 'disposition) allowed-dispositions) #t)
                     (format "wait-audit: ~a occurrence with unknown disposition ~a"
                             key
                             (hash-ref o 'disposition)))
         (when (member (hash-ref o 'disposition) '("retained-canary" "not-a-test-sleep"))
           (check-true (non-empty-string? (hash-ref o 'reason ""))
                       (format "wait-audit: ~a retained occurrence without a named behavioral reason"
                               key))))
       (when (equal? (hash-ref row 'suite) "unit-fast")
         (for ([o (in-list occs)])
           (check-true (or (not (equal? (hash-ref o 'disposition) "retained-canary"))
                           (non-empty-string? (hash-ref o 'reason "")))
                       (format "wait-audit: unjustified retained sleep in unit-fast file ~a"
                               key)))))))
 (test-case "wait-audit lint: declared target families carry zero raw sleeps"
   (define audit (call-with-input-file WAIT-AUDIT-PATH read-json))
   (for ([tf (in-list (hash-ref audit 'target-families))])
     (define f (hash-ref tf 'file))
     (check-equal? (hash-ref tf 'raw-sleep-count) 0 (format "~a must carry zero raw sleeps" f))
     (check-equal? (count-raw-sleeps f)
                   0
                   (format "~a acquired a raw sleep call; update the wait-audit" f))
     (check-true (non-empty-string? (hash-ref tf 'evidence ""))
                 (format "~a must carry remediation evidence" f))))
 (test-case "wait-audit lint: remediated families have benchmark manifests"
   (define audit (call-with-input-file WAIT-AUDIT-PATH read-json))
   (for ([rf (in-list (hash-ref audit 'remediated-families))])
     (check-true (file-exists? (hash-ref rf 'benchmark))
                 (format "missing benchmark manifest for ~a" (hash-ref rf 'file))))))

;; ============================================================
;; Lint support
;; ============================================================

(define WAIT-AUDIT-PATH (format "artifacts/test-runtime/v~a-w1/wait-audit.json" q-version))
(define raw-sleep-rx #px"\\(sleep\\s")
(define allowed-dispositions '("remediated" "retained-canary" "not-a-test-sleep" "verified-clean"))

(define (walk-rkt-files dir)
  (append (for/list ([p (in-list (directory-list dir #:build? #t))]
                     #:when (and (file-exists? p) (regexp-match? #rx"[.]rkt$" p)))
            p)
          (for/fold ([acc '()])
                    ([p (in-list (directory-list dir #:build? #t))]
                     #:when (and (directory-exists? p)
                                 (not (member (path->string (file-name-from-path p))
                                              '("compiled" ".git" "node_modules")))))
            (append acc (walk-rkt-files p)))))

(define (count-raw-sleeps f)
  (call-with-input-file
   f
   (lambda (in) (for/sum ([line (in-lines in)]) (length (regexp-match* raw-sleep-rx line))))))

(module+ main
  (define failures (run-tests deterministic-clock-suite))
  (exit (if (zero? failures) 0 1)))
