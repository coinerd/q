#lang racket/base

;; @suite gsd
;; @speed fast
;; @boundary pure
;; tests/test-campaign-notifier.rkt
;; BUG-0040 characterization pin (v1.00.21 W0; FLIPPED by W6).
;;
;; TODAY no campaign notification surface exists: terminal
;; transitions (gsd.wave.completed / gsd.wave.failed) emit only the
;; in-process telemetry event — no notifier sink, no side effect, no
;; gsd.notify.* settings keys, no notifier module. Every assertion
;; below PASSES against today's red behavior; W6 flips them once a
;; best-effort notification surface lands.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         racket/path
         racket/runtime-path
         "../extensions/gsd/events.rkt")

(define-runtime-path settings-query-src "../runtime/settings-query.rkt")
(define-runtime-path gsd-dir "../extensions/gsd")

;; ── Behavioral probe: terminal transitions have no fan-out ────

(define campaign-notifier-suite
  (test-suite "BUG-0040 characterization: terminal transitions have no notifier sink (W0 pin; W4 flips)"
    (test-case "terminal transition emits ONLY the telemetry event (no notifier sink)"
      (define-values (collect! query) (make-event-collector))
      (set-gsd-event-bus! collect!)
      (emit-gsd-event! 'gsd.wave.completed (hasheq 'wave 0 'status 'done))
      (check-equal? (length (collector-events query))
                    1
                    "exactly one event: the telemetry event itself, nothing else")
      (emit-gsd-event! 'gsd.wave.failed (hasheq 'wave 1 'status 'failed))
      (check-equal? (length (collector-events query))
                    2
                    "still no side channel after a failure transition")
      ;; no notifier/notification trace appears alongside the telemetry
      (define repr (format "~s" (collector-events query)))
      (check-false (string-contains? (string-downcase repr) "notif")))

    ;; ── Absent-seam markers (v1.00.19 freshness-pin precedent) ────

    (test-case "no notify event kind in the GSD taxonomy"
      (check-false (for/or ([n (in-list gsd-event-names)])
                     (and (symbol? n) (string-contains? (symbol->string n) "notif")))))

    (test-case "no gsd.notify.* settings keys / accessor"
      (check-false (string-contains? (file->string settings-query-src) "notif")
                   "TODAY settings-query has no notifier configuration surface"))

    (test-case "no notifier module exists under extensions/gsd"
      (check-false (for/or ([f (in-list (directory-list gsd-dir))])
                     (string-contains? (string-downcase (path->string f)) "notif")))
      (check-false
       (for/or ([f (in-list (find-files (lambda (p)
                                          (and (regexp-match? #rx"\\.rkt$" (path->string p))
                                               (not (string-contains? (path->string p) "compiled"))))
                                        gsd-dir))])
         (string-contains? (file->string f) "gsd.notify"))))))

(module+ main
  (exit (run-tests campaign-notifier-suite)))
