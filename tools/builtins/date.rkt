#lang racket/base

;; tools/builtins/date.rkt — Date/time tool
;;
;; v0.33.2 W0: Converted to define-tool macro.

(require racket/contract
         "../tool.rkt"
         "../define-tool.rkt"
         racket/format
         racket/date)

(define (pad2 n)
  (~a n #:min-width 2 #:pad-string "0" #:align 'right))

;; --------------------------------------------------
;; Handler function
;; --------------------------------------------------

(define (date-handler args [exec-ctx #f])
  (define fmt (hash-ref args 'format "iso"))
  (define now (current-date))
  (define result-text
    (case fmt
      [("iso")
       (format "~a-~a-~aT~a:~a:~a"
               (date-year now)
               (pad2 (date-month now))
               (pad2 (date-day now))
               (pad2 (date-hour now))
               (pad2 (date-minute now))
               (pad2 (date-second now)))]
      [("date") (format "~a-~a-~a" (date-year now) (pad2 (date-month now)) (pad2 (date-day now)))]
      [("time")
       (format "~a:~a:~a" (pad2 (date-hour now)) (pad2 (date-minute now)) (pad2 (date-second now)))]
      [("unix") (number->string (current-seconds))]
      [("weekday")
       (define days '#("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))
       (vector-ref days (date-week-day now))]
      [("iso-full")
       (define days '#("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))
       (format "~a-~a-~a (~a) ~a:~a:~a"
               (date-year now)
               (pad2 (date-month now))
               (pad2 (date-day now))
               (vector-ref days (date-week-day now))
               (pad2 (date-hour now))
               (pad2 (date-minute now))
               (pad2 (date-second now)))]
      [else (format "Unknown format: ~a. Use: iso, date, time, unix, weekday, iso-full" fmt)]))
  (make-success-result (list (hasheq 'type "text" 'text result-text)) (hasheq 'format fmt)))

;; --------------------------------------------------
;; Tool definition via define-tool macro
;; --------------------------------------------------

(define-tool
 date
 #:description
 "Returns the current date and time. Use this tool to learn today's date before answering time-dependent questions."
 #:required ()
 #:properties [(format "string" "Output format: iso, date, time, unix, weekday, iso-full")]
 date-handler)

(provide (contract-out [date any/c]))
