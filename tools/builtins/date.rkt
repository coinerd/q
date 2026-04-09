#lang racket

;; tools/builtins/date.rkt — returns current date/time
;;
;; Provides temporal awareness to the agent so it can reason about
;; "today", "this year", "current season", etc.
;;
;; Contract:
;;   args: (hash 'format [string])  — "iso" (default), "date", "time", "unix", "weekday"
;;   returns: tool-result with current date/time as text content

(require "../../tools/tool.rkt"
         racket/date)

(provide tool-date)

;; --------------------------------------------------
;; Helpers
;; --------------------------------------------------

(define (err msg)
  (make-error-result
   (list (hasheq 'type "text" 'text (format "date error: ~a" msg)))
   (hasheq)))

;; --------------------------------------------------
;; Tool definition
;; --------------------------------------------------

(define (pad2 n)
  (~a n #:min-width 2 #:pad-string "0" #:align 'right))

(define (tool-date args [exec-ctx #f])
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
      [("date")
       (format "~a-~a-~a"
               (date-year now)
               (pad2 (date-month now))
               (pad2 (date-day now)))]
      [("time")
       (format "~a:~a:~a"
               (pad2 (date-hour now))
               (pad2 (date-minute now))
               (pad2 (date-second now)))]
      [("unix")
       (number->string (current-seconds))]
      [("weekday")
       (define days '#("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))
       (vector-ref days (date-week-day now))]
      [("iso-full")
       ;; ISO date + weekday + time + timezone offset hint
       (define days '#("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))
       (format "~a-~a-~a (~a) ~a:~a:~a"
               (date-year now)
               (pad2 (date-month now))
               (pad2 (date-day now))
               (vector-ref days (date-week-day now))
               (pad2 (date-hour now))
               (pad2 (date-minute now))
               (pad2 (date-second now)))]
      [else
       (format "Unknown format: ~a. Use: iso, date, time, unix, weekday, iso-full" fmt)]))
  (make-success-result
   (list (hasheq 'type "text" 'text result-text))
   (hasheq 'format fmt)))

;; Tool metadata
(module+ tool-info
  (define tool-name "date")
  (define tool-description "Returns the current date and time. Use this tool to learn today's date before answering time-dependent questions.")
  (define tool-schema
    (hasheq 'type "object"
            'properties (hasheq 'format (hasheq 'type "string"
                                                 'description "Output format: 'iso' (default, ISO 8601), 'date' (YYYY-MM-DD), 'time' (HH:MM:SS), 'unix' (epoch), 'weekday' (day name), 'iso-full' (date + weekday + time)"))
            'required '())))
