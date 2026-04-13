#lang racket/base

;; util/audit-log.rkt — simple append-only audit log for sensitive path access
;;
;; SECURITY: Provides a lightweight audit trail for file/path operations.
;; Each entry is a single line: TIMESTAMP SESSION-ID ACTION PATH
;;
;; Log location: ~/.q/audit.log
;; No rotation, no size limits for now.

(require racket/date
         racket/port
         racket/file)

(provide audit-log!
         with-audit-log
         audit-log-path)

;; ============================================================
;; Log path
;; ============================================================

;; Returns the audit log path: ~/.q/audit.log
(define (audit-log-path)
  (build-path (find-system-path 'home-dir) ".q" "audit.log"))

;; ============================================================
;; ISO-8601 timestamp
;; ============================================================

(define (iso8601-now)
  (define d (current-date))
  (define (~2 n)
    (define s (number->string n))
    (if (< n 10) (string-append "0" s) s))
  (format "~a-~a-~aT~a:~a:~a"
          (date-year d)
          (~2 (date-month d))
          (~2 (date-day d))
          (~2 (date-hour d))
          (~2 (date-minute d))
          (~2 (date-second d))))

;; ============================================================
;; Core logging
;; ============================================================

;; audit-log! : string? symbol? string? [#:log-path path?] -> void?
;; Appends a single audit entry to the log file.
;; Format: TIMESTAMP SESSION-ID ACTION PATH
(define (audit-log! session-id action path #:log-path [log-path (audit-log-path)])
  (define dir (let-values ([(base _name _must-be-dir?) (split-path log-path)])
                (if (path? base) base (current-directory))))
  ;; Ensure directory exists
  (unless (directory-exists? dir)
    (make-directory* dir))
  ;; Ensure file exists
  (unless (file-exists? log-path)
    (close-output-port (open-output-file log-path #:exists 'truncate)))
  (call-with-output-file log-path
    (lambda (out)
      (fprintf out "~a ~a ~a ~a\n" (iso8601-now) session-id action path))
    #:exists 'append))

;; ============================================================
;; Convenience wrapper
;; ============================================================

;; with-audit-log : string? symbol? string? thunk -> any
;; Logs access before calling thunk, returns thunk's result.
(define (with-audit-log session-id action path thunk #:log-path [log-path (audit-log-path)])
  (audit-log! session-id action path #:log-path log-path)
  (thunk))
