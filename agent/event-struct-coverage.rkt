#lang racket/base

;; agent/event-struct-coverage.rkt -- emission site coverage counting
;; v0.29.2: Provides functions to count typed vs raw-hasheq emission sites.

(require racket/port
         racket/string
         racket/system
         racket/runtime-path)

(provide count-typed-emission-sites
         count-raw-hasheq-emission-sites
         event-struct-coverage-report)

;; Root q/ directory (one up from this file)
(define-runtime-path here ".")
(define q-dir (simplify-path (build-path here "..")))

;; Internal: grep and count lines matching pattern in a directory
(define (grep-count pattern dir)
  (define full-dir (build-path q-dir dir))
  (if (directory-exists? full-dir)
      (let ([out (open-output-string)])
        (parameterize ([current-output-port out]
                       [current-error-port (open-output-string)])
          (system (format "grep -rn '~a' '~a' --include='*.rkt' 2>/dev/null || true"
                          pattern full-dir)))
        (let* ([result (get-output-string out)]
               [lines (if (string=? result "")
                          '()
                          (string-split result "\n"))])
          (length (filter (lambda (l) (not (string=? l ""))) lines))))
      0))

(define (count-typed-emission-sites)
  (+ (grep-count "emit-typed-event!" "agent/")
     (grep-count "emit-typed-event!" "runtime/")
     (grep-count "emit-typed-event!" "llm/")))

(define (count-raw-hasheq-emission-sites)
  (+ (grep-count "emit! bus" "agent/")
     (grep-count "emit! bus" "runtime/")
     (grep-count "emit! bus" "llm/")))

(define (event-struct-coverage-report)
  (hasheq 'typed-sites (count-typed-emission-sites)
          'raw-hasheq-sites (count-raw-hasheq-emission-sites)))
