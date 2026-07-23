#lang racket/base

;; tui/arg-summary.rkt -- Tool argument summary formatting
;;
;; Extracted from tui/state-types.rkt (v0.99.58 W4-2, P4-S).
;; Pure string/JSON formatting with no struct dependencies.
;; The function summarizes tool-call arguments for compact TUI display.

(require racket/contract
         json
         "../util/string-helpers.rkt")

(provide (contract-out [extract-arg-summary (-> (or/c string? hash?) string?)]))

(define (extract-arg-summary args-str)
  (define (summarize-hash h)
    ;; For tools with large text content, show a brief description instead
    (cond
      [(hash-has-key? h 'jobs)
       ;; spawn-subagents: show job count + first task
       (define jobs (hash-ref h 'jobs '()))
       (define n
         (if (list? jobs)
             (length jobs)
             0))
       (define first-task (and (pair? jobs) (hash? (car jobs)) (hash-ref (car jobs) 'task #f)))
       (if (string? first-task)
           (format "~a job~a: \"~a\""
                   n
                   (if (= n 1) "" "s")
                   (substring first-task 0 (min 40 (string-length first-task))))
           (format "~a job~a" n (if (= n 1) "" "s")))]
      [(hash-has-key? h 'task)
       ;; spawn-subagent (single): show task text
       (define t (hash-ref h 'task ""))
       (format "\"~a\"" (substring t 0 (min 50 (string-length t))))]
      [(hash-has-key? h 'content)
       (define c (hash-ref h 'content ""))
       (format "content: \"~a\"~a"
               (substring c 0 (min 30 (string-length c)))
               (if (> (string-length c) 30) "..." ""))]
      [(hash-has-key? h 'query)
       (define q (hash-ref h 'query ""))
       (format "query: \"~a\"" (substring q 0 (min 40 (string-length q))))]
      [else
       (define vals (hash-values h))
       (if (null? vals)
           "(no args)"
           (truncate-string (format "~a" (car vals)) 60))]))
  (cond
    [(hash? args-str) (summarize-hash args-str)]
    [(string? args-str)
     (with-handlers ([exn:fail? (lambda (_) (truncate-string args-str 60))])
       (define h (read-json (open-input-string args-str)))
       (cond
         [(hash? h) (summarize-hash h)]
         [else (truncate-string (format "~a" h) 60)]))]
    [else (truncate-string (format "~a" args-str) 60)]))
