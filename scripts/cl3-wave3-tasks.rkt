#lang racket/base

;; scripts/cl3-wave3-tasks.rkt — CL3 Wave 3 task fixtures (evaluation-only).
;; Unlike Wave 2, each task edits REAL component logic (not an injected probe)
;; and checks real behaviour through the module's public export. Expected values
;; are declared here, independent of the implementation under test.

(require racket/format
         racket/port
         racket/string)

(provide cl3-task?
         cl3-task-name
         cl3-task-module
         cl3-task-before-snippet
         cl3-task-after-snippet
         cl3-task-expression
         cl3-task-expected-before
         cl3-task-expected-after
         tasks
         apply-edit!
         reset-source!
         expression-code
         first-output-line)

(struct cl3-task (name module before-snippet after-snippet expression expected-before expected-after)
  #:transparent)

;; `expression` is real public-API usage. Both transports `require` the module
;; and `displayln` the value, so expected strings are comparable.
(define tasks
  (list (cl3-task "hash-ref* default"
                  "util/hash-utils.rkt"
                  "(define (hash-ref* h keys [default #f])"
                  "(define (hash-ref* h keys [default 'absent])"
                  "(hash-ref* (hash 'a 1) '(b))"
                  "#f"
                  "absent")
        (cl3-task
         "count-occurrences filters non-symbols"
         "runtime/iteration/retry-policy.rkt"
         "(hash-set counts item (add1 (hash-ref counts item (lambda () 0))))))"
         (string-append "(if (symbol? item)\n"
                        "        (hash-set counts item (add1 (hash-ref counts item (lambda () 0))))\n"
                        "        counts)))")
         "(hash-count (count-occurrences '(a b 1 2)))"
         "4"
         "2")
        (cl3-task "route-code for the no-route case"
                  "extensions/gsd/campaign-decision.rkt"
                  "[(none) 'campaign-action]"
                  "[(none) 'campaign-noop]"
                  "(route-code 'none)"
                  "campaign-action"
                  "campaign-noop")))

(define (task-path root task)
  (build-path root (cl3-task-module task)))

(define (read-source root task)
  (call-with-input-file (task-path root task) port->string))

(define (write-source! root task text)
  (call-with-output-file (task-path root task)
                         (lambda (out) (display text out))
                         #:exists 'truncate/replace))

(define (pattern-count haystack needle)
  (length (regexp-match* (regexp (regexp-quote needle)) haystack)))

(define (replace-snippet! root task from to label)
  (define source (read-source root task))
  (define occurrences (pattern-count source from))
  (unless (= 1 occurrences)
    (error 'cl3-wave3 "~a: expected exactly one edit anchor, found ~a" label occurrences))
  (define positions (regexp-match-positions (regexp (regexp-quote from)) source))
  (define start (car (car positions)))
  (write-source!
   root
   task
   (string-append (substring source 0 start) to (substring source (+ start (string-length from)))))
  (void))

(define (apply-edit! root task label)
  (replace-snippet! root task (cl3-task-before-snippet task) (cl3-task-after-snippet task) label))

(define (reset-source! root task)
  (cond
    [(= 1 (pattern-count (read-source root task) (cl3-task-before-snippet task))) (void)]
    [else
     (replace-snippet! root
                       task
                       (cl3-task-after-snippet task)
                       (cl3-task-before-snippet task)
                       (format "~a reset" (cl3-task-name task)))]))

(define (expression-code root task)
  (format "(begin (require (file ~s)) (displayln ~a))"
          (path->string (path->complete-path (task-path root task)))
          (cl3-task-expression task)))

(define (first-output-line text)
  (define lines
    (filter (lambda (line) (non-empty-string? (string-trim line))) (string-split text "\n")))
  (if (pair? lines)
      (string-trim (car lines))
      ""))
