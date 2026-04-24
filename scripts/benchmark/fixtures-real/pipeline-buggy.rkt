#lang racket/base

;; runtime/pipeline.rkt — data pipeline with processing stages (with deliberate bugs)

(require racket/list
         racket/match
         racket/string)

(provide pipeline-process
         stage-filter
         stage-map
         stage-sort
         stage-deduplicate
         make-pipeline)

;; A simple pipeline stage
(struct stage (type fn) #:transparent)

;; BUG: filter stage drops the last element
(define (stage-filter pred)
  (stage 'filter
         (lambda (items)
           (define filtered (filter pred items))
           (if (> (length filtered) 0)
               (take filtered (sub1 (length filtered)))  ;; BUG: drops last
               filtered))))

;; Works correctly
(define (stage-map fn)
  (stage 'map
         (lambda (items)
           (map fn items))))

;; BUG: sorts in wrong direction (uses > instead of < for ascending)
(define (stage-sort [key-fn (lambda (x) x)])
  (stage 'sort
         (lambda (items)
           (sort items (lambda (a b) (> (key-fn a) (key-fn b)))))))  ;; BUG: wrong comparison

;; Works correctly
(define (stage-deduplicate [key-fn (lambda (x) x)])
  (stage 'dedup
         (lambda (items)
           (define seen (make-hash))
           (for/list ([item (in-list items)]
                      #:when (not (hash-has-key? seen (key-fn item))))
             (hash-set! seen (key-fn item) #t)
             item))))

;; BUG: applies stages in reverse order
(define (make-pipeline stages)
  (lambda (input)
    (foldl (lambda (s acc)
             ((stage-fn s) acc))
           input
           (reverse stages))))  ;; BUG: shouldn't reverse

;; Pipeline processing runner
(define (pipeline-process input stages)
  (define pipe (make-pipeline stages))
  (pipe input))
