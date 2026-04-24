#lang racket

(provide flatten-one
         nested-map
         deep-filter
         zip-with)

;; Flatten one level of nesting.
(define (flatten-one lst)
  (apply append (map (lambda (x) (if (list? x) x (list x))) lst)))

;; Apply a function to every element at any nesting depth.
;; BUG: fails on empty sublists — map on '() is fine, but the
;;      recursion doesn't handle the case where a sublist is null
;;      before recursing, causing issues with certain patterns.
(define (nested-map f lst)
  (map (lambda (x)
         (if (list? x)
             (nested-map f x)
             (f x)))
       lst))

;; Filter elements at any nesting depth.
(define (deep-filter pred lst)
  (filter-map
   (lambda (x)
     (cond
       [(and (list? x) (null? x)) '()]
       [(list? x) (deep-filter pred x)]
       [(pred x) x]
       [else #f]))
   lst))

;; Zip two lists with a combining function.
(define (zip-with f lst1 lst2)
  (map f lst1 lst2))
