#lang racket/base

;; util/glob.rkt — Shared glob-to-regexp conversion

(require racket/string)

(provide glob->regexp)

;; Convert a glob pattern to a regexp.
;; #:allow-slash? #t means * matches / (for path-based tools like find)
;; #:allow-slash? #f means * does NOT match / (for file-pattern tools like grep)
(define (glob->regexp pattern #:allow-slash? [allow-slash? #f])
  (define escaped
    (for/list ([ch (in-string pattern)])
      (case ch
        [(#\*) (if allow-slash? ".*" "[^/]*")]
        [(#\?) "."]
        [(#\. #\+ #\( #\) #\[ #\] #\{ #\} #\\ #\^ #\$ #\|) (string #\\ ch)]
        [else (string ch)])))
  (regexp (string-append "^" (string-join escaped "") "$")))
