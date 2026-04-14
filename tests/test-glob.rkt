#lang racket

(require rackunit
         rackunit/text-ui
         "../util/glob.rkt")

(define glob-suite
  (test-suite
   "glob tests"

   (test-case "star matches any filename without slash"
     (define rx (glob->regexp "*.rkt"))
     (check-true (regexp-match? rx "foo.rkt"))
     (check-false (regexp-match? rx "sub/foo.rkt")))

   (test-case "star with allow-slash matches paths"
     (define rx (glob->regexp "*.rkt" #:allow-slash? #t))
     (check-true (regexp-match? rx "sub/foo.rkt"))
     (check-true (regexp-match? rx "a/b/c.rkt")))

   (test-case "question mark matches single character"
     (define rx (glob->regexp "file?.txt"))
     (check-true (regexp-match? rx "file1.txt"))
     (check-false (regexp-match? rx "file12.txt")))

   (test-case "dot is escaped"
     (define rx (glob->regexp "*.rkt"))
     (check-false (regexp-match? rx "fooXrkt")))

   (test-case "literal characters match exactly"
     (define rx (glob->regexp "hello"))
     (check-true (regexp-match? rx "hello"))
     (check-false (regexp-match? rx "hellox"))
     (check-false (regexp-match? rx "xhello")))

   (test-case "multiple stars match path segments"
     (define rx (glob->regexp "*/*.rkt" #:allow-slash? #t))
     (check-true (regexp-match? rx "src/main.rkt"))
     (check-false (regexp-match? rx "main.rkt")))))

(run-tests glob-suite)
