#lang racket

(require rackunit
         "../extensions/api.rkt")

;; ============================================================
;; Extension struct
;; ============================================================

(test-case "extension struct holds fields"
  (define ext (extension "my-ext" "1.0" "1" (hasheq 'before-send (λ (p) p))))
  (check-true (extension? ext))
  (check-equal? (extension-name ext) "my-ext")
  (check-equal? (extension-version ext) "1.0")
  (check-equal? (extension-api-version ext) "1"))

;; ============================================================
;; Extension registry — basic CRUD
;; ============================================================

(test-case "make-extension-registry is empty"
  (define reg (make-extension-registry))
  (check-true (extension-registry? reg))
  (check-equal? (list-extensions reg) '()))

(test-case "register-extension! and lookup-extension"
  (define reg (make-extension-registry))
  (define ext (extension "e1" "0.1" "1" (hasheq)))
  (register-extension! reg ext)
  (check-equal? (lookup-extension reg "e1") ext)
  (check-false (lookup-extension reg "nonexistent")))

(test-case "list-extensions returns all"
  (define reg (make-extension-registry))
  (register-extension! reg (extension "a" "1" "1" (hasheq)))
  (register-extension! reg (extension "b" "1" "1" (hasheq)))
  (check equal? (sort (map extension-name (list-extensions reg))
                      string<?) '("a" "b")))

(test-case "unregister-extension! removes extension"
  (define reg (make-extension-registry))
  (register-extension! reg (extension "gone" "1" "1" (hasheq)))
  (unregister-extension! reg "gone")
  (check-false (lookup-extension reg "gone")))

;; ============================================================
;; handlers-for-point
;; ============================================================

(test-case "handlers-for-point returns matching handlers"
  (define reg (make-extension-registry))
  (define handler1 (λ (p) p))
  (define handler2 (λ (p) p))
  (register-extension! reg (extension "ext-a" "1" "1"
                                       (hasheq 'before-send handler1)))
  (register-extension! reg (extension "ext-b" "1" "1"
                                       (hasheq 'before-send handler2)))
  (define handlers (handlers-for-point reg 'before-send))
  (check-equal? (length handlers) 2)
  (check-equal? (car (car handlers)) "ext-a")
  (check-equal? (car (cadr handlers)) "ext-b"))

(test-case "handlers-for-point returns empty for missing hook"
  (define reg (make-extension-registry))
  (register-extension! reg (extension "e" "1" "1" (hasheq 'other (λ (p) p))))
  (check-equal? (handlers-for-point reg 'before-send) '()))

(test-case "register-extension! overwrites on same name"
  (define reg (make-extension-registry))
  (register-extension! reg (extension "dup" "1" "1" (hasheq)))
  (register-extension! reg (extension "dup" "2" "1" (hasheq)))
  (check-equal? (extension-version (lookup-extension reg "dup")) "2")
  (check-equal? (length (list-extensions reg)) 1))

(test-case "register-extension! overwrite preserves insertion order"
  (define reg (make-extension-registry))
  (register-extension! reg (extension "x" "1.0" "1" (hasheq)))
  (register-extension! reg (extension "y" "1.0" "1" (hasheq)))
  (register-extension! reg (extension "x" "2.0" "1" (hasheq)))
  (define names (map extension-name (list-extensions reg)))
  (check-equal? names '("y" "x")))
