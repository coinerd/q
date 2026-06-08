#lang racket

;; @speed slow  ;; @suite security

;; BOUNDARY: unit
;; @suite runtime
;; @boundary unit
;; @speed fast
;; @mutates env
;; @isolation temp-dir
;; Tests for tests/helpers/test-sandbox.rkt (v0.83.3 W0)

(require rackunit
         racket/file
         racket/path)

(require (only-in "../tests/helpers/test-sandbox.rkt"
                  make-test-sandbox
                  test-sandbox?
                  test-sandbox-project-dir
                  test-sandbox-session-dir
                  test-sandbox-home-dir
                  test-sandbox-q-dir
                  test-sandbox-quarantine-dir
                  test-sandbox-temp-root
                  with-test-sandbox))

;; ---------------------------------------------------------------------------
;; make-test-sandbox
;; ---------------------------------------------------------------------------

(test-case "make-test-sandbox: creates all directories"
  (define sb (make-test-sandbox))
  (check-true (test-sandbox? sb))
  (check-true (directory-exists? (test-sandbox-project-dir sb)))
  (check-true (directory-exists? (test-sandbox-session-dir sb)))
  (check-true (directory-exists? (test-sandbox-home-dir sb)))
  (check-true (directory-exists? (test-sandbox-q-dir sb)))
  (check-true (directory-exists? (test-sandbox-quarantine-dir sb)))
  ;; All under same temp root
  (check-equal? (string-trim (path->string (test-sandbox-temp-root sb)) "/" #:right? #t)
                (string-trim (path->string (simplify-path (build-path (test-sandbox-project-dir sb)
                                                                      "..")))
                             "/"
                             #:right? #t))
  ;; Cleanup
  (delete-directory/files (test-sandbox-temp-root sb)))

(test-case "make-test-sandbox: directories are writable"
  (define sb (make-test-sandbox))
  (define test-file (build-path (test-sandbox-project-dir sb) "test.txt"))
  (call-with-output-file test-file (lambda (out) (display "hello" out)) #:exists 'truncate)
  (check-equal? (file->string test-file) "hello")
  (delete-directory/files (test-sandbox-temp-root sb)))

(test-case "make-test-sandbox: preserve? leaves directories"
  (define sb (make-test-sandbox #:preserve? #t))
  (define root (test-sandbox-temp-root sb))
  ;; Manually clean since preserve? only affects with-test-sandbox
  (check-true (directory-exists? root))
  (delete-directory/files root))

;; ---------------------------------------------------------------------------
;; with-test-sandbox — cwd isolation
;; ---------------------------------------------------------------------------

(test-case "with-test-sandbox: parameterizes current-directory"
  (define outer-cwd (current-directory))
  (define captured-cwd #f)
  (define captured-project #f)
  (with-test-sandbox (lambda (sb)
                       (set! captured-cwd (current-directory))
                       (set! captured-project (test-sandbox-project-dir sb))))
  (check-true (string=? (string-trim (path->string captured-cwd) "/" #:right? #t)
                        (string-trim (path->string captured-project) "/" #:right? #t)))
  ;; cwd restored
  (check-equal? (string-trim (path->string (current-directory)) "/" #:right? #t)
                (string-trim (path->string outer-cwd) "/" #:right? #t)))

(test-case "with-test-sandbox: restores cwd after exception"
  (define outer-cwd (current-directory))
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (with-test-sandbox (lambda (sb) (error "test error"))))
  (check-equal? (string-trim (path->string (current-directory)) "/" #:right? #t)
                (string-trim (path->string outer-cwd) "/" #:right? #t)))

(test-case "with-test-sandbox: cleans up temp dirs after normal exit"
  (define root #f)
  (with-test-sandbox (lambda (sb) (set! root (test-sandbox-temp-root sb))))
  (check-false (directory-exists? root)))

(test-case "with-test-sandbox: cleans up temp dirs after exception"
  (define root #f)
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (with-test-sandbox (lambda (sb)
                         (set! root (test-sandbox-temp-root sb))
                         (error "boom"))))
  (check-false (directory-exists? root)))

(test-case "with-test-sandbox: preserve? keeps temp dirs"
  (define root #f)
  (with-test-sandbox #:preserve? #t (lambda (sb) (set! root (test-sandbox-temp-root sb))))
  (check-true (directory-exists? root))
  (delete-directory/files root))

;; ---------------------------------------------------------------------------
;; with-test-sandbox — env isolation
;; ---------------------------------------------------------------------------

(test-case "with-test-sandbox: isolates HOME"
  (define captured-home #f)
  (with-test-sandbox #:isolate-home? #t
                     (lambda (sb)
                       (set! captured-home (getenv "HOME"))
                       (check-not-false (regexp-match? #rx"q-sandbox-" (or captured-home "")))))
  ;; HOME restored
  (check-not-equal? (getenv "HOME") captured-home))

(test-case "with-test-sandbox: no home isolation when disabled"
  (define outer-home (getenv "HOME"))
  (with-test-sandbox #:isolate-home? #f (lambda (sb) (check-equal? (getenv "HOME") outer-home))))

(test-case "with-test-sandbox: no cwd isolation when disabled"
  (define outer-cwd (current-directory))
  (with-test-sandbox #:isolate-cwd? #f
                     (lambda (sb)
                       (check-equal? (string-trim (path->string (current-directory)) "/" #:right? #t)
                                     (string-trim (path->string outer-cwd) "/" #:right? #t)))))

(test-case "with-test-sandbox: restores env after exception"
  (define outer-home (getenv "HOME"))
  (with-handlers ([exn:fail? (lambda (_) (void))])
    (with-test-sandbox #:isolate-home? #t (lambda (sb) (error "env test error"))))
  (check-equal? (getenv "HOME") outer-home))

;; ---------------------------------------------------------------------------
;; Sandbox structure
;; ---------------------------------------------------------------------------

(test-case "sandbox: q-dir is under home/.config/q"
  (with-test-sandbox
   (lambda (sb)
     (define expected (build-path (test-sandbox-home-dir sb) ".config" "q"))
     (check-equal? (string-trim (path->string (test-sandbox-q-dir sb)) "/" #:right? #t)
                   (string-trim (path->string expected) "/" #:right? #t)))))

(test-case "sandbox: quarantine-dir is under project/.quarantine"
  (with-test-sandbox
   (lambda (sb)
     (define expected (build-path (test-sandbox-project-dir sb) ".quarantine"))
     (check-equal? (string-trim (path->string (test-sandbox-quarantine-dir sb)) "/" #:right? #t)
                   (string-trim (path->string expected) "/" #:right? #t)))))
