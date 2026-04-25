#lang racket

;; q/tests/test-input-file-ref.rkt — Tests for G3.2 @ file reference expansion
;;
;; Covers:
;;   - Unique prefix expansion
;;   - No matching files (unchanged)
;;   - Multiple matches → common prefix
;;   - Tab at non-@ position (no-op)
;;   - Multiple @ references in one line

(require rackunit
         rackunit/text-ui
         racket/file
         "../tui/input.rkt")

;; ── Helpers ──

(define (make-test-input text cursor-pos)
  (input-state text cursor-pos '() #f #f 0 '() '() '()))

(define (buffer-of st)
  (input-state-buffer st))

(define (cursor-of st)
  (input-state-cursor st))

;; Create a temporary directory with known files for glob tests.
(define (with-temp-dir thunk)
  (define tmp (make-temporary-file "q-fileref-~a" 'directory))
  (dynamic-wind (lambda () (void))
                (lambda ()
                  ;; Create test files
                  (make-directory (build-path tmp "src"))
                  (call-with-output-file (build-path tmp "src" "main.rkt") void)
                  (call-with-output-file (build-path tmp "src" "mail.txt") void)
                  (call-with-output-file (build-path tmp "src" "helper.rkt") void)
                  (make-directory (build-path tmp "test"))
                  (call-with-output-file (build-path tmp "test" "foo.rkt") void)
                  (call-with-output-file (build-path tmp "test" "foobar.rkt") void)
                  ;; Run thunk with CWD set to temp dir
                  (parameterize ([current-directory tmp])
                    (thunk)))
                (lambda () (delete-directory/files tmp))))

;; ── Test suite ──

(define test-input-file-ref
  (test-suite "input-expand-file-ref (G3.2)"

    ;; 1. Unique prefix expands to full path
    (test-case "unique prefix with single match expands"
      (with-temp-dir (lambda ()
                       ;; "@src/hel" = 8 chars, cursor at end = 8
                       ;; Expands to "@src/helper.rkt" = 15 chars
                       (define st (make-test-input "@src/hel" 8))
                       (define result (input-expand-file-ref st))
                       (check-equal? (buffer-of result) "@src/helper.rkt")
                       (check-equal? (cursor-of result) 15))))

    (test-case "fully qualified unique file expands"
      (with-temp-dir (lambda ()
                       ;; "@src/main.rk" = 12 chars, cursor at end
                       ;; Expands to "@src/main.rkt" = 13 chars
                       (define st (make-test-input "@src/main.rk" 12))
                       (define result (input-expand-file-ref st))
                       (check-equal? (buffer-of result) "@src/main.rkt")
                       (check-equal? (cursor-of result) 13))))

    ;; 2. No matching files → unchanged
    (test-case "no matching files leaves text unchanged"
      (with-temp-dir (lambda ()
                       (define st (make-test-input "@nonexistent" 12))
                       (define result (input-expand-file-ref st))
                       (check-equal? (buffer-of result) "@nonexistent")
                       (check-equal? (cursor-of result) 12))))

    (test-case "empty partial after @ leaves text unchanged"
      (with-temp-dir (lambda ()
                       (define st (make-test-input "@" 1))
                       (define result (input-expand-file-ref st))
                       (check-equal? (buffer-of result) "@")
                       (check-equal? (cursor-of result) 1))))

    ;; 3. Multiple matching files → insert common prefix
    (test-case "multiple matches insert common prefix"
      (with-temp-dir (lambda ()
                       ;; test/foo.rkt and test/foobar.rkt match test/f*
                       ;; Common prefix of "test/foo.rkt" and "test/foobar.rkt" is "test/foo"
                       ;; Partial is "test/f" → expand to "test/foo"
                       (define st (make-test-input "@test/f" 7))
                       (define result (input-expand-file-ref st))
                       (check-equal? (buffer-of result) "@test/foo")
                       (check-equal? (cursor-of result) 9))))

    (test-case "ambiguous prefix returns unchanged"
      (with-temp-dir (lambda ()
                       ;; src/mai* matches src/main.rkt and src/mail.txt
                       ;; Common prefix is "src/mai" which equals the partial → no expansion
                       (define st (make-test-input "@src/mai" 8))
                       (define result (input-expand-file-ref st))
                       (check-equal? (buffer-of result) "@src/mai")
                       (check-equal? (cursor-of result) 8))))

    ;; 4. Tab at non-@ position does nothing
    (test-case "non-@ position does nothing"
      (with-temp-dir (lambda ()
                       (define st (make-test-input "hello world" 5))
                       (define result (input-expand-file-ref st))
                       (check-equal? (buffer-of result) "hello world")
                       (check-equal? (cursor-of result) 5))))

    (test-case "@ not preceded by whitespace does nothing"
      (with-temp-dir (lambda ()
                       ;; email@example — @ is not a file ref because preceded by non-whitespace
                       (define st (make-test-input "email@example" 13))
                       (define result (input-expand-file-ref st))
                       (check-equal? (buffer-of result) "email@example")
                       (check-equal? (cursor-of result) 13))))

    ;; 5. Multiple @ references in one line
    (test-case "multiple @ references — expand second"
      (with-temp-dir (lambda ()
                       ;; "see @src/main.rkt and @src/hel" = 30 chars, cursor at 30 (end)
                       (define st (make-test-input "see @src/main.rkt and @src/hel" 30))
                       (define result (input-expand-file-ref st))
                       (check-equal? (buffer-of result) "see @src/main.rkt and @src/helper.rkt")
                       ;; Length: "see @src/main.rkt and @src/helper.rkt" = 37
                       (check-equal? (cursor-of result) 37))))

    (test-case "multiple @ references — expand first with unique match"
      (with-temp-dir (lambda ()
                       ;; Cursor at 8 = right after "@src/hel" (first ref)
                       (define st (make-test-input "@src/hel and @test/f" 8))
                       (define result (input-expand-file-ref st))
                       (check-equal? (buffer-of result) "@src/helper.rkt and @test/f")
                       ;; Cursor after expanded path: 15
                       (check-equal? (cursor-of result) 15))))

    ;; 6. Undo restores previous state
    (test-case "expansion is undoable"
      (with-temp-dir (lambda ()
                       (define st (make-test-input "@src/hel" 8))
                       (define expanded (input-expand-file-ref st))
                       (check-equal? (buffer-of expanded) "@src/helper.rkt")
                       ;; Undo should restore original
                       (define undone (input-undo expanded))
                       (check-equal? (buffer-of undone) "@src/hel")
                       (check-equal? (cursor-of undone) 8))))))

;; ── Run ──

(run-tests test-input-file-ref)
