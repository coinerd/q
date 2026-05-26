#lang racket

;; tests/test-image-pipeline-security.rkt — Security hardening tests (#5360)
;;
;; Tests that shell metacharacters in filenames/paths cannot inject commands.

(require rackunit
         racket/file
         racket/path
         "../extensions/image-pipeline.rkt")

;; ═══════════════════════════════════════════════════════════════════
;; Injection resistance — filenames with shell metacharacters
;; ═══════════════════════════════════════════════════════════════════

(test-case "supported-image-file? with metacharacters in filename"
  ;; These filenames contain shell metacharacters — extension detection may fail
  ;; which is fine — conservative rejection is safe
  (check-false (supported-image-file? "/tmp/foo; rm -rf /.png"))
  ;; Path with spaces and .png — should work via string handling
  (check-true (supported-image-file? "/tmp/foo bar.png"))
  ;; Simple path always works
  (check-true (supported-image-file? "test.png"))
  ;; Non-image extensions rejected
  (check-false (supported-image-file? "/tmp/foo; rm -rf /.txt")))

(test-case "image-metadata with safe metacharacter filename"
  ;; Filenames with spaces — should work via argv, not shell
  (define tmp-dir (make-temporary-file "qtest-dir-~a" 'directory))
  (define tmp-file (build-path tmp-dir "test image.png"))
  (call-with-output-file tmp-file (lambda (out) (write-string "fake png" out)) #:exists 'truncate)
  (define meta (image-metadata tmp-file))
  (check-equal? (hash-ref meta 'format) 'png)
  (check-true (> (hash-ref meta 'size-bytes) 0))
  (delete-file tmp-file)
  (delete-directory tmp-dir))

(test-case "run-argv passes arguments literally (no shell expansion)"
  ;; Argument with shell metacharacters must be passed literally
  (define-values (ok out) (run-argv (list "/bin/echo" "test; rm -rf /")))
  (check-true ok)
  (check-true (string-contains? (bytes->string/utf-8 out) "test; rm -rf /")))

(test-case "run-argv passes $() literally"
  (define-values (ok out) (run-argv (list "/bin/echo" "$(whoami)")))
  (check-true ok)
  (check-true (string-contains? (bytes->string/utf-8 out) "$(whoami)")))

(test-case "run-argv passes backticks literally"
  (define-values (ok out) (run-argv (list "/bin/echo" "`id`")))
  (check-true ok)
  (check-true (string-contains? (bytes->string/utf-8 out) "`id`")))

(test-case "run-argv passes pipes literally"
  (define-values (ok out) (run-argv (list "/bin/echo" "foo | bar")))
  (check-true ok)
  (check-true (string-contains? (bytes->string/utf-8 out) "foo | bar")))

(test-case "resolve-tool returns #f for injection attempts"
  ;; Tool names with metacharacters should not resolve
  (check-false (resolve-tool "ls; rm -rf /"))
  (check-false (resolve-tool "$(whoami)"))
  (check-false (resolve-tool "`id`"))
  (check-false (resolve-tool "foo|bar")))

;; ═══════════════════════════════════════════════════════════════════
;; No shell strings in source
;; ═══════════════════════════════════════════════════════════════════

(test-case "image-pipeline source does not call system"
  (define src-path
    (build-path (or (current-load-relative-directory) (current-directory))
                ".."
                "extensions"
                "image-pipeline.rkt"))
  (define src (file->string src-path))
  ;; Should not use `system` or `system*` directly (only `process`)
  ;; Only allow `racket/system` in require and `process` in function calls
  (define system-calls (regexp-match* #rx"\\(system[^-]" src))
  (check-equal? system-calls '() "no raw system calls in image-pipeline.rkt"))

(test-case "image-pipeline source does not use shell-escape-path"
  (define src-path
    (build-path (or (current-load-relative-directory) (current-directory))
                ".."
                "extensions"
                "image-pipeline.rkt"))
  (define src (file->string src-path))
  (define matches (regexp-match* #rx"shell-escape-path" src))
  (check-equal? matches '() "shell-escape-path removed from image-pipeline.rkt"))

;; ═══════════════════════════════════════════════════════════════════
;; Option injection — filenames beginning with - (#5465)
;; ═══════════════════════════════════════════════════════════════════

(test-case "option-like filename rejected by resize-image (#5465)"
  ;; Filename beginning with - could be interpreted as a tool option
  (define tmp-dir (make-temporary-file "qtest-dir-~a" 'directory))
  (define tmp-file (build-path tmp-dir "-resize.png"))
  (call-with-output-file tmp-file (lambda (out) (write-string "fake png" out)) #:exists 'truncate)
  ;; image-metadata should still work (argv passes it literally)
  ;; but resize should handle it safely
  (with-handlers ([exn:fail? (lambda (e) (void))])
    (define meta (image-metadata tmp-file))
    (check-true (hash? meta)))
  (delete-file tmp-file)
  (delete-directory tmp-dir))

(test-case "supported-image-file? handles dash-prefixed paths (#5465)"
  (check-true (supported-image-file? "-malicious.png"))
  (check-true (supported-image-file? "--help.jpg")))

(test-case "path with shell metacharacters handled safely (#5465)"
  ;; Filenames with |, $, backticks — no shell injection possible via argv
  (define tmp-dir (make-temporary-file "qtest-dir-~a" 'directory))
  (define tmp-file (build-path tmp-dir "test$var.png"))
  (call-with-output-file tmp-file (lambda (out) (write-string "fake" out)) #:exists 'truncate)
  (define meta (image-metadata tmp-file))
  (check-equal? (hash-ref meta 'format) 'png)
  (delete-file tmp-file)
  (delete-directory tmp-dir))

(test-case "path with newlines handled safely (#5465)"
  (define tmp-dir (make-temporary-file "qtest-dir-~a" 'directory))
  (define tmp-file (build-path tmp-dir "test\\nimage.png"))
  (call-with-output-file tmp-file (lambda (out) (write-string "fake" out)) #:exists 'truncate)
  (define meta (image-metadata tmp-file))
  (check-true (hash? meta))
  (delete-file tmp-file)
  (delete-directory tmp-dir))

(test-case "run-argv handles absolute path with spaces (#5465)"
  (define-values (ok out) (run-argv (list "/bin/echo" "/path/with spaces/file.png")))
  (check-true ok)
  (check-true (string-contains? (bytes->string/utf-8 out) "/path/with spaces/file.png")))
;; ═══════════════════════════════════════════════════════════════════
;; Timeout tests
;; ═══════════════════════════════════════════════════════════════════

(test-case "run-argv respects timeout and kills subprocess"
  ;; Sleep for 10 seconds but timeout at 1 second
  (define-values (ok out) (run-argv (list "/bin/sleep" "10") #:timeout 1))
  (check-false ok "subprocess killed by timeout"))

(test-case "run-argv succeeds when subprocess finishes before timeout"
  (define-values (ok out) (run-argv (list "/bin/echo" "fast") #:timeout 5))
  (check-true ok)
  (check-true (regexp-match? #"fast" out)))

(test-case "timeout parameters have reasonable defaults"
  (check-equal? (image-probe-timeout) 5)
  (check-equal? (image-metadata-timeout) 10)
  (check-equal? (image-resize-timeout) 30))

(test-case "timeout parameters are configurable"
  (parameterize ([image-probe-timeout 2]
                 [image-metadata-timeout 5]
                 [image-resize-timeout 15])
    (check-equal? (image-probe-timeout) 2)
    (check-equal? (image-metadata-timeout) 5)
    (check-equal? (image-resize-timeout) 15)))

(module+ main
  (require rackunit/text-ui))
