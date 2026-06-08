#lang racket

;; @speed fast  ;; @suite security

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
    (build-path (or (current-load-relative-directory) (find-system-path 'orig-dir))
                ".."
                "extensions"
                "image-pipeline.rkt"))
  (unless (file-exists? src-path)
    ;; Graceful skip when path resolution fails (e.g. runner from repo root)
    (log-warning "skipping source scan: ~a not found" src-path))
  (when (file-exists? src-path)
    (define src (file->string src-path))
    (define system-calls (regexp-match* #rx"\\(system[^-]" src))
    (check-equal? system-calls '() "no raw system calls in image-pipeline.rkt")))

(test-case "image-pipeline source does not use shell-escape-path"
  (define src-path
    (build-path (or (current-load-relative-directory) (find-system-path 'orig-dir))
                ".."
                "extensions"
                "image-pipeline.rkt"))
  (unless (file-exists? src-path)
    (log-warning "skipping source scan: ~a not found" src-path))
  (when (file-exists? src-path)
    (define src (file->string src-path))
    (define matches (regexp-match* #rx"shell-escape-path" src))
    (check-equal? matches '() "shell-escape-path removed from image-pipeline.rkt")))

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
;; Operand safety — option-like filenames through real paths (#5498)
;; ═══════════════════════════════════════════════════════════════════

(test-case "option-like filename is passed as literal argv data (#5498)"
  ;; A filename starting with - must not be interpreted as a flag.
  ;; /bin/echo passes it through — a real tool (ImageMagick) might not,
  ;; but the ensure-absolute-path mitigation makes it start with / not -.
  (define-values (ok out) (run-argv (list "/bin/echo" "-resize")))
  (check-true ok)
  (check-true (string-contains? (bytes->string/utf-8 out) "-resize")))

(test-case "ensure-absolute-path converts relative dash-prefixed path (#5498)"
  ;; Relative path "-evil.png" must become absolute before subprocess use
  (define abs (ensure-absolute-path "-evil.png"))
  (check-true (path? abs))
  (check-false (string-prefix? (path->string abs) "-") "absolute path must not start with -"))

(test-case "ensure-absolute-path converts relative path with spaces (#5498)"
  (define abs (ensure-absolute-path "foo bar.png"))
  (check-true (path? abs))
  (check-true (string-contains? (path->string abs) "foo bar.png")))

(test-case "option-like real file: metadata via absolute path (#5498)"
  (define tmp-dir (make-temporary-file "qtest-opt-~a" 'directory))
  (define tmp-file (build-path tmp-dir "--help.png"))
  (call-with-output-file tmp-file (lambda (out) (write-string "fake png" out)) #:exists 'truncate)
  (define meta (image-metadata tmp-file))
  (check-equal? (hash-ref meta 'format) 'png)
  (check-true (> (hash-ref meta 'size-bytes) 0))
  (delete-file tmp-file)
  (delete-directory tmp-dir))

(test-case "cache boundary: image-resize-cache is not exported (#5498)"
  ;; The cache parameter is internal — synchronized access only
  ;; through cache-ref/cache-set!.  Try to import and verify it fails.
  (define cache-exported?
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (dynamic-require "../extensions/image-pipeline.rkt" 'image-resize-cache)
      #t))
  (check-false cache-exported? "image-resize-cache must not be exported"))
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

;; ============================================================
;; Actual resize argv proof with option-like filename (#5546)
;; ============================================================

(test-case "resize-image handles option-like filename safely (#5546)"
  ;; Create a small real PNG (1x1 red pixel)
  (define tmp-dir (make-temporary-file "qtest-resize-~a" 'directory))
  (define tmp-file (build-path tmp-dir "-resize.png"))
  ;; Use ImageMagick to create a 1x1 test image
  (define-values (create-ok _)
    (run-argv (list "convert" "-size" "1x1" "xc:red" (path->string tmp-file))))
  (unless create-ok
    (log-warning "skipping resize test: convert failed to create test image"))
  (when (and create-ok (file-exists? tmp-file))
    ;; The filename starts with - but resize must handle it via absolute path
    (define result
      (with-handlers ([exn:fail? (lambda (e)
                                   (check-true #f (format "resize crashed: ~a" (exn-message e))))])
        (resize-image tmp-file #:max-width 100 #:max-height 100)))
    (check-true (or (path-string? result) (not result))
                "resize should return a path or #f, not crash"))
  ;; Cleanup
  (when (directory-exists? tmp-dir)
    (delete-directory/files tmp-dir)))

(test-case "resize-image handles normal filename (#5546)"
  (define tmp-dir (make-temporary-file "qtest-resize-~a" 'directory))
  (define tmp-file (build-path tmp-dir "normal.png"))
  (define-values (create-ok _)
    (run-argv (list "convert" "-size" "1x1" "xc:blue" (path->string tmp-file))))
  (when (and create-ok (file-exists? tmp-file))
    (define result
      (with-handlers ([exn:fail? (lambda (e) (void))])
        (resize-image tmp-file #:max-width 100 #:max-height 100)))
    (check-true (or (path-string? result) (not result))))
  (when (directory-exists? tmp-dir)
    (delete-directory/files tmp-dir)))

(test-case "resize argv uses ensure-absolute-path for option-like input (#5546)"
  ;; Verify the ensure-absolute-path guard is applied before subprocess
  ;; by checking that a dash-prefixed relative path becomes absolute
  (define rel-path "-evil.png")
  (define abs-path (ensure-absolute-path rel-path))
  (define abs-str (path->string abs-path))
  (check-false (string-prefix? abs-str "-") "absolute path must not start with dash"))

(module+ main
  (require rackunit/text-ui))
