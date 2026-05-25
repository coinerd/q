#lang racket

;;; tests/test-image-pipeline.rkt — Image pipeline tests (#5268)

(require rackunit
         rackunit/text-ui
         racket/file
         "../extensions/image-pipeline.rkt")

(define image-pipeline-tests
  (test-suite "image pipeline"

    ;; ============================================================
    ;; Tool detection
    ;; ============================================================

    (test-case "detect-image-tools returns a list"
      (reset-image-tools!)
      (define tools (detect-image-tools))
      (check-true (list? tools))
      (for ([t (in-list tools)])
        (check-true (symbol? t))))

    (test-case "available-image-tools caches result"
      (reset-image-tools!)
      (define t1 (available-image-tools))
      (define t2 (available-image-tools))
      (check-equal? t1 t2))

    (test-case "reset-image-tools! clears cache"
      (reset-image-tools!)
      (define t1 (available-image-tools))
      (reset-image-tools!)
      (define t2 (available-image-tools))
      (check-true (list? t1))
      (check-true (list? t2)))

    ;; ============================================================
    ;; Shell escaping
    ;; ============================================================

    (test-case "shell-escape-path wraps in single quotes"
      (check-equal? (shell-escape-path "/tmp/foo.png") "'/tmp/foo.png'"))

    (test-case "shell-escape-path escapes embedded single quotes"
      (check-equal? (shell-escape-path "/tmp/it's.png") "'/tmp/it'\\''s.png'"))

    (test-case "shell-escape-path handles path objects"
      (check-equal? (shell-escape-path (string->path "/tmp/bar.jpg")) "'/tmp/bar.jpg'"))

    ;; ============================================================
    ;; Supported image file detection
    ;; ============================================================

    (test-case "supported-image-file? recognizes common formats"
      (check-true (supported-image-file? "test.png"))
      (check-true (supported-image-file? "photo.jpg"))
      (check-true (supported-image-file? "photo.jpeg"))
      (check-true (supported-image-file? "anim.gif"))
      (check-true (supported-image-file? "image.webp")))

    (test-case "supported-image-file? rejects non-image formats"
      (check-false (supported-image-file? "test.rkt"))
      (check-false (supported-image-file? "data.json"))
      (check-false (supported-image-file? "script.sh"))
      (check-false (supported-image-file? "Makefile")))

    ;; ============================================================
    ;; Token estimation
    ;; ============================================================

    (test-case "estimate-image-tokens for small images"
      (check-equal? (estimate-image-tokens 256 256) 85))

    (test-case "estimate-image-tokens for large images"
      (check-equal? (estimate-image-tokens 2048 2048) 1360))

    (test-case "estimate-image-tokens for zero dimensions"
      (check-equal? (estimate-image-tokens 0 0) 85))

    ;; ============================================================
    ;; Image metadata
    ;; ============================================================

    (test-case "image-metadata on test file reports format and size"
      (define tmp (make-temporary-file "qtest~a.png"))
      (call-with-output-file tmp (lambda (out) (write-string "PNG test data" out)) #:exists 'truncate)
      (define meta (image-metadata tmp))
      (check-equal? (hash-ref meta 'format) 'png)
      (check-true (> (hash-ref meta 'size-bytes) 0))
      (check-true (string? (hash-ref meta 'path)))
      (delete-file tmp))

    (test-case "image-metadata errors on missing file"
      (check-exn exn:fail? (lambda () (image-metadata "/nonexistent/file.png"))))

    ;; ============================================================
    ;; Resize cache
    ;; ============================================================

    (test-case "image-resize-cache is a parameter with a hash"
      (check-true (hash? (image-resize-cache))))

    ;; ============================================================
    ;; Configuration parameters
    ;; ============================================================

    (test-case "max-image-width defaults to 2048"
      (check-equal? (max-image-width) 2048))

    (test-case "max-image-height defaults to 2048"
      (check-equal? (max-image-height) 2048))

    (test-case "max-image-bytes defaults to 5MB"
      (check-equal? (max-image-bytes) (* 5 1024 1024)))

    (test-case "parameters are configurable"
      (parameterize ([max-image-width 1024]
                     [max-image-height 768]
                     [max-image-bytes 1024])
        (check-equal? (max-image-width) 1024)
        (check-equal? (max-image-height) 768)
        (check-equal? (max-image-bytes) 1024)))))

(module+ main
  (run-tests image-pipeline-tests))
