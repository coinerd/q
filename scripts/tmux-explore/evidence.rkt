#lang racket/base

;; scripts/tmux-explore/evidence.rkt — Evidence manifest with SHA and digests.
;;
;; F-03/F-08: Production-shaped evidence must carry a full SHA, artifact
;; digests, and inventory so verification can prove provenance without
;; guessing from absent mock text.

(require json
         racket/file
         racket/format
         racket/list
         racket/path
         racket/port
         racket/string
         "../../util/credential-redaction.rkt")

(provide sha256-file
         compute-directory-digest
         build-evidence-manifest
         write-evidence-manifest!
         verify-evidence-manifest
         leak-scan-passed?)

;; Compute SHA-256 of a single file using sha256sum with file argument.
;; Uses concurrent pipe draining to avoid deadlock.
(define (sha256-file path)
  (define sha256sum (find-executable-path "sha256sum"))
  (define-values (sp stdout stdin stderr) (subprocess #f #f #f sha256sum (path->string path)))
  (close-output-port stdin)
  (define out-box (box ""))
  (define err-box (box ""))
  (define out-reader (thread (lambda () (set-box! out-box (port->string stdout)))))
  (define err-reader (thread (lambda () (set-box! err-box (port->string stderr)))))
  (subprocess-wait sp)
  (thread-wait out-reader)
  (thread-wait err-reader)
  (close-input-port stdout)
  (close-input-port stderr)
  (define parts (string-split (string-trim (unbox out-box))))
  (if (pair? parts)
      (car parts)
      #f))

;; Compute a combined digest for artifact files listed explicitly.
(define (compute-directory-digest paths)
  (define hashes
    (for/list ([p (in-list paths)]
               #:when (file-exists? p))
      (format "~a ~a" (or (sha256-file p) "ERROR") (path->string p))))
  (define combined (string-join (sort hashes string<?) "\n"))
  (define sha256sum (find-executable-path "sha256sum"))
  (define-values (sp stdout stdin stderr) (subprocess #f #f #f sha256sum))
  (display combined stdin)
  (close-output-port stdin)
  (define result (port->string stdout))
  (close-input-port stdout)
  (close-input-port stderr)
  (subprocess-wait sp)
  (define parts (string-split (string-trim result)))
  (if (pair? parts)
      (car parts)
      "ERROR"))

;; List artifact files in a directory (non-recursive).
(define (list-artifact-files dir)
  (for/list ([f (in-list (directory-list dir))]
             #:when (file-exists? (build-path dir f)))
    (build-path dir f)))

;; Build an evidence manifest hash from a scenario result and output dir.
(define (build-evidence-manifest #:tag tag
                                 #:status status
                                 #:classification classification
                                 #:repo-sha repo-sha
                                 #:version version
                                 #:output-dir output-dir
                                 #:scenario-count scenario-count)
  (define files (list-artifact-files output-dir))
  (define digest (compute-directory-digest files))
  (define artifacts
    (for/list ([f (in-list files)])
      (hasheq 'path (path->string (file-name-from-path f)) 'sha256 (or (sha256-file f) "ERROR"))))
  (hasheq 'schema-version
          1
          'tag
          tag
          'status
          (format "~a" status)
          'classification
          (format "~a" classification)
          'repo-sha
          repo-sha
          'version
          version
          'artifact-digest
          (or digest "ERROR")
          'artifact-count
          (length artifacts)
          'scenario-count
          scenario-count
          'artifacts
          artifacts
          'timestamp
          (current-seconds)))

;; Write an evidence manifest as JSON to a file.
(define (write-evidence-manifest! manifest path)
  (call-with-output-file path
                         (lambda (out) (write-json (redact-credential-data manifest) out))
                         #:exists 'replace)
  path)

;; Verify an evidence manifest has required fields and valid values.
(define (verify-evidence-manifest manifest)
  (define reasons '())
  (define (fail! msg)
    (set! reasons (cons msg reasons)))
  (unless (hash? manifest)
    (fail! "manifest is not a hash"))
  (when (hash? manifest)
    (unless (and (hash-ref manifest 'repo-sha #f)
                 (regexp-match? #px"^[0-9a-f]{40}$" (format "~a" (hash-ref manifest 'repo-sha #f))))
      (fail! "repo-sha is missing or not a valid 40-char hex SHA"))
    (unless (hash-ref manifest 'version #f)
      (fail! "version is missing"))
    (unless (hash-ref manifest 'artifact-digest #f)
      (fail! "artifact-digest is missing"))
    (unless (and (integer? (hash-ref manifest 'artifact-count 0))
                 (positive? (hash-ref manifest 'artifact-count 0)))
      (fail! "artifact-count is zero or missing"))
    (unless (and (integer? (hash-ref manifest 'scenario-count 0))
                 (positive? (hash-ref manifest 'scenario-count 0)))
      (fail! "scenario-count is zero or missing"))
    (unless (hash-has-key? manifest 'artifacts)
      (fail! "artifacts list is missing")))
  (if (null? reasons)
      (hasheq 'valid? #t 'reasons '())
      (hasheq 'valid? #f 'reasons (reverse reasons))))

;; Scan captured text and trace for credential leaks.
(define (leak-scan-passed? capture events)
  (define redacted-capture (redact-secrets capture))
  (define trace-text
    (with-output-to-string (lambda ()
                             (for ([event (in-list events)])
                               (when (hash? event)
                                 (display (format "~a " event)))))))
  (define redacted-trace (redact-secrets trace-text))
  ;; Pass if redaction doesn't change the already-redacted text
  ;; (meaning no live credentials were present).
  (and (equal? capture redacted-capture) (equal? trace-text redacted-trace)))
