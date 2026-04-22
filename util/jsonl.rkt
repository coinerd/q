#lang racket/base

;;; util/jsonl.rkt — JSONL file helpers (append-only, crash-safe)
;;;
;;; Operations:
;;;   jsonl-append!        — append one jsexpr line
;;;   jsonl-append-entries! — append multiple jsexprs atomically (all-or-nothing)
;;;   jsonl-read-all        — read all lines, error on invalid
;;;   jsonl-read-all-valid  — read only valid lines (skip partial/corrupted)
;;;   jsonl-line-valid?     — check if a line is complete valid JSON

(provide jsonl-append!
         jsonl-append-entries!
         jsonl-read-all
         jsonl-read-all-valid
         jsonl-read-all-valid-with-count
         jsonl-read-last
         jsonl-line-valid?)

(require json
         racket/port
         racket/string
         racket/file
         racket/list
         racket/path)

;; ── Line validation ──

(define (jsonl-line-valid? line)
  ;; Returns #t if `line` (a string) is complete, valid JSON.
  ;; Empty/whitespace-only strings return #f.
  (and (string? line)
       (> (string-length (string-trim line)) 0)
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (define _ (read-json (open-input-string line)))
         #t)))

;; ── Append ──

(define (jsonl-append! path entry)
  ;; Append a single jsexpr as one JSON line to file.
  ;; Creates the file (and parent dirs) if missing.
  (ensure-parent-dirs! path)
  (call-with-output-file path
                         (lambda (out)
                           (write-json entry out)
                           (newline out))
                         #:mode 'text
                         #:exists 'append))

(define (jsonl-append-entries! path entries)
  ;; Append multiple entries atomically (all-or-nothing).
  ;; Builds a single string blob and writes it in one filesystem operation.
  ;; Does nothing if entries is empty (and file doesn't exist yet).
  (when (null? entries)
    (void))
  (unless (null? entries)
    (ensure-parent-dirs! path)
    ;; Build the blob in memory so we write atomically
    (define blob
      (with-output-to-string (lambda ()
                               (for ([entry (in-list entries)])
                                 (write-json entry)
                                 (newline)))))
    ;; Append the blob in one go
    (call-with-output-file path (lambda (out) (display blob out)) #:mode 'text #:exists 'append)))

;; ── Read ──

(define (jsonl-read-all path)
  ;; Read all complete JSONL lines from file.
  ;; Raises exn:fail if any line is invalid JSON.
  ;; Returns '() if file does not exist.
  (if (not (file-exists? path))
      '()
      (call-with-input-file
       path
       (lambda (in)
         (for/list ([line (in-lines in)]
                    #:when (non-empty-string? (string-trim line)))
           (with-handlers ([exn:fail?
                            (lambda (e)
                              (raise (exn:fail
                                      (format "jsonl-read-all: invalid JSON line in ~a: ~a" path line)
                                      (exn-continuation-marks e))))])
             (read-json (open-input-string line)))))
       #:mode 'text)))

(define (jsonl-read-all-valid path)
  ;; Read all valid JSONL lines, skipping partial/corrupted/empty lines.
  ;; Returns '() if file does not exist.
  (if (not (file-exists? path))
      '()
      (call-with-input-file path
                            (lambda (in)
                              (for/list ([line (in-lines in)]
                                         #:when (jsonl-line-valid? line))
                                (read-json (open-input-string line))))
                            #:mode 'text)))

(define (jsonl-read-all-valid-with-count path)
  ;; Read all valid JSONL lines, skipping partial/corrupted/empty lines.
  ;; Returns (values valid-list corrupted-count).
  ;; corrupted-count is the number of non-empty lines that failed to parse.
  (if (not (file-exists? path))
      (values '() 0)
      (call-with-input-file
       path
       (lambda (in)
         (define-values (valid corrupted)
           (for/fold ([acc '()]
                      [bad 0])
                     ([line (in-lines in)])
             (define trimmed (string-trim line))
             (cond
               [(<= (string-length trimmed) 0) (values acc bad)]
               ;; W10.3 (Q-07): cons + reverse instead of append (O(n²) → O(n))
               [(jsonl-line-valid? line) (values (cons (read-json (open-input-string line)) acc) bad)]
               [else (values acc (add1 bad))])))
         (values (reverse valid) corrupted))
       #:mode 'text)))

(define (jsonl-read-last path [max-lines 1000])
  ;; Read the last `max-lines` valid JSONL lines from file.
  ;; Efficient: seeks from end, reads only the tail.
  ;; Returns '() if file does not exist.
  (if (not (file-exists? path))
      '()
      (call-with-input-file path
                            (lambda (in)
                              ;; For very large files, seek near the end
                              (define sz (file-size path))
                              (when (> sz (* max-lines 2000))
                                ;; Assume ~2KB per line on average; seek to a safe offset
                                (file-position in (max 0 (- sz (* max-lines 4000))))
                                ;; Discard the first partial line after seek
                                (read-line in))
                              ;; Now read remaining lines
                              (define all-lines
                                (for/list ([line (in-lines in)]
                                           #:when (non-empty-string? (string-trim line)))
                                  line))
                              ;; Take only the last max-lines
                              (define tail
                                (if (> (length all-lines) max-lines)
                                    (take-right all-lines max-lines)
                                    all-lines))
                              ;; Parse each line, filter out failures
                              (filter values
                                      (for/list ([line (in-list tail)])
                                        (with-handlers ([exn:fail? (lambda (e) #f)])
                                          (read-json (open-input-string line))))))
                            #:mode 'text)))

;; ── Internal helpers ──

(define (ensure-parent-dirs! path)
  ;; Create parent directories of path if they don't exist.
  (define dir (path-only path))
  (when (and dir (not (directory-exists? dir)))
    (make-directory* dir)))
