#lang racket/base

;; tools/builtins/delete-lines.rkt — line-range deletion tool
;;
;; Exports:
;;   tool-delete-lines : (hash [exec-ctx]) -> tool-result?
;;   Arguments: path (string), start-line (integer), end-line (integer)
;;   Returns:  tool-result with success or error details
;;
;; v0.21.10 (F2+F9): Addresses edit failures from the 500-char old-text
;; limit by providing a line-number-based deletion that avoids text matching
;; entirely. The LLM reads the file, identifies line numbers, and calls
;; delete-lines instead of chunking large removals.
;;
;; Safeguards:
;;   - Pre-edit backup to ~/.q/edit-backups/ (same as edit tool)
;;   - Post-edit line-count delta check with auto-revert on corruption
;;   - Safe-mode path check (same as edit tool)

(require racket/file
         racket/string
         (only-in racket/list last drop take)
         (only-in "../tool.rkt" make-success-result make-error-result)
         (only-in "../../util/path-helpers.rkt" expand-home-path)
         (only-in "builtin-helpers.rkt" require-safe-path!)
         (only-in "../../util/error-sanitizer.rkt" sanitize-error-message))

(require racket/contract)
(provide (contract-out [tool-delete-lines (-> hash? any/c)]))

;; --------------------------------------------------
;; Constants
;; --------------------------------------------------

(define MAX-BACKUPS-PER-FILE 10)

;; --------------------------------------------------
;; Backup helpers (shared pattern with edit.rkt)
;; --------------------------------------------------

(define (ensure-backup-dir)
  (define dir (build-path (find-system-path 'home-dir) ".q" "edit-backups"))
  (unless (directory-exists? dir)
    (make-directory* dir)
    (file-or-directory-permissions dir #o700))
  dir)

(define (save-backup path-str content)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "delete-lines/backup: ~a" (exn-message e)))
                               #f)])
    (define dir (ensure-backup-dir))
    (define basename (file-name-from-path path-str))
    (define timestamp (number->string (abs (current-milliseconds))))
    (define backup-name (format "~a_~a" timestamp basename))
    (define backup-path (build-path dir backup-name))
    (display-to-file content backup-path #:exists 'replace)
    (prune-old-backups dir basename)
    (path->string backup-path)))

(define (file-name-from-path p)
  (define fname
    (if (string? p)
        p
        (path->string p)))
  (define parts (regexp-split #rx"/" fname))
  (if (null? parts)
      "unknown"
      (last parts)))

(define (prune-old-backups dir basename)
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning (format "delete-lines/prune: ~a" (exn-message e)))
                               (void))])
    (define all (directory-list dir))
    (define matching
      (filter (lambda (f) (string-suffix? (path->string f) (format "_~a" basename)))
              (sort (map path->string all) string>?)))
    (when (> (length matching) MAX-BACKUPS-PER-FILE)
      (for ([f (in-list (drop matching MAX-BACKUPS-PER-FILE))])
        (delete-file (build-path dir f))))))

;; --------------------------------------------------
;; Line helpers
;; --------------------------------------------------

(define (line-count s)
  (length (string-split s "\n" #:trim? #f)))

(define (get-lines content)
  (string-split content "\n" #:trim? #f))

;; --------------------------------------------------
;; Main tool function
;; --------------------------------------------------

(define (tool-delete-lines args [exec-ctx #f])
  (define raw-path (hash-ref args 'path #f))
  (define path-str (and raw-path (expand-home-path raw-path)))
  (cond
    [(not path-str) (make-error-result "Missing required argument: path")]
    [else
     (define start-line (hash-ref args 'start-line #f))
     (define end-line (hash-ref args 'end-line #f))
     (cond
       [(not start-line) (make-error-result "Missing required argument: start-line")]
       [(not end-line) (make-error-result "Missing required argument: end-line")]
       [(not (exact-integer? start-line))
        (make-error-result (format "start-line must be an integer, got: ~v" start-line))]
       [(not (exact-integer? end-line))
        (make-error-result (format "end-line must be an integer, got: ~v" end-line))]
       [else
        ;; Defense-in-depth: safe-mode path check (same as edit tool)
        (cond
          [(require-safe-path! path-str "delete-lines")
           =>
           (lambda (err) (make-error-result err))]
          [(not (file-exists? path-str)) (make-error-result (format "File not found: ~a" path-str))]

          [else
           (define content (file->string path-str))
           (define lines (get-lines content))
           (define total-lines (length lines))

           (cond
             ;; Validate line range
             [(< start-line 1)
              (make-error-result (format "start-line ~a is out of range. File has ~a lines (1-based)."
                                         start-line
                                         total-lines))]
             [(> end-line total-lines)
              (make-error-result
               (format "end-line ~a exceeds file length. File has ~a lines (1-based)."
                       end-line
                       total-lines))]
             [(> start-line end-line)
              (make-error-result
               (format "start-line (~a) must be ≤ end-line (~a)" start-line end-line))]

             [else
              ;; Extract preview of deleted lines
              (define deleted-lines (take (drop lines (sub1 start-line)) (- end-line start-line -1)))
              (define preview
                (if (<= (length deleted-lines) 5)
                    (string-join deleted-lines "\n")
                    (string-append (string-join (take deleted-lines 3) "\n")
                                   "\n  ..."
                                   (last deleted-lines))))

              ;; Save backup
              (define backup-path (save-backup path-str content))

              ;; Build new content: lines before start + lines after end
              (define before (take lines (sub1 start-line)))
              (define after (drop lines end-line))
              (define new-lines (append before after))
              (define new-content (string-join new-lines "\n"))

              ;; Post-edit line-count integrity check
              (define expected-deleted (- end-line start-line -1))
              (define actual-deleted (- total-lines (length new-lines)))

              (cond
                [(not (= actual-deleted expected-deleted))
                 ;; Auto-revert
                 (with-handlers ([exn:fail? (lambda (e)
                                              (log-warning (format "delete-lines/auto-revert: ~a"
                                                                   (exn-message e)))
                                              (void))])
                   (display-to-file content path-str #:exists 'replace))
                 (make-error-result
                  (format
                   "Delete reverted: line count mismatch (expected to delete ~a, actually deleted ~a). File restored."
                   expected-deleted
                   actual-deleted))]

                [else
                 ;; Write new content
                 (with-handlers ([exn:fail:filesystem?
                                  (lambda (e)
                                    (make-error-result (sanitize-error-message
                                                        (format "Write error: ~a"
                                                                (exn-message e)))))])
                   (display-to-file new-content path-str #:exists 'replace)

                   (make-success-result
                    (list (hasheq
                           'type
                           "text"
                           'text
                           (format "Deleted lines ~a-~a from ~a (~a lines removed)\nPreview:\n~a"
                                   start-line
                                   end-line
                                   path-str
                                   expected-deleted
                                   preview)))
                    (hasheq 'path
                            path-str
                            'lines-deleted
                            expected-deleted
                            'start-line
                            start-line
                            'end-line
                            end-line
                            'remaining-lines
                            (length new-lines)
                            'backup
                            (or backup-path ""))))])])])])]))
