#lang racket/base

;; q/tui/diagnostic-surface.rkt — BUG-0098 (R98): bounded, terminal-safe
;; diagnostic preview for the TUI status line.
;;
;; The installed stderr-guard callback (tui-init.rkt) used to show the first
;; 160 characters of an error record with no clipping marker and no way to
;; reach the rest of the record. This module keeps the compact preview but
;; makes it honest:
;;
;;  - SANITIZED: C0 controls (except newline) and DEL render as caret
;;    notation, C1 code points render as U+FFFD — no terminal control
;;    injection. Racket strings are codepoint sequences, so nothing is ever
;;    re-encoded: UTF-8 cannot split and CJK/astral text is untouched.
;;  - MULTILINE: newline is preserved by the sanitizer; the single-line
;;    status surface shows it as the visible literal "\n" (count preserved).
;;    The full record keeps its real newlines in the guard's spill file.
;;  - GRAPHEME/CELL SAFE: the preview is cut only at grapheme-cluster
;;    boundaries (via tui/char-width.rkt's grapheme spans, plus an explicit
;;    back-off for cluster-extending characters that the fallback span does
;;    not cover, e.g. ZWJ and emoji skin-tone modifiers) and stays within
;;    BOTH budgets of 160 code points and 160 terminal cells.
;;  - HONEST CLIPPING: when anything was cut, an explicit truncation marker
;;    plus a pointer to the guard's REAL spill path is appended, so the
;;    recovery tail beyond the preview is inspectable via that path.
;;  - HONEST CAPTURE: a #f captured-path means the guard could not write the
;;    spill record; the status then says the full log is UNAVAILABLE and
;;    never claims "full log: <path>" for a log that does not exist.
;;
;; Memory stays bounded: the result is one bounded status string; ownership
;; of the full record remains with the guard's private spill file.

(require racket/contract
         racket/string
         (only-in "char-width.rkt" grapheme-span-at string-visible-width))

(provide (contract-out
          [sanitize-diagnostic-text (-> string? string?)]
          [diagnostic-status-line (-> string? (or/c #f path-string?) string?)]))

;; Compact preview budget (BUG-0098 keeps the existing 160-char scale).
(define preview-max-chars 160) ; code points
(define preview-max-cells 160) ; terminal cells

;; C0/C1 neutralization. #\newline is preserved (multiline records keep
;; their shape); every other C0 control and DEL becomes caret notation; C1
;; code points become U+FFFD.
(define (sanitize-char c)
  (define cp (char->integer c))
  (cond
    [(char=? c #\newline) (string c)]
    [(< cp 32) (string #\^ (integer->char (+ cp 64)))] ; ^@..^_ (ESC -> ^[)
    [(= cp 127) "^?"]                                  ; DEL
    [(<= 128 cp 159) "\uFFFD"]                         ; C1
    [else (string c)]))

(define (sanitize-diagnostic-text s)
  (apply string-append (for/list ([c (in-string s)]) (sanitize-char c))))

;; The status bar renders ONE line; a raw newline would corrupt the frame.
;; Newlines are shown as the visible two-char literal "\n" (count kept).
;; Runs after sanitize, so no control character remains either way.
(define (flatten-for-status s)
  (string-replace s "\n" "\\n"))

;; Cluster-extending characters that char-width.rkt's fallback grapheme span
;; does not treat as extending; used to back the cut off to a true boundary.
(define (cut-extends-cluster? c)
  (define cp (char->integer c))
  (or (memq (char-general-category c) '(mn mc me)) ; combining marks
      (= cp #x200D)                                ; ZWJ
      (<= #x1F3FB cp #x1F3FF)))                    ; emoji skin-tone modifiers

;; safe-cut : string? -> (values string? boolean?)
;; Longest prefix of s within BOTH budgets; cuts only at grapheme boundaries.
;; Second value is #t when anything was cut.
(define (safe-cut s)
  (let loop ([i 0] [chars 0] [cells 0])
    (cond
      [(>= i (string-length s)) (values s #f)]
      [else
       (define span (grapheme-span-at s i))
       (define end (min (+ i span) (string-length s)))
       (define g (substring s i end))
       (define g-chars (string-length g))
       (define g-cells (string-visible-width g))
       (if (and (<= (+ chars g-chars) preview-max-chars)
                (<= (+ cells g-cells) preview-max-cells))
           (loop end (+ chars g-chars) (+ cells g-cells))
           (let back ([cut i])
             (cond
               [(zero? cut) (values "" #t)]
               ;; never leave a dangling extender, never drop an extender
               ;; while keeping (a prefix of) its base
               [(cut-extends-cluster? (string-ref s cut)) (back (sub1 cut))]
               [(cut-extends-cluster? (string-ref s (sub1 cut))) (back (sub1 cut))]
               [else (values (substring s 0 cut) #t)])))])))

;; diagnostic-status-line : string? (or/c #f path-string?) -> string?
;; Full status-line text for one error record. captured-path is the guard's
;; real spill path when THIS record was captured to the spill file, #f when
;; the spill write failed. Truncated records carry an explicit marker plus a
;; discoverable pointer; failed captures are disclosed, never faked.
(define (diagnostic-status-line msg captured-path)
  (define flat (flatten-for-status (sanitize-diagnostic-text msg)))
  (define-values (preview truncated?) (safe-cut flat))
  (define path-text
    (and captured-path
         (sanitize-diagnostic-text
          (if (path? captured-path) (path->string captured-path) captured-path))))
  (string-append
   "error: " preview
   (cond
     [(and truncated? path-text)
      (string-append "... [truncated; full log: " path-text "]")]
     [(and truncated? (not path-text))
      "... [truncated; full log UNAVAILABLE: spill capture failed]"]
     [(and (not truncated?) (not path-text))
      " [warning: spill capture failed; full log UNAVAILABLE]"]
     [else ""])))
