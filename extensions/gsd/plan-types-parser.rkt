#lang racket/base

;; extensions/gsd/plan-types-parser.rkt — Markdown parsing for GSD plans
;;
;; Extracted from plan-types.rkt to keep the parsing code in untyped Racket
;; while plan-types.rkt itself is #lang typed/racket.
;; Returns raw hash tables and values — plan-types.rkt wraps into structs.

(require racket/string
         racket/list)

;; ============================================================
;; Parsing helpers
;; ============================================================

;; Strip surrounding backticks and whitespace from a path token.
;; `q/foo.rkt`, `` `q/foo.rkt` `` and triple-backtick forms all yield
;; `q/foo.rkt`.
(define (strip-backticks s)
  (cond
    [(>= (string-length s) 6)
     (define triple-back (string-prefix? s "```"))
     (define triple-end (string-suffix? s "```"))
     (if (and triple-back triple-end)
         (string-trim (substring s 3 (- (string-length s) 3)))
         (if (and (string-prefix? s "`") (string-suffix? s "`"))
             (string-trim (substring s 1 (- (string-length s) 1)))
             s))]
    [(>= (string-length s) 2)
     (if (and (string-prefix? s "`") (string-suffix? s "`"))
         (string-trim (substring s 1 (- (string-length s) 1)))
         s)]
    [else s]))

;; Strip a trailing parenthetical annotation used in wave docs, e.g.
;;   - File: q/docs/reports/test-regression-log.md (new: full-regression evidence log)
;; The annotation is explanatory prose, not part of the path. A path like
;; `q/foo (bar)/x.rkt` (parentheses INSIDE the path, no space before them)
;; is preserved because the annotation form always has " (" (space + paren).
(define (strip-trailing-paren-annotation s)
  (define paren-pos (regexp-match-positions #rx" \\(" s))
  (if (and paren-pos
           ;; only strip when the paren segment runs to end-of-line (annotation)
           (string-suffix? (string-trim s) ")"))
      (string-trim (substring s 0 (car (car paren-pos))))
      s))

;; Strip a trailing bracket annotation (BUG-0025, v1.00.18 W1): wave docs
;; mark delivery intent after the declared path, e.g.
;;   - File: q/tests/foo.rkt  [NEW]
;;   - File: q/docs/design.md [NEW, design record]
;; Any whitespace-separated "[...]" segment running to end-of-line is
;; annotation metadata about the delivery, not part of the path. Interior
;; brackets glued to the path (`q/foo[1]/x.rkt`) are preserved: they have
;; no separating space and do not terminate the string.
(define (strip-trailing-bracket-annotation s)
  (define bracket-pos (regexp-match-positions #rx" \\[[^\\[\n]*\\] *$" s))
  (if bracket-pos
      (string-trim (substring s 0 (car (car bracket-pos))))
      s))

;; Strip trailing annotation prose — parenthetical and/or bracketed — to a
;; fixpoint, so mixed forms like `q/x.md (new: log) [NEW]` fully clean.
;; Every iteration strictly shortens the string, so the recursion terminates.
(define (strip-trailing-annotations s)
  (define once (strip-trailing-paren-annotation (strip-trailing-bracket-annotation s)))
  (if (string=? once s)
      s
      (strip-trailing-annotations once)))

;; Clean file path: strip surrounding backticks/whitespace and trailing
;; annotation prose ("[NEW]", "[NEW, design record]", "(new: evidence log)")
;; to a fixpoint, so combined forms — "`q/foo.rkt` [NEW]" — also clean.
;; Before BUG-0025 (v1.00.17 W0) bracket annotations survived into the
;; declared path ("q/tests/foo.rkt  [NEW]" parsed verbatim), so the delivery
;; verifier never matched real files and failed waves with
;; "no wave target files changed" despite green delivery.
(define (clean-file-path s)
  (define once (strip-trailing-annotations (strip-backticks (string-trim s))))
  (if (string=? once s)
      once
      (clean-file-path once)))

;; Split a comma-separated declared-path list on ANNOTATION-AWARE commas
;; (BUG-0025): a comma inside a [...] or (...) group belongs to annotation
;; prose ("[NEW, design record]", "(new: a, b)"), not to the separator.
;; Only depth-0 commas split; pieces are trimmed like the old
;; (map string-trim (string-split s ",")) behavior.
(define (split-declared-paths s)
  (define (go chars depth seg acc)
    (cond
      [(null? chars) (reverse (cons (apply string (reverse seg)) acc))]
      [else
       (define c (car chars))
       (cond
         [(and (char=? c #\,) (zero? depth))
          (go (cdr chars) depth '() (cons (apply string (reverse seg)) acc))]
         [(or (char=? c #\[) (char=? c #\()) (go (cdr chars) (add1 depth) (cons c seg) acc)]
         [(or (char=? c #\]) (char=? c #\))) (go (cdr chars) (max 0 (sub1 depth)) (cons c seg) acc)]
         [else (go (cdr chars) depth (cons c seg) acc)])]))
  (map string-trim (go (string->list s) 0 '() '())))

;; Parse structured fields from wave document content.
(define (parse-wave-content content)
  (define lines (string-split content "\n"))
  (define n (length lines))
  (define root-cause "")
  (define files '())
  (define verify-cmd "")
  (define done-criteria '())
  (define in-files-section #f)
  (for ([line lines]
        [i (in-naturals)])
    (define trimmed (string-trim line))
    (when (regexp-match? #rx"^## " trimmed)
      (set! in-files-section (string-prefix? trimmed "## Files")))
    (cond
      [(string-prefix? trimmed "## Verify")
       (define after
         (for/list ([j (in-range (add1 i) (min n (+ i 5)))]
                    #:when (and (> (string-length (string-trim (list-ref lines j))) 0)
                                (not (string-contains? (list-ref lines j) "```"))))
           (string-trim (list-ref lines j))))
       (when (and (string=? verify-cmd "") (not (null? after)))
         (set! verify-cmd (string-join after "; ")))]
      [(regexp-match #rx"^- +[Rr]oot *[Cc]ause *: *(.+)$" line)
       =>
       (lambda (m) (set! root-cause (string-trim (cadr m))))]
      [(regexp-match #rx"^- +[Ff]iles *: *(.+)$" line)
       =>
       (lambda (m)
         ;; Comma-separated paths, but commas inside bracket/paren
         ;; annotations ("[NEW, design record]") are prose, not
         ;; separators (BUG-0025).
         (define paths (split-declared-paths (cadr m)))
         (set! files (append files (map clean-file-path paths))))]
      [(regexp-match #rx"^- +[Ff]ile *: *(.+)$" line)
       =>
       (lambda (m) (set! files (append files (list (clean-file-path (string-trim (cadr m)))))))]
      [(and in-files-section (regexp-match #rx"^- +(.+)$" line))
       =>
       (lambda (m) (set! files (append files (list (clean-file-path (string-trim (cadr m)))))))]
      [(regexp-match #rx"^- +[Vv]erify *: *(.+)$" line)
       =>
       (lambda (m) (set! verify-cmd (string-trim (cadr m))))]
      [(regexp-match #rx"^- +[Dd]one *: *(.+)$" line)
       =>
       (lambda (m) (set! done-criteria (append done-criteria (list (string-trim (cadr m))))))]))
  (hasheq 'root-cause root-cause 'files files 'verify verify-cmd 'done done-criteria))

;; Parse a single wave section → raw data hash (not a gsd-wave struct).
;; plan-types.rkt wraps this into a gsd-wave.
;;
;; Header tolerance (v1.00.14 hotfix): the wave header separator may be a
;; colon (`## Wave 0: Title`), an em/en-dash (`## Wave 1 — Title`), or a
;; hyphen run (`## Wave 2 - Title`). Previously ONLY the colon form matched;
;; any other separator silently fell back to index 0 / empty title, so a
;; plan written with em-dash headers normalized to indices (0 0 0 ...)
;; and `/go` rejected it with "Wave indices not sequential".
(define (parse-wave-section-raw lines)
  (define header (car lines))
  (define body-lines (cdr lines))
  (define header-match
    (regexp-match #px"^## +[Ww]ave +([0-9]+)(?: *: *| *[—–] *| +-+ *)(.+)$" header))
  (define idx
    (if header-match
        (string->number (cadr header-match))
        0))
  (define title
    (if header-match
        (string-trim (caddr header-match))
        ""))
  (define fields (parse-wave-content (string-join body-lines "\n")))
  (hasheq 'index
          idx
          'title
          title
          'root-cause
          (hash-ref fields 'root-cause "")
          'files
          (hash-ref fields 'files '())
          'verify
          (hash-ref fields 'verify "")
          'done
          (hash-ref fields 'done '())))

;; Parse PLAN.md content → list of raw data hashes.
(define (parse-waves-from-markdown-raw md-text)
  (define lines (string-split md-text "\n"))
  (define wave-starts
    (for/list ([line lines]
               [idx (in-naturals)]
               #:when (regexp-match #rx"^## +[Ww]ave +[0-9]+" line))
      idx))
  (define wave-end-idxs
    (if (< (length wave-starts) 2)
        (list (sub1 (length lines)))
        (append (map sub1 (cdr wave-starts)) (list (sub1 (length lines))))))
  (for/list ([start wave-starts]
             [end wave-end-idxs])
    (parse-wave-section-raw (take (drop lines start) (add1 (- end start))))))

;; ============================================================
;; Provide
;; ============================================================

(provide parse-waves-from-markdown-raw
         parse-wave-content
         clean-file-path)
