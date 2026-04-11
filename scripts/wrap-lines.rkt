#!/usr/bin/env racket
#lang racket/base

;; wrap-lines.rkt — Racket-aware line wrapper.
;;
;; Safely wraps long lines in .rkt files.  The #1 rule is: NEVER break
;; inside a string literal.  Uses a character-level scanner to identify
;; string spans before deciding where to wrap.
;;
;; Usage:
;;   racket scripts/wrap-lines.rkt [--dry-run|--check] [--all|FILE ...]
;;
;; Modes:
;;   (default)  Fix files in place
;;   --dry-run  Print what would change, don't modify files
;;   --check    Exit 0 if all lines ≤ MAX-LINE-LENGTH, exit 1 otherwise
;;
;; Flags:
;;   --all      Scan every .rkt file in the project (skips benchmarks/)

(require racket/file
         racket/list
         racket/match
         racket/path
         racket/string)

;;; ---------------------------------------------------------------------------
;;; Configuration
;;; ---------------------------------------------------------------------------

(define MAX-LINE-LENGTH 150)

;;; ---------------------------------------------------------------------------
;;; String-span detection — character-level scanner
;;; ---------------------------------------------------------------------------

;; A string-span is a (cons start-col end-col) marking the region of a
;; string literal including its surrounding double-quotes.  We detect these
;; by scanning character-by-character so that we catch strings inside
;; larger S-expressions (which read-syntax hides from us).

(define (string-spans-on-line line)
  (let ([len (string-length line)])
    (let loop ([i 0] [in-string? #f] [string-start 0] [acc '()])
      (cond
        [(>= i len)
         ;; If we ended inside a string (multi-line), treat rest as string
         (if in-string?
             (reverse (cons (cons string-start i) acc))
             (reverse acc))]
        ;; Check for comment start — everything after ; is a comment
        [(and (not in-string?) (char=? (string-ref line i) #\;))
         (reverse acc)]  ;; rest is comment, stop scanning
        [(and (not in-string?) (char=? (string-ref line i) #\"))
         ;; Start of a string
         (loop (add1 i) #t i acc)]
        [(and in-string? (char=? (string-ref line i) #\"))
         ;; Check if escaped (preceded by odd number of backslashes)
         (if (escaped-backslash-count line i)
             (loop (add1 i) in-string? string-start acc)  ;; escaped quote
             (loop (add1 i) #f string-start
                   (cons (cons string-start (add1 i)) acc)))]  ;; end of string
        [else
         (loop (add1 i) in-string? string-start acc)]))))

;; Count consecutive backslashes before position i. Returns #t if the
;; char at i is escaped (odd number of preceding backslashes).
(define (escaped-backslash-count line i)
  (let loop ([j (sub1 i)] [count 0])
    (cond
      [(< j 0) (odd? count)]
      [(char=? (string-ref line j) #\\) (loop (sub1 j) (add1 count))]
      [else (odd? count)])))

;; Return #t if column `col` falls inside any string span on the line.
(define (inside-string? line col)
  (for/or ([sp (string-spans-on-line line)])
    (and (>= col (car sp))
         (<  col (cdr sp)))))

;;; ---------------------------------------------------------------------------
;;; Line classification helpers
;;; ---------------------------------------------------------------------------

(define (blank-line? line)
  (string=? (string-trim line) ""))

(define (comment-line? line)
  (define trimmed (string-trim line))
  (or (string-prefix? trimmed ";;")
      (string-prefix? trimmed "#;")))

;; Pure comment decoration lines like  ;; ═══════════
(define (comment-decoration? line)
  (and (comment-line? line)
       (regexp-match? #px"[═─━┅┄_]{5,}" line)))

;;; ---------------------------------------------------------------------------
;;; Wrapping strategies
;;; ---------------------------------------------------------------------------

;; Strategy 1: Comment decoration — just truncate to MAX-LINE-LENGTH.
;; These are purely visual separators; shortening them is safe.
(define (wrap-comment-decoration line)
  (substring line 0 MAX-LINE-LENGTH))

;; Strategy 2: Regular comment — try to break at a word boundary
;; near the limit, preserving the ;; prefix.
(define (wrap-comment line)
  (define indent (regexp-match #rx"^[ \t]*" line))
  (define prefix (if indent (car indent) ""))
  (define trimmed (string-trim line))
  ;; Check if it starts with ;;
  (define comment-prefix
    (if (string-prefix? trimmed ";;")
        (substring trimmed 0 (min 3 (string-length trimmed)))  ;; ";;" or ";; "
        ""))
  (if (or (string=? comment-prefix "")
          (<= (string-length line) (+ MAX-LINE-LENGTH 40)))
      ;; Not worth wrapping or too short to bother — just truncate decoration chars
      (if (comment-decoration? line)
          (wrap-comment-decoration line)
          line)
      ;; Wrap at a word boundary
      (let ()
        (define content (substring trimmed (string-length comment-prefix)))
        (define content-len (string-length content))
        (define target (- MAX-LINE-LENGTH
                          (string-length prefix)
                          (string-length comment-prefix)
                          1))
        (define break-pos (find-word-break content (max 20 target)))
        (if (and break-pos (> content-len target))
            (let* ([part1 (substring content 0 break-pos)]
                   [part2 (string-trim (substring content break-pos))]
                   [line1 (string-append prefix comment-prefix " " part1)]
                   [line2 (string-append prefix comment-prefix " " part2)])
              (list line1 line2))
            line))))

;; Find a good word boundary near `target` position in a string.
(define (find-word-break str target)
  (define len (string-length str))
  (cond
    [(>= target len) #f]
    [else
     ;; Search backwards from target for a space
     (let loop ([i target])
       (cond
         [(<= i 0) #f]
         [(char=? (string-ref str i) #\space) (add1 i)]
         [else (loop (sub1 i))]))]))

;; Strategy 3: Code line with string literal(s)
;; If the line is dominated by a single long string, decompose it into
;; (string-append "part1" "part2" ...) splitting at natural boundaries.
(define (wrap-string-line line)
  (let ([spans (string-spans-on-line line)])
    (cond
      [(null? spans) (wrap-code-line line)]  ;; no strings, use code strategy
      [(= (length spans) 1)
       ;; Single string span — decompose
       (let* ([sp (car spans)]
              [start (car sp)]
              [end (cdr sp)]
              [str-content (substring line (add1 start) (sub1 end))]
              [prefix (substring line 0 start)]
              ;; Max length for each string part, accounting for prefix + overhead
              ;; prefix + "(string-append " + "\"...\"" + ")" ≈ prefix + 18 + part
              [max-part (max 20 (- MAX-LINE-LENGTH
                                   (string-length prefix)
                                   (string-length "(string-append ")
                                   2   ;; quotes
                                   1))] ;; closing paren
              [parts (split-string-literal str-content max-part)])
         (cond
           [(<= (length parts) 1)
            ;; Can't split string — try code-level wrapping instead
            (wrap-code-line line)]
           [else
            ;; Build (string-append "p1" "p2" ...)
            ;; Format: prefix(string-append "part1"
            ;;                         "part2"
            ;;                         "part3")
            (let* ([indent (make-string (+ (string-length prefix)
                                           (string-length "(string-append "))
                                       #\space)]
                   [quoted-parts
                    (for/list ([p (in-list parts)])
                      (string-append "\"" p "\""))]
                   [first-part (car quoted-parts)]
                   [rest-parts (cdr quoted-parts)]
                   [line1 (string-append prefix "(string-append " first-part)]
                   [rest-lines
                    (for/list ([p (in-list rest-parts)])
                      (string-append indent p))]
                   [last-line (string-append (substring indent 0
                                                        (max 0 (sub1 (string-length indent))))
                                             ")")])
              (append (list line1) rest-lines (list last-line)))]))]
      [else
       ;; Multiple strings — leave as-is for safety
       line])))

;; Split a string literal content at natural boundaries.
;; Tries: newlines, then JSON structural boundaries, then at a safe length.
(define (split-string-literal content [target-len 70])
  (define len (string-length content))
  (cond
    [(<= len target-len) (list content)]
    ;; Try splitting at \n
    [(string-contains? content "\n")
     (split-at-boundary content "\n")]
    ;; Try splitting at JSON boundaries: "," or "},{"
    [(string-contains? content "\",\"")
     (split-at-boundary content "\",\"")]
    [(string-contains? content "\"},{\"")
     (split-at-boundary content "\"},{\"")]
    ;; Try splitting at comma
    [(string-contains? content ", ")
     (split-at-boundary content ", ")]
    ;; Hard split at safe length
    [else (hard-split-string content target-len)]))

(define (split-at-boundary content sep)
  (define parts (string-split content sep))
  ;; Re-add the separator to all but the last part
  (for/list ([p (in-list parts)]
             [i (in-naturals)])
    (if (< i (sub1 (length parts)))
        (string-append p sep)
        p)))

(define (hard-split-string content target-len)
  (define len (string-length content))
  (cond
    [(<= len target-len) (list content)]
    [else
     ;; Find a safe break (not in the middle of an escape sequence)
     (define break-pos
       (let loop ([i target-len])
         (cond
           [(<= i 0) target-len]
           ;; Don't split right after a backslash
           [(char=? (string-ref content (max 0 (sub1 i))) #\\)
            (loop (sub1 i))]
           [else i])))
     (cons (substring content 0 break-pos)
           (hard-split-string (substring content break-pos) target-len))]))

;; Strategy 4: General code line — wrap at safe positions (paren/space)
;; respecting string boundaries.
(define (wrap-code-line line)
  (if (<= (string-length line) MAX-LINE-LENGTH)
      line
      (let ([break-col (find-safe-break line)])
        (if (not break-col)
            line  ;; can't safely break
            (let* ([indent (or (regexp-match #rx"^[ \t]+" line) '(""))]
                   [indent-str (let ([base (car indent)])
                                 (if (string=? base "") "  " base))]
                   [line1 (substring line 0 break-col)]
                   [line2 (string-append (string-trim indent-str) "  "
                                         (string-trim (substring line break-col)))])
              (list line1 line2))))))

;; Find a safe column to break the line near MAX-LINE-LENGTH.
;; Must not be inside a string literal.
(define (find-safe-break line)
  (define len (string-length line))
  ;; Search backwards from MAX-LINE-LENGTH - 1 to leave margin
  (let loop ([col (min (sub1 len) (sub1 MAX-LINE-LENGTH))])
    (cond
      [(<= col 20) #f]  ;; don't break too early
      [(inside-string? line col) (loop (sub1 col))]
      [(char=? (string-ref line col) #\space)
       col]
      [(char=? (string-ref line col) #\))
       (add1 col)]  ;; break after the close paren
      [else (loop (sub1 col))])))

;;; ---------------------------------------------------------------------------
;;; Top-level line wrapper
;;; ---------------------------------------------------------------------------

;; Returns a list of lines (possibly more than one if wrapping occurred).
(define (wrap-line line)
  (cond
    [(<= (string-length line) MAX-LINE-LENGTH) (list line)]
    [(blank-line? line) (list line)]
    [(comment-decoration? line)
     (list (wrap-comment-decoration line))]
    [(comment-line? line)
     (ensure-list (wrap-comment line))]
    [(not (null? (string-spans-on-line line)))
     ;; Has string literal(s) — use string-safe wrapping
     (ensure-list (wrap-string-line line))]
    [else
     ;; Plain code — use code wrapping
     (ensure-list (wrap-code-line line))]))

(define (ensure-list v)
  (if (list? v) v (list v)))

;;; ---------------------------------------------------------------------------
;;; File processing
;;; ---------------------------------------------------------------------------

(define (skip-path? p)
  (define s (path->string p))
  (or (string-contains? s "/compiled/")
      (string-contains? s "/.git/")
      (string-contains? s "/benchmarks/")))

(define (collect-rkt-files base-dir)
  (sort
   (for/list ([f (in-directory base-dir)]
              #:when (and (not (skip-path? f))
                          (let ([ext (filename-extension f)])
                            (and ext (equal? (bytes->string/utf-8 ext) "rkt")))))
     f)
   path<?))

(define (process-file filepath mode)
  ;; mode: 'fix, 'dry-run, 'check
  (define lines (file->lines filepath))
  (define rel (path->string (find-relative-path (current-directory) filepath)))
  (define changes '())
  (define long-count 0)

  (for ([line (in-list lines)]
        [lineno (in-naturals 1)])
    (when (> (string-length line) MAX-LINE-LENGTH)
      (set! long-count (add1 long-count))
      (define wrapped (wrap-line line))
      (cond
        [(eq? mode 'check)
         (printf "  ~a:~a: ~a chars~n" rel lineno (string-length line))]
        [else
         (unless (and (= (length wrapped) 1)
                        (string=? (car wrapped) line))
           (set! changes
                 (cons (list lineno line wrapped) changes)))])))

  (cond
    [(eq? mode 'check)
     (when (> long-count 0)
       (printf "  TOTAL: ~a long lines in ~a~n" long-count rel))
     long-count]
    [(eq? mode 'dry-run)
     (when (not (null? changes))
       (printf "~a (~a lines to wrap):~n" rel (length changes))
       (for ([c (in-list (reverse changes))])
         (match-define (list lineno original wrapped) c)
         (printf "  line ~a:~n" lineno)
         (printf "    - ~a~n" (if (> (string-length original) 80)
                                 (string-append (substring original 0 77) "...")
                                 original))
         (for ([w (in-list wrapped)])
           (printf "    + ~a~n" (if (> (string-length w) 80)
                                   (string-append (substring w 0 77) "...")
                                   w)))))
     0]
    [else  ;; 'fix
     (when (not (null? changes))
       ;; Rebuild the file
       (define result-lines
         (let loop ([remaining lines] [lineno 1] [acc '()])
           (cond
             [(null? remaining) (reverse acc)]
             [else
              (define line (car remaining))
              (cond
                [(and (> (string-length line) MAX-LINE-LENGTH)
                      (let ([c (assoc lineno changes)])
                        (and c (not (and (= (length (caddr c)) 1)
                                         (string=? (car (caddr c)) line))))))
                 (define c (assoc lineno changes))
                 (loop (cdr remaining)
                       (add1 lineno)
                       (append (reverse (caddr c)) acc))]
                [else
                 (loop (cdr remaining)
                       (add1 lineno)
                       (cons line acc))])])))
       (display-lines-to-file result-lines filepath #:exists 'truncate)
       (printf "FIXED: ~a (~a lines wrapped)~n" rel (length changes)))
     0]))

;;; ---------------------------------------------------------------------------
;;; CLI
;;; ---------------------------------------------------------------------------

(define (usage)
  (displayln "Usage: racket scripts/wrap-lines.rkt [MODE] [FILES]")
  (displayln "")
  (displayln "Modes:")
  (displayln "  (default)  Fix files in place")
  (displayln "  --dry-run  Print what would change, don't modify files")
  (displayln "  --check    Exit 0 if all lines OK, exit 1 if any exceed limit")
  (displayln "")
  (displayln "Arguments:")
  (displayln "  --all      Scan all .rkt files in the project")
  (displayln "  FILE ...   Specific .rkt files to process"))

(define (main)
  (define args (vector->list (current-command-line-arguments)))

  (when (or (null? args)
            (member "--help" args)
            (member "-h" args))
    (usage)
    (exit 0))

  ;; Parse mode
  (define mode
    (cond
      [(member "--dry-run" args) 'dry-run]
      [(member "--check" args)   'check]
      [else                      'fix]))

  ;; Collect file paths
  (define file-args
    (filter (λ (a) (not (string-prefix? a "--"))) args))

  (define files
    (cond
      [(member "--all" args)
       (collect-rkt-files (current-directory))]
      [(null? file-args)
       (if (eq? mode 'check)
           (collect-rkt-files (current-directory))  ;; --check alone defaults to --all
           (begin
             (displayln "Error: no files specified. Use --all or provide file paths.")
             (exit 2)))]
      [else
       (for/list ([f (in-list file-args)])
         (define p (string->path f))
         (unless (file-exists? p)
           (printf "Error: file not found: ~a~n" f)
           (exit 2))
         p)]))

  (when (null? files)
    (displayln "No .rkt files found.")
    (exit 0))

  (case mode
    [(check) (displayln "Checking line lengths...")]
    [(dry-run) (displayln "Dry run — showing what would change:")]
    [else (displayln "Wrapping long lines...")])

  (define total-long
    (for/sum ([f (in-list files)])
      (process-file f mode)))

  (case mode
    [(check)
     (if (zero? total-long)
         (begin
           (displayln (format "All ~a files OK — no lines exceed ~a chars."
                              (length files) MAX-LINE-LENGTH))
           (exit 0))
         (begin
           (displayln (format "FAILED: ~a lines exceed ~a chars across ~a files."
                              total-long MAX-LINE-LENGTH (length files)))
           (exit 1)))]
    [(dry-run)
     (displayln "Done (dry run — no files modified).")
     (exit 0)]
    [else
     (displayln "Done.")
     (exit 0)]))

(main)
