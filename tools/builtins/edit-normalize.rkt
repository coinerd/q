#lang racket/base

;; tools/builtins/edit-normalize.rkt — whitespace-tolerant edit matching

(require racket/contract
         racket/list
         racket/string)

(struct normalize-options
        (strip-trailing? normalize-crlf?
                         collapse-blank-lines?
                         tabs-to-spaces?
                         tab-width
                         trim-boundary-blank-lines?)
  #:transparent)

(struct nline (pairs newline-idx) #:transparent)

(provide (contract-out
          (struct normalize-options
                  ([strip-trailing? boolean?] [normalize-crlf? boolean?]
                                              [collapse-blank-lines? boolean?]
                                              [tabs-to-spaces? boolean?]
                                              [tab-width exact-positive-integer?]
                                              [trim-boundary-blank-lines? boolean?]))
          [default-normalization-options normalize-options?]
          [normalize-text (->* (string?) (normalize-options?) string?)]
          [similarity-score (-> string? string? real?)]
          [fuzzy-find-match
           (->* (string? string?) (#:threshold real? #:options normalize-options?) (or/c pair? #f))]))

(define default-normalization-options (normalize-options #t #t #t #t 2 #t))

(define (emit-pair ch idx pairs)
  (cons (cons ch idx) pairs))

(define (normalize-newlines s opts)
  (define len (string-length s))
  (let loop ([i 0]
             [out '()])
    (cond
      [(>= i len) (reverse out)]
      [else
       (define ch (string-ref s i))
       (cond
         [(and (normalize-options-normalize-crlf? opts) (char=? ch #\return))
          (if (and (< (add1 i) len) (char=? (string-ref s (add1 i)) #\newline))
              (loop (+ i 2) (emit-pair #\newline i out))
              (loop (add1 i) (emit-pair #\newline i out)))]
         [else (loop (add1 i) (emit-pair ch i out))])])))

(define (split-lines pairs)
  (let loop ([remaining pairs]
             [line-pairs '()]
             [lines '()])
    (cond
      [(null? remaining) (reverse (cons (nline (reverse line-pairs) #f) lines))]
      [else
       (define p (car remaining))
       (if (char=? (car p) #\newline)
           (loop (cdr remaining) '() (cons (nline (reverse line-pairs) (cdr p)) lines))
           (loop (cdr remaining) (cons p line-pairs) lines))])))

(define (expand-tabs pairs width)
  (let loop ([remaining pairs]
             [out '()])
    (cond
      [(null? remaining) (reverse out)]
      [else
       (define p (car remaining))
       (if (char=? (car p) #\tab)
           (loop (cdr remaining) (append (make-list (max 1 width) (cons #\space (cdr p))) out))
           (loop (cdr remaining) (cons p out)))])))

(define (trailing-strip-char? ch opts)
  (and (char-whitespace? ch)
       (or (normalize-options-normalize-crlf? opts) (not (char=? ch #\return)))))

(define (strip-trailing-pairs pairs opts)
  (cond
    [(null? pairs) '()]
    [(and (not (normalize-options-normalize-crlf? opts)) (char=? (car (last pairs)) #\return))
     (append (strip-trailing-pairs (drop-right pairs 1) opts) (list (last pairs)))]
    [else
     (let loop ([remaining (reverse pairs)])
       (cond
         [(null? remaining) '()]
         [(trailing-strip-char? (caar remaining) opts) (loop (cdr remaining))]
         [else (reverse remaining)]))]))

(define (pairs->string pairs)
  (list->string (map car pairs)))

(define (blank-line? line)
  (string=? (string-trim (pairs->string (nline-pairs line))) ""))

(define (normalize-line line opts)
  (define tabbed
    (if (normalize-options-tabs-to-spaces? opts)
        (expand-tabs (nline-pairs line) (normalize-options-tab-width opts))
        (nline-pairs line)))
  (define stripped
    (if (normalize-options-strip-trailing? opts)
        (strip-trailing-pairs tabbed opts)
        tabbed))
  (nline stripped (nline-newline-idx line)))

(define (trim-boundary-blank-lines lines opts)
  (if (normalize-options-trim-boundary-blank-lines? opts)
      (dropf-right (dropf lines blank-line?) blank-line?)
      lines))

(define (collapse-blank-lines lines opts)
  (if (normalize-options-collapse-blank-lines? opts)
      (let loop ([remaining lines]
                 [previous-blank? #f]
                 [out '()])
        (cond
          [(null? remaining) (reverse out)]
          [else
           (define line (car remaining))
           (define blank? (blank-line? line))
           (if (and blank? previous-blank?)
               (loop (cdr remaining) #t out)
               (loop (cdr remaining) blank? (cons line out)))]))
      lines))

(define (line-newline-map line)
  (or (nline-newline-idx line)
      (and (pair? (nline-pairs line)) (cdar (reverse (nline-pairs line))))
      0))

(define (join-lines lines)
  (let loop ([remaining lines]
             [out '()])
    (cond
      [(null? remaining) (reverse out)]
      [else
       (define line (car remaining))
       (define with-line (append (reverse (nline-pairs line)) out))
       (if (pair? (cdr remaining))
           (loop (cdr remaining) (emit-pair #\newline (line-newline-map line) with-line))
           (loop (cdr remaining) with-line))])))

(define (normalize-pairs s opts)
  (join-lines (collapse-blank-lines
               (trim-boundary-blank-lines (map (lambda (line) (normalize-line line opts))
                                               (split-lines (normalize-newlines s opts)))
                                          opts)
               opts)))

(define (normalize-with-map s opts)
  (define pairs (normalize-pairs s opts))
  (values (pairs->string pairs) (map cdr pairs)))

(define (normalize-text s (opts default-normalization-options))
  (define-values (normalized maps) (normalize-with-map s opts))
  normalized)

(define (substring-index haystack needle)
  (define m (regexp-match-positions (regexp-quote needle) haystack))
  (and m (caar m)))

(define (lcs-length a b)
  (define la (string-length a))
  (define lb (string-length b))
  (define prev (make-vector (add1 lb) 0))
  (for ([i (in-range la)])
    (define curr (make-vector (add1 lb) 0))
    (for ([j (in-range lb)])
      (vector-set! curr
                   (add1 j)
                   (if (char=? (string-ref a i) (string-ref b j))
                       (add1 (vector-ref prev j))
                       (max (vector-ref prev (add1 j)) (vector-ref curr j)))))
    (set! prev curr))
  (vector-ref prev lb))

(define (similarity-score a b)
  (cond
    [(and (zero? (string-length a)) (zero? (string-length b))) 1.0]
    [(or (zero? (string-length a)) (zero? (string-length b))) 0.0]
    [else (/ (lcs-length a b) (max (string-length a) (string-length b)))]))

(define (normalized-end->original-end maps norm-end original-len)
  (cond
    [(zero? norm-end) 0]
    [(>= norm-end (length maps)) original-len]
    [else (add1 (list-ref maps (sub1 norm-end)))]))

(define (best-window-start normalized-content normalized-old threshold)
  (define content-len (string-length normalized-content))
  (define old-len (string-length normalized-old))
  (cond
    [(or (zero? old-len) (< content-len old-len)) #f]
    [else
     (for/fold ([best-start #f]
                [best-score 0.0]
                #:result (and best-start (>= best-score threshold) best-start))
               ([start (in-range 0 (add1 (- content-len old-len)))])
       (define score
         (similarity-score (substring normalized-content start (+ start old-len)) normalized-old))
       (if (> score best-score)
           (values start score)
           (values best-start best-score)))]))

(define (normalize-text-keep-trailing-nl s opts)
  (define text (normalize-text s opts))
  (define last-idx (sub1 (string-length s)))
  (define ends-with-nl?
    (and (>= last-idx 0)
         (or (char=? (string-ref s last-idx) #\newline)
             (and (>= last-idx 1)
                  (char=? (string-ref s (sub1 last-idx)) #\return)
                  (char=? (string-ref s last-idx) #\newline)))))
  (define lines (string-split s "\n" #:trim? #f))
  (define has-boundary-blanks?
    (or (and (> (length lines) 1) (string=? (car lines) ""))
        (and (> (length lines) 2) (string=? (list-ref lines (- (length lines) 2)) ""))))
  (if (and ends-with-nl?
           (not has-boundary-blanks?)
           (or (zero? (string-length text))
               (not (char=? (string-ref text (sub1 (string-length text))) #\newline))))
      (string-append text "\n")
      text))

(define (fuzzy-find-match content
                          old-text
                          #:threshold (threshold 0.85)
                          #:options (opts default-normalization-options))
  (define normalized-old/precheck (normalize-text-keep-trailing-nl old-text opts))
  (define exact-start
    (and (positive? (string-length normalized-old/precheck)) (substring-index content old-text)))
  (cond
    [exact-start (cons exact-start (+ exact-start (string-length old-text)))]
    [else
     (define-values (normalized-content content-map) (normalize-with-map content opts))
     (define normalized-old normalized-old/precheck)
     (define start
       (or (substring-index normalized-content normalized-old)
           (best-window-start normalized-content normalized-old threshold)))
     (cond
       [(or (not start) (zero? (string-length normalized-old)) (null? content-map)) #f]
       [else
        (define norm-end (+ start (string-length normalized-old)))
        (define original-start (list-ref content-map start))
        (define original-end
          (normalized-end->original-end content-map norm-end (string-length content)))
        (cons original-start original-end)])]))
