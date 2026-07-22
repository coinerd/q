#lang racket/base

;; Strict restricted-JSON reader extracted from scripts/gsd-milestone-truth.rkt
;; (v0.99.58 W1-4 / P1-G). Pure function, no external dependencies beyond
;; racket/port for port->string in the file variant.

(require racket/port)

(provide strict-json-read
         strict-json-read-file)

;; JavaScript safe-integer range. Shared with util/json/canonical-json.rkt —
;; keep in sync.
(define MAX-SAFE-INTEGER 9007199254740991)

(define (strict-json-read source)
  (unless (string? source)
    (raise-argument-error 'strict-json-read "string?" source))
  (define length* (string-length source))
  (define position 0)
  (define (at-end?)
    (= position length*))
  (define (peek)
    (and (not (at-end?)) (string-ref source position)))
  (define (take!)
    (when (at-end?)
      (error 'strict-json-read "unexpected end of input at offset ~a" position))
    (define char (string-ref source position))
    (set! position (add1 position))
    char)
  (define (json-space? char)
    (and char (memv char '(#\space #\tab #\newline #\return))))
  (define (skip-space!)
    (let loop ()
      (when (json-space? (peek))
        (set! position (add1 position))
        (loop))))
  (define (expect! expected)
    (define actual (take!))
    (unless (char=? actual expected)
      (error 'strict-json-read "expected ~s at offset ~a, got ~s" expected (sub1 position) actual)))
  (define (read-hex4!)
    (when (> (+ position 4) length*)
      (error 'strict-json-read "incomplete Unicode escape at offset ~a" position))
    (define text (substring source position (+ position 4)))
    (unless (regexp-match? #px"^[0-9A-Fa-f]{4}$" text)
      (error 'strict-json-read "invalid Unicode escape \\u~a at offset ~a" text position))
    (set! position (+ position 4))
    (string->number text 16))
  (define (write-codepoint! output codepoint)
    (when (or (> codepoint #x10ffff) (<= #xd800 codepoint #xdfff))
      (error 'strict-json-read "invalid Unicode scalar U+~x" codepoint))
    (write-char (integer->char codepoint) output))
  (define (parse-string!)
    (expect! #\")
    (define output (open-output-string))
    (let loop ()
      (when (at-end?)
        (error 'strict-json-read "unterminated string"))
      (define char (take!))
      (cond
        [(char=? char #\") (get-output-string output)]
        [(char=? char #\\)
         (when (at-end?)
           (error 'strict-json-read "unterminated escape"))
         (define escaped (take!))
         (case escaped
           [(#\") (write-char #\" output)]
           [(#\\) (write-char #\\ output)]
           [(#\/) (write-char #\/ output)]
           [(#\b) (write-char #\backspace output)]
           [(#\f) (write-char #\page output)]
           [(#\n) (write-char #\newline output)]
           [(#\r) (write-char #\return output)]
           [(#\t) (write-char #\tab output)]
           [(#\u)
            (define first (read-hex4!))
            (cond
              [(<= #xd800 first #xdbff)
               (unless (and (<= (+ position 2) length*)
                            (char=? (string-ref source position) #\\)
                            (char=? (string-ref source (add1 position)) #\u))
                 (error 'strict-json-read "high surrogate is not followed by a low surrogate"))
               (set! position (+ position 2))
               (define second (read-hex4!))
               (unless (<= #xdc00 second #xdfff)
                 (error 'strict-json-read "high surrogate is not followed by a low surrogate"))
               (write-codepoint! output (+ #x10000 (* (- first #xd800) #x400) (- second #xdc00)))]
              [(<= #xdc00 first #xdfff) (error 'strict-json-read "unpaired low surrogate")]
              [else (write-codepoint! output first)])]
           [else
            (error 'strict-json-read
                   "invalid string escape \\~a at offset ~a"
                   escaped
                   (sub1 position))])
         (loop)]
        [(< (char->integer char) #x20)
         (error 'strict-json-read "unescaped control character in string")]
        [else
         (write-codepoint! output (char->integer char))
         (loop)])))
  (define (consume-literal! text value)
    (define end (+ position (string-length text)))
    (unless (and (<= end length*) (string=? (substring source position end) text))
      (error 'strict-json-read "expected ~a at offset ~a" text position))
    (set! position end)
    value)
  (define (parse-integer!)
    (define start position)
    (when (eqv? (peek) #\-)
      (set! position (add1 position)))
    (when (at-end?)
      (error 'strict-json-read "incomplete number at offset ~a" start))
    (cond
      [(eqv? (peek) #\0) (set! position (add1 position))]
      [(and (char-numeric? (peek)) (not (eqv? (peek) #\0)))
       (let loop ()
         (when (and (peek) (char-numeric? (peek)))
           (set! position (add1 position))
           (loop)))]
      [else (error 'strict-json-read "invalid number at offset ~a" start)])
    (when (and (peek) (memv (peek) '(#\. #\e #\E)))
      (error 'strict-json-read "only integers are allowed at offset ~a" start))
    (define value (string->number (substring source start position)))
    (unless (and (exact-integer? value) (<= (- MAX-SAFE-INTEGER) value MAX-SAFE-INTEGER))
      (error 'strict-json-read "integer is outside the safe range at offset ~a" start))
    value)
  (define parse-value! #f)
  (define (parse-array!)
    (expect! #\[)
    (skip-space!)
    (cond
      [(eqv? (peek) #\])
       (take!)
       '()]
      [else
       (let loop ([values '()])
         (define value (parse-value!))
         (skip-space!)
         (case (peek)
           [(#\,)
            (take!)
            (skip-space!)
            (loop (cons value values))]
           [(#\])
            (take!)
            (reverse (cons value values))]
           [else (error 'strict-json-read "expected comma or ] at offset ~a" position)]))]))
  (define (parse-object!)
    (expect! #\{)
    (skip-space!)
    (define entries (make-hash))
    (cond
      [(eqv? (peek) #\})
       (take!)
       (hash)]
      [else
       (let loop ()
         (unless (eqv? (peek) #\")
           (error 'strict-json-read "object key must be a string at offset ~a" position))
         (define key (parse-string!))
         (when (hash-has-key? entries key)
           (error 'strict-json-read "duplicate object key ~s" key))
         (skip-space!)
         (expect! #\:)
         (skip-space!)
         (hash-set! entries key (parse-value!))
         (skip-space!)
         (case (peek)
           [(#\,)
            (take!)
            (skip-space!)
            (loop)]
           [(#\})
            (take!)
            (make-immutable-hash (hash->list entries))]
           [else (error 'strict-json-read "expected comma or } at offset ~a" position)]))]))
  (set! parse-value!
        (lambda ()
          (skip-space!)
          (case (peek)
            [(#\") (parse-string!)]
            [(#\{) (parse-object!)]
            [(#\[) (parse-array!)]
            [(#\t) (consume-literal! "true" #t)]
            [(#\f) (consume-literal! "false" #f)]
            [(#\n) (consume-literal! "null" 'null)]
            [(#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (parse-integer!)]
            [else (error 'strict-json-read "expected a JSON value at offset ~a" position)])))
  (define result (parse-value!))
  (skip-space!)
  (unless (at-end?)
    (error 'strict-json-read "trailing input at offset ~a" position))
  result)

(define (strict-json-read-file path)
  (call-with-input-file path (lambda (input) (strict-json-read (port->string input)))))
