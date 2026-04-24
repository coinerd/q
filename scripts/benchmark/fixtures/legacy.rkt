#lang racket

(provide parse-config
         format-entry
         validate-entry
         load-config
         save-config
         format-report
         validate-all
         parse-number
         parse-boolean
         format-number
         format-boolean
         entry->string
         string->entry
         merge-configs)

;; ==============================================================================
;; SECTION 1: Parsing
;; ==============================================================================

(define (parse-number str)
  (define n (string->number str))
  (if n n #f))

(define (parse-boolean str)
  (cond
    [(member str '("true" "#t" "yes" "1")) #t]
    [(member str '("false" "#f" "no" "0")) #f]
    [else #f]))

(define (string->entry str)
  (define parts (string-split str "="))
  (if (= (length parts) 2)
      (cons (string-trim (car parts))
            (string-trim (cadr parts)))
      #f))

(define (parse-config lines)
  (filter-map string->entry lines))

;; ==============================================================================
;; SECTION 2: Formatting
;; ==============================================================================

(define (format-number n)
  (number->string n))

(define (format-boolean b)
  (if b "#t" "#f"))

(define (format-entry pair)
  (format "~a = ~a" (car pair) (cdr pair)))

(define (entry->string pair)
  (format "~a=~a" (car pair) (cdr pair)))

(define (format-report config)
  (string-join
   (for/list ([entry config])
     (format "  ~a: ~a" (car entry) (cdr entry)))
   "\n"))

;; ==============================================================================
;; SECTION 3: Validation
;; ==============================================================================

(define (validate-entry pair)
  (and (pair? pair)
       (string? (car pair))
       (string? (cdr pair))
       (> (string-length (car pair)) 0)))

(define (validate-all config)
  (andmap validate-entry config))

(define (merge-configs c1 c2)
  (define h (make-hash))
  (for ([entry c1]) (hash-set! h (car entry) (cdr entry)))
  (for ([entry c2]) (hash-set! h (car entry) (cdr entry)))
  (for/list ([(k v) (in-hash h)])
    (cons k v)))

;; ==============================================================================
;; SECTION 4: I/O Operations
;; ==============================================================================

(define (load-config path)
  (define lines (file->lines path))
  (parse-config lines))

(define (save-config config path)
  (define text (string-join
                (for/list ([entry config])
                  (entry->string entry))
                "\n"))
  (with-output-to-file path
    (lambda () (display text)))
  config)

;; ==============================================================================
;; SECTION 5: Derived Utilities
;; ==============================================================================

(define (config-ref config key [default #f])
  (define found (assoc key config))
  (if found (cdr found) default))

(define (config-set config key value)
  (cons (cons key value)
        (filter (lambda (p) (not (equal? (car p) key)))
                config)))

(define (config-keys config)
  (map car config))

(define (config-values config)
  (map cdr config))

(define (config-count config)
  (length config))
