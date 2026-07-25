#lang racket/base

;; tools/builtins/builtin-helpers.rkt — shared helpers for builtin tools
;;
;; v0.32.1 Wave 2: Extract repeated safe-mode path validation pattern
;; from edit.rkt, write.rkt, and read.rkt into a single helper.

(require racket/contract
         racket/file
         (only-in "../../util/safe-mode/safe-mode-predicates.rkt"
                  safe-mode?
                  allowed-path?
                  safe-mode-project-root))

(provide (contract-out [require-safe-path! (-> (or/c path? string?) string? (or/c #f string?))]
                       [check-utf8-file? (-> path? (or/c #t string?))]
                       [validate-utf8-bytes (-> bytes? (or/c #t string?))]))

;; --------------------------------------------------
;; UTF-8 byte-level validator
;; --------------------------------------------------

;; check-utf8-file? : path? -> (or/c #t string?)
;; Reads raw bytes from the file at PATH (using file->bytes, NOT file->string)
;; and validates that the bytes form well-formed UTF-8 per RFC 3629.
;; Returns #t if the file is valid UTF-8, or a descriptive error string
;; explaining the first encoding issue found.
(define (check-utf8-file? path)
  (define raw-bytes (file->bytes path))
  (validate-utf8-bytes raw-bytes))

;; validate-utf8-bytes : bytes? -> (or/c #t string?)
;; Iterates over raw bytes validating UTF-8 encoding rules:
;;   - 1-byte: 0x00-0x7F
;;   - 2-byte: 0xC2-0xDF + 1 continuation (0x80-0xBF)
;;   - 3-byte: 0xE0-0xEF + 2 continuation; reject overlong (E0 80-9F)
;;             and surrogate range (ED A0-BF)
;;   - 4-byte: 0xF0-0xF4 + 3 continuation; reject overlong (F0 80-8F)
;;             and out-of-range (F4 90-BF = > U+10FFFF)
;;   0xF5-0xF7, 0xF8-0xFB, 0xFC-0xFD, 0xFE, 0xFF: always invalid lead bytes.
;;   0x80-0xBF as lead byte: unexpected continuation.
;; Returns #t on success, or a string describing the first error.
(define (validate-utf8-bytes raw-bytes)
  (define len (bytes-length raw-bytes))
  (let loop ([i 0])
    (if (>= i len)
        #t
        (let ([b (bytes-ref raw-bytes i)])
          (cond
            [(<= b #x7F) (loop (add1 i))]
            [(<= #xC2 b #xDF)
             (if (>= (add1 i) len)
                 (format "Invalid UTF-8 at byte ~a: incomplete 2-byte sequence" i)
                 (let ([c1 (bytes-ref raw-bytes (add1 i))])
                   (if (<= #x80 c1 #xBF)
                       (loop (+ i 2))
                       (format "Invalid UTF-8 at byte ~a: expected continuation 0x80-0xBF, got 0x~X"
                               (add1 i)
                               c1))))]
            [(<= #xE0 b #xEF)
             (if (>= (+ i 2) len)
                 (format "Invalid UTF-8 at byte ~a: incomplete 3-byte sequence" i)
                 (let ([c1 (bytes-ref raw-bytes (add1 i))]
                       [c2 (bytes-ref raw-bytes (+ i 2))])
                   (cond
                     [(not (<= #x80 c1 #xBF))
                      (format "Invalid UTF-8 at byte ~a: expected continuation 0x80-0xBF, got 0x~X"
                              (add1 i)
                              c1)]
                     [(not (<= #x80 c2 #xBF))
                      (format "Invalid UTF-8 at byte ~a: expected continuation 0x80-0xBF, got 0x~X"
                              (+ i 2)
                              c2)]
                     [(and (= b #xE0) (< c1 #xA0))
                      (format "Invalid UTF-8 at byte ~a: overlong 3-byte encoding (E0 0x~X)" i c1)]
                     [(and (= b #xED) (>= c1 #xA0))
                      (format "Invalid UTF-8 at byte ~a: surrogate code point U+D800-U+DFFF (ED 0x~X)"
                              i
                              c1)]
                     [else (loop (+ i 3))])))]
            [(<= #xF0 b #xF4)
             (if (>= (+ i 3) len)
                 (format "Invalid UTF-8 at byte ~a: incomplete 4-byte sequence" i)
                 (let ([c1 (bytes-ref raw-bytes (add1 i))]
                       [c2 (bytes-ref raw-bytes (+ i 2))]
                       [c3 (bytes-ref raw-bytes (+ i 3))])
                   (cond
                     [(not (<= #x80 c1 #xBF))
                      (format "Invalid UTF-8 at byte ~a: expected continuation 0x80-0xBF, got 0x~X"
                              (add1 i)
                              c1)]
                     [(not (<= #x80 c2 #xBF))
                      (format "Invalid UTF-8 at byte ~a: expected continuation 0x80-0xBF, got 0x~X"
                              (+ i 2)
                              c2)]
                     [(not (<= #x80 c3 #xBF))
                      (format "Invalid UTF-8 at byte ~a: expected continuation 0x80-0xBF, got 0x~X"
                              (+ i 3)
                              c3)]
                     [(and (= b #xF0) (< c1 #x90))
                      (format "Invalid UTF-8 at byte ~a: overlong 4-byte encoding" i)]
                     [(and (= b #xF4) (>= c1 #x90))
                      (format
                       "Invalid UTF-8 at byte ~a: code point > U+10FFFF (4-byte sequence out of range)"
                       i)]
                     [else (loop (+ i 4))])))]
            [(<= b #xF7)
             (format "Invalid UTF-8 at byte ~a: obsolete 5-byte lead 0x~X (RFC 3629)" i b)]
            [(<= b #xFB)
             (format "Invalid UTF-8 at byte ~a: obsolete 6-byte lead 0x~X (RFC 3629)" i b)]
            [(<= b #xFD)
             (format "Invalid UTF-8 at byte ~a: obsolete 7-byte lead 0x~X (RFC 3629)" i b)]
            [(>= b #xFE) (format "Invalid UTF-8 at byte ~a: invalid lead byte 0x~X" i b)]
            [else (format "Invalid UTF-8 at byte ~a: unexpected continuation byte 0x~X" i b)])))))

;; require-safe-path! : string? string? -> (or/c #f string?)
;; Validates that a path is allowed under safe-mode constraints.
;; Returns #f if path is allowed, or an error message string if blocked.
;; Encapsulates the repeated pattern:
;;   (and (safe-mode?) (not (allowed-path? path))) -> error message
(define (require-safe-path! path-str tool-name)
  (cond
    [(and (safe-mode?) (not (allowed-path? path-str)))
     (format "~a: path not allowed (safe mode): ~a" tool-name path-str)]
    [else #f]))
