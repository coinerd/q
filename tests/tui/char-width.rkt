#lang racket

;; tests/tui/char-width.rkt — Tests for Unicode-aware terminal column width

(require rackunit
         rackunit/text-ui
         "../../../q/tui/char-width.rkt")

(define char-width-tests
  (test-suite
   "TUI char-width"

   ;; ============================================================
   ;; char-width — single character width
   ;; ============================================================

   (test-case "ASCII letters are width 1"
     (check-equal? (char-width #\a) 1)
     (check-equal? (char-width #\Z) 1)
     (check-equal? (char-width #\0) 1)
     (check-equal? (char-width #\ ) 1))

   (test-case "CJK ideographs are width 2"
     ;; CJK Unified Ideographs (U+4E00–U+9FFF)
     (check-equal? (char-width #\你) 2)
     (check-equal? (char-width #\好) 2)
     (check-equal? (char-width #\世) 2)
     (check-equal? (char-width #\界) 2)
     (check-equal? (char-width #\中) 2)
     (check-equal? (char-width #\文) 2))

   (test-case "CJK Extension B are width 2"
     ;; U+20000 is outside BMP — Racket represents as surrogate pair or
     ;; directly depending on build. Test a known one if available.
     ;; Skip if the character can't be constructed.
     (let ([c (integer->char #x20000)])
       (when c
         (check-equal? (char-width c) 2))))

   (test-case "Hiragana and Katakana are width 2"
     (check-equal? (char-width #\あ) 2)   ; Hiragana
     (check-equal? (char-width #\ア) 2))  ; Katakana

   (test-case "Hangul syllables are width 2"
     (check-equal? (char-width #\한) 2)
     (check-equal? (char-width #\국) 2))

   (test-case "Fullwidth variants are width 2"
     ;; Fullwidth Latin (U+FF01–U+FF5E)
     (check-equal? (char-width #\！) 2)  ; U+FF01
     (check-equal? (char-width #\｝) 2)) ; U+FF5D

   (test-case "Emoji are width 2"
     ;; Basic emoji — these are in BMP supplementary planes or basic ranges
     ;; Racket may represent some as chars directly
     (check-equal? (char-width (integer->char #x2600)) 2)  ; ☀
     (check-equal? (char-width (integer->char #x2605)) 2)  ; ★
     (check-equal? (char-width (integer->char #x263A)) 2)) ; ☺

   (test-case "Control characters are width 0"
     (check-equal? (char-width #\nul) 0)
     (check-equal? (char-width #\tab) 0)
     (check-equal? (char-width #\return) 0)
     (check-equal? (char-width #\newline) 0))

   (test-case "Combining marks are width 0"
     ;; Combining Diacritical Marks (U+0300–U+036F)
     (check-equal? (char-width (integer->char #x0300)) 0)  ; Combining grave
     (check-equal? (char-width (integer->char #x0301)) 0)  ; Combining acute
     (check-equal? (char-width (integer->char #x0302)) 0)  ; Combining circumflex
     (check-equal? (char-width (integer->char #x0308)) 0)) ; Combining diaeresis

   (test-case "Variation selectors are width 0"
     ;; Variation Selectors (U+FE00–U+FE0F)
     (check-equal? (char-width (integer->char #xFE00)) 0)
     (check-equal? (char-width (integer->char #xFE0F)) 0))

   (test-case "Box drawing chars are width 1"
     (check-equal? (char-width #\─) 1)  ; U+2500
     (check-equal? (char-width #\│) 1)  ; U+2502
     (check-equal? (char-width #\┌) 1)  ; U+250C
     (check-equal? (char-width #\┘) 1)) ; U+2518

   ;; ============================================================
   ;; string-visible-width
   ;; ============================================================

   (test-case "ASCII string: hello → 5"
     (check-equal? (string-visible-width "hello") 5))

   (test-case "CJK string: 你好 → 4"
     (check-equal? (string-visible-width "你好") 4))

   (test-case "Mixed ASCII+CJK: hi你好 → 6"
     (check-equal? (string-visible-width "hi你好") 6))

   (test-case "Mixed with fullwidth: A！B → 4"
     (check-equal? (string-visible-width "A！B") 4))

   (test-case "Emoji string: ★★ → 4"
     (check-equal? (string-visible-width (list->string (list (integer->char #x2605) (integer->char #x2605)))) 4))

   (test-case "Empty string → 0"
     (check-equal? (string-visible-width "") 0))

   (test-case "String with combining mark: e + combining acute → 1"
     ;; é as e (U+0065) + combining acute (U+0301)
     (check-equal? (string-visible-width "e\u0301") 1))

   (test-case "CJK with combining mark: 你 + combining acute → 2"
     (check-equal? (string-visible-width "你\u0301") 2))

   (test-case "Control characters contribute 0"
     (check-equal? (string-visible-width "a\tb") 2)
     (check-equal? (string-visible-width "a\nb") 2))

   (test-case "long mixed string"
     ;; "hello你好world世界" = 5 + 4 + 5 + 4 = 18
     (check-equal? (string-visible-width "hello你好world世界") 18))))

(run-tests char-width-tests)
