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
     (check-equal? (string-visible-width "hello你好world世界") 18))

   ;; ============================================================
   ;; display-col->string-offset
   ;; ============================================================

   (test-case "display-col->string-offset: ASCII identity"
     (check-equal? (display-col->string-offset "hello" 0) 0)
     (check-equal? (display-col->string-offset "hello" 3) 3)
     (check-equal? (display-col->string-offset "hello" 5) 5))

   (test-case "display-col->string-offset: CJK width-2 chars"
     ;; "你好" → display widths: [0,2) and [2,4)
     (check-equal? (display-col->string-offset "你好" 0) 0)
     (check-equal? (display-col->string-offset "你好" 1) 0) ; inside first CJK char
     (check-equal? (display-col->string-offset "你好" 2) 1) ; second char
     (check-equal? (display-col->string-offset "你好" 3) 1) ; inside second CJK char
     (check-equal? (display-col->string-offset "你好" 4) 2))

   (test-case "display-col->string-offset: mixed ASCII + CJK"
     ;; "a你b" → display: [0]=a(1) [1-2]=你(2) [3]=b(1)
     (check-equal? (display-col->string-offset "a你b" 0) 0)
     (check-equal? (display-col->string-offset "a你b" 1) 1)
     (check-equal? (display-col->string-offset "a你b" 2) 1) ; inside 你
     (check-equal? (display-col->string-offset "a你b" 3) 2)
     (check-equal? (display-col->string-offset "a你b" 4) 3))

   (test-case "display-col->string-offset: past end returns length"
     (check-equal? (display-col->string-offset "hi" 100) 2))

   ;; ============================================================
   ;; Grapheme cluster utilities (UAX #29)
   ;; ============================================================

   (test-case "grapheme-span-at: ASCII"
     (check-equal? (grapheme-span-at "hello" 0) 1)
     (check-equal? (grapheme-span-at "hello" 4) 1))

   (test-case "grapheme-span-at: combining character"
     ;; e + combining acute = 2 codepoints, 1 grapheme
     (check-equal? (grapheme-span-at "e\u0301" 0) 2))

   (test-case "grapheme-span-at: past end returns 0"
     (check-equal? (grapheme-span-at "hi" 5) 0))

   (test-case "grapheme-count: ASCII"
     (check-equal? (grapheme-count "hello") 5))

   (test-case "grapheme-count: combining chars"
     ;; e+combining acute + o = 3 codepoints, 2 graphemes
     (check-equal? (grapheme-count "e\u0301o") 2))

   (test-case "grapheme-count: CJK"
     (check-equal? (grapheme-count "你好") 2))

   (test-case "grapheme-count: empty string"
     (check-equal? (grapheme-count "") 0))

   (test-case "string-grapheme-length is alias for grapheme-count"
     (check-equal? (string-grapheme-length "hello") 5))

   (test-case "substring-by-graphemes: basic"
     (check-equal? (substring-by-graphemes "hello" 1 3) "el"))

   (test-case "substring-by-graphemes: to end"
     (check-equal? (substring-by-graphemes "hello" 3) "lo"))

   (test-case "substring-by-graphemes: with combining"
     ;; e+acute + l + l + o = 4 graphemes
     (check-equal? (substring-by-graphemes "e\u0301llo" 0 1) "e\u0301"))

   (test-case "prev-grapheme-start: ASCII"
     (check-equal? (prev-grapheme-start "hello" 3) 2))

   (test-case "prev-grapheme-start: combining character"
     ;; "e\u0301x" — cursor at 2 (x) should go back to 0 (start of e+acute)
     (check-equal? (prev-grapheme-start "e\u0301x" 2) 0))

   (test-case "prev-grapheme-start: at beginning returns 0"
     (check-equal? (prev-grapheme-start "hello" 0) 0))

   (test-case "next-grapheme-start: ASCII"
     (check-equal? (next-grapheme-start "hello" 2) 3))

   (test-case "next-grapheme-start: combining character"
     ;; "e\u0301x" — cursor at 0 should go to 2 (start of x)
     (check-equal? (next-grapheme-start "e\u0301x" 0) 2))

   (test-case "next-grapheme-start: at end returns length"
     (check-equal? (next-grapheme-start "hi" 2) 2))
   ))

(run-tests char-width-tests)

;; ============================================================
;; truncate-to-visible-width (#424)
;; ============================================================
(define truncate-tests
  (test-suite
   "truncate-to-visible-width"

   (test-case "ASCII string within width"
     (check-equal? (truncate-to-visible-width "hello" 10) "hello"))

   (test-case "ASCII string truncated"
     (check-equal? (truncate-to-visible-width "hello world" 5) "hello"))

   (test-case "CJK string truncated at wide char boundary"
     ;; Each CJK char is width 2, so 5 cols can hold 2 CJK chars (4 cols)
     (check-equal? (truncate-to-visible-width "\u4f60\u597d\u4e16\u754c" 5) "\u4f60\u597d"))

   (test-case "Mixed ASCII+CJK truncated"
     ;; "AB\u4f60" = 1+1+2 = 4 cols, truncate to 3 cols → "AB"
     (check-equal? (truncate-to-visible-width "AB\u4f60" 3) "AB"))

   (test-case "Empty string"
     (check-equal? (truncate-to-visible-width "" 5) ""))

   (test-case "Zero max-cols returns empty"
     (check-equal? (truncate-to-visible-width "hello" 0) ""))
   ))

(run-tests truncate-tests)
