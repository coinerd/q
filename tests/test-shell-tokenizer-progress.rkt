#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: unit
;; @suite runtime
;; @boundary unit
;; @speed fast
;; @mutates none
;; tests/test-shell-tokenizer-progress.rkt — Deterministic progress tests
;; for the shell tokenizer (F-1 class: CPU spin prevention).
;;
;; Properties:
;; - tokenizer must terminate for all shell-special characters
;; - every token branch consumes input or terminates
;; - heredoc/CSS/HTML payloads with (, ), &, $(), quotes, comments do not hang

(require rackunit
         racket/string
         "../tools/shell-risk.rkt")

;; ---------------------------------------------------------------------------
;; Termination: special character corpus
;; ---------------------------------------------------------------------------

(define special-char-corpus
  '(""
    "("
    ")"
    "&&"
    "||"
    "|"
    ";"
    "&"
    "$()"
    "$(echo hello)"
    "`echo hello`"
    ">"
    ">>"
    "<"
    "<<EOF\nheredoc\nEOF"
    "'single'"
    "\"double\""
    "\\"
    "\\n"
    "#comment"
    "echo hello # comment"
    "echo 'it\\'s'"
    "echo \"hello \\\"world\\\"\""
    "cat <<'HEREDOC'\nsome text\nHEREDOC"
    "for i in $(seq 10); do echo $i; done"
    "echo ${VAR:-default}"
    "echo $((1+2))"
    "find . -name '*.rkt' -exec grep -l 'define' {} \\;"
    "awk '{print $1}'"
    "sed 's/old/new/g'"
    "grep -P '(?<=foo)bar'"
    "perl -e 'print \"hello\\n\"'"
    "python3 -c \"import sys; print(sys.argv)\""
    "echo '<div class=\"foo\">bar</div>'"
    "echo 'body { color: red; }'"
    "cat file.txt | head -n 10 | tail -n 5"
    "export FOO='bar baz'"
    "FOO=bar\\ baz echo $FOO"
    "xargs -I{} echo {}"
    "tee >(grep foo) >(grep bar)"
    "echo a{b,c}d"
    "echo $((2**10))"
    "echo $(echo $(echo nested))"
    ": 'multiline
string
literal'"
    "cat <<-EOF\n\tindented heredoc\nEOF"))

(test-case "tokenizer terminates for special character corpus"
  (for ([input (in-list special-char-corpus)])
    (define tokens (tokenize-shell-command input))
    (check-true (list? tokens)
                (format "tokenize-shell-command did not return a list for: ~v" input))))

;; ---------------------------------------------------------------------------
;; Progress: every call consumes input
;; ---------------------------------------------------------------------------

(test-case "tokenizer produces tokens for non-empty input"
  (for ([input (in-list special-char-corpus)]
        #:when (> (string-length input) 0))
    (define tokens (tokenize-shell-command input))
    ;; For non-empty input, should produce at least one token
    ;; (unless it's all whitespace, which is fine)
    (when (regexp-match? #rx"[^ \t\n]" input)
      (check-true (>= (length tokens) 1)
                  (format "No tokens for non-whitespace input: ~v" input)))))

;; ---------------------------------------------------------------------------
;; Nested quote / bracket handling
;; ---------------------------------------------------------------------------

(test-case "tokenizer handles nested brackets"
  (define inputs '("echo $(echo $(echo nested))"
                    "echo $((1+$((2+3))))"
                    "((a++))"
                    "if [ -f foo ]; then echo bar; fi"))
  (for ([input (in-list inputs)])
    (define tokens (tokenize-shell-command input))
    (check-true (list? tokens))))

(test-case "tokenizer handles mismatched quotes gracefully"
  ;; These should not hang — they should return some tokens
  (define inputs '("echo 'unclosed"
                    "echo \"unclosed"
                    "echo `unclosed"
                    "echo $(unclosed"))
  (for ([input (in-list inputs)])
    (define tokens (tokenize-shell-command input))
    (check-true (list? tokens)
                (format "Hung or crashed on: ~v" input))))

;; ---------------------------------------------------------------------------
;; Risk classification does not crash
;; ---------------------------------------------------------------------------

(test-case "classify-shell-risks does not crash on corpus"
  (for ([input (in-list special-char-corpus)])
    (define tokens (tokenize-shell-command input))
    (define risks (classify-shell-risks tokens))
    (check-true (list? risks))))

(test-case "shell-risk-summary returns hash"
  (for ([input (in-list special-char-corpus)]
        #:when (> (string-length input) 0))
    (define tokens (tokenize-shell-command input))
    (define risks (classify-shell-risks tokens))
    (when (not (null? risks))
      (define summary (shell-risk-summary risks))
      (check-true (hash? summary)))))
