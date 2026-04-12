#lang racket

;; q/tests/test-export-formats.rkt — Tests for session export (Markdown, HTML, JSON)
;;
;; Covers:
;;   - Markdown: basic conversation, tool call + result, errors, determinism
;;   - HTML: structure, escaping, tool calls in details blocks
;;   - JSON: valid output, correct structure
;;   - Export dispatcher: format routing
;;   - Export to file
;;   - Empty session

(require rackunit
         racket/file
         racket/string
         json
         "../util/protocol-types.rkt"
         "../util/export-markdown.rkt"
         "../util/export-html.rkt"
         "../util/export-json.rkt"
         "../cli/export.rkt")

;; ============================================================
;; Test data helpers
;; ============================================================

(define (make-user-msg id text ts)
  (make-message id #f 'user 'message
                (list (make-text-part text))
                ts #f))

(define (make-assistant-msg id text ts)
  (make-message id #f 'assistant 'message
                (list (make-text-part text))
                ts #f))

(define (make-tool-call-msg id tool-name args ts)
  (make-message id #f 'assistant 'tool-call
                (list (make-tool-call-part "tc-1" tool-name args))
                ts #f))

(define (make-tool-result-msg id content is-error ts)
  (make-message id #f 'tool 'tool-result
                (list (make-tool-result-part "tc-1" content is-error))
                ts #f))

(define basic-session
  (list (make-user-msg "m1" "Hello, q!" 1000)
        (make-assistant-msg "m2" "Hi! How can I help?" 1001)))

(define tool-session
  (list (make-tool-call-msg "m1" "bash" "{\"command\": \"ls\"}" 1000)
        (make-tool-result-msg "m2" "file1.txt\nfile2.txt" #f 1001)))

(define error-session
  (list (make-tool-result-msg "m1" "command not found" #t 1000)))

;; ============================================================
;; Markdown: basic conversation
;; ============================================================

(define md-basic (session->markdown basic-session))
(check-true (string-contains? md-basic "### user (message)") "md: user heading")
(check-true (string-contains? md-basic "Hello, q!") "md: user text")
(check-true (string-contains? md-basic "### assistant (message)") "md: assistant heading")
(check-true (string-contains? md-basic "Hi! How can I help?") "md: assistant text")

;; ============================================================
;; Markdown: tool call + tool result rendering
;; ============================================================

(define md-tools (session->markdown tool-session))
(check-true (string-contains? md-tools "**Tool: bash**") "md: tool name bold")
(check-true (string-contains? md-tools "```json") "md: tool args code block")
(check-true (string-contains? md-tools "{\"command\": \"ls\"}") "md: tool args content")
(check-true (string-contains? md-tools "file1.txt") "md: tool result content")
(check-true (string-contains? md-tools "file2.txt") "md: tool result content 2")

;; ============================================================
;; Markdown: error results marked
;; ============================================================

(define md-error (session->markdown error-session))
(check-true (string-contains? md-error "> ⚠ Error:") "md: error prefix")
(check-true (string-contains? md-error "command not found") "md: error content")

;; ============================================================
;; Markdown: deterministic (same session → same output)
;; ============================================================

(check-equal? (session->markdown basic-session)
              (session->markdown basic-session)
              "md: deterministic output")

;; ============================================================
;; HTML: basic structure
;; ============================================================

(define html-basic (session->html basic-session))
(check-true (string-contains? html-basic "<!DOCTYPE html>") "html: DOCTYPE")
(check-true (string-contains? html-basic "<html lang=\"en\">") "html: html tag")
(check-true (string-contains? html-basic "</html>") "html: closing html")
(check-true (string-contains? html-basic "<body>") "html: body open")
(check-true (string-contains? html-basic "</body>") "html: body close")
(check-true (string-contains? html-basic "Hello, q!") "html: user text")
(check-true (string-contains? html-basic "Hi! How can I help?") "html: assistant text")

;; ============================================================
;; HTML: text escaping
;; ============================================================

(define evil-session
  (list (make-user-msg "m1" "<script>alert('xss')</script>" 1000)))
(define html-evil (session->html evil-session))
(check-false (string-contains? html-evil "<script>") "html: no raw script tag")
(check-true (string-contains? html-evil "&lt;script&gt;") "html: escaped script")
(check-true (string-contains? html-evil "alert(") "html: alert text preserved")

;; ============================================================
;; HTML: tool calls in details blocks
;; ============================================================

(define html-tools (session->html tool-session))
(check-true (string-contains? html-tools "<details>") "html: details open tag")
(check-true (string-contains? html-tools "<summary>Tool: bash</summary>") "html: tool summary")
(check-true (string-contains? html-tools "</details>") "html: details close tag")
(check-true (string-contains? html-tools "<pre><code>") "html: code block in details")

;; ============================================================
;; JSON: valid JSON output (round-trip)
;; ============================================================

(define json-str (session->json-string basic-session))
(define parsed (string->jsexpr json-str))
(check-not-false parsed "json: parses successfully")
(check-pred hash? parsed "json: top-level is hash")
(check-true (hash-has-key? parsed 'session) "json: has session key")

;; ============================================================
;; JSON: correct structure (session.entries array)
;; ============================================================

(define sess (hash-ref parsed 'session))
(check-true (hash-has-key? sess 'entries) "json: has entries")
(check-true (hash-has-key? sess 'exported_at) "json: has exported_at")
(check-true (hash-has-key? sess 'version) "json: has version")
(check-equal? (hash-ref sess 'version) "0.5.3" "json: version string")
(define entries (hash-ref sess 'entries))
(check-equal? (length entries) 2 "json: 2 entries")
(check-equal? (hash-ref (car entries) 'role) "user" "json: first entry role")

;; ============================================================
;; Export dispatcher: correct format routing
;; ============================================================

(define test-session-dir (make-temporary-file "q-export-test-~a" 'directory))
(define test-session-path (build-path test-session-dir "test-session.jsonl"))

(call-with-output-file test-session-path
  (lambda (out)
    (for ([msg (in-list basic-session)])
      (displayln (jsexpr->string (message->jsexpr msg)) out)))
  #:mode 'text
  #:exists 'replace)

(define disp-md (export-session test-session-path 'markdown))
(check-true (string-contains? disp-md "### user (message)") "dispatch: markdown")

(define disp-html (export-session test-session-path 'html))
(check-true (string-contains? disp-html "<!DOCTYPE html>") "dispatch: html")

(define disp-json (export-session test-session-path 'json))
(check-true (string-contains? disp-json "\"session\"") "dispatch: json")

;; ============================================================
;; Export to file: writes correctly
;; ============================================================

(define out-path (build-path test-session-dir "output.md"))
(export-session-to-file test-session-path 'markdown out-path)
(check-pred file-exists? out-path "file: exists")
(define file-content (file->string out-path))
(check-true (string-contains? file-content "### user (message)") "file: correct content")

;; ============================================================
;; Empty session: graceful output
;; ============================================================

(define empty-path (build-path test-session-dir "empty.jsonl"))
(call-with-output-file empty-path
  (lambda (out) (void))
  #:mode 'text
  #:exists 'replace)

(define empty-md (export-session empty-path 'markdown))
(check-equal? empty-md "" "empty: markdown is empty string")

(define empty-html (export-session empty-path 'html))
(check-true (string-contains? empty-html "<!DOCTYPE html>") "empty: html has structure")

(define empty-json (export-session empty-path 'json))
(define empty-parsed (string->jsexpr empty-json))
(check-equal? (hash-ref (hash-ref empty-parsed 'session) 'entries) '() "empty: json entries empty list")

;; ============================================================
;; Cleanup
;; ============================================================

(delete-directory/files test-session-dir)
