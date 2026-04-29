#lang racket/base

;; extensions/racket-tooling.rkt — Racket Editing Extension
;;
;; Re-export facade + extension registration. Implementation split (v0.22.6 W3):
;;   racket-tooling-helpers.rkt  — Raco commands, file I/O, sexpr, pattern matching
;;   racket-tooling-handlers.rkt — Tool handlers (check, edit, codemod)
;;   racket-tooling.rkt          — Extension registration + re-exports

(require racket/match
         racket/string
         "define-extension.rkt"
         "dynamic-tools.rkt"
         "ext-commands.rkt"
         "context.rkt"
         "hooks.rkt"
         "tool-api.rkt"
         "racket-tooling-helpers.rkt"
         "racket-tooling-handlers.rkt")

(provide the-extension
         racket-tooling-extension
         handle-racket-check
         handle-racket-edit
         handle-racket-codemod
         register-racket-tools
         register-racket-commands
         raco-fmt
         raco-make
         raco-test
         run-raco
         find-form-end)

;; ============================================================
;; Extension registration
;; ============================================================

(define (register-racket-tools ctx _payload)
  (ext-register-tool!
   ctx
   "racket-check"
   (string-append "Run Racket validation on a file. "
                  "Modes: format (raco fmt), syntax/compile (raco make), "
                  "test (raco test), expand (raco expand), all. "
                  "Default: syntax.")
   (hasheq
    'type
    "object"
    'required
    '("path")
    'properties
    (hasheq 'path
            (hasheq 'type "string" 'description "Path to .rkt file")
            'mode
            (hasheq 'type "string" 'description "Check mode: format, syntax, test, expand, all")))
   handle-racket-check)
  (ext-register-tool!
   ctx
   "racket-edit"
   (string-append "Structural s-expression editing for Racket files. "
                  "Modes: replace (exact text), form (pattern/template), "
                  "skeleton (new file), struct-add-field, provide-append, "
                  "cond-insert-clause, match-insert-clause, rewrite-form. "
                  "Validates with raco fmt + raco make after edit, reverts on failure.")
   (hasheq 'type
           "object"
           'required
           '("file")
           'properties
           (hasheq 'file
                   (hasheq 'type "string" 'description "Path to .rkt file")
                   'mode
                   (hasheq 'type "string" 'description "Edit mode")
                   'oldText
                   (hasheq 'type "string" 'description "Text to find (replace mode)")
                   'newText
                   (hasheq 'type "string" 'description "Replacement text")
                   'pattern
                   (hasheq 'type "string" 'description "S-expression pattern (form mode)")
                   'template
                   (hasheq 'type "string" 'description "S-expression template (form mode)")
                   'startLine
                   (hasheq 'type "integer" 'description "Start line (rewrite-form)")
                   'endLine
                   (hasheq 'type "integer" 'description "End line (rewrite-form)")
                   'content
                   (hasheq 'type "string" 'description "New content (rewrite-form, skeleton)")
                   'structName
                   (hasheq 'type "string" 'description "Struct name (struct-add-field)")
                   'fieldName
                   (hasheq 'type "string" 'description "Field name (struct-add-field)")
                   'ids
                   (hasheq 'type "string" 'description "Comma-separated IDs (provide-append)")
                   'clause
                   (hasheq 'type "string" 'description "Clause to insert (cond/match-insert)")
                   'anchorClause
                   (hasheq 'type "string" 'description "Anchor clause text")
                   'insertPosition
                   (hasheq 'type "string" 'description "before or after")
                   'lang
                   (hasheq 'type "string" 'description "Language for skeleton")
                   'requires
                   (hasheq 'type "string" 'description "Comma-separated requires (skeleton)")
                   'provides
                   (hasheq 'type "string" 'description "Comma-separated provides (skeleton)")
                   'signatures
                   (hasheq 'type "string" 'description "Newline-separated signatures (skeleton)")))
   handle-racket-edit)
  (ext-register-tool!
   ctx
   "racket-codemod"
   (string-append "Pattern/template codemod for Racket files. "
                  "Uses bar-quoted @@PLACEHOLDER atoms to match any subtree. "
                  "Dry run by default; set write=true to apply. "
                  "Validates with raco fmt + raco make, reverts on failure.")
   (hasheq 'type
           "object"
           'required
           '("file" "pattern" "template")
           'properties
           (hasheq 'file
                   (hasheq 'type "string" 'description "Path to .rkt file")
                   'pattern
                   (hasheq 'type "string" 'description "Pattern with @@PLACEHOLDER atoms")
                   'template
                   (hasheq 'type "string" 'description "Replacement template")
                   'write
                   (hasheq 'type "boolean" 'description "Apply changes (default: false)")))
   handle-racket-codemod)
  (hook-pass ctx))

(define (register-racket-commands ctx)
  (ext-register-command! ctx "/fmt" "Format a Racket file" 'general '() '("f"))
  (ext-register-command! ctx "/check" "Compile-check a Racket file" 'general '() '("c"))
  (ext-register-command! ctx "/expand" "Expand a Racket file" 'general '() '("e"))
  (hook-pass ctx))

(define (handle-racket-command payload)
  (define cmd (hash-ref payload 'command #f))
  (define input-text (hash-ref payload 'input ""))
  (cond
    [(member cmd '("/fmt" "/f"))
     (hook-amend
      (hasheq
       'text
       (format
        "Format a Racket file with raco fmt.~a\nUsage: /fmt <path>\n\nUse the racket-check tool with mode='format' for programmatic access."
        (if (string=? input-text "")
            ""
            (format " File: ~a" input-text)))))]
    [(member cmd '("/check" "/c"))
     (hook-amend
      (hasheq
       'text
       (format
        "Compile-check a Racket file.~a\nUsage: /check <path>\n\nUse the racket-check tool with mode='syntax' for programmatic access."
        (if (string=? input-text "")
            ""
            (format " File: ~a" input-text)))))]
    [(member cmd '("/expand" "/e"))
     (hook-amend
      (hasheq
       'text
       (format
        "Expand macros in a Racket file.~a\nUsage: /expand <path>\n\nUse the racket-check tool with mode='expand' for programmatic access."
        (if (string=? input-text "")
            ""
            (format " File: ~a" input-text)))))]
    [else (hook-pass payload)]))

(define-q-extension racket-tooling-extension
                    #:version "1.0.0"
                    #:api-version "1"
                    #:on register-tools
                    register-racket-tools
                    #:on register-shortcuts
                    register-racket-commands
                    #:on execute-command
                    handle-racket-command)

(define the-extension racket-tooling-extension)
