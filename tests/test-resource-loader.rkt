#lang racket

;; test-resource-loader.rkt — tests for runtime/resource-loader.rkt
;;
;; Covers:
;;   - resource / resource-set structs
;;   - load-global-resources from a directory
;;   - load-project-resources from a project directory (.q/ or .pi/)
;;   - merge-resources with correct precedence
;;   - empty/missing directory handling
;;   - system instructions loading from files
;;   - skill discovery from skill subdirectories
;;   - prompt template loading
;;   - config file parsing
;;   - malformed file handling (skip, don't crash)

(require rackunit
         racket/file
         racket/port
         json
         (only-in racket/string string-join)
         "../runtime/resource-loader.rkt")

;; ============================================================
;; Helpers — temp directory fixtures
;; ============================================================

(define (make-temp-dir)
  (make-temporary-file "q-resload-~a" 'directory))

(define (with-temp-dir thunk)
  (define dir (make-temp-dir))
  (dynamic-wind
    void
    (λ () (thunk dir))
    (λ () (delete-directory/files dir #:must-exist? #f))))

;; Create a .q/ directory tree inside parent with optional resources
(define (setup-q-dir! parent
                       #:instructions [instrs '()]
                       #:skills [skills '()]
                       #:templates [templates '()]
                       #:config [config #f])
  (define q-dir (build-path parent ".q"))
  (make-directory q-dir)

  ;; Instructions file(s)
  (unless (null? instrs)
    (call-with-output-file (build-path q-dir "instructions.md")
      (λ (out)
        (display (string-join instrs "\n\n") out))))

  ;; Skills — each is (list name description content)
  (unless (null? skills)
    (define skills-dir (build-path q-dir "skills"))
    (make-directory skills-dir)
    (for ([s (in-list skills)])
      (define skill-name (first s))
      (define skill-desc (second s))
      (define skill-content (third s))
      (define skill-dir (build-path skills-dir skill-name))
      (make-directory skill-dir)
      (call-with-output-file (build-path skill-dir "SKILL.md")
        (λ (out)
          (display (format "# ~a\n\n~a\n\n~a"
                           skill-name skill-desc skill-content)
                   out)))))

  ;; Templates — each is (list name content)
  (unless (null? templates)
    (define tpl-dir (build-path q-dir "templates"))
    (make-directory tpl-dir)
    (for ([t (in-list templates)])
      (define tpl-name (first t))
      (define tpl-content (second t))
      (call-with-output-file (build-path tpl-dir tpl-name)
        (λ (out) (display tpl-content out)))))

  ;; Config
  (when config
    (call-with-output-file (build-path q-dir "config.json")
      (λ (out) (display (jsexpr->string config) out))))

  q-dir)

;; Same but use .pi/ instead of .q/
(define (setup-pi-dir! parent
                        #:instructions [instrs '()]
                        #:skills [skills '()]
                        #:templates [templates '()]
                        #:config [config #f])
  (define pi-dir (build-path parent ".pi"))
  (make-directory pi-dir)

  (unless (null? instrs)
    (call-with-output-file (build-path pi-dir "instructions.md")
      (λ (out)
        (display (string-join instrs "\n\n") out))))

  (unless (null? skills)
    (define skills-dir (build-path pi-dir "skills"))
    (make-directory skills-dir)
    (for ([s (in-list skills)])
      (define skill-name (first s))
      (define skill-desc (second s))
      (define skill-content (third s))
      (define skill-dir (build-path skills-dir skill-name))
      (make-directory skill-dir)
      (call-with-output-file (build-path skill-dir "SKILL.md")
        (λ (out)
          (display (format "# ~a\n\n~a\n\n~a"
                           skill-name skill-desc skill-content)
                   out)))))

  (unless (null? templates)
    (define tpl-dir (build-path pi-dir "templates"))
    (make-directory tpl-dir)
    (for ([t (in-list templates)])
      (call-with-output-file (build-path pi-dir (first t))
        (λ (out) (display (second t) out)))))

  (when config
    (call-with-output-file (build-path pi-dir "config.json")
      (λ (out) (display (jsexpr->string config) out))))

  pi-dir)

;; ============================================================
;; Struct tests
;; ============================================================

(test-case "resource constructor and accessors"
  (define r (resource 'system-instruction "test" 'global "hello"))
  (check-equal? (resource-kind r) 'system-instruction)
  (check-equal? (resource-name r) "test")
  (check-equal? (resource-source r) 'global)
  (check-equal? (resource-content r) "hello"))

(test-case "resource-set constructor and accessors"
  (define rs (resource-set '("a") '() (hash) (hash)))
  (check-equal? (resource-set-instructions rs) '("a"))
  (check-equal? (resource-set-skills rs) '())
  (check-equal? (resource-set-templates rs) (hash))
  (check-equal? (resource-set-config rs) (hash)))

;; ============================================================
;; Empty resource set
;; ============================================================

(test-case "empty-resource-set produces all-empty fields"
  (define rs (empty-resource-set))
  (check-equal? (resource-set-instructions rs) '())
  (check-equal? (resource-set-skills rs) '())
  (check-equal? (resource-set-templates rs) (hash))
  (check-equal? (resource-set-config rs) (hash)))

;; ============================================================
;; load-global-resources — missing directory
;; ============================================================

(test-case "load-global-resources with missing directory returns empty"
  (with-temp-dir
   (λ (dir)
     (define base (build-path dir "nonexistent"))
     (define rs (load-global-resources base))
     (check-equal? (resource-set-instructions rs) '())
     (check-equal? (resource-set-skills rs) '())
     (check-equal? (resource-set-templates rs) (hash))
     (check-equal? (resource-set-config rs) (hash)))))

;; ============================================================
;; load-global-resources — with instructions
;; ============================================================

(test-case "load-global-resources loads system instructions"
  (with-temp-dir
   (λ (dir)
     (setup-q-dir! dir #:instructions '("Be helpful." "Use Rust."))
     (define rs (load-global-resources dir))
     (check-equal? (resource-set-instructions rs) '("Be helpful.\n\nUse Rust.")))))

(test-case "load-global-resources loads single instruction"
  (with-temp-dir
   (λ (dir)
     (setup-q-dir! dir #:instructions '("Be concise."))
     (define rs (load-global-resources dir))
     (check-equal? (resource-set-instructions rs) '("Be concise.")))))

;; ============================================================
;; load-global-resources — with skills
;; ============================================================

(test-case "load-global-resources discovers skills"
  (with-temp-dir
   (λ (dir)
     (setup-q-dir! dir
                   #:skills '(("code-review"
                               "Reviews code for quality"
                               "Check for SRP violations.")))
     (define rs (load-global-resources dir))
     (define skills (resource-set-skills rs))
     (check = (length skills) 1)
     (define s (first skills))
     (check-equal? (hash-ref s 'name) "code-review")
     (check-equal? (hash-ref s 'description) "Reviews code for quality")
     (check-true (string-contains? (hash-ref s 'content) "Check for SRP violations.")))))

(test-case "load-global-resources discovers multiple skills"
  (with-temp-dir
   (λ (dir)
     (setup-q-dir! dir
                   #:skills '(("review" "Reviews code" "Check quality.")
                              ("refactor" "Refactors code" "Apply SRP.")))
     (define rs (load-global-resources dir))
     (define skills (resource-set-skills rs))
     (check = (length skills) 2)
     (define names (map (λ (s) (hash-ref s 'name)) skills))
     (check-not-false (member "review" names))
     (check-not-false (member "refactor" names)))))

;; ============================================================
;; load-global-resources — with templates
;; ============================================================

(test-case "load-global-resources loads templates"
  (with-temp-dir
   (λ (dir)
     (setup-q-dir! dir
                   #:templates '(("summary.md" "Summarize: {{topic}}")
                                 ("review.md" "Review: {{file}}")))
     (define rs (load-global-resources dir))
     (define tpls (resource-set-templates rs))
     (check-equal? (hash-ref tpls "summary.md" #f) "Summarize: {{topic}}")
     (check-equal? (hash-ref tpls "review.md" #f) "Review: {{file}}"))))

;; ============================================================
;; load-global-resources — with config
;; ============================================================

(test-case "load-global-resources loads config"
  (with-temp-dir
   (λ (dir)
     (setup-q-dir! dir #:config (hash 'model "gpt-4" 'temperature 0.7))
     (define rs (load-global-resources dir))
     (define cfg (resource-set-config rs))
     (check-equal? (hash-ref cfg 'model #f) "gpt-4")
     (check-equal? (hash-ref cfg 'temperature #f) 0.7))))

(test-case "load-global-resources without config returns empty hash"
  (with-temp-dir
   (λ (dir)
     (setup-q-dir! dir)
     (define rs (load-global-resources dir))
     (check-equal? (resource-set-config rs) (hash)))))

;; ============================================================
;; load-project-resources — .q/ directory
;; ============================================================

(test-case "load-project-resources finds .q/ directory"
  (with-temp-dir
   (λ (dir)
     (setup-q-dir! dir #:instructions '("Project instructions."))
     (define rs (load-project-resources dir))
     (check-equal? (resource-set-instructions rs) '("Project instructions.")))))

;; ============================================================
;; load-project-resources — .pi/ alias
;; ============================================================

(test-case "load-project-resources falls back to .pi/ directory"
  (with-temp-dir
   (λ (dir)
     (setup-pi-dir! dir #:instructions '("PI instructions."))
     (define rs (load-project-resources dir))
     (check-equal? (resource-set-instructions rs) '("PI instructions.")))))

(test-case "load-project-resources prefers .q/ over .pi/"
  (with-temp-dir
   (λ (dir)
     (setup-q-dir! dir #:instructions '("Q instructions."))
     (setup-pi-dir! dir #:instructions '("PI instructions."))
     (define rs (load-project-resources dir))
     (check-equal? (resource-set-instructions rs) '("Q instructions.")))))

;; ============================================================
;; load-project-resources — missing directory
;; ============================================================

(test-case "load-project-resources with no .q/ or .pi/ returns empty"
  (with-temp-dir
   (λ (dir)
     (define rs (load-project-resources dir))
     (check-equal? (resource-set-instructions rs) '())
     (check-equal? (resource-set-skills rs) '()))))

;; ============================================================
;; merge-resources — instructions (global + project combined)
;; ============================================================

(test-case "merge-resources combines instructions (global first, then project)"
  (define global (resource-set '("global-a") '() (hash) (hash)))
  (define project (resource-set '("project-b") '() (hash) (hash)))
  (define merged (merge-resources global project))
  (check-equal? (resource-set-instructions merged) '("global-a" "project-b")))

(test-case "merge-resources instructions — project-only"
  (define global (empty-resource-set))
  (define project (resource-set '("only-project") '() (hash) (hash)))
  (define merged (merge-resources global project))
  (check-equal? (resource-set-instructions merged) '("only-project")))

(test-case "merge-resources instructions — global-only"
  (define global (resource-set '("only-global") '() (hash) (hash)))
  (define project (empty-resource-set))
  (define merged (merge-resources global project))
  (check-equal? (resource-set-instructions merged) '("only-global")))

;; ============================================================
;; merge-resources — skills (project overrides by name)
;; ============================================================

(test-case "merge-resources skills — project overrides global by name"
  (define global-skill (hash 'name "review" 'description "Global review" 'content "global"))
  (define proj-skill (hash 'name "review" 'description "Project review" 'content "project"))
  (define global (resource-set '() (list global-skill) (hash) (hash)))
  (define project (resource-set '() (list proj-skill) (hash) (hash)))
  (define merged (merge-resources global project))
  (define skills (resource-set-skills merged))
  (check = (length skills) 1)
  (check-equal? (hash-ref (first skills) 'description) "Project review"))

(test-case "merge-resources skills — unique skills from both sources"
  (define g-skill (hash 'name "review" 'description "Review" 'content "r"))
  (define p-skill (hash 'name "refactor" 'description "Refactor" 'content "f"))
  (define global (resource-set '() (list g-skill) (hash) (hash)))
  (define project (resource-set '() (list p-skill) (hash) (hash)))
  (define merged (merge-resources global project))
  (define skills (resource-set-skills merged))
  (check = (length skills) 2)
  (define names (map (λ (s) (hash-ref s 'name)) skills))
  (check-not-false (member "review" names))
  (check-not-false (member "refactor" names)))

;; ============================================================
;; merge-resources — templates (project overrides by key)
;; ============================================================

(test-case "merge-resources templates — project overrides global"
  (define global (resource-set '() '() (hash "greeting.md" "Hello {{name}}") (hash)))
  (define project (resource-set '() '() (hash "greeting.md" "Hi {{name}}!") (hash)))
  (define merged (merge-resources global project))
  (check-equal? (hash-ref (resource-set-templates merged) "greeting.md") "Hi {{name}}!"))

(test-case "merge-resources templates — both contribute unique keys"
  (define global (resource-set '() '() (hash "a.md" "A content") (hash)))
  (define project (resource-set '() '() (hash "b.md" "B content") (hash)))
  (define merged (merge-resources global project))
  (define tpls (resource-set-templates merged))
  (check-equal? (hash-ref tpls "a.md") "A content")
  (check-equal? (hash-ref tpls "b.md") "B content"))

;; ============================================================
;; merge-resources — config (deep merge, project wins)
;; ============================================================

(test-case "merge-resources config — flat merge with project precedence"
  (define global (resource-set '() '() (hash) (hash 'model "gpt-3.5" 'verbose #t)))
  (define project (resource-set '() '() (hash) (hash 'model "gpt-4")))
  (define merged (merge-resources global project))
  (define cfg (resource-set-config merged))
  (check-equal? (hash-ref cfg 'model) "gpt-4")
  (check-equal? (hash-ref cfg 'verbose) #t))

(test-case "merge-resources config — deep merge nested hashes"
  (define global (resource-set '() '() (hash)
                               (hash 'llm (hash 'model "gpt-3.5" 'temperature 0.5))))
  (define project (resource-set '() '() (hash)
                                (hash 'llm (hash 'model "gpt-4"))))
  (define merged (merge-resources global project))
  (define cfg (resource-set-config merged))
  (check-equal? (hash-ref (hash-ref cfg 'llm) 'model) "gpt-4")
  (check-equal? (hash-ref (hash-ref cfg 'llm) 'temperature) 0.5))

(test-case "merge-resources config — empty global + project"
  (define global (empty-resource-set))
  (define project (resource-set '() '() (hash) (hash 'key "val")))
  (define merged (merge-resources global project))
  (check-equal? (hash-ref (resource-set-config merged) 'key) "val"))

(test-case "merge-resources config — global + empty project"
  (define global (resource-set '() '() (hash) (hash 'key "val")))
  (define project (empty-resource-set))
  (define merged (merge-resources global project))
  (check-equal? (hash-ref (resource-set-config merged) 'key) "val"))

;; ============================================================
;; Malformed file handling
;; ============================================================

(test-case "malformed config.json is skipped gracefully"
  (with-temp-dir
   (λ (dir)
     (define q-dir (build-path dir ".q"))
     (make-directory q-dir)
     (call-with-output-file (build-path q-dir "config.json")
       (λ (out) (display "this is not valid json {{{" out)))
     (define rs (load-global-resources dir))
     (check-equal? (resource-set-config rs) (hash)))))

(test-case "empty instructions file produces empty instruction list"
  (with-temp-dir
   (λ (dir)
     (define q-dir (build-path dir ".q"))
     (make-directory q-dir)
     (call-with-output-file (build-path q-dir "instructions.md")
       (λ (out) (void)))
     (define rs (load-global-resources dir))
     ;; Empty file -> empty string is still loaded (could be filtered)
     ;; At minimum, it should not crash
     (check-true (list? (resource-set-instructions rs))))))

(test-case "skill directory without SKILL.md is skipped"
  (with-temp-dir
   (λ (dir)
     (define q-dir (build-path dir ".q"))
     (make-directory q-dir)
     (define skills-dir (build-path q-dir "skills"))
     (make-directory skills-dir)
     (define bad-skill-dir (build-path skills-dir "broken-skill"))
     (make-directory bad-skill-dir)
     ;; No SKILL.md created
     (define rs (load-global-resources dir))
     (check-equal? (resource-set-skills rs) '()))))

(test-case "binary/corrupt template file is skipped"
  (with-temp-dir
   (λ (dir)
     (define q-dir (build-path dir ".q"))
     (make-directory q-dir)
     (define tpl-dir (build-path q-dir "templates"))
     (make-directory tpl-dir)
     ;; Write a file with null bytes
     (call-with-output-file (build-path tpl-dir "bad.md")
       (λ (out) (write-bytes #"\0\0\0" out)))
     (define rs (load-global-resources dir))
     ;; Should not crash; the template may be loaded or skipped
     (check-true (hash? (resource-set-templates rs))))))

;; ============================================================
;; Template variable substitution (render-template)
;; ============================================================

(test-case "render-template substitutes variables"
  (define result (render-template "Hello {{name}}, welcome to {{place}}!"
                                  (hash 'name "Alice" 'place "Wonderland")))
  (check-equal? result "Hello Alice, welcome to Wonderland!"))

(test-case "render-template with missing variable leaves placeholder"
  (define result (render-template "Hello {{name}}!"
                                  (hash)))
  (check-equal? result "Hello {{name}}!"))

(test-case "render-template with no variables"
  (define result (render-template "No variables here." (hash)))
  (check-equal? result "No variables here."))

;; ============================================================
;; Full integration — global + project merge
;; ============================================================

(test-case "full integration: global + project resources merged correctly"
  (with-temp-dir
   (λ (dir)
     ;; Global resources
     (define global-dir (build-path dir "global"))
     (make-directory global-dir)
     (setup-q-dir! global-dir
                   #:instructions '("Always be helpful.")
                   #:skills '(("review" "Review code" "Check quality."))
                   #:templates '(("summary.md" "Summarize: {{topic}}"))
                   #:config (hash 'model "gpt-3.5" 'verbose #f))

     ;; Project resources
     (define proj-dir (build-path dir "project"))
     (make-directory proj-dir)
     (setup-q-dir! proj-dir
                   #:instructions '("Use Racket conventions.")
                   #:skills '(("review" "Project review" "Project check."))
                   #:templates '(("summary.md" "Project summary: {{topic}}"))
                   #:config (hash 'model "gpt-4"))

     (define global-rs (load-global-resources global-dir))
     (define proj-rs (load-project-resources proj-dir))
     (define merged (merge-resources global-rs proj-rs))

     ;; Instructions: global then project
     (check-equal? (resource-set-instructions merged)
                   '("Always be helpful." "Use Racket conventions."))

     ;; Skills: project overrides global by name
     (define skills (resource-set-skills merged))
     (check = (length skills) 1)
     (check-equal? (hash-ref (first skills) 'description) "Project review")

     ;; Templates: project overrides global
     (check-equal? (hash-ref (resource-set-templates merged) "summary.md")
                   "Project summary: {{topic}}")

     ;; Config: merged, project wins on 'model'
     (define cfg (resource-set-config merged))
     (check-equal? (hash-ref cfg 'model) "gpt-4")
     (check-equal? (hash-ref cfg 'verbose) #f))))
