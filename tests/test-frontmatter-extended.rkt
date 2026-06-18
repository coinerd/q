#lang racket

;; @speed fast
;; @suite skills

;; tests/test-frontmatter-extended.rkt
;; v0.99.26 W1: Extended frontmatter parser tests.
;; Tests YAML-subset parsing: flat values, lists, nested maps, inline arrays.

(require rackunit
         rackunit/text-ui
         "../skills/frontmatter.rkt")

(define suite
  (test-suite "Extended Frontmatter Parser (v0.99.26 W1)"

    ;; ---- Flat key:value (backward compatible) ----
    (test-case "flat key:value parsed as strings"
      (define fm
        (parse-skill-frontmatter-extended
         "---\nname: my-skill\ndescription: A test skill\n---\nbody"))
      (check-not-false fm)
      (check-equal? (hash-ref fm 'name) "my-skill")
      (check-equal? (hash-ref fm 'description) "A test skill"))

    ;; ---- Quoted values ----
    (test-case "quoted values stripped"
      (define fm
        (parse-skill-frontmatter-extended
         "---\nname: \"my-skill\"\ndescription: 'A test skill'\n---\n"))
      (check-equal? (hash-ref fm 'name) "my-skill")
      (check-equal? (hash-ref fm 'description) "A test skill"))

    ;; ---- Inline arrays ----
    (test-case "inline array parsed as list of strings"
      (define fm
        (parse-skill-frontmatter-extended
         "---\nname: test\ntags: [research, analysis, report]\n---\n"))
      (check-equal? (hash-ref fm 'tags) '("research" "analysis" "report")))

    (test-case "inline array with quoted elements"
      (define fm
        (parse-skill-frontmatter-extended "---\nname: test\ntags: [\"alpha\", 'beta', gamma]\n---\n"))
      (check-equal? (hash-ref fm 'tags) '("alpha" "beta" "gamma")))

    (test-case "empty inline array"
      (define fm (parse-skill-frontmatter-extended "---\nname: test\ntags: []\n---\n"))
      (check-equal? (hash-ref fm 'tags) '()))

    ;; ---- List of strings ----
    (test-case "list of string items"
      (define fm
        (parse-skill-frontmatter-extended "---\nname: test\nvariables:\n  - topic\n  - depth\n---\n"))
      (check-equal? (hash-ref fm 'variables) '("topic" "depth")))

    ;; ---- List of maps (steps) ----
    (test-case "list of maps with role and task"
      (define fm
        (parse-skill-frontmatter-extended (string-append "---\n"
                                                         "name: research-workflow\n"
                                                         "steps:\n"
                                                         "  - role: researcher\n"
                                                         "    task: Find information\n"
                                                         "  - role: writer\n"
                                                         "    task: Write report\n"
                                                         "---\n")))
      (define steps (hash-ref fm 'steps))
      (check-pred list? steps)
      (check-equal? (length steps) 2)
      (check-equal? (hash-ref (car steps) 'role) "researcher")
      (check-equal? (hash-ref (car steps) 'task) "Find information")
      (check-equal? (hash-ref (cadr steps) 'role) "writer")
      (check-equal? (hash-ref (cadr steps) 'task) "Write report"))

    ;; ---- List of maps with nested list ----
    (test-case "list of maps with nested capabilities list"
      (define fm
        (parse-skill-frontmatter-extended (string-append "---\n"
                                                         "name: complex-workflow\n"
                                                         "steps:\n"
                                                         "  - role: researcher\n"
                                                         "    task: Research topic\n"
                                                         "    capabilities:\n"
                                                         "      - read-only\n"
                                                         "      - network\n"
                                                         "  - role: writer\n"
                                                         "    task: Write report\n"
                                                         "    capabilities:\n"
                                                         "      - read-only\n"
                                                         "---\n")))
      (define steps (hash-ref fm 'steps))
      (check-equal? (length steps) 2)
      (define step1 (car steps))
      (check-equal? (hash-ref step1 'role) "researcher")
      (check-equal? (hash-ref step1 'capabilities) '("read-only" "network"))
      (define step2 (cadr steps))
      (check-equal? (hash-ref step2 'capabilities) '("read-only")))

    ;; ---- Comments and blank lines ----
    (test-case "comments and blank lines ignored"
      (define fm
        (parse-skill-frontmatter-extended (string-append "---\n"
                                                         "# This is a comment\n"
                                                         "name: test\n"
                                                         "\n"
                                                         "description: A skill\n"
                                                         "---\n")))
      (check-not-false fm)
      (check-equal? (hash-ref fm 'name) "test")
      (check-equal? (hash-ref fm 'description) "A skill"))

    ;; ---- Error cases ----
    (test-case "no frontmatter returns #f"
      (check-false (parse-skill-frontmatter-extended "no frontmatter here")))

    (test-case "missing closing --- returns #f"
      (check-false (parse-skill-frontmatter-extended "---\nname: test\nbut no closing")))

    ;; ---- Full mas-workflow frontmatter ----
    (test-case "full mas-workflow frontmatter"
      (define fm
        (parse-skill-frontmatter-extended
         (string-append "---\n"
                        "name: research-and-write\n"
                        "description: Research a topic then write a report\n"
                        "type: mas-workflow\n"
                        "steps:\n"
                        "  - role: researcher\n"
                        "    task: Research {{topic}} and gather key findings\n"
                        "    capabilities:\n"
                        "      - read-only\n"
                        "      - network\n"
                        "  - role: writer\n"
                        "    task: Write a report based on {{result}}\n"
                        "    capabilities:\n"
                        "      - read-only\n"
                        "variables:\n"
                        "  - topic\n"
                        "tags: [research, writing, mas-workflow]\n"
                        "---\n")))
      (check-equal? (hash-ref fm 'name) "research-and-write")
      (check-equal? (hash-ref fm 'type) "mas-workflow")
      (define steps (hash-ref fm 'steps))
      (check-equal? (length steps) 2)
      (check-equal? (hash-ref (car steps) 'role) "researcher")
      (check-equal? (hash-ref (cadr steps) 'task) "Write a report based on {{result}}")
      (check-equal? (hash-ref fm 'variables) '("topic"))
      (check-equal? (hash-ref fm 'tags) '("research" "writing" "mas-workflow")))

    ;; ---- 4-space indentation ----
    (test-case "4-space indentation works"
      (define fm
        (parse-skill-frontmatter-extended (string-append "---\n"
                                                         "name: test\n"
                                                         "steps:\n"
                                                         "    - role: planner\n"
                                                         "      task: Make a plan\n"
                                                         "---\n")))
      (define steps (hash-ref fm 'steps))
      (check-equal? (length steps) 1)
      (check-equal? (hash-ref (car steps) 'role) "planner"))))

(run-tests suite)
