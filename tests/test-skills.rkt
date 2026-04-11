#lang racket

;; tests/test-skills.rkt — Tests für skills/* Module
;;
;; Verifiziert:
;; 1. skills/types.rkt exportiert resource/resource-set structs und render-template
;; 2. skills/prompt-template.rkt exportiert render-template (ohne runtime Abhängigkeit)
;; 3. skills/skill-loader.rkt exportiert resource-* Funktionen (ohne runtime Abhängigkeit)
;; 4. Keine Aufwärtsabhängigkeit von skills/* zu runtime/*

(require rackunit
         racket/file)

;; ============================================================
;; Test 1: skills/types.rkt exports
;; ============================================================

(module+ test
  (test-case "skills/types.rkt exports resource struct"
    (dynamic-require "../skills/types.rkt" 'resource)
    (dynamic-require "../skills/types.rkt" 'resource?)
    (dynamic-require "../skills/types.rkt" 'resource-kind)
    (dynamic-require "../skills/types.rkt" 'resource-name)
    (dynamic-require "../skills/types.rkt" 'resource-source)
    (dynamic-require "../skills/types.rkt" 'resource-content))

  (test-case "skills/types.rkt exports resource-set struct"
    (dynamic-require "../skills/types.rkt" 'resource-set)
    (dynamic-require "../skills/types.rkt" 'resource-set?)
    (dynamic-require "../skills/types.rkt" 'resource-set-instructions)
    (dynamic-require "../skills/types.rkt" 'resource-set-skills)
    (dynamic-require "../skills/types.rkt" 'resource-set-templates)
    (dynamic-require "../skills/types.rkt" 'resource-set-config))

  (test-case "skills/types.rkt exports empty-resource-set constructor"
    (define empty-resource-set (dynamic-require "../skills/types.rkt" 'empty-resource-set))
    (define resource-set? (dynamic-require "../skills/types.rkt" 'resource-set?))
    (check-pred procedure? empty-resource-set)
    (define rs (empty-resource-set))
    (check-true (resource-set? rs)))

  (test-case "skills/types.rkt exports render-template"
    (define render-template (dynamic-require "../skills/types.rkt" 'render-template))
    (check-pred procedure? render-template)
    (check-equal? (render-template "Hello {{name}}!" (hash "name" "World")) "Hello World!")
    (check-equal? (render-template "No vars here" (hash)) "No vars here")
    (check-equal? (render-template "Missing {{var}}" (hash)) "Missing {{var}}"))

  (test-case "skills/types.rkt exports helper functions"
    (dynamic-require "../skills/types.rkt" 'merge-resources)
    (dynamic-require "../skills/types.rkt" 'load-global-resources)
    (dynamic-require "../skills/types.rkt" 'load-project-resources)))

;; ============================================================
;; Test 2: skills/prompt-template.rkt exports (keine runtime Abhängigkeit)
;; ============================================================

(module+ test
  (test-case "prompt-template.rkt exports render-template"
    (define render-template (dynamic-require "../skills/prompt-template.rkt" 'render-template))
    (check-pred procedure? render-template)
    (check-equal? (render-template "Hello {{name}}!" (hash "name" "World")) "Hello World!")))

;; ============================================================
;; Test 3: skills/skill-loader.rkt exports (keine runtime Abhängigkeit)
;; ============================================================

(module+ test
  (test-case "skill-loader.rkt exports resource struct accessors"
    (dynamic-require "../skills/skill-loader.rkt" 'resource)
    (dynamic-require "../skills/skill-loader.rkt" 'resource-name)
    (dynamic-require "../skills/skill-loader.rkt" 'resource-content)
    (dynamic-require "../skills/skill-loader.rkt" 'resource-set)
    (dynamic-require "../skills/skill-loader.rkt" 'resource-set-skills))

  (test-case "skill-loader.rkt exports load functions"
    (dynamic-require "../skills/skill-loader.rkt" 'load-global-resources)
    (dynamic-require "../skills/skill-loader.rkt" 'load-project-resources)
    (dynamic-require "../skills/skill-loader.rkt" 'merge-resources))

  (test-case "skill-loader.rkt exports work correctly"
    (define empty-rs (dynamic-require "../skills/skill-loader.rkt" 'empty-resource-set))
    (define merge-resources (dynamic-require "../skills/skill-loader.rkt" 'merge-resources))
    (check-pred procedure? empty-rs)
    (check-true (procedure? merge-resources))))

;; ============================================================
;; Test 4: Keine Aufwärtsabhängigkeit zu runtime/*
;; ============================================================

(module+ test
  (test-case "skills/types.rkt does not import from runtime/*"
    ;; Dieser Test liest die Datei und prüft auf verbotene Imports
    (define types-content (file->string "../skills/types.rkt"))
    (check-false (regexp-match? #rx"runtime/" types-content)
                 "skills/types.rkt sollte nicht aus runtime/* importieren"))

  (test-case "prompt-template.rkt does not import from runtime/*"
    (define prompt-content (file->string "../skills/prompt-template.rkt"))
    (check-false (regexp-match? #rx"runtime/" prompt-content)
                 "prompt-template.rkt sollte nicht aus runtime/* importieren"))

  (test-case "skill-loader.rkt does not import from runtime/*"
    (define loader-content (file->string "../skills/skill-loader.rkt"))
    (check-false (regexp-match? #rx"runtime/" loader-content)
                 "skill-loader.rkt sollte nicht aus runtime/* importieren")))

;; ============================================================
;; Test 5: Funktionalitätserhalt (Integration)
;; ============================================================

(module+ test
  (test-case "render-template handles symbol keys"
    (define render-template (dynamic-require "../skills/types.rkt" 'render-template))
    (check-equal? (render-template "Hello {{name}}!" (hash 'name "World")) "Hello World!"))

  (test-case "render-template handles whitespace in var names"
    (define render-template (dynamic-require "../skills/types.rkt" 'render-template))
    (check-equal? (render-template "Hello {{ name }}!" (hash "name" "World")) "Hello World!"))

  (test-case "empty-resource-set creates valid empty set"
    (define empty-resource-set (dynamic-require "../skills/types.rkt" 'empty-resource-set))
    (define resource-set? (dynamic-require "../skills/types.rkt" 'resource-set?))
    (define resource-set-instructions (dynamic-require "../skills/types.rkt" 'resource-set-instructions))
    (define resource-set-skills (dynamic-require "../skills/types.rkt" 'resource-set-skills))
    (define rs (empty-resource-set))
    (check-pred resource-set? rs)
    (check-equal? (length (resource-set-instructions rs)) 0)
    (check-equal? (length (resource-set-skills rs)) 0))

  (test-case "merge-resources combines resource sets correctly"
    (define empty-resource-set (dynamic-require "../skills/types.rkt" 'empty-resource-set))
    (define merge-resources (dynamic-require "../skills/types.rkt" 'merge-resources))
    (define resource-set? (dynamic-require "../skills/types.rkt" 'resource-set?))
    (define resource-set-instructions (dynamic-require "../skills/types.rkt" 'resource-set-instructions))
    (define resource-set-skills (dynamic-require "../skills/types.rkt" 'resource-set-skills))

    (define global-rs (empty-resource-set))
    (define project-rs (empty-resource-set))
    (define merged (merge-resources global-rs project-rs))

    (check-pred resource-set? merged)
    (check-equal? (length (resource-set-instructions merged)) 0)))
