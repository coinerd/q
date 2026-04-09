#lang racket

;; tests/test-context-files.rkt — Tests für skills/context-files.rkt

(require rackunit
         rackunit/text-ui
         racket/file
         racket/string
         "../skills/context-files.rkt")

;; ============================================================
;; Test Fixtures
;; ============================================================

(define minimal-agents-md
  "# Test Agent

Eine einfache Testbeschreibung.
")

(define full-agents-md
  "# Code Reviewer Agent

Ein spezialisierter Agent für Code-Reviews und Refactorings.
Dieser Agent analysiert Code auf Qualität und gibt Verbesserungsvorschläge.

## System Instructions

Du bist ein erfahrener Code-Reviewer. Analysiere den übergebenen Code:
1. Prüfe auf Sicherheitslücken
2. Identifiziere Performance-Probleme
3. Schlage Refactorings vor

Sei konstruktiv und präzise in deinen Bewertungen.

## Examples

### Example 1: Simple Function Review
User: Bitte reviewe diese Funktion:
```python
def add(a, b):
    return a + b
```

Agent: Die Funktion ist korrekt, aber verbesserungswürdig:
- Füge Typ-Annotationen hinzu
- Dokumentiere mit Docstrings

### Example 2: Security Review
User: Ist dieser Code sicher?
```python
eval(user_input)
```

Agent: ⚠️ KRITISCHE SICHERHEITSLÜCKE!
- `eval()` mit Benutzereingaben ist gefährlich
- Verwende stattdessen `ast.literal_eval()`

## Tool Preferences

- tool: read
  when: Beim Analysieren von Dateien
  description: Lese Dateien zur Code-Analyse

- tool: edit
  when: Nach dem Review, wenn Änderungen nötig sind
  description: Wende vorgeschlagene Änderungen an
")

(define agents-without-examples-md
  "# Simple Agent

Ein Agent ohne Beispiele.

## System Instructions

Tu einfach, was ich sage.
")

(define agents-without-tool-prefs-md
  "# Minimal Agent

Ein Agent ohne Tool-Präferenzen.

## System Instructions

Sei hilfreich.

## Examples

### Example 1
User: Hallo
Agent: Hallo! Wie kann ich helfen?
")

(define complex-formatting-md
  "#   Complex Agent

  Diese Beschreibung hat Einrückungen
  und mehrere Zeilen.

  Mit einer Leerzeile dazwischen.

## System Instructions

Die Anweisungen haben auch
mehrere Zeilen und

Absätze.

## Examples

### Example 1: Complex Case
User: Erste Zeile
Zweite Zeile des Inputs

Agent: Erste Antwortzeile
Zweite Antwortzeile

Mit Absatz.

### Example 2
User: Kurz
Agent: Auch kurz

## Tool Preferences

- tool: grep
  when: Suchen in Dateien

- tool: find
  when: Dateien finden
  description: Finnt Dateien nach Pattern

- tool: read
  when: Dateien lesen
  description: Lese Dateiinhalte
")

;; ============================================================
;; Tests
;; ============================================================

(define context-files-tests
  (test-suite
   "context-files.rkt Tests"
   
   ;; Test 1: Minimaler Agent
   (test-case "Parse minimal AGENTS.md"
     (define ctx (parse-agent-file minimal-agents-md))
     (check-true (agent-context? ctx))
     (check-equal? (agent-context-name ctx) "Test Agent")
     (check-equal? (agent-context-description ctx) "Eine einfache Testbeschreibung.")
     (check-equal? (agent-context-instructions ctx) "")
     (check-equal? (agent-context-examples ctx) '())
     (check-equal? (agent-context-tool-preferences ctx) '()))
   
   ;; Test 2: Vollständiger Agent mit allen Sektionen
   (test-case "Parse full AGENTS.md with all sections"
     (define ctx (parse-agent-file full-agents-md))
     (check-true (agent-context? ctx))
     (check-equal? (agent-context-name ctx) "Code Reviewer Agent")
     (check-true (string-contains? (agent-context-description ctx)
                                   "Code-Reviews und Refactorings"))
     (check-true (string-contains? (agent-context-instructions ctx)
                                   "erfahrener Code-Reviewer"))
     (check-equal? (length (agent-context-examples ctx)) 2)
     ;; Prüfe erstes Beispiel
     (define ex1 (first (agent-context-examples ctx)))
     (check-equal? (hash-ref ex1 'title) "Example 1: Simple Function Review")
     (check-true (string-contains? (hash-ref ex1 'user) "def add(a, b)"))
     (check-true (string-contains? (hash-ref ex1 'agent) "Typ-Annotationen"))
     ;; Prüfe Tool-Präferenzen
     (check-equal? (length (agent-context-tool-preferences ctx)) 2)
     (define pref1 (first (agent-context-tool-preferences ctx)))
     (check-equal? (hash-ref pref1 'tool) "read")
     (check-true (string-contains? (hash-ref pref1 'when) "Dateien")))
   
   ;; Test 3: Agent ohne Examples-Sektion
   (test-case "Parse AGENTS.md without examples"
     (define ctx (parse-agent-file agents-without-examples-md))
     (check-true (agent-context? ctx))
     (check-equal? (agent-context-name ctx) "Simple Agent")
     (check-equal? (agent-context-description ctx) "Ein Agent ohne Beispiele.")
     (check-true (string-contains? (agent-context-instructions ctx)
                                   "Tu einfach, was ich sage."))
     (check-equal? (agent-context-examples ctx) '())
     (check-equal? (agent-context-tool-preferences ctx) '()))
   
   ;; Test 4: Agent ohne Tool Preferences
   (test-case "Parse AGENTS.md without tool preferences"
     (define ctx (parse-agent-file agents-without-tool-prefs-md))
     (check-true (agent-context? ctx))
     (check-equal? (agent-context-name ctx) "Minimal Agent")
     (check-equal? (agent-context-description ctx) "Ein Agent ohne Tool-Präferenzen.")
     (check-equal? (length (agent-context-examples ctx)) 1)
     (define ex (first (agent-context-examples ctx)))
     (check-equal? (hash-ref ex 'user) "Hallo")
     (check-equal? (hash-ref ex 'agent) "Hallo! Wie kann ich helfen?")
     (check-equal? (agent-context-tool-preferences ctx) '()))
   
   ;; Test 5: Komplexe Formatierung mit mehrzeiligen Einträgen
   (test-case "Parse AGENTS.md with complex formatting"
     (define ctx (parse-agent-file complex-formatting-md))
     (check-true (agent-context? ctx))
     (check-equal? (agent-context-name ctx) "Complex Agent")
     ;; Beschreibung sollte alle Zeilen enthalten
     (check-true (string-contains? (agent-context-description ctx)
                                   "Einrückungen"))
     (check-true (string-contains? (agent-context-description ctx)
                                   "mehrere Zeilen"))
     ;; System Instructions sollten alle Absätze enthalten
     (check-true (string-contains? (agent-context-instructions ctx)
                                   "mehrere Zeilen"))
     ;; Mehrere Beispiele
     (check-equal? (length (agent-context-examples ctx)) 2)
     ;; Erstes Beispiel mit mehrzeiligem Content
     (define ex1 (first (agent-context-examples ctx)))
     (check-true (string-contains? (hash-ref ex1 'user) "Zweite Zeile"))
     (check-true (string-contains? (hash-ref ex1 'agent) "Mit Absatz."))
     ;; Tool-Präferenzen (3 Stück)
     (check-equal? (length (agent-context-tool-preferences ctx)) 3)
     (define prefs (agent-context-tool-preferences ctx))
     (check-equal? (hash-ref (first prefs) 'tool) "grep")
     (check-equal? (hash-ref (second prefs) 'tool) "find")
     (check-equal? (hash-ref (third prefs) 'tool) "read"))
   
   ;; Test 6: load-agent-context aus Datei
   (test-case "Load agent context from file"
     (define temp-dir (make-temporary-file "agenttest~a" 'directory))
     (define agents-file (build-path temp-dir "AGENTS.md"))
     (display-to-file minimal-agents-md agents-file #:exists 'replace)
     
     (define ctx (load-agent-context temp-dir))
     (check-true (agent-context? ctx))
     (check-equal? (agent-context-name ctx) "Test Agent")
     
     ;; Cleanup
     (delete-directory/files temp-dir))
   
   ;; Test 7: load-agent-context für nicht-existente Datei
   (test-case "Load agent context from non-existent file returns #f"
     (define temp-dir (make-temporary-file "agenttest~a" 'directory))
     (define result (load-agent-context temp-dir))
     (check-false result)
     (delete-directory/files temp-dir))
   
   ;; Test 8: Agent-Name ohne H1-Überschrift
   (test-case "Parse AGENTS.md without H1 heading"
     (define content "Keine Überschrift hier.")
     (define ctx (parse-agent-file content))
     (check-true (agent-context? ctx))
     (check-equal? (agent-context-name ctx) "Unnamed Agent")
     (check-equal? (agent-context-description ctx) "Keine Überschrift hier."))
   
   ;; Test 9: Leere AGENTS.md
   (test-case "Parse empty AGENTS.md"
     (define ctx (parse-agent-file ""))
     (check-true (agent-context? ctx))
     (check-equal? (agent-context-name ctx) "Unnamed Agent")
     (check-equal? (agent-context-description ctx) "")
     (check-equal? (agent-context-instructions ctx) "")
     (check-equal? (agent-context-examples ctx) '())
     (check-equal? (agent-context-tool-preferences ctx) '()))
   
   ;; Test 10: Beispiel mit leerem User/Agent Content
   (test-case "Parse example with minimal content"
     (define content "# Test\n\n## Examples\n\n### Example 1\nUser:\nAgent: ")
     (define ctx (parse-agent-file content))
     (check-equal? (length (agent-context-examples ctx)) 1)
     (define ex (first (agent-context-examples ctx)))
     (check-equal? (hash-ref ex 'user) "")
     (check-equal? (hash-ref ex 'agent) ""))))

;; ============================================================
;; Run Tests
;; ============================================================

(module+ main
  (run-tests context-files-tests 'verbose))

(module+ test
  (run-tests context-files-tests))
