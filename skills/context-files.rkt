#lang racket/base

;; skills/context-files.rkt — Parser für AGENTS.md Dateien
;;
;; Dieses Modul parst AGENTS.md Dateien im Markdown-Format und extrahiert
;; Kontext-Informationen für Agenten (Titel, Beschreibung, Instruktionen,
;; Beispiele, Tool-Präferenzen).

(require racket/file
         racket/string
         racket/match
         racket/list
         (only-in "types.rkt" try-read-file))

(provide
 ;; Struct
 (struct-out agent-context)
 agent-context?

 ;; Accessoren
 agent-context-name
 agent-context-description
 agent-context-instructions
 agent-context-examples
 agent-context-tool-preferences

 ;; Parser-Funktionen
 load-agent-context
 parse-agent-file)

;; ============================================================
;; Structs
;; ============================================================

;; Repräsentiert einen geparsten Agenten-Kontext aus einer AGENTS.md Datei
(struct agent-context
  (name           ; string — Name/Titel des Agenten
   description    ; string — Kurzbeschreibung
   instructions   ; string — System-Instruktionen
   examples       ; (listof hash) — Beispiel-Konversationen
   tool-preferences) ; (listof hash) — Tool-Präferenzen
  #:transparent)

;; ============================================================
;; Hilfsfunktionen
;; ============================================================

;; Liest eine Datei als String, gibt #f bei Fehler
;; (imported from skills/types.rkt to avoid duplication #120))

;; Prüft ob eine Zeile eine Überschrift ist
(define (heading-level line)
  (define trimmed (string-trim line))
  (cond
    [(string-prefix? trimmed "# ") 1]
    [(string-prefix? trimmed "## ") 2]
    [(string-prefix? trimmed "### ") 3]
    [(string-prefix? trimmed "#### ") 4]
    [else #f]))

;; Extrahiert den Text einer Überschrift (ohne #)
(define (heading-text line)
  (define trimmed (string-trim line))
  (string-trim
   (cond
     [(string-prefix? trimmed "# ") (substring trimmed 2)]
     [(string-prefix? trimmed "## ") (substring trimmed 3)]
     [(string-prefix? trimmed "### ") (substring trimmed 4)]
     [(string-prefix? trimmed "#### ") (substring trimmed 5)]
     [else trimmed])))

;; Findet den Index einer Zeile die einem Prädikat entspricht
(define (find-index lines pred)
  (let loop ([i 0] [ls lines])
    (cond
      [(null? ls) #f]
      [(pred (car ls)) i]
      [else (loop (add1 i) (cdr ls))])))

;; Nimmt Zeilen bis zu einem Prädikat (exklusive)
(define (take-until lines pred)
  (let loop ([acc '()] [ls lines])
    (cond
      [(null? ls) (reverse acc)]
      [(pred (car ls)) (reverse acc)]
      [else (loop (cons (car ls) acc) (cdr ls))])))

;; Droppt Zeilen bis zu einem Prädikat (exklusive)
(define (drop-until lines pred)
  (cond
    [(null? lines) '()]
    [(pred (car lines)) (cdr lines)]
    [else (drop-until (cdr lines) pred)]))

;; Extrahiert den Inhalt einer Sektion (von ## Überschrift bis zur nächsten ## oder ###)
(define (extract-section lines section-name)
  (define section-heading
    (string-downcase (string-trim section-name)))
  (define start-idx
    (find-index lines
                (λ (l)
                  (and (= (or (heading-level l) 0) 2)
                       (string=? (string-downcase (heading-text l))
                                 section-heading)))))
  (cond
    [(not start-idx) ""]
    [else
     (define after-heading (list-tail lines (add1 start-idx)))
     (define content-lines
       (take-until after-heading
                   (λ (l) (and (heading-level l)
                               (<= (heading-level l) 2)))))
     (string-trim (string-join content-lines "\n"))]))

;; Extrahiert alle Beispiele aus der Examples-Sektion
(define (extract-examples lines)
  (define examples-section (extract-section lines "Examples"))
  (if (string=? examples-section "")
      '()
      (parse-examples examples-section)))

;; Parst Beispiele aus dem Text einer Examples-Sektion
(define (parse-examples section-text)
  (define lines (string-split section-text "\n"))

  ;; Finde alle ### Example X Überschriften
  (define example-indices
    (filter-map
     (λ (line idx)
       (and (= (or (heading-level line) 0) 3)
            (regexp-match? #rx"(?i:example)" (heading-text line))
            idx))
     lines
     (range (length lines))))

  (if (null? example-indices)
      '()
      (for/list ([start-idx (in-list example-indices)])
        (define title (heading-text (list-ref lines start-idx)))
        (define after-heading (list-tail lines (add1 start-idx)))
        (define end-idx
          (or (find-index after-heading
                          (λ (l) (= (or (heading-level l) 0) 3)))
              (length after-heading)))
        (define content-lines (take after-heading end-idx))
        (define content (string-trim (string-join content-lines "\n")))

        ;; Parse User: und Agent: Zeilen
        (define user-text
          (extract-role-content content "User"))
        (define agent-text
          (extract-role-content content "Agent"))

        (hash 'title title
              'user user-text
              'agent agent-text
              'raw content))))

;; Extrahiert Text für eine Rolle (User oder Agent) aus Beispiel-Content
;; Unterstützt sowohl "User: text" (inline) als auch "User:\n text" (multiline)
(define (extract-role-content content role)
  (define lines (string-split content "\n"))
  (define role-pattern (regexp (format "^~a[ \t]*:[ \t]*(.*)$" role)))

  (let loop ([ls lines] [inline-text #f] [acc '()])
    (cond
      [(null? ls)
       (string-trim (string-join (reverse (if inline-text (cons inline-text acc) acc)) "\n"))]
      [else
       (define line (car ls))
       (define rest (cdr ls))
       (define m (regexp-match role-pattern (string-trim line)))
       (cond
         ;; Found our role line — extract inline content after colon
         [(and m (not inline-text))
          (define txt (string-trim (cadr m)))
          (loop rest (if (string=? txt "") #f txt) '())]
         ;; Found another role — stop
         [(and inline-text
               (regexp-match? #rx"^(User|Agent)[ \t]*:" (string-trim line)))
          (string-trim (string-join (reverse (cons inline-text acc)) "\n"))]
         ;; Accumulate continuation lines (including blank lines)
         [inline-text
          (loop rest inline-text (cons line acc))]
         ;; Skip lines before our role
         [else (loop rest #f '())])])))

;; Extrahiert Tool-Präferenzen aus der Tool Preferences Sektion
(define (extract-tool-preferences lines)
  (define prefs-section (extract-section lines "Tool Preferences"))
  (if (string=? prefs-section "")
      '()
      (parse-tool-preferences prefs-section)))

;; Parst Tool-Präferenzen aus dem Text
(define (parse-tool-preferences section-text)
  (define lines (string-split section-text "\n"))

  (let loop ([acc '()] [ls lines] [current-pref #f])
    (cond
      [(null? ls)
       (reverse (if current-pref (cons current-pref acc) acc))]
      [else
       (define line (car ls))
       (define rest (cdr ls))
       (cond
         ;; Neues Tool: - tool: name
         [(regexp-match #rx"^-[ \t]*tool:[ \t]*(.+)$" (string-trim line))
          => (λ (m)
               (define tool-name (cadr m))
               (loop (if current-pref (cons current-pref acc) acc)
                     rest
                     (hash 'tool tool-name 'when "" 'description "")))]
         ;; When: condition
         [(regexp-match #rx"^[ \t]+when:[ \t]*(.+)$" line)
          => (λ (m)
               (define when-text (cadr m))
               (if current-pref
                   (loop acc rest (hash-set current-pref 'when when-text))
                   (loop acc rest current-pref)))]
         ;; Description: text
         [(regexp-match #rx"^[ \t]+description:[ \t]*(.+)$" line)
          => (λ (m)
               (define desc-text (cadr m))
               (if current-pref
                   (loop acc rest (hash-set current-pref 'description desc-text))
                   (loop acc rest current-pref)))]
         ;; Weitere Beschreibungszeilen
         [(and current-pref
               (string-prefix? (string-trim line) "-")
               (not (string-prefix? (string-trim line) "- tool:")))
          (loop acc rest current-pref)]
         ;; Zusätzlicher Beschreibungstext
         [(and current-pref
               (not (string=? (string-trim line) ""))
               (not (regexp-match? #rx"^[ \t]+(when|description):" line)))
          (define existing-desc (hash-ref current-pref 'description ""))
          (define new-desc
            (if (string=? existing-desc "")
                line
                (string-append existing-desc "\n" line)))
          (loop acc rest (hash-set current-pref 'description (string-trim new-desc)))]
         [else
          (loop acc rest current-pref)])])))

;; ============================================================
;; Öffentliche API
;; ============================================================

;; Parst den Inhalt einer AGENTS.md Datei
;; (parse-agent-file content) → agent-context?
(define (parse-agent-file content)
  (define lines (string-split content "\n"))

  ;; Extrahiere Name aus erster # Überschrift
  (define name
    (cond
      [(null? lines) "Unnamed Agent"]
      [(= (or (heading-level (car lines)) 0) 1)
       (heading-text (car lines))]
      [else "Unnamed Agent"]))

  ;; Finde Beschreibung (Text nach erster Überschrift bis zur nächsten ##)
  (define description
    (let ([after-header
           (if (and (not (null? lines))
                    (= (or (heading-level (car lines)) 0) 1))
               (cdr lines)
               lines)])
      (define non-empty-lines
        (dropf after-header (λ (l) (string=? (string-trim l) ""))))
      (define desc-lines
        (take-until non-empty-lines
                    (λ (l) (and (heading-level l) (<= (heading-level l) 2)))))
      (string-trim (string-join desc-lines "\n"))))

  ;; Extrahiere Sektionen
  (define instructions (extract-section lines "System Instructions"))
  (define examples (extract-examples lines))
  (define tool-preferences (extract-tool-preferences lines))

  (agent-context name description instructions examples tool-preferences))

;; Lädt eine AGENTS.md aus einem Verzeichnis
;; (load-agent-context [dir (current-directory)]) → agent-context? oder #f
(define (load-agent-context [dir (current-directory)])
  (define agents-path (build-path dir "AGENTS.md"))
  (define content (try-read-file agents-path))
  (and content (parse-agent-file content)))
