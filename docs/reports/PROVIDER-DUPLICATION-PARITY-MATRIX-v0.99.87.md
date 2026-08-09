# Provider-Duplikations- und Paritätsmatrix — v0.99.87 W3

**Wave:** v0.99.87 W3 (issue #9215, milestone 874)
**Date:** 2026-08-09
**Baseline commit:** `3bf783c2`
**Scope:** `llm/anthropic.rkt` (facade) + `llm/anthropic/format.rkt` +
`llm/anthropic/sse.rkt` + `llm/anthropic-helpers.rkt`, `llm/gemini.rkt`,
`llm/openai-compatible.rkt`, `llm/azure-openai.rkt`, `llm/openrouter.rkt`,
`llm/stream.rkt`, `llm/http-helpers.rkt`, `llm/provider-errors.rkt`,
`llm/adapters/eager-stream.rkt`.

**Ziel (roadmap §W3):** Exakt bestimmen, welche HTTP/SSE/Error-/Tool-Transport-Logik
wirklich gemeinsam ist. Kandidaten klassifizieren als `SHARED_PRIMITIVE`,
`PROVIDER_PROTOCOL`, `ACCIDENTAL_DUPLICATION`, `INTENTIONAL_DUPLICATION`.
Paritätstests definieren — **keine Abstraktion bauen**.

## 1. Methodik

Jeder Kandidat wurde anhand von Quelltext-Lektüre aller vier Provider-Pfade
(Anthropic, Gemini, OpenAI-kompatibel, Azure-OpenAI; OpenRouter ist ein
115-Zeilen-Wrapper, der an `openai-compatible.rkt` delegiert) sowie der
gemeinsamen Module bewertet. Klassifikationsregeln:

- **SHARED_PRIMITIVE** — bereits geteilt *oder* strukturell und semantisch
  identisch inkl. gleichem Fehler-/Timeout-Vertrag.
- **PROVIDER_PROTOCOL** — Differenz ist Protokoll-Identität (Wire-Format,
  Event-Modell, Auth-Schema); Teilen würde inkompatible Protokolle
  homogenisieren.
- **ACCIDENTAL_DUPLICATION** — gleiche Struktur ohne semantischen Grund;
  Kandidat für Extraktion (frühestens v0.99.91, nach Paritätstests).
- **INTENTIONAL_DUPLICATION** — bewusste Abweichung (Endpoint-Quirk,
  Defensiv-Normalisierung); explizit nicht teilen.

## 2. Dimensionsmatrix (Ist-Zustand)

| Dimension | Anthropic | Gemini | OpenAI-kompatibel | Azure-OpenAI |
|---|---|---|---|---|
| Non-Streaming HTTP | `anthropic-do-http-request` → `make-provider-http-request` | `gemini-do-http-request` → shared | `do-http-request` → shared (+ per-model timeout) | `azure-do-http-request` → shared |
| Auth-Header | `x-api-key` + `anthropic-version` | `x-goog-api-key` | `Authorization: Bearer` | `api-key` |
| Timeout-Phasen | `call-with-request-timeout` (shared); kein per-model Timeout | shared; kein per-model | shared + `effective-request-timeout-for` (per-model) | shared; kein per-model |
| SSE-Framing | `stream-sse-events` (shared) | shared | shared | shared |
| Port-Ownership | `close-port-after-stream` + custodian + `stream-owns-port?` | gleiches Muster | gleiches Muster | gleiches Muster |
| Status-Pre-Check (Stream) | inline regexp, Schwelle ≥400 | inline regexp, ≥400 | inline regexp (andere Form), ≥300 | `check-azure-status!`, ≠200 |
| Chunk-Normalisierung | `anthropic-parse-single-event` (Boxen für Tool-State) | `gemini-parse-single-event` (Tool-ID-Counter-Parameter) | `normalize-openai-chunk` (Null-Koerzion) | `normalize-openai-chunk` ( reused) |
| Tool-Calls (Request) | `anthropic-translate-tool` → `input_schema` | `gemini-translate-tool` → `parameters` | `openai-normalize-tool` (Array-Items-Patch) | openai-Pfad reused |
| Tool-Calls (Response) | kanonisches `tool-call`-Hash + `validate-tool-call-intent!` | kanonisch + `validate-tool-call-intent!` | kanonisch + `validate-tool-call-intent!` | openai-Pfad reused |
| Usage-Mapping | `translate-anthropic-usage` (Helper) | **inline, 2× dupliziert** | Pass-Through (wire-kanonisch) | openai-Pfad reused |
| Stop-Reason | `translate-stop-reason 'anthropic` | `translate-stop-reason 'gemini` | `translate-stop-reason #f` (underscore→hyphen) | openai-Pfad reused |
| Error-Mapping (Setup) | `check-provider-status!` | `check-provider-status!` | `check-provider-status!` | **`check-azure-status!` (bespoke)** |
| Error-Mapping (Stream-Phase) | kein Wrapping | kein Wrapping | `openai-wrap-stream-error` → `provider-error 'network` | kein Wrapping |

## 3. Kandidaten-Inventar und Entscheidungen

### Bereits geteilte Primitive (Ist-Zustand, keine Aktion)

| ID | Kandidat | Sites | Entscheidung |
|---|---|---|---|
| C1 | Non-Streaming HTTP POST (`make-provider-http-request`) | alle 4 delegieren | **SHARED_PRIMITIVE** (bereits geteilt) |
| C2 | SSE-Framing (`stream-sse-events`, `parse-sse-line(s)`, `sse-done?`) | alle 4 | **SHARED_PRIMITIVE** (bereits geteilt) |
| C3 | Port-Finalisierung (`close-port-after-stream`) | alle 4 | **SHARED_PRIMITIVE** (bereits geteilt) |
| C4 | Fehlertyp + Statusklassifikation (`provider-error`, `classify-http-status`, `raise-provider-error`) | alle 4 | **SHARED_PRIMITIVE** (bereits geteilt) |
| C5 | Generischer Status-Checker (`check-provider-status!`) | anthropic, gemini, openai | **SHARED_PRIMITIVE** (bereits geteilt; Azure ausgenommen, siehe C11) |
| C6 | Stop-Reason-Übersetzung (`translate-stop-reason`) | anthropic, gemini, openai | **SHARED_PRIMITIVE** (bereits geteilt, tabellengetrieben) |
| C7 | Tool-Call-Delta-Akkumulation (`accumulate-tool-call-deltas`) | shared | **SHARED_PRIMITIVE** (bereits geteilt) |
| C8 | Timeout-Primitive (`call-with-request-timeout`, `read-line/timeout`, `read-response-body/timeout`, `current-http-request-timeout`) | alle 4 | **SHARED_PRIMITIVE** (bereits geteilt) |

### Akzidentelle Duplikation (Kandidaten für v0.99.91; Paritätstests zuerst)

| ID | Kandidat | Sites | Entscheidung | Begründung |
|---|---|---|---|---|
| C9 | Streaming-Setup-Skelett (custodian + `dynamic-wind` + `http-sendrecv` + Status-Pre-Check + `stream-sse-events` + `close-port-after-stream` + Ownership-Transfer) | `anthropic/sse.rkt:103-176`, `gemini.rkt:466-537`, `openai-compatible.rkt:365-428`, `azure-openai.rkt:109-162` | **ACCIDENTAL_DUPLICATION** → SHARED_PRIMITIVE-Kandidat | strukturell identisch, gleicher Timeout-/Cleanup-Vertrag; Variationen (Headers, Pfad, Status-Checker, Schwelle) parametrisierbar. **Vorbedingung:** G1 (Schwellen-Divergenz) auflösen |
| C10 | Inline-Status-Regexp in Stream-Setup (4 Vorkommen, 2 String-Regexp-Formen + 1 Byte-Variante bei Azure: `^HTTP/[^ ]+ ([0-9]+)` vs `HTTP/[0-9.]+ ([0-9]+)` vs Azure-Byte-Regexp) | `anthropic/sse.rkt:143-148`, `gemini.rkt:510-515`, `openai-compatible.rkt:419-424`, `azure-openai.rkt:57-62` | **ACCIDENTAL_DUPLICATION** | `extract-status-code` existiert in `http-helpers.rkt` und wird an keiner dieser Stellen genutzt |
| C11 | `azure-stream` inline URL-Parsing | `azure-openai.rkt:115-121` | **ACCIDENTAL_DUPLICATION** | `parse-provider-url` existiert genau für diesen Zweck |
| C12 | `check-azure-status!` (bespoke: raise bei ≠200, Body-Truncation, inline Regexp) | `azure-openai.rkt:56-68` | **ACCIDENTAL_DUPLICATION** mit semantischem Delta | könnte `check-provider-status!` nutzen; Unterschied ist nur Message-Format und Schwelle (≠200 statt ≥400) |

**Bug-Fund während W3:** `check-azure-status!` nutzte
`(bytes->string/utf-8 body #:error-replacement "?")` — ein Keyword, das
Racket 8.10 nicht unterstützt. Der Non-200-Pfad warf daher immer einen
generischen Keyword-`exn:fail` statt des beabsichtigten strukturierten
`provider-error`; der Bestandstest prüfte nur `exn:fail?` und maskierte den
Defekt. **Fix in dieser Wave:** positionales `err-char`-Argument `#\?`
(1-Zeiler, keine Abstraktion). Paritätstest P6 verifiziert nun die
Kategorie-Parität für 400/401/429/500.
| C13 | Base-URL-Trim + Pfad-Join (`(string-append (string-trim base-url "/") path)`) | 8 Sites (2 pro Provider: Non-Streaming + Streaming: `anthropic/sse.rkt:49,106`, `gemini.rkt:438,472`, `openai-compatible.rkt:274,351`, `azure-openai.rkt:45,113`) | **ACCIDENTAL_DUPLICATION** (trivial) | 1-Zeiler; geringer Share-Wert, niedrige Priorität |
| C14 | `anthropic-translate-tool` vs `gemini-translate-tool` (identische Feld-Extraktion name/description/parameters; nur Output-Key differiert: `input_schema` vs `parameters`) | `anthropic-helpers.rkt:42-48`, `gemini.rkt:237-242` | **ACCIDENTAL_DUPLICATION** | Extraktionshälfte identisch; Kandidat für winzigen Shared-Extractor. Hinweis: Default für fehlendes `parameters` differiert minimal (`hasheq` bei Anthropic vs `hash` bei Gemini) |
| C15 | Gemini-Usage-Mapping inline 2× (`promptTokenCount/candidatesTokenCount/totalTokenCount` → kanonische Keys) | `gemini.rkt:282-290` (non-streaming), `gemini.rkt:411-418` (streaming) | **ACCIDENTAL_DUPLICATION** (gemini-intern) | `translate-gemini-usage`-Helper analog zu `translate-anthropic-usage` extrahieren |

### Provider-Protokoll (nicht teilen)

| ID | Kandidat | Entscheidung | Begründung |
|---|---|---|---|
| C16 | Request-Body-Builder (`anthropic-build-request-body`, `gemini-build-request-body`, `openai-build-request-body`) | **PROVIDER_PROTOCOL** | Wire-Formate divergieren semantisch (Messages-, Contents-, Tools-Shape) |
| C17 | Per-Event-SSE-Parser (`anthropic-parse-single-event`, `gemini-parse-single-event`, `normalize-openai-chunk`) | **PROVIDER_PROTOCOL** | verschiedene Event-Modelle (content_block_* vs candidates/parts vs choices/delta) |
| C18 | Auth-Header-Namen (4 Varianten) | **PROVIDER_PROTOCOL** | Protokoll-Identität |
| C19 | `openai-normalize-tool` Array-Items-Patch | **PROVIDER_PROTOCOL** | OpenAI-Schema-Quirk (fehlende `items` bei Arrays) |
| C20 | Gemini-Tool-ID-Counter (`current-gemini-tool-id-counter`, `gemini-gen-tool-id`) | **PROVIDER_PROTOCOL** | Gemini-SSE liefert keine stabilen Tool-IDs; provider-spezifischer Workaround |
| C21 | Content-Block-Konvertierung (`openai-block->anthropic`, `openai-block->gemini`) | **PROVIDER_PROTOCOL** | nur Dispatcher-Skelett ähnlich; Output-Shapes differieren (source/base64 vs inline_data) |

### Intentionale Duplikation (explizit behalten)

| ID | Kandidat | Entscheidung | Begründung |
|---|---|---|---|
| C22 | Kimi-Eager-Stream-Pfad (`kimi-eager-stream-chunks` + `llm/adapters/eager-stream.rkt`) | **INTENTIONAL_DUPLICATION** | Endpoint-Quirk (Port-Schließung vor Generator-Konsum); bereits isoliert |
| C23 | OpenAI Null-Koerzion (`'null → #f` für usage/reasoning_content) | **INTENTIONAL_DUPLICATION** | DeepSeek/GLM-Quirks; defensive Normalisierung gegen strikten JSON-Parser |

## 4. Paritäts-Lücken (documented asymmetries — keine Duplikation)

| ID | Asymmetrie | Sites | Konsequenz |
|---|---|---|---|
| G1 | Status-Pre-Check-Schwelle im Stream-Setup: openai ≥300, anthropic/gemini ≥400, azure ≠200 | 4 Sites | **Blocker für C9-Sharing** — semantische Divergenz muss vor Extraktion entschieden werden (Redirect-Behandlung!) |
| G2 | Per-Model-Timeout (`effective-request-timeout-for`) nur im OpenAI-Non-Streaming-Pfad | `openai-compatible.rkt:276-282` | Anthropic/Gemini/Azure nutzen globalen Default; Angleichung oder begründete Asymmetrie in v0.99.91 |
| G3 | Stream-Phase-Error-Wrapping (`openai-wrap-stream-error`) nur bei OpenAI | `openai-compatible.rkt:302-313` | Anthropic/Gemini/Azure lassen Stream-Fehler unstrukturiert durch; Retry-Klassifikation asymmetrisch |

## 5. Explizite Rejection-Liste

Folgende Kandidaten werden **nicht** geteilt (auch nicht in v0.99.91):

1. **C16 Request-Body-Builder** — Wire-Formate sind Protokoll-Identität; ein
   Shared-Builder würde eine falsch homogenisierte Basisabstraktion erzeugen
   (genau das im Roadmap-Risiko benannte Szenario).
2. **C17 Per-Event-SSE-Parser** — verschiedene Event-Modelle; nur das Ziel
   (`stream-chunk`) ist gemeinsam, nicht die Ableitung.
3. **C18 Auth-Header-Namen** — Protokoll-Identität.
4. **C19 OpenAI-Tool-Schema-Patch** — Endpoint-Quirk.
5. **C20 Gemini-Tool-ID-Counter** — Protokoll-Lücke, kein gemeinsames
   Primitive.
6. **C21 Content-Block-Konvertierung** — Output-Shapes inkompatibel.
7. **C22 Kimi-Eager-Stream** — bewusste Isolation eines Endpoint-Quirks.
8. **C23 OpenAI-Null-Koerzion** — Defensiv-Normalisierung; Verallgemeinerung
   würde Fehler anderer Provider verschleiern.

## 6. Paritätstest-Definitionen

Implementiert in `tests/test-provider-parity-matrix.rkt` (Fast-Suite). Die
Tests prüfen **Bestandsverhalten** — sie bauen keine Abstraktion und ändern
keinen Produktivcode. Charakterisierungs-Tests pinnen den aktuellen
Duplikations-Stand, damit v0.99.91 jede Änderung bewusst nachziehen muss.

| Test | Art | Assertion |
|---|---|---|
| P1 | strukturell | alle 4 Provider-Module delegieren Non-Streaming-HTTP an `make-provider-http-request` |
| P2 | strukturell | alle 4 Stream-Pfade nutzen `stream-sse-events` |
| P3 | strukturell (Charakterisierung) | exakt die dokumentierten C10-Sites enthalten inline Status-Regexp (Anzahl + Orte gepinnt) |
| P4 | behavioral | `anthropic-parse-response` / `gemini-parse-response` / `openai-parse-response` liefern kanonische Usage-Keys (`prompt_tokens`, `completion_tokens`, `total_tokens`, Integer ≥0) aus Fixture-Responses |
| P5 | behavioral | `translate-stop-reason` bildet Provider-Gründe auf kanonische Symbole ab (anthropic `max_tokens`→`length`, gemini `SAFETY`→`content-filtered`, openai `tool_calls`→`tool-calls`) |
| P6 | behavioral | `check-provider-status!` und `check-azure-status!` raisen `provider-error` mit paritätischer Kategorie für 400/401/429/500 (`bad-request`/`auth`/`rate-limit`/`server`) |
| P7 | behavioral | `anthropic-parse-single-event`, `gemini-parse-single-event`, `normalize-openai-chunk` erzeugen valide `stream-chunk`s (usage hash-or-#f, done? boolean) aus Fixture-Events; OpenAI `'null`-Usage wird zu `#f` koerziert |
| P8 | strukturell (Charakterisierung) | Paritäts-Lücken gepinnt: per-model Timeout nur in `openai-compatible.rkt`; `openai-wrap-stream-error` nur bei OpenAI; `check-azure-status!` bespoke — Änderungen erzwingen Test-Update |

## 7. Implikation für v0.99.91 (Entscheidungs-Gate Path A/B)

- **Path A (Shared Primitives)** ist nur für **C9** (Streaming-Setup-Skelett)
  und die Klein-Kandidaten **C10–C15** denkbar. C9 erfordert vorher die
  Entscheidung von **G1** (Schwellen-Divergenz) — ohne diese ist die
  Roadmap-Bedingung „gleicher Fehler-/Timeout-Vertrag" nicht erfüllt.
- Co-Change-Evidenz (W0-Baseline): `openai-compatible.rkt ↔ stream.rkt`
  (Count 5) ist das einzige Provider-Paar im Top-5 — stützt C9 als
  Co-Change-getriebenen Kandidaten; C10–C15 haben keine nennenswerte
  Co-Change-Historie (eher Opportunitäts-Extraktionen).
- Falls G1 als semantisch notwendig bewertet wird, bleibt nur **Path B**
  (Provider Contract & Test Hardening) für das Skelett, und v0.99.91
  beschränkt sich auf C10–C15 + G2/G3-Angleichung.

## 8. Acceptance-Status

- [x] Entscheidung je Kandidat (C1–C23 klassifiziert)
- [x] Explizite Rejection-Liste (§5, 8 Einträge)
- [x] Paritätstests definiert und implementiert (P1–P8), keine Abstraktion gebaut
- [x] Fast Gate grün
