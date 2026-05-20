#lang racket/base

;; llm/model-defaults.rkt — centralised model name / base URL constants
;;
;; Q-12: Extracted from individual LLM provider modules so that
;;       version bumps and model name changes are made in one place.

(require racket/contract)

;; Anthropic
(provide (contract-out [ANTHROPIC-DEFAULT-MODEL string?]
                       [ANTHROPIC-DEFAULT-BASE-URL string?]
                       ;; Gemini
                       [GEMINI-DEFAULT-MODEL string?]
                       [GEMINI-DEFAULT-BASE-URL string?]
                       ;; OpenAI / Azure OpenAI
                       [OPENAI-DEFAULT-MODEL string?]))

;; ── Anthropic ──────────────────────────────────────────────
(define ANTHROPIC-DEFAULT-MODEL "claude-sonnet-4-20250514")
(define ANTHROPIC-DEFAULT-BASE-URL "https://api.anthropic.com")

;; ── Google Gemini ──────────────────────────────────────────
(define GEMINI-DEFAULT-MODEL "gemini-2.5-pro")
(define GEMINI-DEFAULT-BASE-URL "https://generativelanguage.googleapis.com")

;; ── OpenAI / Azure OpenAI ─────────────────────────────────
(define OPENAI-DEFAULT-MODEL "gpt-4")
