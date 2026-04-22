#lang racket/base

;; runtime/model-defaults.rkt — centralised model name / base URL constants
;;
;; Q-12: Extracted from individual LLM provider modules so that
;;       version bumps and model name changes are made in one place.

;; Anthropic
(provide ANTHROPIC-DEFAULT-MODEL
         ANTHROPIC-DEFAULT-BASE-URL
         ;; Gemini
         GEMINI-DEFAULT-MODEL
         GEMINI-DEFAULT-BASE-URL
         ;; OpenAI / Azure OpenAI
         OPENAI-DEFAULT-MODEL)

;; ── Anthropic ──────────────────────────────────────────────
(define ANTHROPIC-DEFAULT-MODEL "claude-sonnet-4-20250514")
(define ANTHROPIC-DEFAULT-BASE-URL "https://api.anthropic.com")

;; ── Google Gemini ──────────────────────────────────────────
(define GEMINI-DEFAULT-MODEL "gemini-2.5-pro")
(define GEMINI-DEFAULT-BASE-URL "https://generativelanguage.googleapis.com")

;; ── OpenAI / Azure OpenAI ─────────────────────────────────
(define OPENAI-DEFAULT-MODEL "gpt-4")
