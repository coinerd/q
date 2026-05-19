#lang racket/base

;; q/tui/render.rkt — TUI rendering (façade)
;;
;; Re-exports from focused sub-modules:
;;   - render/message-layout.rkt — styled lines, entry formatting, markdown
;;   - render/status-line.rkt    — status bar and input line rendering
;;   - render/diff-render.rkt    — selection highlighting, transcript rendering

(require "render/message-layout.rkt"
         "render/status-line.rkt"
         "render/diff-render.rkt")

(provide styled-line
         styled-line?
         styled-line-segments
         styled-segment
         styled-segment?
         styled-segment-text
         styled-segment-style

         ;; From message-layout
         plain-line
         theme->style
         format-entry
         md-format-assistant
         md-token->segment
         styled-line->text
         styled-line->ansi
         styles->sgr
         wrap-styled-line
         wrap-text
         wrap-single-line

         ;; From status-line
         render-status-bar
         render-input-line

         ;; From diff-render
         apply-selection-highlight
         highlight-line-range
         style-invert
         render-transcript
         render-branch-list
         render-leaf-nodes
         render-children-list)
