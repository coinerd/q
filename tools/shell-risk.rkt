#lang racket/base

;; Compatibility facade. The pure shell-risk classifier is owned by util/ so
;; runtime code can consume it without depending upward on the tools layer.

(require "../util/shell-risk.rkt")

(provide (all-from-out "../util/shell-risk.rkt"))
