;; Protocol registry for check-protocols.rkt
;; Format: ((function-name . expected-return-type) ...)
;;
;; expected-return-type:
;;   struct  — callers must use struct accessors (e.g., hook-result-action)
;;   list    — callers must use list accessors (car, cadr, etc.)
;;
;; Add new entries as protocols are formalized.

((hook-dispatcher . struct))
