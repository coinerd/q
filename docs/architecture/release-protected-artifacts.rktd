;; release-protected-artifacts.rktd
;; Authoritative registry of architecture-policy artifacts that MUST remain
;; byte-stable during release automation unless explicitly included in the
;; release change set.
;;
;; Stability modes:
;;   BYTE_STABLE       — file bytes must be identical before/after automation
;;   EXPLICITLY_MUTABLE — may change during release (e.g. generated metadata)
;;
;; Do NOT add files here merely for convenience. Each entry must be a genuine
;; governance artifact whose accidental mutation would silently change
;; architectural policy.

((version . 1)
 (artifacts
  ;; ── Architecture policy (authoritative input, not generated output) ──
  ((path . "docs/architecture/dependency-policy.rktd")
   (stability . BYTE_STABLE)
   (classification . IMMUTABLE_DURING_RELEASE)
   (description . "Approved dependency boundaries and exceptions"))

  ((path . "docs/architecture/parameter-inventory.rktd")
   (stability . BYTE_STABLE)
   (classification . IMMUTABLE_DURING_RELEASE)
   (description . "Parameter lifetime and cross-turn-state classifications"))

  ((path . "docs/architecture/provider-change-locality-policy.rktd")
   (stability . BYTE_STABLE)
   (classification . IMMUTABLE_DURING_RELEASE)
   (description . "Provider ownership and change-locality rules"))

  ;; ── Terminal architecture decisions (historical immutable evidence) ──
  ((path . "docs/architecture/maintainability-terminal-v0.99.92.rktd")
   (stability . BYTE_STABLE)
   (classification . IMMUTABLE_DURING_RELEASE)
   (description . "Terminal maintainability decision snapshot"))

  ((path . "docs/architecture/maintainability-roadmap-freeze-v0.99.87.rktd")
   (stability . BYTE_STABLE)
   (classification . IMMUTABLE_DURING_RELEASE)
   (description . "Frozen maintainability roadmap"))

  ((path . "docs/architecture/orchestration-surface-reduction-v0.99.92.rktd")
   (stability . BYTE_STABLE)
   (classification . IMMUTABLE_DURING_RELEASE)
   (description . "Orchestration surface reduction decision"))

  ((path . "docs/architecture/provider-hardening-terminal-v0.99.91.rktd")
   (stability . BYTE_STABLE)
   (classification . IMMUTABLE_DURING_RELEASE)
   (description . "Provider hardening terminal decision"))

  ((path . "docs/architecture/session-lifecycle-trace-v0.99.92.rktd")
   (stability . BYTE_STABLE)
   (classification . IMMUTABLE_DURING_RELEASE)
   (description . "Session lifecycle trace snapshot"))

  ;; ── This registry itself is protected ──
  ((path . "docs/architecture/release-protected-artifacts.rktd")
   (stability . BYTE_STABLE)
   (classification . IMMUTABLE_DURING_RELEASE)
   (description . "This registry — modifying the protected list is a governance act"))))
