# Migration Template

Use this template when creating migration guides for MAJOR version bumps.
Copy to `docs/migration/vFROM-to-vTO.md` and fill in the sections.

---

# Migrating from v{FROM} to v{TO}

This guide covers breaking changes between q v{FROM} and v{TO}.

## Summary

- **{count} breaking changes** in {count} modules
- Estimated migration effort: **{low|medium|high}**

## Breaking Changes

### 1. {Change Title}

**Module**: `{module.rkt}`
**Tier**: stable → stable
**Impact**: {description of what breaks}

**Before (v{FROM})**:
```racket
(old-api-call arg1 arg2)
```

**After (v{TO})**:
```racket
(new-api-call arg1 arg2 #:key value)
```

**Migration steps**:
1. {step 1}
2. {step 2}

---

## Deprecations

These features are deprecated in v{TO} and will be removed in v{NEXT}:

| Feature | Replacement | Since |
|---------|-------------|-------|
| `{old}` | `{new}` | v{TO} |

## New Features

Notable additions in v{TO}:

- {feature 1}
- {feature 2}

## Checklist

- [ ] Update all imports of changed modules
- [ ] Update struct constructors with changed fields
- [ ] Update contract assertions
- [ ] Run `racket scripts/lint-version.rkt`
- [ ] Run `raco test tests/`
