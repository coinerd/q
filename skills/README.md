# skills/ Module Architecture

## Overview

The skills modules were refactored to avoid an upward dependency on runtime/.

## Module

### types.rkt

**Purpose**: Contains all shared types and functions for skills.

**Exports**:
- `resource` (struct) — single resource
- `resource-set` (struct) — collection of resources
- `empty-resource-set` — constructor for empty resource sets
- `load-global-resources` — loads global resources from ~/.q/
- `load-project-resources` — loads project resources from .q/ or .pi/
- `merge-resources` — combines global and project resources
- `render-template` — substitutes {{var}} placeholders in templates

**Dependencies**: No dependency on runtime/*

### prompt-template.rkt

**Purpose**: Re-exports `render-template` for backward compatibility.

**Exports**:
- `render-template`

**Dependencies**: Imports only from `types.rkt`

### skill-loader.rkt

**Purpose**: Re-exports all skills functions for backward compatibility.

**Exports**: All bindings from `types.rkt`

**Dependencies**: Imports only from `types.rkt`

## Architecture Improvement

### Before (Anti-Pattern)
```
skills/prompt-template.rkt ──imports──▶ runtime/resource-loader.rkt
skills/skill-loader.rkt ────imports──▶ runtime/resource-loader.rkt
```

### After (Clean Architecture)
```
runtime/resource-loader.rkt ──imports──▶ skills/types.rkt
skills/prompt-template.rkt ───imports──▶ skills/types.rkt
skills/skill-loader.rkt ─────imports──▶ skills/types.rkt
```

The shared types and functions are now centralized in `skills/types.rkt`, and `runtime/resource-loader.rkt` imports from it (instead of the other way around).
