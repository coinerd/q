# skills/ Modul-Architektur

## Übersicht

Die Skills-Module wurden refactored, um eine Aufwärtsabhängigkeit zu runtime/ zu vermeiden.

## Module

### types.rkt

**Zweck**: Enthält alle gemeinsamen Typen und Funktionen für Skills.

**Exportiert**:
- `resource` (struct) - Einzelne Ressource
- `resource-set` (struct) - Sammlung von Ressourcen
- `empty-resource-set` - Konstruktor für leere Ressource-Sets
- `load-global-resources` - Lädt globale Ressourcen aus ~/.q/
- `load-project-resources` - Lädt Projekt-Ressourcen aus .q/ oder .pi/
- `merge-resources` - Kombiniert globale und Projekt-Ressourcen
- `render-template` - Ersetzt {{var}}-Platzhalter in Templates

**Abhängigkeiten**: Keine Abhängigkeit zu runtime/*

### prompt-template.rkt

**Zweck**: Re-Export von `render-template` für Abwärtskompatibilität.

**Exportiert**:
- `render-template`

**Abhängigkeiten**: Importiert nur von `types.rkt`

### skill-loader.rkt

**Zweck**: Re-Export aller Skills-Funktionen für Abwärtskompatibilität.

**Exportiert**: Alle Bindungen aus `types.rkt`

**Abhängigkeiten**: Importiert nur von `types.rkt`

## Architektur-Verbesserung

### Vorher (Anti-Pattern)
```
skills/prompt-template.rkt ──importiert──▶ runtime/resource-loader.rkt
skills/skill-loader.rkt ────importiert──▶ runtime/resource-loader.rkt
```

### Nachher (Saubere Architektur)
```
runtime/resource-loader.rkt ──importiert──▶ skills/types.rkt
skills/prompt-template.rkt ───importiert──▶ skills/types.rkt
skills/skill-loader.rkt ─────importiert──▶ skills/types.rkt
```

Die gemeinsamen Typen und Funktionen sind nun in `skills/types.rkt` zentralisiert, und `runtime/resource-loader.rkt` importiert diese (statt umgekehrt).
