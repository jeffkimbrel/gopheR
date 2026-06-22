# gopheR + GopherScout Roadmap

Consolidated ideas and planned features across the gopheR R package and GopherScout desktop app.
Amplicon-specific items that aren't yet implemented also live here alongside their design docs in `AMPLICON_PLANNING.md`.

---

## gopheR — Data / Schema

### Wildcard object type for universal spec entries

Some result keys (`note`) and file roles (`general`) apply to every object type. Currently each must be inserted per-type in `object_result_spec` and `object_file_type_spec`, and must be repeated whenever a new object type is added.

**Proposed:** add `object_type = '*'` as a reserved row in the `object_type` table. Validation logic checks the specific type AND `*`. FK still holds; no second table needed. GopherScout type dropdowns would need to filter out `*`.

---

### `consolidate_files()`

Move or copy files from scattered locations into a single organized directory, verify checksums, and update the database with new paths.

**Proposed signature:**
```r
consolidate_files(
  dest_dir,
  object_type   = NULL,   # filter by object type (e.g. "genome", "readset")
  file_role     = NULL,   # filter by file role (e.g. "patric_annotation")
  workflow_id   = NULL,   # scope to one workflow's output files
  mode          = c("copy", "move"),
  validate_only = FALSE,
  db_path       = NULL
)
```

**Intended behavior:**
1. Query matching rows from `object_file` and/or `workflow_file`
2. Compute source checksum if missing; warn and skip if source path doesn't exist
3. Copy/move to `dest_dir`, preserving structure as `dest_dir/{object_id}/{file_role}/filename`
4. Verify destination checksum matches source
5. `UPDATE` DB with new path and verified checksum

**Open design questions:**
- Flat vs. structured destination (flat risks name collisions across objects)
- Directory-type paths (e.g. PATRIC annotation folders): copy whole tree, checksum as manifest (sorted md5sum of all files inside)
- Conflict at destination: skip silently if checksum matches; error if checksum differs
- Whether to handle both `object_file` and `workflow_file` in one function or separately

---

### `split_den(study_id)`

Split a multi-study den into per-study dens.

---

### Full provenance graph via igraph/ggraph

Large-scale graph output from R using tools that can handle thousands of nodes.
GopherScout's built-in graph is intentionally capped at ~200 nodes for interactive use.

---

### Unit enforcement

Enforce that a given result key always uses the unit from `object_result_spec`.
Currently advisory — bundle-provided unit can override silently.

---

### Post-ingestion SQL dump

After ingestion, write a diffable SQL text dump to `archive/dens/` so git tracks readable history
instead of binary snapshots.

---

### AI-assisted bundle generation

Agent that reads a folder of bioinformatics tool outputs (coverage TSVs, CheckM tables, assembly stats)
and a plain-English description of what was run, then drafts a gopheR bundle for review.
Handles ID mapping, edge inference, and bulk row generation.

---

## gopheR — Amplicon

### `merge_abundances(primer_set_id, cluster_type, workflow_id = NULL)`

Deferred until core amplicon ingest has been validated on real data. (Core validated 2026-06-17; this function is next.)

**What it does:**
1. Find all `amplicon` objects with `object_subtype == primer_set_id`
2. For each amplicon, locate its abundance matrix TSV via `object_file(file_role = "abundance_matrix")`
3. Read each TSV; remap local ASV labels → `asv_id` via `amplicon_asv`
4. Join `asv_id` → `cluster_id` via `asv_cluster` for the given `cluster_type` (most recent workflow if `workflow_id = NULL`)
5. Sum abundance across ASVs that collapsed into the same cluster
6. Outer-join all study matrices on sample columns
7. Return merged tibble: rows = cluster_ids, columns = all sample readset IDs

**Dependencies that must work first:**
- `read_amplicon()` must have populated `amplicon_asv` with correct label → asv_id mappings
- `read_clustering()` must have populated `asv_cluster` with correct cluster assignments
- `workflow_file` must have valid paths to abundance TSVs that still exist on disk
- Sample column names in the TSVs must match readset object IDs (or `sample_map` was applied)

---

## GopherScout — UI / Features

### Amplicon browsing

`amplicon` objects appear in the Objects tab and graph as first-class objects. What's missing is visibility into the amplicon-specific tables (`asv`, `amplicon_asv`, `asv_taxonomy`):

- **ASV count** on `amplicon` detail panel — query `amplicon_asv` for count of ASVs in this amplicon
- **Taxonomy summary** — top taxa from `asv_taxonomy` for a given `amplicon`, grouped by rank
- **Cross-amplicon ASV overlap** — shared global `asv_id`s between two `amplicon` objects

Scope and design TBD; deferred until the R amplicon layer is stable across multiple datasets.

---

### Context menu / "send to" pattern

- Right-click a chart bar in Overview → "Send to Export" — resolves bar's category value into a filter token and navigates to Export with it pre-applied
- Extend to graph nodes (right-click → send all children to export) and detail panel relationship lists

---

### Canned views / `views` table

- Add a `views` table to the gopheR schema — rows define named visualizations (scatter plot, grouped bar, summary card) with fields, axes, and filters; GopherScout reads and renders them dynamically
- Scatter plots as the first view type — completeness vs. contamination is the obvious first use case
- Taxonomy string parsing in `color_by` should be generic — GTDB, RDP, SILVA, UNITE all have different conventions; an `extract:` field in the view definition keeps this open

---

### Remote database access

"Open from URL" — fetches a SQLite from a GitHub raw URL, caches for the session, enforces read-only mode.
Enables sharing a master database publicly without giving write access. No gopheR changes needed.

---

### Write mode (local databases only)

- Add/edit objects, workflows, results with validated forms; autocomplete against existing IDs and spec tables
- Auto-backup before writes; restore on transaction failure
- Would need gopheR's validation logic accessible as a shared library or duplicated in Rust

---

### Chat / LLM

- Chat tab — natural language interface; LLM receives schema (object types, edge types, result keys) as context and formulates SQL; generated SQL shown and editable before execution; model-agnostic (user supplies key)
- Query history and saved/favorite queries
- API key storage in `~/Library/Application Support/GopherScout/config.yaml`

---

### Settings tab

API keys, model selection, theme, default tab, table page size, backup location.

---

### GitHub Actions build pipeline

Automated cross-platform builds (macOS DMG, Windows MSI/EXE, Linux AppImage) triggered on version tags.
See `.github/workflows/` once wired up.

---

### Long-shot / future

- Mobile read-only app — Tauri supports iOS/Android
