
# gopheR — Design Decisions & Reference

← Back to [README.md](README.md) for installation and quick start.

---

## Architecture

gopheR is the middle layer of a three-part ecosystem:

```
gopheR (R package) ──► .den (SQLite database in a git repo)
                                │
                        GopherScout (desktop GUI, optional)
```

### 1. gopheR (this package)

An installable R package providing the core infrastructure:
- Enforces schema structure (table names, columns, relationships)
- Provides core functions (`read_bundle()`, `write_bundle()`, validation, connection helpers)
- Works with any database following gopheR conventions

**What's rigid:** Table and column names.
**What's flexible:** The values stored in those tables — object types, edge types, result keys, file roles — defined per-project in spec tables.

### 2. The Den

A den is a plain folder (git repo) that holds a gopheR project. It is created by `initialize_den()` and recognized by a `den.yaml` file at its root.

```
my_project/
├── my_project.den         ← SQLite database (.den extension)
├── den.yaml               ← project config (name, settings)
├── my_project.Rproj       ← RStudio project file
├── .gitignore
└── archive/
    ├── dens/              ← timestamped database backups (pre-ingestion)
    └── bundles/           ← archived Excel bundles (post-ingestion)
```

### 3. GopherScout

[GopherScout](https://github.com/jeffkimbrel/gopherscout) is a Tauri desktop app for browsing gopheR databases — no R required. It provides object/edge/workflow browsing, lineage graphs, integrity checks, overview charts, and TSV/JSON export. It is read-only with respect to the database; it reads and writes `den.yaml` for display settings.

---

## Design Decisions

### Edge direction convention

Edges are directed. The convention: **"child IS edge_type OF parent."**

Examples:
- `(child=readset, edge_type=assembled_from, parent=sample)` → "readset is assembled_from sample"
- `(child=assembly, edge_type=assembled_from, parent=readset)` → "assembly is assembled_from readset"
- `(child=MAG, edge_type=binned_from, parent=assembly)` → "MAG is binned_from assembly"
- `(child=MAG, edge_type=member_of, parent=set)` → "MAG is member_of set"

Provenance traversal follows `parent_id` from leaf (e.g. MAG) toward root (e.g. study). The `edge_spec` table records the expected `parent_type` and `child_type` to enforce this at validation time.

### Set objects: organizational grouping only

Objects with `object_type = 'set'` are containers that group related objects (e.g. all MAGs from a site) for display purposes. Sets are **not** provenance nodes:

- They are excluded from "every object has a path to a study" integrity checks.
- Members inherit provenance from their own edges, not from the set.
- The `member_of` edge type connects children to their set.

Sets are primarily a GopherScout display feature and are not a substitute for proper edge provenance.

### Result keys: no units in the name

Units belong in `object_result_spec.unit`, not in the key name.

- **Do:** `key = 'temperature'`, `unit = '°C'`
- **Don't:** `key = 'temperature_c'`

This keeps key names stable if measurement conventions change, keeps display concerns separate from data concerns, and enables future unit enforcement at the spec level.

### Unit auto-population at ingestion

When ingesting results, gopheR looks up `object_result_spec.unit` for each `(object_type, key)` pair and stamps it onto the result row. If the bundle row already has a non-empty unit, it is preserved (bundle value wins). This means:

1. Rows are self-contained — callers (including GopherScout) don't need to join `object_result_spec` to display units.
2. Units stay consistent across rows for the same key.

The same pattern applies to `edge_result` via `edge_result_spec` (joined on `key` only, not `object_type`).

### Spec strictness: controlled vs. extensible

Not all spec values are equally controlled:

| Spec table | Controlled? | Unknown value behavior |
|---|---|---|
| `object_spec` (object type + subtype) | **Strict** | Hard error at validation |
| `edge_spec` (edge type) | **Strict** | Hard error at validation |
| `object_result_spec` (result key) | **Extensible** | Interactive prompt (hard error in scripts) |
| `object_file_type_spec` (file role) | **Extensible** | Interactive prompt (hard error in scripts) |

Object and edge types define the semantic shape of your data — they should be planned. Result keys and file roles are measurement vocabulary that grows organically. In an interactive R session, gopheR prompts you to confirm and add the new value to the spec. In non-interactive contexts (scripts, CI), it hard-errors so failures aren't silently swallowed.

**Interactive prompt for a new result key collects:** confirmation (y/n), applicable `object_type` values (multi-select), `value_type`, `unit`, and `description`.
**Interactive prompt for a new file role collects:** confirmation (y/n), applicable `object_type` values (multi-select), and `description`.

### Excel dropdowns: advisory for extensible fields

`write_bundle()` generates Excel files with data-validation dropdowns. For strict fields (object type, subtype, edge type), the dropdown is enforced — Excel will block values not on the list. For extensible fields (result key, file role), the dropdown is **advisory** — it shows current spec values but does not block new values. The ingestion prompt handles the new-value case.

### den.yaml

`den.yaml` is a YAML config file that lives next to the `.den` database. It is the writable companion to the read-only (by GopherScout) database. It stores:

- Project display name
- UI preferences and settings (e.g. display theme)
- Warnings and notes surfaced by GopherScout

gopheR creates it at `initialize_den()` time. GopherScout reads it at startup and writes to it when settings change. Because it's in the git repo alongside the database, settings travel with the den when shared.

---

## Schema Reference

### Core tables

| Table | Primary key | Purpose |
|---|---|---|
| `object` | `object_id` | Every tracked dataset or entity |
| `edge` | `edge_id` | Directed relationships between objects |
| `workflow` | `workflow_id` | Processes that produced objects or results |
| `object_result` | `result_id` | Key/value measurements attached to objects |
| `edge_result` | `result_id` | Key/value measurements attached to edges |
| `object_file` | `file_id` | File pointers attached to objects |
| `workflow_file` | `file_id` | File pointers attached to workflows |
| `person` | `person_id` | People (for attribution) |

### Spec tables

| Table | Defines |
|---|---|
| `object_spec` | Valid `(object_type, object_subtype)` pairs |
| `edge_spec` | Valid `edge_type` values, plus expected `parent_type` / `child_type` |
| `object_result_spec` | Valid `(object_type, key)` pairs; `value_type`, `unit`, `description` |
| `edge_result_spec` | Valid `key` values for edge results; `value_type`, `unit`, `description` |
| `object_file_type_spec` | Valid `(object_type, file_role)` pairs; `description` |
| `workflow_file_type_spec` | Valid `file_role` values for workflow files |

### Key columns

**`object`**

| Column | Notes |
|---|---|
| `object_id` | Unique string ID (e.g. `ARW_S01`, `BIN_042`) |
| `object_type` | Controlled by `object_spec` |
| `object_subtype` | Controlled by `object_spec` |
| `label` | Human display name |
| `study_id` | FK to the study object this object belongs to |

**`edge`**

| Column | Notes |
|---|---|
| `child_id` | The object that *is* the relationship target |
| `parent_id` | The object that *provides context* for the child |
| `edge_type` | Controlled by `edge_spec` |

**`object_result`**

| Column | Notes |
|---|---|
| `object_id` | FK to `object` |
| `key` | Measurement name — no units encoded in name |
| `value` | String; cast to `value_type` by callers |
| `unit` | Auto-populated from `object_result_spec.unit` at ingestion |

**`object_result_spec`**

| Column | Notes |
|---|---|
| `object_type` | Which object type this key applies to |
| `key` | Measurement name |
| `value_type` | `text`, `real`, or `integer` |
| `unit` | Canonical unit string (e.g. `°C`, `bp`, `%`) |
| `description` | Human description of this measurement |

**`edge_result_spec`** — same as above but without `object_type`.

### Understanding object_result vs object_file

**`object_result`** — queryable properties *in* the database:
- Small, queryable data you want to filter on (completeness, pH, taxonomy string)
- Append-only history: multiple rows per `object_id + key` allowed
- Example: MAG_001 can have GTDB taxonomy from a 2022 workflow and again from a 2025 workflow

**`object_file`** — file manifest pointing to disk:
- Large data files you don't want in the database (genome.fasta, reads_R1.fastq)
- Tracks location (`file_path`), role (`file_role`), and integrity (`checksum`)
- Database stays lightweight; heavy data stays on disk

---

## Validation Pipeline

`read_bundle()` runs validation in two phases before writing anything.

### Phase 1: Pre-flight (fast, no database queries)

1. Required fields present and filled (`created_by`, or `default_user` provided)
2. No duplicate `workflow_id` or `object_id` within the bundle
3. Empty template rows filtered out

If pre-flight fails, stops immediately — no backup created, database untouched.

### Phase 2: Database validation (within transaction, after backup)

1. **Object type + subtype** — pair exists in `object_spec`
2. **Edge type** — exists in `edge_spec`; parent/child types match `edge_spec` expectations
3. **No duplicate IDs** — `workflow_id`, `object_id`, `person_id` not already in database
4. **Reference integrity** — all referenced IDs exist (in bundle or database)
5. **Result key** — exists in `object_result_spec` for the object's type — or, in interactive mode, prompts to add it
6. **File role** — exists in `object_file_type_spec` for the object's type — or, in interactive mode, prompts to add it
7. **File path uniqueness** — `file_path` not already in database

If any phase 2 check fails: transaction rolls back and database is restored from backup.

### Sheet ingestion order

```
people       → must exist before workflows/objects reference them
workflows    → must exist before edges/results reference them
objects      → must exist before edges reference them
edges        → references objects and workflows
results      → references objects; unit auto-populated from spec
object_files → references objects and workflows
```

---

## Working with Dens

### Initialize a new den

```r
initialize_den("~/projects", "ARW_metagenomics")
```

### Database connection

gopheR resolves the database from the active RStudio project's `den.yaml`. Always disconnect when done.

```r
con <- gopher_con()
DBI::dbGetQuery(con, "SELECT object_id, label FROM object WHERE object_type = 'genome'")
DBI::dbDisconnect(con)
```

```r
use_db("~/other/path.den")  # temporary override
use_den()                    # revert to project den
```

### Bundles

```r
write_bundle("data_entry.xlsx", people_sheet = TRUE)  # create blank bundle
read_bundle("data_entry.xlsx", validate_only = TRUE, default_user = "jdoe")  # dry run
read_bundle("data_entry.xlsx", default_user = "jdoe")  # ingest
```

After ingestion, the bundle is archived to `archive/bundles/` and the database is backed up to `archive/dens/`.

### Starter database

`inst/extdata/starter_db.den` ships with the package and demonstrates a metagenomics-focused spec (samples → readsets → assemblies → genomes/MAGs).

```r
system.file("extdata", "starter_db.den", package = "gopheR")
```

---

## Coding Conventions

- Use tidyverse where it improves clarity; native pipe `|>`, not magrittr `%>%`
- Explicit namespacing (`dplyr::`, `tidyr::`, etc.)
- Use `.data$column` for NSE safety inside `dplyr` verbs
- Avoid loops; prefer joins or `purrr::map_*()`

Tests use the starter database as a fixture; run with `devtools::test()`.

---

See [ROADMAP.md](ROADMAP.md) for planned and future features.
