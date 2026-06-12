
# gopheR

<!-- badges: start -->
<!-- badges: end -->

**Genomic Objects & Provenance for Environmental Research**

gopheR is an R package for tracking bioinformatics objects (samples, assemblies, genomes, etc.), the workflows that produced them, and the results attached to them — all in a SQLite database. It is the foundation of a three-layer ecosystem:

| Layer | Tool | Role |
|---|---|---|
| 1 | **gopheR** (this package) | Schema, validation, ingestion |
| 2 | **Den** | Project git repo holding the database |
| 3 | **[GopherScout](https://github.com/jeffkimbrel/gopherscout)** | Desktop GUI browser (no R required) |

---

## Installation

```r
# install.packages("pak")
pak::pak("jeffkimbrel/gopheR")
```

---

## Quick Start

### 1. Create a den

```r
library(gopheR)
initialize_den("~/projects", "my_project")
# Creates ~/projects/my_project/ with database, den.yaml, .Rproj, git init
```

### 2. Open the den in RStudio/Positron

Open `my_project.Rproj` — gopheR will find the database automatically via `den.yaml`. No path setup needed.

To temporarily use a different database (e.g. a test copy):

```r
use_db("~/projects/other_project/other.den")
use_den()  # revert back to the den
```

### 3. Create a data entry bundle

```r
write_bundle("data_entry.xlsx", people_sheet = TRUE)
# Fill in the Excel file, then...
```

### 4. Validate and ingest

```r
# Dry run first
read_bundle("data_entry.xlsx", validate_only = TRUE, default_user = "jdoe")

# Ingest (auto-backup + transaction safety)
read_bundle("data_entry.xlsx", default_user = "jdoe")
```

### 5. Query

```r
con <- gopher_con()
DBI::dbGetQuery(con, "SELECT object_id, label FROM object WHERE object_type = 'genome'")
DBI::dbDisconnect(con)
```

---

## Key Concepts

- **Objects** — your datasets (samples, readsets, assemblies, genomes, etc.)
- **Edges** — directed relationships between objects (`assembled_from`, `binned_from`, etc.)
- **Results** — append-only key/value measurements (`completeness`, `taxonomy`, `pH`, etc.)
- **Workflows** — the processes that produced objects or results
- **Den** — a git repo holding your `.den` database; created by `initialize_den()`

The schema structure (table names, columns) is fixed. The *values* — object types, edge types, result keys — are defined per-project in spec tables, making gopheR domain-agnostic.

---

## The Den Structure

```
my_project/
├── my_project.den         ← SQLite database
├── den.yaml               ← project config
├── my_project.Rproj       ← RStudio project
├── .gitignore
└── archive/
    ├── dens/              ← auto-backup before each ingestion
    └── bundles/           ← archived Excel bundles after each ingestion
```

---

## Further Reading

See [DETAILS.md](DETAILS.md) for:
- Full validation pipeline (pre-flight + database phases)
- Schema reference
- Design principles
- Working with dens
- Common patterns and gotchas
- Example workflows

---

## Planned / Future Ideas

- [ ] `initialize_den()` `create_examples = TRUE` — populate `examples/` with a fresh starter database and bundle built from `data-raw/` scripts; example data script needs to be made robust to starter DB changes (query spec tables to validate example data rather than hardcoding type/key/role strings)
- [ ] `den.yaml` spec — read by GopherScout to resolve the database path and set the title bar name
- [ ] Post-ingestion SQL dump — timestamped text dump to `archive/dens/` after each ingestion; diffable in git, used by `restore_den()` to rebuild from history
- [ ] AI-assisted bundle generation — agent infers object IDs, edges, and file roles from a folder of files and a naming convention; handles MD5s, file sizes, and metadata TSV parsing; user reviews before import
