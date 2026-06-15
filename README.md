
# gopheR

<!-- badges: start -->
<!-- badges: end -->

**Genomic Objects & Provenance for Environmental Research**

gopheR is an R package for tracking bioinformatics objects (samples, assemblies, genomes, etc.), the workflows that produced them, and the results attached to them — all in a SQLite database.

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

Open `my_project.Rproj` — gopheR will find the database automatically via `den.yaml`.

```r
# Temporarily use a different database
use_db("~/projects/other_project/other.den")
use_den()  # revert back to the den
```

### 3. Create a data entry bundle

```r
write_bundle("data_entry.xlsx", people_sheet = TRUE)
```

### 4. Fill in the Excel file, then validate and ingest

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
- **Edges** — directed relationships between objects (`assembled_from`, `binned_from`, etc.); read as "child is edge_type parent" (e.g. "MAG is binned_from assembly")
- **Results** — append-only key/value measurements (`completeness`, `taxonomy`, `pH`, etc.) with units
- **Workflows** — the processes that produced objects or results
- **Den** — a git repo holding your `.den` database; `den.yaml` travels with it and stores display settings

The schema structure (table names, columns) is fixed. The *values* — object types, edge types, result keys, file roles — are defined per-project in spec tables, making gopheR domain-agnostic. See [DETAILS.md](DETAILS.md) for which spec values are strictly controlled and which can be added interactively.

---

## Den Structure

```
my_project/
├── my_project.den         ← SQLite database
├── den.yaml               ← project config (name, settings)
├── my_project.Rproj       ← RStudio project
├── .gitignore
└── archive/
    ├── dens/              ← auto-backup before each ingestion
    └── bundles/           ← archived Excel bundles after ingestion
```

---

## Further Reading

See [DETAILS.md](DETAILS.md) for:
- Architecture and design decisions
- Schema reference and spec table conventions
- Full validation pipeline
- Working with dens
- Key functions reference
- Planned / future ideas
