
# gopheR

<!-- badges: start -->
<!-- badges: end -->

**Genomic Objects & Provenance for Environmental Research**

gopheR is an R package framework for managing omics datasets and their relationships. Each dataset is an object with a defined type (e.g., sample, readset, assembly, genome), and objects are connected via defined edges (e.g., "assembled_from", "sequenced_from"). The package provides functions for:

- Reading and writing to a SQLite database
- Creating Excel templates for data entry
- Validating and ingesting data from Excel bundles

## Architecture

gopheR is designed as a **two-package ecosystem**:

### 1. **gopheR (this package)** - The Framework
The reusable toolkit that any project can use:
- ✅ Enforces schema structure (table names, columns, relationships)
- ✅ Provides core functions (`read_bundle()`, `write_bundle()`, validation)
- ✅ Works with any database following gopheR conventions
- ✅ Testable, maintainable, updateable

**What's rigid:** Table and column names (gopheR expects `object_type`, `edge_spec`, etc.)  
**What's flexible:** The VALUES in those tables (define your own types and relationships)

### 2. **gopherDen** - Your Project Implementation
A template repository you fork/customize for your specific project:
- 🗄️ Project-specific SQLite database with custom object types and edges
- 📊 Quarto report templates for your domain questions
- 🖥️ Shiny dashboards for interactive data exploration
- 🤖 LLM-powered natural language queries (via querychat/elmer)
- 📝 Domain-specific analysis functions

**Think of it as:** gopheR is the engine, gopherDen is the car built around it.

### Why This Design?

✅ **Reusability** - gopheR improvements benefit all projects  
✅ **Customization** - Each Den adapts to its specific science domain  
✅ **Accessibility** - Non-coders use Excel + HTML reports, coders use R directly  
✅ **Scalability** - Start simple, add Shiny/LLM features as needed

### Starter Database

gopheR includes a starter database (`inst/extdata/starter_db.sqlite`) with example omics types (genome, readset, assembly, etc.) that you can use as-is or customize for your domain. The examples below use types from this starter database.

## Installation

You can install the development version of gopheR from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("jeffkimbrel/gopheR")
```

## Object Validation

When ingesting objects from an Excel bundle, `read_bundle()` performs the following validation checks **against your database**:

### 1. Object Type Validity
Checks that all `object_type` values are defined in the database `object_type` table.

**Example types (from starter database):** study, site, sample, readset, assembly, genome, amplicon

**Fails if:** An undefined object type is used (e.g., if your database has `genome` but you try to use `bacteria`)

### 2. Object Subtype Validity
Checks that `object_type:object_subtype` combinations are defined in the `object_subtype` table.

**Example valid combinations (from starter database):**
- `genome:MAG` (metagenome-assembled genome)
- `genome:isolate` (isolate genome)
- `readset:paired_end` (paired-end reads)
- `assembly:metagenome` (metagenome assembly)

**Fails if:** An invalid combination is used (e.g., `genome:unknown` if that combination isn't in your `object_subtype` table)

### 3. Duplicates Within Bundle
Checks that each `object_id` appears only once in the Excel sheet.

**Fails if:** The same `object_id` appears in multiple rows

### 4. Duplicates Against Database
Checks that `object_id` values don't already exist in the database.

**Fails if:** Trying to insert an object that already exists

## Design Principles

### Database-Driven Everything

**Critical:** gopheR never hardcodes object types, edge types, or other domain values. All validation pulls from database spec tables dynamically.

**Why?** gopherDen projects will define their own custom types. A MAG genomics project uses `genome:MAG` and `assembled_from`, but a clinical project might use `patient:adult` and `sampled_from`. gopheR must work with ANY valid schema.

**What's rigid (enforced by gopheR):**
- Table names: `object_type`, `object_subtype`, `edge_spec`, `object`, `edge`, `workflow`, etc.
- Column names: `object_id`, `object_type`, `parent_id`, `child_id`, etc.
- Foreign key relationships and schema structure

**What's flexible (customized in gopherDen):**
- VALUES in `object_type` table (your domain-specific types)
- VALUES in `object_subtype` table (your subtypes)
- VALUES in `edge_spec` table (your relationship types)

### Transaction Safety

All ingestion happens within transactions with automatic rollback:
1. Create database backup before any changes
2. Begin transaction
3. Ingest all sheets (objects, edges, workflows, etc.)
4. If ANY error occurs: rollback transaction AND restore from backup
5. If all succeeds: commit transaction

This ensures all-or-nothing ingestion - no partial data corruption.

## Database Schema

gopheR expects the following table structure:

### Specification Tables (Define Valid Values)
- `object_type` - Valid object types for your domain
- `object_subtype` - Valid subtypes for each object type
- `edge_spec` - Valid edge types and which object types they can connect
- `key_spec` - Valid result keys (measurements, statistics, etc.)
- `object_file_type_spec` - Valid file roles for each object type

### Data Tables (Store Actual Data)
- `object` - Your actual datasets/samples/entities
- `edge` - Relationships between objects
- `workflow` - Processing workflows/pipelines
- `result` - Measurements, statistics, quality metrics
- `object_file` - File paths and metadata
- `people` - Contributors and authors

See `inst/extdata/starter_db.sqlite` for complete schema with examples.

## Key Functions

### Bundle Writing
**`write_bundle(out_xlsx, db_path = NULL, ...)`**
- Creates Excel template for data entry
- Dynamically pulls valid values from database spec tables
- Creates dropdowns for type selection
- Prefills workflow IDs
- Multiple sheets: object, edge, workflow, result, object_file

### Bundle Reading
**`read_bundle(bundle_path, db_path = NULL, validate_only = FALSE, backup = TRUE)`**
- Validates all data against database specs
- Creates backup before changes
- Ingests with transaction safety
- Returns detailed summary (counts by type, etc.)
- `validate_only = TRUE` checks without inserting

### Database Connection
**`gopher_db_path(path = NULL, db = "gopheR_db.sqlite")`**
- Resolves database location from options or environment
- `path` should be DIRECTORY containing database, not full file path
- Set via `options(gopheR.db_path = "/path/to/dir")`

**`gopher_con(db_path = NULL, read_only = FALSE)`**
- Opens SQLite connection with foreign keys enabled
- Use `with_gopher_con()` for automatic cleanup

## Development

### Coding Patterns
- ✅ Use tidyverse where it improves clarity
- ✅ Native pipe `|>` not magrittr `%>%`
- ✅ Explicit namespacing (`dplyr::`, `tidyr::`, etc.)
- ✅ Use `.data$column` for NSE safety
- ✅ Avoid loops - prefer joins or `purrr::map_*()`
- ✅ Balance: don't force tidyverse where base R is clearer

### Testing
Tests use the starter database as a fixture:
- 50+ tests covering validation, ingestion, backup/restore
- Tests are isolated (temp directories)
- Helper functions: `create_test_db()`, `setup_test_env()`, `cleanup_test_env()`
- Run tests: `devtools::test()`

### Starter Database
`inst/extdata/starter_db.sqlite` serves dual purpose:
1. **For tests** - Known schema to test against
2. **For users** - Example to copy/customize

Schema is populated with omics examples (genome, MAG, assembled_from) but these are NOT requirements - just examples.

## For gopherDen Developers

When building a gopherDen template:

### 1. Database Setup
- Copy starter database OR create custom schema
- Customize `object_type`, `object_subtype`, `edge_spec` for your domain
- Leave schema structure unchanged (table/column names)

### 2. Project Structure
```r
gopherDen/
├── inst/extdata/
│   └── my_project.sqlite      # Your database
├── reports/                    # Quarto templates
│   ├── site_summary.qmd
│   └── mag_taxonomy.qmd
├── shiny/                      # Interactive apps
│   └── data_explorer/
├── R/                          # Domain functions
│   ├── queries.R              # Common SQL
│   └── plots.R                # Visualizations
└── analysis/                   # Actual analyses
```

### 3. Use gopheR Functions
```r
# Set your database location
options(gopheR.db_path = system.file("extdata", package = "myGopherDen"))
options(gopheR.db_file = "my_project.sqlite")

# Create bundles for users
gopheR::write_bundle("data_entry.xlsx")

# Ingest completed bundles
gopheR::read_bundle("completed_bundle.xlsx")
```

### 4. Add Domain Features
- Quarto reports using your custom types
- Shiny dashboards with domain-specific queries
- LLM integration (querychat/elmer) for natural language queries
- Analysis workflows specific to your science

**Remember:** gopheR is generic. ALL domain knowledge goes in your Den.

## Example Workflow

``` r
library(gopheR)

# Set database path
options(gopheR.db_path = "/path/to/database/folder")
options(gopheR.db_file = "gopheR_db.sqlite")

# Create Excel template for data entry
write_bundle("data_entry_template.xlsx")

# User fills out Excel...

# Validate before inserting
read_bundle("completed_data.xlsx", validate_only = TRUE)

# Insert validated data (with backup + transaction)
results <- read_bundle("completed_data.xlsx", validate_only = FALSE)

# Check summary
results$results$objects
# $n_processed: 15
# $n_inserted: 15
# $by_type: list(genome = 8, readset = 6, assembly = 1)
```

