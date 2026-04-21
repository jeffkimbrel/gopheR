
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

## Key Features

- 📋 **Excel-based data entry** - Create templates with validated dropdowns
- ✅ **Two-phase validation** - Pre-flight checks before backup, database validation within transaction
- 🔄 **Transaction safety** - All-or-nothing ingestion with automatic rollback and backup restore
- 👥 **People management** - Auto-create from created_by, interactive email prompts, duplicate prevention
- 🗂️ **Sheet ordering** - Left-to-right workflow (people → workflow → object → edge → result → object_file)
- 📊 **Result tracking** - Append-only history for measurements (completeness, taxonomy, N50, etc.)
- 📁 **File manifest** - Track data files on disk (FASTA, FASTQ, annotations) with checksums
- 🚀 **Fast failure** - Pre-flight validation catches issues before expensive operations
- 🎯 **Database-driven** - No hardcoded types, all validation from database specs
- 🧪 **Well-tested** - 45 tests with 114 passing assertions (full coverage of validation and ingestion)

## Validation and Ingestion

`read_bundle()` performs validation in **two phases** for fast failure and safety:

### Phase 1: Pre-flight Checks (Before Backup)
Fast validation on bundle content only, no database queries:

1. **Required Fields** - `created_by` must be filled (or provide `default_user` parameter)
2. **Duplicate IDs** - No duplicate `workflow_id` or `object_id` within bundle
3. **Empty Rows** - Filters out template rows (empty description, etc.)

If any pre-flight check fails, stops immediately without creating backup.

### Phase 2: Database Validation (After Backup, Within Transaction)
Validates against database specs:

1. **Object Type Validity** - All `object_type` values exist in `object_type` table
   - Example types: study, site, sample, readset, assembly, genome, amplicon
   - Fails if undefined type used (e.g., `bacteria` when not in database)

2. **Object Subtype Validity** - `object_type:object_subtype` combinations exist in `object_subtype` table
   - Example: `genome:MAG`, `genome:isolate`, `readset:paired_end`, `assembly:metagenome`
   - Fails if invalid combination (e.g., `genome:unknown` if not defined)

3. **Edge Type Validity** - Edge types defined in `edge_spec` table
   - Example: `assembled_from`, `binned_from`, `sequenced_from`

4. **Edge Compatibility** - Parent/child object type combinations valid per `edge_spec`
   - Example: `assembled_from` requires `readset` → `assembly`
   - Fails if wrong types connected (e.g., `genome` → `assembly` with `assembled_from`)

5. **Existing IDs** - No duplicate IDs already in database
   - Checks `workflow_id`, `object_id`, `person_id`

6. **Reference Integrity** - All referenced IDs exist (in bundle or database)
   - Edge parent/child must reference valid objects
   - Edge workflow_id must reference valid workflows
   - Result object_id and workflow_id must reference valid objects/workflows
   - Object_file object_id and workflow_id must reference valid objects/workflows

7. **Result Key Validity** - Result keys defined in `key_spec` table
   - Example keys: completeness, contamination, N50, pH, GTDB_taxonomy
   - Fails if undefined key used (e.g., "KEGG" if not in key_spec)
   - **Note:** Multiple rows per object_id + key allowed (append-only history)

8. **Object_file Role Validity** - File roles defined in `object_file_type_spec` for that object_type
   - Example: genome can have genome_fasta, protein_fasta, annotation_gff
   - Fails if wrong role for type (e.g., fastq_r1 for genome, should be for readset)

9. **File Path Uniqueness** - file_path must be unique across database
   - Prevents pointing multiple records to same physical file
   - Fails if path already in database or duplicated in bundle

If any validation fails: transaction rolls back + restores from backup.

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
1. **Pre-flight validation** - Fast checks on bundle content (no database queries)
2. **Create backup** - Only if pre-flight passes (saves time on obvious errors)
3. **Begin transaction**
4. **Database validation** - Check against specs within transaction
5. **Ingest all sheets** - Order: people → workflows → objects → edges → results → files
6. **Commit or rollback**:
   - If ANY error: rollback transaction AND restore from backup
   - If all succeeds: commit transaction

This ensures all-or-nothing ingestion - no partial data corruption.

**Processing order matters:** People must exist before workflows/objects reference them. Workflows and objects must exist before edges reference them.

### KISS Principle (Keep It Simple, Stupid)

gopheR follows the **Principle of Least Astonishment** - the interface should behave exactly as users expect:

**Excel Sheet Design:**
- ✅ Sheets ordered left-to-right in fill-out sequence (people → workflow → object → edge)
- ✅ Only include columns users need to fill (exclude auto-generated fields)
- ✅ Set sensible defaults automatically (is_active = 1 for new people)
- ✅ Pre-fill when helpful (workflow IDs), empty when not (people rows)
- ❌ Don't expose complexity users shouldn't think about

**Validation:**
- ✅ Fail fast with clear error messages
- ✅ One error per issue (not cascading errors)
- ✅ Pre-flight checks before expensive operations
- ❌ Don't make users guess what went wrong

**People Management:**
- ✅ Auto-create people from created_by (reduces friction)
- ✅ Prompt for email to prevent typo-duplicates (interactive guidance)
- ✅ Warn but allow proceeding (trust the user's judgment)
- ❌ Don't block workflows with excessive validation

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
- `people` - Contributors and authors
- `result` - Queryable measurements/metrics (append-only history)
- `object_file` - File manifest (pointers to data on disk)

### Understanding Result vs Object_file

**Why two separate tables?**

**`result` table** - Queryable properties **in the database**:
- Small, queryable data you want to filter on
- Example: completeness (95.2%), contamination (2.1%), pH (7.5), GTDB taxonomy
- Enables queries: "All MAGs with completeness > 90%", "Proteobacteria from pH > 7 soils"
- **Append-only history**: Multiple rows per object_id + key allowed
  - Example: MAG_001 has GTDB taxonomy from 2022 workflow AND 2025 workflow
  - Preserves "truth at the time" - taxonomy changes over time but history remains
- Keys constrained by `key_spec` table (controlled vocabulary)

**`object_file` table** - File manifest **pointing to disk**:
- Large data files you don't want in the database
- Example: genome.fasta (50MB), reads_R1.fastq (5GB), annotation.gff (10MB)
- Tracks: where it is (file_path), what it is (file_role), integrity (checksum)
- Database stays lightweight (metadata only), heavy data stays on disk
- File roles constrained by `object_file_type_spec` for each object_type
  - Example: genome can have genome_fasta, protein_fasta, annotation_gff
  - Example: readset can have fastq_r1, fastq_r2

**Design philosophy:**
- Database = catalog/index with queryable metadata
- Disk = actual data files
- Result = IN database (small, queryable)
- Object_file = ON disk (large, referenced)

See `inst/extdata/starter_db.sqlite` for complete schema with examples.

## Key Functions

### Bundle Writing
**`write_bundle(out_xlsx, db_path = NULL, people_sheet = FALSE, ...)`**
- Creates Excel template for data entry
- Dynamically pulls valid values from database spec tables
- Creates dropdowns for type selection (object types, edge types)
- Prefills workflow IDs (10 suggested IDs)
- **Sheet order (left-to-right):** people (optional) → workflow → object → edge → result → object_file
- `people_sheet = TRUE` adds people sheet for adding new contributors
  - Columns: person_id, full_name, email, successor_person_id
  - Excludes is_active (auto-set to 1) and created_at (auto-generated)

### Bundle Reading
**`read_bundle(bundle_path, db_path = NULL, validate_only = FALSE, backup = TRUE, default_user = NULL)`**
- **Pre-flight validation** before backup (fast fail on obvious errors)
- Creates backup before changes (only if pre-flight passes)
- Ingests with transaction safety (all-or-nothing)
- Returns detailed summary (counts by type, auto-created people, etc.)

**Parameters:**
- `validate_only = TRUE` - Checks without inserting
- `backup = FALSE` - Skip backup (faster for testing, not recommended for production)
- `default_user = "username"` - Fills empty `created_by` fields with this value

**People Management:**
- People sheet: Insert new people (errors if person_id exists)
- `created_by` fields: Auto-creates missing people
  - **Interactive mode:** Prompts for email, warns if email exists
  - **Non-interactive:** Creates with NA email (scripts, tests)
- Always sets `is_active = 1` for new people

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

When building a gopherDen template, you're creating a **domain-specific implementation** using gopheR as the engine.

### 1. Database Setup

Start with `inst/extdata/starter_db.sqlite` from gopheR as a template.

**What You Can Customize:**
- VALUES in `object_type` table (your domain-specific types)
  - Example omics: study, site, sample, readset, assembly, genome, amplicon
  - Example clinical: patient, sample, assay, diagnosis, treatment
- VALUES in `object_subtype` table (subtypes for each type)
  - Example: genome:MAG, genome:isolate, readset:paired_end
- VALUES in `edge_spec` table (relationships and which types they connect)
  - Example: assembled_from (readset → assembly), binned_from (assembly → genome)
- VALUES in `key_spec` table (result measurements for your domain)
  - Example: N50, completeness, contamination, coverage

**What You CANNOT Change:**
- Table names: `object_type`, `object_subtype`, `edge_spec`, `object`, `edge`, `workflow`, `people`, etc.
- Column names: `object_id`, `object_type`, `parent_id`, `child_id`, `created_by`, etc.
- Schema structure (foreign keys, relationships)

**Database Location:**
```r
# Store in your package
inst/extdata/my_domain.sqlite

# Set in .Rprofile or package .onLoad():
options(gopheR.db_path = system.file("extdata", package = "myGopherDen"))
options(gopheR.db_file = "my_domain.sqlite")
```

### 2. Project Structure

```r
gopherDen/
├── inst/extdata/
│   └── my_domain.sqlite        # Your customized database
├── data-raw/                   # Scripts to populate spec tables
│   └── setup_domain_types.R
├── reports/                    # Quarto templates
│   ├── site_summary.qmd       # Domain-specific reports
│   └── mag_taxonomy.qmd
├── shiny/                      # Interactive apps
│   └── data_explorer/          # Shiny dashboard
│       ├── app.R
│       └── modules/
├── R/                          # Domain functions
│   ├── queries.R              # Common SQL queries
│   ├── plots.R                # Visualizations
│   └── analysis.R             # Domain-specific functions
├── analysis/                   # User-facing analysis scripts
│   └── examples/
├── vignettes/                  # How-to guides
│   └── getting_started.Rmd
└── README.md                   # Domain-specific instructions
```

### 3. Use gopheR Functions

All data I/O goes through gopheR:

```r
# Create bundle for data entry (uses your custom types)
gopheR::write_bundle("data_entry.xlsx", people_sheet = TRUE)

# Ingest completed bundles
results <- gopheR::read_bundle("completed_bundle.xlsx", default_user = "lab_manager")

# Query with your domain knowledge
con <- gopheR::gopher_con()
my_genomes <- DBI::dbGetQuery(con, "
  SELECT object_id, label 
  FROM object 
  WHERE object_type = 'genome' AND object_subtype = 'MAG'
")
DBI::dbDisconnect(con)
```

### 4. Add Domain Features

**Quarto Reports:**
- Use your custom object/edge types in queries
- Generate domain-specific visualizations
- Example: MAG quality summaries, sample provenance traces

**Shiny Dashboards:**
- Interactive exploration of your data
- Dynamic filtering by your custom types
- Network visualization of edges

**LLM Integration (querychat/elmer):**
- Natural language queries over your database
- Example: "Show me all high-quality MAGs from Site A"
- LLM knows your custom types and relationships

**Domain Functions:**
- Write helper functions for common queries
- Example: `get_mags_by_site()`, `trace_sample_provenance()`
- Keep domain logic in gopherDen, not gopheR

### 5. Critical Architecture Rules

**✅ DO:**
- Trust gopheR for all validation and ingestion
- Use gopheR functions (`read_bundle`, `write_bundle`, `gopher_con`)
- Define domain types in your database
- Write domain-specific queries and reports
- Document your custom types and workflows

**❌ DON'T:**
- Modify gopheR package code
- Hardcode object types in your code (query from database)
- Bypass gopheR's validation
- Store data outside the gopheR database schema
- Mix domain logic into gopheR

### 6. Example Domain Customization

**Omics Project (MAGs):**
```r
# object_type: study, site, sample, readset, assembly, genome, amplicon
# object_subtype: genome:MAG, genome:isolate, readset:paired_end
# edge_spec: sequenced_from, assembled_from, binned_from
# Reports: MAG quality, taxonomy, abundance across sites
```

**Clinical Project:**
```r
# object_type: patient, sample, assay, diagnosis, treatment
# object_subtype: sample:blood, sample:tissue, assay:RNAseq
# edge_spec: sampled_from, derived_from, diagnosed_with
# Reports: Patient timelines, treatment outcomes, sample tracking
```

### 7. Testing Your Den

```r
# Test bundle creation
bundle_path <- tempfile(fileext = ".xlsx")
gopheR::write_bundle(bundle_path)

# Verify sheets have your custom types in dropdowns
wb <- openxlsx::loadWorkbook(bundle_path)
# Check that object_type dropdown has your types

# Test ingestion with sample data
gopheR::read_bundle("test_data.xlsx", validate_only = TRUE)
```

**Remember:** gopheR is the framework, gopherDen is the application. ALL domain knowledge (types, reports, analyses) lives in your Den, not in gopheR.

## Quick Reference

### Common Patterns

**Setup database path:**
```r
options(gopheR.db_path = "/path/to/database/folder")
options(gopheR.db_file = "my_db.sqlite")
```

**Create bundle with people sheet:**
```r
gopheR::write_bundle("data.xlsx", people_sheet = TRUE)
```

**Read bundle with default user:**
```r
gopheR::read_bundle("data.xlsx", default_user = "jkimbrel")
```

**Validation only (dry run):**
```r
gopheR::read_bundle("data.xlsx", validate_only = TRUE, default_user = "jkimbrel")
```

**Query database:**
```r
gopheR::with_gopher_con(function(con) {
  DBI::dbGetQuery(con, "SELECT * FROM object WHERE object_type = 'genome'")
})
```

### Common Gotchas

❌ **Don't hardcode types:**
```r
# BAD - breaks when gopherDen uses different types
if (object_type == "genome") { ... }
```

✅ **Query from database:**
```r
# GOOD - works with any gopherDen
valid_types <- DBI::dbGetQuery(con, "SELECT object_type FROM object_type")
if (object_type %in% valid_types$object_type) { ... }
```

❌ **Don't pass full file path to db_path:**
```r
# BAD - db_path should be DIRECTORY
gopher_con(db_path = "/path/to/db.sqlite")
```

✅ **Pass directory only:**
```r
# GOOD - directory containing the database
options(gopheR.db_path = "/path/to/database")
options(gopheR.db_file = "db.sqlite")
gopher_con()  # Uses options
```

❌ **Don't modify bundles via gopheR:**
```r
# BAD - gopheR is insert-only
# No update/delete functionality for safety
```

✅ **Use SQL for updates (outside gopheR):**
```r
# GOOD - manual SQL for corrections
con <- gopheR::gopher_con(read_only = FALSE)
DBI::dbExecute(con, "UPDATE object SET label = 'New Label' WHERE object_id = 'obj001'")
DBI::dbDisconnect(con)
```

### Data Flow

```
1. User creates bundle:     write_bundle() → Excel file
2. User fills Excel:        Manual data entry
3. Pre-flight validation:   Fast checks (no DB)
4. Backup creation:         Safety checkpoint
5. Database validation:     Deep checks (with DB)
6. Transaction ingestion:   All sheets processed
7. Commit or rollback:      Success or restore backup
```

### Sheet Processing Order

```
people       → Must exist before workflows/objects reference them
workflows    → Must exist before edges/results reference them
objects      → Must exist before edges reference them
edges        → References objects and workflows
results      → References objects and workflows
object_files → References objects and workflows
```

## Example Workflows

### Basic Workflow

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
results <- read_bundle("completed_data.xlsx")

# Check summary
results$results$objects
# $n_processed: 15
# $n_inserted: 15
# $by_type: list(genome = 8, readset = 6, assembly = 1)
```

### Including People Sheet

``` r
# Create bundle with people sheet for adding new contributors
write_bundle("data_entry.xlsx", people_sheet = TRUE)

# User fills in people, workflows, objects, edges...

# If user forgot to fill created_by fields, provide default
results <- read_bundle("data_entry.xlsx", default_user = "jkimbrel")
# ℹ Filled 5 empty workflow created_by field(s) with 'jkimbrel'
# ℹ Filled 12 empty object created_by field(s) with 'jkimbrel'

# New people detected in created_by will prompt for email:
# New user 'alice' detected in bundle.
# Enter email address for 'alice': alice@example.com
# (checks for duplicate emails and warns if found)
```

### Pre-flight Validation Failures

``` r
# Fast failure - stops before backup if issues found
read_bundle("incomplete_data.xlsx")
# ✖ Pre-flight validation failed:
#   - Workflow sheet has 3 row(s) with empty created_by field.
#     Provide default_user parameter or fill in Excel.
#   - Duplicate object_ids in bundle: obj_001, obj_005

# No backup created, no database touched - fix and retry
```

### Validation-Only Mode

``` r
# Check data quality without committing
read_bundle("data.xlsx", validate_only = TRUE, default_user = "jkimbrel")
# ✔ All validations passed (no insertion)
# ✔ Would insert 15 objects, 8 edges, 2 workflows
```

### Including Result and Object_file Sheets

``` r
# Create bundle with all sheets
write_bundle("full_bundle.xlsx", people_sheet = TRUE)

# User fills all sheets with:
# - People: contributors
# - Workflows: GTDB r214 (2025), CheckM v2
# - Objects: MAG genomes
# - Edges: binned_from relationships
# - Result: completeness, contamination, GTDB taxonomy for each MAG
# - Object_file: paths to genome.fasta, protein.fasta, annotation.gff files

# Ingest everything
results <- read_bundle("full_bundle.xlsx", default_user = "jkimbrel")

# Query results (append-only history)
con <- gopheR::gopher_con()
taxonomy_history <- DBI::dbGetQuery(con, "
  SELECT r.object_id, r.workflow_id, r.value, w.workflow_date
  FROM result r
  JOIN workflow w ON r.workflow_id = w.workflow_id
  WHERE r.key = 'GTDB_taxonomy'
  ORDER BY r.object_id, w.workflow_date
")
# Shows taxonomy evolution over time

# Get file paths
files <- DBI::dbGetQuery(con, "
  SELECT object_id, file_role, file_path, checksum
  FROM object_file
  WHERE object_id = 'MAG_001'
")
# Returns paths to genome FASTA, proteins, annotations
DBI::dbDisconnect(con)
```

