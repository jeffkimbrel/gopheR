---
name: fill-bundle
description: Generate a gopheR Excel bundle from a folder of files and a naming convention description
---

# Fill Bundle

You are helping a user generate a gopheR Excel bundle from a set of files. The bundle will be ingested into a gopheR database using `read_bundle()`.

## Your job

Given a folder of files and a description of the naming convention, you will:
1. Read the den's database to understand valid object types, edge types, result keys, and file roles
2. Infer objects, edges, and file associations from the file names and user description
3. Produce a populated Excel bundle (or a script that generates one) ready for review and ingestion

## Step 1: Read the den's spec tables

Before doing anything else, find `den.yaml` in the current directory (or walk up to find it). Then query the database to understand what is valid in this project:

```bash
# Find the database
cat den.yaml

# Query valid types (adjust path to match den.yaml database: field)
sqlite3 <database_path> "SELECT object_type, description FROM object_type;"
sqlite3 <database_path> "SELECT object_type, object_subtype FROM object_subtype;"
sqlite3 <database_path> "SELECT edge_type, parent_type, child_type, description FROM edge_spec;"
sqlite3 <database_path> "SELECT key, object_type, description FROM key_spec;"
sqlite3 <database_path> "SELECT object_type, file_role, description FROM object_file_type_spec;"
```

Report back to the user what types, edges, keys, and file roles are available. Ask for clarification if the project types are unfamiliar.

## Step 2: Understand the file structure

Ask the user to describe (or paste) the file listing. If they can run it locally:

```bash
find /path/to/files -type f | sort
```

If the files are on a remote server and not accessible locally, work from a representative sample or a manually described structure. **Do not assume you can access remote files directly.**

From the file listing and the user's description of the naming convention, identify:
- What each file is (object type, file role)
- What the object IDs should be (usually derived from the filename stem)
- What the parent-child relationships (edges) are between objects
- Which workflow produced which files (ask the user if not obvious)

## Step 3: Build the bundle structure

Map files to gopheR bundle sheets. The bundle has these sheets in order:

### people
Only needed if new contributors are being added. Ask the user if any `created_by` values are new to the database.

### workflow
One row per workflow. Ask the user for:
- `workflow_id` — short identifier (e.g. `checkm2_run1`)
- `description` — what the workflow did, including tool version and key parameters
- `workflow_date` — date run (YYYY-MM-DD)
- `created_by` — person ID

### object
One row per object. Derive from file names where possible. Ask the user to confirm object IDs, types, labels, and descriptions before proceeding.

### edge
One row per relationship. Use the `edge_spec` table to validate that the parent/child type combination is valid. Natural reading: "child IS edge_type parent" (e.g. "assembly assembled_from readset").

### result
One row per measurement. Only include if the user has result data to add. Results are append-only — re-running a workflow creates new rows, it does not overwrite old ones.

### object_file
One row per file. Columns: `object_id`, `file_role`, `file_path`, `workflow_id`, `checksum`.

**Checksums:** If the files are not locally accessible, leave `checksum` blank and note clearly that checksums need to be filled in before ingestion. Blank checksums are allowed by gopheR.

**File paths:** Use the actual server/disk paths, not local paths. Ask the user to confirm the base path.

## Step 4: Produce the bundle

Options — ask the user which they prefer:

**Option A: R script** — generate an R script using `write_bundle()` + `openxlsx` that the user runs locally in their den. This is the most reliable approach.

**Option B: Direct write** — if R is available locally and the den database is accessible, write the bundle directly using `gopheR::write_bundle()` followed by `openxlsx` to populate the sheets.

**Option C: Summary for manual entry** — produce a structured summary (one section per sheet) that the user copies into a bundle they generate themselves with `write_bundle()`.

## Rules and judgment calls

**When object IDs are ambiguous:** Propose a convention and ask for confirmation before proceeding. Document the convention in a comment at the top of the R script.

**When edge relationships are unclear:** Ask. Don't guess. A wrong edge is harder to fix than a missing one.

**When a file role doesn't exist in the spec tables:** Flag it to the user — do not invent file roles. The user may need to add it to `object_file_type_spec` first.

**When result keys don't exist in the spec tables:** Same — flag and ask. Do not add result rows with keys not in `key_spec`.

**When the same object ID appears to come from multiple files:** Ask whether these are the same object (one row, multiple file roles) or different objects (multiple rows).

**When files are on a remote server:** Be explicit that you cannot calculate checksums, verify file existence, or detect file formats from binary headers. The bundle will be a draft — the user should review and fill in checksums before ingesting.

## What this skill does NOT do

- It does not ingest the bundle — that is `read_bundle()` in R
- It does not validate against the database — that happens during ingestion
- It does not calculate checksums for remote files
- It does not modify the database directly
- It does not make decisions about scientific interpretation (which MAG came from which assembly is a scientific question, not a file naming question)

## Customizing this skill for your project

Edit this file to add project-specific context:
- The object type hierarchy used in this project
- Common naming conventions for this project's files
- Which workflows are typically run and what they produce
- Any non-obvious edge relationships specific to this domain

---

*This skill is generated by `initialize_den()` as a starting point. Customize it for your project's naming conventions and object hierarchy.*
