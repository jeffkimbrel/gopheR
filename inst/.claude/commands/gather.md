---
name: gather
description: File-content extraction guided by the provenance graph — traverses object_file registrations to find the right files, generates scripts for local or remote execution, produces a merge script for multi-location data
---

# Gather

You extract data from bioinformatics files tracked in a gopheR den. Given a plain-English goal, you traverse the provenance graph to find the right objects and their registered file paths, confirm a plan, then generate scripts the user can run wherever the data lives.

**Always generate scripts — never run extractions yourself.** Files may live on a remote server, HPC, or shared filesystem. A generated script is portable, reproducible, and auditable. The user runs it; you write it.

**Never hardcode file role names.** Every den is different. Your understanding of what a file role contains must come from `object_file_type_spec.description` — not from assumptions about role names. A role called `ko_hits` and one called `kegg_annotation` might both contain KO annotations depending on the project; the description is the only reliable signal.

**Propose before generating.** After reading the spec and identifying matching roles, describe your plan to the user and wait for confirmation before writing any scripts.

**Stop and wait.** Any time you ask the user a question or present a plan for confirmation, stop completely. Do not proceed until they respond.

---

## Setup: Read the den

Locate `den.yaml` in the current directory or walk up the tree:

```bash
cat den.yaml
```

Extract:
1. `database:` — path to the `.den` SQLite file
2. `agent_context:` — project-specific naming conventions and domain context

Then load the file role specs — **this is your vocabulary for the entire session:**

```bash
DB=<database_path>

echo "=== Object file roles ==="
sqlite3 "$DB" "SELECT object_type, file_role, description FROM object_file_type_spec ORDER BY object_type, file_role;"

echo "=== Workflow file roles ==="
sqlite3 "$DB" "SELECT file_role, description FROM workflow_file_type_spec ORDER BY file_role;"
```

Read every row. The `description` column is the semantic label — it tells you what the file actually contains. You will use these descriptions to infer which roles are relevant to the user's request. Do not skip this step.

Also load result specs for context (useful when the user's goal involves result-filtered provenance traversal):

```bash
sqlite3 "$DB" "SELECT object_type, key, value_type, unit, description FROM object_result_spec;"
sqlite3 "$DB" "SELECT object_type, object_subtype, description FROM object_subtype;"
```

---

## Understand the request

Read the user's extraction goal carefully. Common patterns:

- "Get all protein sequences for genes hitting K02702" → find objects with a KO-annotation file role, identify locus tags from that file, extract from a protein FASTA file role
- "Collect all GFF files for high-quality MAGs" → filter objects by result value (completeness, contamination), gather registered GFF file paths
- "Pull read depths for all samples from site X" → traverse edges (site → sample), gather coverage files
- "Get taxonomy strings for all MAGs in set Y" → query `set_member` → object, gather taxonomy result values (note: this may be a `/query-den` task if it is already stored as a result, not a file task)

Determine:
1. Which object type(s) are the targets (MAGs, readsets, assemblies, etc.)
2. Whether a provenance traversal is needed (e.g. start from site, arrive at genome)
3. Which file roles are involved — **infer from descriptions, not role names**
4. Whether a result-based filter applies (completeness > 90, etc.)

---

## Resolve file roles

Present the candidate file roles you've identified from the spec, with their descriptions, and explain why you think each is relevant:

> Based on the spec, I see two file roles that might contain KO annotation data:
> - `kegg_annotation` — "KEGG/KOfam annotation results (TSV, locus_tag, KO, score, e-value)" — **this looks like the right one for querying KO hits**
> - `functional_annotation` — "Combined functional annotation table (KEGG, COG, Pfam, one row per gene)" — **also relevant; might contain KO column**
>
> I also see:
> - `protein_fasta` — "Protein sequences in FASTA format, headers are locus_tag" — **this is where I'd pull sequences from**
>
> Does the plan look right? Should I use `kegg_annotation` for the KO lookup, `functional_annotation`, or both?

Wait for confirmation. If only one role clearly matches the description, proceed without asking — but still state which role you chose and why.

If no registered file role clearly matches the user's goal, say so explicitly:

> I don't see a file role in this den whose description suggests it contains [X]. The registered roles are: [list]. You may need to register a file role or adjust your goal.

---

## Coverage check

Before generating any scripts, query what's actually in the den:

```bash
# Which objects have the target file role(s)?
sqlite3 "$DB" "
SELECT o.object_id, o.object_type, o.object_subtype, o.label,
       f.file_path, f.file_format
FROM object o
JOIN object_file f ON f.object_id = o.object_id
WHERE f.file_role = '<target_role>'
  AND o.object_type = '<target_type>'
ORDER BY o.object_id;
"

# How many objects of this type exist total?
sqlite3 "$DB" "
SELECT COUNT(*) FROM object WHERE object_type = '<target_type>';
"
```

If a result filter applies (e.g. high-quality MAGs only):

```bash
sqlite3 "$DB" "
SELECT o.object_id, o.label,
       MAX(CASE WHEN r.key = 'completeness' THEN CAST(r.value AS REAL) END) AS completeness,
       MAX(CASE WHEN r.key = 'contamination' THEN CAST(r.value AS REAL) END) AS contamination,
       f.file_path
FROM object o
LEFT JOIN object_result r ON r.object_id = o.object_id
LEFT JOIN object_file f ON f.object_id = o.object_id AND f.file_role = '<target_role>'
WHERE o.object_type = '<target_type>'
GROUP BY o.object_id
HAVING completeness > 90 AND contamination < 5
ORDER BY o.object_id;
"
```

---

## Accessibility check

For each registered file path, test whether it's locally accessible:

```bash
while IFS= read -r path; do
  if [ -f "$path" ]; then
    echo "LOCAL $path"
  else
    echo "MISSING $path"
  fi
done < <(sqlite3 "$DB" "SELECT file_path FROM object_file WHERE file_role = '<target_role>';")
```

Group inaccessible paths by their longest common prefix to infer which server they likely live on. For example, if 12 paths all start with `/srv/projects/lab/`, they probably share a server. Paths that start with `/home/user/`, `/Users/`, or a relative path are likely local and just missing (stale path or unmounted drive — flag these separately).

---

## Coverage report

**Before generating any scripts, present a coverage report.** This is the user's signal to bail out if something is wrong.

Example format:

> **Coverage report — `protein_fasta` for genome objects**
>
> Total genome objects in den: 45
> Objects with `protein_fasta` registered: 38 (7 have no file registered)
>
> Missing registrations (not in `object_file`):
> - `mARW1_009`, `mARW1_015`, `mARW2_003`, `mARW3_001`, `mARW3_008`, `mARW4_002`, `mARW5_001`
>
> Of the 38 registered:
> - 22 accessible locally ✓
> - 16 inaccessible — paths start with `/srv/jgi/`:
>   - `mARW1_001`, `mARW1_002`, ... (16 total — likely on the JGI server)
>
> **Plan:** I'll generate two scripts:
> 1. `extract_local.sh` — run locally to process the 22 local files
> 2. `extract_remote_jgi.sh` — run on the server where `/srv/jgi/` is mounted (processes 16 files)
> 3. `merge_results.sh` — run locally after copying outputs back; concatenates all results
>
> The 7 objects with no file registered will be omitted and listed in the summary log.
>
> Does this look right? Any corrections before I generate the scripts?

Wait for confirmation.

---

## Script generation

Generate separate scripts for each group of files. Store them in `archive/agent/<YYYY-MM-DD_HHMM>/`.

```bash
SESSION_DIR="archive/agent/$(date +%Y-%m-%d_%H%M)"
mkdir -p "$SESSION_DIR"
```

### Header: always include this block at the top of every generated script

```bash
#!/usr/bin/env bash
# Generated by /gather — gopheR den extraction script
# Den: <den_name> (<database_path>)
# Generated: <date>
# Goal: <user's original request in one sentence>
# File role: <role_name> — "<description from spec>"
# Objects: <N> targets; <M> processed here
#
# UNCERTAINTY NOTICE:
#   Tier 1 (checked before generation): which objects have <role> registered
#   Tier 2 (checked at runtime): whether each file exists at its registered path,
#            whether file headers match expected format
#   Tier 3 (unknowable): file contents may have changed since ingestion;
#            paths may be stale if files were moved
#
# Run this script on the machine where the listed file paths are accessible.
# Output goes to: <output_dir>/

set -euo pipefail
OUTPUT_DIR="<session_dir>/output_<server_or_local>"
mkdir -p "$OUTPUT_DIR"
LOG="$OUTPUT_DIR/run_summary.log"
echo "Started: $(date)" > "$LOG"
```

### Tier 2 validation: build into every script

Every script must check the file exists and (if the format has a parseable header) validate that the expected columns or format are present before processing:

```bash
# Per-file loop pattern
while IFS=$'\t' read -r object_id file_path; do
  if [ ! -f "$file_path" ]; then
    echo "MISSING\t$object_id\t$file_path" >> "$LOG"
    continue
  fi

  # Format validation (example for TSV with expected columns)
  HEADER=$(head -1 "$file_path")
  if ! echo "$HEADER" | grep -q "locus_tag"; then
    echo "BAD_FORMAT\t$object_id\t$file_path\texpected locus_tag column, got: $HEADER" >> "$LOG"
    continue
  fi

  # ... extraction logic ...
  echo "OK\t$object_id\t$file_path\t$HITS hits" >> "$LOG"

done < object_file_list.tsv

echo "Completed: $(date)" >> "$LOG"
echo "Summary written to: $LOG"
```

### Checksum validation (if registered)

If checksums are stored in `object_file.checksum`, include a validation step before parsing:

```bash
REGISTERED_CHECKSUM="<checksum from DB>"
ACTUAL_CHECKSUM=$(md5sum "$file_path" | awk '{print $1}')
if [ "$ACTUAL_CHECKSUM" != "$REGISTERED_CHECKSUM" ]; then
  echo "CHECKSUM_MISMATCH\t$object_id\t$file_path" >> "$LOG"
  continue
fi
```

### Format-specific extraction patterns

Use the `file_format` column from `object_file` to determine the right extraction approach. Do not assume a format from the file extension alone.

**FASTA (sequence extraction by ID list):**

Preferred: use `seqkit` if available; fall back to Python:
```bash
# seqkit (check availability first)
if command -v seqkit &>/dev/null; then
  seqkit grep -f ids_to_extract.txt "$file_path" >> "$OUTPUT_DIR/combined.fasta"
else
  python3 - <<'PYEOF'
import sys
target_ids = set(open("ids_to_extract.txt").read().splitlines())
in_target = False
for line in open(sys.argv[1]):
    if line.startswith(">"):
        in_target = line[1:].split()[0] in target_ids
    if in_target:
        print(line, end="")
PYEOF
fi
```

When merging FASTAs across multiple objects, **prepend the object_id to each sequence header** to preserve provenance:
```bash
awk -v obj="$object_id" '/^>/{print ">" obj "|" substr($0,2); next} {print}' "$fasta_path"
```

**TSV/CSV (column filtering, row selection):**

```bash
# Find column index (portable awk, header on row 1)
COL_IDX=$(head -1 "$file_path" | tr '\t' '\n' | grep -n "^KO$" | cut -d: -f1)
if [ -z "$COL_IDX" ]; then
  echo "BAD_FORMAT\t$object_id\tcould not find KO column" >> "$LOG"
  continue
fi
# Filter rows matching target value, prepend object_id
awk -v col="$COL_IDX" -v obj="$object_id" -v target="$TARGET_KO" \
  'NR>1 && $col == target {print obj "\t" $0}' "$file_path"
```

**GFF3:**
```bash
# Extract features matching attribute value
awk -v target="$LOCUS_TAG" \
  '$3 == "CDS" && $9 ~ target {print}' "$file_path"
```

**Unknown format:** If `file_format` is blank or unfamiliar, describe what you can infer from the spec description and ask the user what extraction approach to use before writing extraction code.

---

## Multi-location script set

When files span multiple servers, produce a named script per location plus a merge script:

### `extract_<location>.sh` (one per inaccessible group)

Name the location based on the common path prefix (e.g. `extract_srv_jgi.sh` for paths under `/srv/jgi/`). Include at the top:

```
# Run this script on the machine where /srv/jgi/ is accessible.
# After it completes, copy the entire output/ directory back to your local machine
# and run merge_results.sh.
```

Hardcode the exact file paths from the database into the script — no dynamic discovery. The user should be able to inspect the list before running.

### `merge_results.sh` (local, run after collecting all outputs)

```bash
#!/usr/bin/env bash
# Run locally after copying all remote output directories here.
# Expected inputs:
#   output_local/      (from extract_local.sh)
#   output_srv_jgi/    (copied from JGI server)
#
# Output: combined_<goal>_<date>.fasta (or .tsv)

set -euo pipefail
OUTPUT="combined_<goal>_$(date +%Y%m%d).fasta"

for dir in output_local output_srv_jgi; do
  if [ ! -d "$dir" ]; then
    echo "WARNING: $dir not found — did you copy it back?" >&2
    continue
  fi
  cat "$dir"/*.fasta >> "$OUTPUT"
done

echo "Objects processed:"
grep "^OK" output_*/run_summary.log | wc -l
echo "Objects missing files:"
grep "^MISSING" output_*/run_summary.log | wc -l
echo "Format errors:"
grep "^BAD_FORMAT" output_*/run_summary.log | wc -l
echo "Output: $OUTPUT"
```

---

## Final summary to user

After generating the scripts, tell the user:

1. What scripts were generated and where
2. Which script to run where (local vs. each remote location)
3. What to copy back after remote runs
4. How to run the merge script
5. What the summary log will contain

Example:

> **Scripts generated in `archive/agent/2026-07-20_1430/`:**
>
> - `extract_local.sh` — run on this machine; processes 22 local genome files
> - `extract_srv_jgi.sh` — run on the server where `/srv/jgi/` is accessible; processes 16 files
> - `merge_results.sh` — run here after copying `output_srv_jgi/` back locally
>
> **To run:**
> 1. `bash archive/agent/2026-07-20_1430/extract_local.sh`
> 2. Copy `extract_srv_jgi.sh` to the JGI server → `bash extract_srv_jgi.sh` → copy back `output_srv_jgi/`
> 3. `bash archive/agent/2026-07-20_1430/merge_results.sh`
>
> Final output: `combined_K02702_proteins_20260720.fasta`
> Per-object run log: `output_*/run_summary.log`
>
> The 7 objects with no `protein_fasta` registered were skipped and listed in the coverage report above.

---

## Rules

- **No hardcoded file role names.** Everything comes from `object_file_type_spec.description`.
- **Ask when ambiguous.** If two roles could plausibly match, ask — don't guess.
- **Coverage report before scripts.** Never generate scripts without first showing what's registered and what's accessible.
- **Always generate scripts.** Never try to SSH, rsync, or remotely execute extraction yourself.
- **Group remote files by path prefix.** Files sharing a common path prefix likely share a server — one script per server.
- **Tier 2 checks in every script.** File existence, header/format validation, optional checksum. Always produce a run_summary.log.
- **Prepend object_id to merged FASTA headers.** Preserves provenance when combining sequences across objects.
- **State Tier 3 uncertainty.** Include the UNCERTAINTY NOTICE block in every script header.
- **Scientific judgment stays with the user.** Do not decide which locus tag format is correct, which gene is the right hit, or how to handle multi-copy genes. Flag and ask.

---

## What this skill does NOT do

- Ingest data into the den — use `/fill-bundle` for that
- Query result values stored in the database — use `/query-den` for that
- Execute scripts on remote servers
- Make biological decisions about which hits are valid or which format is authoritative
- Compute checksums for files not locally accessible
