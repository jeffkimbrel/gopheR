---
name: query-den
description: Natural language queries against a gopheR den — translates questions into SQL, confirms the query, executes it, and returns results; SELECT only
---

# Query Den

You translate natural language questions about a gopheR den into SQL queries, confirm the generated SQL with the user, execute it, and return the results.

**SELECT only.** Never generate or run INSERT, UPDATE, DELETE, DROP, ALTER, or any other data-modifying SQL. If the user asks for something that would modify data, redirect them to gopheR's R functions or GopherScout's inline editing.

**Show your work.** Always display the generated SQL before running it. If you are unsure which table or column applies, say so — don't silently guess.

**Bias toward structured output.** Return results as a markdown table when the result set is small (≤ 50 rows). For larger result sets, write to a TSV and tell the user the path.

---

## Setup: Read the den

Before writing any SQL, locate `den.yaml` in the current directory (or walk up the tree) and read it:

```bash
cat den.yaml
```

Extract:
1. `database:` — path to the `.den` SQLite file
2. `agent_context:` — project naming conventions, object types used, and domain context. Use this to ground your SQL (e.g. if the den uses `genome:MAG` vs `genome:isolate`, the context will say so).

Then load the spec tables so you understand what vocabulary this den uses:

```bash
DB=<database_path>

sqlite3 "$DB" "SELECT object_type, description FROM object_type;"
sqlite3 "$DB" "SELECT object_type, object_subtype, description FROM object_subtype;"
sqlite3 "$DB" "SELECT edge_type, parent_type, child_type, description FROM edge_spec;"
sqlite3 "$DB" "SELECT object_type, key, value_type, unit, description FROM object_result_spec;"
sqlite3 "$DB" "SELECT key, value_type, unit, description FROM edge_result_spec;"
sqlite3 "$DB" "SELECT object_type, file_role, description FROM object_file_type_spec;"
sqlite3 "$DB" "SELECT db_name, label FROM external_db;"
```

Also get a sense of the data volume:

```bash
sqlite3 "$DB" "SELECT object_type, object_subtype, COUNT(*) AS n FROM object GROUP BY object_type, object_subtype ORDER BY object_type;"
```

Use the spec descriptions — not your assumptions — to interpret what the user is asking. If a result key called `completeness` has unit `%` per the spec, that's what it means. If `kegg_annotation` has description `"KEGG/KOfam annotation results (TSV, one row per gene)"`, that's how you know to look there for KO information.

---

## Core schema

All dens share this table structure. Column names are exact:

| Table | Key columns |
|---|---|
| `object` | `object_id`, `object_type`, `object_subtype`, `label`, `description`, `created_by`, `created_at` |
| `edge` | `edge_id`, `parent_id`, `child_id`, `edge_type`, `workflow_id`, `created_at` |
| `workflow` | `workflow_id`, `description`, `created_by`, `workflow_date`, `created_at` |
| `person` | `person_id`, `full_name`, `email`, `successor_person_id` |
| `object_result` | `result_id`, `object_id`, `workflow_id`, `key`, `value`, `unit` |
| `object_file` | `file_id`, `object_id`, `file_role`, `file_path`, `file_format`, `workflow_id`, `checksum` |
| `workflow_file` | `file_id`, `workflow_id`, `file_role`, `file_path`, `file_format`, `checksum` |
| `edge_result` | `result_id`, `edge_id`, `key`, `value`, `unit` |
| `external_id` | `external_id_id`, `object_id`, `db_name`, `accession_id` |
| `set` | `set_id`, `label`, `created_by`, `created_at` |
| `set_member` | `set_id`, `object_id` |

`result_id`, `file_id`, `edge_id`, `external_id_id` are autoincrement integers. `object_id`, `workflow_id`, `person_id`, `set_id` are TEXT primary keys assigned by the user/agent at ingest.

`object_result.value` is TEXT in the database — cast to REAL or INTEGER in SQL when comparing numerically:
```sql
CAST(value AS REAL) > 90
```

Edge semantics: **child IS edge_type OF parent.** "readset `sequenced_from` sample" means the readset is the child, the sample is the parent.

---

## Query generation

**Understand the intent first.** Map the user's question to the relevant tables:

- "Which MAGs have completeness > 90%?" → `object` JOIN `object_result` on `object_id` WHERE `key = 'completeness'` AND `CAST(value AS REAL) > 90`
- "What samples came from site X?" → `edge` table, walk from site (parent) to sample (child) via `edge_type = 'collected_at'` (or whatever the spec says)
- "Show me all files registered for genome Y" → `object_file` WHERE `object_id = 'Y'`
- "Which workflows created edges between readsets and assemblies?" → `edge` JOIN `workflow` WHERE parent/child types match
- "What external IDs does MAG Z have?" → `external_id` WHERE `object_id = 'Z'`
- "Who created the most objects?" → `object` GROUP BY `created_by` ORDER BY `COUNT(*) DESC`

**For provenance traversal**, use a recursive CTE when you need ancestors or descendants beyond one hop:

```sql
-- All descendants of a given object (BFS)
WITH RECURSIVE descendants(object_id, depth) AS (
  SELECT 'ROOT_ID', 0
  UNION ALL
  SELECT e.child_id, d.depth + 1
  FROM edge e
  JOIN descendants d ON e.parent_id = d.object_id
)
SELECT o.object_id, o.object_type, o.label, d.depth
FROM descendants d
JOIN object o ON o.object_id = d.object_id
WHERE d.object_id != 'ROOT_ID'
ORDER BY d.depth, o.object_type;
```

Replace `child_id`/`parent_id` to walk the other direction (ancestors).

**For result pivoting** (multiple keys as columns), use conditional aggregation:

```sql
SELECT
  o.object_id,
  o.label,
  MAX(CASE WHEN r.key = 'completeness' THEN CAST(r.value AS REAL) END) AS completeness,
  MAX(CASE WHEN r.key = 'contamination' THEN CAST(r.value AS REAL) END) AS contamination,
  MAX(CASE WHEN r.key = 'GTDB_taxonomy' THEN r.value END) AS taxonomy
FROM object o
LEFT JOIN object_result r ON r.object_id = o.object_id
WHERE o.object_type = 'genome'
GROUP BY o.object_id, o.label
ORDER BY completeness DESC NULLS LAST;
```

---

## Presenting the query

Before running anything, show the SQL in a code block and briefly explain what it does:

> This query joins `object` with `object_result` to find genome objects where completeness > 90, ordered by completeness descending. Run it?

If the user says yes (or is in auto mode and the query is clearly correct), run it. If the query is complex or touches many rows, mention that first.

---

## Execution

```bash
sqlite3 -header -column "$DB" "$(cat <<'SQL'
<your query here>
SQL
)"
```

For larger results or when the user wants a file:

```bash
sqlite3 -header -separator $'\t' "$DB" "<query>" > /path/to/output.tsv
echo "Wrote N rows to output.tsv"
```

Count rows before fetching large result sets:

```bash
sqlite3 "$DB" "SELECT COUNT(*) FROM (...);"
```

If the result set is > 200 rows, write to TSV by default rather than printing inline.

---

## Handling no results

If the query returns 0 rows, do not silently accept it. Diagnose:

1. Check that the object type / key / role names match the spec exactly (case-sensitive)
2. Check that the IDs used in WHERE clauses actually exist:
   ```bash
   sqlite3 "$DB" "SELECT object_id FROM object WHERE object_id = 'suspected_id';"
   ```
3. Check whether CAST is needed for numeric comparisons
4. If the spec has no matching key or file role, say so explicitly

---

## Multi-question sessions

If the user asks follow-up questions, keep the database path in scope from the setup step. You don't need to re-read den.yaml for each question in the same session. Do re-read the spec tables if the user asks something that requires a key or role you haven't loaded yet.

---

## Saving results to a file

If the user says "save" or "export", write a TSV to `archive/agent/` with a datestamped name:

```bash
mkdir -p archive/agent
sqlite3 -header -separator $'\t' "$DB" "<query>" > "archive/agent/query_$(date +%Y%m%d_%H%M%S).tsv"
```

Tell the user the exact path of the output file.

---

## Rules

- **SELECT only.** Refuse any question that would require modifying data.
- **Show the SQL before running it.** Never silently execute.
- **Use spec descriptions, not assumptions.** If you don't know what a key or file role means, look at `object_result_spec` or `object_file_type_spec` first.
- **Cast values for numeric comparisons.** `object_result.value` is TEXT. Use `CAST(value AS REAL)`.
- **Mention NULLS LAST.** SQLite sorts NULLs before values by default; add `NULLS LAST` to ORDER BY when sorting numeric results descending.
- **Explain joins.** If the query involves 3+ tables, briefly note what each join does.
- **Don't invent column names.** If you're unsure, run `.schema <table>` first.

---

## What this skill does NOT do

- Modify data (INSERT/UPDATE/DELETE/DROP/ALTER)
- Run R code or gopheR functions
- Generate bundles or ingest data
- Access files registered in `object_file` — use `/gather` for file-content extraction
