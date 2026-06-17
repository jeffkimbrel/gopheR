# Amplicon / ASV Schema Planning

**Status:** Implemented and real-data validated in gopheR 0.8.0 (V4 16S + 18S, ARW den, 2026-06-17)

---

## Current tables

Five tables exist in the `.den` schema, all wired into the gopheR workflow:

| Table | Purpose |
|---|---|
| `primer_set` | Reference table of primer pairs (`primer_set_id` is TEXT, e.g. `"V4"`) |
| `asv` | Global sequence dictionary — each unique amplicon sequence gets one row |
| `amplicon_asv` | Per-project mapping of local ASV labels → global `asv_id` |
| `asv_taxonomy` | Taxonomy assignments per ASV per classifier workflow |
| `asv_cluster` | Cross-study OTU clustering — maps `asv_id` → `cluster_id` at a given threshold |

---

## Core design vision

Individual studies generate amplicon data using their own local ASV naming (e.g. `ASV_001` from study A, `ASV_001` from study B — same label, potentially different sequences). When ingested into gopheR, sequences are deduplicated into the global `asv` table. The per-project label → global ID mapping lives in `amplicon_asv`, keyed to an `asv_batch` object (see below).

Periodically, all ASVs sharing the same primer set can be pooled and clustered (97%, 99%, or other thresholds) to produce a unified OTU table for cross-study ecology.

```
sample → readset → asv_batch:V4 ──────────────────────────────┐
                       ↓                                       ↓
                  workflow_file                          amplicon_asv
                (abundance matrix)              (local label → asv_id)
                                                               ↓
                                                   asv (global sequence dict)
                                                       ↓             ↓
                                                asv_taxonomy     asv_cluster
                                                               (→ cluster_id)
```

---

## `asv_batch` object type

**`asv_batch` is a new first-class object type** representing one round of ASV generation on a set of samples (e.g. a single DADA2 run). It is the provenance anchor for the amplicon layer.

**Object type:** `asv_batch`
**Subtypes:** the amplicon region — `V4`, `V3-V4`, `ITS2`, `18S`, etc.

The subtype IS the link to `primer_set`. The `asv_batch` subtype vocabulary and the `primer_set` table share the same identifiers — adding a new primer region means adding a new `asv_batch` subtype AND a matching `primer_set` row. There is no separate FK needed; the convention enforces the relationship.

**What an `asv_batch` object carries:**
- Edges to source readsets: `asv_batch derived_from readset`
- `object_file`: filtered ASV FASTA (`asv_fasta`) — the sequence representation of this batch
- `object_result` entries: ASV counts, filter threshold, depth stats
- Its subtype encodes which primer set / amplicon region was used

The abundance matrix and raw count table are `workflow_file` entries on the DADA2 workflow that produced this batch, not on the `asv_batch` object itself. The phylogenetic tree is a `workflow_file` on a separate tree-building workflow.

**`amplicon_asv.amplicon_object_id` points to an `asv_batch` object.** One `asv_batch` per DADA2 run (which may cover many samples). Local ASV labels are scoped to this batch; global `asv_id`s are shared across all batches.

**Markers we use:**

| Subtype | Marker | Notes |
|---|---|---|
| `V4` | 16S V4 | Primary |
| `V3-V4` | 16S V3-V4 | |
| `ITS2` | ITS2 | Fungi |
| `18S` | 18S | Legacy |

---

## Abundance data

Abundance matrices (samples × ASVs) are too large for long-format DB storage (20,000 ASVs × 1,000 samples = 20M rows). They stay as external TSV files. The database stores the path via `workflow_file` with `file_role = "abundance_matrix"` on the `asv_batch` workflow.

**File inventory per `asv_batch`:**

| File | Type | `file_role` | Notes |
|---|---|---|---|
| Filtered ASV FASTA | `object_file` on `asv_batch` | `asv_fasta` | Defines the sequences in this batch — analogous to genome FASTA on a MAG object |
| Filtered abundance matrix | `workflow_file` on DADA2 workflow | `abundance_matrix` | Primary input for `merge_abundances()` |
| Raw (unfiltered) abundance matrix | `workflow_file` on DADA2 workflow | `abundance_matrix_raw` | Preserved on disk; sequences not ingested to `asv` table |
| Phylogenetic tree | `workflow_file` on tree-building workflow | `phylogenetic_tree` | Separate workflow (FastTree, IQ-TREE, etc.); if cross-study, edges back to source `asv_batch` objects |

`object_file_type_spec` needs `asv_batch → asv_fasta`. `workflow_file_type_spec` needs `abundance_matrix`, `abundance_matrix_raw`, `phylogenetic_tree`. Additional roles (rarefaction tables, etc.) can be added per project.

A `merge_abundances()` function (or agent task) can reconstruct a unified cross-study matrix on demand:
1. Accept `primer_set_id` (= `asv_batch` subtype) + `cluster_type` as inputs
2. Find all `asv_batch` objects with that subtype
3. Locate each one's abundance TSV via `workflow_file`
4. Read each TSV, remap local labels → global `asv_id` → `cluster_id` via join tables
5. Sum abundances for ASVs that collapsed into the same OTU
6. Return one merged matrix (all samples × OTU clusters)

---

## Ingest functions

Amplicon data does not fit the Excel bundle format — it comes from machine-generated tool output at scales that don't belong in spreadsheets. Dedicated ingest functions read tool output directly:

| Function | Reads | Populates | Status |
|---|---|---|---|
| `read_amplicon(count_table, asv_batch_id, workflow_id, fasta_path, sample_map)` | DADA2 count TSV + FASTA | `asv`, `amplicon_asv`, `workflow_file` | **Done** |
| `read_taxonomy(taxonomy_table, workflow_id, asv_batch_id)` | SILVA/RDP two-col TSV | `asv_taxonomy` | **Done** |
| `read_clustering(clustering_output, workflow_id, cluster_type, primer_set_id)` | Two-col TSV (member, rep) or VSEARCH UC | `asv_cluster`, triggers `trim_clustering()` | **Done** |
| `trim_clustering(primer_set_id, cluster_type, keep_workflow_id)` | -- | Archives old runs to `archive/clustering/*.json`, removes from DB | **Done** |
| `restore_clustering(workflow_id)` | JSON archive | Re-inserts rows into `asv_cluster` | **Done** |
| `merge_abundances(primer_set_id, cluster_type)` | abundance TSVs via `workflow_file` | returns merged matrix in R | **Roadmap** |

The regular Excel bundle handles everything upstream: sample objects, readset objects, `asv_batch` objects, their edges, and workflow metadata. These functions take over once the provenance scaffolding is in place.

**Agent + functions split:**

The fill-bundle agent handles Stages 1 and 3 as usual — `asv_batch` is just a new object type it needs to know about:
- Stage 1: `asv_batch` objects (with correct subtype), edges to readsets, workflow metadata for DADA2 / taxonomy classifiers / tree building / clustering runs
- Stage 3: `object_file` entries (filtered FASTA), `workflow_file` entries (abundance matrices, tree), `object_result` entries on `asv_batch` (total ASVs, post-filter ASV count, median depth — from DADA2 summary output)

The user then runs the sequence data functions directly — no inference needed, arguments are explicit:

```r
read_amplicon("dada2_seqtab.tsv",
              asv_batch_id = "ARW_V4_batch1",
              workflow_id  = "dada2_ARW_2024-06")

read_taxonomy("silva_taxonomy.tsv",
              workflow_id = "silva_v138_2024-01")

read_clustering("vsearch_clusters.txt",
                workflow_id = "vsearch_97_2024-06")
```

The fill-bundle skill will need amplicon-specific additions: the `asv_batch` type and subtypes, expected `workflow_file` roles, and what DADA2 summary output looks like for `object_result` mapping.

---

## Taxonomy as a result

Taxonomy is a workflow output, not a property of the ASV. The reference database changes over time, and you may want multiple classifiers (SILVA, RDP, QIIME2 naive Bayes) stored side by side — the same pattern as GTDB vs CheckM2 on genome objects.

Since taxonomy is likely the only result type ASVs will have (chimera status handled upstream; prevalence computable from abundance matrix), a dedicated `asv_taxonomy` table is cleaner than generic key-value:

```sql
CREATE TABLE asv_taxonomy (
  asv_id      TEXT NOT NULL REFERENCES asv(asv_id) ON DELETE CASCADE,
  workflow_id TEXT NOT NULL REFERENCES workflow(workflow_id),
  taxonomy    TEXT NOT NULL,
  PRIMARY KEY (asv_id, workflow_id)
);
```

Cluster taxonomy is derived from `asv_taxonomy` on the representative ASV — no separate cluster taxonomy table needed.

---

## Schema changes required

### 1. Add `primer_set_id` to `asv`

```sql
ALTER TABLE asv ADD COLUMN primer_set_id TEXT NOT NULL REFERENCES primer_set(primer_set_id);
```

Partitions the global sequence dictionary by amplicon region. Clustering is only valid within a single `primer_set_id`.

---

### 2. Fix `asv_cluster` — remove hardcoded CHECK, add versioning and representative

**Problem:** `CHECK (cluster_type IN ('cluster97', 'cluster99'))` is too restrictive. Primary key `(asv_id, cluster_type)` excludes `workflow_id`, so re-clustering overwrites prior results. No representative is tracked.

**Fix:** Remove the CHECK constraint; add `workflow_id` to the PK; add `is_representative`:

```sql
CREATE TABLE asv_cluster (
  asv_id            TEXT NOT NULL REFERENCES asv(asv_id) ON DELETE CASCADE,
  cluster_type      TEXT NOT NULL,
  cluster_id        TEXT NOT NULL,
  workflow_id       TEXT NOT NULL REFERENCES workflow(workflow_id),
  is_representative INTEGER NOT NULL DEFAULT 0,
  PRIMARY KEY (asv_id, cluster_type, workflow_id)
);
```

Each clustering workflow produces its own rows. The most recent workflow per primer set is the live view; older runs are archived (see below).

---

### 3. Add `asv_taxonomy` table

See "Taxonomy as a result" above.

---

### 4. Add `asv_batch` object type and all spec entries

In a bundle or via SQL:

**`object_type`:** `asv_batch`

**`object_subtype`:** `V4`, `V3-V4`, `ITS2`, `18S` — must match `primer_set_id` values in `primer_set` table

**`edge_spec`:**
- `asv_batch derived_from readset`

**`object_file_type_spec`:**
- `asv_batch → asv_fasta` (filtered ASV sequences)

**`workflow_file_type_spec`:**
- `abundance_matrix` (filtered count table)
- `abundance_matrix_raw` (unfiltered DADA2 output)
- `phylogenetic_tree` (Newick tree from FastTree/IQ-TREE/etc.)

**`object_result_spec`** (for `asv_batch`):
- `total_asvs` — total ASVs before prevalence filtering
- `filtered_asvs` — ASVs retained after filtering
- `filter_threshold` — e.g. `"2x2"`, `"5x5"` (prevalence filter applied)
- `median_depth` — median per-sample sequencing depth (optional)

---

## Clustering archive pattern

Old clustering runs are archived to disk rather than kept in the database indefinitely. The DB holds only the most recent clustering per primer set; the archive holds the rest for reproducibility.

**Automatic behavior:** When a new clustering run is ingested, `trim_clustering()` runs automatically (or is offered). It:
1. Identifies all clustering workflows for the same primer set except the most recent
2. Serializes their `asv_cluster` rows to `archive/clustering/clustering_{primer_set_id}_{workflow_id}.json`
3. Removes those rows from `asv_cluster`

**`den.yaml`** gains an `archive:` section:

```yaml
database: myproject.den
archive:
  clustering: archive/clustering/
```

**`initialize_den()`** creates `archive/clustering/` alongside `archive/dens/`, `archive/bundles/`, `archive/agent/`.

**`restore_clustering(workflow_id)`** reads a JSON archive file and re-inserts rows — straightforward since the schema is simple.

**Result:** DB stays bounded — always one clustering run per primer set in the live table.

---

## Readset subtype redesign

**Principle:** The readset subtype answers "what can I group this with?" — not "how was it made?" Raw reads don't go in gopheR; the end product (assembled contigs, amplicon sequences) is what matters. Technology is metadata.

**Subtype encodes library type / biological target:**

| Subtype | Target | Notes |
|---|---|---|
| `readset:shotgun` | Whole metagenome | |
| `readset:16S_V4` | 16S V4 | Primary bacterial |
| `readset:16S_V3-V4` | 16S V3-V4 | |
| `readset:16S_FL` | 16S full-length | PacBio/Nanopore long-read |
| `readset:ITS2` | ITS2 | Fungi |
| `readset:18S` | 18S | Legacy |
| `readset:WANDA` | SSU rRNA (AMF) | WANDA primers for arbuscular mycorrhizal fungi |

PacBio 16S and Illumina 16S both produce `readset:16S_V4` — they belong in the same `asv_batch`. The technology is irrelevant to grouping. Each subtype also has a matching `primer_set` row.

**Technology goes in `object_result` on the readset:**
- `technology` = `illumina`, `pacbio`, `nanopore`
- `layout` = `paired_end`, `single_end`, `long_read`

**Migration:** Existing ARW readsets in the DB are currently `readset:paired_end`. These need to be updated to `readset:shotgun`. Simple UPDATE on the one production database.

---

## Implementation notes

**`asv_id` format:** MD5 hash of the uppercase, gap-removed sequence only. 32-character hex string. Compatible with QIIME2's ASV hashing convention. Do not include `primer_set_id` in the hash — the sequence alone is the identity.

**`read_amplicon()` input format:** A standard tibble with ASV names in column 1 (not rownames), sample names as remaining column headers. This is the user's post-DADA2 processed table, not the raw `seqtab.nochim`. Sample column names should be readset object IDs — this ensures `merge_abundances()` produces an unambiguous merged matrix across studies without a separate name-mapping step. If local sample names differ from object IDs, `read_amplicon()` will accept an optional `sample_map` named vector.

**Schema migration:** Only one production database exists. Migration can be done by direct SQL on that database or by re-creating it from existing bundles against an updated `starter_db.den`. No formal migration function needed yet.

**GopherScout integration:** Deferred. Build gopheR amplicon support completely first. GopherScout integration follows once the R layer is stable.

**Agent skill — extend fill-bundle, not a separate skill:** The amplicon workflow still produces bundles for objects, workflows, edges, files, and results — the same work fill-bundle already does. A separate skill would duplicate ~90% of fill-bundle. Instead, extend fill-bundle with amplicon-specific knowledge: the `asv_batch` type and subtypes, expected file roles, DADA2 summary output for `object_result` mapping, and sample name → object ID resolution. At the end of Stage 3 the agent generates the `read_amplicon()` / `read_taxonomy()` / `read_clustering()` calls for the user to run directly.

**First test dataset — partial object overlap:** The first amplicon dataset will have some samples already in the DB (ingested via metagenomics bundles) and new amplicon-only samples not yet in the DB. The `read-amplicon` agent must handle this explicitly:
1. Query DB for existing sample objects
2. Cross-reference against ASV table sample column names
3. Split into: already in DB (map directly) vs amplicon-only (need new sample + readset objects)
4. For new samples: run a Stage 1 bundle first to create them
5. Check whether metagenomics samples already have readsets — amplicon sequencing is a separate library and may need new readset objects even for existing samples
6. Create `asv_batch` with edges to all readsets
7. Generate `read_amplicon()` call with confirmed `sample_map`

---

## Open questions

- `cluster_type_spec` table vs. free text: free text is probably fine given the small number of real-world values (`cluster97`, `cluster99`). Defer until there's a reason to constrain.
- `cluster_result` table: not needed -- cluster size and other per-cluster stats are computable from `asv_cluster` on demand.
- **Database size:** Rough estimate for 5 studies, 50K unique ASVs, 5 re-clusters, 2 taxonomy classifiers: ~85-110MB with SQLite overhead. Addressed by the clustering archive pattern, which keeps `asv_cluster` bounded to one run per primer set at a time.

---

See [ROADMAP.md](ROADMAP.md) for amplicon roadmap items (`merge_abundances`, fill-bundle amplicon extension).
