# Amplicon / ASV Schema

**Status:** Core schema validated on real data (V4 16S + 18S, ARW den, 2026-06-17). Schema redesigned 2026-06-17 to align with the mature gopheR object/edge/result model.

---

## Core design

Amplicon sequencing produces ASV datasets from sets of readsets, processed through a denoising pipeline (DADA2, Deblur, etc.). gopheR captures this at two levels:

1. **Provenance layer** — standard objects and edges: who produced what from what
2. **Global sequence layer** — special tables for cross-study sequence identity, taxonomy, and clustering

```
sample → readset ──[inferred_from]──→ amplicon:V4
              ↓ (optional)
         [sequenced_in]
              ↓
         set (Illumina run)
```

---

## The `amplicon` object type

**`amplicon` is a first-class object** representing one ASV generation run on a set of samples — the output of a denoising pipeline applied to a defined set of readsets and a single primer set.

**Object type:** `amplicon`
**Subtypes:** the amplicon region — `V4`, `V3-V4`, `ITS2`, `18S`, `WANDA`, etc.

The subtype matches the `primer_set` table's `primer_set_id` (TEXT primary key, e.g. `"V4"`). Adding a new primer region means adding a new `amplicon` subtype AND a matching `primer_set` row.

**Files on the amplicon object (`object_file`):**

| `file_role` | Description |
|---|---|
| `asv_fasta` | Filtered ASV sequences (FASTA) |
| `abundance_matrix` | Sample × ASV count table (TSV) |
| `abundance_matrix_raw` | Unfiltered DADA2 seqtab output |
| `phylogenetic_tree` | Newick tree (FastTree, IQ-TREE, etc.) |
| `alignment` | Multiple sequence alignment (FASTA) |
| `phyloseq_rds` | phyloseq object (R RDS file) |
| `amplify_provenance` | amplify provenance JSON (if used) |
| `amplify_experiment` | amplify experiment RDS object (if used) |

**Results on the amplicon object (`object_result`):**

| Key | Description |
|---|---|
| `total_asvs` | ASVs before prevalence filtering |
| `filtered_asvs` | ASVs retained after filtering |
| `filter_threshold` | Prevalence filter applied (e.g. `"2x2"`) |
| `median_depth` | Median per-sample sequencing depth |

---

## Edges

### Core provenance: `amplicon inferred_from readset`

```
child_id = amplicon_id
parent_id = readset_id
edge_type = inferred_from
workflow_id = dada2_workflow_id
```

One row per readset that contributed to this amplicon. This is the primary provenance link.

### Optional Illumina run grouping: `readset sequenced_in set`

```
child_id = readset_id
parent_id = set_id   (a set representing one Illumina run)
edge_type = sequenced_in
workflow_id = fastq_info_workflow_id (optional)
```

Edge results on `sequenced_in` (from fastq header parsing):

| Key | Description |
|---|---|
| `flowcell_id` | Illumina flowcell identifier |
| `instrument_id` | Instrument serial / machine ID |
| `lane` | Lane number (may differ per readset) |

### Collapsed FASTQ handling: `readset derived_from readset`

Some sequencing centers collapse multiple Illumina runs into a single FASTQ when read depth is insufficient. When detected (via per-read header parsing), the file is split back into per-run readsets:

```
split_readset_1 ──[derived_from]──→ main_readset (original collapsed FASTQ)
split_readset_2 ──[derived_from]──→ main_readset

amplicon:V4 ──[inferred_from]──→ split_readset_1  (the one kept, e.g. more reads)
```

`split_readset_2` exists in the DB as a derived readset that did not contribute to the amplicon — honest provenance of a processing decision.

---

## Two usage profiles

### Simple profile (most users)

One Illumina run, or don't need to track runs:

```
readset_1 ──┐
readset_2 ──┤ [inferred_from] ──→ amplicon:V4
readset_3 ──┘
```

No sets, no `sequenced_in` edges. Works exactly like metagenomics provenance.

### Advanced profile (amplify users)

Multiple Illumina runs, tracked explicitly. amplify's `batches` map to sets; `fastq_info` stage output populates edge results:

```
readset_1 ──[sequenced_in, flowcell=HX1]──→ set z16S_run1
readset_2 ──[sequenced_in, flowcell=HX1]──→ set z16S_run1
readset_3 ──[sequenced_in, flowcell=HX2]──→ set z16S_run2
readset_4 ──[sequenced_in, flowcell=HX2]──→ set z16S_run2

readset_1 ──┐
readset_2 ──┤
readset_3 ──┤ [inferred_from] ──→ amplicon:V4
readset_4 ──┘
```

The `sequenced_in` edges are organizational metadata running in parallel to the provenance chain — they don't sit between readsets and the amplicon in the provenance path.

**amplify provenance JSON** (`amplify_provenance` file role on the amplicon) contains everything needed to reconstruct a bundle automatically: batch names, fastq paths, software versions, parameters, primer sequences, timestamps. An agent can parse this file to fill the bundle with minimal user input.

---

## Global sequence layer

Amplicon sequences are deduplicated globally across all projects into a set of special tables. These are not first-class objects but power cross-study queries and `merge_abundances()`.

### Tables

| Table | Purpose |
|---|---|
| `primer_set` | Reference table of primer pairs; `primer_set_id TEXT PRIMARY KEY` = region string (e.g. `"V4"`) |
| `asv` | Global sequence dictionary: `asv_id` (MD5 hash), `sequence`, `primer_set_id` |
| `amplicon_asv` | Per-project label → global `asv_id` mapping, scoped to one `amplicon` object |
| `asv_taxonomy` | Taxonomy per ASV per classifier workflow: `(asv_id, workflow_id)` PK |
| `asv_cluster` | OTU assignments: `asv_id → cluster_id` per `cluster_type` + `workflow_id` |

### `asv_id` format

MD5 hash of the uppercase, gap-removed sequence only. 32-character hex string. Compatible with QIIME2's ASV hashing convention. `primer_set_id` is NOT included in the hash — the sequence alone is the identity.

### `read_amplicon()` input

A tab-delimited count table: ASV labels in column 1, sample names as remaining column headers. This is the post-DADA2 processed table (post-chimera removal, post-length filtering). Sample column names should be readset object IDs or mapped via `sample_map`.

---

## Ingest functions

| Function | Reads | Populates |
|---|---|---|
| `read_amplicon(count_table, amplicon_id, workflow_id, fasta_path, sample_map)` | DADA2 count TSV + FASTA | `asv`, `amplicon_asv`, `workflow_file` |
| `read_taxonomy(taxonomy_table, workflow_id, amplicon_id)` | Two-col TSV (label/hash + taxonomy string) | `asv_taxonomy` |
| `read_clustering(clustering_output, workflow_id, cluster_type, primer_set_id)` | VSEARCH UC or two-col TSV | `asv_cluster`; calls `trim_clustering()` |
| `trim_clustering(primer_set_id, cluster_type, keep_workflow_id)` | — | Archives old runs to `archive/clustering/`, removes from DB |
| `restore_clustering(workflow_id)` | JSON archive | Re-inserts rows into `asv_cluster` |
| `merge_abundances(primer_set_id, cluster_type)` | abundance TSVs via `object_file` | Returns merged matrix in R (**roadmap**) |

The provenance scaffolding (amplicon objects, edges, workflows, files, results) is built by the fill-bundle agent. These functions take over once that scaffolding is in place — their arguments are explicit, no inference needed.

**Note:** `read_amplicon()` currently uses the parameter name `asv_batch_id` — this should be updated to `amplicon_id` to match the renamed object type.

---

## Clustering archive pattern

Only the most recent clustering run per primer set is kept in the live `asv_cluster` table. Older runs are archived to `archive/clustering/` as JSON and removed from the DB.

`trim_clustering()` runs automatically after `read_clustering()` (when `trim = TRUE`). `restore_clustering(workflow_id)` reads a JSON archive file and re-inserts its rows.

---

## Readset subtypes

The readset subtype encodes the library type / biological target — what the reads can be grouped with for denoising, not how they were made.

| Subtype | Target |
|---|---|
| `shotgun` | Whole metagenome |
| `V4` | 16S V4 |
| `V3-V4` | 16S V3-V4 |
| `16S_FL` | 16S full-length (PacBio/Nanopore) |
| `ITS2` | ITS2 (fungi) |
| `18S` | 18S |
| `WANDA` | SSU rRNA (arbuscular mycorrhizal fungi) |

Technology and layout go in `object_result` on the readset: `technology` = `illumina`/`pacbio`/`nanopore`; `layout` = `paired_end`/`single_end`/`long_read`.

---

## Open questions

- `cluster_type_spec` table vs. free text: free text is probably fine (`cluster97`, `cluster99`). Defer until there's a reason to constrain.
- GopherScout integration: `amplicon` objects appear as first-class objects in the Objects tab and graph automatically. Additional surfacing of ASV counts and taxonomy summaries in the detail panel is deferred — see ROADMAP.md.

---

See [ROADMAP.md](ROADMAP.md) for planned features (`merge_abundances`, GopherScout amplicon browsing).
