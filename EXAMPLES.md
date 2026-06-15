
# gopheR — Worked Examples

Real-world bioinformatics scenarios and how to model them in gopheR. For schema reference and design decisions, see [DETAILS.md](DETAILS.md).

---

## Example 1: MetaWRAP coassembly → binning → quality filtering

**The scenario:** You ran MetaWRAP on 5 readsets. MetaSPAdes built one coassembly. CONCOCT, MaxBin2, and MetaBat2 each produced bins, which MetaWRAP refined into a single bin set. You kept only MiMAG medium-quality or better MAGs. CheckM assessed completeness and contamination.

### Objects to add

| object_id | object_type | object_subtype | label | study_id |
|---|---|---|---|---|
| ARW_ASM_01 | assembly | metagenome | ARW coassembly | ARW_study |
| ARW_MAG_001 | genome | MAG | ARW MAG 001 | ARW_study |
| ARW_MAG_002 | genome | MAG | ARW MAG 002 | ARW_study |
| … | … | … | … | … |

Readsets (`ARW_RS_01` through `ARW_RS_05`) are assumed to already be in the database.

### Workflows to add

Use separate workflows for each distinct processing step so you can attribute results and files to the right step:

| workflow_id | workflow_name | workflow_version | workflow_date |
|---|---|---|---|
| ARW_W_coassembly | MetaSPAdes | 3.15.5 | 2024-03-10 |
| ARW_W_binning | MetaWRAP | 1.3.2 | 2024-03-12 |
| ARW_W_checkm | CheckM2 | 1.0.2 | 2024-03-14 |

**Why three workflows?** You can then attribute the assembly edges to `ARW_W_coassembly`, the `binned_from` edges to `ARW_W_binning`, and the CheckM results to `ARW_W_checkm`. If you re-run CheckM2 next year with updated databases, you add a new result row attributed to a new workflow — the history is preserved.

### Edges to add

**Coassembly assembled from each readset** (5 edges):

| child_id | edge_type | parent_id | workflow_id |
|---|---|---|---|
| ARW_ASM_01 | assembled_from | ARW_RS_01 | ARW_W_coassembly |
| ARW_ASM_01 | assembled_from | ARW_RS_02 | ARW_W_coassembly |
| … | … | … | … |

**Each MAG binned from the assembly** (one per MAG):

| child_id | edge_type | parent_id | workflow_id |
|---|---|---|---|
| ARW_MAG_001 | binned_from | ARW_ASM_01 | ARW_W_binning |
| ARW_MAG_002 | binned_from | ARW_ASM_01 | ARW_W_binning |
| … | … | … | … |

### Results to add

Attach quality metrics to each MAG, attributed to the CheckM workflow:

| object_id | key | value | workflow_id |
|---|---|---|---|
| ARW_MAG_001 | completeness | 94.2 | ARW_W_checkm |
| ARW_MAG_001 | contamination | 1.8 | ARW_W_checkm |
| ARW_MAG_001 | MiMAG_quality | high | ARW_W_checkm |
| ARW_MAG_002 | completeness | 72.1 | ARW_W_checkm |
| ARW_MAG_002 | contamination | 3.4 | ARW_W_checkm |
| ARW_MAG_002 | MiMAG_quality | medium | ARW_W_checkm |
| … | … | … | … |

**Why `MiMAG_quality` as a result and not as `object_subtype`?**
Quality assessments get updated. When CheckM2 v2 comes out with a new database, you run it again and add a new result row attributed to a new workflow. The history of "what did we think the quality was at each point in time" is preserved. If you baked quality into `object_subtype = 'MAG_MQ'`, you'd have to update the object itself — and you'd lose history.

**What about the MAGs that didn't pass MiMAG filtering?** Don't add them as objects. They're captured in the MetaWRAP output directory (see Files below). If you later want to rescue a borderline bin, you can add it then.

### Files to add

Point to the MetaWRAP refined bin directory so the intermediate per-binner outputs are still findable, without cluttering the database with every intermediate bin:

| object/workflow | file_role | file_path |
|---|---|---|
| ARW_W_binning (workflow_file) | metawrap_output_dir | /data/ARW/metawrap/bin_refinement/ |
| ARW_MAG_001 (object_file) | genome_fasta | /data/ARW/mags/ARW_MAG_001.fa |
| ARW_MAG_001 (object_file) | protein_fasta | /data/ARW/mags/ARW_MAG_001.faa |
| … | … | … |

**Why a workflow file for the directory?** The intermediate bins from CONCOCT, MaxBin2, and MetaBat2 are all still inside that MetaWRAP output directory. You don't need objects for them, but you want to be able to find them later. A `workflow_file` pointing to the directory is the right level of tracking.

---

## Example 2: Mapping 5 readsets to a MAG collection

**The scenario:** You have 5 readsets and 75 MAGs. You mapped each readset individually against the full MAG collection using CoverM. You want to record the mapping relationships and — optionally — per-readset × per-MAG coverage data.

How much to store in the database is a judgment call. You could store raw numbers (`mean_coverage`, `percent_mapped`), an interpreted presence call (`detected: TRUE`), both, or just a pointer to the CoverM TSV on disk. The structure supports any of these; what matters is that the edges exist to hang the data on. Below are two approaches depending on how many edges you want to create.

---

### Approach A: Provenance only — simple, manual-friendly

Use this when you need to record that mapping happened and where the results live, but you don't need to query coverage per-MAG from inside GopherScout.

**MAGset object** (one row):

| object_id | object_type | label | study_id |
|---|---|---|---|
| ARW_MAGSET_01 | set | ARW MAG collection v1 | ARW_study |

**Workflows** — one per readset mapping run, plus a separate workflow for the presence-calling step. Separating them matters: the detection workflow name encodes the criteria, and if you re-call presence later with a different threshold you add a new detection workflow and new edge_results without touching the original mapping records.

| workflow_id | workflow_name | workflow_version | workflow_date |
|---|---|---|---|
| ARW_W_map_RS01 | CoverM | 0.7.0 | 2024-04-01 |
| ARW_W_map_RS02 | CoverM | 0.7.0 | 2024-04-01 |
| … | … | … | … |
| ARW_W_detect_v1 | presence_call_1x_cov_25pct_breadth | — | 2024-04-02 |

The detection workflow name encodes the criteria: ≥1x mean coverage across ≥25% genome breadth. Change the threshold later? Add `ARW_W_detect_v2` — the old calls stay attributed to v1.

**5 edges** — one per readset → MAGset:

| child_id | edge_type | parent_id | workflow_id |
|---|---|---|---|
| ARW_RS_01 | mapped_to | ARW_MAGSET_01 | ARW_W_map_RS01 |
| ARW_RS_02 | mapped_to | ARW_MAGSET_01 | ARW_W_map_RS02 |
| … | … | … | … |

**Workflow files** — one coverage TSV per readset mapping workflow:

| workflow_id | file_role | file_path |
|---|---|---|
| ARW_W_map_RS01 | coverage_tsv | /data/ARW/coverm/RS01_coverage.tsv |
| ARW_W_map_RS02 | coverage_tsv | /data/ARW/coverm/RS02_coverage.tsv |
| … | … | … |

**MAGset membership** — 75 `member_of` edges. These are also onerous to enter by hand; generate them programmatically from a database query (see Approach B below for the pattern):

| child_id | edge_type | parent_id |
|---|---|---|
| ARW_MAG_001 | member_of | ARW_MAGSET_01 |
| ARW_MAG_002 | member_of | ARW_MAGSET_01 |
| … (75 rows) | … | … |

**What you can do with this:** Provenance is intact. GopherScout can show that each readset was mapped to the collection. Detailed per-MAG numbers are findable via the workflow file pointers. If you later want per-MAG queryability, you can add the individual edges and results at that point.

---

### Approach B: Full per-MAG coverage — programmatically generated

Use this when you want to query coverage per readset × MAG from GopherScout or in SQL. The 375-edge model is data-model-correct; the key is that you **never fill this in by hand** — you generate it from CoverM's output.

Everything from Approach A applies, except instead of 5 readset → MAGset edges, you create 375 readset → MAG edges with edge_results:

```r
library(gopheR)
library(dplyr)
library(readr)

# Load CoverM output (columns: Genome, Sample, Mean, Covered_Fraction, ...)
coverm <- read_tsv("/data/ARW/coverm/all_samples_coverage.tsv")

# Build edge rows
edges <- coverm |>
  transmute(
    child_id  = sample_to_readset_id(Sample),   # your ID mapping
    edge_type = "mapped_to",
    parent_id = genome_to_mag_id(Genome),        # your ID mapping
    workflow_id = sample_to_workflow_id(Sample)
  )

# Build edge_result rows — raw numbers, a presence call, or both are all valid
# If storing a presence call, attribute it to the detection workflow, not the mapping workflow
edge_results <- bind_rows(
  coverm |> transmute(child_id = sample_to_readset_id(Sample), parent_id = genome_to_mag_id(Genome),
                      workflow_id = sample_to_workflow_id(Sample),
                      key = "mean_coverage", value = as.character(Mean)),
  coverm |> mutate(detected = Mean >= 1 & Covered_Fraction >= 0.25) |> filter(detected) |>
            transmute(child_id = sample_to_readset_id(Sample), parent_id = genome_to_mag_id(Genome),
                      workflow_id = "ARW_W_detect_v1",
                      key = "detected", value = "TRUE")
)

# Write bundle programmatically (omitting other sheets for brevity)
write_bundle("mapping_bundle.xlsx")
# ... then fill the edge and edge_result sheets from the data frames above
# or use openxlsx directly to write the sheets
```

**MAGset membership** — same pattern: query the database for all MAGs in the study, generate the 75 `member_of` rows:

```r
con <- gopher_con()
mags <- dbGetQuery(con, "SELECT object_id FROM object WHERE object_type = 'genome' AND study_id = 'ARW_study'")
dbDisconnect(con)

member_edges <- mags |>
  mutate(edge_type = "member_of", parent_id = "ARW_MAGSET_01")
```

---

### Which approach to use?

| | Approach A | Approach B |
|---|---|---|
| Bundle rows to create | ~85 (5 edges + 75 member_of + 5 workflow files) | ~375 edges + N edge_results + 75 member_of |
| Manual entry feasible? | Yes (with care on the member_of rows) | No — generate programmatically from tool output |
| Per-MAG data queryable in GopherScout? | No — lives in TSV files | Yes |
| Provenance intact? | Yes (readset → set) | Yes (readset → each MAG) |

Approach A is easier to set up. Approach B gives you more to query but requires programmatic bundle generation — not something you fill in by hand. Choose based on whether you need the per-MAG data queryable in the database or are happy loading the TSV in R when you need it.

---

## Modeling principles illustrated by these examples

**Capture what's stable; let workflows carry the rest.**
Intermediate bins from three binners are not stable outputs — they're working material. The final MAGs are stable. The intermediate work lives in the workflow file pointer.

**Append-only results enable re-analysis.**
Re-running CheckM with a new database? Add new result rows attributed to a new workflow. The old assessment stays in the database. GopherScout can show the history.

**Sets are for display; edges carry the data.**
The MAGset groups the collection for browsing. The `mapped_to` edges carry the actual per-readset provenance. Per-readset data needs to live on edges between the readset and the individual MAG — it can't be collapsed onto the set.

**When the row count is too high for manual entry, generate programmatically.**
Bulk tool outputs (coverage TSVs, taxonomy tables, assembly stats) can be pivoted directly into bundle rows from R. The Excel workflow is for human-scale additions; large imports are better scripted.

**Edge direction: child IS edge_type OF parent.**
- "assembly is assembled_from readset" ✓
- "MAG is binned_from assembly" ✓
- "readset is mapped_to MAG" ✓
- "MAG is member_of set" ✓
