# Amplicon workflow starter
# Run this after gopheR 0.6.0.9000 is installed and your den is set up.
#
# ORDER OF OPERATIONS:
#   1. Migrate existing den (one-time, skip for new dens)
#   2. Populate primer_set table
#   3. Create + ingest a bundle (asv_batch object, edges, workflows, files, results)
#   4. read_amplicon()
#   5. (optional) read_taxonomy(), read_clustering()

library(gopheR)
library(DBI)

# Point to your den
use_db("/path/to/your/project.den")

# =============================================================================
# STEP 1: Migrate an existing den (SKIP THIS if den was created with gopheR
# 0.6.1 or later -- the schema is already up to date)
# =============================================================================
# source(system.file("extdata", "migrate_amplicon.R", package = "gopheR"))

# =============================================================================
# STEP 2: Populate primer_set (required before any ASVs can be inserted)
# One row per amplicon region you use. primer_set_id must match asv_batch subtype.
# =============================================================================
con <- gopher_con()

DBI::dbExecute(con, "INSERT OR IGNORE INTO primer_set
  (primer_set_id, marker, region, forward_primer, reverse_primer, fwd_name, rev_name)
  VALUES ('V4', '16S', 'V4', 'GTGYCAGCMGCCGCGGTAA', 'GGACTACNVGGGTWTCTAAT', '515F', '806R')")

# Add other primer sets you use, e.g.:
# DBI::dbExecute(con, "INSERT OR IGNORE INTO primer_set
#   (primer_set_id, marker, region, forward_primer, reverse_primer, fwd_name, rev_name)
#   VALUES ('ITS2', 'ITS', 'ITS2', 'GTGAATCATCGAATCTTTGAA', 'GCTGCGTTCTTCATCGATGC', 'ITS3', 'ITS4')")

DBI::dbDisconnect(con)

# =============================================================================
# STEP 3: Create a bundle and ingest it
#
# The bundle needs (use fill-bundle agent or build manually):
#   object sheet:
#     - readset objects (one per sample, subtype = "V4") -- may already exist
#     - one asv_batch object (object_type = "asv_batch", subtype = "V4")
#   edge sheet:
#     - readset -> asv_batch  (edge_type = "derived_from")
#   workflow sheet:
#     - one workflow for the DADA2 run
#   object_file sheet:
#     - asv_batch | asv_fasta | /path/to/filtered_asvs.fasta | fasta
#   workflow_file sheet:
#     - dada2_workflow_id | abundance_matrix_raw | /path/to/seqtab_nochim.tsv | tsv
#     - dada2_workflow_id | abundance_matrix     | /path/to/filtered_counts.tsv | tsv
#   object_result sheet:
#     - asv_batch_id | dada2_workflow_id | total_asvs    | 8432  | (unit blank)
#     - asv_batch_id | dada2_workflow_id | filtered_asvs | 3201  | (unit blank)
#     - asv_batch_id | dada2_workflow_id | filter_threshold | 2x2 | (unit blank)
# =============================================================================
read_bundle("/path/to/your_amplicon_bundle.xlsx")

# =============================================================================
# STEP 4: Ingest ASV sequences and label mappings
#
# count_table: your filtered count TSV (col 1 = local ASV label, rest = samples)
# fasta_path:  filtered ASV FASTA (headers must match col 1 of count_table)
# asv_batch_id: the object_id you gave the asv_batch in the bundle
# workflow_id:  the DADA2 workflow_id from the bundle
# sample_map:  named vector if your sample column names != readset object IDs
#              e.g. c(S01 = "ARW_S01_R1", S02 = "ARW_S02_R1")
#              leave NULL if column names already match object IDs
# =============================================================================
read_amplicon(
  count_table  = "/path/to/filtered_counts.tsv",
  fasta_path   = "/path/to/filtered_asvs.fasta",
  asv_batch_id = "YOUR_ASV_BATCH_ID",
  workflow_id  = "YOUR_DADA2_WORKFLOW_ID",
  sample_map   = NULL,      # or c(local_name = "readset_object_id", ...)
  validate_only = TRUE      # flip to FALSE once validation passes
)

# =============================================================================
# STEP 5a (optional): Ingest taxonomy
#
# taxonomy_table: two-column TSV (col 1 = local ASV label or asv_id, col 2 = taxonomy string)
# asv_batch_id: provide if col 1 is local labels; omit if col 1 is already MD5 asv_ids
# =============================================================================
read_taxonomy(
  taxonomy_table = "/path/to/silva_taxonomy.tsv",
  workflow_id    = "YOUR_TAXONOMY_WORKFLOW_ID",
  asv_batch_id   = "YOUR_ASV_BATCH_ID",
  validate_only  = TRUE
)

# =============================================================================
# STEP 5b (optional): Ingest clustering
#
# clustering_output: two-column data frame or TSV (col 1 = member ASV, col 2 = representative ASV)
# Built from your DECIPHER workflow -- see AMPLICON_PLANNING.md
# =============================================================================
read_clustering(
  clustering_output = your_cluster_map,   # data frame from your clustering package
  workflow_id       = "YOUR_CLUSTER_WORKFLOW_ID",
  cluster_type      = "cluster97",
  primer_set_id     = "V4",
  asv_batch_id      = "YOUR_ASV_BATCH_ID",
  validate_only     = TRUE
)
