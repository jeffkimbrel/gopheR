# Add missing spec entries to support example data

library(gopheR)
library(DBI)

# Set database path
options(gopheR.db_path = "inst/extdata")
options(gopheR.db_file = "starter_db.den")

# Get connection using gopheR helper
con <- gopher_con()

# ==============================================================================
# ADD MISSING RESULT KEYS
# ==============================================================================

# Check what's already there
existing_keys <- dbGetQuery(con, "SELECT DISTINCT key FROM object_result_spec")$key
cat("Existing keys:", paste(existing_keys, collapse = ", "), "\n\n")

# Keys we need for our example
new_keys <- data.frame(
  object_type = c(
    "sample", "sample",
    "readset", "readset",
    "assembly",
    "genome", "genome"
  ),
  key = c(
    "pH", "temperature",
    "read_pairs", "total_bases",
    "total_length",
    "genome_size", "GTDB_taxonomy"
  ),
  value_type = c(
    "real", "real",
    "integer", "integer",
    "integer",
    "integer", "text"
  ),
  description = c(
    "Sample pH measurement",
    "Sample temperature in Celsius",
    "Number of read pairs in readset",
    "Total sequencing bases in readset",
    "Total assembly length in base pairs",
    "Genome size in base pairs",
    "GTDB taxonomy classification string"
  ),
  unit = c(
    NA, "°C",
    NA, "bp",
    "bp",
    "bp", NA
  ),
  stringsAsFactors = FALSE
)

# Only insert keys that don't exist
for (i in 1:nrow(new_keys)) {
  key_exists <- dbGetQuery(con,
    "SELECT COUNT(*) as n FROM object_result_spec WHERE object_type = ? AND key = ?",
    params = list(new_keys$object_type[i], new_keys$key[i]))$n > 0

  if (!key_exists) {
    dbExecute(con,
      "INSERT INTO object_result_spec (object_type, key, value_type, description, unit) VALUES (?, ?, ?, ?, ?)",
      params = list(
        new_keys$object_type[i],
        new_keys$key[i],
        new_keys$value_type[i],
        new_keys$description[i],
        new_keys$unit[i]
      ))
    cat("✓ Added key:", new_keys$key[i], "for", new_keys$object_type[i], "\n")
  }
}

# ==============================================================================
# ADD MISSING FILE ROLES
# ==============================================================================

existing_roles <- dbGetQuery(con, "SELECT DISTINCT file_role FROM object_file_type_spec")$file_role
cat("\nExisting file roles:", paste(existing_roles, collapse = ", "), "\n\n")

# File roles we need
new_file_roles <- data.frame(
  object_type = c(
    "genome", "genome"
  ),
  file_role = c(
    "protein_fasta", "annotation_gff"
  ),
  description = c(
    "Protein sequences in FASTA format",
    "Genome annotation in GFF format"
  ),
  stringsAsFactors = FALSE
)

# Only insert roles that don't exist
for (i in 1:nrow(new_file_roles)) {
  role_exists <- dbGetQuery(con,
    "SELECT COUNT(*) as n FROM object_file_type_spec WHERE object_type = ? AND file_role = ?",
    params = list(new_file_roles$object_type[i], new_file_roles$file_role[i]))$n > 0

  if (!role_exists) {
    dbExecute(con,
      "INSERT INTO object_file_type_spec (object_type, file_role, description) VALUES (?, ?, ?)",
      params = list(
        new_file_roles$object_type[i],
        new_file_roles$file_role[i],
        new_file_roles$description[i]
      ))
    cat("✓ Added file role:", new_file_roles$file_role[i], "for", new_file_roles$object_type[i], "\n")
  }
}

# ==============================================================================
# ADD AMPLICON OBJECT TYPE AND SPEC ENTRIES (gopheR 0.9.x+)
# ==============================================================================

# New object type: amplicon (replaces asv_batch going forward)
type_exists <- dbGetQuery(con, "SELECT COUNT(*) as n FROM object_type WHERE object_type = 'amplicon'")$n > 0
if (!type_exists) {
  dbExecute(con,
    "INSERT INTO object_type (object_type, description) VALUES (?, ?)",
    params = list(
      "amplicon",
      "An ASV generation run on a set of samples using a defined primer set"
    ))
  cat("✓ Added object type: amplicon\n")
}

# Amplicon subtypes
amplicon_subtypes <- data.frame(
  object_type = "amplicon",
  object_subtype = c("V4", "V3-V4", "ITS2", "18S", "16S_FL", "WANDA"),
  description = c(
    "16S V4 amplicon",
    "16S V3-V4 amplicon",
    "ITS2 amplicon (fungi)",
    "18S amplicon",
    "16S full-length amplicon (PacBio/Nanopore)",
    "SSU rRNA amplicon for arbuscular mycorrhizal fungi"
  ),
  stringsAsFactors = FALSE
)
for (i in 1:nrow(amplicon_subtypes)) {
  exists <- dbGetQuery(con,
    "SELECT COUNT(*) as n FROM object_subtype WHERE object_type = ? AND object_subtype = ?",
    params = list(amplicon_subtypes$object_type[i], amplicon_subtypes$object_subtype[i]))$n > 0
  if (!exists) {
    dbExecute(con,
      "INSERT INTO object_subtype (object_type, object_subtype, description) VALUES (?, ?, ?)",
      params = list(amplicon_subtypes$object_type[i], amplicon_subtypes$object_subtype[i], amplicon_subtypes$description[i]))
    cat("✓ Added subtype: amplicon:", amplicon_subtypes$object_subtype[i], "\n")
  }
}

# New edge types
new_edges <- data.frame(
  parent_type = c("readset",          "readset",      "readset"),
  child_type  = c("amplicon",          "set",          "readset"),
  edge_type   = c("inferred_from",     "sequenced_in", "derived_from"),
  description = c(
    "Amplicon was inferred from these readsets via a denoising pipeline (DADA2, Deblur, etc.)",
    "Readset was sequenced in this Illumina run (use with set:readset)",
    "Readset was split or derived from another readset (e.g. collapsed FASTQ splitting)"
  ),
  stringsAsFactors = FALSE
)
for (i in 1:nrow(new_edges)) {
  exists <- dbGetQuery(con,
    "SELECT COUNT(*) as n FROM edge_spec WHERE parent_type = ? AND child_type = ? AND edge_type = ?",
    params = list(new_edges$parent_type[i], new_edges$child_type[i], new_edges$edge_type[i]))$n > 0
  if (!exists) {
    dbExecute(con,
      "INSERT INTO edge_spec (parent_type, child_type, edge_type, description) VALUES (?, ?, ?, ?)",
      params = list(new_edges$parent_type[i], new_edges$child_type[i], new_edges$edge_type[i], new_edges$description[i]))
    cat("✓ Added edge:", new_edges$edge_type[i], "(", new_edges$parent_type[i], "→", new_edges$child_type[i], ")\n")
  }
}

# Edge results for sequenced_in
sequenced_in_results <- data.frame(
  edge_type   = "sequenced_in",
  key         = c("flowcell_id", "instrument_id", "lane"),
  value_type  = "text",
  description = c(
    "Illumina flowcell identifier",
    "Instrument serial number or machine ID",
    "Sequencing lane number"
  ),
  stringsAsFactors = FALSE
)
for (i in 1:nrow(sequenced_in_results)) {
  exists <- dbGetQuery(con,
    "SELECT COUNT(*) as n FROM edge_result_spec WHERE edge_type = ? AND key = ?",
    params = list(sequenced_in_results$edge_type[i], sequenced_in_results$key[i]))$n > 0
  if (!exists) {
    dbExecute(con,
      "INSERT INTO edge_result_spec (edge_type, key, value_type, description) VALUES (?, ?, ?, ?)",
      params = list(
        sequenced_in_results$edge_type[i],
        sequenced_in_results$key[i],
        sequenced_in_results$value_type[i],
        sequenced_in_results$description[i]
      ))
    cat("✓ Added edge result:", sequenced_in_results$edge_type[i], "/", sequenced_in_results$key[i], "\n")
  }
}

# Object file roles for amplicon
amplicon_files <- data.frame(
  object_type = "amplicon",
  file_role   = c("asv_fasta", "abundance_matrix", "abundance_matrix_raw",
                  "phylogenetic_tree", "alignment", "phyloseq_rds",
                  "amplify_provenance", "amplify_experiment"),
  description = c(
    "Filtered ASV sequences (FASTA)",
    "Sample × ASV count table (TSV)",
    "Unfiltered DADA2 seqtab output (TSV)",
    "Phylogenetic tree of ASV sequences (Newick)",
    "Multiple sequence alignment of ASVs (FASTA)",
    "phyloseq object (R RDS)",
    "amplify provenance JSON",
    "amplify experiment object (R RDS)"
  ),
  stringsAsFactors = FALSE
)
for (i in 1:nrow(amplicon_files)) {
  exists <- dbGetQuery(con,
    "SELECT COUNT(*) as n FROM object_file_type_spec WHERE object_type = ? AND file_role = ?",
    params = list(amplicon_files$object_type[i], amplicon_files$file_role[i]))$n > 0
  if (!exists) {
    dbExecute(con,
      "INSERT INTO object_file_type_spec (object_type, file_role, description) VALUES (?, ?, ?)",
      params = list(amplicon_files$object_type[i], amplicon_files$file_role[i], amplicon_files$description[i]))
    cat("✓ Added file role: amplicon /", amplicon_files$file_role[i], "\n")
  }
}

# Object result keys for amplicon
amplicon_results <- data.frame(
  object_type = "amplicon",
  key         = c("total_asvs", "filtered_asvs", "filter_threshold", "median_depth"),
  value_type  = c("integer", "integer", "text", "numeric"),
  description = c(
    "Total ASVs before prevalence filtering",
    "ASVs retained after prevalence filtering",
    "Prevalence filter applied, e.g. 2x2 or 5x5",
    "Median per-sample sequencing depth"
  ),
  stringsAsFactors = FALSE
)
for (i in 1:nrow(amplicon_results)) {
  exists <- dbGetQuery(con,
    "SELECT COUNT(*) as n FROM object_result_spec WHERE object_type = ? AND key = ?",
    params = list(amplicon_results$object_type[i], amplicon_results$key[i]))$n > 0
  if (!exists) {
    dbExecute(con,
      "INSERT INTO object_result_spec (object_type, key, value_type, description) VALUES (?, ?, ?, ?)",
      params = list(
        amplicon_results$object_type[i],
        amplicon_results$key[i],
        amplicon_results$value_type[i],
        amplicon_results$description[i]
      ))
    cat("✓ Added result key: amplicon /", amplicon_results$key[i], "\n")
  }
}

# ==============================================================================

dbDisconnect(con)
cat("\n✓ Spec tables updated!\n")
cat("\nNote: study:general and site:general subtypes are now included\n")
cat("      in the gopheR starter database (no longer need to be added).\n")
