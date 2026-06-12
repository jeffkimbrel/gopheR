# Add missing spec entries to support example data

library(gopheR)
library(DBI)

# Set database path
options(gopheR.db_path = "inst/extdata")
options(gopheR.db_file = "blank_db.sqlite")

# Get connection using gopheR helper
con <- gopher_con()

# ==============================================================================
# ADD MISSING RESULT KEYS
# ==============================================================================

# Check what's already there
existing_keys <- dbGetQuery(con, "SELECT DISTINCT key FROM key_spec")$key
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
  stringsAsFactors = FALSE
)

# Only insert keys that don't exist
for (i in 1:nrow(new_keys)) {
  key_exists <- dbGetQuery(con,
    "SELECT COUNT(*) as n FROM key_spec WHERE object_type = ? AND key = ?",
    params = list(new_keys$object_type[i], new_keys$key[i]))$n > 0

  if (!key_exists) {
    dbExecute(con,
      "INSERT INTO key_spec (object_type, key, value_type, description) VALUES (?, ?, ?, ?)",
      params = list(
        new_keys$object_type[i],
        new_keys$key[i],
        new_keys$value_type[i],
        new_keys$description[i]
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

dbDisconnect(con)
cat("\n✓ Spec tables updated!\n")
cat("\nNote: study:general and site:general subtypes are now included\n")
cat("      in the gopheR starter database (no longer need to be added).\n")
