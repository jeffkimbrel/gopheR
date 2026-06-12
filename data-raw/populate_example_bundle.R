# Script to populate example_bundle.xlsx with realistic MAG data
# Based on example_data_design.md
# This script generates the Excel bundle structure using gopheR's write_bundle(),
# which creates validated dropdowns based on the gopheR starter database

library(openxlsx)
library(dplyr)
library(gopheR)
library(devtools)

# Load gopherDen functions to get make_new_db()
load_all()

# Create a temporary database from gopheR starter
temp_db <- tempfile(fileext = ".sqlite")
make_new_db(temp_db, overwrite = TRUE)

# Point to the temporary database
temp_dir <- dirname(temp_db)
temp_file <- basename(temp_db)
options(gopheR.db_path = temp_dir)
options(gopheR.db_file = temp_file)

# Generate a fresh template from gopheR
write_bundle("inst/extdata/example_bundle.xlsx", people_sheet = TRUE, overwrite = TRUE)

# Load the fresh template
wb <- loadWorkbook("inst/extdata/example_bundle.xlsx")

# ==============================================================================
# PEOPLE SHEET
# ==============================================================================
people_data <- data.frame(
  person_id = c("jkimbrel", "seq_facility"),
  full_name = c("Jeff Kimbrel", "Sequencing Core Facility"),
  email = c("jeff.kimbrel@example.com", "sequencing@example.com"),
  successor_person_id = c(NA, NA),
  stringsAsFactors = FALSE
)

writeData(wb, sheet = "people", x = people_data, startRow = 2, colNames = FALSE)

# ==============================================================================
# WORKFLOW SHEET
# ==============================================================================
workflow_data <- data.frame(
  workflow_id = c("WF_001", "WF_002", "WF_003", "WF_004", "WF_005", "WF_006", "WF_007"),
  description = c(
    "Field sampling March 2025: Manual field sampling of forest soil cores",
    "DNA extraction: DNeasy PowerSoil Kit, standard protocol",
    "Illumina sequencing: NovaSeq 6000 2x150bp paired-end sequencing",
    "metaSPAdes assembly: Metagenomic assembly using metaSPAdes v3.15.5, --meta -k 21,33,55,77,99",
    "MetaBAT2 binning: MAG binning using MetaBAT2 v2.15, --minContig 2500 --maxEdges 200",
    "CheckM2 quality assessment: Quality assessment with CheckM2 v1.0.2, default parameters",
    "GTDB-Tk taxonomy: Taxonomic classification with GTDB-Tk v2.3.2, GTDB r214, --out_dir gtdbtk_out"
  ),
  created_by = c("jkimbrel", "jkimbrel", "seq_facility", "jkimbrel",
                 "jkimbrel", "jkimbrel", "jkimbrel"),
  workflow_date = c(
    "2025-03-25", "2025-03-28", "2025-04-05", "2025-04-10",
    "2025-04-15", "2025-04-18", "2025-04-20"
  ),
  stringsAsFactors = FALSE
)

writeData(wb, sheet = "workflow", x = workflow_data, startRow = 2, colNames = FALSE)

# ==============================================================================
# OBJECT SHEET
# ==============================================================================

# Helper to create objects (note: object_type can be "type" or "type:subtype")
make_objects <- function(ids, type, labels, descriptions) {
  data.frame(
    object_id = ids,
    object_type = type,
    label = labels,
    description = descriptions,
    created_by = "jkimbrel",
    stringsAsFactors = FALSE
  )
}

# Study (using general subtype from gopheR starter)
study <- make_objects(
  "study_001",
  "study:general",
  "Forest Soil Microbiome Study 2025",
  "Comparative metagenomics of temperate forest soils"
)

# Sites (using general subtype from gopheR starter)
sites <- make_objects(
  c("site_A", "site_B"),
  "site:general",
  c("Sunny Ridge Forest", "Pine Valley Forest"),
  c("Sunny Ridge Forest, 35.9641°N 84.2853°W, 280m elevation, clay loam",
    "Pine Valley Forest, 36.1234°N 84.5678°W, 450m elevation, sandy loam")
)

# Samples
samples <- make_objects(
  c("sample_A1", "sample_A2", "sample_A3", "sample_B1", "sample_B2", "sample_B3"),
  "sample:soil",
  c("Sunny Ridge Plot 1", "Sunny Ridge Plot 2", "Sunny Ridge Plot 3",
    "Pine Valley Plot 1", "Pine Valley Plot 2", "Pine Valley Plot 3"),
  c("Sunny Ridge Plot 1, 0-10cm depth, collected 2025-03-15",
    "Sunny Ridge Plot 2, 0-10cm depth, collected 2025-03-15",
    "Sunny Ridge Plot 3, 10-20cm depth, collected 2025-03-15",
    "Pine Valley Plot 1, 0-10cm depth, collected 2025-03-20",
    "Pine Valley Plot 2, 0-10cm depth, collected 2025-03-20",
    "Pine Valley Plot 3, 10-20cm depth, collected 2025-03-20")
)

# Readsets
readsets <- make_objects(
  c("reads_A1", "reads_A2", "reads_A3", "reads_B1", "reads_B2", "reads_B3"),
  "readset:paired_end",
  c("Sample A1 reads", "Sample A2 reads", "Sample A3 reads",
    "Sample B1 reads", "Sample B2 reads", "Sample B3 reads"),
  c("Illumina paired-end reads, 2x150bp, 45M pairs",
    "Illumina paired-end reads, 2x150bp, 52M pairs",
    "Illumina paired-end reads, 2x150bp, 38M pairs",
    "Illumina paired-end reads, 2x150bp, 48M pairs",
    "Illumina paired-end reads, 2x150bp, 55M pairs",
    "Illumina paired-end reads, 2x150bp, 42M pairs")
)

# Assemblies
assemblies <- make_objects(
  c("asm_A1", "asm_A2", "asm_A3", "asm_B1", "asm_B2", "asm_B3"),
  "assembly:metagenome",
  c("Assembly from sample A1", "Assembly from sample A2", "Assembly from sample A3",
    "Assembly from sample B1", "Assembly from sample B2", "Assembly from sample B3"),
  c("metaSPAdes assembly: 2,450 contigs, N50 8,234bp",
    "metaSPAdes assembly: 2,890 contigs, N50 9,102bp",
    "metaSPAdes assembly: 2,120 contigs, N50 7,456bp",
    "metaSPAdes assembly: 2,680 contigs, N50 8,891bp",
    "metaSPAdes assembly: 3,120 contigs, N50 9,567bp",
    "metaSPAdes assembly: 2,340 contigs, N50 7,889bp")
)

# MAGs
mags <- make_objects(
  c("MAG_A1_001", "MAG_A1_002", "MAG_A2_001", "MAG_A2_002",
    "MAG_A3_001", "MAG_A3_002", "MAG_B1_001", "MAG_B1_002",
    "MAG_B2_001", "MAG_B2_002", "MAG_B3_001", "MAG_B3_002"),
  "genome:MAG",
  c("Site A Sample 1 MAG 1", "Site A Sample 1 MAG 2",
    "Site A Sample 2 MAG 1", "Site A Sample 2 MAG 2",
    "Site A Sample 3 MAG 1", "Site A Sample 3 MAG 2",
    "Site B Sample 1 MAG 1", "Site B Sample 1 MAG 2",
    "Site B Sample 2 MAG 1", "Site B Sample 2 MAG 2",
    "Site B Sample 3 MAG 1", "Site B Sample 3 MAG 2"),
  c("High-quality Acidobacteria MAG, 95.2% complete",
    "Medium-quality Proteobacteria MAG, 78.5% complete",
    "High-quality Actinobacteria MAG, 97.8% complete",
    "High-quality Verrucomicrobia MAG, 92.1% complete",
    "Medium-quality Bacteroidetes MAG, 81.3% complete",
    "High-quality Planctomycetes MAG, 94.7% complete",
    "High-quality Proteobacteria MAG, 96.5% complete",
    "Medium-quality Firmicutes MAG, 76.8% complete",
    "High-quality Chloroflexi MAG, 93.4% complete",
    "High-quality Gemmatimonadetes MAG, 91.2% complete",
    "Medium-quality Acidobacteria MAG, 79.5% complete",
    "High-quality Nitrospirae MAG, 95.8% complete")
)

# Combine all objects
all_objects <- bind_rows(study, sites, samples, readsets, assemblies, mags)
writeData(wb, sheet = "object", x = all_objects, startRow = 2, colNames = FALSE)

# ==============================================================================
# EDGE SHEET
# ==============================================================================

# Helper for edges
make_edges <- function(parents, children, edge_type, workflow) {
  data.frame(
    parent_id = parents,
    child_id = children,
    edge_type = edge_type,
    workflow_id = workflow,
    stringsAsFactors = FALSE
  )
}

# Sites to Study (samples collected at sites that are part of study)
edge_sites <- make_edges(
  rep("study_001", 2),
  c("site_A", "site_B"),
  "part_of",
  "WF_001"
)

# Samples collected from sites
edge_samples_sites <- make_edges(
  c("site_A", "site_A", "site_A", "site_B", "site_B", "site_B"),
  c("sample_A1", "sample_A2", "sample_A3", "sample_B1", "sample_B2", "sample_B3"),
  "collected_from",
  "WF_001"
)

# Readsets sequenced from samples
edge_readsets <- make_edges(
  c("sample_A1", "sample_A2", "sample_A3", "sample_B1", "sample_B2", "sample_B3"),
  c("reads_A1", "reads_A2", "reads_A3", "reads_B1", "reads_B2", "reads_B3"),
  "sequenced_from",
  "WF_003"
)

# Assemblies from readsets
edge_assemblies <- make_edges(
  c("reads_A1", "reads_A2", "reads_A3", "reads_B1", "reads_B2", "reads_B3"),
  c("asm_A1", "asm_A2", "asm_A3", "asm_B1", "asm_B2", "asm_B3"),
  "assembled_from",
  "WF_004"
)

# MAGs binned from assemblies
edge_mags <- make_edges(
  c("asm_A1", "asm_A1", "asm_A2", "asm_A2", "asm_A3", "asm_A3",
    "asm_B1", "asm_B1", "asm_B2", "asm_B2", "asm_B3", "asm_B3"),
  c("MAG_A1_001", "MAG_A1_002", "MAG_A2_001", "MAG_A2_002",
    "MAG_A3_001", "MAG_A3_002", "MAG_B1_001", "MAG_B1_002",
    "MAG_B2_001", "MAG_B2_002", "MAG_B3_001", "MAG_B3_002"),
  "binned_from",
  "WF_005"
)

all_edges <- bind_rows(edge_sites, edge_samples_sites, edge_readsets,
                       edge_assemblies, edge_mags)
writeData(wb, sheet = "edge", x = all_edges, startRow = 2, colNames = FALSE)

# ==============================================================================
# RESULT SHEET
# ==============================================================================

# Assembly metrics (only n_contigs and N50 - valid in gopheR starter)
assembly_results <- bind_rows(
  data.frame(
    object_id = c("asm_A1", "asm_A2", "asm_A3", "asm_B1", "asm_B2", "asm_B3"),
    workflow_id = "WF_004",
    key = "n_contigs",
    value = c("2450", "2890", "2120", "2680", "3120", "2340"),
    stringsAsFactors = FALSE
  ),
  data.frame(
    object_id = c("asm_A1", "asm_A2", "asm_A3", "asm_B1", "asm_B2", "asm_B3"),
    workflow_id = "WF_004",
    key = "N50",
    value = c("8234", "9102", "7456", "8891", "9567", "7889"),
    stringsAsFactors = FALSE
  )
)

# MAG quality metrics
mag_results <- bind_rows(
  # Completeness
  data.frame(
    object_id = c("MAG_A1_001", "MAG_A1_002", "MAG_A2_001", "MAG_A2_002",
                  "MAG_A3_001", "MAG_A3_002", "MAG_B1_001", "MAG_B1_002",
                  "MAG_B2_001", "MAG_B2_002", "MAG_B3_001", "MAG_B3_002"),
    workflow_id = "WF_006",
    key = "completeness",
    value = c("95.2", "78.5", "97.8", "92.1", "81.3", "94.7",
              "96.5", "76.8", "93.4", "91.2", "79.5", "95.8"),
    stringsAsFactors = FALSE
  ),
  # Contamination
  data.frame(
    object_id = c("MAG_A1_001", "MAG_A1_002", "MAG_A2_001", "MAG_A2_002",
                  "MAG_A3_001", "MAG_A3_002", "MAG_B1_001", "MAG_B1_002",
                  "MAG_B2_001", "MAG_B2_002", "MAG_B3_001", "MAG_B3_002"),
    workflow_id = "WF_006",
    key = "contamination",
    value = c("1.8", "3.2", "0.9", "2.1", "4.5", "1.2",
              "1.5", "5.1", "1.7", "2.3", "4.8", "1.1"),
    stringsAsFactors = FALSE
  ),
  # Genome size (using size_bp from gopheR starter)
  data.frame(
    object_id = c("MAG_A1_001", "MAG_A1_002", "MAG_A2_001", "MAG_A2_002",
                  "MAG_A3_001", "MAG_A3_002", "MAG_B1_001", "MAG_B1_002",
                  "MAG_B2_001", "MAG_B2_002", "MAG_B3_001", "MAG_B3_002"),
    workflow_id = "WF_006",
    key = "size_bp",
    value = c("4200000", "3100000", "5800000", "4500000", "3400000", "6200000",
              "4800000", "2900000", "5100000", "4600000", "3200000", "3700000"),
    stringsAsFactors = FALSE
  ),
  # GC content
  data.frame(
    object_id = c("MAG_A1_001", "MAG_A1_002", "MAG_A2_001", "MAG_A2_002",
                  "MAG_A3_001", "MAG_A3_002", "MAG_B1_001", "MAG_B1_002",
                  "MAG_B2_001", "MAG_B2_002", "MAG_B3_001", "MAG_B3_002"),
    workflow_id = "WF_006",
    key = "gc_content",
    value = c("62.1", "58.4", "71.3", "65.7", "42.8", "68.9",
              "61.2", "48.3", "69.5", "64.8", "61.7", "55.4"),
    stringsAsFactors = FALSE
  ),
  # Taxonomy (using taxonomy key from gopheR starter)
  data.frame(
    object_id = c("MAG_A1_001", "MAG_A1_002", "MAG_A2_001", "MAG_A2_002",
                  "MAG_A3_001", "MAG_A3_002", "MAG_B1_001", "MAG_B1_002",
                  "MAG_B2_001", "MAG_B2_002", "MAG_B3_001", "MAG_B3_002"),
    workflow_id = "WF_007",
    key = "taxonomy",
    value = c(
      "d__Bacteria;p__Acidobacteriota;c__Acidobacteriae;o__Acidobacteriales",
      "d__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Burkholderiales",
      "d__Bacteria;p__Actinobacteriota;c__Actinomycetes;o__Streptomycetales",
      "d__Bacteria;p__Verrucomicrobiota;c__Verrucomicrobiae;o__Chthoniobacterales",
      "d__Bacteria;p__Bacteroidota;c__Bacteroidia;o__Chitinophagales",
      "d__Bacteria;p__Planctomycetota;c__Planctomycetes;o__Planctomycetales",
      "d__Bacteria;p__Proteobacteria;c__Alphaproteobacteria;o__Rhizobiales",
      "d__Bacteria;p__Firmicutes;c__Bacilli;o__Bacillales",
      "d__Bacteria;p__Chloroflexota;c__Chloroflexia;o__Chloroflexales",
      "d__Bacteria;p__Gemmatimonadota;c__Gemmatimonadetes;o__Gemmatimonadales",
      "d__Bacteria;p__Acidobacteriota;c__Acidobacteriae;o__Bryobacterales",
      "d__Bacteria;p__Nitrospirota;c__Nitrospiria;o__Nitrospirales"
    ),
    stringsAsFactors = FALSE
  )
)

all_results <- bind_rows(assembly_results, mag_results)
writeData(wb, sheet = "result", x = all_results, startRow = 2, colNames = FALSE)

# ==============================================================================
# OBJECT_FILE SHEET
# ==============================================================================

# Readset files (correct column order: object_id, file_role, file_path, file_format, workflow_id, checksum)
readset_files <- bind_rows(
  data.frame(
    object_id = rep(c("reads_A1", "reads_A2", "reads_A3", "reads_B1", "reads_B2", "reads_B3"), each = 2),
    file_role = rep(c("fastq_r1", "fastq_r2"), 6),
    file_path = c(
      "/data/reads/sample_A1_R1.fastq.gz", "/data/reads/sample_A1_R2.fastq.gz",
      "/data/reads/sample_A2_R1.fastq.gz", "/data/reads/sample_A2_R2.fastq.gz",
      "/data/reads/sample_A3_R1.fastq.gz", "/data/reads/sample_A3_R2.fastq.gz",
      "/data/reads/sample_B1_R1.fastq.gz", "/data/reads/sample_B1_R2.fastq.gz",
      "/data/reads/sample_B2_R1.fastq.gz", "/data/reads/sample_B2_R2.fastq.gz",
      "/data/reads/sample_B3_R1.fastq.gz", "/data/reads/sample_B3_R2.fastq.gz"
    ),
    file_format = "fastq.gz",
    workflow_id = "WF_003",
    checksum = NA,
    stringsAsFactors = FALSE
  )
)

# Assembly files
assembly_files <- data.frame(
  object_id = c("asm_A1", "asm_A2", "asm_A3", "asm_B1", "asm_B2", "asm_B3"),
  file_role = "assembly_fasta",
  file_path = c(
    "/data/assemblies/asm_A1.fasta",
    "/data/assemblies/asm_A2.fasta",
    "/data/assemblies/asm_A3.fasta",
    "/data/assemblies/asm_B1.fasta",
    "/data/assemblies/asm_B2.fasta",
    "/data/assemblies/asm_B3.fasta"
  ),
  file_format = "fasta",
  workflow_id = "WF_004",
  checksum = NA,
  stringsAsFactors = FALSE
)

# MAG files (only genome_fasta - valid in gopheR starter)
mag_files <- data.frame(
  object_id = c("MAG_A1_001", "MAG_A1_002", "MAG_A2_001", "MAG_A2_002",
                "MAG_A3_001", "MAG_A3_002", "MAG_B1_001", "MAG_B1_002",
                "MAG_B2_001", "MAG_B2_002", "MAG_B3_001", "MAG_B3_002"),
  file_role = "genome_fasta",
  file_path = c(
    "/data/mags/MAG_A1_001.fasta",
    "/data/mags/MAG_A1_002.fasta",
    "/data/mags/MAG_A2_001.fasta",
    "/data/mags/MAG_A2_002.fasta",
    "/data/mags/MAG_A3_001.fasta",
    "/data/mags/MAG_A3_002.fasta",
    "/data/mags/MAG_B1_001.fasta",
    "/data/mags/MAG_B1_002.fasta",
    "/data/mags/MAG_B2_001.fasta",
    "/data/mags/MAG_B2_002.fasta",
    "/data/mags/MAG_B3_001.fasta",
    "/data/mags/MAG_B3_002.fasta"
  ),
  file_format = "fasta",
  workflow_id = "WF_005",
  checksum = NA,
  stringsAsFactors = FALSE
)

all_files <- bind_rows(readset_files, assembly_files, mag_files)
writeData(wb, sheet = "object_file", x = all_files, startRow = 2, colNames = FALSE)

# ==============================================================================
# SAVE WORKBOOK
# ==============================================================================
saveWorkbook(wb, "inst/extdata/example_bundle.xlsx", overwrite = TRUE)

# Clean up temporary database
unlink(temp_db)

cat("✓ Example bundle populated and saved!\n")
cat("  - 2 people\n")
cat("  - 7 workflows\n")
cat("  - 33 objects (1 study, 2 sites, 6 samples, 6 readsets, 6 assemblies, 12 MAGs)\n")
cat("  - 32 edges\n")
cat("  - 72 results (assembly: n_contigs/N50, MAG: completeness/contamination/size_bp/gc_content/taxonomy)\n")
cat("  - 30 object files\n")
