# Example MAG Dataset Design

This document outlines the structure of the example data that ships with gopherDen.

## Overview
A small metagenomic study examining soil microbial communities from two forest sites.

## Object Hierarchy

### Study (plain type, no subtype)
- **study_001**: "Forest Soil Microbiome Study 2025"
  - Description: Comparative metagenomics of temperate forest soils

### Sites
- **site_A**: "Sunny Ridge Forest"
  - Location: 35.9641° N, 84.2853° W
  - Elevation: 280m
  - Soil type: Clay loam
  
- **site_B**: "Pine Valley Forest"
  - Location: 36.1234° N, 84.5678° W
  - Elevation: 450m
  - Soil type: Sandy loam

### Samples (3 per site = 6 total)
- **sample_A1**: Sunny Ridge, Plot 1, 0-10cm depth, collected 2025-03-15
- **sample_A2**: Sunny Ridge, Plot 2, 0-10cm depth, collected 2025-03-15
- **sample_A3**: Sunny Ridge, Plot 3, 10-20cm depth, collected 2025-03-15
- **sample_B1**: Pine Valley, Plot 1, 0-10cm depth, collected 2025-03-20
- **sample_B2**: Pine Valley, Plot 2, 0-10cm depth, collected 2025-03-20
- **sample_B3**: Pine Valley, Plot 3, 10-20cm depth, collected 2025-03-20

### Readsets (1 per sample = 6 total)
All paired-end Illumina sequencing, 2x150bp
- **reads_A1**
- **reads_A2**
- **reads_A3**
- **reads_B1**
- **reads_B2**
- **reads_B3**

### Assemblies (1 per readset = 6 total)
Using metaSPAdes
- **asm_A1**: 2,450 contigs, N50: 8,234 bp
- **asm_A2**: 2,890 contigs, N50: 9,102 bp
- **asm_A3**: 2,120 contigs, N50: 7,456 bp
- **asm_B1**: 2,680 contigs, N50: 8,891 bp
- **asm_B2**: 3,120 contigs, N50: 9,567 bp
- **asm_B3**: 2,340 contigs, N50: 7,889 bp

### MAGs (2 per assembly = 12 total)
Using MetaBAT2 + CheckM2 for quality assessment

**From asm_A1:**
- **MAG_A1_001**: High-quality Acidobacteria
  - Completeness: 95.2%, Contamination: 1.8%
  - Size: 4200000 bp, GC: 62.1%
  - Taxonomy: d__Bacteria;p__Acidobacteriota;c__Acidobacteriae;o__Acidobacteriales

- **MAG_A1_002**: Medium-quality Proteobacteria
  - Completeness: 78.5%, Contamination: 3.2%
  - Size: 3100000 bp, GC: 58.4%
  - Taxonomy: d__Bacteria;p__Proteobacteria;c__Gammaproteobacteria;o__Burkholderiales

**From asm_A2:**
- **MAG_A2_001**: High-quality Actinobacteria
  - Completeness: 97.8%, Contamination: 0.9%
  - Size: 5800000 bp, GC: 71.3%
  - Taxonomy: d__Bacteria;p__Actinobacteriota;c__Actinomycetes;o__Streptomycetales

- **MAG_A2_002**: High-quality Verrucomicrobia
  - Completeness: 92.1%, Contamination: 2.1%
  - Size: 4500000 bp, GC: 65.7%
  - Taxonomy: d__Bacteria;p__Verrucomicrobiota;c__Verrucomicrobiae;o__Chthoniobacterales

**From asm_A3:**
- **MAG_A3_001**: Medium-quality Bacteroidetes
  - Completeness: 81.3%, Contamination: 4.5%
  - Size: 3400000 bp, GC: 42.8%
  - Taxonomy: d__Bacteria;p__Bacteroidota;c__Bacteroidia;o__Chitinophagales

- **MAG_A3_002**: High-quality Planctomycetes
  - Completeness: 94.7%, Contamination: 1.2%
  - Size: 6200000 bp, GC: 68.9%
  - Taxonomy: d__Bacteria;p__Planctomycetota;c__Planctomycetes;o__Planctomycetales

**From asm_B1:**
- **MAG_B1_001**: High-quality Proteobacteria
  - Completeness: 96.5%, Contamination: 1.5%
  - Size: 4800000 bp, GC: 61.2%
  - Taxonomy: d__Bacteria;p__Proteobacteria;c__Alphaproteobacteria;o__Rhizobiales

- **MAG_B1_002**: Medium-quality Firmicutes
  - Completeness: 76.8%, Contamination: 5.1%
  - Size: 2900000 bp, GC: 48.3%
  - Taxonomy: d__Bacteria;p__Firmicutes;c__Bacilli;o__Bacillales

**From asm_B2:**
- **MAG_B2_001**: High-quality Chloroflexi
  - Completeness: 93.4%, Contamination: 1.7%
  - Size: 5100000 bp, GC: 69.5%
  - Taxonomy: d__Bacteria;p__Chloroflexota;c__Chloroflexia;o__Chloroflexales

- **MAG_B2_002**: High-quality Gemmatimonadetes
  - Completeness: 91.2%, Contamination: 2.3%
  - Size: 4600000 bp, GC: 64.8%
  - Taxonomy: d__Bacteria;p__Gemmatimonadota;c__Gemmatimonadetes;o__Gemmatimonadales

**From asm_B3:**
- **MAG_B3_001**: Medium-quality Acidobacteria
  - Completeness: 79.5%, Contamination: 4.8%
  - Size: 3200000 bp, GC: 61.7%
  - Taxonomy: d__Bacteria;p__Acidobacteriota;c__Acidobacteriae;o__Bryobacterales

- **MAG_B3_002**: High-quality Nitrospirae
  - Completeness: 95.8%, Contamination: 1.1%
  - Size: 3700000 bp, GC: 55.4%
  - Taxonomy: d__Bacteria;p__Nitrospirota;c__Nitrospiria;o__Nitrospirales

## Workflows

### WF_001: Sample Collection
- Label: "Field sampling March 2025"
- Date: 2025-03-25
- Software: Manual collection
- Created by: jkimbrel

### WF_002: DNA Extraction
- Label: "DNeasy PowerSoil DNA extraction"
- Date: 2025-03-28
- Software: DNeasy PowerSoil Kit
- Created by: jkimbrel

### WF_003: Sequencing
- Label: "Illumina NovaSeq 6000 sequencing"
- Date: 2025-04-05
- Software: NovaSeq Control Software v1.7
- Created by: seq_facility

### WF_004: Assembly
- Label: "metaSPAdes v3.15.5 assembly"
- Date: 2025-04-10
- Software: metaSPAdes v3.15.5
- Parameters: --meta -k 21,33,55,77,99
- Created by: jkimbrel

### WF_005: Binning
- Label: "MetaBAT2 v2.15 binning"
- Date: 2025-04-15
- Software: MetaBAT2 v2.15
- Parameters: --minContig 2500 --maxEdges 200
- Created by: jkimbrel

### WF_006: Quality Assessment
- Label: "CheckM2 v1.0.2 quality assessment"
- Date: 2025-04-18
- Software: CheckM2 v1.0.2
- Created by: jkimbrel

### WF_007: Taxonomy
- Label: "GTDB-Tk v2.3.2 taxonomy (r214)"
- Date: 2025-04-20
- Software: GTDB-Tk v2.3.2 with GTDB r214
- Created by: jkimbrel

## Edges (Relationships)

### sampled_from (sample → site)
- sample_A1 → site_A (WF_001)
- sample_A2 → site_A (WF_001)
- sample_A3 → site_A (WF_001)
- sample_B1 → site_B (WF_001)
- sample_B2 → site_B (WF_001)
- sample_B3 → site_B (WF_001)

### extracted_from (readset → sample)
- reads_A1 → sample_A1 (WF_002/WF_003)
- reads_A2 → sample_A2 (WF_002/WF_003)
- reads_A3 → sample_A3 (WF_002/WF_003)
- reads_B1 → sample_B1 (WF_002/WF_003)
- reads_B2 → sample_B2 (WF_002/WF_003)
- reads_B3 → sample_B3 (WF_002/WF_003)

### assembled_from (assembly → readset)
- asm_A1 → reads_A1 (WF_004)
- asm_A2 → reads_A2 (WF_004)
- asm_A3 → reads_A3 (WF_004)
- asm_B1 → reads_B1 (WF_004)
- asm_B2 → reads_B2 (WF_004)
- asm_B3 → reads_B3 (WF_004)

### binned_from (genome → assembly)
- MAG_A1_001 → asm_A1 (WF_005)
- MAG_A1_002 → asm_A1 (WF_005)
- MAG_A2_001 → asm_A2 (WF_005)
- MAG_A2_002 → asm_A2 (WF_005)
- MAG_A3_001 → asm_A3 (WF_005)
- MAG_A3_002 → asm_A3 (WF_005)
- MAG_B1_001 → asm_B1 (WF_005)
- MAG_B1_002 → asm_B1 (WF_005)
- MAG_B2_001 → asm_B2 (WF_005)
- MAG_B2_002 → asm_B2 (WF_005)
- MAG_B3_001 → asm_B3 (WF_005)
- MAG_B3_002 → asm_B3 (WF_005)

## Results

### Sample-level results
- None (sample metadata in description only)

### Readset-level results
- None (readset metadata in description only)

### Assembly-level results (WF_004)
- n_contigs, N50 for each assembly

### MAG-level results
- completeness, contamination (WF_006)
- size_bp, gc_content (WF_006)
- taxonomy (WF_007)

## Object Files

### Readsets
- fastq_r1, fastq_r2 (example paths: /data/reads/sample_A1_R1.fastq.gz)

### Assemblies
- assembly_fasta (example: /data/assemblies/asm_A1.fasta)

### MAGs
- genome_fasta (example: /data/mags/MAG_A1_001.fasta)

## People

- **jkimbrel**: Jeff Kimbrel, jeff.kimbrel@example.com
- **seq_facility**: Sequencing Core Facility, sequencing@example.com
