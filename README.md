# Assesing-Biological-Replication-in-sedaDNA
---
This repository contains the data and R scripts used to analyse the contribution of biological and technical replication in sedimentary ancient DNA (sedaDNA) metabarcoding datasets. The study compares mitochondrial (COI) and nuclear (18S) markers across intertidal sediment cores, integrating diversity analyses, community ordination, PERMANOVA, and hierarchical modelling of species communities (HMSC). All scripts required to reproduce the figures, statistical analyses, and supplementary datasets are provided.


## 📁 Repository structure
data/ # Input data tables (metadata, abundance tables)
scripts/ # R scripts for data processing and analyses
supplementary/ # Supplementary tables and additional outputs

---

## 🔬 Data description

The dataset includes:

- Relative abundance tables for COI and 18S markers  
- Metadata describing sampling site, sediment depth, and biological replicates  
- Taxonomic assignments at different levels (e.g. kingdom, phylum)  

All data were generated from sedaDNA metabarcoding of intertidal sediment cores collected at three sites (CCO, CLR, STO).

---

## 📊 Analyses included

The repository reproduces all analyses presented in the manuscript, including:

- Alpha diversity (Shannon index and ASV richness)  
- Linear mixed-effects models (technical vs biological replication)  
- Beta diversity analyses (NMDS, PCoA)  
- Distance-based redundancy analysis (db-RDA)  
- PERMANOVA tests  
- Hierarchical modelling of species communities (HMSC)  
- Variance partitioning of community structure  

---

## ⚙️ Requirements

Analyses were performed in:

- R version 4.3.1  

---

## ▶️ Reproducibility

All analyses can be reproduced by running the scripts in the `scripts/` folder in the following order:

---

## 📦 Supplementary data

Full simple model dating, outputs of the HMSC models and additional results are provided in:

- `supplementary/`  

---

## 📍 Study context

Sediment cores were collected from three intertidal wetland sites in northern Spain (CCO, CLR, STO), with biological replication (three cores per site) and technical replication (PCR replicates). The study evaluates the relative importance of spatial, stratigraphic, and analytical sources of variation in sedaDNA datasets.

---

## 📄 Citation

If you use this repository, please cite:

> 

---

## 📬 Contact

For questions or collaboration:

elena.bl@ceab.csic.es
