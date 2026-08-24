# Replication Code and Synthetic Data for: Exploring healthy aging through preclinical Alzheimer disease in the Knight Alzheimer Research Imaging dataset 

## Overview
This repository contains the analysis scripts and synthetic datasets used to produce the figures, tables, and statistical summaries for the manuscript:

Title: Exploring healthy aging through preclinical Alzheimer disease in the Knight Alzheimer Research Imaging dataset 
Author List: David A. Hoagey, Nicole S. McKay, Nelly Joseph-Mathurin, Shaney Flores, Stephanie Doering, Sarah J. Keefe, Savannah Tiemann Powles, Russ C. Hornbeck, Thomas H. Smith, Jalen Scott, Gengsheng Chen, Parinaz Massoumzadeh,  Pamela J. LaMontagne, Qing Wang, Jason Hassenstab, Maria Rosana Ponisio, Andrea Denny, Joyce E. Balls-Berry, B. Joy Snider, Susan L. Stark, Chengjie Xiong, Suzanne E. Schindler, Richard J. Perrin, Joshua S. Shimony, Manu S. Goyal, Andrei G. Vlassenko, Marcus E. Raichle, John C. Morris, Cyrus A. Raji, Brian A. Gordon, Tammie L. S. Benzinger

Journal: Nature Communications - 2026

---

## Directory & Manuscript Mapping

| Directory | Script | Manuscript Output |
| :--- | :--- | :--- |
| `Clinical_CDR` | `KARI_DF25_Clinical_CDR_clean.r` | Clinical CDR figures / tables |
| `Cognition` | `KARI_DF25_Cognition_clean.r` | Cognitive performance analyses |
| `cortsig_centiloid_tauopathy_tss` | `KARI_DF25_cortsig_centiloid_tauopathy_tss_clean.r` | Cortical signature / tau analyses |
| `Demographics_alluvial` | `KARI_DF25_Demographics_Alluvial_clean.r` | Demographics alluvial plots |
| `Demographics_table` | `KARI_DF25_Demographics_clean.r` | Baseline demographics table |
| `Leuk_CMB_INF` | `KARI_DF25_leuk_CMB_INF_clean.r` | Leuk / CMB / INF summaries |
| `MR_cumulative_venn` | `KARI_DF25_MRcumulative_count_venn_clean.r` | MR cumulative Venn diagrams |
| `PET_cumulative` | `KARI_DF25_PETcumulative_count_clean.r` | PET cumulative count figures |
| `Staging` | `KARI_DF25_staging_clean.r` | Disease staging model analyses |
| `WMH` | `KARI_DF25_WMH_clean.r` | White matter hyperintensity summaries |

---

## System Requirements
* **R Version:** 4.3.1 ("Beagle Scouts")
* **RStudio:** Version 2025.5.1.513 ("Mariposa Orchid")
* **Operating System Tested:** Windows 11 Desktop
* **Dependencies:** Required packages are listed at the top of each script and are configured to self-install if missing.

## Installation Guide
All packages are free and available for public download. Initial installation of R, RStudio, and associated packages takes roughly 15–20 minutes.

## Synthetic Data Notice
The full KARI dataset is available to researchers upon approval. Because the original data contains protected health information required for matching participants cross-sectionally and longitudinally, the files provided here are synthetic. They are designed to reproduce the analytical workflows and generate comparable outputs without exposing protected data.

## Instructions for Use
1. Open the `.r` script for the target analysis in RStudio or an R console.
2. Update the `inPATH` variable at the top of the script to point to the local directory containing its corresponding synthetic data files.
3. Run the script. Expected execution time is less than 30 seconds per script.
