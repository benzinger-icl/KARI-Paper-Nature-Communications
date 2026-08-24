## -----------------------------------------------------------------------------
## MIT License
## Copyright (c) 2025 David A. Hoagey - Washington University in St. Louis
##
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in all
## copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.
## -----------------------------------------------------------------------------
#### KARI Imaging Methods Paper
rm(list=ls())

## Load packages ################################################################
list.of.packages <- c("tidyverse", "readxl", "paletteer", "mclust")

## Check if they are installed and install them if they are not
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load all packages
lapply(list.of.packages, require, character.only = TRUE)

#################################################################################
## Set Paths
inPATH <- paste0("B:/path/synthetic_data/")

## Import MRI
img <- read_excel(paste0(inPATH,"mri_3t.xlsx")) %>%
  select(ID, MR_Session, MRI_Accession, Subject_Accession, MR_Date, Project, FS_QC_Status, Field_Strength, FS_Version, T1UsedForProcessing, TOTAL_CORTEX=mr_vol_tot_cortex, Entorhinal_L_Vol=mr_vol_l_ctx_entorhinal, Entorhinal_R_Vol=mr_vol_r_ctx_entorhinal, Inferior_Temporal_L_Vol=mr_vol_l_ctx_inftmp, Inferior_Temporal_R_Vol=mr_vol_r_ctx_inftmp, Middle_Temporal_L_Vol=mr_vol_l_ctx_midtmp, Middle_Temporal_R_Vol=mr_vol_r_ctx_midtmp, Amygdala_L_Vol=mr_vol_l_amygdala, Amygdala_R_Vol=mr_vol_r_amygdala, Bankssts_L_Vol=mr_vol_l_ctx_sstsbank, Bankssts_R_Vol=mr_vol_r_ctx_sstsbank, Cuneus_L_Vol=mr_vol_l_ctx_cuneus, Cuneus_R_Vol=mr_vol_r_ctx_cuneus, Inferior_Parietal_L_Vol=mr_vol_l_ctx_infprtl, Inferior_Parietal_R_Vol=mr_vol_r_ctx_infprtl, Isthmus_Cingulate_L_Vol=mr_vol_l_ctx_isthmuscng, Isthmus_Cingulate_R_Vol=mr_vol_r_ctx_isthmuscng, Lateral_Occipital_L_Vol=mr_vol_l_ctx_latocc, Lateral_Occipital_R_Vol=mr_vol_r_ctx_latocc, Lingual_L_Vol=mr_vol_l_ctx_lingual, Lingual_R_Vol=mr_vol_r_ctx_lingual,Parahippocampal_L_Vol=mr_vol_l_ctx_parahpcmpl, Parahippocampal_R_Vol=mr_vol_r_ctx_parahpcmpl, Posterior_Cingulate_L_Vol=mr_vol_l_ctx_postcng, Posterior_Cingulate_R_Vol=mr_vol_r_ctx_postcng, Precuneus_L_Vol=mr_vol_l_ctx_precuneus, Precuneus_R_Vol=mr_vol_r_ctx_precuneus, Superior_Parietal_L_Vol=mr_vol_l_ctx_superprtl, Superior_Parietal_R_Vol=mr_vol_r_ctx_superprtl, Superior_Temporal_L_Vol=mr_vol_l_ctx_supertmp, Superior_Temporal_R_Vol=mr_vol_r_ctx_supertmp, Supermarginal_L_Vol=mr_vol_l_ctx_supramrgnl, Supermarginal_R_Vol=mr_vol_r_ctx_supramrgnl) %>%
  mutate(FS_Version = sub(".*v([0-9]+\\.[0-9]+\\.[0-9]+).*", "\\1", FS_Version)) %>%
  mutate(FS_Version = sub(".*-(\\d+\\.\\d+\\.\\d+)-.*", "\\1", FS_Version)) %>%
  filter(FS_QC_Status != "NA")

## Amyloid (need manual to pull centiloids)
av45m <- read_excel(paste0(inPATH,"av45_mrfreepet.xlsx")) %>%
  select(ID, AMY_PET_Session=PET_Session, AMY_PET_Accession=PET_Accession, AMY_PET_Date=PET_Date, AMY_Project=Project, AMY_Tracer=Tracer, AMY_PET_status=PET_status, Centiloid=MRFreePET_Centiloid)

pibm <- read_excel(paste0(inPATH,"pib_mrfreepet.xlsx")) %>%
  select(ID, AMY_PET_Session=PET_Session, AMY_PET_Accession=PET_Accession, AMY_PET_Date=PET_Date, AMY_Project=Project, AMY_Tracer=Tracer, AMY_PET_status=PET_status, Centiloid=MRFreePET_Centiloid)

fbbm <- read_excel(paste0(inPATH,"fbb_mrfreepet.xlsx")) %>%
  select(ID, AMY_PET_Session=PET_Session, AMY_PET_Accession=PET_Accession, AMY_PET_Date=PET_Date, AMY_Project=Project, AMY_Tracer=Tracer, AMY_PET_status=PET_status, Centiloid=MRFreePET_Centiloid)

## Tau
tau_pet <- read_excel(paste0(inPATH,"tau.xlsx")) %>%
  select(ID, TAU_PET_Session=PET_Session, TAU_PET_Accession=PET_Accession, TAU_PET_Date=PET_Date, TAU_Project=Project, TAU_Tracer=Tracer, TAU_PET_status=PET_status, Entorhinal_L_tau=pet_fsuvr_l_ctx_entorhinal, Entorhinal_R_tau=pet_fsuvr_r_ctx_entorhinal, Inferior_Temporal_L_tau=pet_fsuvr_l_ctx_infrtmp, Inferior_Temporal_R_tau=pet_fsuvr_r_ctx_inftmp, Middle_Temporal_L_tau=pet_fsuvr_l_ctx_midtmp, Middle_Temporal_R_tau=pet_fsuvr_r_ctx_midtmp, Amygdala_L_tau=pet_fsuvr_l_amygdala, Amygdala_R_tau=pet_fsuvr_r_amygdala, Bankssts_L_tau=pet_fsuvr_l_ctx_sstsbank, Bankssts_R_tau=pet_fsuvr_r_ctx_sstsbank, Cuneus_L_tau=pet_fsuvr_l_ctx_cuneus, 
  Cuneus_R_tau=pet_fsuvr_r_ctx_cuneus, Inferior_Parietal_L_tau=pet_fsuvr_l_ctx_infrprtl, Inferior_Parietal_R_tau=pet_fsuvr_r_ctx_infprtl, Isthmus_Cingulate_L_tau=pet_fsuvr_l_ctx_isthmuscng, Isthmus_Cingulate_R_tau=pet_fsuvr_r_ctx_isthmuscng, Lateral_Occipital_L_tau=pet_fsuvr_l_ctx_latocc, Lateral_Occipital_R_tau=pet_fsuvr_r_ctx_latocc, Lingual_L_tau=pet_fsuvr_l_ctx_lingual, Lingual_R_tau=pet_fsuvr_r_ctx_lingual, Parahippocampal_L_tau=pet_fsuvr_l_ctx_parahpcmpl, Parahippocampal_R_tau=pet_fsuvr_r_ctx_parahpcmpl, Posterior_Cingulate_L_tau=pet_fsuvr_l_ctx_postcng, Posterior_Cingulate_R_tau=pet_fsuvr_r_ctx_postcng, Precuneus_L_tau=pet_fsuvr_l_ctx_precuneus, Precuneus_R_tau=pet_fsuvr_r_ctx_precuneus, Superior_Parietal_L_tau=pet_fsuvr_l_ctx_superprtl, Superior_Parietal_R_tau=pet_fsuvr_r_ctx_superprtl, Superior_Temporal_L_tau=pet_fsuvr_l_ctx_supertmp, Superior_Temporal_R_tau=pet_fsuvr_r_ctx_supertmp, Supermarginal_L_tau=pet_fsuvr_l_ctx_supramrgnl, Supermarginal_R_tau=pet_fsuvr_r_ctx_supramrgnl, TSS, Tauopathy)

## Demographics
dem <- read_excel(paste0(inPATH,"demographics.xlsx")) %>%
  select(ID, SES, BIRTH, DEATH, ENTDATE, EDUC, HAND, grantc, race, sex)

## Clinical
cdr <- read_excel(paste0(inPATH,"b4_cdr.xlsx")) %>%
  mutate(TESTDATE = as.Date(TESTDATE))

## Cognition (PACC)
cog <- read_excel(paste0(inPATH,"psychometrics.xlsx")) %>%
  select(ID, cog_date = psy_date, K1) %>%
  mutate(cog_date = as.Date(cog_date))

#################################################################################
## Remove rows where img$TOTAL_CORTEX is NA
img <- img %>% filter(!is.na(TOTAL_CORTEX))

## Add demographics
img_dem <- merge(img, dem, by = 'ID', all.x = TRUE)

## Combine amyloid
amyloid_pet <- bind_rows(av45m, pibm, fbbm)

## Remove rows where Centiloid is NA
amyloid_pet <- amyloid_pet %>% filter(!is.na(Centiloid))

## Remove rows where Tauopathy is NA
tau_pet <- tau_pet %>% filter(!is.na(Tauopathy))

# Convert dates to Date objects if they are not already
amyloid_pet$AMY_PET_Date <- as.Date(amyloid_pet$AMY_PET_Date)
tau_pet$TAU_PET_Date <- as.Date(tau_pet$TAU_PET_Date)
img_dem$MR_Date <- as.Date(img_dem$MR_Date)
#################################################################################
## Combine all (starting with PET)
PET_merge <- merge(amyloid_pet, tau_pet)

# Calculate the absolute difference between the two dates for each ID
PET_merge <- PET_merge %>%
  group_by(TAU_PET_Session) %>%
  mutate(PET_date_diff = abs(AMY_PET_Date - TAU_PET_Date))

# Identify the row with the minimum date difference within each session
PET_merge <- PET_merge %>%
  group_by(AMY_PET_Session) %>%
  slice_min(order_by = PET_date_diff, with_ties = FALSE) %>%
  mutate(PET_date_diff = as.numeric(PET_date_diff)) %>%
  ungroup()

## Remove rows where date_diff is greater than 365
PET_merge <- PET_merge %>% filter(PET_date_diff <= 365)

## Combine all (merge MRI)
ALL_merge <- merge(PET_merge, img_dem)

# Calculate the absolute difference between the two dates for each ID
ALL_merge <- ALL_merge %>%
  group_by(MR_Session) %>%
  mutate(MR_AMY_date_diff = abs(AMY_PET_Date - MR_Date))

# Identify the row with the minimum date difference within each MR session
ALL_merge <- ALL_merge %>%
  group_by(AMY_PET_Session) %>%
  slice_min(order_by = MR_AMY_date_diff, with_ties = FALSE) %>%
  mutate(MR_AMY_date_diff = as.numeric(MR_AMY_date_diff)) %>%
  ungroup()

## Remove rows where date_diff is greater than 365
ALL_merge <- ALL_merge %>% filter(MR_AMY_date_diff <= 365)

# Calculate the absolute different between the two dates for each ID
ALL_merge <- ALL_merge %>%
  group_by(MR_Session) %>%
  mutate(MR_TAU_date_diff = abs(TAU_PET_Date - MR_Date))

## Remove rows where date_diff is greater than 365
ALL_merge <- ALL_merge %>% filter(MR_TAU_date_diff <= 365)

##################################################################################
## Create MTL ROI 
ALL_merge$MTL <- ((ALL_merge$Entorhinal_R_tau * ALL_merge$Entorhinal_R_Vol) + (ALL_merge$Entorhinal_L_tau * ALL_merge$Entorhinal_L_Vol) + (ALL_merge$Parahippocampal_L_tau * ALL_merge$Parahippocampal_L_Vol) + (ALL_merge$Parahippocampal_R_tau * ALL_merge$Parahippocampal_R_Vol) + (ALL_merge$Amygdala_L_tau * ALL_merge$Amygdala_L_Vol) + (ALL_merge$Amygdala_R_tau * ALL_merge$Amygdala_R_Vol)) / (ALL_merge$Entorhinal_R_Vol + ALL_merge$Entorhinal_L_Vol + ALL_merge$Parahippocampal_L_Vol + ALL_merge$Parahippocampal_R_Vol + ALL_merge$Amygdala_L_Vol + ALL_merge$Amygdala_R_Vol)

## Create NEO ROI 
ALL_merge$NEO <- ((ALL_merge$Inferior_Temporal_L_tau * ALL_merge$Inferior_Temporal_L_Vol) + (ALL_merge$Inferior_Temporal_R_tau  * ALL_merge$Inferior_Temporal_R_Vol ) + 
  (ALL_merge$Middle_Temporal_L_tau * ALL_merge$Middle_Temporal_L_Vol) + (ALL_merge$Middle_Temporal_R_tau * ALL_merge$Middle_Temporal_R_Vol) + 
  (ALL_merge$Bankssts_L_tau * ALL_merge$Bankssts_L_Vol) +
  (ALL_merge$Bankssts_R_tau * ALL_merge$Bankssts_R_Vol) + 
  (ALL_merge$Cuneus_L_tau * ALL_merge$Cuneus_L_Vol) +
  (ALL_merge$Cuneus_R_tau * ALL_merge$Cuneus_R_Vol) + 
  (ALL_merge$Inferior_Parietal_L_tau * ALL_merge$Inferior_Parietal_L_Vol) +
  (ALL_merge$Inferior_Parietal_R_tau * ALL_merge$Inferior_Parietal_R_Vol)+ 
  (ALL_merge$Isthmus_Cingulate_L_tau * ALL_merge$Isthmus_Cingulate_L_Vol) +
  (ALL_merge$Isthmus_Cingulate_R_tau * ALL_merge$Isthmus_Cingulate_R_Vol)+ 
  (ALL_merge$Lateral_Occipital_L_tau * ALL_merge$Lateral_Occipital_L_Vol) +
  (ALL_merge$Lateral_Occipital_R_tau * ALL_merge$Lateral_Occipital_R_Vol)+ 
  (ALL_merge$Lingual_L_tau * ALL_merge$Lingual_L_Vol) +
  (ALL_merge$Lingual_R_tau * ALL_merge$Lingual_R_Vol)+ 
  (ALL_merge$Posterior_Cingulate_L_tau * ALL_merge$Posterior_Cingulate_L_Vol) +
  (ALL_merge$Posterior_Cingulate_R_tau * ALL_merge$Posterior_Cingulate_R_Vol)+ 
  (ALL_merge$Precuneus_L_tau * ALL_merge$Precuneus_L_Vol) +
  (ALL_merge$Precuneus_R_tau * ALL_merge$Precuneus_R_Vol)+ 
  (ALL_merge$Superior_Parietal_L_tau * ALL_merge$Superior_Parietal_L_Vol) +
  (ALL_merge$Superior_Parietal_R_tau * ALL_merge$Superior_Parietal_R_Vol)+ 
  (ALL_merge$Superior_Temporal_L_tau * ALL_merge$Superior_Temporal_L_Vol) +
  (ALL_merge$Superior_Temporal_R_tau * ALL_merge$Superior_Temporal_R_Vol)+ 
  (ALL_merge$Supermarginal_L_tau * ALL_merge$Supermarginal_L_Vol) +
  (ALL_merge$Supermarginal_R_tau * ALL_merge$Supermarginal_R_Vol)
  ) / 
  (ALL_merge$Inferior_Temporal_L_Vol + ALL_merge$Inferior_Temporal_R_Vol + ALL_merge$Middle_Temporal_L_Vol + ALL_merge$Middle_Temporal_R_Vol + ALL_merge$Bankssts_L_Vol + ALL_merge$Bankssts_R_Vol + ALL_merge$Cuneus_L_Vol + ALL_merge$Cuneus_R_Vol + ALL_merge$Inferior_Parietal_L_Vol + ALL_merge$Inferior_Parietal_R_Vol + ALL_merge$Isthmus_Cingulate_L_Vol + ALL_merge$Isthmus_Cingulate_R_Vol + ALL_merge$Lateral_Occipital_L_Vol + ALL_merge$Lateral_Occipital_R_Vol + ALL_merge$Lingual_L_Vol + ALL_merge$Lingual_R_Vol + ALL_merge$Posterior_Cingulate_L_Vol + ALL_merge$Posterior_Cingulate_R_Vol + ALL_merge$Precuneus_L_Vol + ALL_merge$Precuneus_R_Vol + ALL_merge$Superior_Parietal_L_Vol + ALL_merge$Superior_Parietal_R_Vol + ALL_merge$Superior_Temporal_L_Vol + ALL_merge$Superior_Temporal_R_Vol + ALL_merge$Supermarginal_L_Vol + ALL_merge$Supermarginal_R_Vol)

#################################################################################
## GMM to determine Centiloid cut points
Centiloid_GMM <- Mclust(ALL_merge$Centiloid, G=2)
CENT.df <- data.frame(Centiloid=ALL_merge$Centiloid, Group=as.factor(Centiloid_GMM$classification))
CENTlow <- max(CENT.df$Centiloid[CENT.df$Group == 1])
CENT.df$Group <- ifelse(CENT.df$Centiloid < CENTlow, 1, 2)
CENT.df$Group <- as.factor(CENT.df$Group)

## Create annotations for the density plot
CENT.Group1 <- CENT.df %>% filter(Group == 1)
density_data <- density(CENT.Group1$Centiloid)
half_density <- max(density_data$y)/2
CENT.sw <- shapiro.test(CENT.df$Centiloid)

## Create density plot
CENT.density <- ggplot(CENT.df, aes(x = Centiloid, color = factor(Group))) +
    geom_density(alpha = 1, adjust = 2, show_guide=FALSE, linewidth = 1) +
    stat_density(geom="line", adjust = 2, position="identity") +
    scale_color_manual(values = c("#73AF48FF", "#CC503EFF"), labels = c("Amyloid Negative", "Amyloid Positive")) +
    labs(x = "Centiloid", y = "Density", color = "Group") +
    ggtitle(paste0("GMM of Centiloid - Shapiro-Wilk = ",signif(CENT.sw$p.value,4))) +
    geom_vline(xintercept = CENTlow, linetype = "dotted", linewidth = .75) +
    annotate("text", x = (CENTlow), y = half_density, label = paste0(round(CENTlow,digits = 2)), vjust = -1) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12), 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12))

#################################################################################
## GMM to determine MTL cut points
MTL_GMM <- Mclust(ALL_merge$MTL, G=2)
MTL.df <- data.frame(MTL=ALL_merge$MTL, Group=as.factor(MTL_GMM$classification))
MTLlow <- max(MTL.df$MTL[MTL.df$Group == 1])
MTL.df$Group <- ifelse(MTL.df$MTL < MTLlow, 1, 2)
MTL.df$Group <- as.factor(MTL.df$Group)

## Create annotations for the density plot
MTL.Group1 <- MTL.df %>% filter(Group == 1)
density_data <- density(MTL.Group1$MTL)
half_density <- max(density_data$y)/2
MTL.sw <- shapiro.test(MTL.df$MTL)

## Create density plot
MTL.density <- ggplot(MTL.df, aes(x = MTL, color = factor(Group))) +
    geom_density(alpha = 0.5, adjust = 2, show_guide=FALSE, linewidth = 1) +
    stat_density(geom="line", adjust = 2, position="identity") +
    scale_color_manual(values = c("#73AF48FF", "#CC503EFF"), labels = c("Tau Negative", "Tau Positive")) +
    labs(x = "Tau SUVR", y = "Density", color = "Group") +
    ggtitle(paste0("GMM of Medial Temporal Tau - Shapiro-Wilk = ",signif(MTL.sw$p.value,4))) +
    geom_vline(xintercept = MTLlow, linetype = "dotted", linewidth = .75) +
    annotate("text", x = (MTLlow), y = half_density, label = paste0(round(MTLlow,digits = 2)), vjust = -1) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12), 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12))

MTL.density_data <- ggplot(MTL.df, aes(x = MTL, fill = factor(Group))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 40) +
  scale_fill_manual(values = c("#73AF48FF", "#CC503EFF"),
                    labels = c("Tau Negative", "Tau Positive")) +
  labs(x = "Tau SUVR", y = "Count", fill = "Group") +
  geom_vline(xintercept = MTLlow, linetype = "dotted", linewidth = .75) +
  annotate("text", x = (MTLlow), y = 20, 
           label = paste0(round(MTLlow, digits = 2)), vjust = -1) +
  theme_minimal()

#################################################################################
## GMM to determine NEO cut points
NEO_GMM <- Mclust(ALL_merge$NEO, G=3)
NEO.df <- data.frame(NEO=ALL_merge$NEO, Group=as.factor(NEO_GMM$classification))
NEOmod <- max(NEO.df$NEO[NEO.df$Group == 1])
NEOhigh <- max(NEO.df$NEO[NEO.df$Group == 2])
NEO.df$Group <- ifelse(NEO.df$NEO < NEOmod, 1, ifelse(NEO.df$NEO < NEOhigh, 2, 3))
NEO.df$Group <- as.factor(NEO.df$Group)

## Create annotations for the density plot
NEO.Group1 <- NEO.df %>% filter(Group == 1)
density_data1 <- density(NEO.Group1$NEO)
half_density1 <- max(density_data1$y)/2

NEO.Group2 <- NEO.df %>% filter(Group == 2)
density_data2 <- density(NEO.Group2$NEO)
half_density2 <- max(density_data2$y)/2

NEO.sw <- shapiro.test(NEO.df$NEO)

## Create density plot
NEO.density <- ggplot(NEO.df, aes(x = NEO, color = factor(Group))) +
    geom_density(alpha = 0.5, adjust = 2, show_guide=FALSE, linewidth = .75) +
    stat_density(geom="line", adjust = 2, position="identity") + 
    scale_color_manual(values = c("#73AF48FF","#E17C05FF","#CC503EFF"), labels = c("Tau Negative","Tau Moderate", "Tau High")) +
    labs(x = "Tau SUVR", y = "Density", color = "Group") +
    ggtitle(paste0("GMM of Neocortical Tau - Shapiro-Wilk = ",signif(NEO.sw$p.value,4))) +
    geom_vline(xintercept = NEOmod, linetype = "dotted", linewidth = .75) +
    geom_vline(xintercept = NEOhigh, linetype = "dotted", linewidth = .75) +
    annotate("text", x = (NEOmod), y = half_density1, label = paste0(round(NEOmod,digits = 2)), vjust = -1) +
    annotate("text", x = (NEOhigh), y = half_density2, label = paste0(round(NEOhigh,digits = 2)), vjust = -1) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12), 
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12))

NEO.density_data <- ggplot(NEO.df, aes(x = NEO, fill = factor(Group))) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 40) +
  scale_fill_manual(values = c("#73AF48FF","#E17C05FF","#CC503EFF"), labels = c("Tau Negative","Tau Moderate", "Tau High")) +
  labs(x = "Tau SUVR", y = "Count", fill = "Group") +
  geom_vline(xintercept = NEOmod, linetype = "dotted", linewidth = .75) +
  annotate("text", x = (NEOhigh), y = 20, 
           label = paste0(round(NEOhigh, digits = 2)), vjust = -1) +
  theme_minimal()

##################################################################################
## Create Summary table
ALL_merge$stage <- ifelse(ALL_merge$Centiloid < CENTlow & ALL_merge$MTL < MTLlow & ALL_merge$NEO < NEOmod, "AnTn",
    ifelse(ALL_merge$Centiloid < CENTlow & ALL_merge$MTL >= MTLlow & ALL_merge$NEO < NEOmod, "AnMTLpNEOn",
    ifelse(ALL_merge$Centiloid < CENTlow & ALL_merge$MTL < MTLlow & ALL_merge$NEO >= NEOmod, "AnMTLnNEOp",
    ifelse(ALL_merge$Centiloid < CENTlow & (ALL_merge$MTL >= MTLlow | ALL_merge$NEO >= NEOmod), "AnMTLpNEOp",
    ifelse(ALL_merge$Centiloid >= CENTlow & ALL_merge$MTL < MTLlow & ALL_merge$NEO < NEOmod, "A", 
    ifelse(ALL_merge$Centiloid >= CENTlow & ALL_merge$MTL >= MTLlow & ALL_merge$NEO < NEOmod, "B", 
    ifelse(ALL_merge$Centiloid >= CENTlow & ALL_merge$MTL >= MTLlow & ALL_merge$NEO >= NEOmod & ALL_merge$NEO < NEOhigh, "C", 
    ifelse(ALL_merge$Centiloid >= CENTlow & ALL_merge$MTL >= MTLlow & ALL_merge$NEO >= NEOhigh, "D",
    ifelse(ALL_merge$Centiloid >= CENTlow & ALL_merge$MTL < MTLlow & ALL_merge$NEO >= NEOmod, "ApMTLnNEOp",
    "Z")))))))))

results.df <- ALL_merge %>% select(ID, AMY_PET_Session, TAU_PET_Session, Centiloid, MTL, NEO, TSS, Tauopathy, stage)

## Save out data used for further analyses
## write.csv(ALL_merge, file = paste0(inPATH,"/PET_merged_DF25.csv"), row.names = FALSE)

## Create a summary table
summary_table <- results.df %>%
  group_by(stage) %>%
  dplyr::summarise(
    Count = n(),
    Centiloid_mean = round(mean(Centiloid, na.rm = TRUE),digits=2),
    Centiloid_SE = round(sd(Centiloid, na.rm = TRUE) / sqrt(n()), digits=2),
    MTL_mean = round(mean(MTL, na.rm = TRUE), digits = 2),
    MTL_SE = round(sd(MTL, na.rm = TRUE) / sqrt(n()), digits = 2),
    NEO_mean = round(mean(NEO, na.rm = TRUE), digits = 2),
    NEO_SE = round(sd(NEO, na.rm = TRUE) / sqrt(n()), digits = 2),
    TSS_mean = round(mean(TSS, na.rm = TRUE), digits = 3),
    TSS_SE = round(sd(TSS, na.rm = TRUE) / sqrt(n()), digits = 2),
    Tauopathy_mean = round(mean(Tauopathy, na.rm = TRUE), digits = 2),
    Tauopathy_SE = round(sd(Tauopathy, na.rm = TRUE) / sqrt(n()), digits = 2)
  ) %>%
  arrange(Centiloid_mean)

# View the summary table
print(summary_table)