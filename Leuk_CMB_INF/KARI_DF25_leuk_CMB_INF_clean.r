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
list.of.packages <- c("tidyverse", "readxl", "gmodels", "chisq.posthoc.test")

## Check if they are installed and install them if they are not
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load all packages
lapply(list.of.packages, require, character.only = TRUE)

#################################################################################
## Set Paths
inPATH <- paste0("B:/path/synthetic_data/")

#################### PREP DATA AND TEST ######################################################################################
## Create dataset
## APOE
apoe <- read_excel(paste0(inPATH,"apoe.xlsx"))%>%
  select(ID=id, apoe)

## Demographics
dem <- read_excel(paste0(inPATH,"demographics.xlsx")) %>%
  select(ID, BIRTH, EDUC, HAND, race, sex) %>%
  mutate(BIRTH = as.Date(BIRTH, format="%Y-%m-%d"))

## radreads
radreads <- read_excel(paste0(inPATH,"radreads.xlsx")) %>%
  select(ID, MR_Session, MR_Date, hippocampal_atrophy, leukoaraiosis, cortical_atrophy, large_infarcts, small_infarcts, microbleeds, site_microbleeds, other_significant_findings, aging_changes) %>%
  mutate(MR_Date = as.Date(MR_Date, format="%Y-%m-%d"))

## WMH
wmh <- read_excel(paste0(inPATH,"wmh.xlsx")) %>%
  select(ID, MR_Session, MR_Date, WMH_volume) %>%
  mutate(MR_Date = as.Date(MR_Date, format="%Y-%m-%d"))

## CDR
## Clinical
cdr <- read_excel(paste0(inPATH,"b4_cdr.xlsx")) %>%
  select(ID, CDR_Date=TESTDATE, cdr) %>%
  mutate(CDR_Date = as.Date(CDR_Date, format="%Y-%m-%d"))
######################################################################################
## Merge data
## Combine apoe and dem
dem_apoe <- inner_join(dem, apoe, by = "ID")

## Combine radreads and WMH by MR_Session
radreads_wmh <- inner_join(wmh, radreads, by = c("MR_Session", "ID", "MR_Date"))

## Combine all
combined_df <- left_join(radreads_wmh, dem_apoe, by = "ID")
#################################################################################
## Add CDR by closest date to MR_Date
combined_df_cdr <- combined_df %>%
  left_join(cdr, by = "ID", suffix = c(".mri", ".cdr"), relationship = "many-to-many") %>%
  mutate(CDR_Diff = abs(difftime(MR_Date, CDR_Date, units = "days"))) %>%
  group_by(MR_Session) %>%
  slice_min(CDR_Diff, n = 1, with_ties = FALSE) %>%
  filter(CDR_Diff <= 365*2) %>%
  ungroup()
  ##############################################################################
## Calculate age at MRI
combined_df_cdr <- combined_df_cdr %>%
  mutate(Age = as.numeric(difftime(MR_Date, BIRTH, units = "days"))/365.25)

## Remove NAs in leukoaraiosis column
rads <- combined_df_cdr %>%
  filter(!is.na(leukoaraiosis))

## Create a visit count for each ID
rads <- rads %>%
  group_by(ID) %>%
  arrange(MR_Date) %>%
  mutate(visit_ct_rads = row_number()) %>%
  ungroup()

## Create a last visit indicator for each ID
rads <- rads %>%
  group_by(ID) %>%
  arrange(MR_Date) %>%
  mutate(visit_last_rads = ifelse(row_number() == n(), 1, 0)) %>%
  ungroup()
###################################################################################
## reassign variables
rads=subset(rads, rads$visit_last_rads==1)
rads$cdr <- as.numeric(rads$cdr)
rads$cdr_3grp<- as.numeric(rads$cdr) 
rads$cdr_3grp[rads$cdr > 0.5] <- '1+'
rads$cdr_3grp <- factor(rads$cdr_3grp, levels = c("0", "0.5", "1+"))
table(rads$cdr_3grp)
rads$cdr_cat<- as.numeric(rads$cdr)
rads$cdr_cat[rads$cdr > 0] <- '>0'
rads$cdr_cat <- factor(rads$cdr_cat, levels = c("0", ">0"))
table(rads$cdr_cat)
rads$cdr <- factor(rads$cdr, levels = c("0", "0.5", "1", "2"))
table(rads$cdr)

################### PREP VARIBLE OF INTEREST ##############################################

#leukoariasis
rads$leukoaraiosis <- factor(rads$leukoaraiosis, levels = c("None", "Mild", "Moderate", "Severe"))
rads$leuko <- NA
rads$leuko[rads$leukoaraiosis == "None"] <- 'None'
rads$leuko[rads$leukoaraiosis == "Mild"] <- 'Mild'
rads$leuko[rads$leukoaraiosis == "Moderate"] <- 'Moderate to severe'
rads$leuko[rads$leukoaraiosis == "Severe"] <- 'Moderate to severe'
rads$leuko <- factor(rads$leuko, levels = c("None", "Mild", "Moderate to severe"))
table(rads$leuko)

#hippocampal atrophy
table(rads$hippocampal_atrophy)
rads$hippocampal_atrophy <- factor(rads$hippocampal_atrophy, levels = c("None", "Mild", "Moderate", "Severe"))
rads$hipp <- NA
rads$hipp[rads$hippocampal_atrophy == "None"] <- 'None'
rads$hipp[rads$hippocampal_atrophy == "Mild"] <- 'Mild'
rads$hipp[rads$hippocampal_atrophy == "Moderate"] <- 'Moderate to severe'
rads$hipp[rads$hippocampal_atrophy == "Severe"] <- 'Moderate to severe'
rads$hipp <- factor(rads$hipp, levels = c("None", "Mild", "Moderate to severe"))
table(rads$hipp)

#cortical atrophy
table(rads$cortical_atrophy)
rads$cortical_atrophy <- factor(rads$cortical_atrophy, levels = c("None", "Mild", "Moderate", "Severe"))
rads$cort <- NA
rads$cort[rads$cortical_atrophy == "None"] <- 'None'
rads$cort[rads$cortical_atrophy == "Mild"] <- 'Mild'
rads$cort[rads$cortical_atrophy == "Moderate"] <- 'Moderate to severe'
rads$cort[rads$cortical_atrophy == "Severe"] <- 'Moderate to severe'
rads$cort <- factor(rads$cort, levels = c("None", "Mild", "Moderate to severe"))
table(rads$cort)

#Count data - categorizing/binarizing these variables
#microbleeds
table(rads$microbleeds)
rads$microbleeds<-as.numeric(rads$microbleeds)
rads$cmb <- NA
rads$cmb[rads$microbleeds == 0] <- 'None'
rads$cmb[rads$microbleeds == 1] <- 'Mild'
rads$cmb[rads$microbleeds == 5] <- 'Moderate to severe'
rads$cmb[rads$microbleeds == 11] <- 'Moderate to severe'
rads$cmb <- factor(rads$cmb, levels = c("None", "Mild", "Moderate to severe"))
table(rads$cmb)
rads$cmbpres <- NA
rads$cmbpres[rads$microbleeds == 0] <- '0'
rads$cmbpres[rads$microbleeds > 0] <- '1'
table(rads$cmbpres)
table(rads$cmbpres,rads$cdr_cat)
table(rads$cmbpres,rads$cdr_3grp)

#infarcts - large
table(rads$large_infarcts,rads$cdr_cat)#not enough data in each category
rads$large_infarcts<-as.integer(rads$large_infarcts)
#infacrt - small
table(rads$small_infarcts,rads$cdr_cat)#same not really enough data for a reliable test
rads$small_infarcts<-as.numeric(rads$small_infarcts)
rads$s_inf <- NA
rads$s_inf[rads$small_infarcts == 0] <- '0'
rads$s_inf[rads$small_infarcts > 0] <- '1'
table(rads$s_inf)
table(rads$s_inf, rads$cdr_cat)

#combine - presence of infarct (large of small)
rads$inf <- 'Yes'
rads$inf[rads$small_infarcts ==0 & rads$large_infarcts ==0] <- 'No'
table(rads$inf)
table(rads$inf, rads$cdr_cat)


################## CHI-SQUARE TESTS + VISUALIZATION #########################################
#chi-square test of independence to assess whether leukoaraiosis is related to the CDR status

#leukoaraiosis (USED IN PAPER!)
ct <- xtabs(~leuko+cdr_cat, rads)
CrossTable(ct, expected = TRUE, 
           prop.c = TRUE, prop.r = FALSE, prop.t = FALSE
           , fisher = TRUE)
#posthoc
chisq.posthoc.test(ct, method = "bonferroni")

#hippocampal atrophy
ct2 <- xtabs(~hipp+cdr_cat, rads)
CrossTable(ct2, expected = TRUE, 
           prop.c = TRUE, prop.r = FALSE, prop.t = FALSE
           ,fisher = TRUE)
#posthoc
chisq.posthoc.test(ct2, method = "bonferroni")

#cortical atrophy
ct3 <- xtabs(~cort+cdr_cat, rads)
CrossTable(ct3, expected = TRUE, 
           prop.c = TRUE, prop.r = FALSE, prop.t = FALSE
           ,fisher = TRUE)
#posthoc
chisq.posthoc.test(ct3, method = "bonferroni")

#microbleeds severity
ct4 <- xtabs(~cmb+cdr_cat, rads)
CrossTable(ct4, expected = TRUE, 
           prop.c = TRUE, prop.r = FALSE, prop.t = FALSE
           ,fisher = TRUE)
#posthoc
chisq.posthoc.test(ct4, method = "bonferroni")

#microbleed present per CDR status (USED IN PAPER!)
rads$cmbpres<-as.factor(rads$cmbpres)
ct6 <- xtabs(~cmbpres+cdr_cat, rads)
CrossTable(ct6, expected = TRUE, 
           prop.c = TRUE, prop.r = FALSE, prop.t = FALSE
           ,fisher = TRUE)
### Because Age is a main factor - glm is better suited to adjust for this
glm6 <- glm(cmbpres ~ Age + cdr_cat, data=rads, family=binomial) ## age+seq as covariates
summary(glm6)

#microbleed present per CDR score
ct7 <- xtabs(~cmbpres+cdr_3grp, rads)
CrossTable(ct7, expected = TRUE, 
           prop.c = TRUE, prop.r = FALSE, prop.t = FALSE
           ,fisher = TRUE)
#posthoc
chisq.posthoc.test(ct7, method = "bonferroni")
#check with accounting for age in glm
glm7 <- glm(cmbpres ~ Age + cdr_3grp, data=rads, family=binomial) ## age+seq as covariates
summary(glm7)

#infarct present (small or large) (USED IN PAPER!)
ct5 <- xtabs(~inf+cdr_cat, rads)
CrossTable(ct5, expected = TRUE,
           prop.c = TRUE, prop.r = FALSE, prop.t = FALSE
           ,fisher = TRUE)


### Because Age is a main factor - glm is better suited to adjust for this
# rerun of evaluation for 
rads$cmbpres<-as.factor(rads$cmbpres)