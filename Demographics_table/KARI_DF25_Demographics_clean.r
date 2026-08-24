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
list.of.packages <- c("tidyverse", "readxl", "effectsize")

## Check if they are installed and install them if they are not
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load all packages
lapply(list.of.packages, require, character.only = TRUE)

#################################################################################
## Set Paths
inPATH <- paste0("B:/path/synthetic_data/")

## Load scan_type data with header info
kari_raw <- read_csv(paste0(inPATH,"KARI_scan_mapping_DF25.csv"))

## Trim Kari
kari <- kari_raw %>% 
  mutate(AcquisitionDateTime = as.POSIXct(AcquisitionDateTime, format = "%Y-%m-%d %H:%M:%S")) %>% 
  mutate(MRdate = as.Date(AcquisitionDateTime), Time = format(AcquisitionDateTime, format = "%H:%M:%S")) %>%
  filter(grepl("MR", Modality)) %>%
  drop_na(MRdate) %>%
  select(MAPID=subject_label, experiment_label, scan_type, MRdate) 

## Collapse across experiment_label while retaining the MRdate column
mri <- kari %>%
  group_by(experiment_label) %>%
  distinct(experiment_label, .keep_all = TRUE) %>%
  select(MAPID, experiment_label, MRdate) %>%
  rename(ID = MAPID, Session = experiment_label, Date = MRdate) %>%
  ungroup()

## Add PET data
## Amyloid
av45 <- read_excel(paste0(inPATH,"av45.xlsx"))
av45 <- av45 %>%
  mutate(PET_Date = as.Date(PET_Date)) %>%
  group_by(PET_Session) %>%
  distinct(PET_Session, .keep_all = TRUE) %>%
  select(ID, PET_Session, PET_Date) %>%
  rename(Session = PET_Session, Date = PET_Date) %>%
  ungroup()

fbb <- read_excel(paste0(inPATH,"fbb.xlsx"))
fbb <- fbb %>%
  mutate(PET_Date = as.Date(PET_Date)) %>%
  group_by(PET_Session) %>%
  distinct(PET_Session, .keep_all = TRUE) %>%
  select(ID, PET_Session, PET_Date) %>%
  rename(Session = PET_Session, Date = PET_Date) %>%
  ungroup()

pib <- read_excel(paste0(inPATH,"pib.xlsx"))
pib <- pib %>%
  mutate(PET_Date = as.Date(PET_Date)) %>%
  group_by(PET_Session) %>%
  distinct(PET_Session, .keep_all = TRUE) %>%
  select(ID, PET_Session, PET_Date) %>%
  rename(Session = PET_Session, Date = PET_Date) %>%
  ungroup()

## Combine ID and PET_Session for each av45, fbb, and pib
amy <- bind_rows(
  av45 %>% select(ID, Session, Date),
  pib %>% select(ID, Session, Date),
  fbb %>% select(ID, Session, Date)
)

## Tau
tau <- read_excel(paste0(inPATH,"tau.xlsx"))
tau <- tau %>%
  mutate(PET_Date = as.Date(PET_Date)) %>%
  group_by(PET_Session) %>%
  distinct(PET_Session, .keep_all = TRUE) %>%
  select(ID, PET_Session, PET_Date) %>%
  rename(Session = PET_Session, Date = PET_Date) %>%
  ungroup()

## FDG
fdg <- read_excel(paste0(inPATH,"fdg.xlsx"))
fdg <- fdg %>%
  mutate(PET_Date = as.Date(PET_Date)) %>%
  group_by(PET_Session) %>%
  distinct(PET_Session, .keep_all = TRUE) %>%
  select(ID, PET_Session, PET_Date) %>%
  rename(Session = PET_Session, Date = PET_Date) %>%
  ungroup()

## Combine all rows from mri, amy, tau, and fdg 
combined_df <- bind_rows(mri, amy, tau, fdg)

# Remove duplicate Session values, keeping only the first occurrence
combined_df <- combined_df %>% distinct(Session, .keep_all = TRUE)

## Demographics
dem <- read_excel(paste0(inPATH,"demographics.xlsx")) %>%
  select(ID, SES, BIRTH, DEATH, ENTDATE, EDUC, HAND, grantc, race, sex)

## Clinical
cdr <- read_excel(paste0(inPATH,"b4_cdr.xlsx"))%>%
  select(ID, TESTDATE, cdr)%>%
  mutate(CDR_Date = as.Date(TESTDATE, origin = "1899-12-30"))%>%
  select(-TESTDATE)
  
## APOE
apoe <- read_excel(paste0(inPATH,"apoe.xlsx"))%>%
  select(ID=id, apoe)
############################################################################
############################################################################
## Add CDR data from clinical
## Ensure Date columns are in Date format
cdr$CDR_Date <- as.Date(cdr$CDR_Date)

## Create an empty column for the closest Date and CDR
combined_df_cdr <- combined_df
combined_df_cdr$ClosestCDR <- NA
combined_df_cdr$CDR <- NA

# Loop through each row in combined_df_cdr
for (i in 1:nrow(combined_df_cdr)) {
  img_ID <- combined_df_cdr$ID[i]
  ## Test if img_ID is in clin$ID
  if (img_ID %in% cdr$ID) {
  } else {
    next
  }
  # Subset clin for the current participant
  subset_cdr <- cdr[cdr$ID == combined_df_cdr$ID[i], ]
  
  # Find the index of the closest Date
  closest_index <- which.min(abs(subset_cdr$CDR_Date - combined_df_cdr$Date[i]))
  
  # Assign the closest Date and CDR to the data frame
  combined_df_cdr$ClosestCDR[i] <- subset_cdr$CDR_Date[closest_index]
  combined_df_cdr$CDR[i] <- subset_cdr$cdr[closest_index]
}

combined_df_cdr$ClosestCDR <- as.Date(combined_df_cdr$ClosestCDR)
combined_df_cdr <- combined_df_cdr %>% drop_na(CDR)

## Calculate time between CDR date and MR date
combined_df_cdr$CDR_gap <- as.numeric(difftime(combined_df_cdr$Date, combined_df_cdr$ClosestCDR, units = "days") / 365.25)

## Remove rows where CDR is more than 2 years from MR date
combined_df_cdr <- combined_df_cdr %>% filter(abs(CDR_gap) <= 2)

############################################################################
############################################################################
## Calculate baseline age
combined_df_cdr_baseage <- combined_df_cdr %>%
  left_join(dem, by = "ID") %>%
  mutate(Age = as.numeric(difftime(Date, BIRTH, units = "days")/365.25)) %>%
  group_by(ID) %>%
  slice_min(Date, with_ties = FALSE) %>%
  mutate(CDR_factor = case_when(CDR == 0 ~ "Unimpaired", CDR >= 0.5 ~ "Impaired")) %>%
  ungroup()

## Calculate Age min, max, and standard deviation
age_range <- range(combined_df_cdr_baseage$Age)
age_sd <- sd(combined_df_cdr_baseage$Age)
mean_age <- mean(combined_df_cdr_baseage$Age)

## Create age mean and sd for each CDR factor
age_cdr_unimpaired <- combined_df_cdr_baseage %>%
  filter(CDR_factor == "Unimpaired") %>%
  summarise(mean_age = mean(Age), sd_age = sd(Age), n = n())

age_cdr_impaired <- combined_df_cdr_baseage %>%
  filter(CDR_factor == "Impaired") %>%
  summarise(mean_age = mean(Age), sd_age = sd(Age), n = n())

## Calculate stats for Age
Age_T <- t.test(Age ~ CDR_factor, data = combined_df_cdr_baseage)
Age_T$CD <- cohens_d(combined_df_cdr_baseage$Age ~ combined_df_cdr_baseage$CDR_factor)$Cohens_d

## Pull Highest CDR for each ID
combined_df_cdr_max <- combined_df_cdr %>%
  group_by(ID) %>%
  slice_max(order_by = CDR, with_ties = FALSE) %>%
  ungroup()

## Combine demographic data
combined_df_cdr_max_dem <- combined_df_cdr_max %>%
    left_join(dem, by = c("ID" = "ID")) %>%
    mutate(Age = as.numeric(difftime(Date, BIRTH, units = "days")/365.25)) %>%
    mutate(CDR_factor = case_when(CDR == 0 ~ "Unimpaired", CDR >= 0.5 ~ "Impaired"))

## Recode handedness to R=Right, L=left, and O=Other
combined_df_cdr_max_dem <- combined_df_cdr_max_dem %>%
  mutate(HAND = case_when(
    HAND %in% c("L", "R") ~ HAND,   # Keep "L" and "R" as is
    HAND %in% c("?", "B", "O") ~ "O",  # Change "?", "B", "O", and others to "O"
    TRUE ~ "O"  # Any other value not covered in the above conditions will be changed to "O"
))

## Combine apoe
combined_df_cdr_max_dem <- combined_df_cdr_max_dem %>%
  left_join(apoe, by = c("ID" = "ID"))

combined_df_cdr_max_dem$apoe <- as.character(combined_df_cdr_max_dem$apoe)

## If apoe column contains a 4, then set that cell to "e4p", otherwise set it to "e4n"
combined_df_cdr_max_dem$apoe_factor <- ifelse(grepl("4", combined_df_cdr_max_dem$apoe), "e4p", "e4n")

## Clean data further for stats
table.df <- combined_df_cdr_max_dem %>%
  select(ID, Date, ClosestCDR, CDR, CDR_factor, Age, sex, HAND, EDUC, race, apoe, apoe_factor ) %>%
  mutate(sex=as.factor(sex), HAND=as.factor(HAND), apoe=as.factor(apoe), CDR_factor=as.factor(CDR_factor)) %>%
  group_by(CDR_factor) 

## if table.df$race is not "White", "Black", or "ASIAN" then set it to "Other"
table.df$race <- as.factor(ifelse(table.df$race %in% c("White", "Black", "ASIAN"), table.df$race, "Other"))

## Summary stats
summary_stats_mean_SD <- table.df %>%
  summarise(
    count = n(),
    Age_mean = mean(Age, na.rm = TRUE),
    Age_SD = sd(Age, na.rm = TRUE),
    EDUC_mean = mean(EDUC, na.rm = TRUE),
    EDUC_SD = sd(EDUC, na.rm = TRUE),
    
    sex_F_sum = sum(sex == "F"),
    sex_M_sum = sum(sex == "M"),
    sex_F_per = sum(sex == "F") / n(),
    sex_M_per = sum(sex == "M") / n(),
    HAND_R_sum = sum(HAND == "R"),
    HAND_L_sum = sum(HAND == "L"),
    HAND_O_sum = sum(HAND == "O"),
    HAND_R_per = sum(HAND == "R") / n(),
    HAND_L_per = sum(HAND == "L") / n(),
    HAND_O_per = sum(HAND == "O") / n(),
    race_W_sum = sum(race == "White"),
    race_W_per = sum(race == "White") / n(),
    race_B_sum = sum(race == "Black"),
    race_B_per = sum(race == "Black") / n(),
    race_A_sum = sum(race == "ASIAN"),
    race_A_per = sum(race == "ASIAN") / n(),
    race_O_sum = sum(race == "Other"),
    race_O_per = sum(race == "Other") / n(),
    apoe_e4p_sum = sum(apoe_factor == "e4p"),
    apoe_e4n_sum = sum(apoe_factor == "e4n"),
    apoe_e4p_per = sum(apoe_factor == "e4p") / n(),
    apoe_e4n_per = sum(apoe_factor == "e4n") / n(),
  )
summary_stats_mean_SD <- t(summary_stats_mean_SD)

# T-tests for continuous variables
EDUC_T <- t.test(EDUC ~ CDR_factor, data = table.df)
EDUC_T$CD <- cohens_d(table.df$EDUC ~ table.df$CDR_factor)$Cohens_d

# Chi-square tests for factors
sex_chi2_table <- table(table.df$sex, table.df$CDR_factor)
sex_chi2 <- chisq.test(sex_chi2_table)
sex_phi <- sqrt(sex_chi2$statistic / sum(sex_chi2_table))
sex_CV <- sqrt(sex_chi2$statistic / sum(sex_chi2_table) * (min(dim(sex_chi2_table)) - 1))

HAND_chi2_table <- table(table.df$HAND, table.df$CDR_factor)
HAND_chi2 <- chisq.test(HAND_chi2_table)
HAND_phi <- sqrt(HAND_chi2$statistic / sum(HAND_chi2_table))
HAND_CV <- sqrt(HAND_chi2$statistic / sum(HAND_chi2_table) * (min(dim(HAND_chi2_table)) - 1))

race_chi2_table <- table(table.df$race, table.df$CDR_factor)
race_chi2 <- chisq.test(race_chi2_table)
race_phi <- sqrt(race_chi2$statistic / sum(race_chi2_table))
race_CV <- sqrt(race_chi2$statistic / sum(race_chi2_table) * (min(dim(race_chi2_table)) - 1))

apoe_chi2_table <- table(table.df$apoe_factor, table.df$CDR_factor)
apoe_chi2 <- chisq.test(apoe_chi2_table)  
apoe_phi <- sqrt(apoe_chi2$statistic / sum(apoe_chi2_table))
apoe_CV <- sqrt(apoe_chi2$statistic / sum(apoe_chi2_table) * (min(dim(apoe_chi2_table)) - 1))

# Combine all results into a single data frame
summary_stats_p_ES <- bind_rows(
  data.frame(
    variable = c("Scan_Age", "EDUC"),
    p_value = c(Age_T$p.value, EDUC_T$p.value),
    ES = c(round(Age_T$CD,digits=4),round(EDUC_T$CD,digits=4))
  ),
  data.frame(
    variable = c("sex", "HAND", "race", "apoe"),
    p_value = c(sex_chi2$p.value, HAND_chi2$p.value, race_chi2$p.value, apoe_chi2$p.value),
    ES = c(round(sex_phi,digits=4), round(HAND_phi,digits=4), round(race_phi,digits=4), round(apoe_phi,digits=4))
  )
)

## Remove scientific notation from p-values
summary_stats_p_ES$p_value2 <- format(summary_stats_p_ES$p_value, scientific = FALSE)

## count number of table.df$sex == M when table.df$CDR_factor == Impaired
summary_stats_p_ES$sex_M_imp <- sum(table.df$sex == "M" & table.df$CDR_factor == "Impaired")

## Count number of table.df$sex == F when table.df$CDR_factor == Impaired
summary_stats_p_ES$sex_F_imp <- sum(table.df$sex == "F" & table.df$CDR_factor == "Impaired")
