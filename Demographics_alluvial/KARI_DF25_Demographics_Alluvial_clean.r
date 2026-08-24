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
list.of.packages <- c("tidyverse", "readxl", "ggalluvial", "paletteer")

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

# ## Identify mismatches between experiment_label and MRdate
# kari_inconsistent <- kari %>%
#   drop_na() %>%
#   group_by(experiment_label) %>%
#   filter(n_distinct(MRdate) > 1) %>%
#   ungroup()

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
cdr <- read_excel(paste0(inPATH,"b4_cdr.xlsx")) %>%
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

## Calculate time between CDR date and scan date
combined_df_cdr$CDR_gap <- as.numeric(difftime(combined_df_cdr$Date, combined_df_cdr$ClosestCDR, units = "days") / 365.25)

## Remove rows where CDR is more than 2 years from MR date
combined_df_cdr <- combined_df_cdr %>% filter(abs(CDR_gap) <= 2)

############################################################################
############################################################################
## Pull Closest CDR for each ID
combined_df_cdr_max <- combined_df_cdr %>%
  group_by(ID) %>%
  summarize(CDR = max(CDR, na.rm = TRUE), across(everything(), first)) %>%
  select(ID, Session, Date, ClosestCDR, CDR_gap, CDR) %>%
  mutate(CDR_factor = case_when(CDR == 0 ~ "Unimpaired", CDR >= 0.5 ~ "Impaired")) %>%
  ungroup()

## Combine demographic data
combined_df_cdr_max_dem <- combined_df_cdr_max %>%
    left_join(dem, by = c("ID" = "ID"))

## Calculate age at scan
combined_df_cdr_max_dem$Scan_Age <- as.numeric(difftime(combined_df_cdr_max_dem$Date, combined_df_cdr_max_dem$BIRTH, units = "days")/365.25)

## Calculate the min, max, and standard deviation for Scan_Age
age_range <- range(combined_df_cdr_max_dem$Scan_Age)
age_sd <- sd(combined_df_cdr_max_dem$Scan_Age)

#################################################################################
#################################################################################
## Alluvial plot (age, sex, race, CDR)
## Create age group break points for 3 groups
breaks <- quantile(combined_df_cdr_max_dem$Scan_Age, probs = c(0, 1/3, 2/3, 1),names=FALSE)
agemin <- breaks[1]
agelower <- breaks[2]
ageupper <- breaks[3]
agemax <- breaks[4]

strlow <- paste0(floor(agemin),'-',floor(agelower))
strmid <- paste0(floor(agelower),'-',floor(ageupper))
strhigh <- paste0(floor(ageupper),'-',floor(agemax))

## Build data frame for alluvial plot
alluvial.df <- combined_df_cdr_max_dem %>%
  select(CDR, Scan_Age, sex, race)%>%
  mutate(Age = factor(case_when(Scan_Age >= agemin & Scan_Age <= agelower ~ strlow, Scan_Age >= agelower & Scan_Age <= ageupper ~ strmid, Scan_Age >= ageupper & Scan_Age <= agemax ~ strhigh)), Sex = factor(str_replace_all(sex, c("M" = "Male", "F" = "Female"))), CDR = factor(case_when(CDR == 0 ~ "Unimpaired", CDR >= 0.5 ~ "Impaired")), Race = if_else(race %in% c("White", "Black", "ASIAN", "more than one"), race,"Non-White")) %>%
  mutate(Race = factor(str_replace_all(Race, c("Black" = "Non-White", "ASIAN" = "Non-White", "more than one" = "Non-White"))))

  ## Reorder factors for CDR so that Unimpaired is first
alluvial.df$Race <- factor(alluvial.df$Race, levels = c("White", "Non-White"))
alluvial.df$CDR <- factor(alluvial.df$CDR, levels = c("Impaired","Unimpaired"))
alluvial.df$Age <- factor(alluvial.df$Age, levels = c(strhigh, strmid, strlow))

colorfill <- c("#6F4070FF", "#EDAD08FF")

## Attempt 2
ggplot(alluvial.df, aes(axis1 = Age, axis2 = Sex, axis3 = Race, axis4 = CDR)) +
  scale_x_discrete(limits = c("Age", "Sex", "Race", "CDR")) +
  geom_alluvium(aes(fill = CDR), stat = "alluvium", aes.bind = "alluvia") +
  geom_stratum(aes(fill = CDR), alpha = 1) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  labs(title = "Demographics According to CDR", y = "Number of Participants") + 
  scale_fill_manual(values = colorfill, na.translate = FALSE) +
  theme(panel.border = element_blank(), axis.line = element_line(),  
        plot.title = element_text(hjust = 0.5),  
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size = 12), axis.title = element_text(size=12), legend.position = "none")