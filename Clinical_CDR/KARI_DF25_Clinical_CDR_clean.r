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
list.of.packages <- c("tidyverse", "readxl")

## Check if they are installed and install them if they are not
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load all packages
lapply(list.of.packages, require, character.only = TRUE)

#################################################################################
## Set Paths
imgPATH <- paste0(B3path,"/hoagey/data_freezes/df27_prerelease/")
clinPATH <- paste0(B3path,"/hoagey/data_freezes/df26_april25/clinical_core")

## Import WMH
WMH <- read_excel(paste0(imgPATH,"WMH.xlsx"))

## Import PET data
## Amyloid
av45mrf <- read_excel(paste0(imgPATH,"av45_mrfreepet.xlsx"))
av45mrf <- av45mrf %>%
  mutate(PET_Date = as.Date(PET_Date)) %>%
  group_by(PET_Session) %>%
  distinct(PET_Session, .keep_all = TRUE) %>%
  select(ID, PET_Session, PET_Date, MRFreePET_Centiloid) %>%
  rename(Centiloid = MRFreePET_Centiloid) %>%
  filter(!is.na(Centiloid)) %>%
  ungroup()

fbbmrf <- read_excel(paste0(imgPATH,"fbb_mrfreepet.xlsx"))
fbbmrf <- fbbmrf %>%
  mutate(PET_Date = as.Date(PET_Date)) %>%
  group_by(PET_Session) %>%
  distinct(PET_Session, .keep_all = TRUE) %>%
  select(ID, PET_Session, PET_Date, MRFreePET_Centiloid) %>%
  rename(Centiloid = MRFreePET_Centiloid) %>%
  filter(!is.na(Centiloid)) %>%
  ungroup()

pibmrf <- read_excel(paste0(imgPATH,"pib_mrfreepet.xlsx"))
pibmrf <- pibmrf %>%
  mutate(PET_Date = as.Date(PET_Date)) %>%
  group_by(PET_Session) %>%
  distinct(PET_Session, .keep_all = TRUE) %>%
  select(ID, PET_Session, PET_Date, MRFreePET_Centiloid) %>%
  rename(Centiloid = MRFreePET_Centiloid) %>%
  filter(!is.na(Centiloid)) %>%
  ungroup()

## Combine ID and PET_Session for each av45, fbb, and pib
amy <- bind_rows(
  av45mrf %>% select(ID, PET_Session, PET_Date, Centiloid),
  pibmrf %>% select(ID, PET_Session, PET_Date, Centiloid),
  fbbmrf %>% select(ID, PET_Session, PET_Date, Centiloid)
)

## Clinical
physical <- read_excel(paste0(clinPATH,"b1_physical_eval.xlsx"))%>%
  select(ID, TESTDATE, WEIGHT, HEIGHT) %>%
  mutate(Phys_Date = as.Date(TESTDATE, origin = "1899-12-30"))

############################################################################
############################################################################
## Combine data of interest (sometimes mri and amyloid, other times its not)
combined_df <- amy %>%
  rename(Date = PET_Date, Session = PET_Session)

## Add CDR data from clinical
## Ensure Date columns are in Date format
combined_df$Date <- as.Date(combined_df$Date)
cdr$CDR_Date <- as.Date(cdr$CDR_Date)

## Create an empty column for the closest Date and CDR
kari_cdr <- combined_df
kari_cdr$ClosestCDR <- NA
kari_cdr$CDR <- NA

# Loop through each row in kari_cdr
for (i in 1:nrow(kari_cdr)) {
  img_ID <- kari_cdr$ID[i]
  ## Test if img_ID is in clin$ID
  if (img_ID %in% cdr$ID) {
  } else {
    next
  }
  # Subset clin for the current participant
  subset_cdr <- cdr[cdr$ID == kari_cdr$ID[i], ]
  
  # Find the index of the closest Date
  closest_index <- which.min(abs(subset_cdr$CDR_Date - kari_cdr$Date[i]))
  
  # Assign the closest Date and CDR to the data frame
  kari_cdr$ClosestCDR[i] <- subset_cdr$CDR_Date[closest_index]
  kari_cdr$CDR[i] <- subset_cdr$cdr[closest_index]
}

kari_cdr$ClosestCDR <- as.Date(kari_cdr$ClosestCDR)
kari_cdr <- kari_cdr %>% drop_na(CDR)

## Calculate time between CDR date and MR date
kari_cdr$CDR_gap <- as.numeric(difftime(kari_cdr$Date, kari_cdr$ClosestCDR, units = "days") / 365.25)

## Remove rows where CDR is more than 2 years from MR date
kari_cdr <- kari_cdr %>% filter(abs(CDR_gap) <= 2)

############################################################################
############################################################################
# Process the data to categorize CDR trends
result <- kari_cdr %>%
  group_by(ID) %>%
  arrange(Date) %>%
  mutate(CDR_change = CDR - lag(CDR)) %>%
  summarise(
    increase_count = sum(CDR_change > 0, na.rm = TRUE),
    decrease_count = sum(CDR_change < 0, na.rm = TRUE),
    stable_count = sum(CDR_change == 0, na.rm = TRUE),
    trend = case_when(
      increase_count > 0 & decrease_count == 0 ~ "increases",
      decrease_count > 0 & increase_count == 0 ~ "decreases",
      increase_count > 0 & decrease_count > 0 ~ "fluctuates",
      TRUE ~ "stable"
  )
)

## Remove rows where the trend column is no "stable"
result_stable_CC <- result %>%
  filter(trend == "stable") %>%
  filter(stable_count == 0) %>%
  left_join(kari_cdr %>% 
  select(ID, CDR), by = "ID")

result_stable_CC_table <- result_stable_CC %>%
  distinct(ID, CDR) %>%
  group_by(CDR) %>%
  summarise(Count = n())

## Remove rows where the trend column is stable but the stable_count is greater than 0
result_stable_long <- result %>%
  filter(trend == "stable") %>%
  filter(stable_count != 0) %>%
  left_join(kari_cdr %>% 
  select(ID, CDR), by = "ID")

result_stable_long_table <- result_stable_long %>%
  distinct(ID, CDR) %>%
  group_by(CDR) %>%
  summarise(Count = n())

## Add trend column back to kari_cdr
kari_cdr <- kari_cdr %>%
  left_join(result, by = "ID")

# Condense the data by only keeping rows where the CDR value changes from the previous timepoint
kari_cdr_condensed <- kari_cdr %>%
  group_by(ID) %>%
  arrange(ID, Date) %>%  # Ensure the data is ordered by date
  filter(CDR != lag(CDR, default = first(CDR)) | row_number() == 1) %>%  # Keep rows where CDR is different or the first row
  mutate(timepoint_condensed = row_number())  # Re-assign new timepoints based on distinct CDR changes

## Categorize the different types of CDR change in the data
kari_cdr_types <- kari_cdr_condensed %>%
  summarize(CDR_sequence = paste(CDR, collapse = " -> "),
            trend = first(trend)) %>%  # Use first() to get the trend for each ID
  ungroup()

# Merge the change type back to the original data
kari_cdr <- kari_cdr %>%
  left_join(kari_cdr_types %>% 
  select(ID, CDR_sequence), by = "ID")

increase0to1 <- kari_cdr %>% 
  filter(CDR_sequence == "0 -> 0.5 -> 1")

# Count the number of participants (ID) for each unique CDR_sequence
cdr_sequence_counts <- kari_cdr_types %>%
  count(CDR_sequence, trend) %>%
  arrange(desc(n)) 

## Try Alluvial plot
## Reformat data so that each participant has a CDR for first, max, and last timepoint
kari_cdr_FML <- kari_cdr %>%
  # Group by ID
  group_by(ID) %>%
  # Summarize to get first, max, and last CDR scores
  summarise(
    first_CDR = CDR[which.min(ClosestCDR)],  # First CDR score based on earliest Date
    max_CDR = max(CDR, na.rm = TRUE),     # Maximum CDR score
    last_CDR = CDR[which.max(ClosestCDR)],    # Last CDR score based on latest Date
    max_Cent = max(Centiloid, na.rm = TRUE),  # Maximum Centiloid value
    .groups = 'drop'                              # Drop the grouping
  )


## Set first_CDR, max_CDR, and last_CDR as factors with the levels in the order of 0.0, 0.5, 1.0, 2.0
kari_cdr_FML$first_CDR <- factor(kari_cdr_FML$first_CDR, levels = c(0, 0.5, 1, 2))
kari_cdr_FML$max_CDR <- factor(kari_cdr_FML$max_CDR, levels = c(0, 0.5, 1, 2))
kari_cdr_FML$last_CDR <- factor(kari_cdr_FML$last_CDR, levels = c(0, 0.5, 1, 2))
kari_cdr_FML$max_Cent <- factor(ifelse(kari_cdr_FML$max_Cent > 18.41, "A+", "A-"))

## Remove rows with the same CDR for first, max, and last timepoints
kari_cdr_FML_converters <- kari_cdr_FML %>%
 filter(first_CDR != max_CDR | max_CDR != last_CDR)

data <- kari_cdr_FML_converters %>% 
  group_by(ID)%>%
  gather(TimePoint, CDR, first_CDR:last_CDR) %>%
  mutate(TimePoint = as.factor(TimePoint), 
         CDR = factor(CDR, levels = c("0", "0.5", "1", "2")), ID = as.factor(ID), 
         TimePoint = str_replace_all(TimePoint, c("first_CDR"= "Initial CDR", "max_CDR"= "Highest CDR", "last_CDR"= "Final CDR")), 
         TimePoint = factor(TimePoint, levels = c("Initial CDR", "Highest CDR", "Final CDR")),)

# Try simplified version of the plot...
ggplot(data, aes(x = TimePoint, stratum = CDR, alluvium = ID)) +
  geom_alluvium(aes(fill = max_Cent), stat = "alluvium", aes.bind = "alluvia") +
  scale_fill_manual(name = "Amyloid Status", values = c("A+" = "#6F4070FF", "A-" = "#EDAD08FF"), na.translate = FALSE) +
  geom_stratum(alpha=0) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  labs(title = "Changes in CDR Scores Throughout the Study", y = "Number of Participants", x="") +
  theme(panel.border = element_blank(), axis.line = element_line(),
        plot.title = element_text(hjust = 0.5),  
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size = 12), axis.title = element_text(size=12), legend.position = "right")

## #0F8554FF, #1D6996FF, #CC503EFF, #5F4690FF

paletteer_d("rcartocolor::Prism")

kari_cdr_FML2 <- kari_cdr %>%
  # Group by ID
  group_by(ID) %>%
  # Summarize to get first, max, and last CDR scores
  summarise(
    first_CDR = CDR[which.min(ClosestCDR)],  # First CDR score based on earliest Date
    max_CDR = max(CDR, na.rm = TRUE),     # Maximum CDR score
    last_CDR = CDR[which.max(ClosestCDR)],    # Last CDR score based on latest Date
    max_Cent = max(Centiloid, na.rm = TRUE),  # Maximum Centiloid value
    .groups = 'drop'                              # Drop the grouping
  )


## Set first_CDR, max_CDR, and last_CDR as factors with the levels in the order of 0.0, 0.5, 1.0, 2.0
kari_cdr_FML2$first_CDR <- factor(kari_cdr_FML2$first_CDR, levels = c(2, 1, 0.5, 0))
kari_cdr_FML2$max_CDR <- factor(kari_cdr_FML2$max_CDR, levels = c(2, 1, 0.5, 0))
kari_cdr_FML2$last_CDR <- factor(kari_cdr_FML2$last_CDR, levels = c(2, 1, 0.5, 0))
kari_cdr_FML2$max_Cent <- factor(ifelse(kari_cdr_FML2$max_Cent > 18.4, "A+", "A-"))

## Remove rows with the same CDR for first, max, and last timepoints
kari_cdr_FML_converters2 <- kari_cdr_FML2 %>%
 filter(first_CDR != max_CDR | max_CDR != last_CDR)

## Attempt 2
data2 <- kari_cdr_FML_converters2 %>% 
  group_by(ID)%>%
  mutate(colorx = as.factor(max_Cent)) %>%
  gather(TimePoint, CDR, first_CDR:max_Cent) %>%
  mutate(TimePoint = as.factor(TimePoint), 
         CDR = factor(CDR, levels = c("2", "1", "0.5", "0", "A+", "A-")), ID = as.factor(ID), 
         TimePoint = str_replace_all(TimePoint, c("first_CDR"= "Initial CDR", "max_CDR"= "Highest CDR", "last_CDR"= "Final CDR", "max_Cent" = "Amyloid")), 
         TimePoint = factor(TimePoint, levels = c("Initial CDR", "Highest CDR", "Final CDR", "Amyloid")),)


ggplot(data2, aes(x = TimePoint, stratum = CDR, alluvium = ID)) +
  geom_alluvium(aes(fill = colorx), stat = "alluvium", aes.bind = "alluvia", alpha=.5) +
  geom_stratum(aes(fill = after_stat(stratum)), alpha=1) +
  scale_fill_manual(name = "Amyloid Status", values = c("A+" = "#6F4070FF", "A-" = "#EDAD08FF"), na.translate = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 4) +
  labs(title = "Changes in CDR Scores by Amyloid-PET Positivity", y = "Number of Participants", x="") +
  theme(panel.border = element_blank(), axis.line = element_line(),
        plot.title = element_text(hjust = 0.5),  
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent",colour = NA),
        axis.text = element_text(size = 12), axis.title = element_text(size=12), legend.position = "none")