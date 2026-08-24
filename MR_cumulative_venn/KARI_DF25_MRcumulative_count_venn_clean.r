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
list.of.packages <- c("tidyverse", "readxl", "paletteer", "ggvenn")

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

## Re map values in the "scan-type" column from "T2star", "swi", and "gre" to "T2-swi-gre"
kari_all <- kari_raw %>% 
  mutate(scan_type = ifelse(scan_type %in% c("T2star", "swi", "gre"), "T2-star/SWI", scan_type))

## Within the scan_type column, capitalize acronyms such as asl to ASL, dwi to DWI, and bold-rest to BOLD-REST
kari_all <- kari_all %>% 
  mutate(scan_type = ifelse(scan_type == "asl", "ASL", scan_type)) %>% 
  mutate(scan_type = ifelse(scan_type == "dwi", "dMRI", scan_type)) %>% 
  mutate(scan_type = ifelse(scan_type == "bold-rest", "BOLD-rest", scan_type))

## Trim Kari
kari <- kari_all %>% 
  mutate(AcquisitionDateTime = as.POSIXct(AcquisitionDateTime, format = "%Y-%m-%d %H:%M:%S")) %>% 
  mutate(MRdate = as.Date(AcquisitionDateTime), Time = format(AcquisitionDateTime, format = "%H:%M:%S")) %>%
  filter(grepl("MR", Modality)) %>%
  drop_na(MRdate) %>%
  select(MAPID=subject_label, experiment_label, scan_type, ImageType, MRdate) 

## Remove instances where ImageType contains the string "derived" or "Derived"
kari_original <- kari %>% 
  filter(!grepl("derived", ImageType, ignore.case = TRUE))

## Remove duplicate scan_type within each experiment_label
kari_truncated <- kari_original %>%
  distinct(experiment_label, scan_type, .keep_all = TRUE)

## extract year from MRdate
kari_truncated$year <- as.numeric(format(kari_truncated$MRdate, "%Y"))

## Count the number of Tracer per PET_year
kari_counts <- kari_truncated %>%
  #Get unique PET_year and Tracer combinations
  distinct(year, scan_type) %>%
  #Create a complete grid of PET_year and Tracer
  complete(year, scan_type) %>%
  #Join with counts from original data
  left_join(
    kari_truncated %>%
      group_by(year, scan_type) %>%
      summarise(n = n(), .groups = "drop"),
    by = c("year", "scan_type")
  ) %>%
  #Replace NA with 0
  mutate(n = replace_na(n, 0)) %>%
  #Arrange and calculate cumulative sum
  arrange(scan_type, year) %>%
  group_by(scan_type) %>%
  mutate(cumulative_n = cumsum(n)) %>%
  ungroup()

## Filter for the important scan_types
kari_counts <- kari_counts %>% 
   filter(scan_type %in% c("T1w", "T2w", "T2-star/SWI", "FLAIR", "BOLD-rest", "dMRI", "ASL"))

## Determine stacked order
kari_counts <- kari_counts %>%
    mutate(scan_type = fct_relevel(scan_type, "BOLD-rest", "ASL", "dMRI", "FLAIR", "T2-star/SWI", "T2w", "T1w"))

## Edit color scale
custom_palette <- paletteer_d("rcartocolor::Prism")[3:9]

# Plot
p <- kari_counts %>% 
  ggplot( aes(x=year, y=n, fill=scan_type, text=scan_type)) +
    geom_area( ) +
    scale_fill_manual(values = custom_palette) +
    scale_x_continuous(breaks = seq(min(kari_counts$year), max(kari_counts$year), by = 2)) +
    labs(y = "Total Acquisitions", x="Year", fill="Scan Types") +
    theme(legend.position="none") +
    ggtitle("MRI Acquisitions by Year") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

# Plot
p2 <- kari_counts %>% 
  ggplot(aes(x = year, y = cumulative_n, fill = scan_type, text = scan_type)) +
  geom_area() +
  scale_fill_manual(values = custom_palette) +
  scale_x_continuous(breaks = seq(min(kari_counts$year), max(kari_counts$year), by = 2)) +
  labs(y = "Total Acquisitions", x = "Year", fill = "Scan Types") +
  theme(legend.position = "none") +
  ggtitle("Accumulation of MRI Acquisitions Over Time") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## Add PET data
## Amyloid
av45 <- read_excel(paste0(inPATH,"av45.xlsx"))
length(unique(av45$ID))
length(unique(av45$PET_Session))
av45long <- av45 %>%
  group_by(ID) %>%
  summarise(label_count = n_distinct(PET_Session)) %>%
  filter(label_count > 1) %>%
  group_by(label_count) %>%
  summarise(num_mapids = n())

fbb <- read_excel(paste0(inPATH,"fbb.xlsx"))
length(unique(fbb$ID))
length(unique(fbb$PET_Session))
fbblong <- fbb %>%
  group_by(ID) %>%
  summarise(label_count = n_distinct(PET_Session)) %>%
  filter(label_count > 1) %>%
  group_by(label_count) %>%
  summarise(num_mapids = n())

pib <- read_excel(paste0(inPATH,"pib.xlsx"))
length(unique(pib$ID))
length(unique(pib$PET_Session))
piblong <- pib %>%
  group_by(ID) %>%
  summarise(label_count = n_distinct(PET_Session)) %>%
  filter(label_count > 1) %>%
  group_by(label_count) %>%
  summarise(num_mapids = n())

## Combine ID and PET_Session for each av45, fbb, and pib
amy <- bind_rows(
  av45 %>% select(ID, PET_Session, PET_Date),
  pib %>% select(ID, PET_Session, PET_Date),
  fbb %>% select(ID, PET_Session, PET_Date)
)

longitudinal_time_amy <- amy %>%
  mutate(Date = as.Date(PET_Date)) %>%
  distinct(ID, PET_Session, Date) %>%
  arrange(ID, Date) %>%
  group_by(ID) %>%
  mutate(num_experiments = n()) %>%
  mutate(days_since_last = as.numeric(Date - lag(Date))) %>%
  filter(num_experiments > 2) %>%
  ungroup()

longitudinal_time_amy <- longitudinal_time_amy %>% filter(!is.na(days_since_last))
longitudinal_time_amy <- longitudinal_time_amy %>% filter(days_since_last >= 180)
mean(longitudinal_time_amy$days_since_last, na.rm = TRUE)
sd(longitudinal_time_amy$days_since_last, na.rm = TRUE)

## Amy
amylong <- amy %>%
  group_by(ID) %>%
  summarise(label_count = n_distinct(PET_Session)) %>%
  filter(label_count > 1) %>%
  group_by(label_count) %>%
  summarise(num_mapids = n())

## Tau
tau <- read_excel(paste0(inPATH,"tau.xlsx"))
length(unique(tau$ID))
length(unique(tau$PET_Session))
taulong <- tau %>%
  group_by(ID) %>%
  summarise(label_count = n_distinct(PET_Session)) %>%
  filter(label_count > 1) %>%
  group_by(label_count) %>%
  summarise(num_mapids = n())

longitudinal_time_tau <- tau %>%
  mutate(Date = as.Date(PET_Date)) %>%
  distinct(ID, PET_Session, Date) %>%
  arrange(ID, Date) %>%
  group_by(ID) %>%
  mutate(num_experiments = n()) %>%
  mutate(days_since_last = as.numeric(Date - lag(Date))) %>%
  filter(num_experiments > 1) %>%
  ungroup()

longitudinal_time_tau <- longitudinal_time_tau %>% filter(!is.na(days_since_last))
longitudinal_time_tau <- longitudinal_time_tau %>% filter(days_since_last >= 180)
mean(longitudinal_time_tau$days_since_last, na.rm = TRUE)
sd(longitudinal_time_tau$days_since_last, na.rm = TRUE)

## FDG
fdg <- read_excel(paste0(inPATH,"fdg.xlsx"))
length(unique(fdg$ID))
length(unique(fdg$PET_Session))
fdglong <- fdg %>%
  group_by(ID) %>%
  summarise(label_count = n_distinct(PET_Session)) %>%
  filter(label_count > 1) %>%
  group_by(label_count) %>%
  summarise(num_mapids = n())

longitudinal_time_fdg <- fdg %>%
  mutate(Date = as.Date(PET_Date)) %>%
  distinct(ID, PET_Session, Date) %>%
  arrange(ID, Date) %>%
  group_by(ID) %>%
  mutate(num_experiments = n()) %>%
  mutate(days_since_last = as.numeric(Date - lag(Date))) %>%
  filter(num_experiments > 1) %>%
  ungroup()

longitudinal_time_fdg <- longitudinal_time_fdg %>% filter(!is.na(days_since_last))
longitudinal_time_fdg <- longitudinal_time_fdg %>% filter(days_since_last >= 180)
mean(longitudinal_time_fdg$days_since_last, na.rm = TRUE)
sd(longitudinal_time_fdg$days_since_last, na.rm = TRUE)

## Combine all to compare unique IDs
MRIIDs <- unique(kari$MAPID)
AMYIDs <- unique(c(av45$ID, fbb$ID, pib$ID))
TAUIDs <- unique(tau$ID)
FDGIDs <- unique(fdg$ID)
ALLIDs <- unique(c(MRIIDs, AMYIDs, TAUIDs, FDGIDs))

## Count the number of IDs that are unique to each modality
unique_to_MRIIDs <- setdiff(MRIIDs, union(AMYIDs, TAUIDs))
unique_to_AMYIDs <- setdiff(AMYIDs, union(MRIIDs, TAUIDs))
unique_to_TAUIDs <- setdiff(TAUIDs, union(MRIIDs, AMYIDs))

intersection_MRI_AMY <- intersect(MRIIDs, AMYIDs)
MRI_AMY_only <- setdiff(intersection_MRI_AMY, TAUIDs)
intersection_MRI_TAU <- intersect(MRIIDs, TAUIDs)
MRI_TAU_only <- setdiff(intersection_MRI_TAU, AMYIDs)
intersection_AMY_TAU <- intersect(AMYIDs, TAUIDs)
AMY_TAU_only <- setdiff(intersection_AMY_TAU, MRIIDs)

common_to_all <- Reduce(intersect, list(MRIIDs, AMYIDs, TAUIDs))

## Combine all to compare unique sessions
MRIses <- unique(kari$experiment_label)
AMYses <- unique(c(av45$PET_Session, fbb$PET_Session, pib$PET_Session))
TAUses <- unique(tau$PET_Session)
FDGses <- unique(fdg$PET_Session)
ALLses <- unique(c(MRIses, AMYses, TAUses, FDGses))

## Create a Venn diagram to compare unique IDs
venn_data <- list(
  MRI = MRIIDs,
  Amyloid = AMYIDs,
  Tau = TAUIDs,
  FDG = FDGIDs
)

names(venn_data) <- c("3T-MRI", "Amyloid-PET", "Tau-PET", "FDG-PET")

## Edit color scale
custom_palette <- paletteer_d("rcartocolor::Prism")[c(3, 5, 7, 9)]
ggvenn(
    venn_data, 
    fill_color = custom_palette,
    fill_alpha = 0.6,
    stroke_size = 0.1, set_name_size = 4,
    show_percentage = FALSE,
)