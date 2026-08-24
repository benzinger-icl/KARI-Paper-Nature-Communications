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
list.of.packages <- c("tidyverse", "readxl", "paletteer")

## Check if they are installed and install them if they are not
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load all packages
lapply(list.of.packages, require, character.only = TRUE)

#################################################################################
## Set Paths
inPATH <- paste0("B:/path/synthetic_data/")

# Load PET data
## Amyloid
FBP <- read_excel(paste0(inPATH,"av45.xlsx")) %>%
    ## Change AV45 in Tracer column to FBP
    mutate(Tracer = "FBP")

fbb <- read_excel(paste0(inPATH,"fbb.xlsx"))

pib <- read_excel(paste0(inPATH,"pib.xlsx"))

## Tau
tau <- read_excel(paste0(inPATH,"tau.xlsx")) %>%
    select(ID, Tracer, PET_Session, PET_Date) %>%
    ## Change AV1451 in Tracer column to FTP
    mutate(Tracer = "FTP")

## FDG
fdg <- read_excel(paste0(inPATH,"fdg.xlsx")) %>%
    select(ID, Tracer, PET_Session, PET_Date)

## Combine ID and PET_Session for each av45, fbb, and pib
PET <- bind_rows(
  FBP %>% select(ID, Tracer, PET_Session, PET_Date),
  pib %>% select(ID, Tracer, PET_Session, PET_Date),
  fbb %>% select(ID, Tracer, PET_Session, PET_Date),
  tau %>% select(ID, Tracer, PET_Session, PET_Date),
  fdg %>% select(ID, Tracer, PET_Session, PET_Date)
)

## extract PET_year from MRdate
PET <- PET %>%
    mutate(PET_Date = as.Date(PET_Date)) %>%
    mutate(PET_year = as.numeric(format(PET$PET_Date, "%Y")))

## Count the number of Tracer per PET_year
PET_counts <- PET %>%
  #Get unique PET_year and Tracer combinations
  distinct(PET_year, Tracer) %>%
  #Create a complete grid of PET_year and Tracer
  complete(PET_year, Tracer) %>%
  #Join with counts from original data
  left_join(
    PET %>%
      group_by(PET_year, Tracer) %>%
      summarise(n = n(), .groups = "drop"),
    by = c("PET_year", "Tracer")
  ) %>%
  #Replace NA with 0
  mutate(n = replace_na(n, 0)) %>%
  #Arrange and calculate cumulative sum
  arrange(Tracer, PET_year) %>%
  group_by(Tracer) %>%
  mutate(cumulative_n = cumsum(n)) %>%
  ungroup()


## Determine stacked order
PET_counts <- PET_counts %>%
    mutate(Tracer = fct_relevel(Tracer, "FDG", "AV-1451", "Florbetaben", "FBP", "PIB"))

## Edit color scale
custom_palette <- paletteer_d("rcartocolor::Prism")[c(8,1,3,4,2)]


# Plot
p <- PET_counts %>% 
  ggplot( aes(x=PET_year, y=n, fill=Tracer, text=Tracer)) +
    geom_area( ) +
    scale_fill_manual(values = custom_palette) +
    scale_x_continuous(breaks = seq(min(PET_counts$PET_year), max(PET_counts$PET_year), by = 2)) +
    labs(y = "Total Acquisitions", x="Year", fill="Tracer") +
    theme(legend.position="none") +
    ggtitle("PET Acquisitions by Year") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))

# Plot
p2 <- PET_counts %>% 
  ggplot(aes(x = PET_year, y = cumulative_n, fill = Tracer, text = Tracer)) +
  geom_area() +
  scale_fill_manual(values = custom_palette) +
  scale_x_continuous(breaks = seq(min(PET_counts$PET_year), max(PET_counts$PET_year), by = 2)) +
  labs(y = "Total Acquisitions", x = "Year", fill = "Tracer") +
  theme(legend.position = "none") +
  ggtitle("Accumulation of PET Acquisitions Over Time") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))