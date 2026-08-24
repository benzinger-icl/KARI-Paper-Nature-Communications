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
## 1. Cleaned Package List
list.of.packages <- c("tidyverse", "readxl", "psych", "ppcor", "paletteer", "lme4", "emmeans", "ggforce", "RVAideMemoire")

## 2. Check and Install
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## 3. Load
lapply(list.of.packages, require, character.only = TRUE)
#################################################################################
## Set Paths
inPATH <- paste0("B:/path/synthetic_data/")
#################################################################################

### Load cognition data
cog <- read_excel(paste0(inPATH,"/psychometrics.xlsx")) %>%
  dplyr::select(ID, psy_date, mocatots, craftdre, MEMUNITS, srtfree, asscmem, minttots, ANIMALS, VEG, BOSTON, tma, tmb, trailb, TRAILA, digsym, switchmixed, DIGIF, DIGIB, digforct, digbacct, lettnum) %>% 
  mutate(psy_Date = as.Date(psy_date)) %>% 
  dplyr::select(-psy_date) %>% 
  mutate(MEMUNITS = case_when(MEMUNITS <= 25 ~ MEMUNITS),switchmixed = case_when(switchmixed <= 52 ~ switchmixed))

full_data <- cog %>%
  mutate(BOSTON = as.numeric(BOSTON), minttots = as.numeric(minttots), eqMINT = case_when (
    minttots == 32 ~ 30, minttots == 31 ~ 29, minttots == 30 ~ 28,
    minttots == 29 ~ 27, minttots == 28 ~ 26, minttots == 27 ~ 25,
    minttots == 26 ~ 24, minttots == 25 ~ 22, minttots == 24 ~ 21,
    minttots == 23 ~ 20, minttots == 22 ~ 18, minttots == 21 ~ 17,
    minttots == 20 ~ 16, minttots == 19 ~ 15, minttots == 18 ~ 14,
    minttots == 17 ~ 13, minttots == 16 ~ 12, minttots == 15 ~ 11,
    minttots == 14 ~ 11, minttots == 13 ~ 10, minttots == 12 ~ 9,
    minttots == 11 ~ 9, minttots == 10 ~ 8, minttots == 9 ~ 8,
    minttots == 8 ~ 7, minttots == 7 ~ 7, minttots == 6 ~ 6,
    minttots == 5 ~ 6, minttots == 4 ~ 5, minttots == 3 ~ 4,
    minttots == 2 ~ 3, minttots == 1 ~ 2, minttots == 0 ~ 1,
    is.na(minttots) ~ BOSTON), 
    MEMUNITS = as.numeric(MEMUNITS), craftdre = as.numeric(craftdre),eqCRFTDel = case_when (
      craftdre == 25 ~ 24, craftdre == 24 ~ 23, craftdre == 23 ~ 22,     
      craftdre == 22 ~ 21, craftdre == 21 ~ 20, craftdre == 20 ~ 19, 
      craftdre == 19 ~ 18, craftdre == 18 ~ 16, craftdre == 17 ~ 15, 
      craftdre == 16 ~ 14, craftdre == 15 ~ 13, craftdre == 14 ~ 12, 
      craftdre == 13 ~ 11, craftdre == 12 ~ 10, craftdre == 11 ~ 9, 
      craftdre == 10 ~ 8, craftdre == 9 ~ 8, craftdre == 8 ~ 7, 
      craftdre == 7 ~ 6, craftdre == 6 ~ 5, craftdre == 5 ~ 5, 
      craftdre == 4 ~ 4, craftdre == 3 ~ 3, craftdre == 2 ~ 3, 
      craftdre == 1 ~ 1, craftdre == 0 ~ 0, is.na(craftdre) ~ MEMUNITS), 
    DIGIF = as.numeric(DIGIF), digforct = as.numeric(digforct), eqDIGFCT = case_when (
      digforct == 14 ~ 12, digforct == 13 ~ 12, 
      digforct == 12 ~ 11, digforct == 11 ~ 11, digforct == 10 ~ 10, 
      digforct == 9 ~ 9, digforct == 8 ~ 9, digforct == 7 ~ 8, 
      digforct == 6 ~ 7, digforct == 5 ~ 6, digforct == 4 ~ 5, 
      digforct == 3 ~ 3, digforct == 2 ~ 2, digforct == 1 ~ 1, 
      digforct == 0 ~ 0, is.na(digforct) ~ DIGIF), DIGIB = as.numeric(DIGIB), digbacct = as.numeric(digbacct), eqDIGBCT = case_when (
        digbacct == 14 ~ 12, digbacct == 13 ~ 12, digbacct == 12 ~ 12, 
        digbacct == 11 ~ 11, digbacct == 10 ~ 10, digbacct == 9 ~ 9, 
        digbacct == 8 ~ 8, digbacct == 7 ~ 7, digbacct == 6 ~ 6, 
        digbacct == 5 ~ 5, digbacct == 4 ~ 4, digbacct == 3 ~ 3, 
        digbacct == 2 ~ 2, digbacct == 1 ~ 1, digbacct == 0 ~ 0, 
        is.na(digbacct) ~ DIGIB), tma = as.numeric(tma), tmb = as.numeric(tmb), TRAILA = as.numeric(TRAILA), trailb = as.numeric(trailb), tmax = case_when(
          tma > 149 ~ 150, tma < 150 ~ tma), TRAILAx = case_when(
            TRAILA > 149 ~ 150, TRAILA < 150 ~ TRAILA), tmbx = case_when(
              tmb > 300 ~ 300, tmb < 300 ~ tmb), TRAILBx = case_when(
                trailb > 299 ~ 300, trailb < 300 ~ trailb),TRAILA_Final = case_when (
                  is.na(TRAILAx) ~ tmax, TRAILAx > 0 ~ TRAILAx), TRAILB_Final = case_when (
                    is.na(TRAILBx) ~ tmbx, TRAILBx > 0 ~ TRAILBx))%>% 
  dplyr::select(ID, psy_Date, srtfree, ANIMALS, TRAILB_Final, digsym)

full_data<- full_data%>%
  mutate(year = format(psy_Date, format = "%Y"))%>% 
  dplyr::filter(year > 2003)

m_TRAILB_Final <- mean(full_data$TRAILB_Final, na.rm = T); sd_TRAILB_Final <- sd(full_data$TRAILB_Final, na.rm = T); full_data$z_TRAILB_Final <- -1*(full_data$TRAILB_Final - m_TRAILB_Final)/sd_TRAILB_Final
m_ANIMALS <- mean(full_data$ANIMALS, na.rm = T); sd_ANIMALS <- sd(full_data$ANIMALS, na.rm = T); full_data$z_ANIMALS <- (full_data$ANIMALS - m_ANIMALS)/sd_ANIMALS
m_srtfree  <- mean(full_data$srtfree, na.rm = T); sd_srtfree <- sd(full_data$srtfree, na.rm = T); full_data$z_srtfree <- (full_data$srtfree - m_srtfree)/sd_srtfree
m_digsym  <- mean(full_data$digsym, na.rm = T); sd_digsym <- sd(full_data$digsym, na.rm = T); full_data$z_digsym <- (full_data$digsym - m_digsym)/sd_digsym

full_data$K1_na_count <- apply(full_data [, c("z_srtfree", "z_digsym", "z_ANIMALS", "z_TRAILB_Final")], 1, function(x) sum(is.na(x)))

index_srtfree <- grep("z_srtfree", colnames(full_data))
index_animals <- grep("z_ANIMALS", colnames(full_data))
index_tmb <- grep("z_TRAILB_Final", colnames(full_data))
index_digsym <- grep("z_digsym", colnames(full_data))

full_data$K1 <- rowMeans(full_data[, c(index_srtfree,index_animals,index_digsym, index_tmb)], na.rm = TRUE)
full_data$K1 = ifelse(full_data$K1_na_count <3, full_data$K1, NA)

desc <- data.frame(t(describe(full_data$K1)))
colnames(desc) <- "K1"

full_data <- full_data %>%
  mutate(K1 = case_when(K1 <= (desc$K1[3]+3.5*desc$K1[4]) & K1 >= (desc$K1[3]-3.5*desc$K1[4]) ~ K1))%>%
  dplyr::select(ID, psy_Date, year, K1)%>%
  dplyr::filter(year > 2003)

mri <- read_excel(paste0(inPATH,"mri_3t.xlsx")) %>% 
  mutate(mri_Date = as.Date(MR_Date), CortSig = LOAD_CorticalSignature_Thickness)%>% 
  dplyr::select(ID, mri_Date, CortSig)%>% 
  drop_na(CortSig)

# Read in TAU
tau <- read_excel(paste0(inPATH,"tau.xlsx")) %>% 
  mutate(tau_Date = as.Date(PET_Date)) %>% 
  dplyr::select(ID, tau_Date, Tauopathy) %>%
  drop_na(Tauopathy)

# Read in PIB
pib <- read_excel(paste0(inPATH,"pib_mrfreepet.xlsx")) %>% 
  mutate(ab_Date = as.Date(PET_Date), Tracer = "pib") %>% 
  dplyr::select(ID, ab_Date, Centiloid = MRFreePET_Centiloid, Tracer) %>% 
  drop_na(Centiloid)

av <- read_excel(paste0(inPATH,"av45_mrfreepet.xlsx")) %>% 
  mutate(ab_Date = as.Date(PET_Date), Tracer = "av") %>% 
  dplyr::select(ID, ab_Date, Centiloid = MRFreePET_Centiloid, Tracer) %>% 
  drop_na(Centiloid)

fbb <- read_excel(paste0(inPATH,"fbb_mrfreepet.xlsx")) %>% 
  mutate(ab_Date = as.Date(PET_Date), Tracer = "fbb") %>% 
  dplyr::select(ID, ab_Date, Centiloid = MRFreePET_Centiloid, Tracer) %>% 
  drop_na(Centiloid)

# merge the types of amyloid-PET
ab <- rbind(pib, av, fbb)

# Remove Outliers 
desc <- data.frame(t(describe(mri$CortSig))) 
mri <- mri %>% 
  mutate(CortSig = case_when(CortSig >= (desc$X1[3]-3*desc$X1[4]) & CortSig <= (desc$X1[3]+3*desc$X1[4])~ CortSig))

desc <- data.frame(t(describe(tau$Tauopathy)))
tau <- tau %>% 
  mutate(Tauopathy = case_when(Tauopathy <= (desc$X1[3]+3*desc$X1[4]) & Tauopathy >= (desc$X1[3]-3*desc$X1[4]) ~ Tauopathy))

desc <- data.frame(t(describe(ab$Centiloid)))
ab <- ab %>% 
  mutate(Centiloid = case_when(Centiloid <= (desc$X1[3]+3*desc$X1[4]) & Centiloid >= (desc$X1[3]-3*desc$X1[4]) ~ Centiloid))

rm(desc, pib, av) 

# add closest cognition to each biomarker and then subset to the most complete first visit based on cognition values missing (and time must be within two years of biomarker)
ab <- merge(ab, full_data, by = "ID", all.x = TRUE) %>% #ab_pos
  drop_na(Centiloid) %>% 
  mutate(abs_diff=as.numeric(abs(ab_Date-psy_Date)), abs_diff = replace_na(abs_diff, 0))%>% 
  group_by(ID, ab_Date) %>% 
  slice_min(abs_diff) %>% 
  ungroup() %>% 
  dplyr::filter(abs_diff <= (365.25*2)) %>% 
  dplyr::select(-abs_diff)

ab$na_count <- apply(ab [, c("K1")], 1, function(x) sum(is.na(x)))
ab <- ab %>% 
  arrange(na_count) %>% 
  group_by(ID, psy_Date) %>% 
  slice_min(na_count) %>% 
  ungroup() %>% 
  arrange(ab_Date) %>% 
  group_by(ID) %>% 
  slice_min(ab_Date) %>% 
  ungroup()

tau <- merge(tau, full_data, by = "ID", all.x = TRUE) %>% #ab_pos
  drop_na(Tauopathy) %>% 
  mutate(abs_diff=as.numeric(abs(tau_Date-psy_Date)), abs_diff = replace_na(abs_diff, 0)) %>% 
  group_by(ID, tau_Date) %>% 
  slice_min(abs_diff) %>% 
  ungroup() %>% 
  dplyr::filter(abs_diff <= (365.25*2)) %>% 
  dplyr::select(-abs_diff)

tau$na_count <- apply(tau [, c("K1")], 1, function(x) sum(is.na(x)))
tau <- tau %>% 
  arrange(na_count) %>% 
  group_by(ID, psy_Date) %>% 
  slice_min(na_count) %>% 
  ungroup() %>% 
  arrange(tau_Date) %>% 
  group_by(ID) %>% 
  slice_min(tau_Date) %>% 
  ungroup()

mri <- merge(mri, full_data, by = "ID", all.x = TRUE) %>% #ab_pos
  drop_na(CortSig) %>% 
  mutate(abs_diff=as.numeric(abs(mri_Date-psy_Date)), abs_diff = replace_na(abs_diff, 0)) %>% 
  group_by(ID, mri_Date) %>% 
  slice_min(abs_diff) %>% 
  ungroup() %>% 
  dplyr::filter(abs_diff <= (365.25*2)) %>% 
  dplyr::select(-abs_diff)

mri$na_count <- apply(mri [, c("K1")], 1, function(x) sum(is.na(x)))
mri <- mri %>% 
  arrange(na_count) %>% 
  group_by(ID, psy_Date) %>% 
  slice_min(na_count) %>% 
  ungroup() %>% 
  arrange(mri_Date) %>% 
  group_by(ID) %>% 
  slice_min(mri_Date) %>% 
  ungroup()

# Add apoe, Age, sex, EDUC
demo <- read_excel(paste0(inPATH,"demographics.xlsx")) %>% 
  mutate(birth_Date = as.Date(BIRTH) )%>%
  distinct(ID, birth_Date, EDUC, race, sex) %>% 
  mutate(sex = as.numeric(str_replace_all(sex, c("M" = "1", "F" = "2"))))

demo <- read_excel(paste0(inPATH,"apoe.xlsx")) %>% 
  mutate(ID = id)%>% distinct(ID, apoe) %>% 
  mutate(apoe = as.numeric(str_replace_all(apoe, c("22" = "0", "24" = "1", "44" = "1", "34" = "1", "23" = "0", "33" = "0")))) %>%
  merge(., demo, by = "ID")

ab <- merge(ab, demo, by = "ID", all.x = TRUE)
ab <- ab %>% 
  mutate(Age = as.numeric(round(psy_Date - birth_Date)/365))

tau <- merge(tau, demo, by = "ID", all.x = TRUE)
tau <- tau %>% 
  mutate(Age = as.numeric(round(psy_Date - birth_Date)/365))

mri <- merge(mri, demo, by = "ID", all.x = TRUE)
mri <- mri %>% 
  mutate(Age = as.numeric(round(psy_Date - birth_Date)/365))

full_data <- merge(full_data, demo, by = "ID", all.x = TRUE) %>% 
  mutate(Age = as.numeric(round(psy_Date - birth_Date)/365))

#### Plots ####
# set data aside 
data_corr_ab <- ab %>% 
  drop_na(Centiloid) %>% 
  mutate(AgeS = as.numeric(scale(Age)), EDUCS = as.numeric(scale (EDUC, scale = FALSE)))%>% 
  drop_na(AgeS, EDUCS, sex, apoe, K1)

K1 <- RVAideMemoire::pcor.test(data_corr_ab$Centiloid, data_corr_ab$K1, list(data_corr_ab$AgeS, data_corr_ab$EDUCS, data_corr_ab$sex, data_corr_ab$apoe), semi = FALSE, conf.level = 0.95, nrep = 1000, method = "pearson")

K1$p.value
K1$estimate
K1$conf.int[1:2]
describe(data_corr_ab$K1)
## Calculate the mean of K1
mean(data_corr_ab$K1, na.rm = TRUE)
## Calculate the standard error of K1
sd(data_corr_ab$K1, na.rm = TRUE) / sqrt(length(data_corr_ab$K1[!is.na(data_corr_ab$K1)]))

##
data_corr_tau <- tau%>% drop_na(Tauopathy) %>% 
  mutate(AgeS = scale(Age), EDUCS = scale (EDUC, scale = FALSE)) %>% 
  drop_na(AgeS, EDUCS, sex, apoe, K1)

K1 <- RVAideMemoire::pcor.test(data_corr_tau$Tauopathy, data_corr_tau$K1, list(data_corr_tau$AgeS, data_corr_tau$EDUCS, data_corr_tau$sex, data_corr_tau$apoe), semi = FALSE, conf.level = 0.95, nrep = 1000, method = "pearson")

K1$p.value
K1$estimate
K1$conf.int[1:2]
describe(data_corr_tau$K1)
## Calculate the mean of K1
mean(data_corr_tau$K1, na.rm = TRUE)
## Calculate the standard error of K1
sd(data_corr_tau$K1, na.rm = TRUE) / sqrt(length(data_corr_tau$K1[!is.na(data_corr_tau$K1)]))

##
data_corr_mri <- mri %>% 
  drop_na(CortSig) %>% 
  mutate(AgeS = scale(Age), EDUCS = scale (EDUC, scale = FALSE)) %>% 
  drop_na(AgeS, EDUCS, sex, apoe, K1)

K1 <- RVAideMemoire::pcor.test(data_corr_mri$CortSig, data_corr_mri$K1, list(data_corr_mri$AgeS, data_corr_mri$EDUCS, data_corr_mri$sex, data_corr_mri$apoe), semi = FALSE, conf.level = 0.95, nrep = 1000, method = "pearson")

K1$p.value
K1$estimate
K1$conf.int[1:2]
describe(data_corr_mri$K1)

## Calculate the mean of K1
mean(data_corr_mri$K1, na.rm = TRUE)
## Calculate the standard error of K1
sd(data_corr_mri$K1, na.rm = TRUE) / sqrt(length(data_corr_mri$K1[!is.na(data_corr_mri$K1)]))

# line plots with partial correlations added 
ab_plot <- data_corr_ab %>%
  ggplot(aes(x = Centiloid, y = K1)) +
  geom_point(alpha = 0.3, color = "steelblue", size = 1.5) +
  geom_smooth(method = "lm", color = "firebrick", fill = "firebrick", se = TRUE, linewidth = 1) +
  labs(
    x = "Amyloid PET (Centiloid)",
    y = "PACC Score (z-score)",
    title = "Association Between Amyloid Burden and Cognition"
  ) +
  coord_cartesian(ylim = c(-2.5, 3)) +  # Adjust limits without clipping smooth
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

tau_plot <- data_corr_tau %>%
  ggplot(aes(x = Tauopathy, y = K1)) +
  geom_point(alpha = 0.3, color = "steelblue", size = 1.5) +
  geom_smooth(method = "lm", color = "firebrick", fill = "firebrick", se = TRUE, linewidth = 1) +
  labs(
    x = "Tauopathy (SUVR)",
    y = "PACC Score (z-score)",
    title = "Association Between Tau Burden and Cognition"
  ) +
  coord_cartesian(ylim = c(-2.5, 3)) +  # Keeps smoothing line intact
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

mri_plot <- data_corr_mri %>%
  ggplot(aes(x = CortSig, y = K1)) +
  geom_point(alpha = 0.3, color = "steelblue", size = 1.5) +
  geom_smooth(method = "lm", color = "firebrick", fill = "firebrick", se = TRUE, linewidth = 1) +
  labs(
    x = "Cortical Thickness (mm)",
    y = "PACC Score (z-score)",
    title = "Association Between Cortical Thickness and Cognition"
  ) +
  coord_cartesian(ylim = c(-2.5, 3)) +  # Prevents clipping of smoother
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

###################################################
## Load CDR data to allow for stratification
cdr <- read_excel(paste0(inPATH,"b4_cdr.xlsx"))%>%
  dplyr::select(ID, TESTDATE, cdr) %>%
  mutate(CDR_Date = as.Date(TESTDATE, origin = "1899-12-30"))%>%
  dplyr::select(-TESTDATE) %>%
  mutate(cdr_cat = as.factor(ifelse(cdr == 0, "Unimpaired", "Impaired"))) %>%
  mutate(cdr = as.factor(cdr))

## Merge CDR wtih data_corr_ab  
data_corr_ab_cdr <- merge(data_corr_ab, cdr, by = "ID", all.x = TRUE) %>% 
  mutate(abs_diff=as.numeric(abs(ab_Date-CDR_Date)), abs_diff = replace_na(abs_diff, 0)) %>% 
  group_by(ID, ab_Date) %>% 
  slice_min(abs_diff) %>% 
  ungroup() %>% 
  dplyr::filter(abs_diff <= (365.25*2)) %>% 
  dplyr::select(-abs_diff)


## Adjust CDR factors such that 1, 2, and 3 are coded as "1+"
data_corr_ab_cdr <- data_corr_ab_cdr %>%
  mutate(cdr = case_when(
    cdr == "1" ~ "1+",
    cdr == "2" ~ "1+",
    cdr == "3" ~ "1+",
    TRUE ~ as.character(cdr) # Keep 0 and 0.5 as they are
  )) %>%
  mutate(cdr = factor(cdr, levels = c("0", "0.5", "1+")))

## Linear model for CDR stratification
lm_ab_cdr <- lm(K1 ~ Centiloid*cdr + AgeS + EDUCS + sex + apoe, data = data_corr_ab_cdr) 

emtrends(lm_ab_cdr, pairwise ~ cdr, var = "Centiloid")

## Plot
ggplot(data_corr_ab_cdr, aes(x = Centiloid, y = K1)) +
  geom_point(alpha = 0.2, color = "gray40") +
  geom_smooth(method = "lm", se = TRUE, color = "#e41a1c", fill = "#e41a1c", linewidth = 1.2) +
  facet_wrap(~ cdr, labeller = as_labeller(c("0" = "CDR 0", "0.5" = "CDR 0.5", "1+" = "CDR 1+"))) +
  labs(
    x = "Amyloid PET (Centiloid)",
    y = "PACC Score (z-score)",
    title = "Association Between Amyloid PET and Cognition by CDR Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()
  )

## Merge CDR wtih data_corr_tau  
data_corr_tau_cdr <- merge(data_corr_tau, cdr, by = "ID", all.x = TRUE) %>% 
  mutate(abs_diff=as.numeric(abs(tau_Date-CDR_Date)), abs_diff = replace_na(abs_diff, 0)) %>% 
  group_by(ID, tau_Date) %>% 
  slice_min(abs_diff) %>% 
  ungroup() %>% 
  dplyr::filter(abs_diff <= (365.25*2)) %>% 
  dplyr::select(-abs_diff)

## Adjust CDR factors such that 1, 2, and 3 are coded as "1+"
data_corr_tau_cdr <- data_corr_tau_cdr %>%
  mutate(cdr = case_when(
    cdr == "1" ~ "1+",
    cdr == "2" ~ "1+",
    cdr == "3" ~ "1+",
    TRUE ~ as.character(cdr) # Keep 0 and 0.5 as they are
  )) %>%
  mutate(cdr = factor(cdr, levels = c("0", "0.5", "1+")))

## Linear model for CDR stratification
lm_tau_cdr <- lm(K1 ~ Tauopathy*cdr + AgeS + EDUCS + sex + apoe, data = data_corr_tau_cdr) 

emtrends(lm_tau_cdr, pairwise ~ cdr, var = "Tauopathy")


## Plot
ggplot(data_corr_tau_cdr, aes(x = Tauopathy, y = K1, color = as.factor(cdr))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_manual(
    values = c("0" = "#1b9e77", "0.5" = "#d95f02", "1+" = "#7570b3"),
    labels = c("CDR 0", "CDR 0.5", "CDR 1+"),
    name = "CDR Group"
  ) +
  labs(
    x = "Tau PET (Tauopathy)",
    y = "PACC Score (z-score)",
    title = "PACC ~ Tau PET by Clinical Status"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

## Merge CDR wtih data_corr_mri  
data_corr_mri_cdr <- merge(data_corr_mri, cdr, by = "ID", all.x = TRUE) %>% 
  mutate(abs_diff=as.numeric(abs(mri_Date-CDR_Date)), abs_diff = replace_na(abs_diff, 0)) %>% 
  group_by(ID, mri_Date) %>% 
  slice_min(abs_diff) %>% 
  ungroup() %>% 
  dplyr::filter(abs_diff <= (365.25*2)) %>% 
  dplyr::select(-abs_diff)


## Adjust CDR factors such that 1, 2, and 3 are coded as "1+"
data_corr_mri_cdr <- data_corr_mri_cdr %>%
  mutate(cdr = case_when(
    cdr == "1" ~ "1+",
    cdr == "2" ~ "1+",
    cdr == "3" ~ "1+",
    TRUE ~ as.character(cdr) # Keep 0 and 0.5 as they are
  )) %>%
  mutate(cdr = factor(cdr, levels = c("0", "0.5", "1+")))

## Linear model for CDR stratification
lm_mri_cdr <- lm(K1 ~ CortSig*cdr + AgeS + EDUCS + sex + apoe, data = data_corr_mri_cdr) 

emtrends(lm_mri_cdr, pairwise ~ cdr, var = "CortSig")

## Plot
ggplot(data_corr_mri_cdr, aes(x = CortSig, y = K1, color = as.factor(cdr))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_manual(
    values = c("0" = "#1b9e77", "0.5" = "#d95f02", "1+" = "#7570b3"),
    labels = c("CDR 0", "CDR 0.5", "CDR 1+"),
    name = "CDR Group"
  ) +
  labs(
    x = "Cortical Signature",
    y = "PACC Score (z-score)",
    title = "PACC ~ Cortical Signature by Clinical Status"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )


## Instead of plotting the simple slopes, lets take the original pearson corerlation plot and just add the cdr stratification to the data points
## MRI
mri_plot <- data_corr_mri_cdr %>%
  ggplot(aes(x = CortSig, y = K1, color = as.factor(cdr))) +  # color by CDR group
  geom_point(alpha = 0.7, size = 1.8) +
  geom_smooth(method = "lm", color = "black", fill = "gray", se = TRUE, linewidth = 1) +
  labs(
    x = "Cortical Signature",
    y = "PACC Score (z-score)",
    color = "CDR",
    title = "Association between PACC and Cortical Signature"
  ) +
  coord_cartesian(ylim = c(-2.5, 3)) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "right"
  ) +
  scale_color_manual(
    values = c(
      "0" = "#73AF48FF",     # Green
      "0.5" = "#38A6A5FF",   # Teal
      "1+" = "#E17C05FF"      # Orange
    )
  )

## Amyloid
ab_plot <- data_corr_ab_cdr %>%
  ggplot(aes(x = Centiloid, y = K1, color = as.factor(cdr))) +  # color by CDR
  geom_point(alpha = 0.7, size = 1.8) +
  geom_smooth(method = "lm", color = "black", fill = "gray", se = TRUE, linewidth = 1) +
  labs(
    x = "Amyloid PET - Centiloid",
    y = "PACC Score (z-score)",
    color = "CDR",
    title = "Association between PACC and Amyloid Centiloid"
  ) +
  coord_cartesian(ylim = c(-2.5, 3)) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "right"
  ) +
  scale_color_manual(
    values = c(
      "0" = "#73AF48FF",     # Green
      "0.5" = "#38A6A5FF",   # Teal
      "1+" = "#E17C05FF"      # Orange
    )
  )

## Tau
tau_plot <- data_corr_tau_cdr %>%
  ggplot(aes(x = Tauopathy, y = K1, color = as.factor(cdr))) +  # color by CDR
  geom_point(alpha = 0.7, size = 1.8) +
  geom_smooth(method = "lm", color = "black", fill = "gray", se = TRUE, linewidth = 1) +
  labs(
    x = "Tau PET - Tauopathy",
    y = "PACC Score (z-score)",
    color = "CDR",
    title = "Association between PACC and Tauopathy"
  ) +
  coord_cartesian(ylim = c(-2.5, 3)) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.position = "right"
  ) +
  scale_color_manual(
    values = c(
      "0" = "#73AF48FF",     # Green
      "0.5" = "#38A6A5FF",   # Teal
      "1+" = "#E17C05FF"      # Orange
    )
  )

#####################################################   
## Create a density plot for all PACC data by CDR score
## Merge CDR wtih PACC data  
full_data_cdr <- merge(full_data, cdr, by = "ID", all.x = TRUE) %>% 
  mutate(abs_diff=as.numeric(abs(psy_Date-CDR_Date)), abs_diff = replace_na(abs_diff, 0)) %>% 
  group_by(ID, psy_Date) %>% 
  slice_min(abs_diff) %>% 
  ungroup() %>% 
  dplyr::filter(abs_diff <= (365.25*2)) %>% 
  dplyr::select(-abs_diff) %>%
  dplyr::filter(!is.na(K1)) %>%
  mutate(K1neg = -K1) %>%
  dplyr::filter(!is.na(cdr))

## Adjust CDR factors such that 1, 2, and 3 are coded as "1+"
full_data_cdr <- full_data_cdr %>%
  mutate(cdr = case_when(
    cdr == "2" ~ "2+",
    cdr == "3" ~ "2+",
    TRUE ~ as.character(cdr) # Keep 0 and 0.5 as they are
  )) %>%
  mutate(cdr = factor(cdr, levels = c("0", "0.5", "1", "2+")))

## Density plot
custom_colors <- paletteer_d("rcartocolor::Prism")[c(5, 3, 7, 9)]

# Means
mean_PACC <- full_data_cdr %>%
  dplyr::ungroup() %>%
  dplyr::group_by(cdr) %>%
  dplyr::summarize(K1neg = mean(K1neg, na.rm = TRUE))

## Plot
ggplot(full_data_cdr, aes(x = K1neg, fill = as.factor(cdr))) +
  geom_density(alpha = 0.7) +
  # Corrected geom_vline line:
  geom_vline(data = mean_PACC, aes(xintercept = K1neg, color = as.factor(cdr)),
             linetype = "dashed", linewidth = .75, show.legend = FALSE) +
  labs(title = "PACC by CDR Score",
       x = "PACC Score (z-score)",
       y = "Density",
       fill = "CDR Score") +
  scale_fill_manual(values = custom_colors, name = "CDR Score",
                    breaks = c("0", "0.5", "1", "2+"),
                    labels = c("0", "0.5", "1", "2+")) +
  scale_color_manual(values = custom_colors, guide = "none") + # Added guide = "none" for color legend
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## Create some stats the align with density plot
model <- lmer(K1 ~ cdr + scale(Age, scale=FALSE) + scale(EDUC, scale=FALSE) + sex + apoe + (1 | ID), data = full_data_cdr)
emm <- emmeans(model, ~cdr, df.method = "kenward-roger", pbkrtest.limit = 12000)
emm.p <- pairs(emm)
emm_df <- as.data.frame(emm)

## Violin plot with data displayed
# Your ggplot code to generate the plot
my_plot <- ggplot(full_data_cdr, aes(x = cdr, y = K1)) +
  geom_violin(aes(fill = cdr), trim = FALSE, alpha = 0.3, color = NA) +
  geom_sina(
    aes(color = cdr),
    size = 1.5,
    alpha = 0.7,
    method = "density",
    maxwidth = 0.95
  ) +
  # Optional: boxplot overlay for summary stats
  geom_boxplot(
    width = 0.1,
    outlier.shape = NA,
    alpha = 0.7,
    color = "black"
  ) +
  # Estimated marginal means (EMMs)
  geom_point(
    data = emm_df,
    aes(x = cdr, y = emmean),
    color = "black",
    shape = 18,
    size = 3,
    inherit.aes = FALSE
  ) +
  # Confidence intervals for EMMs
  geom_errorbar(
    data = emm_df,
    aes(x = cdr, ymin = lower.CL, ymax = upper.CL),
    width = 0.2,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Observed and Model-Adjusted PACC Scores by CDR Status",
    x = "CDR Score",
    y = "PACC Score (z-score)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )