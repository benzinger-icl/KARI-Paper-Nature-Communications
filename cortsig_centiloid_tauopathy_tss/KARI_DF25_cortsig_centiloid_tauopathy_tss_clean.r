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
list.of.packages <- c("tidyverse", "readxl",  "emmeans", "lme4", "lmerTest", "ggplot2", "paletteer", "ggforce")

## Check if they are installed and install them if they are not
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load all packages
lapply(list.of.packages, require, character.only = TRUE)

#################################################################################
## Set Paths
inPATH <- paste0("B:/path/synthetic_data/")

## Load kari datafreeze 
kari_3t <- read_excel(paste0(inPATH,"mri_3t.xlsx"))

## Demographics
dem <- read_excel(paste0(inPATH,"demographics.xlsx")) %>%
  select(ID, SES, BIRTH, DEATH, ENTDATE, EDUC, HAND, grantc, race, sex)

## Trim Kari
mri <- kari_3t %>% 
    select(ID, MR_Session, MR_Date, LOAD_CorticalSignature_Thickness) %>%
    mutate(MR_Date = as.Date(MR_Date)) %>%
    filter(!is.na(LOAD_CorticalSignature_Thickness)) %>%
    rename(CortSig = LOAD_CorticalSignature_Thickness) %>%
    left_join(dem %>% select(ID, BIRTH), by = "ID") %>%
    mutate(Age = as.numeric(difftime(MR_Date, BIRTH, units = "days"))) %>%
    mutate(Age = Age / 365.25) %>%
    filter(!is.na(Age))

## Add PET data
## Amyloid
av45 <- read_excel(paste0(inPATH,"av45_mrfreepet.xlsx"))
av45 <- av45 %>%
  mutate(PET_Date = as.Date(PET_Date)) %>%
  group_by(PET_Session) %>%
  distinct(PET_Session, .keep_all = TRUE) %>%
  select(ID, PET_Session, PET_Date, MRFreePET_Centiloid) %>%
  rename(Centiloid = MRFreePET_Centiloid) %>%
  filter(!is.na(Centiloid)) %>%
  left_join(dem %>% select(ID, BIRTH), by = "ID") %>%
  mutate(Age = as.numeric(difftime(PET_Date, BIRTH, units = "days"))) %>%
  mutate(Age = Age / 365.25) %>%
  filter(!is.na(Age))

fbb <- read_excel(paste0(inPATH,"fbb_mrfreepet.xlsx"))
fbb <- fbb %>%
  mutate(PET_Date = as.Date(PET_Date)) %>%
  group_by(PET_Session) %>%
  distinct(PET_Session, .keep_all = TRUE) %>%
  select(ID, PET_Session, PET_Date, MRFreePET_Centiloid) %>%
  rename(Centiloid = MRFreePET_Centiloid) %>%
  filter(!is.na(Centiloid)) %>%
  left_join(dem %>% select(ID, BIRTH), by = "ID") %>%
  mutate(Age = as.numeric(difftime(PET_Date, BIRTH, units = "days"))) %>%
  mutate(Age = Age / 365.25) %>%
  filter(!is.na(Age))

pib <- read_excel(paste0(inPATH,"pib_mrfreepet.xlsx"))
pib <- pib %>%
  mutate(PET_Date = as.Date(PET_Date)) %>%
  group_by(PET_Session) %>%
  distinct(PET_Session, .keep_all = TRUE) %>%
  select(ID, PET_Session, PET_Date, MRFreePET_Centiloid) %>%
  rename(Centiloid = MRFreePET_Centiloid) %>%
  filter(!is.na(Centiloid)) %>%
  left_join(dem %>% select(ID, BIRTH), by = "ID") %>%
  mutate(Age = as.numeric(difftime(PET_Date, BIRTH, units = "days"))) %>%
  mutate(Age = Age / 365.25) %>%
  filter(!is.na(Age))

## Combine ID and PET_Session for each av45, fbb, and pib
amy <- bind_rows(
  av45 %>% select(ID, PET_Session, PET_Date, Age, Centiloid),
  pib %>% select(ID, PET_Session, PET_Date, Age, Centiloid),
  fbb %>% select(ID, PET_Session, PET_Date, Age, Centiloid)
)

## Tau
tau <- read_excel(paste0(inPATH,"tau.xlsx"))
tau <- tau %>%
  mutate(PET_Date = as.Date(PET_Date)) %>%
  group_by(PET_Session) %>%
  distinct(PET_Session, .keep_all = TRUE) %>%
  filter(!is.na(Tauopathy)) %>%
  left_join(dem %>% select(ID, BIRTH), by = "ID") %>%
  mutate(Age = as.numeric(difftime(PET_Date, BIRTH, units = "days"))) %>%
  mutate(Age = Age / 365.25) %>%
  filter(!is.na(Age)) %>%
  select(ID, PET_Session, PET_Date, Age, Tauopathy, TSS) 

## Clinical
cdr <- read_excel(paste0(inPATH,"b4_cdr.xlsx"))%>%
  select(ID, TESTDATE, cdr) %>%
  mutate(CDR_Date = as.Date(TESTDATE, origin = "1899-12-30"))%>%
  select(-TESTDATE) %>%
  mutate(cdr_cat = as.factor(ifelse(cdr == 0, "Unimpaired", "Impaired")))

##############################################################################
# Find closest CDR_date for each MR_Date within ID
mri_cdr <- mri %>%
  left_join(cdr, by = "ID", suffix = c(".mri", ".cdr"), relationship = "many-to-many") %>%
  group_by(ID, MR_Date) %>%
  mutate(
    MR_CDR_Diff = abs(difftime(MR_Date, CDR_Date, units = "days"))
  ) %>%
  slice_min(MR_CDR_Diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ID, MR_Date, Age, CDR_Date, MR_CDR_Diff, CortSig, cdr, cdr_cat) %>%
  filter(!is.na(CortSig))

## Remove rows where MR_PET_Diff is greater than 2 years
mri_cdr <- mri_cdr %>%
  filter(MR_CDR_Diff <= 365 * 2)

# Find closest CDR_date for each amyloid PET_Date within ID
amy_cdr <- amy %>%
  left_join(cdr, by = "ID", suffix = c(".amy", ".cdr"), relationship = "many-to-many") %>%
  group_by(ID, PET_Date) %>%
  mutate(
    PET_CDR_Diff = abs(difftime(PET_Date, CDR_Date, units = "days"))
  ) %>%
  slice_min(PET_CDR_Diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ID, PET_Date, Age, CDR_Date, PET_CDR_Diff, Centiloid, cdr, cdr_cat) %>%
  filter(!is.na(Centiloid))

## Remove rows where MR_PET_Diff is greater than 2 years
amy_cdr <- amy_cdr %>%
  filter(PET_CDR_Diff <= 365 * 2)

# Find closest CDR_date for each tau PET_Date within ID
tau_cdr <- tau %>%
  left_join(cdr, by = "ID", suffix = c(".tau", ".cdr"), relationship = "many-to-many") %>%
  group_by(ID, PET_Date) %>%
  mutate(
    PET_CDR_Diff = abs(difftime(PET_Date, CDR_Date, units = "days"))
  ) %>%
  slice_min(PET_CDR_Diff, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(ID, PET_Date, Age, CDR_Date, PET_CDR_Diff, Tauopathy, TSS, cdr, cdr_cat) %>%
  filter(!is.na(Tauopathy))

## Remove rows where MR_PET_Diff is greater than 2 years
tau_cdr <- tau_cdr %>%
  filter(PET_CDR_Diff <= 365 * 2)

#################################################################################
## Significance tests
## Cortical Signature
t.cortsig <- t.test(CortSig ~ cdr_cat, data = mri_cdr, var.equal = TRUE)
cortsig.p <- t.cortsig$p.value
cortsig.ci <- t.cortsig$conf.int
cortsig.mean <- tapply(mri_cdr$CortSig, mri_cdr$cdr_cat, mean)
cortsig.sd <- tapply(mri_cdr$CortSig, mri_cdr$cdr_cat, sd)

## Centiloid
t.centiloid <- t.test(Centiloid ~ cdr_cat, data = amy_cdr, var.equal = TRUE)
centiloid.p <- t.centiloid$p.value
centiloid.ci <- t.centiloid$conf.int
centiloid.mean <- tapply(amy_cdr$Centiloid, amy_cdr$cdr_cat, mean)
centiloid.sd <- tapply(amy_cdr$Centiloid, amy_cdr$cdr_cat, sd)

## Tauopathy
t.tauopathy <- t.test(Tauopathy ~ cdr_cat, data = tau_cdr, var.equal = TRUE)
tauopathy.p <- t.tauopathy$p.value
tauopathy.ci <- t.tauopathy$conf.int
tauopathy.mean <- tapply(tau_cdr$Tauopathy, tau_cdr$cdr_cat, mean)
tauopathy.sd <- tapply(tau_cdr$Tauopathy, tau_cdr$cdr_cat, sd)

## TSS
t.tss <- t.test(TSS ~ cdr_cat, data = tau_cdr, var.equal = TRUE)
tss.p <- t.tss$p.value  
tss.ci <- t.tss$conf.int
tss.mean <- tapply(tau_cdr$TSS, tau_cdr$cdr_cat, mean)
tss.sd <- tapply(tau_cdr$TSS, tau_cdr$cdr_cat, sd)

#################################################################################
# desnity plots
## Cortical Signature
custom_colors <- paletteer_d("rcartocolor::Prism")[c(5, 3, 7, 9)]

## Z-score CortSig
mri_cdr <- mri_cdr %>%
    mutate(CortSigz = (CortSig - mean(CortSig, na.rm = TRUE)) / sd(CortSig)) %>%
    mutate(CortSigzneg = -CortSigz)

# Means
mean_cortsig <- mri_cdr %>%
  group_by(cdr) %>%
  summarize(mean_cortsig = mean(CortSig, na.rm = TRUE)) 

mean_cortsigz <- mri_cdr %>%
  group_by(cdr) %>%
  summarize(mean_cortsigz = mean(CortSigz, na.rm = TRUE)) %>%
  mutate(mean_cortsigzneg = mean_cortsigz * -1) 

## Plot
ggplot(mri_cdr, aes(x = CortSigzneg, fill = as.factor(cdr))) +
  geom_density(alpha = 0.7) +
  geom_vline(data = mean_cortsigz, aes(xintercept = mean_cortsigzneg, color = as.factor(cdr)),
             linetype = "dashed", linewidth = .75, show.legend = FALSE) +
  labs(title = "Cortical Signature by CDR Score",
       x = "Normalized Cortical Signature",
       y = "Density",
       fill = "CDR Score") +
  scale_fill_manual(values = custom_colors, name = "CDR Score",
                          breaks = c("0", "0.5", "1", "2"),
                          labels = c("0", "0.5", "1", "2")) +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#################################################################################
## Centiloid
# Means
mean_centiloid <- amy_cdr %>%
  group_by(cdr) %>%
  summarize(mean_Centiloid = mean(Centiloid, na.rm = TRUE))

## Plot
ggplot(amy_cdr, aes(x = Centiloid, fill = as.factor(cdr))) +
  geom_density(alpha = 0.7) +
  geom_vline(data = mean_centiloid, aes(xintercept = mean_Centiloid, color = as.factor(cdr)), linetype = "dashed", linewidth = .75, show.legend = FALSE) +
  labs(title = "Centiloid by CDR Score",
       x = "Centiloid",
       y = "Density",
       fill = "CDR Score") +
  scale_fill_manual(values = custom_colors, name = "CDR Score",
                          breaks = c("0", "0.5", "1", "2"),
                          labels = c("0", "0.5", "1", "2")) +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#################################################################################
## Tauopathy
# Means
mean_tauopathy <- tau_cdr %>%
  group_by(cdr) %>%
  summarize(mean_tauopathy = mean(Tauopathy, na.rm = TRUE))

## Plot
ggplot(tau_cdr, aes(x = Tauopathy, fill = as.factor(cdr))) +
  geom_density(alpha = 0.7) +
  geom_vline(data = mean_tauopathy, aes(xintercept = mean_tauopathy, color = as.factor(cdr)), linetype = "dashed", linewidth = .75, show.legend = FALSE) +
  labs(title = "Tauopathy by CDR Score",
       x = "Tauopathy (SUVR)",
       y = "Density",
       fill = "CDR Score") +
  scale_fill_manual(values = custom_colors, name = "CDR Score",
                          breaks = c("0", "0.5", "1", "2"),
                          labels = c("0", "0.5", "1", "2")) +
  scale_x_continuous(limits = c(0, NA)) +  # Ensures x-axis starts at 0
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#################################################################################
## TSS
# Means
mean_tss <- tau_cdr %>%
  group_by(cdr) %>%
  summarize(mean_tss = mean(TSS, na.rm = TRUE))

## Plot
ggplot(tau_cdr, aes(x = TSS, fill = as.factor(cdr))) +
  geom_density(alpha = 0.7) +
  geom_vline(data = mean_tss, aes(xintercept = mean_tss, color = as.factor(cdr)),linetype = "dashed", linewidth = .75, show.legend = FALSE) +
  labs(title = "TSS by CDR Score",
       x = "TSS",
       y = "Density",
       fill = "CDR Score") +
  scale_fill_manual(values = custom_colors, name = "CDR Score",
                          breaks = c("0", "0.5", "1", "2"),
                          labels = c("0", "0.5", "1", "2")) +
  scale_x_continuous(limits = c(0, NA)) +  # Ensures x-axis starts at 0
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#################################################################################
# Pathology x Age plots
## Cortical Signature
mri_cdr_cat <- mri_cdr %>%
    mutate(cdr = as.factor(cdr))

## All four CDR categories
ggplot(mri_cdr_cat, aes(x = Age, y = CortSig, color = cdr)) +
  geom_smooth(method = "loess", span = .9, se = TRUE, alpha = .2) + 
  labs(title = "Cortical Signature Across the Lifespan",
       x = "Age",
       y = "Normalized Cortical Signature",
       color = "CDR Score") +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(alpha = 0)))

## Unimpaired vs Impaired
ggplot(mri_cdr, aes(x = Age, y = CortSig, color = cdr_cat)) +
  geom_smooth(method = "loess", span = .9, se = TRUE, alpha = .2) + 
  scale_color_manual(values = c("Impaired" = "#CC503EFF", "Unimpaired" = "#73AF48FF")) +
  labs(title = "Cortical Signature Across the Lifespan",
       x = "Age",
       y = "Cortical Signature",
       color = "CDR Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(alpha = 0)))

## Centiloid
amy_cdr_cat <- amy_cdr %>%
    mutate(cdr = as.factor(cdr))

## All four CDR categories
ggplot(amy_cdr_cat, aes(x = Age, y = Centiloid, color = cdr)) +
  geom_smooth(method = "loess", span = .5, se = TRUE, alpha = .2) + 
  labs(title = "Centiloid Across the Lifespan",
       x = "Age",
       y = "Centiloid",
       color = "CDR Score") +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(alpha = 0)))

ggplot(amy_cdr, aes(x = Age, y = Centiloid, color = cdr_cat)) +
  geom_smooth(method = "loess", span = .9, se = TRUE, alpha = .2) + 
  scale_color_manual(values = c("Impaired" = "#CC503EFF", "Unimpaired" = "#73AF48FF")) +
  labs(title = "Centiloid Across the Lifespan",
       x = "Age",
       y = "Centiloid",
       color = "CDR Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(alpha = 0)))

## Tauopathy
tau_cdr_cat <- tau_cdr %>%
    mutate(cdr = as.factor(cdr))

## All four CDR categories
ggplot(tau_cdr_cat, aes(x = Age, y = Tauopathy, color = cdr)) +
  geom_smooth(method = "loess", span = .9, se = TRUE, alpha = .2) + 
  labs(title = "Tauopathy Across the Lifespan",
       x = "Age",
       y = "Tauopathy (SUVR)",
       color = "CDR Score") +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(alpha = 0)))

ggplot(tau_cdr, aes(x = Age, y = Tauopathy, color = cdr_cat)) +
  geom_smooth(method = "loess", span = .9, se = TRUE, alpha = .2) + 
  scale_color_manual(values = c("Impaired" = "#CC503EFF", "Unimpaired" = "#73AF48FF")) +
  labs(title = "Tauopathy Across the Lifespan",
       x = "Age",
       y = "Tauopathy",
       color = "CDR Score") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(alpha = 0)))

#################################################################################
## Model Cortical Signature by age and CDR
## Remove scientific notation
options(scipen = 999)  # Disable scientific notation
model.cortsig <- lmer(CortSig ~ scale(Age, scale=FALSE) * cdr + (1 | ID), data = mri_cdr_cat)
lme.cortsig <- summary(model.cortsig)
emm.cortsig <- emmeans(model.cortsig, ~cdr, df.method = "kenward-roger", pbkrtest.limit = 4000)
emm.cortsig.p <- pairs(emm.cortsig)
omnibus.cortsig <- anova(model.cortsig)

## Model Centiloid by age and CDR
model.centiloid <- lmer(Centiloid ~ scale(Age, scale=FALSE) * cdr + (1 | ID), data = amy_cdr_cat)
lme.centiloid <- summary(model.centiloid)
emm.centiloid <- emmeans(model.centiloid, ~cdr, df.method = "kenward-roger", pbkrtest.limit = 4000)
emm.centiloid.p <- pairs(emm.centiloid)
omnibus.centiloid <- anova(model.centiloid)

## Model Tauopathy by age and CDR
model.tauopathy <- lmer(Tauopathy ~ scale(Age, scale=FALSE) * cdr + (1 | ID), data = tau_cdr_cat)
lme.tauopathy <- summary(model.tauopathy)
emm.tauopathy <- emmeans(model.tauopathy, ~cdr, df.method = "kenward-roger", pbkrtest.limit = 4000)
emm.tauopathy.p <- pairs(emm.tauopathy)
omnibus.tauopathy <- anova(model.tauopathy)

## Model TSS by age and CDR
model.tss <- lmer(TSS ~ scale(Age, scale=FALSE) * cdr + (1 | ID), data = tau_cdr_cat)
lme.tss <- summary(model.tss)
emm.tss <- emmeans(model.tss, ~cdr, df.method = "kenward-roger", pbkrtest.limit = 4000)
emm.tss.p <- pairs(emm.tss)
omnibus.tss <- anova(model.tss)

##############################################################################
## Create violin plots that align with density plot
## Cortical signature
model.cortsigz <- lmer(CortSigz ~ scale(Age, scale=FALSE) * cdr + (1 | ID), data = mri_cdr_cat)
lme.cortsigz <- summary(model.cortsigz)
emm.cortsigz <- emmeans(model.cortsigz, ~cdr, df.method = "kenward-roger", pbkrtest.limit = 4000)
emm.cortsigz.p <- pairs(emm.cortsigz)
emm_df_cortsigz <- as.data.frame(emm.cortsigz)

## New violin plots
## Violin plot with data displayed
my_plot_CortSig <- ggplot(mri_cdr_cat, aes(x = cdr, y = CortSigz)) +
  geom_violin(aes(fill = cdr), trim = FALSE, alpha = 0.3, color = NA) +
  geom_sina(
    aes(color = cdr),
    size = 1.5,
    alpha = 0.7,
    maxwidth = 0.95,
    method = "density"
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
    data = emm_df_cortsigz,
    aes(x = cdr, y = emmean),
    color = "black",
    shape = 18,
    size = 3,
    inherit.aes = FALSE
  ) +
  # Confidence intervals for EMMs
  geom_errorbar(
    data = emm_df_cortsigz,
    aes(x = cdr, ymin = lower.CL, ymax = upper.CL),
    width = 0.2,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Cortical Signature by Clinical Stage",
    x = "CDR Score",
    y = "Normalized Cortical Signature"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.position = "none"
  )


## Centiloid
emm_df_centiloid <- as.data.frame(emm.centiloid)

## New violin plots
## Violin plot with data displayed
my_plot_Cent <- ggplot(amy_cdr_cat, aes(x = cdr, y = Centiloid)) +
  geom_violin(aes(fill = cdr), trim = FALSE, alpha = 0.3, color = NA) +
  geom_sina(
    aes(color = cdr),
    size = 1.5,
    alpha = 0.7,
    maxwidth = 0.95,
    method = "density"
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
    data = emm_df_centiloid,
    aes(x = cdr, y = emmean),
    color = "black",
    shape = 18,
    size = 3,
    inherit.aes = FALSE
  ) +
  # Confidence intervals for EMMs
  geom_errorbar(
    data = emm_df_centiloid,
    aes(x = cdr, ymin = lower.CL, ymax = upper.CL),
    width = 0.2,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Centiloid by Clinical Stage",
    x = "CDR Score",
    y = "Centiloid"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.position = "none"
  )


## Tauopathy
emm_df_tauopathy <- as.data.frame(emm.tauopathy)

## New violin plots
## Violin plot with data displayed
my_plot_TI <- ggplot(tau_cdr_cat, aes(x = cdr, y = Tauopathy)) +
  geom_violin(aes(fill = cdr), trim = FALSE, alpha = 0.3, color = NA) +
  geom_sina(
    aes(color = cdr),
    size = 1.5,
    alpha = 0.7,
    maxwidth = 0.95,
    method = "density"
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
    data = emm_df_tauopathy,
    aes(x = cdr, y = emmean),
    color = "black",
    shape = 18,
    size = 3,
    inherit.aes = FALSE
  ) +
  # Confidence intervals for EMMs
  geom_errorbar(
    data = emm_df_tauopathy,
    aes(x = cdr, ymin = lower.CL, ymax = upper.CL),
    width = 0.2,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Tauopathy by Clinical Stage",
    x = "CDR Score",
    y = "Tauopathy"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.position = "none"
  )


  ## TSS
emm_df_tss <- as.data.frame(emm.tss)

## New violin plots
## Violin plot with data displayed
my_plot_tss <- ggplot(tau_cdr_cat, aes(x = cdr, y = TSS)) +
  geom_violin(aes(fill = cdr), trim = FALSE, alpha = 0.3, color = NA) +
  geom_sina(
    aes(color = cdr),
    size = 1.5,
    alpha = 0.7,
    maxwidth = 0.95,
    method = "density"
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
    data = emm_df_tss,
    aes(x = cdr, y = emmean),
    color = "black",
    shape = 18,
    size = 3,
    inherit.aes = FALSE
  ) +
  # Confidence intervals for EMMs
  geom_errorbar(
    data = emm_df_tss,
    aes(x = cdr, ymin = lower.CL, ymax = upper.CL),
    width = 0.2,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "Tau Spatial Spread by Clinical Stage",
    x = "CDR Score",
    y = "TSS"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24),
    axis.title.x = element_text(size = 22),
    axis.title.y = element_text(size = 22),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    legend.position = "none"
  )