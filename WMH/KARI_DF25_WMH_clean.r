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
list.of.packages <- c("tidyverse", "readxl", "emmeans", "lme4", "lmerTest", 
                      "paletteer", "moments", "ggforce", "rstatix", "ggpubr")

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
  mutate(Age = as.numeric(difftime(MR_Date, BIRTH, units = "days"))/365.25) %>%
  filter(WMH_volume > 0)

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
## recode into groups
wmh <- combined_df_cdr
wmh$cdr <- as.numeric(wmh$cdr)
wmh$cdr_3grp<- as.numeric(wmh$cdr) 
#wmh$cdr_3grp[wmh$cdr < 1] <- wmh$cdr
wmh$cdr_3grp[wmh$cdr > 0.5] <- '1+'
wmh$cdr_3grp <- factor(wmh$cdr_3grp, levels = c("0", "0.5", "1+"))
table(wmh$cdr_3grp)
wmh$cdr_cat<- as.numeric(wmh$cdr)
wmh$cdr_cat[wmh$cdr > 0] <- '>0'
wmh$cdr_cat <- factor(wmh$cdr_cat, levels = c("0", ">0"))
table(wmh$cdr_cat)
wmh$cdr <- factor(wmh$cdr, levels = c("0", "0.5", "1", "2"))
table(wmh$cdr)

#### NORMALITY CHECK AND DATA TRANSFORMATION ##############################################################

#checking the distribution
hist(wmh$WMH_volume)
shapiro.test(wmh$WMH_volume)#not normal
hist(log10(wmh$WMH_volume))
shapiro.test(log10(wmh$WMH_volume)) #not normal even after logtransform

#other visualisation of distribution of data
skewness(wmh$WMH_volume, na.rm = TRUE)
skewness(log10(wmh$WMH_volume), na.rm = TRUE)
ggdensity(wmh, x= "WMH_volume", fill = "lightgray", title = "WMH volume") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(wmh, x= "log10(WMH_volume)", fill = "lightgray", title = "log-transf WMH volume") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

#identify outlier and check distribution - I tried different things but in the end did not applied this to the final data ################
lower_bound <- quantile(wmh$WMH_volume, 0.001)
upper_bound <- quantile(wmh$WMH_volume, 0.999)
outlier_ind <- which(wmh$WMH_volume < lower_bound | wmh$WMH_volume > upper_bound)
wmh[outlier_ind, ]
wmhout<-subset(wmh, wmh$WMH_volume > lower_bound & wmh$WMH_volume < upper_bound)
skewness(wmhout$WMH_volume, na.rm = TRUE)
skewness(log10(wmhout$WMH_volume), na.rm = TRUE)
ggdensity(wmhout, x= "WMH_volume", fill = "lightgray", title = "WMH volume (no outliers)") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
ggdensity(wmhout, x= "log10(WMH_volume)", fill = "lightgray", title = "log-transf WMH volume (no outliers)") +
  scale_x_continuous() +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

###############################################################################################
### Mirror other analyses

#WMH by CDR status - Significance tests
t.wmh <- t.test(log10(WMH_volume) ~ cdr_cat, data = wmh, var.equal = TRUE)
wmh.p <- t.wmh$p.value
wmh.ci <- t.wmh$conf.int
wmh.mean <- tapply(log(wmh$WMH_volume), wmh$cdr_cat, mean)
wmh.sd <- tapply(log(wmh$WMH_volume), wmh$cdr_cat, sd)
wmh.median <- tapply(wmh$WMH_volume, wmh$cdr_cat, median)
wmh.sd <- tapply(wmh$WMH_volume, wmh$cdr_cat, sd)
#evaluation of non-parametric test
wilcox_test(WMH_volume ~ cdr_cat, data = wmh)
wilcox_effsize(
  data = wmh,
  formula = WMH_volume ~ cdr_cat,
  comparisons = NULL,
  ref.group = NULL,
  paired = FALSE,
  alternative = "two.sided",
  mu = 0,
  ci = TRUE,
  conf.level = 0.95,
  ci.type = "perc",
  nboot = 4000
)
wilcox_effsize(WMH_volume ~ cdr_cat, data = wmh)
wmh.p <- t.wmh$p.value
wmh.ci <- t.wmh$conf.int

###################### PLOTS VISUALISATION ######################################
## WMH
custom_colors <- paletteer_d("rcartocolor::Prism")[c(5, 3, 7)]

## Plot with original WMH (~positive skewed distribution - median)
mdn_wmh <- wmh %>%
  group_by(cdr_3grp) %>%
  summarize(mdn_wmh = median(WMH_volume, na.rm = TRUE))

ggplot(wmh, aes(x = WMH_volume, fill = as.factor(cdr_3grp))) +
  geom_density(alpha = 0.7) +
  geom_vline(data = mdn_wmh, 
             aes(xintercept = mdn_wmh, color = as.factor(cdr_3grp)), 
             linetype = "dashed", linewidth = .75, show.legend = FALSE) +
  labs(title = "WMH volume by CDR Score",
       x = "Total volume (mm3)",
       y = "Density",
       fill = "CDR Score") +
  scale_fill_manual(values = custom_colors, name = "CDR Score",
                    breaks = c("0", "0.5", "1+"),
                    labels = c("0", "0.5", "1+")) +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#plot with log-transformed WMH (~normal distribution - mean)
mean_logwmh <- wmh %>%
  group_by(cdr_3grp) %>%
  summarize(mean_logwmh = mean(log10(WMH_volume), na.rm = TRUE))

ggplot(wmh, aes(x = log10(WMH_volume), fill = as.factor(cdr_3grp))) +
  geom_density(alpha = 0.7) +
  geom_vline(data = mean_logwmh, 
             aes(xintercept = mean_logwmh, color = as.factor(cdr_3grp)), 
             linetype = "dashed", linewidth = .75, show.legend = FALSE) +
  labs(title = "WMH volume by CDR Score",
       x = "Total volume (log10 mm3)",
       y = "Density",
       fill = "CDR Score") +
  scale_fill_manual(values = custom_colors, name = "CDR Score",
                    breaks = c("0", "0.5", "1+"),
                    labels = c("0", "0.5", "1+")) +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

## All four CDR categories
ggplot(wmh, aes(x = Age, y = WMH_volume, color = cdr_3grp)) +
  geom_smooth(method = "loess", span = .9, se = TRUE, alpha = .2) + 
  labs(title = "WMH volume Across the Lifespan",
       x = "Age",
       y = "Total volume (mm3)",
       color = "CDR Score") +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(alpha = 0)))

#log-transformed data
ggplot(wmh, aes(x = Age, y = log10(WMH_volume), color = cdr_3grp)) +
  geom_smooth(method = "loess", span = .9, se = TRUE, alpha = .2) + 
  labs(title = "WMH volume Across the Lifespan",
       x = "Age",
       y = "Total volume (log mm3)",
       color = "CDR Score") +
  scale_color_manual(values = custom_colors) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(alpha = 0)))

## Unimpaired vs Impaired
ggplot(wmh, aes(x = Age, y = WMH_volume, color = cdr_cat)) +
  geom_smooth(method = "loess", span = .9, se = TRUE, alpha = .2) + 
  scale_color_manual(values = c("0" = "#CC503EFF", ">0" = "#73AF48FF")) +
  labs(title = "WMH volume Across the Lifespan",
       x = "Age",
       y = "Total volume (mm3)",
       color = "CDR Status") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(alpha = 0)))

#log-transformed data
ggplot(wmh, aes(x = Age, y = log10(WMH_volume), color = cdr_cat)) +
  geom_smooth(method = "loess", span = .9, se = TRUE, alpha = .2) + 
  scale_color_manual(values = c("0" = "#CC503EFF", ">0" = "#73AF48FF")) +
  labs(title = "WMH volume Across the Lifespan",
       x = "Age",
       y = "Total volume (log10 mm3)",
       color = "CDR Status") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(alpha = 0)))

#################################################################################
## Model by age and CDR
options(scipen = 999)  # Disable scientific notation
wmh$logWMH <- log10(wmh$WMH_volume)
model.wmh <- lmer(logWMH ~ Age * cdr_3grp + (1 | ID), data = wmh)
summary(model.wmh)
hist(residuals(model.wmh))
lme.wmh <- summary(model.wmh)
emm.wmh <- emmeans(model.wmh, ~cdr_3grp, df.method = "kenward-roger", 
                   pbkrtest.limit = 4000)
emm.wmh.p <- pairs(emm.wmh)
emm_df <- as.data.frame(emm.wmh)

## Violin plot with data displayed
my_plot_WMH <- ggplot(wmh, aes(x = cdr_3grp, y = logWMH)) +
  # Faint violin outline
  geom_violin(aes(fill = cdr_3grp), trim = FALSE, alpha = 0.3, color = NA) +
  # Raw data points, jittered like a violin
  geom_sina(
    aes(color = cdr_3grp),
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
    data = emm_df,
    aes(x = cdr_3grp, y = emmean),
    color = "black",
    shape = 18,
    size = 3,
    inherit.aes = FALSE
  ) +
  # Confidence intervals for EMMs
  geom_errorbar(
    data = emm_df,
    aes(x = cdr_3grp, ymin = lower.CL, ymax = upper.CL),
    width = 0.2,
    inherit.aes = FALSE
  ) +
  scale_fill_manual(values = custom_colors) +
  scale_color_manual(values = custom_colors) +
  labs(
    title = "White Matter Hyperintensity Volume by Clinical Stage",
    x = "CDR Stage",
    y = "WMH volume (log10 mm³)",
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 24),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18),
    legend.position = "none"
  )


