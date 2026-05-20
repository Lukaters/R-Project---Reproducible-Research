library(plm)          # Panel linear models (re, fe)
library(panelr)       # xthybrid equivalent (within-between / Mundlak models)
library(modelsummary) # esttab equivalent - publication-ready tables
library(dplyr)        # Data manipulation
library(ggplot2)      # Figures (kdensity, histogram)
library(patchwork)    # Combining plots (graph combine equivalent)

# BLOCK 1: MAIN RESULTS
# Compare OLS, Random Effects, Fixed Effects, and Hybrid models on NHP

# --- Convert to panel data format ---
pdata <- pdata.frame(df, index = c("ANONID", "TIME"))

# Model 1: Pooled OLS (equivalent to Stata's `reg`)
m1_ols <- lm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED +
  SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3, data = df)

# Model 2: Random Effects (equivalent to Stata's `xtreg, re`)
m2_re <- plm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED +
  SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3, data = pdata, model = "random")

# Model 3: Fixed Effects (equivalent to Stata's `xtreg, fe`)
m3_fe <- plm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED +
  SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3, data = pdata, model = "within")

# Model 4: Hybrid / Within-Between model (`xthybrid`)

panel_df <- panel_data(df, id = ANONID, wave = TIME)

m4_hybrid_base <- wbm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED +
                         SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3,
                       data = panel_df)

m5_hybrid_full <- wbm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED +
                         SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 +
                         MALE + EDUC_2 + EDUC_3,
                       data = panel_df)

# Summary table (esttab with bic, aic, ci)
modelsummary(
  list("OLS" = m1_ols, "RE" = m2_re, "FE" = m3_fe,
       "Hybrid (base)" = m4_hybrid_base, "Hybrid (full)" = m5_hybrid_full),
  statistic = "conf.int",
  gof_map = c("aic", "bic")
)

# BLOCK 2: SUB-DIMENSIONS OF NHP — Hybrid model, base SUBJPOSIT
# Run the same full hybrid model for each NHP sub-scale

nhp_subscales <- c("NHP_EL", "NHP_ER", "NHP_P", "NHP_PA", "NHP_S", "NHP_SI")

models_block2 <- lapply(nhp_subscales, function(outcome) {
  formula <- as.formula(paste(
    outcome, "~ AGE + AGESQ + JOB + STDINC + MARRIED +
    SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 + MALE + EDUC_2 + EDUC_3"
  ))
  wbm(formula, data = panel_df)
})
names(models_block2) <- nhp_subscales

modelsummary(
  models_block2,
  statistic = "conf.int"
)

# BLOCK 3: SUB-DIMENSIONS OF NHP — SUBJPOSIT split into category dummies
# Replace single SUBJPOSIT with dummies SUBJPOSIT_1, _2, _4, _5

models_block3 <- lapply(nhp_subscales, function(outcome) {
  formula <- as.formula(paste(
    outcome, "~ AGE + AGESQ + JOB + STDINC + MARRIED +
    SIZE_2 + SIZE_3 + SUBJPOSIT_1 + SUBJPOSIT_2 + SUBJPOSIT_4 + SUBJPOSIT_5 +
    TIME2 + TIME3 + MALE + EDUC_2 + EDUC_3"
  ))
  wbm(formula, data = panel_df)
})
names(models_block3) <- nhp_subscales

modelsummary(models_block3, statistic = "conf.int")

# BLOCK 4: INTERACTION ANALYSES (Supplementary)
# Create interaction terms, then re-run hybrid models for each subscale

#interaction variables
df <- df %>%
  mutate(
    SUBJPOSIT_MALE = SUBJPOSIT * MALE,   # Social position × Gender
    SUBJPOSIT_AGE  = SUBJPOSIT * AGE,    # Social position × Age
    SUBJPOSIT_EDUC = SUBJPOSIT * EDUC    # Social position × Education
  )

panel_df <- panel_data(df, id = ANONID, wave = TIME)  # refresh with new vars

#4a: Interaction with MALE
models_block4a <- lapply(nhp_subscales, function(outcome) {
  formula <- as.formula(paste(
    outcome, "~ AGE + AGESQ + JOB + STDINC + MARRIED +
    SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_MALE +
    TIME2 + TIME3 + MALE + EDUC_2 + EDUC_3"
  ))
  wbm(formula, data = panel_df)
})
names(models_block4a) <- nhp_subscales

modelsummary(models_block4a, statistic = "conf.int")

#4b: Interaction with AGE
models_block4b <- lapply(nhp_subscales, function(outcome) {
  formula <- as.formula(paste(
    outcome, "~ AGE + AGESQ + JOB + STDINC + MARRIED +
    SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_AGE +
    TIME2 + TIME3 + MALE + EDUC_2 + EDUC_3"
  ))
  wbm(formula, data = panel_df)
})
names(models_block4b) <- nhp_subscales

modelsummary(models_block4b, statistic = "conf.int")

#4c: Interaction with EDUC (continuous)
models_block4c <- lapply(nhp_subscales, function(outcome) {
  formula <- as.formula(paste(
    outcome, "~ AGE + AGESQ + JOB + STDINC + MARRIED +
    SIZE_2 + SIZE_3 + SUBJPOSIT + SUBJPOSIT_EDUC +
    TIME2 + TIME3 + MALE + EDUC"
  ))
  wbm(formula, data = panel_df)
})
names(models_block4c) <- nhp_subscales

modelsummary(models_block4c, statistic = "conf.int")

# BLOCK 5: SUBSAMPLE — OLDER ADULTS (AGE > 60)
# Repeat main model comparison restricted to respondents over 60

df_old     <- df %>% filter(AGE > 60)
pdata_old  <- pdata.frame(df_old, index = c("ANONID", "TIME"))
panel_old  <- panel_data(df_old, id = ANONID, wave = TIME)

m1_ols_old   <- lm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED +
  SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3, data = df_old)
m2_re_old    <- plm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED +
  SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3,  data = pdata_old, model = "random")
m3_fe_old    <- plm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED +
  SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3,  data = pdata_old, model = "within")

m4_hybrid_old_base <- wbm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED +
                              SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3,
                            data = panel_old)

m5_hybrid_old_full <- wbm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED +
                              SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3 +
                              MALE + EDUC_2 + EDUC_3,
                            data = panel_old)

modelsummary(
  list("OLS" = m1_ols_old, "RE" = m2_re_old, "FE" = m3_fe_old,
       "Hybrid (base)" = m4_hybrid_old_base, "Hybrid (full)" = m5_hybrid_old_full)
)

# BLOCK 6: FIGURES
# Histogram of SUBJPOSIT + density plots for NHP and all sub-scales
# Equivalent to Stata's histogram + kdensity + graph combine

df_plot <- df %>% filter(TIME >= 1 & TIME <= 3)

ggplot(df_plot, aes(x = SUBJPOSIT)) +
  geom_histogram(aes(y = after_stat(density))) +
  stat_function(fun = dnorm,
                args = list(mean = mean(df_plot$SUBJPOSIT, na.rm = TRUE),
                            sd   = sd(df_plot$SUBJPOSIT, na.rm = TRUE)))

fig_nhp <- ggplot(df_plot, aes(x = NHP))    + geom_density(aes(fill = after_stat(density)))
fig_nhp_el <- ggplot(df_plot, aes(x = NHP_EL)) + geom_density(aes(fill = after_stat(density)))
fig_nhp_er <- ggplot(df_plot, aes(x = NHP_ER)) + geom_density(aes(fill = after_stat(density)))
fig_nhp_p <- ggplot(df_plot, aes(x = NHP_P))  + geom_density(aes(fill = after_stat(density)))
fig_nhp_pa <- ggplot(df_plot, aes(x = NHP_PA)) + geom_density(aes(fill = after_stat(density)))
fig_nhp_s <- ggplot(df_plot, aes(x = NHP_S))  + geom_density(aes(fill = after_stat(density)))
fig_nhp_si <- ggplot(df_plot, aes(x = NHP_SI)) + geom_density(aes(fill = after_stat(density)))

(fig_nhp | fig_nhp_el | fig_nhp_er | fig_nhp_p) /
(fig_nhp_pa | fig_nhp_s | fig_nhp_si)


table(df$TIME)