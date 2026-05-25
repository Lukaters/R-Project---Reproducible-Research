install.packages(c("tidyverse", "plm", "lmtest", "sandwich", "ggplot2"))
library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)
library(ggplot2)



nhp_weights <- list(
  EL = list(items = c(1, 12, 26),
             weights = c(39.20, 36.80, 24.00)),
  P  = list(items = c(2, 4, 8, 19, 24, 28, 36, 38),
             weights = c(12.91, 19.74, 9.99, 11.22, 8.96, 20.86, 5.83, 10.49)),
  ER = list(items = c(3, 6, 7, 16, 20, 23, 31, 32, 37),
             weights = c(10.47, 9.31, 7.22, 7.08, 9.76, 13.99, 13.95, 16.21, 12.01)),
  S  = list(items = c(5, 13, 22, 29, 33),
             weights = c(22.37, 12.57, 27.26, 16.10, 21.70)),
  SI = list(items = c(9, 15, 21, 30, 34),
             weights = c(22.01, 19.36, 20.13, 22.53, 15.97)),
  PA = list(items = c(10, 11, 14, 17, 18, 25, 27, 35),
             weights = c(11.54, 10.57, 21.30, 10.79, 9.30, 12.61, 11.20, 12.69))
)

compute_nhp <- function(df, prefix, recode_2_to_0 = FALSE) {
  get_item <- function(i) {
    col_name <- paste0(prefix, sprintf("%02d", i))
    x <- df[[col_name]]
    if (recode_2_to_0) {
      x <- case_when(x == 1 ~ 1, x == 2 ~ 0, TRUE ~ NA_real_)
    } else {
      x <- case_when(x == 1 ~ 1, x == 0 ~ 0, TRUE ~ NA_real_)
    }
    x
  }
  scores <- map_dfc(names(nhp_weights), function(comp) {
    w  <- nhp_weights[[comp]]
    sc <- map2(w$items, w$weights, ~ get_item(.x) * .y)
    tibble(!!comp := rowSums(bind_cols(sc), na.rm = FALSE))
  })
  scores$NHP_TOTAL <- rowSums(scores, na.rm = FALSE)
  scores
}

nhp_2008 <- compute_nhp(df, prefix = "VZ02A",  recode_2_to_0 = TRUE)
nhp_2013 <- compute_nhp(df, prefix = "UR01A_", recode_2_to_0 = FALSE)
nhp_2018 <- compute_nhp(df, prefix = "TP09A_", recode_2_to_0 = FALSE)

df <- df %>%
  mutate(
    NHP_EL2008 = nhp_2008$EL,  NHP_P2008  = nhp_2008$P,
    NHP_ER2008 = nhp_2008$ER,  NHP_S2008  = nhp_2008$S,
    NHP_SI2008 = nhp_2008$SI,  NHP_PA2008 = nhp_2008$PA,
    NHP2008    = nhp_2008$NHP_TOTAL,

    NHP_EL2013 = nhp_2013$EL,  NHP_P2013  = nhp_2013$P,
    NHP_ER2013 = nhp_2013$ER,  NHP_S2013  = nhp_2013$S,
    NHP_SI2013 = nhp_2013$SI,  NHP_PA2013 = nhp_2013$PA,
    NHP2013    = nhp_2013$NHP_TOTAL,

    NHP_EL2018 = nhp_2018$EL,  NHP_P2018  = nhp_2018$P,
    NHP_ER2018 = nhp_2018$ER,  NHP_S2018  = nhp_2018$S,
    NHP_SI2018 = nhp_2018$SI,  NHP_PA2018 = nhp_2018$PA,
    NHP2018    = nhp_2018$NHP_TOTAL
  )


df <- df %>%
  mutate(
    AGESQ2008 = AGE2008^2,
    AGESQ2013 = AGE2013^2,
    AGESQ2018 = AGE2018^2,
    MALE = if_else(GENDER == 1, 1, 0),

    
    EDUC_22008 = if_else(EDUC2008 >= 4 & EDUC2008 <= 6, 1, 0),
    EDUC_32008 = if_else(EDUC2008 >= 7 & EDUC2008 <= 8, 1, 0),
    EDUC_22013 = if_else(EDUC2013 >= 4 & EDUC2013 <= 6, 1, 0),
    EDUC_32013 = if_else(EDUC2013 >= 7 & EDUC2013 <= 8, 1, 0),
    EDUC_22018 = if_else(EDUC2018 >= 4 & EDUC2018 <= 6, 1, 0),
    EDUC_32018 = if_else(EDUC2018 >= 7 & EDUC2018 <= 8, 1, 0),

    JOB2008 = JOB2008_SCALE3,
    JOB2013 = JOB2013_SCALE3,
    JOB2018 = JOB2018_SCALE3
  )


df_long <- df %>%
  pivot_longer(
    cols = matches("^(AGE|AGESQ|EDUC|EDUC_2|EDUC_3|JOB|STDINC|SUBJPOSIT|NHP|NHP_EL|NHP_ER|NHP_P|NHP_PA|NHP_S|NHP_SI|MARRIED|SIZE)(2008|2013|2018)$"),
    names_to  = c(".value", "YEAR"),
    names_pattern = "^(.*?)(2008|2013|2018)$"
  ) %>%
  mutate(
    YEAR  = as.numeric(YEAR),
    TIME  = 1 + (YEAR - 2008) / 5,
    TIME2 = as.integer(TIME == 2),
    TIME3 = as.integer(TIME == 3),
    SIZE_2 = if_else(SIZE >= 2 & SIZE <= 3, 1, 0),
    SIZE_3 = if_else(SIZE >= 4 & SIZE <= 5, 1, 0)
  )

p_df <- pdata.frame(df_long, index = c("ANONID", "TIME"))




cluster_se <- function(model) {
  coeftest(model, vcov = vcovHC(model, type = "HC1", cluster = "group"))
}


make_hybrid_data <- function(pdata) {
  
  time_varying_vars <- c("AGE", "AGESQ", "JOB", "STDINC", "MARRIED",
                         "SIZE_2", "SIZE_3", "SUBJPOSIT",
                         "EDUC_2", "EDUC_3",
                         "TIME2", "TIME3")
  df_h <- as.data.frame(pdata)
  for (v in time_varying_vars) {
    if (v %in% names(df_h)) {
      mean_v <- ave(df_h[[v]], df_h[["ANONID"]],
                   FUN = function(x) mean(x, na.rm = TRUE))
      df_h[[paste0(v, "_mean")]] <- mean_v
      df_h[[paste0(v, "_dev")]]  <- df_h[[v]] - mean_v
    }
  }
  pdata.frame(df_h, index = c("ANONID", "TIME"))
}

p_df_h <- make_hybrid_data(p_df)


run_hybrid <- function(outcome, data) {
  formula_str <- paste0(
    outcome, " ~ ",
    "AGE_dev + AGESQ_dev + JOB_dev + STDINC_dev + MARRIED_dev + ",
    "SIZE_2_dev + SIZE_3_dev + SUBJPOSIT_dev + ",
    "EDUC_2_dev + EDUC_3_dev + ",           # FIX #2: time-varying educ
    "TIME2_dev + TIME3_dev + ",
    "AGE_mean + AGESQ_mean + JOB_mean + STDINC_mean + MARRIED_mean + ",
    "SIZE_2_mean + SIZE_3_mean + SUBJPOSIT_mean + ",
    "EDUC_2_mean + EDUC_3_mean + ",
    "MALE"                                  # MALE: time-invariant, not demeaned
  )
  model <- plm(as.formula(formula_str), data = data, model = "random")
  model  
}


cat("\n===== MODEL 1: Random Effects =====\n")
re_model <- plm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED +
                  SIZE_2 + SIZE_3 + SUBJPOSIT + EDUC_2 + EDUC_3 +
                  TIME2 + TIME3 + MALE,
                data = p_df, model = "random")
print(cluster_se(re_model))

cat("\n===== MODEL 2: Fixed Effects =====\n")
fe_model <- plm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED +
                  SIZE_2 + SIZE_3 + SUBJPOSIT + EDUC_2 + EDUC_3 +
                  TIME2 + TIME3,
                data = p_df, model = "within")
print(cluster_se(fe_model))

cat("\n===== MODEL 3: Hybrid (no time-invariant) =====\n")
hybrid_m3 <- plm(
  NHP ~ AGE_dev + AGESQ_dev + JOB_dev + STDINC_dev + MARRIED_dev +
    SIZE_2_dev + SIZE_3_dev + SUBJPOSIT_dev +
    EDUC_2_dev + EDUC_3_dev + TIME2_dev + TIME3_dev +
    AGE_mean + AGESQ_mean + JOB_mean + STDINC_mean + MARRIED_mean +
    SIZE_2_mean + SIZE_3_mean + SUBJPOSIT_mean +
    EDUC_2_mean + EDUC_3_mean,
  data = p_df_h, model = "random"
)
print(cluster_se(hybrid_m3))   # FIX #3: clustered SEs

cat("\n===== MODEL 4: Hybrid + MALE (Main Model) =====\n")
hybrid_main <- run_hybrid("NHP", p_df_h)
print(cluster_se(hybrid_main))   # FIX #3: clustered SEs


cat("\n===== HAUSMAN TEST =====\n")
print(phtest(fe_model, re_model))


cat("\n===== GENDER-STRATIFIED: MALES (GENDER == 1) =====\n")
p_df_male   <- p_df[p_df$GENDER == 1, ]
p_df_female <- p_df[p_df$GENDER == 2, ]

components <- c("NHP_EL", "NHP_ER", "NHP_P", "NHP_PA", "NHP_S", "NHP_SI", "NHP")
comp_labels <- c("Energy Level", "Emotional Reaction", "Pain",
                 "Physical Abilities", "Sleep", "Social Isolation", "NHP Total")

for (comp in components) {
  cat(sprintf("\n--- FE: %s (Males) ---\n", comp))
  m_male <- tryCatch(
    plm(as.formula(paste0(comp, " ~ AGE + AGESQ + JOB + STDINC + MARRIED +
        SIZE_2 + SIZE_3 + SUBJPOSIT + EDUC_2 + EDUC_3 + TIME2 + TIME3")),
        data = p_df_male, model = "within"),
    error = function(e) { cat("  Error:", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(m_male)) print(cluster_se(m_male))
}

cat("\n===== GENDER-STRATIFIED: FEMALES (GENDER == 2) =====\n")
for (comp in components) {
  cat(sprintf("\n--- FE: %s (Females) ---\n", comp))
  m_female <- tryCatch(
    plm(as.formula(paste0(comp, " ~ AGE + AGESQ + JOB + STDINC + MARRIED +
        SIZE_2 + SIZE_3 + SUBJPOSIT + EDUC_2 + EDUC_3 + TIME2 + TIME3")),
        data = p_df_female, model = "within"),
    error = function(e) { cat("  Error:", conditionMessage(e), "\n"); NULL }
  )
  if (!is.null(m_female)) print(cluster_se(m_female))
}

# RE versions by gender (matching Stata's xtreg ... or re if GENDER==1/2)
cat("\n===== GENDER-STRATIFIED RE: MALES =====\n")
re_male <- plm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED +
                 SIZE_2 + SIZE_3 + SUBJPOSIT + EDUC_2 + EDUC_3 + TIME2 + TIME3,
               data = p_df_male, model = "random")
print(cluster_se(re_male))

cat("\n===== GENDER-STRATIFIED RE: FEMALES =====\n")
re_female <- plm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED +
                   SIZE_2 + SIZE_3 + SUBJPOSIT + EDUC_2 + EDUC_3 + TIME2 + TIME3,
                 data = p_df_female, model = "random")
print(cluster_se(re_female))


nhp_components  <- c("NHP_EL","NHP_ER","NHP_P","NHP_PA","NHP_S","NHP_SI")
nhp_comp_labels <- c("Energy Level","Emotional Reaction","Pain",
                     "Physical Abilities","Sleep","Social Isolation")

cat("\n===== NHP COMPONENTS — HYBRID MODEL 4 =====\n")
comp_results <- map2(nhp_components, nhp_comp_labels, function(comp, label) {
  cat(sprintf("\n--- %s (%s) ---\n", label, comp))
  m <- run_hybrid(comp, p_df_h)
  print(cluster_se(m))   # FIX #3
  m
})
names(comp_results) <- nhp_components


subjposit_within <- map_dfr(seq_along(nhp_components), function(i) {
  cf  <- coef(summary(comp_results[[i]]))
  row <- cf["SUBJPOSIT_dev", ]
  tibble(
    Component = nhp_comp_labels[[i]],
    Estimate  = row["Estimate"],
    SE        = row["Std. Error"],
    CI_low    = row["Estimate"] - 1.96 * row["Std. Error"],
    CI_high   = row["Estimate"] + 1.96 * row["Std. Error"],
    p_value   = row["Pr(>|z|)"]
  )
})
cat("\n===== SUBJPOSIT WITHIN-PERSON EFFECTS BY COMPONENT =====\n")
print(subjposit_within)


cat("\n===== CURVILINEAR AGE EFFECT (Figure S2 equivalent) =====\n")

df_long_age <- as.data.frame(p_df) %>%
  filter(!is.na(AGE), !is.na(NHP)) %>%
  mutate(AGE_5Y = cut(AGE,
                      breaks = c(20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 95),
                      labels = c("21-25","26-30","31-35","36-40","41-45",
                                 "46-50","51-55","56-60","61-65","66-70","71-75","76+"),
                      right  = TRUE))


age_margins <- df_long_age %>%
  group_by(AGE_5Y) %>%
  summarise(
    mean_NHP = mean(NHP, na.rm = TRUE),
    se_NHP   = sd(NHP, na.rm = TRUE) / sqrt(n()),
    n        = n(),
    .groups  = "drop"
  )

print(age_margins)


age_plot <- ggplot(age_margins, aes(x = AGE_5Y, y = mean_NHP, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_errorbar(aes(ymin = mean_NHP - 1.96 * se_NHP,
                    ymax = mean_NHP + 1.96 * se_NHP),
                width = 0.3, color = "steelblue") +
  labs(
    title = "Curvilinear Association between Age and NHP Score",
    x = "Age Group",
    y = "Mean NHP Score (higher = worse health)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(age_plot)
ggsave("curvilinear_age_NHP.png", age_plot, width = 8, height = 5)
cat("Plot saved as curvilinear_age_NHP.png\n")
