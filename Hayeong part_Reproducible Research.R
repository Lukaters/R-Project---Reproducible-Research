
install.packages(c("tidyverse", "plm", "lmtest", "sandwich"))

library(tidyverse)
library(plm)
library(lmtest)   # for coeftest (cluster-robust SE)
library(sandwich) # for vcovHC




df <- read_csv("cleaned_data (1).csv")




nhp_weights <- list(
  EL = list(items = c(1, 12, 26),           weights = c(39.20, 36.80, 24.00)),
  P  = list(items = c(2, 4, 8, 19, 24, 28, 36, 38),
            weights = c(12.91, 19.74, 9.99, 11.22, 8.96, 20.86, 5.83, 10.49)),
  ER = list(items = c(3, 6, 7, 16, 20, 23, 31, 32, 37),
            weights = c(10.47, 9.31, 7.22, 7.08, 9.76, 13.99, 13.95, 16.21, 12.01)),
  S  = list(items = c(5, 13, 22, 29, 33),   weights = c(22.37, 12.57, 27.26, 16.10, 21.70)),
  SI = list(items = c(9, 15, 21, 30, 34),   weights = c(22.01, 19.36, 20.13, 22.53, 15.97)),
  PA = list(items = c(10, 11, 14, 17, 18, 25, 27, 35),
            weights = c(11.54, 10.57, 21.30, 10.79, 9.30, 12.61, 11.20, 12.69))
)

compute_nhp <- function(df, prefix, recode_2_to_0 = FALSE) {
 
  get_item <- function(i) {
    col_name <- if (nchar(prefix) > 5) {
      paste0(prefix, sprintf("%02d", i))   
    } else {
      paste0(prefix, sprintf("%02d", i))   
    }
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

nhp_2008 <- compute_nhp(df, prefix = "VZ02A",   recode_2_to_0 = TRUE)
nhp_2013 <- compute_nhp(df, prefix = "UR01A_",  recode_2_to_0 = FALSE)
nhp_2018 <- compute_nhp(df, prefix = "TP09A_",  recode_2_to_0 = FALSE)

df <- df %>%
  mutate(
    NHP_EL2008 = nhp_2008$EL,  NHP_P2008 = nhp_2008$P,
    NHP_ER2008 = nhp_2008$ER,  NHP_S2008 = nhp_2008$S,
    NHP_SI2008 = nhp_2008$SI,  NHP_PA2008 = nhp_2008$PA,
    NHP2008    = nhp_2008$NHP_TOTAL,
    
    NHP_EL2013 = nhp_2013$EL,  NHP_P2013 = nhp_2013$P,
    NHP_ER2013 = nhp_2013$ER,  NHP_S2013 = nhp_2013$S,
    NHP_SI2013 = nhp_2013$SI,  NHP_PA2013 = nhp_2013$PA,
    NHP2013    = nhp_2013$NHP_TOTAL,
    
    NHP_EL2018 = nhp_2018$EL,  NHP_P2018 = nhp_2018$P,
    NHP_ER2018 = nhp_2018$ER,  NHP_S2018 = nhp_2018$S,
    NHP_SI2018 = nhp_2018$SI,  NHP_PA2018 = nhp_2018$PA,
    NHP2018    = nhp_2018$NHP_TOTAL
  )


df <- df %>%
  mutate(
    AGESQ2008 = AGE2008^2,
    AGESQ2013 = AGE2013^2,
    AGESQ2018 = AGE2018^2,
    MALE = if_else(GENDER == 1, 1, 0),   
    
    
    EDUC_2 = if_else(EDUC2018 >= 4 & EDUC2018 <= 6, 1, 0),
    EDUC_3 = if_else(EDUC2018 >= 7 & EDUC2018 <= 8, 1, 0),
    
   
    JOB2008 = JOB2008_SCALE3,
    JOB2013 = JOB2013_SCALE3,
    JOB2018 = JOB2018_SCALE3
  )



df_long <- df %>%
  pivot_longer(
    cols = matches("^(AGE|AGESQ|EDUC|JOB|STDINC|SUBJPOSIT|NHP|NHP_EL|NHP_ER|NHP_P|NHP_PA|NHP_S|NHP_SI|MARRIED|SIZE)(2008|2013|2018)$"),
    names_to  = c(".value", "YEAR"),
    names_pattern = "^(.*?)(2008|2013|2018)$"
  ) %>%
  mutate(
    YEAR = as.numeric(YEAR),
    TIME = 1 + (YEAR - 2008) / 5,   
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
                         "SIZE_2", "SIZE_3", "SUBJPOSIT", "TIME2", "TIME3")
  
  df_h <- as.data.frame(pdata)
  
  for (v in time_varying_vars) {
    if (v %in% names(df_h)) {
      mean_v <- ave(df_h[[v]], df_h[["ANONID"]], FUN = function(x) mean(x, na.rm = TRUE))
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
    "SIZE_2_dev + SIZE_3_dev + SUBJPOSIT_dev + TIME2_dev + TIME3_dev + ",
    "AGE_mean + AGESQ_mean + JOB_mean + STDINC_mean + MARRIED_mean + ",
    "SIZE_2_mean + SIZE_3_mean + SUBJPOSIT_mean + ",
    "MALE + EDUC_2 + EDUC_3"
  )
  plm(as.formula(formula_str), data = data, model = "random")
}



cat("\n MODEL 1: Random Effects \n")
re_model <- plm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED + 
                  SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3,
                data = p_df, model = "random")
print(summary(re_model))

cat("\n MODEL 2: Fixed Effects \n")
fe_model <- plm(NHP ~ AGE + AGESQ + JOB + STDINC + MARRIED + 
                  SIZE_2 + SIZE_3 + SUBJPOSIT + TIME2 + TIME3,
                data = p_df, model = "within")
print(cluster_se(fe_model))

cat("\n MODEL 3: Hybrid Effects (no gender/educ)\n")
hybrid_no_invariant <- plm(
  NHP ~ AGE_dev + AGESQ_dev + JOB_dev + STDINC_dev + MARRIED_dev +
    SIZE_2_dev + SIZE_3_dev + SUBJPOSIT_dev + TIME2_dev + TIME3_dev +
    AGE_mean + AGESQ_mean + JOB_mean + STDINC_mean + MARRIED_mean +
    SIZE_2_mean + SIZE_3_mean + SUBJPOSIT_mean,
  data = p_df_h, model = "random"
)
print(summary(hybrid_no_invariant))

cat("\n MODEL 4: Hybrid Effects + Gender + Education (Main Model) \n")
hybrid_main <- run_hybrid("NHP", p_df_h)
print(summary(hybrid_main))



cat("\n HAUSMAN TEST \n")
phtest(fe_model, re_model)



components <- c("NHP_EL", "NHP_ER", "NHP_P", "NHP_PA", "NHP_S", "NHP_SI")
comp_labels <- c("Energy Level", "Emotional Reaction", "Pain",
                 "Physical Abilities", "Sleep", "Social Isolation")

cat("\n===== NHP COMPONENTS — HYBRID MODELS =====\n")
comp_results <- map2(components, comp_labels, function(comp, label) {
  cat(sprintf("\n--- %s (%s) ---\n", label, comp))
  m <- run_hybrid(comp, p_df_h)
  print(summary(m))
  m
})
names(comp_results) <- components



subjposit_within <- map_dfr(seq_along(components), function(i) {
  m   <- comp_results[[i]]
  cf  <- coef(summary(m))
  row <- cf["SUBJPOSIT_dev", ]
  tibble(
    Component = comp_labels[[i]],
    Estimate  = row["Estimate"],
    SE        = row["Std. Error"],
    CI_low    = row["Estimate"] - 1.96 * row["Std. Error"],
    CI_high   = row["Estimate"] + 1.96 * row["Std. Error"],
    p_value   = row["Pr(>|z|)"]
  )
})

cat("\n===== SUBJPOSIT WITHIN-PERSON EFFECTS BY COMPONENT =====\n")
print(subjposit_within)