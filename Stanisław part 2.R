


library(tidyverse)
library(plm)
library(lme4)
library(broom.mixed)


# Claude AI was utilized in the assisting of translating the code from Stata to R
# Load the dataset
df_wide <- read_csv("cleaned_data_computed.csv")


# Convert dataset from wide format (separate columns per wave) to long format
# so that each individual appears once per time period (2008, 2013, 2018)
df_long <- df_wide %>%
  pivot_longer(
    cols = matches("^(AGE|AGESQ|VOIVOD|SIZE|EDUC|JOB|NHP|NHP_EL|NHP_ER|NHP_P|NHP_PA|NHP_S|NHP_SI)(2008|2013|2018)$"),
    names_to  = c(".value", "YEAR"),
    names_pattern = "^(.*?)(2008|2013|2018)$"
  ) %>%
  mutate(
    # Convert year into numeric time index for panel structure
    YEAR  = as.numeric(YEAR),
    TIME  = 1 + (YEAR - 2008) / 5,
    TIME2 = as.integer(TIME == 2),
    TIME3 = as.integer(TIME == 3),
    
    # Create demographic and household structure variables
    MALE  = as.integer(GENDER == 1),
    SIZE_2 = if_else(SIZE >= 2 & SIZE <= 3, 1L, 0L),
    SIZE_3 = if_else(SIZE >= 4 & SIZE <= 5, 1L, 0L),
    
    # Create education category dummies
    EDUC_1 = case_when(EDUC >= 1 & EDUC <= 3 ~ 1L, EDUC >= 1 & EDUC <= 8 ~ 0L, TRUE ~ NA_integer_),
    EDUC_2 = case_when(EDUC >= 4 & EDUC <= 6 ~ 1L, EDUC >= 1 & EDUC <= 8 ~ 0L, TRUE ~ NA_integer_),
    EDUC_3 = case_when(EDUC >= 7 & EDUC <= 8 ~ 1L, EDUC >= 1 & EDUC <= 8 ~ 0L, TRUE ~ NA_integer_),
    
    # Define age thresholds for subgroup analysis
    age_40_90 = as.integer(AGE > 40),
    age_45_90 = as.integer(AGE > 45)
  )



# Identify observations with no missing values across key regression variables
model_vars <- c("NHP", "AGE", "AGESQ", "JOB", "SIZE_2", "SIZE_3",
                "TIME2", "TIME3", "MALE", "EDUC_2", "EDUC_3")

df_long <- df_long %>%
  mutate(sample_full = as.integer(rowSums(is.na(across(all_of(model_vars)))) == 0))



# Count number of observations per wave for the estimation sample
cat("\n--- WAVE COUNTS (sample_full only) ---\n")
df_long %>%
  filter(sample_full == 1) %>%
  count(TIME) %>%
  print()

# Display summary statistics for key regression variables
cat("\n--- SUMMARY STATISTICS (sample_full, key vars) ---\n")
df_long %>%
  filter(sample_full == 1) %>%
  select(AGE, AGESQ, JOB, SIZE_2, SIZE_3,
         TIME2, TIME3, MALE, EDUC_2, EDUC_3) %>%
  summary() %>%
  print()

# Examine gender composition across different wave-completeness conditions
cat("\n--- GENDER by wave completeness ---\n")
df_wide %>%
  filter(!is.na(NHP2018) & !is.na(NHP2013) & !is.na(NHP2008)) %>%
  count(GENDER) %>% print()

df_wide %>%
  filter(!is.na(NHP2018) & !is.na(NHP2008)) %>%
  count(GENDER) %>% print()

df_wide %>%
  filter(!is.na(NHP2018) & !is.na(NHP2013)) %>%
  count(GENDER) %>% print()

df_wide %>%
  filter(!is.na(NHP2008) & !is.na(NHP2013)) %>%
  count(GENDER) %>% print()



# Convert dataset into panel-data format indexed by individual ID and time wave
pdata <- pdata.frame(
  df_long %>% filter(sample_full == 1),
  index = c("ANONID", "TIME")
)

# Define baseline regression specification
covars <- c("AGE", "AGESQ", "JOB", "SIZE_2", "SIZE_3", "TIME2", "TIME3")
fmla_base <- as.formula(paste("NHP ~", paste(covars, collapse = " + ")))

# (Re-specification used for Hausman test excluding time dummies)
covars <- c("AGE", "AGESQ", "JOB", "SIZE_2", "SIZE_3")
fmla_base <- as.formula(paste("NHP ~", paste(covars, collapse = " + ")))



# Compare fixed effects vs random effects estimators to test correlation with unobserved heterogeneity
cat("\n--- HAUSMAN TEST ---\n")

fe_model <- plm(fmla_base, data = pdata, model = "within")
re_model <- plm(fmla_base, data = pdata, model = "random")

print(hausman_test <- phtest(fe_model, re_model))

# Define function to estimate Mundlak models separating within- and between-individual effects
run_mundlak <- function(data, outcome, predictors, cluster_id = "ANONID") {
  
  # Identify time-varying predictors for decomposition
  time_varying <- setdiff(predictors, c("MALE", "EDUC_2", "EDUC_3"))
  
  # Compute individual-level means (between effects)
  means <- data %>%
    group_by(!!sym(cluster_id)) %>%
    summarise(across(all_of(time_varying),
                     ~ mean(., na.rm = TRUE),
                     .names = "{.col}_mean"), .groups = "drop")
  
  # Merge means and compute within-individual deviations
  d <- data %>%
    left_join(means, by = cluster_id) %>%
    mutate(across(all_of(time_varying),
                  ~ . - get(paste0(cur_column(), "_mean")),
                  .names = "{.col}_within"))
  
  # Define variable sets for model
  within_vars  <- paste0(time_varying, "_within")
  mean_vars    <- paste0(time_varying, "_mean")
  time_dummies <- intersect(predictors, c("MALE", "EDUC_2", "EDUC_3"))
  
  # Construct Mundlak regression formula
  fmla <- as.formula(paste(
    outcome, "~",
    paste(c(within_vars, mean_vars, time_dummies), collapse = " + ")
  ))
  
  # Estimate mixed-effects model with random intercepts by individual
  lme4::lmer(
    update(fmla, . ~ . + (1 | ANONID)),
    data = d,
    REML = FALSE
  )
}

# Define predictors and outcomes for analysis
predictors_full <- c(covars, "MALE", "EDUC_2", "EDUC_3")
outcomes <- c("NHP", "NHP_EL", "NHP_ER", "NHP_P", "NHP_PA", "NHP_S", "NHP_SI")


# Subset data for respondents older than 40
data_40 <- df_long %>% filter(sample_full == 1, age_40_90 == 1)

# Estimate Mundlak models for each outcome (age > 40 sample)
results_40 <- map(outcomes, function(y) {
  cat("Running Mundlak model:", y, "(age>40)\n")
  tryCatch(
    suppressWarnings(run_mundlak(data_40, y, predictors_full)),
    error = function(e) { cat("  ERROR:", conditionMessage(e), "\n"); NULL }
  )
}) %>% set_names(outcomes)

# Extract and display fixed-effect coefficients with confidence intervals
cat("\n--- RESULTS TABLE (age > 40, 95% CI) ---\n")
map_dfr(results_40, ~ if (!is.null(.x)) tidy(.x, conf.int = TRUE) else NULL,
        .id = "outcome") %>%
  filter(effect == "fixed") %>%
  select(outcome, term, estimate, conf.low, conf.high) %>%
  print(n = Inf)





# Subset data for respondents older than 45
data_45 <- df_long %>% filter(sample_full == 1, age_45_90 == 1)

# Estimate Mundlak models for each outcome (age > 45 sample)
results_45 <- map(outcomes, function(y) {
  cat("Running Mundlak model:", y, "(age>45)\n")
  tryCatch(
    suppressWarnings(run_mundlak(data_45, y, predictors_full)),
    error = function(e) { cat("  ERROR:", conditionMessage(e), "\n"); NULL }
  )
}) %>% set_names(outcomes)

# Extract and display fixed-effect coefficients with confidence intervals
cat("\n--- RESULTS TABLE (age > 45, 95% CI) ---\n")
map_dfr(results_45, ~ if (!is.null(.x)) tidy(.x, conf.int = TRUE) else NULL,
        .id = "outcome") %>%
  filter(effect == "fixed") %>%
  select(outcome, term, estimate, conf.low, conf.high) %>%
  print(n = Inf)
