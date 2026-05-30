
library(tidyverse)
library(plm)

df <- read_csv("cleaned_data (1).csv")

df_cleaned <- df %>%
  mutate(
   
    AGESQ2008 = AGE2008^2,
    AGESQ2013 = AGE2013^2,
    AGESQ2018 = AGE2018^2,
    
    # Define gender data Malw 0r Female
    MALE = if_else(GENDER == 1, 1, 0)
  )

# (Hayeong part)
df_long <- df_cleaned %>%
  pivot_longer(
    cols = matches("^(AGE|EDUC|JOB|STDINC|SUBJPOSIT|NHP|MARRIED|SIZE|AGESQ).*20(08|13|18)$"),
    names_to = c(".value", "YEAR"),
    names_pattern = "(.*)(2008|2013|2018)"
  ) %>%
  mutate(
    YEAR = as.numeric(YEAR),
    TIME = 1 + (YEAR - 2008) / 5
  )

# 5. Panel Data analistics
p_df <- pdata.frame(df_long, index = c("ANONID", "TIME"))

# Compare between individuals
fe_result <- plm(NHP ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME), 
                 data = p_df, model = "within")

# (2) Random Effects
re_result <- plm(NHP ~ AGE + AGESQ + EDUC + JOB + STDINC + MARRIED + SIZE + SUBJPOSIT + factor(TIME) + MALE, 
                 data = p_df, model = "random")

# 6. Report reault
summary(fe_result)
summary(re_result)