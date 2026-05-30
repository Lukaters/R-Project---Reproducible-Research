library(tidyverse)

# Load the dataset and remove duplicate column names.
df <- read_csv("C:/Users/slapm/Downloads/cleaned_data (1).csv")

if(any(duplicated(names(df)))) {
  message("️ Found duplicate columns. Keeping first occurrence only.")
  df <- df %>% select(!duplicated(names(df)))
}



# Calculate 2008 NHP health scores from survey items.
recode_08 <- function(x) case_when(x == 1 ~ 1, x == 2 ~ 0, TRUE ~ NA_real_)

df <- df %>%
  mutate(
    NHP1_08  = recode_08(VZ02A01),  NHP2_08  = recode_08(VZ02A02),
    NHP3_08  = recode_08(VZ02A03),  NHP4_08  = recode_08(VZ02A04),
    NHP5_08  = recode_08(VZ02A05),  NHP6_08  = recode_08(VZ02A06),
    NHP7_08  = recode_08(VZ02A07),  NHP8_08  = recode_08(VZ02A08),
    NHP9_08  = recode_08(VZ02A09),  NHP10_08 = recode_08(VZ02A10),
    NHP11_08 = recode_08(VZ02A11),  NHP12_08 = recode_08(VZ02A12),
    NHP13_08 = recode_08(VZ02A13),  NHP14_08 = recode_08(VZ02A14),
    NHP15_08 = recode_08(VZ02A15),  NHP16_08 = recode_08(VZ02A16),
    NHP17_08 = recode_08(VZ02A17),  NHP18_08 = recode_08(VZ02A18),
    NHP19_08 = recode_08(VZ02A19),  NHP20_08 = recode_08(VZ02A20),
    NHP21_08 = recode_08(VZ02A21),  NHP22_08 = recode_08(VZ02A22),
    NHP23_08 = recode_08(VZ02A23),  NHP24_08 = recode_08(VZ02A24),
    NHP25_08 = recode_08(VZ02A25),  NHP26_08 = recode_08(VZ02A26),
    NHP27_08 = recode_08(VZ02A27),  NHP28_08 = recode_08(VZ02A28),
    NHP29_08 = recode_08(VZ02A29),  NHP30_08 = recode_08(VZ02A30),
    NHP31_08 = recode_08(VZ02A31),  NHP32_08 = recode_08(VZ02A32),
    NHP33_08 = recode_08(VZ02A33),  NHP34_08 = recode_08(VZ02A34),
    NHP35_08 = recode_08(VZ02A35),  NHP36_08 = recode_08(VZ02A36),
    NHP37_08 = recode_08(VZ02A37),  NHP38_08 = recode_08(VZ02A38)
  )

df <- df %>%
  mutate(
    NHP_EL2008 = NHP1_08*39.20 + NHP12_08*36.80 + NHP26_08*24.00,
    NHP_P2008  = NHP2_08*12.91 + NHP4_08*19.74  + NHP8_08*9.99   + NHP19_08*11.22 +
      NHP24_08*8.96 + NHP28_08*20.86 + NHP36_08*5.83  + NHP38_08*10.49,
    NHP_ER2008 = NHP3_08*10.47 + NHP6_08*9.31   + NHP7_08*7.22   + NHP16_08*7.08  +
      NHP20_08*9.76 + NHP23_08*13.99 + NHP31_08*13.95 + NHP32_08*16.21 + NHP37_08*12.01,
    NHP_S2008  = NHP5_08*22.37 + NHP13_08*12.57 + NHP22_08*27.26 + NHP29_08*16.10 + NHP33_08*21.70,
    NHP_SI2008 = NHP9_08*22.01 + NHP15_08*19.36 + NHP21_08*20.13 + NHP30_08*22.53 + NHP34_08*15.97,
    NHP_PA2008 = NHP10_08*11.54 + NHP11_08*10.57 + NHP14_08*21.30 + NHP17_08*10.79 +
      NHP18_08*9.30  + NHP25_08*12.61 + NHP27_08*11.20 + NHP35_08*12.69,
    NHP2008    = NHP_EL2008 + NHP_P2008 + NHP_ER2008 + NHP_S2008 + NHP_SI2008 + NHP_PA2008
  )

# Calculate 2013 NHP health scores from survey items.
recode_13 <- function(x) case_when(x == 1 ~ 1, x == 0 ~ 0, TRUE ~ NA_real_)

df <- df %>%
  mutate(
    NHP1_13  = recode_13(UR01A_01),  NHP2_13  = recode_13(UR01A_02),
    NHP3_13  = recode_13(UR01A_03),  NHP4_13  = recode_13(UR01A_04),
    NHP5_13  = recode_13(UR01A_05),  NHP6_13  = recode_13(UR01A_06),
    NHP7_13  = recode_13(UR01A_07),  NHP8_13  = recode_13(UR01A_08),
    NHP9_13  = recode_13(UR01A_09),  NHP10_13 = recode_13(UR01A_10),
    NHP11_13 = recode_13(UR01A_11),  NHP12_13 = recode_13(UR01A_12),
    NHP13_13 = recode_13(UR01A_13),  NHP14_13 = recode_13(UR01A_14),
    NHP15_13 = recode_13(UR01A_15),  NHP16_13 = recode_13(UR01A_16),
    NHP17_13 = recode_13(UR01A_17),  NHP18_13 = recode_13(UR01A_18),
    NHP19_13 = recode_13(UR01A_19),  NHP20_13 = recode_13(UR01A_20),
    NHP21_13 = recode_13(UR01A_21),  NHP22_13 = recode_13(UR01A_22),
    NHP23_13 = recode_13(UR01A_23),  NHP24_13 = recode_13(UR01A_24),
    NHP25_13 = recode_13(UR01A_25),  NHP26_13 = recode_13(UR01A_26),
    NHP27_13 = recode_13(UR01A_27),  NHP28_13 = recode_13(UR01A_28),
    NHP29_13 = recode_13(UR01A_29),  NHP30_13 = recode_13(UR01A_30),
    NHP31_13 = recode_13(UR01A_31),  NHP32_13 = recode_13(UR01A_32),
    NHP33_13 = recode_13(UR01A_33),  NHP34_13 = recode_13(UR01A_34),
    NHP35_13 = recode_13(UR01A_35),  NHP36_13 = recode_13(UR01A_36),
    NHP37_13 = recode_13(UR01A_37),  NHP38_13 = recode_13(UR01A_38)
  )

df <- df %>%
  mutate(
    NHP_EL2013 = NHP1_13*39.20 + NHP12_13*36.80 + NHP26_13*24.00,
    NHP_P2013  = NHP2_13*12.91 + NHP4_13*19.74  + NHP8_13*9.99   + NHP19_13*11.22 +
      NHP24_13*8.96 + NHP28_13*20.86 + NHP36_13*5.83  + NHP38_13*10.49,
    NHP_ER2013 = NHP3_13*10.47 + NHP6_13*9.31   + NHP7_13*7.22   + NHP16_13*7.08  +
      NHP20_13*9.76 + NHP23_13*13.99 + NHP31_13*13.95 + NHP32_13*16.21 + NHP37_13*12.01,
    NHP_S2013  = NHP5_13*22.37 + NHP13_13*12.57 + NHP22_13*27.26 + NHP29_13*16.10 + NHP33_13*21.70,
    NHP_SI2013 = NHP9_13*22.01 + NHP15_13*19.36 + NHP21_13*20.13 + NHP30_13*22.53 + NHP34_13*15.97,
    NHP_PA2013 = NHP10_13*11.54 + NHP11_13*10.57 + NHP14_13*21.30 + NHP17_13*10.79 +
      NHP18_13*9.30  + NHP25_13*12.61 + NHP27_13*11.20 + NHP35_13*12.69,
    NHP2013    = NHP_EL2013 + NHP_P2013 + NHP_ER2013 + NHP_S2013 + NHP_SI2013 + NHP_PA2013
  )

# Calculate 2018 NHP scores and safely handle missing variables.
get_col_or_na <- function(df, col) {
  if (col %in% names(df)) df[[col]] else rep(NA_real_, nrow(df))
}

df <- df %>%
  mutate(
    NHP_EL2018 = get_col_or_na(., "TP09A_01")*39.20 +
      get_col_or_na(., "TP09A_12")*36.80 +
      get_col_or_na(., "TP09A_26")*24.00,
    
    NHP_P2018  = get_col_or_na(., "TP09A_02")*12.91 +
      get_col_or_na(., "TP09A_04")*19.74 +
      get_col_or_na(., "TP09A_08")*9.99  +
      get_col_or_na(., "TP09A_19")*11.22 +
      get_col_or_na(., "TP09A_24")*8.96  +
      get_col_or_na(., "TP09A_28")*20.86 +
      get_col_or_na(., "TP09A_36")*5.83  +
      get_col_or_na(., "TP09A_38")*10.49,
    
    NHP_ER2018 = get_col_or_na(., "TP09A_03")*10.47 +
      get_col_or_na(., "TP09A_06")*9.31  +
      get_col_or_na(., "TP09A_07")*7.22  +
      get_col_or_na(., "TP09A_16")*7.08  +
      get_col_or_na(., "TP09A_20")*9.76  +
      get_col_or_na(., "TP09A_23")*13.99 +
      get_col_or_na(., "TP09A_31")*13.95 +
      get_col_or_na(., "TP09A_32")*16.21 +
      get_col_or_na(., "TP09A_37")*12.01,
    
    NHP_S2018  = get_col_or_na(., "TP09A_05")*22.37 +
      get_col_or_na(., "TP09A_13")*12.57 +
      get_col_or_na(., "TP09A_22")*27.26 +
      get_col_or_na(., "TP09A_29")*16.10 +
      get_col_or_na(., "TP09A_33")*21.70,
    
    NHP_SI2018 = get_col_or_na(., "TP09A_09")*22.01 +
      get_col_or_na(., "TP09A_15")*19.36 +
      get_col_or_na(., "TP09A_21")*20.13 +
      get_col_or_na(., "TP09A_30")*22.53 +
      get_col_or_na(., "TP09A_34")*15.97,
    
    NHP_PA2018 = get_col_or_na(., "TP09A_10")*11.54 +
      get_col_or_na(., "TP09A_11")*10.57 +
      get_col_or_na(., "TP09A_14")*21.30 +
      get_col_or_na(., "TP09A_17")*10.79 +
      get_col_or_na(., "TP09A_18")*9.30  +
      get_col_or_na(., "TP09A_25")*12.61 +
      get_col_or_na(., "TP09A_27")*11.20 +
      get_col_or_na(., "TP09A_35")*12.69,
    
    NHP2018 = NHP_EL2018 + NHP_P2018 + NHP_ER2018 + NHP_S2018 + NHP_SI2018 + NHP_PA2018
  )

# Recode demographic and socioeconomic control variables.
if ("VW05" %in% names(df)) df$SUBJMOB2008 <- 6L - df$VW05
if ("UM05" %in% names(df)) df$SUBJMOB2013 <- 6L - df$UM05
if ("TM31" %in% names(df)) df$SUBJMOB2018 <- 6L - df$TM31

df <- df %>%
  rename(
    JOB2008 = JOB2008_SCALE3,
    JOB2013 = JOB2013_SCALE3,
    JOB2018 = JOB2018_SCALE3
  )

if ("VM01" %in% names(df)) df$SUBJPOSIT2008 <- 11L - df$VM01
if ("UB04" %in% names(df)) df$SUBJPOSIT2013 <- if_else(df$UB04 <= 1, 1L, as.integer(df$UB04))
if ("TD01" %in% names(df)) df$SUBJPOSIT2018 <- if_else(df$TD01 <= 1, 1L, as.integer(df$TD01))

if ("VR01" %in% names(df)) df$MARRIED2008 <- case_when(df$VR01 == 2 ~ 1L, df$VR01 %in% c(1,3,4,5) ~ 0L, TRUE ~ NA_integer_)
if ("UK01" %in% names(df)) df$MARRIED2013 <- case_when(df$UK01 == 2 ~ 1L, df$UK01 %in% c(1,3,4,5) ~ 0L, TRUE ~ NA_integer_)
if ("TK01" %in% names(df)) df$MARRIED2018 <- case_when(df$TK01 == 2 ~ 1L, df$TK01 %in% c(1,3,4,5) ~ 0L, TRUE ~ NA_integer_)

if ("VR22" %in% names(df)) df$STDINC2008 <- as.numeric(scale(df$VR22))
if ("UK21" %in% names(df)) df$STDINC2013 <- as.numeric(scale(df$UK21))
if ("TK21" %in% names(df)) df$STDINC2018 <- as.numeric(scale(df$TK21))

df <- df %>%
  mutate(
    AGESQ2008 = AGE2008^2,
    AGESQ2013 = AGE2013^2,
    AGESQ2018 = AGE2018^2
  )

# Keep only the final variables needed for analysis.
keep_vars <- c(
  "ANONID", "WAVE2008", "WAVE2013", "WAVE2018", "GENDER", "YRBIRTH",
  "AGE2008", "AGE2013", "AGE2018",
  "VOIVOD2008", "VOIVOD2013", "VOIVOD2018",
  "SIZE2008", "SIZE2013", "SIZE2018",
  "EDUC2008", "EDUC2013", "EDUC2018",
  "JOB2008", "JOB2013", "JOB2018",
  "AGESQ2008", "AGESQ2013", "AGESQ2018",
  "STDINC2008", "STDINC2013", "STDINC2018",
  "MARRIED2008", "MARRIED2013", "MARRIED2018",
  "SUBJPOSIT2008", "SUBJPOSIT2013", "SUBJPOSIT2018",
  "SUBJMOB2008", "SUBJMOB2013", "SUBJMOB2018",
  "NHP2008", "NHP2013", "NHP2018",
  "NHP_EL2008", "NHP_ER2008", "NHP_P2008", "NHP_PA2008", "NHP_S2008", "NHP_SI2008",
  "NHP_EL2013", "NHP_ER2013", "NHP_P2013", "NHP_PA2013", "NHP_S2013", "NHP_SI2013",
  "NHP_EL2018", "NHP_ER2018", "NHP_P2018", "NHP_PA2018", "NHP_S2018", "NHP_SI2018"
)

df_final <- df %>% select(any_of(keep_vars))

# Check sample sizes and export the cleaned dataset.
cat("N with all 3 waves of NHP:\n")
df_final %>% filter(!is.na(NHP2008) & !is.na(NHP2013) & !is.na(NHP2018)) %>% count(GENDER) %>% print()

cat("\nN with NHP2008 + NHP2013:\n")
df_final %>% filter(!is.na(NHP2008) & !is.na(NHP2013)) %>% count(GENDER) %>% print()

cat("\nN with NHP2008 + NHP2018:\n")
df_final %>% filter(!is.na(NHP2008) & !is.na(NHP2018)) %>% count(GENDER) %>% print()

cat("\nN with NHP2013 + NHP2018:\n")
df_final %>% filter(!is.na(NHP2013) & !is.na(NHP2018)) %>% count(GENDER) %>% print()

cat("\nYRBIRTH summary (NHP2008 + NHP2013 + NHP2018):\n")

df_check <- df_final %>% filter(!is.na(NHP2008) & !is.na(NHP2013) | !is.na(NHP2018))
if(nrow(df_check) > 0) {
  df_check %>% summarise(across(YRBIRTH, list(mean=mean, min=min, max=max), na.rm=TRUE)) %>% print()
} else {
  cat("0 observations\n")
}

write_csv(df_final, "cleaned_data_computed.csv")
