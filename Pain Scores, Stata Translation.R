
# STATA TO R TRANSLATION PROJECT

# In this section,I was responsible for
#recoding one variable so that values 1 and 10 become 0, while everything else stays the same. 
#Then I convert a group of variables into simple 0/1 indicators. 
#After that, I create two new scores to factor in pain scores by combining several variables with weights. 
#I also square the age variable and keep only the columns I actually need. 
library(tidyverse)


df <- read_csv("C:/Users/slapm/Downloads/cleaned_data (1).csv")


df_final <- df %>%
  mutate(
    SUBJ_RECODED = case_when(
      TP09A_01 == 1  ~ 0,
      TP09A_01 == 10 ~ 0,
      TRUE           ~ as.numeric(TP09A_01)
    ),
    
    
    across(starts_with("VZ02A"), 
           ~ case_when(. == 1 ~ 1, . == 2 ~ 0, TRUE ~ as.numeric(.)),
           .names = "NHP{str_extract(.col, '\\\\d+$')}_08"),
    
    
    NHP_EL2008 = NHP01_08 * 39.2 + NHP12_08 * 36.80 + NHP26_08 * 24.00,
    NHP_P2008  = NHP02_08 * 12.91 + NHP04_08 * 19.74 + NHP08_08 * 9.99 + 
      NHP19_08 * 11.22 + NHP24_08 * 8.96 + NHP28_08 * 20.86 + 
      NHP36_08 * 5.83 + NHP38_08 * 10.49,
    
    
    JOB2008   = JOB2008_SCALE3,
    AGESQ2008 = AGE2008^2
  ) %>%
  
  select(ANONID, GENDER, AGE2008, AGESQ2008, JOB2008, 
         NHP_EL2008, NHP_P2008, SUBJ_RECODED)


#Descriptive Statistics
print("--- SUMMARY STATISTICS ---")
df_final %>% 
  select(AGE2008, NHP_EL2008, NHP_P2008, SUBJ_RECODED) %>% 
  summary() %>% 
  print()

print("--- GENDER TABLE ---")
table(df_final$GENDER) %>% print()


# Visualizations 
plot1 <- ggplot(df_final, aes(x = factor(SUBJ_RECODED))) +
  geom_bar(fill = "steelblue", color = "white") +
  labs(title = "Distribution of Recoded Variable (1 and 10 set to 0)",
       x = "Recoded Value",
       y = "Frequency") +
  theme_minimal()


plot2 <- ggplot(df_final, aes(x = NHP_P2008)) +
  geom_histogram(fill = "darkred", color = "white", bins = 15) +
  labs(title = "Distribution of NHP Pain Scores (2008)",
       x = "Pain Score",
       y = "Count") +
  theme_minimal()


print(plot1)
print(plot2)
