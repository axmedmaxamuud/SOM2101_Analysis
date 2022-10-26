## FSL Indicator preparation
rm(list = ls())
today <- Sys.Date()

# load packages
library(tidyverse)
library(readxl)
library(openxlsx)
library(healthyr)

# Step 1: load clean data
df <- read_excel("input/HQ_validated_data/MSNA_CleanData_v6.xlsx")

# recoding RCSI indicators

df <- df %>% mutate(
  hhs_1 = case_when(
    hh_no_food == "yes" ~ 1,
    hh_no_food == "no" ~ 0,
  ),
  
  hhs_2 = case_when(
    hh_no_food_freq == "rarely" ~ 1,
    hh_no_food_freq == "sometimes" ~ 2,
    hh_no_food_freq == "ofter" ~ 3,
  ),
  
  hhs_3 = case_when(
    hh_hunger == "yes" ~ 1,
    hh_hunger == "no" ~ 0,
  ),
  
  hhs_4 = case_when(
    hh_hunger_freq == "rarely" ~ 1,
    hh_hunger_freq == "sometimes" ~ 2,
    hh_hunger_freq == "ofter" ~ 3,
  ),
  
  hhs_5 = case_when(
    fs_not_enough_food == "yes" ~ 1,
    fs_not_enough_food == "no" ~ 0,
  ),
  
  hhs_6 = case_when(
    freq_not_enough_food == "rarely" ~ 1,
    freq_not_enough_food == "sometimes" ~ 2,
    freq_not_enough_food == "ofter" ~ 3,
  ),
  
)

# Step 2: format dataset
df2 <- format_nut_health_indicators(df = df,
                                    cluster = "settlements",
                                    # FCS
                                    fcs_cereal = "fs_cereals_grains",
                                    fcs_legumes = "fs_beans_nuts",
                                    fcs_dairy = "fs_milk_dairy",
                                    fcs_meat = "fs_meat_fish",
                                    fcs_veg = "fs_vegetables_leaves",
                                    fcs_fruit = "fs_fruits",
                                    fcs_oil = "fs_oil",
                                    fcs_sugar = "fs_sugar",
                                    # HHs
                                    hhs_nofoodhh_1 = "hhs_1",
                                    hhs_nofoodhh_1a = "hhs_2", 
                                    hhs_sleephungry_2 = "hhs_3", 
                                    hhs_sleephungry_2a = "hhs_4", 
                                    hhs_alldaynight_3 = "hhs_5", 
                                    hhs_alldaynight_3a = "hhs_6",
                                    # RCSI
                                    rcsi_lesspreferred_1 = "rcsi_less_qlty",
                                    rcsi_borrowfood_2 = "rcsi_borrow",
                                    rcsi_limitportion_3 = "rcsi_meal_size",
                                    rcsi_restrict_4 = "rcsi_meal_adults",
                                    rcsi_reducemeals5 = "rcsi_meals_freq",
                                    # LCSI
                                    lcs_variables = c(
                                      "lcsi_emergency_1",
                                      "lcsi_emergency_2",
                                      "lcsi_emergency_3",
                                      "lcsi_crisis_1",
                                      "lcsi_crisis_2",
                                      "lcsi_crisis_3",
                                      "lcsi_stress_1",
                                      "lcsi_stress_2",
                                      "lcsi_stress_3",
                                      "lcsi_stress_4"))

(res <- analyse_survey_results(df = df2,
                               
                               aggregation = "clusters",
                               
                               sample_design = "two_stage_cluster",
                               cluster = "cluster",
                               
                               proportions = c("fcs_cat", "hhs_cat", "rcsi_cat", "lcs_cat",
                                               "fc_phase", "fclc_phase"),
                               
                               means = c("fcs_score", "hhs_score", "rcsi_score")))

(res_2 <- analyse_survey_results(df = df2,
                               
                               aggregation = "cluster_id",
                               
                               sample_design = "two_stage_cluster",
                               cluster = "cluster",
                               
                               proportions = c("fcs_cat", "hhs_cat", "rcsi_cat", "lcs_cat",
                                               "fc_phase", "fclc_phase"),
                               
                               means = c("fcs_score", "hhs_score", "rcsi_score")))






# Export final clean data
write.xlsx(df2, paste0("input/HQ_validated_data/clean-data-fsl_",today,".xlsx"))
write.xlsx(res, paste0("output/fsl-population-group_",today,".xlsx"))
write.xlsx(res_2, paste0("output/fsl-stata_id_",today,".xlsx"))




