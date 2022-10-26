## Shelter loop Analysis
rm(list = ls())
today <- Sys.Date()

# load packages
library(tidyverse)
library(tidyr)
library(readxl)
library(openxlsx)
library(srvyr)
library(reshape2)

# load shelter loop data
shelter_df <- read_excel("input/data/REACH_SOM_MSNA_CleanData_Dissemination.xlsx", sheet = "shelter_repeat") %>%
  select(uuid, shelter_types, strata, weights)

# reshape data
data <- shelter_df %>% 
  pivot_wider(names_from = shelter_types, values_from = weights, values_fn = sum)


df <- dcast(data = shelter_df, formula = uuid~shelter_types, fun.aggregate = sum, value.var = "weights")




select_one_topX <- function(df, question_name, X = 3) { # by default return top 3
  # test message
  if(length(colnames(df)[grepl(question_name, colnames(df), fixed = T)]) == 0) {
    stop(print(paste("question name:", question_name, "doesn't exist in the main dataset. Please double check and make required changes!")))
  }
  
  df <- df %>%
    select(!!sym(question_name))%>%          
    filter(!is.na(!!sym(question_name))) %>% # remove NA values from percentage calculations 
    mutate_at(question_name, as.factor) %>%  # added to have all groups
    group_by(!!sym(question_name), .drop=F) %>% 
    summarise(n = n()) %>%
    mutate(percentages = round(n / sum(n) * 100, digits = 2)) %>%
    arrange(desc(percentages)) %>% 
    mutate_at(question_name, as.character) %>% 
    head(X)   # keep top X percentages only 
  
  if(nrow(df) == 0) warning("None of the choices was selected!")
  
  return(df)
}

test <- select_one_topX(shelter_df, question_name = "shelter_types", X = 6)




########## option 2

ase_survey_design <- svydesign(ids = ~strata,      # cluster ids
                               weights = ~weights, # weight variable created above
                               strata = ~strata,             # sampling was stratified by district
                               data = shelter_df                     # have to specify the dataset
)

tab_DH_cond <- ase_survey_design %>%
  as_tibble() %>%
  group_by(shelter_types) %>% 
  mutate(
    n_healthGen = sum(n),
    Prop_Depressed = n/sum(n)
  ) %>% 
  ungroup()




