### MSNA 2022 - Data Preparation
rm(list = ls())
today <- Sys.Date()

# load packages
if (!require("pacman")) install.packages("pacman")
p_load(
  rio,
  tidyverse,
  crayon,
  hypegrammaR,
  sjmisc,
  koboquest,
  reshape2,
  openxlsx
)

source("src/functions/functions.R")

clean_data <- openxlsx::read.xlsx("input/clean_data/draft_clean_data_2022-09-06.xlsx")
data <- clean_data

questions <- import("input/tool/SOM_REACH_MSNA_2022_Tool_v22_AM.xlsx", sheet = "survey") %>% 
  select(-1) %>% 
  filter(!is.na(name))

questions$`label::English` <- gsub("<[^>]*>","",questions$`label::English`,perl = T)

## Deleting group names from the columns
group_names <- questions %>% filter(type=="begin_group") %>% pull(name)
group_names_rgx <- sprintf("(%s)",
                           gsub(" ","|",paste(paste0("^",group_names,"\\."),collapse = " ")))
colnames(data) <- gsub(group_names_rgx,"",colnames(data))

## 


