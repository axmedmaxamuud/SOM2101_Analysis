## Data preparation

if (!require("pacman")) install.packages("pacman")

p_load(
  rio,
  tidyverse,
  crayon,
  hypegrammaR,
  sjmisc,
  koboquest,
  reshape2
)

source("src/functions/functions.R")

# load clean data (this case version 6) will replace v_7 once receive the final
file_path <- "input/data/REACH SOM_MSNA_CleanData_v9.xlsx"
sf <- read.csv("input/sample_frame/msna_weights.csv", stringsAsFactors = FALSE)

# load data
df_hh <- readxl::read_xlsx(file_path, sheet = "Clean_Data") %>% 
  dplyr::mutate(q0_1_enqueteur_nom = NULL)

df_raw <- readxl::read_xlsx(file_path, sheet = "Raw_Data")
df_deletions <- readxl::read_xlsx(file_path, sheet = "Deletions")
df_clog <- readxl::read_xlsx(file_path, sheet = "Cleaning_Log")


# loop data (remove all the entries for qansaxdheere/kunturwary districts)
df_roster <- readxl::read_xlsx(file_path, sheet = "hh_roster") %>% dplyr::mutate(nom = NULL) %>% 
  mutate(delete = ifelse(uuid %in% df_hh$uuid, "no", "yes")) %>% 
  filter(delete == "no")
df_roster$strata <- sf$strata[match(df_roster$uuid, sf$uuid)]
df_roster$weights <- sf$weights[match(df_roster$uuid, sf$uuid)]

df_health <- readxl::read_xlsx(file_path, sheet = "health_loop") %>% 
  mutate(delete = ifelse(uuid %in% df_hh$uuid, "no", "yes")) %>% 
  filter(delete == "no")
df_health$strata <- sf$strata[match(df_health$uuid, sf$uuid)]
df_health$weights <- sf$weights[match(df_health$uuid, sf$uuid)]

df_died <- readxl::read_xlsx(file_path, sheet = "died_repeat") %>% dplyr::mutate(name_died = NULL) %>% 
  mutate(delete = ifelse(uuid %in% df_hh$uuid, "no", "yes")) %>% 
  filter(delete == "no")
df_died$strata <- sf$strata[match(df_died$uuid, sf$uuid)]
df_died$weights <- sf$weights[match(df_died$uuid, sf$uuid)]

df_left <- readxl::read_xlsx(file_path, sheet = "leavers_repeat") %>% dplyr::mutate(name_left = NULL) %>% 
  mutate(delete = ifelse(uuid %in% df_hh$uuid, "no", "yes")) %>% 
  filter(delete == "no")
df_left$strata <- sf$strata[match(df_left$uuid, sf$uuid)]
df_left$weights <- sf$weights[match(df_left$uuid, sf$uuid)]

df_nutr <- readxl::read_xlsx(file_path, sheet = "nutrition_repeat") %>% dplyr::mutate(name_left = NULL) %>% 
  mutate(delete = ifelse(uuid %in% df_hh$uuid, "no", "yes")) %>% 
  filter(delete == "no")
df_nutr$strata <- sf$strata[match(df_nutr$uuid, sf$uuid)]
df_nutr$weights <- sf$weights[match(df_nutr$uuid, sf$uuid)]

df_shelter <- readxl::read_xlsx(file_path, sheet = "shelter_repeat") %>% dplyr::mutate(name_left = NULL) %>% 
  mutate(delete = ifelse(uuid %in% df_hh$uuid, "no", "yes")) %>% 
  filter(delete == "no")
df_shelter$strata <- sf$strata[match(df_shelter$uuid, sf$uuid)]
df_shelter$weights <- sf$weights[match(df_shelter$uuid, sf$uuid)]

df_wcb <- readxl::read_xlsx(file_path, sheet = "wcb_repeat") %>% dplyr::mutate(name_left = NULL) %>% 
  mutate(delete = ifelse(uuid %in% df_hh$uuid, "no", "yes")) %>% 
  filter(delete == "no")
df_wcb$strata <- sf$strata[match(df_wcb$uuid, sf$uuid)]
df_wcb$weights <- sf$weights[match(df_wcb$uuid, sf$uuid)]

df_wgss <- readxl::read_xlsx(file_path, sheet = "wgq_repeat") %>% 
  mutate(delete = ifelse(uuid %in% df_hh$uuid, "no", "yes")) %>% 
  filter(delete == "no")
df_wgss$strata <- sf$strata[match(df_wgss$uuid, sf$uuid)]
df_wgss$weights <- sf$weights[match(df_wgss$uuid, sf$uuid)]

# Export final
msna_loops <- list("Raw_Data" = df_raw,
                   "Clean_Data" = df_hh,
                   "Cleaning_Log" = df_clog,
                   "Deletions" = df_deletions,
                   "hh_roster"= df_roster,
                   "health_loop" = df_health,
                   "died_repeat" = df_died,
                   "leavers_repeat" = df_left,
                   "nutrition_repeat" = df_nutr,
                   "shelter_repeat" = df_shelter,
                   "wcb_repeat" = df_wcb,
                   "wgq_repeat" = df_wgss)

# Export 
write.xlsx(msna_loops, paste0("input/data/REACH_SOM_MSNA_CleanData_HNO_",today,".xlsx"))





