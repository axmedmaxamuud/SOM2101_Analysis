## Mortality Analysis
rm(list = ls())
cat("\014")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, 
  healthyr
)

# Directory Paths Update
todays_file_path <- paste0("output/MSNA_2022/", Sys.Date())
dir.create(todays_file_path)
file_path <- "input/HQ_validated_data/REACH_SOM_MSNA_2022_Data.xlsx"

# Load Data

df_hh <- readxl::read_xlsx(file_path, sheet = "Clean_Data") %>% 
  dplyr::mutate(q0_1_enqueteur_nom = NULL)
df_roster <- readxl::read_xlsx(file_path, sheet = "hh_roster") %>% dplyr::mutate(nom = NULL)
df_wgss <- readxl::read_xlsx(file_path, sheet = "wgq_repeat")
df_health <- readxl::read_xlsx(file_path, sheet = "health_loop")
df_woman <- readxl::read_xlsx(file_path, sheet = "wcb_repeat")
df_left <- readxl::read_xlsx(file_path, sheet = "leavers_repeat") %>% dplyr::mutate(name_left = NULL)
df_died <- readxl::read_xlsx(file_path, sheet = "hh_died_repeat") %>% dplyr::mutate(name_died = NULL)

# df_ind <- merge(df_roster, df_wgss %>% rename(person_id = wgss_person_id) %>% mutate(`_index` = NULL, `_parent_index` = NULL), all.x = TRUE)
# df_ind <- merge(df_ind, df_child_prot %>% rename(person_id = olderchild_person_id) %>% mutate(`_index` = NULL, `_parent_index` = NULL), all.x = TRUE)
# df_ind <- merge(df_ind, df_health %>% rename(person_id = health_person_id) %>% mutate(`_index` = NULL, `_parent_index` = NULL), all.x = TRUE)
# df_ind <- merge(df_ind, df_woman %>% rename(person_id = woman_person_id) %>% mutate(`_index` = NULL, `_parent_index` = NULL), all.x = TRUE)
# df_ind <- merge(df_ind, df_school_age %>% rename(person_id = ecolechild_person_id) %>% mutate(`_index` = NULL, `_parent_index` = NULL), all.x = TRUE)
# df_ind <- merge(df_ind, df_vaccine %>% rename(person_id = child_vaccine_person_id) %>% mutate(`_index` = NULL, `_parent_index` = NULL), all.x = TRUE)

merge_info <- df_hh %>% dplyr::select(deviceid, region, district, idp_settlement, settlements, today, consent, uuid) %>%
  dplyr::rename(uuid = `_uuid`) %>%
  dplyr::mutate(today= stringr::str_sub(today, start = 1, end = 10))
#names(merge_info)[names(merge_info) == "uuid"] <- "_uuid"

names(df_roster)[names(df_roster) == "uuid"] <- "_submission__uuid"
df_roster2 <- df_roster %>%
  #dplyr::rename(uuid = `_submission__uuid`) %>%
  dplyr::left_join(merge_info, by = "uuid") %>%
  dplyr::mutate(days_since_born = ifelse(is.na(months), NA, round(months*(365.25/12))),
                ind_dob = ifelse(is.na(know_birth_date), NA, ifelse(know_birth_date == "no" & !is.na(months), 
                                                                    as.Date(today) - days_since_born, as.Date(ind_dob) )),
                ind_dob = lubridate::as_date(ind_dob),
                born = ifelse(is.na(ind_dob), NA, ifelse(ind_dob > format(lubridate::parse_date_time("2022-05-02", orders = "ymd"), "%Y-%m-%d"), "y", "n" )),
                curr_joined = case_when(
                  curr_joined == "yes" ~ "n",
                  curr_joined == "no" ~ "y"
                ))

df_left2 <- df_left %>%
  dplyr::rename(uuid = `_submission__uuid`) %>%
  dplyr::left_join(merge_info, by = "uuid") %>%
  dplyr::mutate(birthdate = lubridate::as_date(leavers_date),
                birthdate = case_when(
                  !is.na(know_leavers_date) ~ birthdate,
                ),
                born = case_when(
                  birthdate > format(lubridate::parse_date_time("2022-05-02", orders = "ymd"), "%Y-%m-%d") ~ "y",
                  birthdate < format(lubridate::parse_date_time("2022-05-02", orders = "ymd"), "%Y-%m-%d") ~ "n"
                ))



df_died2 <- df_died %>%
  dplyr::rename(uuid = `_submission__uuid`) %>%
  dplyr::left_join(merge_info, by = "uuid") %>%
  dplyr::mutate(date_deaths = lubridate::as_date(date_death),
                date_death = NULL,
                month_year_date_death = lubridate::as_date(month_year_date_death),
                dob_died = lubridate::as_date(dob_died),
                month_year_dob = lubridate::as_date(month_year_dob),
                date_died = case_when(
                  !is.na(date_deaths) ~ date_deaths,
                  !is.na(month_year_date_death) ~ month_year_date_death,
                  TRUE ~ NA_real_
                ),
                date_dob = case_when(
                  !is.na(dob_died) ~ dob_died,
                  !is.na(month_year_dob) ~ month_year_dob,
                  TRUE ~ NA_real_
                ),
                date_dob = lubridate::as_date(date_dob),
                died_born = ifelse(is.na(date_dob), NA, ifelse(date_dob > format(lubridate::parse_date_time("2022-05-02", orders = "ymd"), "%Y-%m-%d"), "y", "n" )),
                
  )

df_mortality <- format_mortality_current_census(date_recall_event = "2022-05-02", # May 2nd 2022 (Eid al Fitr)
                                                #Current roster data and columns
                                                df_roster = df_roster2, # your current roster dataset
                                                date_dc_roster = "today", # date of data collection
                                                enum_roster = "deviceid", # enumerator or team id
                                                cluster_roster = "settlements", # cluster id
                                                admin2_roster = "district", # optional but recommended admin level
                                                hh_id_roster = "uuid", # unique household id, like the UUID. Must match between datasets
                                                sex_roster = "ind_gender",
                                                age_roster = "ind_age",
                                                joined_roster = "curr_joined",
                                                birth_roster = "born",
                                                # joined_date_roster = "curr_joined_date",
                                                # birthdate_roster = "ind_dob",
                                                #Left people data and columns
                                                df_left = df_left2, # Left household member dataset
                                                date_dc_left = "today", # date of data collection
                                                enum_left = "deviceid", # enumerator or team number
                                                cluster_left = "settlements", # cluster id
                                                admin2_left = "district", #optional but recommended admin level
                                                hh_id_left = "uuid", # unique household id, like the UUID. Must match between datasets
                                                sex_left = "sex_left", # sex of left household member
                                                age_left = "age_left", # age in years of left household member
                                                birth_left = "born", # birth of left household member
                                                joined_left = "leavers_present",  # joined the household before leaving the household
                                                # joined_date_left = "left_join_date",
                                                # left_date_left = "left_leave_date", 
                                                # birthdate_left = "birthdate", 
                                                #Died people data and columns
                                                df_died = df_died2, # your deaths dataset
                                                date_dc_died = "today", # date of data collection
                                                enum_died = "deviceid",# enumerator or team number
                                                cluster_died = "settlements", # cluster id
                                                admin2_died = "district", #optional but recommended admin level
                                                hh_id_died = "uuid", # unique household id, like the UUID. Must match between datasets
                                                sex_died = "gender_died", # sex of deceased
                                                age_died = "age_dead", # age in years of the deceased at time of death
                                                birth_died = "died_born", # born during recall period   
                                                joined_died = "died_present", # joined the HH during recall period
                                                death_cause = "how_died", # cause of death
                                                death_location = "where_pass_away",  # location of death
                                                # date_death = "date_died",
                                                # joined_date_died = "died_join_date",
                                                # birthdate_died = "date_dob"
) 

# Create Mortality Plausibility Report

(create_mortality_quality_report(df_mortality, exp_hh_size = 6, exp_ratio_0_4 = 0.25))

# Create Age Pyramid 

(plot_agepyramid(df_mortality))

# Export to ENA Formatted File 

healthyr::format_mortality_to_ena(df_mortality) %>% writexl::write_xlsx("ena_mortality.xlsx")
