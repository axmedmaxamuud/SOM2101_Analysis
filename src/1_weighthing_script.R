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

clean_data <- read_excel("input/data/REACH SOM_MSNA_CleanData_v9.xlsx", sheet = 3, guess_max=50000)
sf <- read.csv("input/sample_frame/sf_final.csv", stringsAsFactors = FALSE)
sf_pop <- read.csv("input/sample_frame/sampling_frame.csv", stringsAsFactors = FALSE)

# add strata.column on the clean data
clean_df <- clean_data
clean_df$strata <- sf$strata.names[match(clean_df$settlements, sf$P_CODE)]

# check not-matched surveys
clean_df <- clean_df %>% 
  mutate(matched_sf = ifelse(strata %in% sampling.frame$strata.names, "clear", "check"))

# deletion log
qansaxdheere_log <- clean_df %>% filter(matched_sf == "check")
kurtunwaarey_log <- clean_df %>% filter(district == "kurtunwaarey")

deletion_log <- rbind(qansaxdheere_log, kurtunwaarey_log)

#data <- clean_df %>% filter(consent == "yes" & strata != "Qansax Dheere_hc" & matched_sf == "clear")
data <- clean_df %>% filter(district != "kurtunwaarey" & matched_sf == "clear")
names(data)[names(data) == "uuid"] <- "_uuid"

questions <- import("input/tool/SOM_REACH_MSNA_2022_Tool_v22_AM.xlsx", sheet = "survey",guess_max=50000) %>%
  select(-1) %>%
  filter(!is.na(name))

questions$`label::English` <- gsub("<[^>]*>","",questions$`label::English`,perl = T)

## Deleting group names from the columns 

group_names <- questions %>% filter(type=="begin_group") %>% pull(name)

group_names_rgx <- sprintf("(%s)",
                           gsub(" ","|",paste(paste0("^",group_names,"\\."), collapse=" ")))

colnames(data) <- gsub(group_names_rgx,"",colnames(data))

## District names and strata

# xml_names = c("abdulaziz", "aden_yabaal", "afgooye", "afmadow", "baardheere", "badhaadhe", "badhan", "baidoa", "baki", "balcad", "bandarbayla", "baraawe", "belet_weyne", "belet_xaawo", "berbera", "boondheere", "borama", "bossaso", "buaale", "bulo_burto", "burco", "burtinle", "buuhoodle", "buur_hakaba", "cabudwaaq", "cadaado", "cadale", "caluula", "caynabo", "ceel_afweyn", "ceel_barde", "ceel_buur", "ceel_dheer", "ceel_waaq", "ceerigaabo", "daynile", "dharkenley", "dhuusamarreeb", "diinsoor", "doolow", "eyl", "gaalkacyo_north", "gaalkacyo_south", "galdogob", "garbahaarey", "garowe", "gebiley", "hamar_jaab_jab", "hamar_weyne", "hargeysa", "hawl_wadaag", "heliwa", "hobyo", "hodan", "iskushuban", "jalalaqsi", "jamaame", "jariiban", "jowhar", "kahda", "karaan", "kismayo", "kurtunwaarey", "laas_caanood", "laasqoray", "lughaye", "luuq", "marka", "mataban", "owdweyne", "qandala", "qansax_dheere", "qardho", "qoryooley", "rab_dhuure", "sablaale", "shangaani", "sheikh", "shibis", "taleex", "tayeeglow", "waaberi", "waajid", "wadajir", "wanla_weyn", "wardhiigleey", "xudun", "xudur", "yaaqshiid", "zeylac")
# pop_names = c("Banadir", "Adan_Yabaal", "Afgooye", "Afmadow", "Baardheere", "Badhaadhe", "Laasqoray", "Baidoa", "Baki", "Balcad", "Bandarbayla", "Baraawe", "Belet_Weyne", "Belet_Xaawo", "Berbera", "Banadir", "Borama", "Bossaso", "Bu'aale", "Bulo_Burto", "Burco", "Burtinle", "Buuhoodle", "Buur_Hakaba", "Cabudwaaq", "Cadaado", "Cadale", "Caluula", "Caynabo", "Ceel_Afweyn", "Ceel_Barde", "Ceel_Buur", "Ceel_Dheer", "Ceel_Waaq", "Ceerigaabo", "Banadir", "Banadir", "Dhuusamarreeb", "Diinsoor", "Doolow", "Eyl", "Gaalkacyo", "Gaalkacyo", "Galdogob", "Garbahaarey", "Garowe", "Gebiley", "Banadir", "Banadir", "Hargeysa", "Banadir", "Banadir", "Hobyo", "Banadir", "Iskushuban", "Jalalaqsi", "Jamaame", "Jariiban", "Jowhar", "Banadir", "Banadir", "Kismayo", "Kurtunwaarey", "Laas_Caanood", "Laasqoray", "Lughaye", "Luuq", "Marka", "Belet_Weyne", "Owdweyne", "Qandala", "Qansax_Dheere", "Qardho", "Qoryooley", "Rab_Dhuure", "Sablaale", "Banadir", "Sheikh", "Banadir", "Taleex", "Tayeeglow", "Banadir", "Waajid", "Banadir", "Wanla_Weyn", "Banadir", "Xudun", "Xudur", "Banadir", "Zeylac")
# 
# 
# district_names_matching = data.frame(district=xml_names,
#                                      pop_names=pop_names,
#                                      stringsAsFactors = F)
# 
# data$district = left_join(data,district_names_matching) %>% pull(pop_names)
# 
# data <- data %>% mutate(
#   population_group = case_when(
#     idp_settlement == "yes" ~ "IDP",
#     idp_settlement == "no" ~ "HC"
#   ),
#   
#   strata = paste0(district,"_",population_group)
# )

### Convert numerical questions
num_q = questions %>% filter(type %in% c("calculate","integer")) %>% pull(name)
num_q = num_q[8:length(num_q)] 
num_q = num_q[!num_q %in% c("member_position","age_wgq","number_years","number_months","child_not_liv_position")]

data <- mutate_at(data,num_q,as.numeric)

### Load questionnaire

choices <- import("input/tool/SOM_REACH_MSNA_2022_Tool_v22_AM.xlsx", sheet = "choices",guess_max=50000)

questionnaire <- load_questionnaire(
  data = data,
  questions = questions,
  choices = choices,
  choices.label.column.to.use = "label::English"
)


sampling.frame <- load_samplingframe("input/sample_frame/sampling_frame.csv")

weighting_function <-map_to_weighting(sampling.frame = sampling.frame,
                                      data.stratum.column = "strata",
                                      sampling.frame.population.column = "population",
                                      sampling.frame.stratum.column = "strata.names",
                                      data = data)

data[["weights"]] <-  weighting_function(data)

data$uuid =data$`_uuid`

# Exporting final weighted data
write.xlsx(data, paste0("input/data/clean_data_w_",today,".xlsx"))
write.xlsx(deletion_log, paste0("input/data/deleted_surveys_",today,".xlsx"))
# create another file with uuid and weight to for loops
df <- data %>% 
  select(uuid, weights)
write.csv(df, file = "input/sample_frame/msna_weights.csv", row.names = FALSE)

