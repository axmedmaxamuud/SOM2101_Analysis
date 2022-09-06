## Data Preparation Script
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

# load clean data
#data <- openxlsx::read.xlsx("input/clean_data/draft_clean_data_2022-09-06.xlsx")
data <- read.csv("input/clean_data/clean_data.csv", stringsAsFactors = FALSE)


questions <- import("input/tool/SOM_REACH_MSNA_2022_Tool_v22_AM.xlsx", sheet = "survey") %>% 
  select(-1) %>% 
  filter(!is.na(name))

# Making sure binaries of SM questions are stored as numerical values (0/1) instead of ("0"/"1")
binary_q <- questions %>% filter(grepl("^select_multiple",type)) %>% pull(name)

binary_q_regex <- paste0("(",paste(paste0("^",questions %>% filter(grepl("select_multiple",type)) %>% pull(name),"\\."),collapse = '|'),")")
data <- mutate_at(data,
                  names(data)[str_detect(pattern = binary_q_regex,string = names(data))]
                  ,as.numeric)

# Remove binary fields added to select one questions if any
regex_expr <- paste0("(",paste(paste0("^",questions %>% filter(grepl("select_one",type)) %>% pull(name),"\\."),collapse = '|'),")")

names(data)[str_detect(pattern = regex_expr,string = names(data))]

data <- data %>% select(-any_of(c(names(data)[str_detect(pattern = regex_expr,string = names(data))])))

# Making sure numerical variables are formatted correctly
num_q <- questions %>% filter(type %in% c("calculate","integer")) %>% pull(name)

num_q <- num_q[num_q %in% names(data)]

data <- mutate_at(data,num_q,as.numeric)

# Remove non needed questions from data/analysis
questions_to_remove <- c("start",
                         "end",
                         "today",
                         "deviceid",
                         "instance_name",
                         "enum_name",
                         "enum_phone",
                         "consent",
                         "start_date",
                         "end_date",
                         "date_match",
                         "startformatted",
                         "endformatted",
                         "start_hour",
                         "end_hour",
                         "start_minutes",
                         "end_minutes",
                         "duration_hours",
                         "duration_minutes",
                         "durationtext",
                         "durationinmin",
                         "tooshort",
                         "X_id",
                         "X_submission_time",
                         "X_validation_status",
                         "X_notes",
                         "X_status",
                         "X_submitted_by",
                         "X_tags",
                         "X_index",
                         "interview_duration",
                         "CHECK_interview_duration",
                         "uuid"
)


data <- data %>% select(-any_of(questions_to_remove))

#write.csv(data, file = "input/clean_data/clean_data.csv", row.names = FALSE)




