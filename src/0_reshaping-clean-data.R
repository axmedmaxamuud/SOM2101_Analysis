## Data preparation script
rm(list = ls())
today <- Sys.Date()

# laod packages
library(tidyverse)
library(readxl)
library(openxlsx)

# load clean data
clean_df <- read_excel("input/data/REACH_SOM_MSNA_2022_Dataset.xlsx", sheet = "Clean_Data") %>% # removing drop-out columns
  select(-c(dropped_out_girls_3_5, dropped_out_boys_3_5, dropped_out_girls_6_11, dropped_out_boys_6_11, dropped_out_girls_12_17, dropped_out_boys_12_17))

# preparing demographics as per the DAP
hh_roster <- read_excel("input/data/REACH_SOM_MSNA_2022_Dataset.xlsx", sheet = "hh_roster") %>% 
  mutate(female_3_5 = ifelse(ind_gender == "female" & ind_age %in% c(3, 4, 5), 1, 0),
         male_3_5 = ifelse(ind_gender == "male" & ind_age %in% c(3, 4, 5), 1, 0),
         female_6_11 = ifelse(ind_gender == "female" & ind_age %in% c(6, 7, 8, 9, 10, 11), 1, 0),
         male_6_11 = ifelse(ind_gender == "male" & ind_age %in% c(6, 7, 8, 9, 10, 11), 1, 0),
         female_12_17 = ifelse(ind_gender == "female" & ind_age %in% c(12, 13, 14, 15, 16, 17), 1, 0),
         male_12_17 = ifelse(ind_gender == "male" & ind_age %in% c(12, 13, 14, 15, 16, 17), 1, 0)) %>% 
  group_by(uuid) %>% 
  summarise_at(c("female_3_5",
                 "male_3_5",
                 "female_6_11",
                 "male_6_11",
                 "female_12_17",
                 "male_12_17"), sum, na.rm = TRUE)

# calculating dropout indicator based on the enrollment and attendance
# change education questions from character to numeric
data <- clean_df %>% mutate_at(c("enrolled_girls_3_5", "girls_3_5_attend",
                                 "enrolled_boys_3_5", "boys_3_5_attend",
                                 "enrolled_girls_6_11", "girls_6_11_attend",
                                 "enrolled_boys_6_11", "boys_6_11_attend",
                                 "enrolled_girls_12_17", "girls_12_17_attend",
                                 "enrolled_boys_12_17", "boys_12_17_attend"), as.numeric) %>% 
  left_join(hh_roster, by = "uuid")


# calculating dropout from enrollment and attendance
df <- data %>% 
  mutate(dropped_out_girls_3_5 = ifelse(enrolled_girls_3_5 == 0, 0, enrolled_girls_3_5 - girls_3_5_attend),
         dropped_out_boys_3_5 = ifelse(enrolled_boys_3_5 == 0, 0, enrolled_boys_3_5 - boys_3_5_attend),
         dropped_out_girls_6_11 = ifelse(enrolled_girls_6_11 == 0, 0, enrolled_girls_6_11 - girls_6_11_attend),
         dropped_out_boys_6_11 = ifelse(enrolled_boys_6_11 == 0, 0, enrolled_boys_6_11 - boys_6_11_attend),
         dropped_out_girls_12_17 = ifelse(enrolled_girls_12_17 == 0, 0, enrolled_girls_12_17 - girls_12_17_attend),
         dropped_out_boys_12_17 = ifelse(enrolled_boys_12_17 == 0, 0, enrolled_boys_12_17 - boys_12_17_attend))

### Skip-Logic

# What do you think are the main safety and security concerns for girls in this area?
df$boys_security_concerns[df$hh_number_girls_count < 0] <- "NA"
# What do you think are the main safety and security concerns for boys in this area?
df$girls_security_concerns[df$hh_number_boys_count < 0] <- "NA"
# What do you think are the main safety and security concerns for women in this area?
df$women_security_concerns[df$hh_number_women_count < 0] <- "NA"
# What do you think are the main safety and security concerns for men in this area?
df$men_security_concerns[df$hh_number_men_count < 0] <- "NA"

# % of households who have at least 1 child (boy or girl) married 
df$child_marriage_status[df$hh_number_boys_count < 0 | df$hh_number_girls_count < 0] <- "NA"
# Which of the following services are available for children in your community?
df$available_children_service[df$hh_number_boys_count < 0 | df$hh_number_girls_count < 0] <- "NA"
# If yes, how many children (<18) in your household showed those signs?
df$children_psychosocial_signs[df$hh_number_boys_count < 0 | df$hh_number_girls_count < 0] <- "NA"

# Which of the following services are available for girls and women in your community?
df$gbv_avialable_services[df$hh_number_women_count < 0 | df$hh_number_girls_count < 0] <- "NA"
# Can women and girls move freely inside your community to attend distributions, gather firewood, go to women/girl-friendly spaces, go to markets etc?
df$women_move_freely[df$hh_number_women_count < 0 | df$hh_number_girls_count < 0] <- "NA"
# Were there any children enrolled in schools, that were withrawn due to consequences of drought: such as inability to pay school fees, domestic or paid work, search of food etc.
df$any_dropout_children[df$hh_number_children_count < 0] <- "NA"
# If available, what type of support would help your child with attending school or participating in regular learning activities? [Do not read options to respondent]
df$educ_support[df$hh_number_children_count < 0] <- "NA"
# How long does it usually take the children to get to school?
df$school_time[df$hh_number_children_count < 0] <- "NA"
# How do the children usually get to school?
df$school_transportion_mode[df$hh_number_children_count < 0] <- "NA"

## Export data
write.xlsx(df, paste0("input/clean_data/final_clean_data_",today,".xlsx"))


