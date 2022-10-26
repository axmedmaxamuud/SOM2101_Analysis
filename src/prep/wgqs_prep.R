## Shelter loop Analysis
rm(list = ls())
today <- Sys.Date()

# load packages
library(tidyverse)
library(healthyr)

# load data
wgqs_df <- read_excel("input/data/REACH_SOM_MSNA_CleanData_Dissemination.xlsx", sheet = "wgq_repeat")

# format dataset

df2 <- format_nut_health_indicators(df = wgqs_df,
                                    hhid = "uuid",
                                    cluster = "strata",
                                    
                                    sex_var = "wgss_ind_gender",
                                    age_years_var = "wgss_ind_age",
                                    
                                    wgss_seeing = "wgq_seeing",
                                    wgss_hearing = "wgq_hearing",
                                    wgss_walking = "wgq_walking",
                                    wgss_remembering = "wgq_remembering",
                                    wgss_selfcare = "wgq_selfcare",
                                    wgss_communicating = "wgq_language"
)


(plot_agepyramid(df2))

(plot_agepyramid(df2, filtering = "disability1"))

(plot_agepyramid(df2, filtering = "disability2"))

(plot_agepyramid(df2, filtering = "disability3"))

(plot_agepyramid(df2, filtering = "disability4"))

(plot_age_years_distribution(df2, min_age = 15, max_age = 80, breaks = 1))

(plot_age_years_distribution(df2, min_age = 0, max_age = 18, breaks = 1))


(g <- plot_domain_radar(df = df2,
                        domain_cols = c("wg_sum_seeing_34","wg_sum_hearing_34","wg_sum_communication_34","wg_sum_walking_34","wg_sum_selfcare_34","wg_sum_remembering_34"),
                        domain_labels = c("Seeing", "Hearing", "Communication", "Walking", "Self-care", "Remembering")))

(g <- plot_domain_radar(df = df2,
                        domain_cols = c("wg_sum_seeing_34","wg_sum_hearing_34","wg_sum_communication_34","wg_sum_walking_34","wg_sum_selfcare_34","wg_sum_remembering_34"),
                        domain_labels = c("Seeing", "Hearing", "Communication", "Walking", "Self-care", "Remembering"),
                        grouping = "governorate"))

(g <- plot_domain_radar(df = df2,
                        domain_cols = c("wg_sum_seeing_34","wg_sum_hearing_34","wg_sum_communication_34","wg_sum_walking_34","wg_sum_selfcare_34","wg_sum_remembering_34"),
                        domain_labels = c("Seeing", "Hearing", "Communication", "Walking", "Self-care", "Remembering"),
                        grouping = "age_group"))

(g <- plot_domain_radar(df = df2,
                        domain_cols = c("wg_sum_seeing_34","wg_sum_hearing_34","wg_sum_communication_34","wg_sum_walking_34","wg_sum_selfcare_34","wg_sum_remembering_34"),
                        domain_labels = c("Seeing", "Hearing", "Communication", "Walking", "Self-care", "Remembering"),
                        grouping = "sex"))


