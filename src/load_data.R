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
  reshape2
)