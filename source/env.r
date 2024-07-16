# To be run with:
# R version 4.3.0 (2023-04-21)
# Rearrangement_2.1 quantreg_5.95     SparseM_1.81      cmna_1.0.5
# gbm_2.1.8.1       latex2exp_0.9.6   foreach_1.5.2     lubridate_1.9.2
# forcats_1.0.0     stringr_1.5.0     dplyr_1.1.2       purrr_1.0.1
# readr_2.1.4       tidyr_1.3.0       tibble_3.2.1      ggplot2_3.4.2
# tidyverse_2.0.0


library(tidyverse)
library(foreach)
library(latex2exp)
library(gbm)
library(cmna)
library(Rearrangement)

job <- read.csv("data/behaghel.csv")

Xbin <- c(
  "College_education",
  "nivetude2",
  "Vocational",
  "High_school_dropout",
  "Manager",
  "Technician",
  "Skilled_clerical_worker",
  "Unskilled_clerical_worker",
  "Skilled_blue_colar",
  "Unskilled_blue_colar",
  "Woman",
  "Married",
  "French",
  "African",
  "Other_Nationality",
  "Paris_region",
  "North",
  "Other_regions",
  "Employment_component_level_1",
  "Employment_component_level_2",
  "Employment_component_missing",
  "Economic_Layoff",
  "Personnal_Layoff",
  "End_of_Fixed_Term_Contract",
  "End_of_Temporary_Work",
  "Other_reasons_of_unemployment",
  "Statistical_risk_level_2",
  "Statistical_risk_level_3",
  "Other_Statistical_risk",
  "Search_for_a_full_time_position",
  "Sensitive_suburban_area",
  "Insertion",
  "Interim",
  "Conseil"
)

Xnum <- c(
  "age",
  "Number_of_children",
  "exper", # years experience on the job
  "salaire.num", # salary target
  "mois_saisie_occ", # when assigned
  "ndem" # Num. unemployment spell
)

Xall <- c(Xbin, Xnum)


ps <- seq(0.01, 1, 0.01)

zz <- 1.64485


rhos <- c(-1, -0.5, 0, 0.5, 0.9, 0.95, 1)