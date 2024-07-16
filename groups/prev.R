
library(tidyverse)
library(foreach)
library(latex2exp)
library(gbm)
library(cmna)
library(Rearrangement)

job = read.csv('https://raw.githubusercontent.com/CausalML/TreatmentEffectRisk/main/data/behaghel.csv')

Xbin = c(
  'College_education',
  'nivetude2',
  'Vocational',
  'High_school_dropout',
  'Manager',
  'Technician',
  'Skilled_clerical_worker',
  'Unskilled_clerical_worker',
  'Skilled_blue_colar',
  'Unskilled_blue_colar',
  'Woman',
  'Married',
  'French',
  'African',
  'Other_Nationality',
  'Paris_region',
  'North',
  'Other_regions',
  'Employment_component_level_1',
  'Employment_component_level_2',
  'Employment_component_missing',
  'Economic_Layoff',
  'Personnal_Layoff',
  'End_of_Fixed_Term_Contract',
  'End_of_Temporary_Work',
  'Other_reasons_of_unemployment',
  'Statistical_risk_level_2',
  'Statistical_risk_level_3',
  'Other_Statistical_risk',
  'Search_for_a_full_time_position',
  'Sensitive_suburban_area',
  'Insertion',
  'Interim',
  'Conseil'
)

Xnum = c(
  'age',  
  'Number_of_children', 
  'exper', # years experience on the job
  'salaire.num', # salary target
  'mois_saisie_occ', # when assigned
  'ndem' # Num. unemployment spell
)

Xall = c(Xbin,Xnum)

# Focus on private vs public

job_binary = job %>% filter(A_public == 1 | A_private == 1) %>% mutate(sw = sw/mean(sw)) %>% mutate(A = A_public, ipw = 1 / (A_standard*mean(sw*A_standard) + A_private*mean(sw*A_private) + A_public*mean(sw*A_public)))

# Cross fitting

make.cvgroup = function(n, K, right = TRUE) {
  split     = runif(n)
  return(as.numeric(cut(split, quantile(split, probs = seq(0, 1, 1/K)), include.lowest = TRUE, right = right)))
}

make.cvgroup.balanced = function(data, K, form_t) {
  cvgroup = numeric(nrow(data))
  cvgroup[data[[form_t]]==1] = make.cvgroup(sum(data[[form_t]]==1), K, right = TRUE)
  cvgroup[data[[form_t]]==0] = make.cvgroup(sum(data[[form_t]]==0), K, right = FALSE)
  return(cvgroup)
}

set.seed(0)

K = 5
cvgroup = job_binary %>% make.cvgroup.balanced(., K, 'A')


# 
# Result CVAr

## If Functions

# ```{r}
# functions if
source(
  here("source", "calculate_if.r")
)

# plot
source(
  here("source", "plots.r")
)

# CVAr
source(
  here("source", "calculate_cvar.r")
)


source(
  here("source", "utils.r")
)
# variables

source(
  here("source", "env.r")
)

