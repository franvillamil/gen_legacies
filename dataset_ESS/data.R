# setwd("~/Documents/Projects/gen_legacies")
options(stringsAsFactors = FALSE)
options("modelsummary_format_numeric_latex" = "plain")
# List of packages
pkg = c("dplyr", "stringr", "countrycode")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
invisible(lapply(pkg, library, character.only = TRUE))
# Functions
source("func/misc.R")

## ==================================================================
## LOAD DATA AND BASICS

# Load data
dataraw = readRDS("input/ESS_combined.rds") %>%
  filter(cntry != "XK") %>% # Remove Kosovo (too few obs anyway)
  mutate(countryname = tolower(countrycode(cntry, "iso2c", "country.name"))) %>%
  mutate(countryname = capitalize(countryname)) %>%
  # Rounds
  rename(round = essround) %>%
  mutate(svyear = recode(round,
    "1" = 2002,
    "2" = 2004,
    "3" = 2006,
    "4" = 2008,
    "5" = 2010,
    "6" = 2012,
    "7" = 2014,
    "8" = 2016,
    "9" = 2018,
    "10" = 2020,
    "11" = 2023))

## ==================================================================
## CLEAN UP AND PREPARE

# Clean up data
data = dataraw %>%
  # Extra variables
  mutate(
    birthyear = ifelse(yrbrn > 2010, NA, yrbrn),
    age = ifelse(agea == 999, NA, agea),
    female = ifelse(gndr == 9, NA, gndr - 1),
    ideology = ifelse(lrscale > 10, NA, lrscale)) %>%
  # Education level variable 
  mutate(educ_level = ifelse(edulvla > 5, NA, edulvla)) %>%
  mutate(educ_level = ifelse(round > 4 & edulvlb > 1000, NA, educ_level)) %>%
  mutate(educ_level = ifelse(round > 4 &
    edulvlb %in% c(0, 113), 1, educ_level)) %>%
  mutate(educ_level = ifelse(round > 4 &
    edulvlb %in% c(129, 212, 213, 221, 222, 223), 2, educ_level)) %>%
  mutate(educ_level = ifelse(round > 4 &
    edulvlb %in% c(229, 311, 312, 313, 321, 322, 323), 3, educ_level)) %>%
  mutate(educ_level = ifelse(round > 4 &
    edulvlb %in% c(412, 413, 421, 422, 423), 4, educ_level)) %>%
  mutate(educ_level = ifelse(round > 4 &
    edulvlb %in% c(510, 520, 610, 620, 710, 720, 800), 5, educ_level)) %>%
  # # Efficacy -- TOO MANY NAs
  # mutate(polefficacy = ifelse(cptppol %in% 0:10, cptppol/10, NA)) %>%
  # mutate(polefficacy = ifelse(cptppola %in% 1:5, (cptppola-1)/4, polefficacy)) %>%
  # Political interest
  mutate(polinterest = ifelse(polintr %in% 1:4, 5 - polintr, NA)) %>%
  # Trust
  mutate(trust_polit = ifelse(trstplt %in% 0:10, trstplt, NA)) %>%
  mutate(trust_parties = ifelse(trstprt %in% 0:10, trstplt, NA)) %>%
  # Born in country
  mutate(born_ctry = ifelse(brncntr == 1, 1, 0)) %>%
  mutate(born_ctry = ifelse(brncntr %in% 7:9, NA, born_ctry)) %>%
  # Dependent variable: any association (ONLY ROUND 1)
  mutate(asoc_part_r1 = ifelse(round == 1, 0, NA)) %>%
  mutate(asoc_part_r1 = ifelse(cltommb == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(cltoptp == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(cnsommb == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(cnsoptp == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(epaommb == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(epaoptp == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(hmnommb == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(hmnoptp == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(othvmmb == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(othvptp == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(prfommb == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(prfoptp == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(rlgommb == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(rlgoptp == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(sclcmmb == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(sclcptp == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(setommb == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(setoptp == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(sptcmmb == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(sptcptp == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(trummb == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(truptp == 1, 1, asoc_part_r1)) %>%
  mutate(asoc_part_r1 = ifelse(
    round == 1 & (is.na(cltommb) & is.na(cltoptp) & is.na(cnsommb) &
      is.na(cnsoptp) & is.na(epaommb) & is.na(epaoptp) & is.na(hmnommb) &
      is.na(hmnoptp) & is.na(othvmmb) & is.na(othvptp) & is.na(prfommb) &
      is.na(prfoptp) & is.na(rlgommb) & is.na(rlgoptp) & is.na(sclcmmb) &
      is.na(sclcptp) & is.na(setommb) & is.na(setoptp) & is.na(sptcmmb) &
      is.na(sptcptp) & is.na(trummb) & is.na(truptp)), NA, asoc_part_r1)) %>%
  # Dependent variable: political associations (R1)
  mutate(asoc_part_pol_r1 = ifelse(round == 1, 0, NA)) %>%
  mutate(asoc_part_pol_r1 = ifelse(trummb == 1, 1, asoc_part_pol_r1)) %>%
  mutate(asoc_part_pol_r1 = ifelse(truptp == 1, 1, asoc_part_pol_r1)) %>%
  mutate(asoc_part_pol_r1 = ifelse(
    round == 1 & (is.na(trummb) & is.na(truptp)), NA, asoc_part_pol_r1)) %>%
  # Dependent variable: political associations (ALL ROUNDS)
  mutate(asoc_part_pol = ifelse(mbtru %in% 1:2, 1, 0)) %>%
  mutate(asoc_part_pol = ifelse(mbtru >= 7, NA, asoc_part_pol)) %>%
  # Other transformations and filters
  rowwise %>%
  mutate(
    cohort10 = birthyear - (birthyear %% 10),
    cohort20 = birthyear - ((birthyear+10) %% 20),
    age10 = age - (age %% 10),
    age_l = log(age)) %>%
  ungroup %>%
  mutate(
    cohort10 = ifelse(birthyear < 1920, NA, cohort10),
    cohort20 = ifelse(!birthyear %in% 1930:1989, NA, cohort20),
    age10 = ifelse(!age %in% 20:89, NA, age10)) %>%
  mutate(
    cohort10 = factor(cohort10),
    cohort20 = factor(cohort20),
    age10 = factor(age10)) %>%
  mutate(
    born_bf_1958 = ifelse(birthyear < 1958, 1, 0),
    born_bf_1978 = ifelse(birthyear < 1978, 1, 0))

## ==================================================================
## SAVE

saveRDS(data, "dataset_ESS/output/data.rds")