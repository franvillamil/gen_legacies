# setwd("~/Documents/Projects/gen_legacies")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "ggplot2", "stringr")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
invisible(lapply(c(pkg, "muniSpain"), library, character.only = TRUE))

# LOAD and transform muni_code
data = read.csv("input/encuestas_CIS.csv") %>%
  mutate(muni_code = ifelse(is.na(muni_code), NA,
    sprintf("%05d", as.integer(muni_code))))

# Get local population, merge with data
census = read.csv("input/INE_census.csv") %>%
  mutate(muni_code = sprintf("%05d", as.integer(muni_code))) %>%
  select(muni_code, pop1970 = c1970, pop1960 = c1960)
data = merge(data, census, all.x = TRUE)

# Get cohort variables
data$birthy = data$year - data$age
data$cohort10 = data$birthy - (data$birthy %% 10)
data$cohort5 = data$birthy - (data$birthy %% 5)
data$cohort20 = data$birthy - ((data$birthy - 25) %% 20)
data$cohort_bf1965 = ifelse(data$birthy >= 1965, 0, 1)
data$cohort_bf1958 = ifelse(data$birthy >= 1958, 0, 1)
# And survey year
data$year10 = data$year - (data$year %% 10)
data$year5 = data$year - (data$year %% 5)
# And age
data$age10 = data$age - (data$age %% 10)
data$age10[data$age10 %in% c(10, 80, 90, 100)] = NA
data$age20 = data$age - (data$age %% 20)
data$age20[!data$age20 %in% 20:60] = NA
data$age_cat = "18-35"
data$age_cat[data$age >= 35] = "35-59"
data$age_cat[data$age >= 60] = "60+"

# Remove cohorts before 1910 (too few obs)
data = subset(data, birthy > 1909)

# Extra variables
data$female = ifelse(data$sex == "female", 1, 0)
data$age_l = log(data$age)
data$educ_level[data$educ_level %in% c("FP", "otros")] = "FP/otros"
data$ccaa = NA
data$ccaa[!is.na(data$prov)] = prov_to_ccaa(
  data$prov[!is.na(data$prov)])
data$ideog = ifelse(data$ideology %in% 7:10, "right", NA)
data$ideog = ifelse(data$ideology %in% 4:6, "center", data$ideog)
data$ideog = ifelse(data$ideology %in% 0:3, "left", data$ideog)

## SAVE
write.csv(data, "dataset_CIS/output/dataset.csv", row.names = FALSE)
