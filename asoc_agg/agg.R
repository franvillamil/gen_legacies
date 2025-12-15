# setwd("~/Documents/Projects/gen_legacies")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("tidyr", "dplyr", "stringr")
# muniSpain
if(!"muniSpain" %in% rownames(installed.packages())){
  if(!"devtools" %in% rownames(installed.packages())){install.packages("devtools")}
  library(devtools)
  install_github("franvillamil/muniSpain")
}
# Load
invisible(lapply(c(pkg, "muniSpain"), library, character.only = TRUE))


# Load
asoc = read.csv("input/asoc.csv") %>%
  filter(!is.na(date)) %>%
  mutate(date = ifelse(date == "1111-01-01" & grepl("gal_1978", id),
    "1978-01-01", date)) %>%
  mutate(date = ifelse(date == "1111-01-01" & grepl("gal_1988", id), 
    "1988-01-01", date)) %>%
  mutate(date = ifelse(date == "1111-01-01" & grepl("gal_1997", id),
    "1997-01-01", date)) %>%
  mutate(date = as.Date(date)) %>%
  mutate(year = as.integer(format(date, "%Y"))) %>%
  # Muni adaptations like rest of dataset (TOP period to today)
  mutate(muni_code = changes_newcode(muni_code, "1960", "2011")) %>%
  filter(!is.na(muni_code))

# Aggregate, by year and full period
asoc_agg = asoc %>%
  filter(date >= "1976-01-01") %>%
  group_by(muni_code, year) %>%
  summarize(asoc = length(id)) %>%
  ungroup() %>%
  pivot_wider(names_from = year,
    names_glue = "asoc_{year}",
    values_from = c(asoc)) %>%
  ungroup() %>%
  mutate(across(starts_with("asoc"), ~replace_na(., 0))) %>%
  rowwise() %>%
  mutate(asoc_post75 = sum(c_across(starts_with("asoc")))) %>%
  ungroup() %>%
  select(order(colnames(.)))

# Associations 1966-1975
asoc_66_75 = asoc %>%
  filter(date >= "1966-01-01" & date < "1976-01-01") %>%
  group_by(muni_code) %>%
  summarize(asoc_66_75 = n())

# Merge
asoc_agg = full_join(asoc_agg, asoc_66_75) %>%
  mutate(asoc_66_75 = coalesce(asoc_66_75, 0))

## Save
write.csv(asoc_agg, "asoc_agg/output/asoc_agg.csv", row.names = FALSE)