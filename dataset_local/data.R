# setwd("~/Documents/Projects/gen_legacies")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "stringr", "readstata13", "tidyr", "readxl")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# muniSpain
if(!"muniSpain" %in% rownames(installed.packages())){
  if(!"devtools" %in% rownames(installed.packages())){install.packages("devtools")}
  library(devtools)
  install_github("franvillamil/muniSpain")
}
# Load
invisible(lapply(c(pkg, "muniSpain"), library, character.only = TRUE))

## Function: NA to 0
NA_to_0 = function(v){v[is.na(v)] = 0; return(v)}

## Base dataset: 2011 census
data = read.csv("input/INE_census.csv") %>%
  rename(muni_code_raw = muni_code) %>%
  mutate(muni_code = changes_newcode(muni_code_raw, "1960", "2011")) %>%
  filter(!is.na(muni_code)) %>%
  group_by(muni_code) %>%
  summarize(
    pop2011 = sum(c2011, na.rm = T),
    pop2001 = sum(c2001, na.rm = T),
    pop1991 = sum(c1991, na.rm = T),
    pop1981 = sum(c1981, na.rm = T),
    pop1970 = sum(c1970, na.rm = T),
    pop1960 = sum(c1960, na.rm = T),
    pop1940 = sum(c1940, na.rm = T),
    pop1930 = sum(c1930, na.rm = T)) %>%
  mutate(prov = code_to_prov(str_sub(muni_code, -5L, -4L))) %>%
  mutate(
    pop_chg4060 = (pop1960 - pop1940) / pop1940,
    pop_chg4070 = (pop1970 - pop1940) / pop1940,
    pop_chg6011 = (pop2011 - pop1960) / pop1960) %>%
  mutate(
    pop_chg4060 = ifelse(pop_chg4060 == "Inf", NA, pop_chg4060),
    pop_chg4070 = ifelse(pop_chg4070 == "Inf", NA, pop_chg4070)) %>%
as.data.frame

## Add variables from TOP
top = read.csv("input/aggregated_TOP.csv") %>%
  mutate(muni_code = sprintf("%05s", muni_code))
if(!all(top$muni_code %in% data$muni_code)){stop("!?")}
data = merge(data, top, all.x = TRUE) %>%
  mutate(across(starts_with("top"), ~replace_na(., 0))) %>%
  # Binary variables
  mutate(
    top_bin = ifelse(top > 0, 1, 0),
    top_noeta_bin = ifelse(top_noeta > 0, 1, 0),
    top_vio_bin = ifelse(top_vio > 0, 1, 0),
    top_nonvio_bin = ifelse(top_nonvio > 0, 1, 0),
    top_asocprop_bin = ifelse(top_asocprop > 0, 1, 0)) %>%
  # Pop-weighted (1960)
  mutate(
    top_p = top / pop1960 * 1000,
    top_noeta_p = top_noeta / pop1960 * 1000,
    top_vio_p = top_vio / pop1960 * 1000,
    top_nonvio_p = top_nonvio / pop1960 * 1000,
    top_asocprop_p = top_asocprop / pop1960 * 1000)


## Replication data from Drelichman, Vidal-Robert & Voth 2021
dvv = read.dta13("input/Inquisition_analysis_dataset.dta", convert.factors = F) %>%
  mutate(muni_code_raw = sprintf("%05s", preferred_inecode)) %>%
  mutate(muni_code = changes_newcode(muni_code_raw, "1960", "2011")) %>%
  filter(!is.na(muni_code)) %>%
  mutate(c_secondplus_count = c_secondplus * pop_padron) %>%
  group_by(muni_code) %>%
  summarize(gdppc = mean(gdppc, na.rm = TRUE),
    med_income = mean(income_med, na.rm = TRUE),
    trust = mean(trust2, na.rm = TRUE),
    pop_padron = sum(pop_padron, na.rm = TRUE),
    edu_secondplus = sum(c_secondplus_count, na.rm = TRUE)) %>%
  mutate(edu_secondplus = edu_secondplus / pop_padron)
# Check & merge
if(!all(dvv$muni_code %in% data$muni_code)){stop("!?")}
data = merge(data, dvv, all.x = TRUE)

## GIS data
gis = read.csv("input/elev_sd_1960_2011.csv") %>%
  mutate(muni_code = sprintf("%05s", muni_code)) %>%
  select(muni_code, elev_sd)
# Check & merge
if(!all(gis$muni_code %in% data$muni_code)){stop("!?")}
data = merge(data, gis, all.x = TRUE)

## Distance to province capital
distcap = read.csv("input/distcap.csv") %>%
  mutate(muni_code = sprintf("%05s", muni_code)) %>%
  select(muni_code, dist_prov_cap)
# Check & merge
if(!all(distcap$muni_code %in% data$muni_code)){stop("!?")}
data = merge(data, distcap, all.x = TRUE)

## Asociaciones
asoc = read.csv("asoc_agg/output/asoc_agg.csv") %>%
  mutate(muni_code = sprintf("%05s", muni_code)) %>%
  # 5-year periods
  rowwise() %>%
  mutate(
    asoc_76_80 = sum(c_across(asoc_1976:asoc_1980)),
    asoc_81_85 = sum(c_across(asoc_1981:asoc_1985)),
    asoc_86_90 = sum(c_across(asoc_1986:asoc_1990)),
    asoc_91_95 = sum(c_across(asoc_1991:asoc_1995)),
    asoc_96_00 = sum(c_across(asoc_1996:asoc_2000)),
    asoc_01_05 = sum(c_across(asoc_2001:asoc_2005)),
    asoc_06_10 = sum(c_across(asoc_2006:asoc_2010)),
    asoc_11_15 = sum(c_across(asoc_2011:asoc_2015)),
    asoc_16_20 = sum(c_across(asoc_2016:asoc_2020)))
# Random check (just in case - above only works if column ordered)
if(!identical(sum(asoc$asoc_81_85),
  with(asoc, sum(asoc_1981, asoc_1982, asoc_1983, asoc_1984, asoc_1985)))){stop("!!!")}
# Check municipalites & merge
if(!all(asoc$muni_code %in% data$muni_code)){stop("!?")}
data = merge(data, asoc, all.x = TRUE)
# Turn to 0, only in provinces in dataset
data$prov_in_asoc = data$prov %in% code_to_prov(str_sub(asoc$muni_code, -5L, -4L))
for(v in names(data)[grepl("^asoc_", names(data))]){
  data[, v] = ifelse(is.na(data[, v]) & data$prov_in_asoc, 0, data[, v])
}

## Last filters
data = data %>%
  # Remove Ceuta and Melilla
  filter(!prov %in% c("ceuta", "melilla")) %>%
  # And problematic municipalities (Badia del V, etc)
  filter(pop1960 != 0)

# Create new variables
data = data %>%
  mutate(
    gdppc_l = log(gdppc),
    pop2011_l = log(pop2011),
    pop1991_l = log(pop1991),
    pop1981_l = log(pop1981),
    pop1960_l = log(pop1960),
    pop_padron_l = log(pop_padron),
    top_pl = log(top_p + 1),
    top_vio_pl = log(top_vio_p + 1),
    top_nonvio_pl = log(top_nonvio_p + 1),
    top_asocprop_pl = log(top_asocprop_p + 1)
  ) %>%
  mutate(
    asoc_66_75_p = asoc_66_75 / pop1970 * 1000,
    asoc_post75_p = asoc_post75 / pop2011 * 1000,
    asoc_76_80_p = asoc_76_80 / pop1981 * 1000,
    asoc_81_85_p = asoc_81_85 / pop1981 * 1000,
    asoc_86_90_p = asoc_86_90 / pop1991 * 1000,
    asoc_91_95_p = asoc_91_95 / pop1991 * 1000,
    asoc_96_00_p = asoc_96_00 / pop2001 * 1000,
    asoc_01_05_p = asoc_01_05 / pop2001 * 1000,
    asoc_06_10_p = asoc_06_10 / pop2011 * 1000,
    asoc_11_15_p = asoc_11_15 / pop2011 * 1000,
    asoc_16_20_p = asoc_16_20 / pop2011 * 1000) %>%
  mutate(
    asoc_1976_p = asoc_1976 / pop1981 * 1000,
    asoc_1977_p = asoc_1977 / pop1981 * 1000,
    asoc_1978_p = asoc_1978 / pop1981 * 1000,
    asoc_1979_p = asoc_1979 / pop1981 * 1000,
    asoc_1980_p = asoc_1980 / pop1981 * 1000,
    asoc_1981_p = asoc_1981 / pop1981 * 1000,
    asoc_1982_p = asoc_1982 / pop1981 * 1000,
    asoc_1983_p = asoc_1983 / pop1981 * 1000,
    asoc_1984_p = asoc_1984 / pop1981 * 1000,
    asoc_1985_p = asoc_1985 / pop1981 * 1000,
    asoc_1986_p = asoc_1986 / pop1991 * 1000,
    asoc_1987_p = asoc_1987 / pop1991 * 1000,
    asoc_1988_p = asoc_1988 / pop1991 * 1000,
    asoc_1989_p = asoc_1989 / pop1991 * 1000,
    asoc_1990_p = asoc_1990 / pop1991 * 1000,
    asoc_1991_p = asoc_1991 / pop1991 * 1000,
    asoc_1992_p = asoc_1992 / pop1991 * 1000,
    asoc_1993_p = asoc_1993 / pop1991 * 1000,
    asoc_1994_p = asoc_1994 / pop1991 * 1000,
    asoc_1995_p = asoc_1995 / pop1991 * 1000,
    asoc_1996_p = asoc_1996 / pop2001 * 1000,
    asoc_1997_p = asoc_1997 / pop2001 * 1000,
    asoc_1998_p = asoc_1998 / pop2001 * 1000,
    asoc_1999_p = asoc_1999 / pop2001 * 1000,
    asoc_2000_p = asoc_2000 / pop2001 * 1000,
    asoc_2001_p = asoc_2001 / pop2001 * 1000,
    asoc_2002_p = asoc_2002 / pop2001 * 1000,
    asoc_2003_p = asoc_2003 / pop2001 * 1000,
    asoc_2004_p = asoc_2004 / pop2001 * 1000,
    asoc_2005_p = asoc_2005 / pop2001 * 1000,
    asoc_2006_p = asoc_2006 / pop2011 * 1000,
    asoc_2007_p = asoc_2007 / pop2011 * 1000,
    asoc_2008_p = asoc_2008 / pop2011 * 1000,
    asoc_2009_p = asoc_2009 / pop2011 * 1000,
    asoc_2010_p = asoc_2010 / pop2011 * 1000,
    asoc_2011_p = asoc_2011 / pop2011 * 1000,
    asoc_2012_p = asoc_2012 / pop2011 * 1000,
    asoc_2013_p = asoc_2013 / pop2011 * 1000,
    asoc_2014_p = asoc_2014 / pop2011 * 1000,
    asoc_2015_p = asoc_2015 / pop2011 * 1000,
    asoc_2016_p = asoc_2016 / pop2011 * 1000,
    asoc_2017_p = asoc_2017 / pop2011 * 1000,
    asoc_2018_p = asoc_2018 / pop2011 * 1000,
    asoc_2019_p = asoc_2019 / pop2011 * 1000,
    asoc_2020_p = asoc_2020 / pop2011 * 1000) %>%
  # binary
  mutate(
    asoc_1976_bin = ifelse(asoc_1976 > 0, 1, 0),
    asoc_1977_bin = ifelse(asoc_1977 > 0, 1, 0),
    asoc_1978_bin = ifelse(asoc_1978 > 0, 1, 0),
    asoc_1979_bin = ifelse(asoc_1979 > 0, 1, 0),
    asoc_1980_bin = ifelse(asoc_1980 > 0, 1, 0),
    asoc_1981_bin = ifelse(asoc_1981 > 0, 1, 0),
    asoc_1982_bin = ifelse(asoc_1982 > 0, 1, 0),
    asoc_1983_bin = ifelse(asoc_1983 > 0, 1, 0),
    asoc_1984_bin = ifelse(asoc_1984 > 0, 1, 0),
    asoc_1985_bin = ifelse(asoc_1985 > 0, 1, 0),
    asoc_1986_bin = ifelse(asoc_1986 > 0, 1, 0),
    asoc_1987_bin = ifelse(asoc_1987 > 0, 1, 0),
    asoc_1988_bin = ifelse(asoc_1988 > 0, 1, 0),
    asoc_1989_bin = ifelse(asoc_1989 > 0, 1, 0),
    asoc_1990_bin = ifelse(asoc_1990 > 0, 1, 0),
    asoc_1991_bin = ifelse(asoc_1991 > 0, 1, 0),
    asoc_1992_bin = ifelse(asoc_1992 > 0, 1, 0),
    asoc_1993_bin = ifelse(asoc_1993 > 0, 1, 0),
    asoc_1994_bin = ifelse(asoc_1994 > 0, 1, 0),
    asoc_1995_bin = ifelse(asoc_1995 > 0, 1, 0),
    asoc_1996_bin = ifelse(asoc_1996 > 0, 1, 0),
    asoc_1997_bin = ifelse(asoc_1997 > 0, 1, 0),
    asoc_1998_bin = ifelse(asoc_1998 > 0, 1, 0),
    asoc_1999_bin = ifelse(asoc_1999 > 0, 1, 0),
    asoc_2000_bin = ifelse(asoc_2000 > 0, 1, 0),
    asoc_2001_bin = ifelse(asoc_2001 > 0, 1, 0),
    asoc_2002_bin = ifelse(asoc_2002 > 0, 1, 0),
    asoc_2003_bin = ifelse(asoc_2003 > 0, 1, 0),
    asoc_2004_bin = ifelse(asoc_2004 > 0, 1, 0),
    asoc_2005_bin = ifelse(asoc_2005 > 0, 1, 0),
    asoc_2006_bin = ifelse(asoc_2006 > 0, 1, 0),
    asoc_2007_bin = ifelse(asoc_2007 > 0, 1, 0),
    asoc_2008_bin = ifelse(asoc_2008 > 0, 1, 0),
    asoc_2009_bin = ifelse(asoc_2009 > 0, 1, 0),
    asoc_2010_bin = ifelse(asoc_2010 > 0, 1, 0),
    asoc_2011_bin = ifelse(asoc_2011 > 0, 1, 0),
    asoc_2012_bin = ifelse(asoc_2012 > 0, 1, 0),
    asoc_2013_bin = ifelse(asoc_2013 > 0, 1, 0),
    asoc_2014_bin = ifelse(asoc_2014 > 0, 1, 0),
    asoc_2015_bin = ifelse(asoc_2015 > 0, 1, 0),
    asoc_2016_bin = ifelse(asoc_2016 > 0, 1, 0),
    asoc_2017_bin = ifelse(asoc_2017 > 0, 1, 0),
    asoc_2018_bin = ifelse(asoc_2018 > 0, 1, 0),
    asoc_2019_bin = ifelse(asoc_2019 > 0, 1, 0),
    asoc_2020_bin = ifelse(asoc_2020 > 0, 1, 0)
  )

# Valladolid: weird (only 86, and only after 1990s), remove all
for(v in names(data)[grepl("^asoc_", names(data))]){
  data[, v][data$prov=="valladolid"] = NA
}

## SAVE
write.csv(data, "dataset_local/output/data.csv", row.names = FALSE)
