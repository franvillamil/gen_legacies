# setwd("~/Documents/Projects/gen_legacies")
options(stringsAsFactors = FALSE)
options("modelsummary_format_numeric_latex" = "plain")
# List of packages
pkg = c("dplyr", "broom", "modelsummary", "ggplot2",
  "marginaleffects", "paletteer")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
invisible(lapply(pkg, library, character.only = TRUE))
# Functions
source("func/misc.R")
source("func/sim.R")
# ggplot defaults: geoms theme
theme_set(my_theme())

## ==================================================================
## LOAD DATA AND LIMIT COUNTRIES

data = readRDS("dataset_ESS/output/data.rds")

# Limit to countries with at least 15000 observations
ct = table(data$countryname)[order(ct = table(data$countryname), decreasing = TRUE)]
# data = subset(data, countryname %in% names(ct)[1:10])
data = subset(data, countryname %in% names(ct[ct > 20000]))

# Get combination of unique countries and rounds
combs = expand.grid(c = unique(data$countryname), y = unique(data$svyear))

## ==================================================================
## SAME THRESHOLD (1958) ACROSS COUNTRIES

# Empty df
coh58 = data.frame(NULL)

# Run
for(i in 1:nrow(combs)){
  
  # Subset data and checks (skip if no data)
  c = combs$c[i]
  y = combs$y[i]
  df = data %>%
    filter(countryname == c & svyear == y)
  if(nrow(df) == 0){next}
  if(all(is.na(df$asoc_part_pol))){next}

  m = glm(asoc_part_pol ~ born_bf_1958 +
    female + ideology + age_l + educ_level,
    family = "binomial", data = df)

  coh58 = bind_rows(
    coh58,
    tidy(m) %>% filter(grepl("1958", term)) %>%
      dplyr::select(est=estimate, se=std.error) %>%
      upr_lwr %>%
      mutate(country = c, year = y)
  )

}

coh58 = coh58 %>%
  mutate(sig95 = factor(ifelse(abs(est / se) >= qnorm(0.975), 1, 0)))

p = ggplot(coh58, aes(x = year, y = est, color = sig95, group = 1)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_line(alpha = 0.2, color = "black") +
  geom_point() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  facet_wrap(~country, scales = "free", ncol = 4) +
  scale_color_manual(values = c("gray75", "black")) +
  labs(x = "Survey round (ESS rounds)",
    y = "Effect of being born before 1958 on participation in trade unions") +
  theme(legend.position = "none")
ggsave("analyses_ESS/output/cutoff58_over_time.pdf",
  width = 10, height = 4)

## ==================================================================
## COHORTS OVER TIME

# Empty df
cohcoefs = data.frame(NULL)

# Run
for(i in 1:nrow(combs)){
  
  # Subset data and checks (skip if no data)
  c = combs$c[i]
  y = combs$y[i]
  df = data %>% filter(countryname == c & svyear == y)
  if(nrow(df) == 0){next}
  if(all(is.na(df$asoc_part_pol))){next}

  # Model
  df$cohort = df$cohort20
  m = glm(asoc_part_pol ~ cohort +
    female + ideology + age_l + educ_level,
    family = "binomial", data = df)

  preds = predictions(m, by = "cohort") %>%
    as.data.frame() %>%
    dplyr::select(cohort, est = estimate, lwr = conf.low, upr = conf.high) %>%
    mutate(country = c, year = y)
  cohcoefs = bind_rows(cohcoefs, preds)

}

cohcoefs$cohlab = recode(cohcoefs$cohort,
  "1930" = "1930-40s", "1950" = "1950-60s", "1970" = "1970-80s")

plot = ggplot(cohcoefs,
    aes(x = year, y = est, group = cohlab, color = cohlab)) +
  # geom_hline(yintercept = 0, linetype = "dotted") +
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0.2) +
  facet_wrap(~country, scales = "free_y", ncol = 4) +
  labs(color = "Cohort", x = "Survey year (ESS rounds)",
    y = "Predicted probability of\nparticipating in trade unions") +
  scale_color_paletteer_d(`"yarrr::google"`) +
  theme(legend.position = "bottom")
  # theme(legend.position.inside = c(0.1, 0.1))
ggsave("analyses_ESS/output/cohorts_over_time.pdf",
  width = 10, height = 5)