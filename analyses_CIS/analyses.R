# setwd("~/Documents/Projects/gen_legacies")
options(stringsAsFactors = FALSE)
options("modelsummary_format_numeric_latex" = "plain")
# List of packages
pkg = c("dplyr", "modelsummary", "ggplot2", "tinytable", "tidyr",
  "marginaleffects", "stringr", "MASS", "purrr", "paletteer")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
invisible(lapply(pkg, library, character.only = TRUE))
# Functions
source("func/misc.R")
source("func/sim.R")
# Seed
set.seed(23456798)

## PREPS ------------------------------------

# Load data
data = read.csv("dataset_CIS/output/data.csv")

# Turn to factor
data$cohort10 = factor(data$cohort10)
data$cohort20 = factor(data$cohort20)
data$cohort_bf1965 = factor(data$cohort_bf1965)
data$cohort_bf1958 = factor(data$cohort_bf1958)
# Years (for time-varying models)
data$year5 = data$year - (data$year %% 5)
data$year10 = data$year - (data$year %% 10)
data$year5[data$year == "1989"] = "1990"
data$year10[data$year == "1989"] = "1990"
data$year5 = factor(data$year5)
data$year10 = factor(data$year10)
data$yearn = data$year
data$year = factor(data$year)
# Age
data$age2 = data$age^2
# Small rown
data$rural = ifelse(
  data$muni_size_lab %in% c("0-2k", "2k-10k"), 1, 0)
data$rural = ifelse(is.na(data$muni_size_lab), NA, data$rural)
  
# ggplot defaults: geoms theme
theme_set(my_theme())

## MAPS & CIA for TABLES ------------------------------------

coef_recode = c(
  'cohort101920' = "1920 cohort",
  'cohort101930' = "1930 cohort",
  'cohort101940' = "1940 cohort",
  'cohort101950' = "1950 cohort",
  'cohort101960' = "1960 cohort",
  'cohort101970' = "1970 cohort",
  'cohort101980' = "1980 cohort",
  'cohort101990' = "1990 cohort",
  'cohort_bf19651' = 'Born before 1965',
  'cohort_bf19581' = 'Born before 1958',
  'age' = 'Age',
  'age2' = 'Age$^2$',
  'top_pop' = 'Local-level repression',
  'top_vio_pop' = 'Local-level repression (violence)',
  'cohort_bf19581:top_pop' = 'Born bf 1958 $\\times$ repression',
  'cohort_bf19581:top_vio_pop' = 'Born bf 1958 $\\times$ repression (violence)',
  'cohort_bf19651:top_pop' = 'Born bf 1965 $\\times$ repression',
  'cohort_bf19651:top_vio_pop' = 'Born bf 1965 $\\times$ repression (violence)')

gs = list(
  list("raw" = "nobs", "clean" = "$n$", "fmt" = 0),
  list("raw" = "aic", "clean" = "AIC", "fmt" = 1))

n = "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001. Control variables not shown: gender, ideology, age (log), rural, voted in last election and education level."

# =================================================================
# =================================================================
# =================================================================

### BASE MODELS ------------------------------------

### BY YEAR

data$cohortall = factor(data$birthy)
yfrom = 1920
yto = 1982

m_cohort_byyear = modnames(list(
  glm(asoc_part ~ factor(birthy) + factor(ESTU) +
    female + ideology + age_l + factor(educ_level) + rural + voted_lastelec,
    family = "binomial", data = data %>% filter(birthy %in% yfrom:yto)),
  glm(asoc_part_pol ~ factor(birthy) + factor(ESTU) +
    female + ideology + age_l + factor(educ_level) + rural + voted_lastelec,
    family = "binomial", data = data %>% filter(birthy %in% yfrom:yto))
  ))

preds_coh_byyear = bind_rows(
    predictions(m_cohort_byyear[[1]], by = "birthy") %>%
      mutate(dv = "All associations"),
    predictions(m_cohort_byyear[[2]], by = "birthy") %>%
      mutate(dv = "Political associations")) %>%
  dplyr::select(dv, birthy, est = estimate, upr = conf.high, lwr = conf.low)

preds_coh_byyear90 = bind_rows(
    predictions(m_cohort_byyear[[1]], by = "birthy", conf_level = 0.9) %>%
      mutate(dv = "All associations"),
    predictions(m_cohort_byyear[[2]], by = "birthy", conf_level = 0.9) %>%
      mutate(dv = "Political associations")) %>%
  dplyr::select(dv, birthy, upr90 = conf.high, lwr90 = conf.low)

preds_coh_byyear = as.data.frame(merge(preds_coh_byyear, preds_coh_byyear90))

# Plot: base cohort effects -----------------
p = ggplot(preds_coh_byyear, aes(x = birthy, y = est)) +
  geom_errorbar(aes(ymin = lwr90, ymax = upr90), width = 0, linewidth = 1.1) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0) +
  geom_point(fill = "white", shape = 21) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~dv, scales = "free_y") +
  labs(x = "\nCohort (birth year)", y = "Pr. participation in associations\n")
ggsave("analyses_CIS/output/fig_cohort_byyear.pdf",
  height = 3, width = 7, device = "pdf")

### BY DECADE

m_cohort = modnames(list(
  glm(asoc_part ~ cohort10 + factor(ESTU) +
    female + ideology + age_l + factor(educ_level) + rural + voted_lastelec,
    family = "binomial", data = data),
  glm(asoc_part_pol ~ cohort10 + factor(ESTU) +
    female + ideology + age_l + factor(educ_level) + rural + voted_lastelec,
    family = "binomial", data = data)
  ))

## Tables ##

# Base cohort models
modelsummary(
    models = m_cohort,
    output = "latex",
    # vcov = ~ESTU,
    estimate = "{estimate}{stars}",
    coef_map = coef_recode,
    gof_map = gs,
    title = "Cohort effects on participation in associations\\label{tab:glm_cohort}",
    notes = n,
    add_rows = as.data.frame(rbind(c("Survey FE",
      rep("Yes", length(m_cohort))))),
    threeparttable = TRUE, escape = FALSE) %>%
group_tt(j = list("All associations" = 2,
  "Political associations" = 3)) %>%
save_tt("analyses_CIS/output/tab_glm_cohort.tex", overwrite = TRUE)

## Simulations and plots ##

# Run simulations
sims_coh = sim(m_cohort[[1]])
sims_coh_p = sim(m_cohort[[2]])

# Prepare and merge all data for plotting
coh10names = paste0(seq(10, 99, 10), "s")
coh = rbind(
  data.frame(
    outcome = "All associations",
    cohort = coh10names,
    mean = map_dbl(sims_coh, mean),
    lwr = map_dbl(sims_coh, quantile, 0.025),
    upr = map_dbl(sims_coh, quantile, 0.975),
    lwr90 = map_dbl(sims_coh, quantile, 0.05),
    upr90 = map_dbl(sims_coh, quantile, 0.95)),
  data.frame(
    outcome = "Political associations",
    cohort = coh10names,
    mean = map_dbl(sims_coh_p, mean),
    lwr = map_dbl(sims_coh_p, quantile, 0.025),
    upr = map_dbl(sims_coh_p, quantile, 0.975),
    lwr90 = map_dbl(sims_coh_p, quantile, 0.05),
    upr90 = map_dbl(sims_coh_p, quantile, 0.95))
)

# Plot: base cohort effects -----------------
p = ggplot(coh, aes(x = cohort, y = mean)) +
  geom_errorbar(aes(ymin = lwr90, ymax = upr90), width = 0, linewidth = 1.1) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0) +
  geom_point(fill = "white", shape = 21) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~outcome, scales = "free_y") +
  labs(x = "\nCohort (birth decade)", y = "Pr. participation in associations\n")
ggsave("analyses_CIS/output/fig_cohort.pdf", height = 3, width = 7, device = "pdf")

### MODELS BY GENDER ------------------------------------

m_cohort_m = modnames(list(
  glm(asoc_part ~ cohort10 + factor(ESTU) +
    ideology + age_l + factor(educ_level) + rural + voted_lastelec,
    family = "binomial", data = data %>% filter(sex == "male")),
  glm(asoc_part_pol ~ cohort10 + factor(ESTU) +
    ideology + age_l + factor(educ_level) + rural + voted_lastelec,
    family = "binomial", data = data %>% filter(sex == "male"))
  ))

m_cohort_f = modnames(list(
  glm(asoc_part ~ cohort10 + factor(ESTU) +
    ideology + age_l + factor(educ_level) + rural + voted_lastelec,
    family = "binomial", data = data %>% filter(sex == "female")),
  glm(asoc_part_pol ~ cohort10 + factor(ESTU) +
    ideology + age_l + factor(educ_level) + rural + voted_lastelec,
    family = "binomial", data = data %>% filter(sex == "female"))
  ))

## Tables ##

# Base cohort models
modelsummary(
  models = m_cohort_m,
  output = "latex",
  # vcov = ~ESTU,
  estimate = "{estimate}{stars}",
  coef_map = coef_recode,
  gof_map = gs,
  title = "Cohort effects on participation in associations, men\\label{tab:glm_cohort_m}",
  notes = n,
  add_rows = as.data.frame(rbind(c("Survey FE", rep("Yes", length(m_cohort))))),
  threeparttable = TRUE, escape = FALSE) %>%
group_tt(j = list("All associations" = 2,
  "Political associations" = 3)) %>%
save_tt("analyses_CIS/output/tab_glm_cohort_m.tex", overwrite = TRUE)

modelsummary(
  models = m_cohort_f,
  output = "latex",
  # vcov = ~ESTU,
  estimate = "{estimate}{stars}",
  coef_map = coef_recode,
  gof_map = gs,
  title = "Cohort effects on participation in associations, women\\label{tab:glm_cohort_f}",
  notes = n,
  add_rows = as.data.frame(rbind(c("Survey FE", rep("Yes", length(m_cohort))))),
  threeparttable = TRUE, escape = FALSE) %>%
group_tt(j = list("All associations" = 2,
  "Political associations" = 3)) %>%
save_tt("analyses_CIS/output/tab_glm_cohort_f.tex", overwrite = TRUE)

## Simulations and plots ##

# Run simulations
sims_coh_m = sim(m_cohort_m[[1]])
sims_coh_m_p = sim(m_cohort_m[[2]])
sims_coh_f = sim(m_cohort_f[[1]])
sims_coh_f_p = sim(m_cohort_f[[2]])

# Prepare and merge all data for plotting
coh10names = paste0(seq(10, 99, 10), "s")
coh_gender = rbind(
  data.frame(
    outcome = "All associations",
    gender = "Men",
    cohort = coh10names,
    mean = map_dbl(sims_coh_m, mean),
    lwr = map_dbl(sims_coh_m, quantile, 0.025),
    upr = map_dbl(sims_coh_m, quantile, 0.975),
    lwr90 = map_dbl(sims_coh_m, quantile, 0.05),
    upr90 = map_dbl(sims_coh_m, quantile, 0.95)),
  data.frame(
    outcome = "Political associations",
    gender = "Men",
    cohort = coh10names,
    mean = map_dbl(sims_coh_m_p, mean),
    lwr = map_dbl(sims_coh_m_p, quantile, 0.025),
    upr = map_dbl(sims_coh_m_p, quantile, 0.975),
    lwr90 = map_dbl(sims_coh_m_p, quantile, 0.05),
    upr90 = map_dbl(sims_coh_m_p, quantile, 0.95)),
  data.frame(
    outcome = "All associations",
    gender = "Women",
    cohort = coh10names,
    mean = map_dbl(sims_coh_f, mean),
    lwr = map_dbl(sims_coh_f, quantile, 0.025),
    upr = map_dbl(sims_coh_f, quantile, 0.975),
    lwr90 = map_dbl(sims_coh_f, quantile, 0.05),
    upr90 = map_dbl(sims_coh_f, quantile, 0.95)),
  data.frame(
    outcome = "Political associations",
    gender = "Women",
    cohort = coh10names,
    mean = map_dbl(sims_coh_f_p, mean),
    lwr = map_dbl(sims_coh_f_p, quantile, 0.025),
    upr = map_dbl(sims_coh_f_p, quantile, 0.975),
    lwr90 = map_dbl(sims_coh_f_p, quantile, 0.05),
    upr90 = map_dbl(sims_coh_f_p, quantile, 0.95))
)

# Plot: base cohort effects -----------------
p = ggplot(coh_gender, aes(x = cohort, y = mean, color = gender)) +
  geom_errorbar(position = position_dodge(1/3),
    aes(ymin = lwr90, ymax = upr90), width = 0, linewidth = 1.1) +
  geom_errorbar(position = position_dodge(1/3),
    aes(ymin = lwr, ymax = upr), width = 0) +
  geom_point(position = position_dodge(1/3), fill = "white", shape = 21) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  facet_wrap(~outcome, scales = "free_y") +
  labs(x = "\nCohort (birth decade)", y = "Pr. participation in associations\n") +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  scale_color_paletteer_d(`"yarrr::google"`)
ggsave("analyses_CIS/output/fig_cohort_gender.pdf",
  height = 4, width = 7, device = "pdf")

# ===================================================================
### OVER-TIME MODELS ------------------------------------

## Effect of being Born before 1958 by survey year
m_overtime58 = modnames(list(
  glm(asoc_part ~ cohort_bf1958 * year +
    female + ideology + age_l + factor(educ_level) + rural + voted_lastelec,
    family = "binomial", data = subset(data, birthy < 2000)),
  glm(asoc_part_pol ~ cohort_bf1958 * year +
    female + ideology + age_l + factor(educ_level) + rural + voted_lastelec,
    family = "binomial", data = subset(data, birthy < 2000))
))

## Simulations and plots ##

# Coefficient plot (table way too long)
coh58_coefplot = do.call("rbind", lapply(m_overtime58, function(x)
  broom::tidy(x) %>% mutate(model = as.character(formula(x)[2])) ))

# Prepare coefficient names and upr/lwr
coh58_coefplot = coh58_coefplot %>%
  mutate(
    upr = estimate + qnorm(0.975) * std.error,
    lwr = estimate - qnorm(0.975) * std.error,
    upr90 = estimate + qnorm(0.95) * std.error,
    lwr90 = estimate - qnorm(0.95) * std.error) %>%
  mutate(model2 = ifelse(model == "asoc_part", "All associations", "Political associations")) %>%
  mutate(term2 = term) %>%
  mutate(term2 = gsub("year(\\d\\d\\d\\d)", "\\1", term2)) %>%
  mutate(term2 = gsub("cohort_bf19581", "Born before 1958", term2)) %>%
  mutate(term2 = gsub(":", " x ", term2)) %>%
  mutate(term2 = recode(term2,
    "female" = "Female",
    "ideology" = "Ideology (0-10)",
    "age_l" = "Log. Age",
    "rural" = "Live in rural area",
    "voted_lastelec" = "Voted in last election",
    "factor(educ_level)medios univ" = "Education level (higher, medium)",
    "factor(educ_level)primaria" = "Education level (primary)",
    "factor(educ_level)secundaria" = "Education level (secondary)",
    "factor(educ_level)sin estudios" = "Education level (none)",
    "factor(educ_level)superiores" = "Education level (higher)"))

p = ggplot(coh58_coefplot, aes(x = reorder(term2, desc(term2)), y = estimate)) +
  geom_errorbar(aes(ymin = lwr90, ymax = upr90), width = 0, linewidth = 1.1) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0) +
  facet_wrap(~model2, scales = "free_y") +
  geom_point(fill = "white", shape = 21) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "", y = "Estimate + 90/95 CIs") +
  coord_flip()
ggsave("analyses_CIS/output/fig_coefs_coh1958.pdf",
  height = 8, width = 8, device = "pdf")

# Simulations
sim_coh58 = sim(m_overtime58[[1]])
sim_coh58b = sim(m_overtime58[[2]])

sims_time58 = rbind(
  data.frame(
    outcome = "All associations",
    mean = map_dbl(sim_coh58, mean),
    lwr = map_dbl(sim_coh58, quantile, 0.025),
    upr = map_dbl(sim_coh58, quantile, 0.975),
    lwr90 = map_dbl(sim_coh58, quantile, 0.05),
    upr90 = map_dbl(sim_coh58, quantile, 0.95)),
  data.frame(
    outcome = "Political associations",
    mean = map_dbl(sim_coh58b, mean),
    lwr = map_dbl(sim_coh58b, quantile, 0.025),
    upr = map_dbl(sim_coh58b, quantile, 0.975),
    lwr90 = map_dbl(sim_coh58b, quantile, 0.05),
    upr90 = map_dbl(sim_coh58b, quantile, 0.95)))

sims_time58$cohort = rep(sapply(str_split(names(sim_coh58), "_"), function(x) x[2]), 2)
sims_time58$cohort = ifelse(sims_time58$cohort == "bf19581", "Born before 1958", "Born after 1958")
sims_time58$time = as.integer(rep(sapply(str_split(names(sim_coh58), "_"),
  function(x) gsub("year", "", x[3])), 2))

# Plot: Cohort (binary, 1958) effects by time -----------------
p = ggplot(sims_time58, aes(x = time, y = mean, color = cohort, group = cohort)) +
  geom_line(alpha = 0.2) +
  geom_errorbar(aes(ymin = lwr90, ymax = upr90), width = 0, linewidth = 1.1) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0) +
  facet_wrap(~outcome, scales = "free_y") +
  geom_point(fill = "white", shape = 21) +
  labs(x = "\nSurvey year", y = "Pr. participation in associations\n",
    # title = "Effect of being Born before 1958 over time",
    # subtitle = "Cohort effects are constant over time, particularly for political associationism",
    color = "Survey year") +
  theme(
    # plot.subtitle = element_text(size = 10),
    legend.title = element_blank(),
    legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1990, 2015, 5)) +
  scale_color_paletteer_d(`"yarrr::google"`)
ggsave("analyses_CIS/output/fig_cohort1958.pdf",
  height = 3.25, width = 7, device = "pdf")

## Same but setting 1965 as threshold

m_overtime65 = modnames(list(
  glm(asoc_part ~ cohort_bf1965 * year +
    female + ideology + age_l + factor(educ_level) + rural + voted_lastelec,
    family = "binomial", data = subset(data, birthy < 2000)),
  glm(asoc_part_pol ~ cohort_bf1965 * year +
    female + ideology + age_l + factor(educ_level) + rural + voted_lastelec,
    family = "binomial", data = subset(data, birthy < 2000))
))

# TABLE WAY TOO LONG - BETTER TO DO A COEFFICIENT PLOT
coh65_coefplot = do.call("rbind", lapply(m_overtime65, function(x)
  broom::tidy(x) %>% mutate(model = as.character(formula(x)[2])) ))

# Prepare coefficient names and upr/lwr
coh65_coefplot = coh65_coefplot %>%
  mutate(
    upr = estimate + qnorm(0.975) * std.error,
    lwr = estimate - qnorm(0.975) * std.error,
    upr90 = estimate + qnorm(0.95) * std.error,
    lwr90 = estimate - qnorm(0.95) * std.error) %>%
  mutate(model2 = ifelse(model == "asoc_part", "All associations", "Political associations")) %>%
  mutate(term2 = term) %>%
  mutate(term2 = gsub("year(\\d\\d\\d\\d)", "\\1", term2)) %>%
  mutate(term2 = gsub("cohort_bf19651", "Born before 1965", term2)) %>%
  mutate(term2 = gsub(":", " x ", term2)) %>%
  mutate(term2 = recode(term2,
    "female" = "Female",
    "ideology" = "Ideology (0-10)",
    "age_l" = "Log. Age",
    "rural" = "Live in rural area",
    "voted_lastelec" = "Voted in last election",
    "factor(educ_level)medios univ" = "Education level (higher, medium)",
    "factor(educ_level)primaria" = "Education level (primary)",
    "factor(educ_level)secundaria" = "Education level (secondary)",
    "factor(educ_level)sin estudios" = "Education level (none)",
    "factor(educ_level)superiores" = "Education level (higher)"))

p = ggplot(coh65_coefplot, aes(x = reorder(term2, desc(term2)), y = estimate)) +
  geom_errorbar(aes(ymin = lwr90, ymax = upr90), width = 0, linewidth = 1.1) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0) +
  facet_wrap(~model2, scales = "free_y") +
  geom_point(fill = "white", shape = 21) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "", y = "Estimate + 90/95 CIs") +
  coord_flip()
ggsave("analyses_CIS/output/fig_coefs_coh1965.pdf",
  height = 8, width = 8, device = "pdf")

# Simulations
sim_coh65 = sim(m_overtime65[[1]])
sim_coh65b = sim(m_overtime65[[2]])

sims_time65 = rbind(
  data.frame(
    outcome = "All associations",
    mean = map_dbl(sim_coh65, mean),
    lwr = map_dbl(sim_coh65, quantile, 0.025),
    upr = map_dbl(sim_coh65, quantile, 0.975),
    lwr90 = map_dbl(sim_coh65, quantile, 0.05),
    upr90 = map_dbl(sim_coh65, quantile, 0.95)),
  data.frame(
    outcome = "Political associations",
    mean = map_dbl(sim_coh65b, mean),
    lwr = map_dbl(sim_coh65b, quantile, 0.025),
    upr = map_dbl(sim_coh65b, quantile, 0.975),
    lwr90 = map_dbl(sim_coh65b, quantile, 0.05),
    upr90 = map_dbl(sim_coh65b, quantile, 0.95)))

sims_time65$cohort = rep(sapply(str_split(names(sim_coh65), "_"), function(x) x[2]), 2)
sims_time65$cohort = ifelse(sims_time65$cohort == "bf19651", "Born before 1965", "Born after 1965")
sims_time65$time = as.integer(rep(sapply(str_split(names(sim_coh65), "_"),
  function(x) gsub("year", "", x[3])), 2))

# Plot: Cohort (binary, 1965) effects by time -----------------
p = ggplot(sims_time65, aes(x = time, y = mean, color = cohort, group = cohort)) +
  geom_line(alpha = 0.2) +
  geom_errorbar(aes(ymin = lwr90, ymax = upr90), width = 0, linewidth = 1.1) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0) +
  facet_wrap(~outcome, scales = "free_y") +
  geom_point(fill = "white", shape = 21) +
  labs(x = "\nSurvey year", y = "Pr. participation in associations\n",
    # title = "Effect of being born before 1965 over time",
    # subtitle = "Cohort effects are constant over time, particularly for political associationism",
    color = "Survey year") +
  theme(
    # plot.subtitle = element_text(size = 10),
    legend.title = element_blank(),
    legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1990, 2015, 5)) +
  scale_color_paletteer_d(`"yarrr::google"`)
ggsave("analyses_CIS/output/fig_cohort1965.pdf",
  height = 3.25, width = 7, device = "pdf")

## Effect of age across survey years

m_age = modnames(list(glm(asoc_part ~ age + age2 +
    female + ideology + factor(educ_level) + factor(ESTU) + rural + voted_lastelec,
    family = "binomial", data = subset(data, year5 == "1990")),
  glm(asoc_part ~ age + age2 +
    female + ideology + factor(educ_level) + factor(ESTU) + rural + voted_lastelec,
    family = "binomial", data = subset(data, year5 == "1995")),
  glm(asoc_part ~ age + age2 +
    female + ideology + factor(educ_level) + factor(ESTU) + rural + voted_lastelec,
    family = "binomial", data = subset(data, year5 == "2000")),
  glm(asoc_part ~ age + age2 +
    female + ideology + factor(educ_level) + factor(ESTU) + rural + voted_lastelec,
    family = "binomial", data = subset(data, year5 == "2005")),
  glm(asoc_part ~ age + age2 +
    female + ideology + factor(educ_level) + factor(ESTU) + rural + voted_lastelec,
    family = "binomial", data = subset(data, year5 == "2010")),
  glm(asoc_part ~ age + age2 +
    female + ideology + factor(educ_level) + factor(ESTU) + rural + voted_lastelec,
    family = "binomial", data = subset(data, year5 == "2015"))))

m_age_pol = modnames(list(glm(asoc_part_pol ~ age + age2 +
    female + ideology + factor(educ_level) + factor(ESTU) + rural + voted_lastelec,
    family = "binomial", data = subset(data, year5 == "1990")),
  glm(asoc_part_pol ~ age + age2 +
    female + ideology + factor(educ_level) + factor(ESTU) + rural + voted_lastelec,
    family = "binomial", data = subset(data, year5 == "1995")),
  glm(asoc_part_pol ~ age + age2 +
    female + ideology + factor(educ_level) + factor(ESTU) + rural + voted_lastelec,
    family = "binomial", data = subset(data, year5 == "2000")),
  glm(asoc_part_pol ~ age + age2 +
    female + ideology + factor(educ_level) + factor(ESTU) + rural + voted_lastelec,
    family = "binomial", data = subset(data, year5 == "2005")),
  glm(asoc_part_pol ~ age + age2 +
    female + ideology + factor(educ_level) + factor(ESTU) + rural + voted_lastelec,
    family = "binomial", data = subset(data, year5 == "2010")),
  glm(asoc_part_pol ~ age + age2 +
    female + ideology + factor(educ_level) + factor(ESTU) + rural + voted_lastelec,
    family = "binomial", data = subset(data, year5 == "2015"))))

## TABLES

# Age models
modelsummary(
  models = m_age,
  output = "latex",
  # # vcov = ~ESTU,
  estimate = "{estimate}{stars}",
  coef_map = coef_recode,
  gof_map = gs,
  title = "Age effects in participation in associations by survey year\\label{tab:glm_age}",
  notes = n,
  add_rows = as.data.frame(rbind(c("Survey FE", rep("Yes", length(m_age))))),
  threeparttable = TRUE, escape = FALSE) %>%
group_tt(j = list("1989--1994" = 2, "1995--1999" = 3,
  "2000--2004" = 4, "2005-2009" = 5, "2010-2014" = 6,
  "2015-2017" = 7)) %>%
save_tt("analyses_CIS/output/tab_glm_age.tex", overwrite = TRUE)



# Age models, political associations
modelsummary(
  models = m_age_pol,
  output = "latex",
  # # vcov = ~ESTU,
  estimate = "{estimate}{stars}",
  coef_map = coef_recode,
  gof_map = gs,
  title = "Age effects in participation in political associations by survey year\\label{tab:glm_age_pol}",
  notes = n,
  add_rows = as.data.frame(rbind(c("Survey FE", rep("Yes", length(m_age_pol))))),
  threeparttable = TRUE, escape = FALSE) %>%
group_tt(j = list("1989--1994" = 2, "1995--1999" = 3,
  "2000--2004" = 4, "2005-2009" = 5, "2010-2014" = 6,
  "2015-2017" = 7)) %>%
save_tt("analyses_CIS/output/tab_glm_age_pol.tex", overwrite = TRUE)

## Simulations and plots

# Simulated effect of age in earliest vs latest surveys
ageseq = seq(18, 82, 4)
y1 = which(unlist(lapply(m_age, function(x) unique(x$data$year5) == 1990)))
y2 = which(unlist(lapply(m_age, function(x) unique(x$data$year5) == 2015)))
y1n = "1989-1994"
y2n = "2015-2017"
age_sim = rbind(
    sim_to_df(sim_age(m_age[[y1]], age_val = ageseq), "All") %>% mutate(y = y1n),
    sim_to_df(sim_age(m_age_pol[[y1]], age_val = ageseq), "Political") %>% mutate(y = y1n),
    sim_to_df(sim_age(m_age[[y2]], age_val = ageseq), "All") %>% mutate(y = y2n),
    sim_to_df(sim_age(m_age_pol[[y2]], age_val = ageseq), "Political") %>% mutate(y = y2n)) %>%
  mutate(age = rep(ageseq, 4), outcome = paste(outcome, "associations"))

p = ggplot(age_sim %>% filter(grepl("Pol", outcome)),
    aes(x = age, y = mean, color = y, group = y)) +
  geom_line(alpha = 0.2, position = position_dodge(1/2)) +
  geom_errorbar(aes(ymin = lwr90, ymax = upr90), width = 0, linewidth = 1.1,
    position = position_dodge(1/2)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0,
    position = position_dodge(1/2)) +
  geom_point(position = position_dodge(1/2), fill = "white", shape = 21) +
  labs(x = "\nAge", y = "Pr. participation in\npolitical associations\n",
    # title = "Age effect in earliest vs latest surveys",
    # subtitle = "Respondents 55+ significantly less likely to have participated in political associations\nas of early 1990s (~ pre-war generation vs baby boomers)",
    color = "Survey year") +
  theme(
    #plot.subtitle = element_text(size = 10),
    legend.position = "bottom") +
  scale_x_continuous(breaks = seq(20, 80, 10)) +
  scale_color_paletteer_d(`"yarrr::google"`)
ggsave("analyses_CIS/output/fig_funcform_age_pol.pdf",
  height = 3, width = 6, device = "pdf")

p = ggplot(age_sim %>% filter(grepl("All", outcome)),
    aes(x = age, y = mean, color = y, group = y)) +
  geom_line(alpha = 0.2, position = position_dodge(1/2)) +
  geom_errorbar(aes(ymin = lwr90, ymax = upr90), width = 0, linewidth = 1.1,
    position = position_dodge(1/2)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0,
    position = position_dodge(1/2)) +
  geom_point(position = position_dodge(1/2), fill = "white", shape = 21) +
  labs(x = "\nAge", y = "Pr. participation in\nany association\n",
    # title = "Age effect in earliest vs latest surveys",
    # subtitle = "...But we do not observe the same when considering all associations\n",
    color = "Survey year") +
  theme(
    #plot.subtitle = element_text(size = 10),
    legend.position = "bottom") +
  scale_x_continuous(breaks = seq(20, 80, 10)) +
  scale_color_paletteer_d(`"yarrr::google"`)
ggsave("analyses_CIS/output/fig_funcform_age_all.pdf",
  height = 3, width = 6, device = "pdf")

# ===================================================================
### EXPLORING AGE-COHORT-PERIOD EFFECTS (cf Rohrer 2025)

avg = data %>%
  mutate(
    cohort = as.integer(as.character(cohort5)),
    period = as.integer(as.character(year5))) %>%
  group_by(cohort, period) %>%
  filter(age >= 18) %>%
  filter(birthy %in% 1925:1984) %>%
  summarize(
    n = n(),
    est_part = mean(asoc_part, na.rm = TRUE),
    se_part = sd(asoc_part, na.rm = TRUE) / sqrt(n()),
    est_partpol = mean(asoc_part_pol, na.rm = TRUE),
    se_partpol = sd(asoc_part_pol, na.rm = TRUE) / sqrt(n())) %>%
  pivot_longer(
    cols = est_part:se_partpol,
    names_to = c("stat", "dv"),
    names_pattern = "(.*)_(.*)") %>%
  pivot_wider(names_from = "stat", values_from = "value") %>%
  upr_lwr %>%
  mutate(cohortlab = paste0(cohort, "s")) %>%
  mutate(periodlab = recode(period,
    "1990" = "1989-94",
    "1995" = "1995-99",
    "2000" = "2000-04",
    "2005" = "2005-09",
    "2010" = "2010-14",
    "2015" = "2015-17")) %>%
  mutate(age = period - cohort) %>%
  filter(age >= 18) %>%
  mutate(agelab = paste0(age, "+")) %>%
  mutate(dv = recode(dv,
    "part" = "All associations",
    "partpol" = "Political associations")) %>%
  mutate(cregime = ifelse(cohort >= 1955,
    "Post-Franco cohort", "Franco cohort"))

blues = paletteer_d("RColorBrewer::PuBuGn")
reds = paletteer_d("RColorBrewer::OrRd")
cols = c(blues[4:9], reds[2:7])

p = ggplot(avg, aes(x = age, y = est,
    group = cohortlab, color = cohortlab)) +
  geom_line() +
  geom_point(fill = "white") +
  facet_wrap(~dv, scales = "free_y") +
  labs(x = "\nAge",
    y = "Participation average\n", color = "Cohort\n(5y groups)",
    # title = "Cohort-age trajectories",
    subtitle = "Post-Franco cohorts in warm colors, Franco cohorts in cold colors.") +
  theme(
    plot.subtitle = element_text(size = 8),
    legend.position = "bottom") +
  scale_color_manual(values = cols)
ggsave("analyses_CIS/output/fig_avg_ac.pdf", height = 5, width = 8)
