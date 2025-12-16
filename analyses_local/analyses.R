# setwd("~/Documents/Projects/gen_legacies")
options(stringsAsFactors = FALSE)
options("modelsummary_format_numeric_latex" = "plain")
# List of packages
pkg = c("dplyr", "modelsummary", "ggplot2", "kableExtra", "paletteer",
  "broom", "stringr", "marginaleffects", "tidyr", "sandwich", "broom", "lmtest")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
invisible(lapply(pkg, library, character.only = TRUE))

# Func
source("func/misc.R")

### ====================================================================
## PREPS

# ggplot defaults: geoms theme
theme_set(my_theme())

# Load
data = read.csv("dataset_local/output/data.csv")

# Turn TOP to logged
data$top_p = log(data$top_p+1)

## Tidy function including vcovCL
my_tidy = function(x){
  df = tidy(coeftest(x, vcov = vcovCL, cluster = data$prov))
  return(df)
}

## MAPS & CIA for TABLES/PLOTS ------------------------------------

coef_recode = c(
  'top_bin' = 'Repression (binary)',
  'top_p' = 'Repression (continuous)',
  'gdppc_l' = 'Log GDPpc',
  'pop2011_l' = 'Log Pop 2011',
  'pop1960_l' = 'Log Pop 1960',
  'pop_chg6011' = 'Pop change 1960-2011',
  'elev_sd' = 'Elev SD',
  'dist_prov_cap' = 'Dist to prov capital (km)'
)

gs = list(
  list("raw" = "nobs", "clean" = "$n$", "fmt" = 0),
  list("raw" = "r.squared", "clean" = "$R^2$", "fmt" = 2),
  list("raw" = "adj.r.squared", "clean" = "Adj. $R^2$", "fmt" = 2))

n = "+ p $<$ 0.1, * p $<$ 0.05, ** p $<$ 0.01, *** p $<$ 0.001. Province FE not shown, clustered SE (province). Outcome variable is the number of associations formed in each period, divided by population (closest census) and multiplied by 1,000."

n_cont = "Continuous TOP variable is coded as the logged number of TOP sentences per 1,000 inhabitants in 1960."

### ====================================================================
### MAIN MODELS

# Formulae
controls = "+ gdppc_l + pop1960_l + pop_chg6011 + elev_sd + dist_prov_cap + factor(prov)"
dv = paste0(c("asoc_post75", "asoc_76_80", "asoc_81_85", "asoc_86_90",
  "asoc_91_95", "asoc_96_00", "asoc_01_05", "asoc_06_10",
  "asoc_11_15", "asoc_16_20"), "_p")
f_all = paste(dv, "~ top_bin", controls)
f_cont_all = paste(dv, "~ top_p", controls)

# DV labels and header for modelsummary
dv_label = recode(
  gsub("_", "-",
    gsub("^(7|8|9)", "19\\1",
      gsub("^(0|1)", "20\\1",
        gsub("^asoc_|_(p|pl)$", "", dv)))),
  "post75" = "Full")
dv_header = rep(1, length(dv_label)+1)
names(dv_header) = c(" ", dv_label)
dv_header_double = c(1, dv_header[-1]*2)
names(dv_header_double)[1] = " "

# Models
m_asoc = modnames(lapply(f_all, function(x) lm(x, data = data)))
m_asoc_cont = modnames(lapply(f_cont_all, function(x) lm(x, data = data)))

## Tables --------------------------------

# Base models (binary version)
modelsummary(
  models = m_asoc,
  output = "latex",
  vcov = lapply(m_asoc, function(x) vcovCL(x, cluster = data$prov)),
  estimate = "{estimate}{stars}",
  coef_map = coef_recode,
  coef_omit = "prov",
  gof_map = gs,
  title = "Francoist repression (TOP, binary version) and local associations formed after 1975\\label{tab:lm_asoc}",
  add_rows = as.data.frame(rbind(c("Province FE", rep("Yes", length(m_asoc))))),
  threeparttable = TRUE, escape = FALSE) %>%
add_header_above(dv_header) %>%
kable_styling(latex_options = c("scale_down", "hold_position")) %>%
footnote(general = n, threeparttable = TRUE, footnote_as_chunk = TRUE, escape = FALSE) %>%
save_kable(file = "analyses_local/output/tab_asoc.tex")

# Base models (continuous measure)
modelsummary(
  models = m_asoc_cont,
  output = "latex",
  vcov = lapply(m_asoc_cont, function(x) vcovCL(x, cluster = data$prov)),
  estimate = "{estimate}{stars}",
  coef_map = coef_recode,
  coef_omit = "prov",
  gof_map = gs,
  title = "Francoist repression (TOP, continuous form) and local associations formed after 1975\\label{tab:lm_asoc_cont}",
  add_rows = as.data.frame(rbind(c("Province FE", rep("Yes", length(m_asoc_cont))))),
  threeparttable = TRUE, escape = FALSE) %>%
add_header_above(dv_header) %>%
kable_styling(latex_options = c("scale_down", "hold_position")) %>%
footnote(general = paste(n, n_cont), threeparttable = TRUE,
  footnote_as_chunk = TRUE, escape = FALSE) %>%
save_kable(file = "analyses_local/output/tab_asoc_cont.tex")


## Coefficient plots --------------------------------

coefs_asoc = bind_rows(
  do.call(bind_rows, lapply(m_asoc, function(x) my_tidy(x) %>%
    filter(grepl("^top", term)))),
  do.call(bind_rows, lapply(m_asoc_cont, function(x) my_tidy(x) %>%
    filter(grepl("^top", term))))) %>%
  mutate(dv = rep(gsub("(19|20)(\\d\\d)-", "\\1\\2\n\\1", dv_label), 2)) %>%
  mutate(dv = gsub("1900", "2000", dv)) %>%
  filter(dv != "Full") %>%
  select(dv, term, est = estimate, se = std.error) %>%
  mutate(
    upr = est + se * qnorm(0.975),
    lwr = est - se * qnorm(0.975),
    upr90 = est + se * qnorm(0.95),
    lwr90 = est - se * qnorm(0.95)) %>%
  mutate(top = recode(term,
    "top_bin" = "IV: Binary measure of repression",
    "top_p" = "IV: Continuous measure of repression"))

p = ggplot(coefs_asoc,
    aes(x = dv, y = est)) +
  geom_errorbar(aes(ymin = lwr90, ymax = upr90), width = 0, linewidth = 1.1,
    position = position_dodge(width = 1/3)) +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = 0,
    position = position_dodge(width = 1/3)) +
  geom_point(fill = "white", shape = 21, position = position_dodge(width = 1/3)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(x = "", #\nDependent variable (associations formed during time periods)",
    y = "Coefficient and 95/90% CIs\n") +
  facet_wrap(~top, ncol = 2) +
  theme(
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    legend.position = "none")
ggsave("analyses_local/output/fig_coefs_asoc.pdf",
  width = 7.5, height = 3, device = "pdf")