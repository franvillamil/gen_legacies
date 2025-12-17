# setwd("~/Documents/Projects/gen_legacies")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "ggplot2", "mapSpain", "sf")
  # "paletteer", "maptools", "scales")
# Checks if they are installed, install if not
if (length(setdiff(pkg, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(pkg, rownames(installed.packages())))}
# Load
invisible(lapply(pkg, library, character.only = TRUE))

## PREPS ------------------------------------

# Functions
source("func/misc.R")

# ggplot defaults: geoms theme
theme_set(my_theme())

# Data, base
data = read.csv("dataset_local/output/data.csv")

## MAPS ------------------------------------

# Load shapefile
shp = st_read("input/ESP_adm4_1960_2011.shp", "ESP_adm4_1960_2011")

# Moving the Canary Islands
crs = st_crs(shp)
shp_mainland = shp[!shp$NAME_2 %in% c("santa cruz de tenerife", "las palmas"),]
shp_ci_raw = shp[shp$NAME_2 %in% c("santa cruz de tenerife", "las palmas"),]
shp_ci = esp_move_can(shp_ci_raw, moveCAN = TRUE)
shp = rbind(shp_mainland, shp_ci)

# Add variables: in sample
pt = table(data$prov, is.na(data$asoc_post75))
data$insample = ifelse(data$prov %in% rownames(pt)[pt[,1] == 0],
  FALSE, TRUE)
shp$in_sample = data$insample[match(shp$muni_code, data$muni_code)]
# Add variables: TOP
shp$top_bin = data$top_bin[match(shp$muni_code, data$muni_code)]

# Get subdivisions et al
ccaa = esp_get_ccaa()
provs = esp_get_prov()
box = esp_get_can_box()
line = esp_get_can_provinces()

# Get submaps for layers
top = subset(shp, top_bin == 1)
no_top = subset(shp, top_bin == 0)
notinsample = subset(shp, !in_sample)

# Plot
map = ggplot() +
  geom_sf(data = top,
    fill = "#fa3232", color = NA, linewidth = 0) +
  geom_sf(data = no_top,
    fill = "white", color = NA, linewidth = 0) +
  geom_sf(data = shp, linewidth = 0.05,
    color = "gray",  fill = NA)+
  geom_sf(data = notinsample,
    fill = "grey", color = "grey", size = 0.01)+
  labs(x = "", y = "") +
  geom_sf(data = provs,  
    color = "gray50", fill = NA, linewidth = 0.2) +
  geom_sf(data = box,  
    color = "slategray", fill = NA, size = 0.7) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank())

# Saving
ggsave("desc_local/output/map_top_reduced.pdf", device = "pdf")

# # find ./desc_local/output -name "map*" -exec pdfcrop {} {} \;
# system2("pdfcrop", args = c(
#   "/Users/franvillamil/Documents/Projects/gen_legacies/desc_local/output/map_top_reduced.pdf",
#   "/Users/franvillamil/Documents/Projects/gen_legacies/desc_local/output/map_top_reduced.pdf"))

## ASOCIACIONES OVER TIME ------------------------------------

asoc = read.csv("input/asoc.csv") %>%
  filter(!is.na(date) & date != "1111-01-01") %>%
  rename(dateraw = date) %>%
  mutate(date = as.Date(dateraw)) %>%
  filter(date < "2022-01-01") %>%
  filter(prov != "valladolid") %>%
  mutate(year = as.integer(format(date, "%Y")))
asoc$prov = capitalize(asoc$prov)

# Assocs over time, since 1966
p = ggplot(subset(asoc, year >= 1966), aes(x = year)) +
  geom_histogram(binwidth = 1, position = "dodge2", color = "white") +
  scale_x_continuous(breaks = seq(1966, 2020, 3)) +
  labs(#caption = "(Starting in 1966 to armonize regional datasets)",
    x = "", y = "") +
  theme(
    legend.title = element_blank(), legend.position = c(0.93,0.15),
    plot.subtitle = element_text(size = 10),
    plot.caption=element_text(size = 8, hjust=0))
ggsave("desc_local/output/hist_asoc.pdf", height = 4, width = 8, device = "pdf")
