# setwd("~/Documents/Projects/gen_legacies")
options(stringsAsFactors = FALSE)
# List of packages
pkg = c("dplyr", "ggplot2", "rgdal", "rgeos", "sp", "classInt",
  "paletteer", "maptools", "scales")
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

## PREPS ------------------------------------

# Functions
source("func/misc.R")

# ggplot defaults: geoms theme
theme_set(my_theme())

# Data, base
data = read.csv("dataset_local/output/data.csv")

## MAPS ------------------------------------

# Load shapefile
shp = readOGR("input/ESP_adm4_1960_2011.shp", layer = "ESP_adm4_1960_2011")

# Changing Canary Islands
shpEA = spTransform(shp,CRS("+init=epsg:2163"))
ci = shpEA[shpEA$NAME_2 %in% c("santa cruz de tenerife", "las palmas"),]
ci = fix1(ci, c(-20.5,1,-2.5e05,2e06))
proj4string(ci) = proj4string(shpEA)
shpEA = shpEA[!shpEA$NAME_2 %in% c("santa cruz de tenerife", "las palmas"),]
shpEA = rbind(shpEA, ci)
shp2 = spTransform(shpEA, CRS("+init=epsg:4326"))

# Get whole country and provinces
c = gUnaryUnion(shp2)
p = gUnaryUnion(shp2, id = shp2@data$NAME_2)

# Mark provinces in sample
pt = table(data$prov, is.na(data$asoc_post75))
data$insample = ifelse(data$prov %in% rownames(pt)[pt[,1] == 0], FALSE, TRUE)
shp2$insample = data$insample[match(shp2$muni_code, data$muni_code)]
shp2$insample[is.na(shp2$insample)] = FALSE

# Add variables
shp2$top_bin = data$top_bin[match(shp2$muni_code, data$muni_code)]
# colsc = c("white", "#5999ff", "#fa3232", "#541111")
shp2$top_col = ifelse(shp2$top_bin == 0, "white", "#fa3232")

# Plot
pdf("desc_local/output/map_top_reduced.pdf", height = 10, width = 8)
# Base map, grey borders
plot(shp2, col = "grey", border = "grey", lwd = 0.25)
# Municipalities in the sample
plot(shp2[shp2$insample,], col = shp2$top_col[shp2$insample],
    border = "grey", lwd = 0.25, add = TRUE)
# Country profile (provinces)
plot(p, border = grey(0.5), lwd = 0.5, add = TRUE)
# Canary Islands box
segments(x0 = -1.85, y0 = 34.7, y1 = 36, col = grey(0.5), lwd = 2, lty = "dashed")
segments(x0 = -0.5, x1 = 3.8, y0 = 36.75, col = grey(0.5), lwd = 2, lty = "dashed")
segments(x0 = -1.85, y0 = 36, x1 = -0.5, y1 = 36.75, col = grey(0.5), lwd = 2, lty = "dashed")
dev.off()

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
