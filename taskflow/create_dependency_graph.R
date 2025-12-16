if(!"stringr" %in%
  rownames(installed.packages())){install.packages("stringr")}
library(stringr)

# -------------------------

# Load makefile
l = readLines(file("Makefile"))

# Remove the "|"
l = gsub(" \\| ", "", l)

# Replace functions
funcs = l[grepl("^[a-z].* = ", l)]
if(length(funcs) > 0){
  funcs = str_split(funcs, " = ")
  funcs_name = sapply(funcs, function(x) paste0("\\$\\(", x[1], "\\)"))
  funcs_files = sapply(funcs, function(x) x[2])
  for(i in 1:length(funcs_name)){l = gsub(funcs_name[i], funcs_files[i], l)}
}

# Remove comments
l = l[!grepl("^#", l)]

# Select lines with target & dependencies
l = l[grepl(".*/.*: .*/", l)]

# Get targets
t = gsub("/.*", "", l)

# Get dependencies
d = lapply(str_split(gsub(".*: ", "", l), " "),
  function(x) gsub("/.*", "", x))

# Repeat targets
t = rep(t, unlist(lapply(d, function(x) length(x))))

# Create matrix with repeated targets
m = matrix(c(unlist(d), t), ncol = 2)

# Drop duplicates and same target > output
m = unique(m[m[,1] != m[,2],])

# Dependencies
d_lines = apply(m, 1, paste, collapse = "->")

# [>>> OPTIONAL <<<]
# Remove from graph dependencies related to input/ or func/
d_lines = d_lines[!grepl("^input|func", d_lines)]

# Write file
writeLines(c("digraph G {", d_lines, "}"), file("taskflow/dependency_list.txt"))
