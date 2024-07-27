
# Runs all scripts to produce updates of each chart

file_list <- list.files("scripts")
file_list <- file_list[grepl(".R", file_list, fixed = TRUE)]
lapply(file_list, function(x) {source(paste0("scripts/", x))})
