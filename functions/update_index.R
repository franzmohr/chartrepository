

update_index <- function() {
  
  file_list <- list.files("scripts")
  
  pb <- txtProgressBar(style = 3)
  result <- NULL
  for (i in 1:length(file_list)) {
    temp <- readLines(paste0("scripts/", file_list[i]), n = 1)
    temp <- gsub("#", "", temp)
    temp <- gsub(" ", "", temp)
    temp <- unlist(strsplit(temp, ","))
    temp <- data.frame(tag = temp, file = file_list[i])
    result <- dplyr::bind_rows(result, temp)
    setTxtProgressBar(pb, i / length(file_list))
  }
  
  saveRDS(result, file = "tagindex.rds") 
  
}
