# load libraries
# Project
library(tidyverse)
library(jsonlite)

# base strings:
part_1 <- "https://api.themoviedb.org/3/movie/"
part_3 <- "?api_key=4bc51a932fcb6f2cb7b523ea3902f439"

all_data <- data.frame()
for(i in 1:10000) {
  complete_url <- paste0(part_1, i, part_3)
  
  # read data in
  this_data_frame <- tryCatch({
    fromJSON(complete_url)  %>%
      unlist() %>%
        data.frame() %>%
          rownames_to_column() %>%
            rename(value = ".") %>%
              pivot_wider(names_from = rowname,
                          values_from = value)
  }, error = function(e) {
    return(NA)
  })
  
  if (length(this_data_frame) > 1){
    # combine this data frame with everything
    all_data <- bind_rows(all_data,
                        this_data_frame)
  }
}

write_csv(all_data, "data/themoviedb-api-data.csv")