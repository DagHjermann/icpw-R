
#
# based on 
# https://stackoverflow.com/a/40141132/1734247
#

library(readr)
library(stringr)

extract_rmd <- function(file_path, output_file = NULL) {
  
  stopifnot(is.character(file_path) && length(file_path) == 1)
  if (is.null(output_file)){
    output_file <- tempfile(fileext = ".R")
  }
  .con <- file(output_file) 
  on.exit(close(.con))
  full_rmd <- read_file(file_path)
  codes <- str_match_all(string = full_rmd, pattern = "```(?s)\\{r[^{}]*\\}\\s*\\n(.*?)```")
  stopifnot(length(codes) == 1 && ncol(codes[[1]]) == 2)
  codes <- paste(codes[[1]][, 2], collapse = "\n")
  codes <- gsub("\r\n", "\n", codes)
  writeLines(codes, .con)
  flush(.con)
  cat(sprintf("R code extracted to tempfile: %s\nSourcing tempfile...", output_file))
  # source(output_file)

}

# debugonce(extract_rmd)
# extract_rmd("260parm_Time_series_results_James.Rmd", "260parm_Time_series_results_James.R")
