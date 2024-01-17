# There is a bug in Lolly 1.42 which changes the output date format.
# This script searches for CSV files in the incorrect format and converts them to the standard TOMST output format.
# The original file is preserved with the name <filename>_orig.csv.

library(stringr)
library(purrr)
library(progress)

#                         ;datetime                                           ;    ;T1             ;T2             ;T3             ;mois;    ;
bug_row_pattern <- "^(\\d+;)(\\d{2}).(\\d{2}).(\\d{4})( \\d{2}:\\d{2})?(:00)?(;\\d+;-?\\d+[.,]?\\d*;-?\\d+[.,]?\\d*;-?\\d+[.,]?\\d*;\\d+;\\d+;\\d+.*)$"

convert <- function(directory_path) {
    files <- list.files(directory_path, pattern=".+\\.csv$", full.names=TRUE)
    progress_bar <- progress_bar$new(format = "convert [:bar] :current/:total files",
                                     total=length(files))
    row_function <- function(row) {
        if(is.na(row[[6]])) {
            row[[6]] <- " 00:00"
        }
        result <- paste0(c(row[[2]], row[[5]], ".", row[[4]], ".", row[[3]], row[[6]], row[[8]]), collapse = "")
        return(result)
    }

    file_function <- function (filename) {
        con <- file(filename, "r")
        lines <- readLines(con)
        close(con)
        if(!all(str_detect(lines[1:10], bug_row_pattern))) {
            warning(str_glue("File {filename} is not in format to convert."))
            progress_bar$tick()
            return()
        }
        orig_name <- str_replace(filename, ".csv$", "_orig.csv")
        file.copy(filename, orig_name)
        match <- str_match(lines, bug_row_pattern)
        rows <- split(match, row(match))
        converted_lines <- map_chr(rows, row_function)
        writeLines(converted_lines, filename)
        progress_bar$tick()
    }

    walk(files, file_function)
}

# Set directory path with files in incorrect format to this variable.
directory_path <- "path/to/directory"
convert(directory_path)