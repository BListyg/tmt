#' Read survey data into a DataFrame
#'
#' Skip the second row which contains the survey questions
#'
#' Thanks to https://stackoverflow.com/a/15860268/3420371
#'
#' @param filename Path to the CSV file to read
read_survey <- function(filename) {

  # read the entire file into a list
  all_content <- readLines(filename)

  # discard the second item
  skip_second <- all_content[-2]

  # load into a dataframe
  data <-
    read.csv(textConnection(skip_second),
             header = TRUE,
             stringsAsFactors = FALSE,
             na.strings = c('')
             )
}
