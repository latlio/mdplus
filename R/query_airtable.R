#' Queries Airtable
#'
#' This function queries the MD+ Airtable Directory.
#' @param sheet_url URL of Airtable Google sheet
#' @return Airtable tibble
#' @importFrom janitor clean_names
#' @importFrom googlesheets4 read_sheet
#' @export
#' @examples
#' query_airtable()

query_airtable <- function(sheet_url = "https://docs.google.com/spreadsheets/d/1dCOcNwOYZDQXeyAHnc5ypDyaKmwm0OOKWq5nFAWR924/edit#gid=417127965") {
  options(gargle_oauth_email = "lathanliu21@gmail.com")
  read_sheet(sheet_url) %>%
    clean_names()
}
