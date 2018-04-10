#' CSO Extract Category Clean
#'
#' Takes a data frame that is data loaded from a CSO.ie .csv file, and the number of rows of the file for each category; a cleaned version of the dataset is returned.
#' Cleaning involves removing the header rows that are inserted every \code{category.num.rows} number of rows and inserts these headers as a new set of variables: \code{category.code} and \code{category.name}. The Year-Quarter variable is also split into separate year and quarter variables. This cleaned & modified data frame is returned.
#' @author Cormac Nolan
#' @param cso.data Data frame that is in the format that the CSO.ie website provides.
#' @param category.num.rows The number of rows between each category must be provided.
#' @return Data frame with cleaned data.
#' @export

cso_category_clean <- function(cso.data, category.num.rows){

  require(dplyr)

  # Create index for filtering
  heading.index <- !vector(mode = "logical", length = nrow(cso.data))

  heading.index[seq(1, nrow(cso.data), by = category.num.rows)] <- FALSE

  # grab the headings (categories) before filtering.
  headings <- cso.data[seq(1, nrow(cso.data), by = category.num.rows),1]

  # Insert categories into new variable
  cso.data$category <- rep(headings, each = category.num.rows)

  # Filter out the category header rows and split the category into code and
  # name variables.
  cso.data <-
    cso.data %>%
    filter(heading.index) %>%
    separate(category, c("category.code", "category.name"), sep = " ,")

  return(cso.data)
}
