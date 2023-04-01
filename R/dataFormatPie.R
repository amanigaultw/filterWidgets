#' Formats a given categorical vector for use by filterPie
#'
#' @param rawVector input data; should be a categorical vector.
#'
#' @export
#'
#' @examples
#' rawVector <- sample(c("French", "German", "British", "American", "Canadian", "Dutch"),
#' 1000, replace=TRUE, prob=c(0.2, 0.3, 0.3, .1, .05, .05))
#'
#' dataFormatPie(rawVector)
dataFormatPie <- function(rawVector){
  temp <- table(rawVector, useNA = "ifany")
  formattedData <- list()
  for(i in seq_along(temp)){
    formattedData[[i]] <- list(value = unname(temp[i]), name = names(temp)[i])
  }
  formattedData
}
