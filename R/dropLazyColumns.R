#' Drop lazy columns
#'
#' Drop columns of a data.frame that correspond to yet-to-be-materialized ALTREP vectors.
#'
#' @param x A data.frame containing any number of lazy vectors.
#'
#' @return A data.frame containing only the materialized columns of \code{x}.
#'
#' @details
#' This function aims to slim down a data.frame of 
#' @author Aaron Lun
#' @examples
#' Y <- matrix(rnorm(1000), ncol=10)
#' colnames(Y) <- LETTERS[1:10]
#'
#' df <- createLazyColumns(Y)
#' colnames(df)
#' head(df$A + 1) # used 'A'
#' tail(df$E + 2) # used 'E'
#'
#' dropped.df <- dropLazyColumns(df)
#' colnames(dropped.df)
#' 
#' @export 
dropLazyColumns <- function(x) {
    x[,check_lazy_vectors(x),drop=FALSE]
}
