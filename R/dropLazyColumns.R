#' Drop lazy columns
#'
#' Drop columns of a data.frame to only retain non-lazy vectors or lazy vectors that have already been mterialized.
#'
#' @param x A data.frame containing any number of lazy vectors.
#'
#' @return A data.frame containing only the materialized columns of \code{x}.
#'
#' @details
#' \code{dropLazyColumns} slims down a data.frame of lazy vectors to retain only the columns that were actually used.
#' The idea is to run this function at the end of a sequence of operations on the data.frames 
#' produced by \code{\link{createLazyRows}} or \code{\link{createLazyColumns}},
#' thereby avoiding inadvertent materialization of all columns when saving the workspace.
#'
#' @author Aaron Lun
#' @examples
#' Y <- matrix(rnorm(1000), ncol=10)
#' colnames(Y) <- LETTERS[1:10]
#'
#' df <- createLazyColumns(Y)
#' colnames(df)
#' head(df$A) # used 'A'
#' tail(df$E) # used 'E'
#'
#' dropped.df <- dropLazyColumns(df)
#' colnames(dropped.df)
#' 
#' @export 
dropLazyColumns <- function(x) {
    x[,check_lazy_vectors(x),drop=FALSE]
}
