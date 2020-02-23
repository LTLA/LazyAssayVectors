#' Create a data.frame of lazy rows
#'
#' Create a data.frame where each column is a lazily materialized row from a matrix or matrix-like object.
#'
#' @param x A matrix or matrix-like object containing integer, logical or double-precision values.
#'
#' @return
#' A data.frame where each column is named after and contains all values from a row of \code{x}.
#' Each row of the data.frame corresponds to and is named after a column of \code{x}.
#'
#' @author Aaron Lun
#'
#' @details 
#' Users should generally not call this function directly,
#' rather it is a utility that is most typically used by other functions.
#' This is because direct use of the output data.frame requires some care to avoid unintended materialization,
#' see \code{?"\link{Using lazy vectors}"} for more details.
#'
#' @examples
#' # Creating a large sparse matrix:
#' library(Matrix)
#' y <- rsparsematrix(20000, 1000, density=0.01)
#' rownames(y) <- sprintf("GENE_%i", seq_len(nrow(y)))
#'
#' # This does not collapse sparsity:
#' df <- createLazyRows(y)
#' summary(df$GENE_2)
#' 
#' @export
createLazyRows <- function(x) {
    if (is.null(rownames(x))) {
        stop("'rownames(x)' cannot be NULL")
    }

    FUN <- .make_lazy_function(x, getcol=FALSE) 
    assay_vals <- lapply(seq_len(nrow(x)), FUN)
    names(assay_vals) <- rownames(x)

    class(assay_vals) <- "data.frame"
    row.names(assay_vals) <- colnames(x)
    assay_vals
}
