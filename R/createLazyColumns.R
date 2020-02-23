#' Create a data.frame of lazy columns
#'
#' Create a data.frame where each column is a lazily materialized column from a matrix or matrix-like object.
#'
#' @param x A matrix or matrix-like object containing integer, logical or double-precision values.
#'
#' @return
#' A data.frame where each column is named after and contains all values from a column of \code{x}.
#' Each row of the data.frame corresponds to and is named after a row of \code{x}.
#'
#' @author Aaron Lun
#'
#' @details 
#' Users should generally not call this function directly,
#' rather it is a utility that is most typically used by other functions.
#' This is because direct use of the output data.frame requires some care to avoid unintended materialization,
#' see \code{?"\link{Using-lazy-vectors}"} for more details.
#'
#' @examples
#' # Creating a large sparse matrix:
#' library(Matrix)
#' y <- rsparsematrix(20000, 1000, density=0.01)
#' colnames(y) <- sprintf("Cell_%i", seq_len(ncol(y)))
#'
#' # This does not collapse sparsity:
#' df <- createLazyColumns(y)
#' summary(df$Cell_5)
#' 
#' @export
createLazyColumns <- function(x) {
    if (is.null(colnames(x))) {
        stop("'colnames(x)' cannot be NULL")
    }

    FUN <- .make_lazy_function(x, getcol=TRUE) 
    assay_vals <- lapply(seq_len(ncol(x)), FUN)
    names(assay_vals) <- colnames(x)

    class(assay_vals) <- "data.frame"
    row.names(assay_vals) <- if (!is.null(rownames(x))) {
        rownames(x)
    } else {
        seq_len(nrow(x))
    }

    assay_vals
}
