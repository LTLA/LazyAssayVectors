#' @export
createLazyRows <- function(x) {
    if (is.null(rownames(x))) {
        stop("'rownames(x)' cannot be NULL")
    }

    FUN <- .make_lazy_function(x, getcol=FALSE) 
    assay_vals <- vector("list", nrow(x))
    for (i in seq_along(assay_vals)) {
        assay_vals[[i]] <- FUN(i)
    }
    names(assay_vals) <- rownames(x)
}
