lazyRegistry <- new.env()

#' Saving other matrices
#'
#' Serializing other (non-ordinary, non-sparse) matrices for processing with \code{\link{watcherLoop}}.
#'
#' @param x A matrix-like object.
#'
#' @details
#' This function calls \code{\link{saveRDS}} to transfer \code{x} to the child process running \code{\link{watcherLoop}}.
#' To save time, no action will be performed if \code{x} was the same as the last transferred object,
#' based on the last transferred entity in \code{lazyRegistry}.
#'
#' @return String containing the file prefix corresponding to the transferred matrix.
#' This will be used by ALTREP methods to retrieve the results of subsetting.
#'
#' @author Aaron Lun
#'
#' @rdname INTERNAL_saveOtherMatrix
saveOtherMatrix <- function(x) {
    candidate <- getWatcherLocation(start=TRUE)

    # Avoids constantly resaving and reloading objects 
    # if we're making lots of queries to the same thing.
    lastObject <- lazyRegistry$lastObject
    if (identical(x, lastObject)) {
        ID <- lazyRegistry$lastID
    } else {
        ID0 <- tempfile(tmpdir=candidate)
        saveRDS(x, paste0(ID0, ".rds"))
        ID <- basename(ID0)

        lazyRegistry$lastObject <- x
        lazyRegistry$lastID <- ID
    }

    file.path(candidate, ID)
}
