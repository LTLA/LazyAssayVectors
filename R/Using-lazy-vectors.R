#' Comments on using lazy vectors
#'
#' @description
#' Lazily materialized vectors are best suited for use in situations
#' where we need to create vectors for all possible rows/columns but 
#' only a small fraction of those vectors will ultimately be used.
#' The most concrete example is that of \pkg{ggplot2} where the initial
#' \code{ggplot} call requires a data.frame containing all possible 
#' fields, of which only a few are used for the final visualization.
#' Lazy vectors allow this initial data.frame to be constructed very
#' cheaply, avoiding the need to explicitly specify the desired fields
#' in the data.frame (and again in the various \pkg{ggplot2} layers).
#'
#' Lazy vectors are very compatible with downstream R machinery,
#' materializing as ordinary contiguous vectors upon first use. 
#' This is achieved via the ALTREP system that allows us to defer the
#' materialization of each vector until it is explicitly requested.
#' Thus, even though functions like \code{\link{createLazyRows}}
#' and \code{\link{createLazyColumns}} might appear to break sparsity or
#' exceed memory limits, they do not actually do so (see Examples).
#' 
#' The major point of concern when using lazy vectors (or indeed, any 
#' object with significant ALTREP components) is that any attempt to save
#' an object containing many such vectors will trigger materialization 
#' of all of them.
#' This includes explicit saves with \code{\link{saveRDS}} or implicit
#' saves with \code{\link{save}}, via \pkg{knitr} caching, etc.
#' Full materialization is obviously undesirable as all memory and speed
#' advantages of having a lazy representation are lost.
#' Where possible, we suggest running \code{\link{dropLazyColumns}}
#' to eliminate unused lazy vectors from a data.frame so that it will
#' not choke in any saving steps that might occur later.
#' 
#' @author Aaron Lun
#'
#' @examples
#' # Normally, this matrix would require several TB of RAM in dense form:
#' library(Matrix)
#' y <- rsparsematrix(1e6, 1e6, density=0.0000001)
#' rownames(y) <- sprintf("Gene_%i", seq_len(nrow(y)))
#'
#' # This is possible, despite appearing to require ridiculous RAM usage:
#' df <- createLazyRows(y)
#' object.size(df)
#' 
#' @name Using-lazy-vectors 
NULL
