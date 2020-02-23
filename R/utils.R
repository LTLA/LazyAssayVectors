#' @importClassesFrom Matrix dgCMatrix lgCMatrix
.make_lazy_function <- function(x, getcol) {
    d <- dim(x)
    force(getcol)
    
    if (is.matrix(x)) {
        matclass <- 0L
        type <- .pack_to_type(x)
    } else if (is(x, "dgCMatrix")) {
        matclass <- 1L
        type <- 1L
    } else if (is(x, "lgCMatrix")) {
        matclass <- 1L
        type <- 2L
    } else {
        matclass <- 2L
        type <- .pack_to_type(as.matrix(x[0,0]))

        # Yes, we are replacing the matrix with a list of stuff!
        # This will be used for retrieval in the ALTREP method.
        # We add a 5 second time-out limit for retrieval; beyond
        # that, we assume that the process died somewhere.
        x <- list(location=saveOther(x), timeout=5)
    }

    function(i) {
        create_lazy_vector(x, d, i-1L, getcol=getcol, matclass=matclass, type=type)
    }
}

.pack_to_type <- function(x) {
    switch(typeof(x), 
        integer=0L,
        double=1L,
        logical=2L,
        default=-1L)
}
