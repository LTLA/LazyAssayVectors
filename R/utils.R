#' @importClassesFrom Matrix dgCMatrix lgCMatrix
.make_lazy_function <- function(x, getcol) {
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
    }

    d <- dim(x)
    force(getcol)
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

.launch_process <- function() {
    candidate <- tempfile()
    dir.create(candidate)
    rscript <- file.path(R.home("bin"), "Rscript")
    system2(rscript, c("-e", "'LazyAssayVectors:::slaveLoop()'", shQuote(candidate)), wait=FALSE)
    candidate
}
