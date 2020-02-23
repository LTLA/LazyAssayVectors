startWatcher <- function() {
    candidate <- tempfile()
    lazyRegistry$location <- candidate
    dir.create(candidate)

    rscript <- file.path(R.home("bin"), "Rscript")
    system2(rscript, c("-e", "'LazyAssayVectors:::watcherLoop()'", shQuote(candidate)), wait=FALSE)
    candidate
}

stopWatcher <- function() {
    unlink(watchedValues$location, recursive=TRUE)
    invisible(NULL)
}

getWatcherLocation <- function(start=TRUE) {
    out <- lazyRegistry$location
    if (is.null(out)) {
        out <- startWatcher()
    }
    out
}

#' @export
restartWatcher <- function() {
    stopWatcher()
    startWatcher()
}