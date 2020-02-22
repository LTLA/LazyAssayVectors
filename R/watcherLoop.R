watcherContents  <- new.env()
watcherContents$collection <- list()

watcherLoop <- function(dir=commandArgs(trailingOnly=TRUE)) {
    # Much of this is copied from parallel:::.watcherPSOCK or
    # parallel:::watcherLoop.
    repeat tryCatch({
        delay <- 0.1
        counter <- 1L

        repeat {
            if (!dir.exists(dir)) {
                return(invisible(NULL))
            }
            all.files <- list.files(dir, pattern="\\.")
            if (length(all.files)) {
                break
            }

            # Keep polling. If it's been 30 seconds since we saw a change,
            # the delay is bumped up to 1 second; this gets reset whenever
            # we do see a change.
            Sys.sleep(delay)
            counter <- counter + 1L
            if (counter*delay > 30) {
                delay <- 1
            }
        }

        # Scanning through the RDS files, representing serialized matrices.
        mats <- all.files[grep("\\.rds$", all.files)]
        for (x in mats) {
            id <- sub("\\.rds$", "", x)
            full <- file.path(dir, x)
            curmat <- readRDS(full)
            watcherContents$collection[[id]] <- curmat

            pkg <- attr(class(curmat), "package")
            if (!is.null(pkg)) {
                requireNamespace(pkg, quietly=TRUE)
            }
            unlink(full)
        }

        # Checking for new input requests and processing them.
        ins <- all.files[grep("\\.in$", all.files)]
        for (x in ins) {
            id <- sub("\\.in$", "", x)
            curmat <- watcherContents$collection[[id]]
            full <- file.path(dir, x)

            codes <- readBin(full, integer(), 2)
            i <- codes[1] + 1L
            j <- codes[2] + 1L

            target <- try({
                if (i>0L && j>0L) {
                    curmat[i,j]
                } else if (i>0L) {
                    curmat[i,]
                } else {
                    curmat[,j]
                }
            }, silent=TRUE)

            if (is(target, "try-error")) {
                target <- raw(0)
            }
            out <- file.path(dir, paste0(id, ".out"))
            writeBin(con=out, target)
            unlink(full)
        }
    }, interrupt = function(e) NULL)
}
