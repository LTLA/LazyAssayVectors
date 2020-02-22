watcherHistory <- new.env()

#' The watcher loop in the child
#'
#' This function implements a polling loop in a child process that checks the contents of \code{dir},
#' an agreed-upon directory known by both the parent and child.
#' It then reads in files and processes them based on their file extensions,
#' spitting out result files with the same prefix.
#' This provides a simple, portable method of communicating between the parent and child.
#'
#' @param dir String containing the path to the specified location.
#' Extracted from the command line to make it easier to launch with \code{startWatcher}.
#'
#' @details
#' This function will continually poll \code{dir} for new files, processing them as described below:
#' \itemize{
#' \item RDS files (ending with \code{".rds"}) are assumed to be serialized matrix-like objects.
#' These are read in with \code{readRDS} and stored in \code{watcherContents} according to the file name (sans extension).
#' For S4 classes, the originating package is also loaded into the namespace.
#' \item Binary input files (ending with \code{".in"}) are assumed to contain an integer vector of length 2,
#' specifying the row and column indices to use for subsetting a particular matrix.
#' The requested matrix is identified from the file name (sans extension) and extracted from \code{watcherContents}.
#' }
#'
#' In return, this function will create output binary files (ending with \code{".out"}) containing the extracted vector or scalar.
#' One such file is created for each \code{".in"} file and has the same file name prefix. 
#' Successful creation of the \code{".out"} file is indicated by the removal of the \code{".in"} file.
#'
#' To interact with this function, an ALTREP method is assumed to have a matrix ID (i.e., a file prefix).
#' It should then:
#' \enumerate{
#' \item Check that there is no existing \code{".in"} or \code{".out"} file for its matrix ID.
#' Poll until that is the case (to avoid overwriting content from other processes).
#' \item Save the request parameters (i.e., two integers) to a binary input file,
#' by simply appending \code{".in"} to the matrix ID.
#' \item Poll until the \code{".in"} file is deleted,
#' at which point it is possible to read the \code{".out"} file.
#' It is assumed that the ALTREP method knows the type and length of the binary content.
#' \item Delete the \code{".out"} file.
#' }
#'
#' \code{watcherContents} holds a history of all ingested matrices to handle situations where multiple processes might be simultaneously requesting content from different matrices.
#' This is possible under some parallelization schemes involving forks where a single process runing \code{watcherLoop} is responsible for handling requests from all forked child processes.
#' Note that this function will still handle each request in serial, but at least there will not be have race conditions.
#' 
#' The polling cycle for this function slows to a 1 second delay after 30 seconds of inactivity.
#' The ALTREP method is assumed to implement its own sensible time-outs in case this process is killed.
#' If \code{dir} is deleted at any point, this function will exit.
#'
#' @return
#' \code{NULL}, invisibly.
#' Files in \code{dir} are created and deleted as described above.
#'  
#' @author Aaron Lun
#' @rdname INTERNAL_watcherLoop
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

            all.files <- list.files(pattern="\\.(in|rds)$", recursive=TRUE)
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
            watcherHistory[[id]] <- curmat

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
            curmat <- watcherHistory[[id]]
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
