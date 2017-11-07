#' Dump Evalutaion Environments on Errors
#' 
#' The function sets the error handling options to dumping the evaluation
#' environments for post-mortem debugging (see \code{\link[utils]{debugger}}
#' for details).
#' 
#' @param dumpto character; The name of the object or file to dump to. See
#'  For default values see Details-section below and \code{dumpto} argument of
#'  the \code{\link[utils]{dump.frames}()} function.
#' @param dump.once logical; If \code{TRUE} the error handling options are 
#'  reset after an error was raised.
#' @param to.file logical; If \code{TRUE} the dump is to a file rather than
#'  to an \R object.
#' @param quit.on.error logical; If \code{TRUE} \R is quit on an error.
#' 
#' @details The default value for \code{dumpto} is given by the
#'  \link[=exceptionHandlerOptions]{package option} \code{"dumpTo"}. If
#'  \code{to.file = TRUE}, the file's path is given by the
#'  \link[=exceptionHandlerOptions]{package option} \code{"dumpPath"}. If
#'  the path does not exist, the current working directory is used instead.
#' 
#' @seealso
#'  See \code{\link[utils]{debugger}} for details on post-mortem debugging.
#'  
#'  See \code{\link[base]{options}("error")} for details on error handling
#'  options.
#' 
#' @examples
#' \dontrun{
#' dumpOnError('testdump')
#' 
#' f <- function() {
#'  g <- function() stop('test dumpOnError')
#'  g()
#' }
#' 
#' f()    # will generate a dump on file 'testdump.rda'
#' 
#' ## possibly in another R session
#' load('testdump.rda')
#' debugger(testdump)
#' }
#' @export
dumpOnError <- function(dumpto, dump.once = TRUE, to.file = TRUE,
                        quit.on.error = FALSE) {
  if (missing(dumpto)) dumpto <- exceptionHandlerOptions$get('dumpTo')
  
  to.path <- exceptionHandlerOptions$get('dumpPath')
  if (!dir.exists(to.path)) to.path <- getwd()
  
  .dump <- function() {
    if (dump.once) options(.opts)
    if (to.file) to.path <- setwd(to.path)
    utils::dump.frames(dumpto, to.file = to.file)
    if (to.file) setwd(to.path)
    if (quit.on.error) q(status = 1)
  }
  
  .opts <- options(error = .dump)
  invisible(.opts)
}
