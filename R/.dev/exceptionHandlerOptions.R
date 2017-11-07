#' Options for Exception Handling
#' 
#' @section Options Used for Package \code{exceptionHandler}:
#' \describe{
#'  \item{\code{dumpTo}}{default name of the object of file to dump evaluation
#'    environments in case of an error; Default is
#'    \code{"exceptionHandlerDump"}. See \code{\link{dumpOnError}()} for
#'    details.}
#'  \item{\code{dumpPath}}{path to dump evaluation environments on errors;
#'    Default for \code{dumpPath} is the working directory when loading the
#'    package. See \code{\link{dumpOnError}()} for details. \emph{Note:} The
#'    path should exist and \emph{must} be writeable.}
#' }
#' 
#' @format An object of S3 class \code{\link[packageOptions]{PackageOptions}}.
#' 
#' @export
#' @keywords list
exceptionHandlerOptions <- packageOptions::packageOptions(
  dumpPath = getwd(),
  dumpTo   = 'exceptionHandlerDump'
)
