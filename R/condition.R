#' Custom Conditions S3-Classes
#' 
#' The functions are wrapper for functions \code{\link[base]{simpleCondition}}
#' and \code{\link[base]{simpleError}} extending the classes with a custom
#' subclass. The resulting S3 class objects can be used to signal a custom
#' condition or, more specific, a custom error.
#' 
#' @param subclass character, \emph{unique} name(s) subclass(es) that
#'   extend the base classes \code{class}, see details in
#'   \code{\link{extend_class}()}
#' @param message character, condition message
#' @param call call stack,
#' 
#' @return The function returns an S3 class of type \code{subclass} inheriting
#'  from \code{error} (only for \code{error()}) and \code{condition}.
#' 
#' @seealso \code{\link[base]{simpleCondition}} and
#'   \code{\link[base]{simpleError}} for general details on condition handling
#' 
#' @export
condition <- function(subclass = NULL, message, call = sys.call(-1)) {
  c <- simpleCondition(message, call = call)
  extend_class(c) <- subclass
  return(c)
}




#' @rdname condition
#' @export
error <- function(subclass = NULL, message, call = sys.call(-1)) {
  e <- simpleError(message, call = call)
  extend_class(e) <- subclass
  return(e)
}





#' @rdname condition
#' @description Functions \code{is.condition()}, \code{is.error()},
#'  \code{is.warning()}, and \code{is.tryError()} check for inheritance from
#'  abstract S3 class \code{condition}, \code{error}, \code{warning}, or
#'  \code{try-error} respectively. The latter can be used to test results of
#'  \code{\link[base]{try}()} to handle error-recovery.
#' 
#' @param x an object to check for inheritance
#' @export
is.condition <- function(x) inherits(x, 'condition')





#' @rdname condition
#' @export
is.error <- function(x) inherits(x, 'error')





#' @rdname condition
#' @export
is.tryError <- function(x) inherits(x, 'try-error')





#' @rdname condition
#' @export
is.warning <- function(x) inherits(x, 'warning')
