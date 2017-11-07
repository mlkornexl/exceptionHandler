#' Signal Custom Conditions
#' 
#' The functions signal a \link[=condition]{custom condition}.
#' 
#' @param subclass character, the name of the condition
#' 
#' @return The functions return functions of a single argument \code{c}, which
#'  will signal a custom condition \code{c} of class \code{subclass}.
#' 
#' @seealso See \code{\link{condition}} for details on custom conditions.
#' 
#' @export
signalCustomCondition <- function(subclass) {
  extend_class(c) <- subclass
  function(c) {
    extend_class(c) <- subclass
    signalCondition(c)
  }
}

#' @describeIn signalCustomCondition signals a custom \emph{error}, i.e. a
#'  custom condition that inherits from subclasses \code{error} and
#'  \code{simpleError}
#' @export
signalCustomError <- function(subclass) {
  signalCustomCondition(c(subclass, 'simpleError', 'error'))
}



#' @describeIn signalCustomCondition signals a custom \emph{warning}, i.e. a
#'  custom condition that inherits from subclasses \code{warning} and
#'  \code{simpleWarning}
#' @export
signalCustomWarning <- function(subclass) {
  signalCustomCondition(c(subclass, 'simpleWarning', 'warning'))
}


#' @describeIn condition raises an \code{\link[base]{error}} of custom class
#'   \code{subclass}
#' @export
custom_stop <- function(subclass, message, call = sys.call(-1), ...) {
  c <- condition(c(subclass, 'error'), message, call = call, ...)
  stop(c)
}



#' @describeIn condition raises an \code{\link[base]{warning}} of custom class
#'   \code{subclass}
#' @export
custom_warning <- function(subclass, message, call = sys.call(-1), ...) {
  w <- condition(c(subclass, 'warning'), message, call = call, ...)
  warning(w)
}

