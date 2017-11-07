#' Customized Exception Handling
#' 
#' The functions provide interfaces to raise customized errors, warnings, or
#' messages.
#' 
#' @param ... zero or more objects which can be coerced to character or a
#'   single condition, see \code{\link[base]{stop}()},
#'   \code{\link[base]{warning}()}, or \code{\link[base]{message}()} for
#'   details
#' @param subclass \emph{unique} name(s) of subclass(es) of the customized
#'   conditions
#' 
#' @seealso \code{\link[base]{stop}()}, \code{\link[base]{warning}()}, or
#'   \code{\link[base]{message}()} for basic handling of errors, warnings, or
#'   messages, respectively
#'   
#'   \code{\link[base]{signalCondition}()} for signalling a condition
#' 
#' @name exceptionHandling
NULL



#' @describeIn exceptionHandling raises a customized error and
#'   \link[base:stop]{stops} execution of current expression
#' 
#' @param call. passed to \code{\link[base]{stop}} or
#'   \code{\link[base]{warning}}, for details see references therein
#' @param domain passed to \code{\link[base]{stop}},
#'   \code{\link[base]{warning}} or \code{\link[base]{message}}, for details
#'   see references therein
#' 
#' @export
custom_stop <- function(..., subclass = NULL, call. = TRUE, domain = NULL) {
  
  if (is.condition(list(...)[[1]])) {
    e <- list(...)[[1]]
    extend_class(e) <- 'error'
  } else {
    e <- simpleError(paste0(...), sys.call(-1))
  }

  extend_class(e) <- subclass
  stop(e)
}



#' @describeIn exceptionHandling raises a customized warning and generates
#'   the corresponding \link[base:warning]{warning message}
#' 
#' @param immediate. passed to \code{\link[base]{warning}}, for details see
#'   references therein
#' @param noBreaks. passed to \code{\link[base]{warning}}, for details see
#'   references therein
#' 
#' @export
custom_warning <- function(..., subclass = NULL, call. = TRUE,
                           immediate. = FALSE, noBreaks. = FALSE,
                           domain = NULL) {
  if (is.condition(list(...)[[1]])) {
    w <- list(...)[[1]]
    extend_class(w) <- 'warning'
  } else {
    w <- simpleWarning(paste0(...), sys.call(-1))
  }
  
  extend_class(w) <- subclass
  warning(w)
}



#' @describeIn exceptionHandling raises a customized message and shows the 
#'   corresponding \link[base:message]{diagnostic message}
#' 
#' @param appendLF passed to \code{\link[base]{message}}, for details see
#'   references therein
#' 
#' @export
custom_message <- function(..., subclass = NULL, domain = NULL,
                           appendLF = TRUE) {
  if (is.condition(list(...)[[1]])) {
    m <- list(...)[[1]]
    extend_class(m) <- 'message'
  } else {
    m <- simpleMessage(paste0(...), sys.call(-1))
  }

  extend_class(m) <- subclass
  message(m)
}



#' @describeIn exceptionHandling signals a customized condition. Note in this
#'   case nothing will happen unless a custom signal handler was instantiated
#'   (e.g. using \code{\link[base]{tryCatch}()} or
#'   \code{\link[base]{withCallingHandlers}()})
#' @param message character string
#' @param call call expression, see \code{\link[base]{simpleCondition}} for
#'   details
#' @param type character string, type of the condition
#' 
#' @details For signalling customized condtions using
#'   \code{custom_signalCondition} no exception handling is invoked even for
#'   types \code{"error"}, \code{"warning"}, or \code{"message"}, i.e. nothing
#'   will happen unless a signal handler is invoked (e.g. using
#'   \code{\link[base]{tryCatch}()} or
#'   \code{\link[base]{withCallingHandlers}()})
#' 
#' @export
custom_signalCondition <- function(subclass = NULL, message,
                                   call = sys.call(-1),
                                   type = c('condition', 'error', 'warning',
                                            'message')) {
  type <- match.arg(type)

  if (is.condition(message)) {
    c <- message
    extend_class(c) <- type
  } else {
    c <- switch(type,
                condition = simpleCondition(message, call),
                error = simpleError(message, call),
                warning = simpleWarning(message, call),
                message = simpleMessage(message, call))
  }
  extend_class(c) <- subclass
  signalCondition(c)
}



#' @describeIn exceptionHandling provides a generating function for customized
#'   exception handlers
#' 
#' @details 
#' 
#' @return The function \code{exceptionHandler} returns a function with
#'   arguments \code{...} (see arguments above for a description). Customized
#'   exception handlers invoke the basic functions \code{\link[base]{stop}()},
#'   \code{\link[base]{warning}()}, or \code{\link[base]{message}()}. Note that
#'   the customized exception handler lacks any optional arguments of these
#'   functions.
#' 
#' @export
custom_exceptionHandler <- function(subclass = NULL,
                                    type = c('condition', 'error', 'warning',
                                             'message')) {
  type <- match.arg(type)
  c_type <- switch(type,
                   condition = simpleCondition,
                   error     = simpleError,
                   warning   = simpleWarning,
                   message   = simpleMessage)
  c_handler <- switch(type,
                      condition = signalCondition,
                      error     = stop,
                      warning   = warning,
                      message   = message)
  
  function(...) {
    if (is.condition(list(...)[[1]])) {
      message <- list(...)[[1]]
    } else {
      message <- paste0(...)
    }
    
    c <- c_type(message, call = sys.call(-1))
    c_handler(c)
  }
}

