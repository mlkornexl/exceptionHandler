#' Apply a Function over a List of Vector with Exception Handling
#' 
#' The functions provide "error proof" versions of \code{\link[base]{lapply}},
#' \code{\link[base]{sapply}}, and \code{\link[base]{vapply}}. 
#' 
#' @param X a vector (atomic or list) \code{\link[base]{expression}}. Other
#'  objects (including class objects) will be coerced by
#'  \code{\link[base]{as.list}}.
#' @param FUN the function to be applied to each element of \code{X}, see
#'  'Details' section of \code{\link[base]{lapply}}.
#' @param ... optional arguments to \code{FUN}
#' @param DEFAULT default value to be returned if \code{FUN} returns an error
#' 
#' @details For details see 'Details' section of \code{\link[base]{lapply}}.
#' If the function \code{FUN}, applied to an element of \code{X} would result
#' in an error, a default value will be returned. Warnings will be handled
#' according to the option \code{\link[base:options]{options("warn")}}.
#' 
#' @return For return values see 'Value' section of \code{\link[base]{lapply}}.
#'  In the case of \code{FUN} returning an error, the default value
#'  \code{DEFAULT} is returned.
#' 
#' @export
tryLapply <- function(X, FUN, ..., DEFAULT = NULL) {
  lapply(X, function(x, ...) {    
    withCallingHandlers(
      expr = tryCatch(
        expr = FUN(x, ...),
        error = function(e) DEFAULT),
      warning = function(w) {
        warning(conditionMessage(w), call. = FALSE)
        invokeRestart('muffleWarning')
      })
  }, ...)
}



#' @rdname tryLapply
#' @param simplify logical or character string, see \code{\link[base]{sapply}}
#'  for details
#' @param USE.NAMES logical, see \code{\link[base]{sapply}} for details
#' @export
trySapply <- function(X, FUN, ..., DEFAULT = NULL, simplify = TRUE,
                      USE.NAMES = TRUE) {
  sapply(X, function(x, ...) {    
    withCallingHandlers(
      expr = tryCatch(
        expr = FUN(x, ...),
        error = function(e) DEFAULT),
      warning = function(w) {
        warning(conditionMessage(w), call. = FALSE)
        invokeRestart('muffleWarning')
      })
  }, ..., simplify = simplify, USE.NAMES = USE.NAMES)
}



#' @rdname tryLapply
#' @param FUN.VALUE a (generalized) vector; a template for the return value
#'  from \code{FUN}, see 'Details' section of \code{\link[base]{vapply}}
#' 
#' @note For \code{tryVapply} the default value \code{DEFAULT} must be of the
#'  same type as \code{FUN.VALUE}.
#' @export
tryVapply <- function(X, FUN, FUN.VALUE, ..., DEFAULT = NULL) {
  vapply(X, function(x, ...) {    
    withCallingHandlers(
      expr = tryCatch(
        expr = FUN(x, ...),
        error = function(e) DEFAULT),
      warning = function(w) {
        warning(conditionMessage(w), call. = FALSE)
        invokeRestart('muffleWarning')
      })
  }, FUN.VALUE = FUN.VALUE, ...)
}

