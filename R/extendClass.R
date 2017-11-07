#' Extend Classes
#' 
#' The function consistently extends classes. See details for algorithm.
#' 
#' @param subclass,value character, \emph{unique} name(s) of subclass(es) that
#'   extend the base classes \code{class}
#' @param class character, \emph{unique} names of classes to be extended
#' 
#' @details Both \code{class} and \code{subclass} (or equivalently \code{value})
#'   must be an ordered set of subclasses. If \code{class} and \code{subclass}
#'   are \emph{not} disjoint, the intersection has to be of the same order.
#'   Note that if either \code{class} or \code{subclass} are not unique or the
#'   order of the intersection differs, an error is thrown.
#'   
#'   If (the first element of) \code{subclass} is contained in \code{class},
#'   the return value contain subclasses of \code{class} with \code{subclass}
#'   the first element, i.e. those elements of \code{class} that "come before"
#'   \code{subclass} omitted. Otherwise \code{subclass} will be prepended. For
#'   \code{subclass} of length greater 1, this will be done iteratively.
#'   
#' @return a character string containing a extended list of classes
#' 
#' @export
#' @rdname extendClass
extend_class <- function(subclass, class) {
  
  if (length(subclass) == 0) return(class)
  
  stopifnot(
    all(!duplicated(class)),  # unique base classes
    all(!duplicated(subclass)),  # unique extended subclasses
    all(intersect(class, subclass) == intersect(subclass, class))  # order of common subclasses
  )
  
  ii <- which(subclass[1] == class)
  if (length(ii)) {
    class <- class[seq(max(ii), length(class))]
  } else {
    class <- c(subclass[1], class)
  }
  
  if (length(subclass) > 1) {
    class <- c(class[1], extend_class(subclass[-1], class[-1]))
  }

  return(class)
  
}

#' @rdname extendClass
#' @param x any object, which's class is to be expanded
#' @export
`extend_class<-` <- function(x, value) {
  class(x) <- extend_class(value, class(x))
  return(x)
}
