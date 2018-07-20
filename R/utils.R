
# function to return tibble with NAs as specified
# list_NA(c("mean", "lwr.ci", "upr.ci"))
list_NA <- function(...) {
  names <- c(...)
  results <- as.list(c(rep(NA, length(names))))
  names(results) <- names
  results <- tibble::as_tibble(results)
  results[1:length(results)] <- as.numeric(results[1:length(results)])
  return(results)
}


# Check Dots --------------------------------------------------------------

# Author:   Andri Signorell
# Version:	0.99.x
# LICENSE: GPL (>= 2)
# Original: DescTools v0.99.24
# Link: https://github.com/cran/DescTools

#' Is a Specific Argument in the Dots-Arguments?
#'
#' Returns \code{TRUE} if a specific named argument was given in the dots.
#' @param ... the dots arguments of the function whose arguments are to be checked.
#' @param arg the name of argument to test for.
#' @param default the default value to return, if the argument \code{arg} does not exist in the dots.
#'
#' @return the value of the argument, if it exists else the specified default value.
#' @export
#' @author Andri Signorell <andri@signorell.net>
#' @keywords internal
InDots <- function(..., arg, default){

  # was arg in the dots-args? parse dots.arguments
  arg <- unlist(match.call(expand.dots = FALSE)$...[arg])

  # if arg was not in ... then return default
  if (is.null(arg)) arg <- default

  return(arg)

}
