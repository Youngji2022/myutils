
#' A summary of a numeric vector
#'
#' Custom version of summary function for a numeric vector.
#'
#' @param x a numeric vector
#' @param na.rm an optional logical parameter. TRUE by default.
#' @param verbose is TRUE for verbose output
#'
#' @return A named vector with six values.
#' @export
#' @import("ggplot2")
#' @importFrom("stats", "rnorm")
#'
#' @examples
#' x <- c(4,7,8)
#' numeric_summary(x)


numeric_summary <- function(x, na.rm=TRUE, verbose=FALSE){


  if (!is.numeric(x)) stop("x must be a numeric vector")

  if (verbose)message ("Opening database")
  #print( "See documentation in here")

  d<-data.frame (i = 1:10, b=stats::rnorm(10))
  ggplot(d, aes(i,b)) + geom_line()

  min = min(x, na.rm=na.rm)
  max = max(x, na.rm=na.rm)
  mean = mean(x, na.rm=na.rm)
  sd = sd(x, na.rm=na.rm)
  length = length(x)
  Nmiss = sum(is.na(x))

  c(min=min, max=max, mean=mean, sd=sd, length=length, Nmiss=Nmiss)

}

#' A summary of a character vector
#'
#' Custom version of \code{summary} function for a character vector.
#'
#' @param x a character vector
#' @param na.rm an optional logical parameter. \code{TRUE} by default.
#'
#' @return A named vector with three values.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- c("A", "B", "A", "C")
#' char_summary(x)
#'}
char_summary <- function(x, na.rm=TRUE){

  length = length(x)
  Nmiss = sum(is.na(x))
  Nunique = length(unique(x))

  c(length = length,
    Nmiss = Nmiss,
    Nunique = Nunique )

}
