.onAttach <- function(libname, pkgname) {
  packageStartupMessage(paste(pkgname, utils::packageVersion(pkgname)))
}

#' The 'slopeflex' package.
#'
#' @description A DESCRIPTION OF THE PACKAGE
#'
#' @docType package
#' @name slopeflex-package
#' @aliases slopeflex
#' @useDynLib slopeflex, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#'
#' @references
#' Stan Development Team (2019). RStan: the R interface to Stan. R package version 2.19.2. https://mc-stan.org
#'
NULL