#' function to build a slopeflex model matrix
#' @importFrom magrittr %>%
#' @param ds data frame or tibble with 2 columns. First column is a date, second some value
#' @param sf data frame or tibble with 3 columns.
#'     First column is date, second the expected value of the slope change,
#'     third is the standard deviation of the the slope change.
#' @param h positive integer value. this is the forecast horizon.
#' @export

slopeflex_build_model <- function(ds, sf, h) {

  y <- ds$y
  times <- as.numeric(ds$date)
  times_fc <- as.numeric(seq(max(ds$date) + 1, max(ds$date) + h, by = 1))
  idx <- as.vector(times - min(times) + 1)
  idx_fc <- as.vector(times_fc - min(times) + 1)
  #
  build_slope_flex <- function(x, v) {
    # x assumed character of dates. convert to numeric.
    if(class(x) %in% c("Date", "character")) {
      x <- x %>%
        sapply(as.Date)
    }
    x <- x %>%
      sapply(as.numeric)
    # generate named list
    names(x) <-
      paste0(v, seq_along(x))
    assign(v, x, envir = parent.frame())
    # assign locally to function env
    for(i in names(x))
      assign(i, x[i], envir = parent.frame())
  }
  #
  build_slope_flex(sf$date, 'slope_flex')
  #
  fml <- paste(
    "~ idx",
    paste(
      sprintf("pmax(0, times - %s)", names(slope_flex)),
      collapse = " + "),
    sep = ' + ')
  fml_fc <- paste(
    "~ idx_fc",
    paste(
      sprintf("pmax(0, times_fc - %s)", names(slope_flex)),
      collapse = " + "),
    sep = ' + ')
  #
  X <- model.matrix(as.formula(fml))
  X_fc <- model.matrix(as.formula(fml_fc))
  #
  within(list(), {
    X <- X
    X_fc <- X_fc
    y <- y
    params <- sf$params
    params_sd <- sf$params_sd
    n <- length(y)
    m <- ncol(X)
    k <- length(params)
    h <- h
  })

}