#' function to build a slopeflex model matrix. can handle unobserved slopeflexes
#'     if engine = 'stan'
#' @param ds data frame or tibble with 2 columns.
#'     First column is a date, second some value
#' @param sx data frame or tibble with 3 columns.
#'     First column is date, second the expected value of the slope change,
#'     third is the standard deviation of the the slope change.
#' @param h positive integer value. this is the forecast horizon.
#'     added to the end of the last slopeflex (even if unobserved)
#' @param lb_ub numeric(2) vector with lower bound and upper bound.
#' @param engine c('stan', 'lm')
#' @export

slopeflex_fit_model <- function(
  ds, sx, h, engine = c('stan', 'lm')[1],
  lb_ub = c(.05,.95), ...
) {

  # if things fail to build, go to
  # https://discourse.mc-stan.org/t/rstantools-rstan-package-skeleton-failing-
  # with-error-in-getdllregisteredroutines-dllinfo/6848/31
  stopifnot(engine %in% c('stan', 'lm'))
  dots = list(...)

  if(engine == 'stan') {
    mc.cores = if(is.null(dots$mc.cores)) 1 else dots$mc.cores
    mcmc_list = if(is.null(dots$mcmc_list)) {
      list(n_iter = 2500, n_chain = 1, n_thin = 1, n_warmup = 500,
             control = list(adapt_delta = .8, max_treedepth = 10))
    } else dots$mcmc_list
    slopeflex_fit_model_stan(ds, sx, h, lb_ub, mc.cores, mcmc_list)
  } else {
    slopeflex_fit_model_lm(ds, sx, h, lb_ub)
  }

}

#' function to build a slopeflex model matrix using 'stan' as the engine
#' @param ds data frame or tibble with 2 columns.
#'     First column is a date, second some value
#' @param sx data frame or tibble with 3 columns.
#'     First column is date, second the expected value of the slope change,
#'     third is the standard deviation of the the slope change.
#' @param h positive integer value. this is the forecast horizon.
#'     added to the end of the last slopeflex (even if unobserved)
#' @param lb_ub numeric(2) vector with lower bound and upper bound.
#' @param mc.cores number of cores to use. defaults 1 (recommended; faster)
#' @param mcmc_list rstan list of options

slopeflex_fit_model_stan <- function(
  ds, sx, h, lb_ub, mc.cores, mcmc_list
) {

  options(mc.cores = mc.cores)
  dates = ds[[1]]
  standata <- slopeflex_build_model(ds, sx, h)
  fit <- rstan::sampling(
    stanmodels$XB,
    data = standata,
    control = mcmc_list$control,
    chains = mcmc_list$n_chain,
    warmup = mcmc_list$n_warmup,
    iter = mcmc_list$n_iter,
    thin = mcmc_list$n_thin)

  n_chains = length(fit@stan_args)
  col_name = if(n_chains == 1) 'mean-chain:1' else 'mean-all chains'

  yhat_CI <- function(fit, par = 'yhat_fc', p) {
    XB <- rstan::extract(fit)[[par]]
    sig <- rstan::get_posterior_mean(fit, pars = 'sig')[,col_name]
    apply(matrix(XB, nrow(XB), ncol(XB)), 2, quantile, p) +
      qnorm(p, 0, sd = sig)
    # sig <- rstan::extract(fit)[['sig']]
    # sims <- c(XB) + rnorm(length(XB), 0, c(sig))
    # apply(matrix(sims, nrow(XB), ncol(XB)), 2, quantile, p)
  }
  yhat <- rstan::get_posterior_mean(fit, pars = 'yhat')[,col_name]
  yhat_fc <- rstan::get_posterior_mean(fit, pars = 'yhat_fc')[,col_name]
  X <- standata$X
  X_fc <- standata$X_fc
  x <- X[,2];
  xx <- rbind(X,X_fc)[,2];
  x <- x + min(dates) - 1
  xx <- xx + min(dates) - 1
  yhat_LB = yhat_CI(fit, p = lb_ub[1])
  yhat_UB = yhat_CI(fit, p = lb_ub[2])

  sx_obj = list(
    fit = fit,
    engine = 'stan',
    data = standata,
    ds = ds,
    x = x,
    x_fc = xx,
    y = ds[[2]],
    yhat = c(yhat),
    yhat_fc = c(yhat_fc),
    yhat_LB = yhat_LB,
    yhat_UB = yhat_UB
  )
  class(sx_obj) <- 'slopeflex'
  sx_obj

}

#' function to build a slopeflex model matrix using 'lm' as the engine
#' @param ds data frame or tibble with 2 columns.
#'     First column is a date, second some value
#' @param sx data frame or tibble with 3 columns.
#'     First column is date, second the expected value of the slope change,
#'     third is the standard deviation of the the slope change.
#' @param h positive integer value. this is the forecast horizon.
#'     added to the end of the last slopeflex (even if unobserved)
#' @param lb_ub numeric(2) vector with lower bound and upper bound.
#' @param mc.cores number of cores to use. defaults 1 (recommended; faster)
#' @param mcmc_list rstan list of options

slopeflex_fit_model_lm<- function(
  ds, sx, h, lb_ub
) {

  dat <- slopeflex_build_model(ds, sx, h)
  fit <- with(dat, lm(y ~ X - 1))
  dates <- ds[[1]]

  unobs_dates <- as.Date(sx[[1]]) > max(as.Date(ds[[1]]))
  keep <- !summary(fit)$aliased
  b <- rep(0, length(keep))
  b_se <- rep(0, length(keep))
  names(b) <- names(keep)
  names(b_se) <- names(keep)
  nms <- names(summary(fit)[['coefficients']][,1])
  b[nms] <- summary(fit)[['coefficients']][,1]
  # way too much variations; damn you flat priors
  # b_se[nms] <- summary(fit)[['coefficients']][,2]
  b_se <- rep(0, length(b))
  # get unobs priors
  if(sum(unobs_dates) > 0) {
    idx <- (length(b) - sum(unobs_dates) + 1):length(b)
    mu <- tail(dat$params, sum(unobs_dates)) # take out intercept, index
    mu_se <- tail(dat$params_sd, sum(unobs_dates))
    # join together to sample
    b[idx] <- mu
    b_se[idx] <- mu_se
  }

  yhat_CI <- function(fit, p) {

    N <- 2000
    B <- matrix(rnorm(N*length(b), b, b_se), ncol = length(b), byrow = TRUE)
    X <- rbind(dat$X, dat$X_fc)
    XB <- B %*% t(X)
    sig <- sigma(fit)
    # sims <- c(XB) + rnorm(length(XB), 0, c(sig))
    # apply(matrix(sims, nrow(XB), ncol(XB)), 2, quantile, p)
    apply(matrix(XB, nrow(XB), ncol(XB)), 2, quantile, p) +
      qnorm(p, 0, sig)

  }

  X <- dat$X
  X_fc <- dat$X_fc
  yhat <- X %*% b
  yhat_fc <- rbind(X, X_fc) %*% b
  xx <- rbind(X, X_fc)[,2]
  x <- X[,2]
  x <- x + min(dates) - 1
  xx <- xx + min(dates) - 1
  yhat_LB = yhat_CI(fit, p = lb_ub[1])
  yhat_UB = yhat_CI(fit, p = lb_ub[2])

  sx_obj <- list(
    fit = fit,
    engine = 'lm',
    data = dat,
    ds = ds,
    x = x,
    x_fc = xx,
    y = ds[[2]],
    yhat = c(yhat),
    yhat_fc = c(yhat_fc),
    yhat_LB = yhat_LB,
    yhat_UB = yhat_UB
  )
  class(sx_obj) <- 'slopeflex'
  sx_obj

}