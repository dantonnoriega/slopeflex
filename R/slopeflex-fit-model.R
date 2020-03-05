#' function to build a slopeflex model matrix
#' @inheritParams slopeflex_build_model
#' @param mc.cores number of cores to use. defaults 1 (recommended; faster)
#' @param mcmc_list rstan list of options
#' @export

slopeflex_fit_model <- function(
  ds, sf, h, mc.cores = 1,
  mcmc_list = list(n_iter = 2500, n_chain = 1, n_thin = 1, n_warmup = 500,
                   control = list(adapt_delta = .8, max_treedepth = 10))
) {

  options(mc.cores = mc.cores)
  H = as.integer(max(sf$date) - max(dates)) + h # add h after last obs forecast slopeflex
  standata <- slopeflex_build_model(ds, sf, H)
  stan_file <- here::here("stan/XB.stan")
  cat(paste(readLines(stan_file)), sep = '\n')
  model <- rstan::stan_model(stan_file, auto_write = TRUE)
  fit <- rstan::sampling(
    model,
    data = standata,
    control = mcmc_list$control,
    chains = mcmc_list$n_chain,
    warmup = mcmc_list$n_warmup,
    iter = mcmc_list$n_iter,
    thin = mcmc_list$n_thin)
  attr(fit, 'slopeflex') = TRUE
  attr(fit, 'ds') = ds
  fit

}