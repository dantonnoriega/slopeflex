######################################
## SLOPEFLEX ----------------
######################################
set.seed(2020)
dates <- seq(as.Date('2019-01-01'), as.Date('2019-11-01'), by = 'day')
# create 4 segments (3 breaks/flexes)
n <- length(dates) # length of original data 274
slope <- rnorm(4, 5, 40)
slope_sd <- .3
breaks <- dates[c(1, 121, 159, 230)]
# generate the data
y0 <- pmax(0, dates - breaks[1]) * rnorm(n, slope[1], slope_sd)
y1 <- pmax(0, dates - breaks[2]) * rnorm(n, slope[2], slope_sd)
y2 <- pmax(0, dates - breaks[3]) * rnorm(n, slope[3], slope_sd)
y3 <- pmax(0, dates - breaks[4]) * rnorm(n, slope[4], slope_sd)
y <- y0 + y1 + y2 + y3
plot(y)

# want to build a dataset that maps each point to a date then takes only a sample
ds <- tibble::tibble(date = dates, y = y) # expects "date" and "y"
ds <- ds %>%
  dplyr::sample_n(60) %>%
  dplyr::arrange(date)
# plot the redux ds dataframe
with(ds, plot(y = y, x = date))

# build a list of existing AND future slope flexes (sf)
## add some uncertainty to the break points
sf <- tibble::tribble(
  ~date, ~params, ~params_sd,
  # these dates comprise the dates we sort of know with our best guesses on the slope changes
  breaks[2] + round(rnorm(1, 2, 3)), 5, 1,
  breaks[3] + round(rnorm(1, 2, 3)), 10, 10,
  breaks[4] + round(rnorm(1, 2, 3)), -20, 10,
  # these dates comprise future dates as "best guesses" by folks with knowledge (e.g. engineers)
  as.Date("2019-12-08"), 32, 5,
  as.Date("2020-02-01"), 9, 3,
  as.Date("2020-03-13"), -16, 5
)
sf


##########################################################################################
# (0) PROTOTYPE SLOPE FLEX --------------------------------------
##########################################################################################
#' input dataframe ds with values (date, y) and slope flex table sf with horizon h
build_slope_flex_model <- function(ds, sf, h) {
  #
  y <- ds$y
  dates <- ds$date
  dates_fc <- seq(max(dates) + 1, max(dates) + h, by = 1)
  time <- as.numeric(dates)
  time_fc <- as.numeric(dates_fc)
  idx <- as.vector(dates - min(dates) + 1)
  idx_fc <- as.vector(dates_fc - min(dates) + 1)
  #
  build_slope_flex <- function(x, v) {
    # x assumed character of dates. convert to numeric.
    x <- x %>%
      sapply(as.Date) %>%
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
      sprintf("pmax(0, time - %s)", names(slope_flex)),
      collapse = " + "),
    sep = ' + ')
  fml_fc <- paste(
    "~ idx_fc",
    paste(
      sprintf("pmax(0, time_fc - %s)", names(slope_flex)),
      collapse = " + "),
    sep = ' + ')
  #
  X <- model.matrix(as.formula(fml))
  X_fc <- model.matrix(as.formula(fml_fc))
  #
  standata <- within(list(), {
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
  #
  standata
  #
}

options(mc.cores = 1)
mcmc_list = list(n_iter = 5000, n_chain = 2, n_thin = 1, n_warmup = 1000,
                 control = list(adapt_delta = .8, max_treedepth = 10))
h = as.integer(max(sf$date) - max(dates)) + 60 # end of data to last assump flex + 60 days
standata <- build_slope_flex_model(ds, sf, h)
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
rstan::get_posterior_mean(fit, 'B')[,1] # intercept, slope (B[1:2]) and flex estimates (B[3:])
apply(rstan::extract(fit)[['B']], 2, sd) # variation in parameter estimates


# build plot ------------------------------------------------------------------------
yhat_CI <- function(fit, par = 'yhat_fc', p) {
  XB <- rstan::extract(fit)[[par]]
  sig <- rstan::extract(fit)[['sig']]
  sims <- c(XB) + rnorm(length(XB), 0, c(sig))
  apply(matrix(sims, nrow(XB), ncol(XB)), 2, quantile, p)
}
yhat <- rstan::get_posterior_mean(fit, pars = 'yhat')[,'mean-all chains']
yhat_fc <- rstan::get_posterior_mean(fit, pars = 'yhat_fc')[,'mean-all chains']
xx <- rstan::get_posterior_mean(fit, pars = 'x_fc')[,'mean-all chains']
x <- rstan::get_posterior_mean(fit, pars = 'x')[,'mean-all chains']
x <- x + min(dates)
xx <- xx + min(dates)
yhat_LB = yhat_CI(fit, p = .05)
yhat_UB = yhat_CI(fit, p = .95)

# output plots -----------
plot(yhat_fc, x = xx, ylim = c(min(yhat_LB), max(yhat_UB)),
     xlim = c(min(xx), max(xx)),
     type = 'l', lty = 3, lwd = 2,
     col = 4)
polygon(x = c(xx,rev(xx)),
        y = c(yhat_LB, rev(yhat_UB)),
        border = NA, col = scales::alpha('gray50', .3))
lines(yhat, x = x, lty = 1, lwd = 2, col = 1)
points(standata$y, x = x, pch = 20, col = 2)


