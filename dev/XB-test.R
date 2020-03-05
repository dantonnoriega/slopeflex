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
  breaks[4] + round(rnorm(1, 2, 3)), -20, 20,
  # these dates comprise future dates as "best guesses" by folks with knowledge (e.g. engineers)
  as.Date("2019-12-08"), 32, 5,
  as.Date("2020-02-01"), 51, 3,
  as.Date("2020-03-13"), -36, 5
)
sf


##########################################################################################
# (0) PROTOTYPE SLOPE FLEX --------------------------------------
##########################################################################################
#' input dataframe ds with values (date, y) and slope flex table sf with horizon h
options(mc.cores = 1)
mcmc_list = list(n_iter = 2500, n_chain = 1, n_thin = 1, n_warmup = 500,
                 control = list(adapt_delta = .8, max_treedepth = 10))
h = as.integer(max(sf$date) - max(dates)) + 60 # end of data to last assump flex + 60 days
standata <- slopeflex_build_model(ds, sf, h)
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

rstan::get_posterior_mean(fit, 'B')[,1] # intercept, slope (B[1:2]) and flex estimates (B[3:])
apply(rstan::extract(fit)[['B']], 2, sd) # variation in parameter estimates

# build plot ------------------------------------------------------------------------
slopeflex_plot_fit(fit)
