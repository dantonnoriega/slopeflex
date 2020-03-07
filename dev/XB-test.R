######################################
## SLOPEFLEX ----------------
######################################
library(slopeflex)
library(tidyverse)
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

# build a list of existing AND future slope flexes (sx)
## add some uncertainty to the break points
sx <- tibble::tribble(
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
sx


##########################################################################################
# (0) PROTOTYPE SLOPE FLEX --------------------------------------
##########################################################################################
# input dataframe ds with values (date, y) and slope flex table sx with horizon h
h = 365
sx_obj_stan <- slopeflex_fit_model(ds,sx,h)
sx_obj_lm <- slopeflex_fit_model(ds,sx,h, 'lm')

# build plot ------------------------------------------------------------------------
plot(sx_obj_stan)
plot(sx_obj_lm)
