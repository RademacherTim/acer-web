#===============================================================================
#
# Section 3.3: Number of taps
#
# script to explore relationship between sap volume and succrose concentration 
# and number of taps
#-------------------------------------------------------------------------------

# load dependencies ------------------------------------------------------------
library("brms")
library("tidybayes")

# load the wrangled data -------------------------------------------------------
source("0_wrangle_data.R")

# get number of data points for sap yield --------------------------------------
sap_data %>% filter(!is.na(n_taps) & sap_volume > 100) %>% count() # number of data points
seasonal_data %>% filter(!is.na(n_taps) & sap_volume > 100) %>% count() # number of data points
seasonal_data %>% filter(!is.na(n_taps) & sap_volume > 0) %>% group_by(site, tree, tap) %>% n_groups() # number of taps
seasonal_data %>% filter(!is.na(n_taps) & sap_volume > 0) %>% group_by(site, tree) %>% n_groups() # number of trees
seasonal_data %>% filter(!is.na(n_taps) & sap_volume > 0 & n_taps == 1) %>% group_by(site, tree) %>% n_groups() # number of trees with one tap
seasonal_data %>% filter(!is.na(n_taps) & sap_volume > 0 & n_taps == 2) %>% group_by(site, tree) %>% n_groups() # number of trees with one tap
seasonal_data %>% filter(!is.na(n_taps) & sap_volume > 0 & n_taps == 3) %>% group_by(site, tree) %>% n_groups() # number of trees with one tap
seasonal_data %>% filter(!is.na(n_taps) & sap_volume > 0) %>% group_by(site) %>% n_groups() # number of sites
seasonal_data %>% filter(!is.na(n_taps) & sap_volume > 0) %>% group_by(year) %>% n_groups() # number of years

# get number of data points for sap brix ---------------------------------------
sap_data %>% filter(!is.na(n_taps) & sap_brix > 0) %>% count() # number of data points
seasonal_data %>% filter(!is.na(n_taps) & sap_brix > 0) %>% count() # number of data points
seasonal_data %>% filter(!is.na(n_taps) & !is.na(sap_brix)) %>% group_by(site, tree, tap) %>% n_groups() # number of taps
seasonal_data %>% filter(!is.na(n_taps) & !is.na(sap_brix)) %>% group_by(site, tree) %>% n_groups() # number of trees
seasonal_data %>% filter(!is.na(n_taps) & !is.na(sap_brix) & n_taps == 1) %>% group_by(site, tree) %>% n_groups() # number of trees with one tap
seasonal_data %>% filter(!is.na(n_taps) & !is.na(sap_brix) & n_taps == 2) %>% group_by(site, tree) %>% n_groups() # number of trees with one tap
seasonal_data %>% filter(!is.na(n_taps) & !is.na(sap_brix) & n_taps == 3) %>% group_by(site, tree) %>% n_groups() # number of trees with one tap
seasonal_data %>% filter(!is.na(n_taps) & !is.na(sap_brix)) %>% group_by(site) %>% n_groups() # number of sites
seasonal_data %>% filter(!is.na(n_taps) & !is.na(sap_brix)) %>% group_by(year) %>% n_groups() # number of years

# remove single data point for one tree with three taps ------------------------
data3.3 <- seasonal_data %>% filter(n_taps %in% 1:2) %>% 
  select(site, tree, tap, year, spp, n_taps, log_yield, sap_volume, sap_brix, dbh) %>%
  mutate(tree = factor(tree))

# second tap effect on sap yield -----------------------------------------------
# fit a lognormal distibution (NOT accounting for difference in tree size)
mod3.3.1a <- brms::brm(brms::bf(sap_volume ~
                                (1 | year) +   # interannual differences in sap yield
                                (1 | n_taps) + # effect of first and second tap
                                (1 | tree) +   # tree-specific effects
                                (1 | spp) +    # species-specific effects 
                                (1 | site)),   # site-specific effects
                  data = data3.3 %>% filter(!is.na(sap_volume)),
                  family = lognormal(), 
                  # TR - Are these priors still meaningful?
                  prior = c(set_prior("normal(3.7, 10)", class = "Intercept"), # Corresponds to roughly 40L of sap or 1L of syrup with 40:1 conversion
                            set_prior("exponential(1)", class = "sigma"),
                            set_prior("normal(0, 2)", class = "sd"), # the interannual difference falls within -20L to +20L with 95% chance
                            set_prior("normal(0, 1)", class = "sd", coef = "Intercept", group = "n_taps")),
                  cores = 4, chains = 4,
                  control = list(adapt_delta = 0.99, max_treedepth = 11), # model looks good, so I 
                  # tried increasing adapt_delta, as a last resort to reduce 
                  # divergent transitions
                  # NB.: Changing adapt_delta only works with backend = cmdstanr 
                  # and not with rstan at the moment, see:
                  # https://discourse.mc-stan.org/t/2-16-1-and-adapt-delta/24239/5
                  iter = 6000,
                  seed = 1353,
                  backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod3.3.1a)
plot(conditional_effects(mod3.2.1a))

# additional posterior distribution checks -------------------------------------
pp_check(mod3.3.1a, ndraws = 100)
pp_check(mod3.3.1a, type = "error_hist",  ndraws = 10)
pp_check(mod3.3.1a, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed

# get model summary and coefficients -------------------------------------------
summary(mod3.3.1a)
ranef(mod3.3.1a)$n_taps[, , "Intercept"]
ranef(mod3.3.1a)$tree[, , "Intercept"]
ranef(mod3.3.1a)$spp[, , "Intercept"]
ranef(mod3.3.1a)$site[, , "Intercept"]

# include an interaction between dbh and n_taps in the model to see whether size 
# affects the amount of additional sap per tap ---------------------------------
mod3.3.1b <- brms::brm(brms::bf(sap_volume ~ 
                                  (1 | year) + 
                                  (1 | n_taps) +
                                  dbh + 
                                  (1 | tree) + # tree-specific effects
                                  (1 | spp) + 
                                  (1 | site)),
                  data = data3.3 %>% filter(!is.na(sap_volume)),
                  family = lognormal(), 
                  prior = c(set_prior("normal(3.7, 10)", class = "Intercept"), # Corresponds to roughly 40L of sap or 1L of syrup with 40:1 conversion
                            set_prior("exponential(1)", class = "sigma"),
                            set_prior("normal(1, 2)", class = "b"),
                            set_prior("normal(0, 2)", class = "sd"), # the interannual difference falls within -20L to +20L with 95% chance
                            set_prior("normal(0, 1)", class = "sd", coef = "Intercept", group = "n_taps")),
                  cores = 4, chains = 4,
                  control = list(adapt_delta = 0.99, max_treedepth = 12),
                  iter = 6000, 
                  seed = 1353,
                  backend = "cmdstanr")
# TR - The fact that the number of taps was not a randomised treatment in any of 
# the component data sets may cause some or all of the  convergence problems. 
# Overall, there is nothing suggesting that the model does not fit the data though. 

# posterior distribution checks ------------------------------------------------
plot(mod3.3.1b)
plot(conditional_effects(mod3.2.1b))

# additional posterior distribution checks -------------------------------------
pp_check(mod3.3.1b, ndraws = 100)
pp_check(mod3.3.1b, type = "error_hist",  ndraws = 10)
pp_check(mod3.3.1b, type = "scatter_avg", ndraws = 100)

# get model summary and coeficcients -------------------------------------------
summary(mod3.3.1b)
ranef(mod3.3.1b)

# draw from posterior ----------------------------------------------------------

# effect of the number of taps on sugar content --------------------------------
# fit a truncated normal distibution, as brix cannot be negative
mod3.3.2 <- brms::brm(brms::bf(sap_brix | trunc(lb = 0) ~
                                 (1 | year) + 
                                 (1 | n_taps) + 
                                 (1 | tree) +
                                 (1 | spp) + 
                                 (1 | site)),
                  data = data3.3 %>% filter(!is.na(sap_brix)), # exclude NAs 
                  family = gaussian(), 
                  prior = c(set_prior("normal(2, 1)", class = "Intercept"),
                            set_prior("exponential(1)", class = "sigma"),
                            set_prior("normal(0, 2)", class = "sd"),
                            set_prior("normal(0, 1)", class = "sd", coef = "Intercept", group = "n_taps")),
                  cores = 4, chains = 4,
                  control = list(adapt_delta = 0.9),
                  iter = 6000,
                  seed = 1353,
                  backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod3.3.2)

# additional posterior distribution checks -------------------------------------
pp_check(mod3.3.2, ndraws = 100)
pp_check(mod3.3.2, type = "error_hist",  ndraws = 10)
pp_check(mod3.3.2, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed 

# get model summary and coeficcients -------------------------------------------
summary(mod3.3.2)
ranef(mod3.3.2)
ranef(mod3.3.2)$n_taps

# Conclusion: A second tap reduces sap yield for the second tap substantially, 
#             but does not affect sugar content.