#===============================================================================
# script to explore relationship between sap volume and succrose concentration 
# and number of taps
#-------------------------------------------------------------------------------

# load dependencies ------------------------------------------------------------
library("brms")
library("tidybayes")

# load the wrangled data -------------------------------------------------------
source("0_wrangle_data.R")

# get basic stats for sap yield ------------------------------------------------
seasonal_data %>% filter(!is.na(n_taps) & sap_volume > 0) %>% group_by(site, tree, tap) %>% n_groups() # number of taps
seasonal_data %>% filter(!is.na(n_taps) & sap_volume > 0) %>% group_by(site, tree) %>% n_groups() # number of trees
seasonal_data %>% filter(!is.na(n_taps) & sap_volume > 0 & n_taps == 1) %>% group_by(site, tree) %>% n_groups() # number of trees with one tap
seasonal_data %>% filter(!is.na(n_taps) & sap_volume > 0 & n_taps == 2) %>% group_by(site, tree) %>% n_groups() # number of trees with one tap
seasonal_data %>% filter(!is.na(n_taps) & sap_volume > 0 & n_taps == 3) %>% group_by(site, tree) %>% n_groups() # number of trees with one tap
seasonal_data %>% filter(!is.na(n_taps) & sap_volume > 0) %>% group_by(site) %>% n_groups() # number of sites
seasonal_data %>% filter(!is.na(n_taps) & sap_volume > 0) %>% group_by(year) %>% n_groups() # number of years

# get basic stats for sap brix -------------------------------------------------
seasonal_data %>% filter(!is.na(n_taps) & !is.na(sap_brix)) %>% group_by(site, tree, tap) %>% n_groups() # number of taps
seasonal_data %>% filter(!is.na(n_taps) & !is.na(sap_brix)) %>% group_by(site, tree) %>% n_groups() # number of trees
seasonal_data %>% filter(!is.na(n_taps) & !is.na(sap_brix) & n_taps == 1) %>% group_by(site, tree) %>% n_groups() # number of trees with one tap
seasonal_data %>% filter(!is.na(n_taps) & !is.na(sap_brix) & n_taps == 2) %>% group_by(site, tree) %>% n_groups() # number of trees with one tap
seasonal_data %>% filter(!is.na(n_taps) & !is.na(sap_brix) & n_taps == 3) %>% group_by(site, tree) %>% n_groups() # number of trees with one tap
seasonal_data %>% filter(!is.na(n_taps) & !is.na(sap_brix)) %>% group_by(site) %>% n_groups() # number of sites
seasonal_data %>% filter(!is.na(n_taps) & !is.na(sap_brix)) %>% group_by(year) %>% n_groups() # number of years

# remove single data point for one tree with three taps ------------------------
data3.2 <- seasonal_data %>% filter(n_taps %in% 1:2) %>% 
  select(site, tree, tap, year, spp, n_taps, log_yield, sap_brix)

# fit anormal distibution to log-transformed sap yield data
mod3.2.1a <- brms::brm(brms::bf(log_yield ~
                                  year +       # interannual differences in sap yield
                                  mo(n_taps) + # monotonic effect of ordinal predictor of number of taps
                                  (1 | tree) + # tree-specific effects
                                  (1 | spp) +  # species-specific effects 
                                  (1 | site)), # site-specific effects
                  data = data3.2,
                  family = gaussian(), 
                  prior = c(set_prior("normal(3.7, 10)", class = "Intercept"), # Corresponds to roughly 40L of sap or 1L of syrup with 40:1 conversion
                            set_prior("normal(0, 3)", class = "b"), # the interannual difference falls within -20L to +20L with 95% chance
                            set_prior("normal(0, 2)", class = "b", coef = "mon_taps"),
                            set_prior("dirichlet(1)", class = "simo", coef = "mon_taps1")),
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
plot(mod3.2.1a)
plot(conditional_effects(mod3.2.1a))

# additional posterior distribution checks -------------------------------------
pp_check(mod3.2.1a, ndraws = 100)
pp_check(mod3.2.1a, type = "error_hist",  ndraws = 10)
pp_check(mod3.2.1a, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed
# Maybe there is a bias (under-estimation at high sap yield values)

# get model summary and coefficients -------------------------------------------
summary(mod3.2.1a)
ranef(mod3.2.1a)$tree[, , "Intercept"]
ranef(mod3.2.1a)$spp[, , "Intercept"]
ranef(mod3.2.1a)$site[, , "Intercept"]

# include an interaction between dbh and n_taps in the model to see whether size 
# affects the amount of additional sap per tap ---------------------------------
mod3.2.1b <- brms::brm(brms::bf(log_yield ~ 
                                  year + 
                                  dbh * mo(n_taps) + 
                                  (1 | tree) + # tree-specific effects
                                  (1 | spp) + 
                                  (1 | site)),
                  data = seasonal_data,
                  family = gaussian(), 
                  prior = c(set_prior("normal(3.7, 10)", class = "Intercept"),
                            set_prior("normal(0, 3)", class = "b"),
                            set_prior("normal(0, 2)", class = "b", coef = "mon_taps"),
                            set_prior("dirichlet(1)", class = "simo", coef = "mon_taps:dbh1"),
                            set_prior("dirichlet(1)", class = "simo", coef = "mon_taps1")),
                  cores = 1, chains = 1,
                  control = list(adapt_delta = 0.99, max_treedepth = 11),
                  iter = 6000, 
                  seed = 1353,
                  backend = "cmdstanr")
#brms::prior_summary(mod3.2.1b)

# posterior distribution checks ------------------------------------------------
plot(mod3.2.1b)
plot(conditional_effects(mod3.2.1b))

# additional posterior distribution checks -------------------------------------
pp_check(mod3.2.1b, ndraws = 100)
pp_check(mod3.2.1b, type = "error_hist",  ndraws = 10)
pp_check(mod3.2.1b, type = "scatter_avg", ndraws = 100)

# get model summary and coeficcients -------------------------------------------
summary(mod3.2.1b)
ranef(mod3.2.1b)

# draw from posterior ----------------------------------------------------------
mod3.2.1b %>%
  spread_draws(b_Intercept, b_dbh, bsp_mon_taps, r_tree[tree, ], r_site[site, ], r_spp[spp, ]) %>%
  mutate(mean_effect = exp(b_Intercept + b_dbh + bsp_mon_taps + bsp_mon_taps:dbh + r_tree + r_site + r_spp)) %>%
  median_hdi()
m %>%
  spread_draws(b_Intercept, r_condition[condition,]) %>%
  mutate(condition_mean = b_Intercept + r_condition) %>%
  ggplot(aes(y = condition, x = condition_mean)) +
  stat_halfeye()

# fit a truncated normal distibution to determine the effect of the number of taps
mod3.2.2 <- brms::brm(brms::bf(sap_brix | trunc(lb = 0) ~ 
                                 year + 
                                 mo(n_taps) + 
                                 (1 | tree) +
                                 (1 | spp) + 
                                 (1 | site)),
                  data = seasonal_data,
                  family = gaussian(), 
                  prior = c(set_prior("normal(0, 2)", class = "b"),
                            set_prior("dirichlet(c(1, 1))", class = "simo", coef = "mon_taps1")),
                  cores = 1, chains = 1,
                  control = list(adapt_delta = 0.9),
                  iter = 4000)

# posterior distribution checks ------------------------------------------------
plot(mod3.2.2)

# additional posterior distribution checks -------------------------------------
pp_check(mod3.2.2, ndraws = 100)
pp_check(mod3.2.2, type = "error_hist",  ndraws = 10)
pp_check(mod3.2.2, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed 

# get model summary and coeficcients -------------------------------------------
summary(mod3.2.2)
ranef(mod3.2.2)
