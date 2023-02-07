#===============================================================================
#
# Section 4.5: Height of the tap hole on the stem on sap yield and sugar content
#
# script to explore relationship between the height of the tap hole on the stem
# with sap yield and sap sugar content (i.e., succrose concentration).
#-------------------------------------------------------------------------------

# load the wrangled data -------------------------------------------------------
source("0_wrangle_data.R")

# get number of data points for sap yield --------------------------------------
sap_data %>% filter(!is.na(dbh)) %>% count() # number of data points
seasonal_data %>% filter(!is.na(dbh) & sap_volume > 0) %>% count() # number of data points
seasonal_data %>% filter(!is.na(dbh) & sap_volume > 0) %>% group_by(site, tree, tap, year) %>% n_groups() # number of taps
seasonal_data %>% filter(!is.na(dbh) & sap_volume > 0) %>% group_by(site, tree) %>% n_groups() # number of trees
seasonal_data %>% filter(!is.na(dbh) & sap_volume > 0) %>% group_by(site) %>% n_groups() # number of sites
seasonal_data %>% filter(!is.na(dbh) & sap_volume > 0) %>% group_by(year) %>% n_groups() # number of years

# get number of data points for sap brix ---------------------------------------
sap_data %>% filter(!is.na(dbh) & sap_brix > 0) %>% count() # number of data points
seasonal_data %>% filter(!is.na(dbh) & sap_brix > 0) %>% count() # number of data points
seasonal_data %>% filter(!is.na(dbh) & !is.na(sap_brix)) %>% group_by(site, tree, tap) %>% n_groups() # number of taps
seasonal_data %>% filter(!is.na(dbh) & !is.na(sap_brix)) %>% group_by(site, tree) %>% n_groups() # number of trees
seasonal_data %>% filter(!is.na(dbh) & !is.na(sap_brix)) %>% group_by(site) %>% n_groups() # number of sites
seasonal_data %>% filter(!is.na(dbh) & !is.na(sap_brix)) %>% group_by(year) %>% n_groups() # number of years

# get range of tap_height values -----------------------------------------------
range(seasonal_data$tap_height, na.rm = TRUE)

# remove irrelevant data -------------------------------------------------------
data4.5 <- seasonal_data %>% filter(!is.na(dbh) & !is.na(tap_height)) %>% 
  select(site, lat, tree, year, spp, sap_volume, sap_brix, dbh, tap_height) %>%
  mutate(tree = factor(tree))

# effect of tap_height on the stem on sap yield --------------------------------
# fit a lognormal distibution 
mod4.5.1 <- brms::brm(brms::bf(sap_volume ~
                                 (1 | year) +  # interannual differences in sap yield
                                 tap_height + 
                                 dbh +
                                 (1 | spp) +   # species-specific effects 
                                 (1 | site / tree)),  # site- and tree-specific effects
                      data = data4.5 %>% filter(!is.na(sap_volume)),
                      family = lognormal(), 
                      prior = c(set_prior("normal(3.7, 10)", class = "Intercept"), # Corresponds to roughly 40L of sap or 1L of syrup with 40:1 conversion
                                set_prior("exponential(1)", class = "sigma"),
                                set_prior("normal(0, 2)", class = "b"),
                                set_prior("normal(0, 2)", class = "sd")), # the interannual difference falls within -20L to +20L with 95% chance
                      cores = 4, chains = 4,
                      control = list(adapt_delta = 0.98, max_treedepth = 11),
                      iter = 6000,
                      seed = 1353,
                      backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod4.5.1)
plot(conditional_effects(mod4.5.1))[[1]] + ggplot2::ylim(40, 60)

# additional posterior distribution checks -------------------------------------
pp_check(mod4.5.1, ndraws = 100)
pp_check(mod4.5.1, type = "error_hist",  ndraws = 10)
pp_check(mod4.5.1, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed

# get model summary and coefficients -------------------------------------------
summary(mod4.5.1)

# effect of tap height (tap_height) on sugar content ---------------------------
# fit a truncated normal distibution, as brix cannot be negative
# include dbh (section 4.5) to account for different sized trees
mod4.5.2 <- brms::brm(brms::bf(sap_brix | trunc(lb = 0) ~
                                 (1 | year) +
                                 tap_height +
                                 dbh + 
                                 (1 | spp) + 
                                 (1 | site / tree)),
                      data = data4.5 %>% filter(!is.na(sap_brix)), # exclude NAs 
                      family = gaussian(), 
                      prior = c(set_prior("normal(2, 1)", class = "Intercept"),
                                set_prior("exponential(1)", class = "sigma"),
                                set_prior("normal(0, 2)", class = "b"),
                                set_prior("normal(0, 2)", class = "sd")),
                      cores = 4, chains = 4,
                      control = list(adapt_delta = 0.99, max_treedepth = 12),
                      iter = 6000,
                      seed = 1353,
                      backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod4.5.2)
plot(conditional_effects(mod4.5.2))[[1]] + ggplot2::ylim(2, 3)

# additional posterior distribution checks -------------------------------------
pp_check(mod4.5.2, ndraws = 100)
pp_check(mod4.5.2, type = "error_hist",  ndraws = 10)
pp_check(mod4.5.2, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed 

# get model summary and coefficients -------------------------------------------
summary(mod4.5.2)
ranef(mod4.5.2)

# conclusions ------------------------------------------------------------------
# There appears to be a small reduction in sap yield with tap height from 60 to 
# 180 cm, but there is an diminishingly small positive effect on sugar content.
#===============================================================================