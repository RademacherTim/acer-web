#===============================================================================
#
# Section 2.1: Sources of variability
#
# script to quantify the relative importance of various factors in introducing 
# variability in sap yield and sugar content. Factors we account for are:
# - year
# - site
# - tree
# - tap
# - species
#-------------------------------------------------------------------------------

# load dependencies ------------------------------------------------------------
library("brms")
library("tidybayes")

# load the wrangled data -------------------------------------------------------
source("0_wrangle_data.R")

# use only relevant data -------------------------------------------------------
data2 <- seasonal_data %>%
  select(site, tree, tap, year, spp, sap_volume, sap_brix) %>%
  mutate(tree = factor(tree))

# quantify sources of variability ----------------------------------------------
# fit a lognormal distibution
mod2.1 <- brms::brm(brms::bf(sap_volume ~
                    (1 | year) +  # year-specific effects
                    (1 | tap) +   # tap-specific effects
                    (1 | tree) +  # tree-specific effects
                    (1 | spp) +   # species-specific effects 
                    (1 | site)),  # site-specific effects
                    data = data2 %>% filter(!is.na(sap_volume)),
                    family = lognormal(), 
                    prior = c(set_prior("normal(3.7, 10)", class = "Intercept"), # Corresponds to roughly 40L of sap or 1L of syrup with 40:1 conversion
                              set_prior("exponential(1)", class = "sigma"),
                              set_prior("normal(0, 2)", class = "sd")), # the interannual difference falls within -20L to +20L with 95% chance
                    cores = 4, chains = 4,
                    control = list(adapt_delta = 0.9, max_treedepth = 10), # model looks good
                    iter = 6000,
                    seed = 1353,
                    backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod2.1)

# additional posterior distribution checks -------------------------------------
pp_check(mod2.1, ndraws = 100)
pp_check(mod2.1, type = "error_hist",  ndraws = 10)
pp_check(mod2.1, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed

# get model summary and coefficients -------------------------------------------
summary(mod2.1)
ranef(mod2.1)$n_taps[, , "Intercept"]
ranef(mod2.1)$tree[, , "Intercept"]
ranef(mod2.1)$spp[, , "Intercept"]
ranef(mod2.1)$site[, , "Intercept"]
