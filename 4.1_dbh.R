#===============================================================================
#
# Section 4.1: Tree size on sap yield and sugar content
#
# script to explore relationship between tree size (i.e. diameter at breast 
# height) with sap yield and sap sugar content (i.e., succrose concentration).
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
range(seasonal_data$dbh, na.rm = TRUE)

# remove irrelevant data -------------------------------------------------------
data4.1 <- seasonal_data %>% filter(!is.na(dbh)) %>% 
  select(site, lat, tree, year, spp, sap_volume, sap_brix, dbh) %>%
  mutate(tree = factor(tree))

# effect of size (i.e., dbh) on the stem on sap yield --------------------------
# fit a lognormal distibution 
# same model as for dbh (section 4.1) to account for different size 
# distributions between species
mod4.1.1 <- brms::brm(brms::bf(sap_volume ~
                                 (1 | year) +  # interannual differences in sap yield
                                 dbh + 
                                 (1 | tree) +  # tree-specific effects
                                 (1 | spp) +   # species-specific effects 
                                 (1 | site)),  # site-specific effects
                      data = data4.1 %>% filter(!is.na(sap_volume)),
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
plot(mod4.1.1)
plot(conditional_effects(mod4.1.1))$dbh + ggplot2::ylim(0, 120)

# additional posterior distribution checks -------------------------------------
pp_check(mod4.1.1, ndraws = 100)
pp_check(mod4.1.1, type = "error_hist",  ndraws = 10)
pp_check(mod4.1.1, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed

# get model summary and coefficients -------------------------------------------
summary(mod4.1.1)
ranef(mod4.1.1)$spp [, , "Intercept"]

# effect of size (dbh) on sugar content ----------------------------------------
# fit a truncated normal distibution, as brix cannot be negative
# same model as for dbh (section 4.1) to account for different size 
# distributions between species
mod4.1.2 <- brms::brm(brms::bf(sap_brix | trunc(lb = 0) ~
                                 (1 | year) + 
                                 dbh + 
                                 (1 | tree) +
                                 (1 | spp) + 
                                 (1 | site)),
                      data = data4.1 %>% filter(!is.na(sap_brix)), # exclude NAs 
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
plot(mod4.1.2)
plot(conditional_effects(mod4.1.2))$dbh + ggplot2::ylim(0, 5)

# additional posterior distribution checks -------------------------------------
pp_check(mod4.1.2, ndraws = 100)
pp_check(mod4.1.2, type = "error_hist",  ndraws = 10)
pp_check(mod4.1.2, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed 

# get model summary and coefficients -------------------------------------------
summary(mod4.1.2)
ranef(mod4.1.2)$spp [, , "Intercept"]

# conclusions ------------------------------------------------------------------
# Both sap yield and sugar content increase with tree size. Sap yield increases 
# super-linearly, while sugar content increases linearly (tried non-linear 
# effects, but they gave a straight line).
#===============================================================================