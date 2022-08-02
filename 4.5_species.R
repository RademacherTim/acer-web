# #===============================================================================
#
# Section 4.5: Differences in sap yield and sugar content between maple species
#
# script to explore differences in sap yield and sugar content between the three 
# maple species for which we found data. 
#-------------------------------------------------------------------------------

# load the wrangled data -------------------------------------------------------
source("0_wrangle_data.R")

# get the number of individual trees from each species -------------------------
sap_data %>% filter(!is.na(sap_volume) & sap_volume > 0) %>% count() # number of data points
seasonal_data %>% filter(!is.na(sap_volume) & sap_volume > 0) %>% count() # number of data points
seasonal_data %>% filter(!is.na(sap_volume) & sap_volume > 0 & spp == "ACSA") %>% group_by(site, tree) %>% n_groups() # number of sugar maples
seasonal_data %>% filter(!is.na(sap_volume) & sap_volume > 0 & spp == "ACRU") %>% group_by(site, tree) %>% n_groups() # number of red maples
seasonal_data %>% filter(!is.na(sap_volume) & sap_volume > 0) %>% group_by(site) %>% n_groups() # number of sites
seasonal_data %>% filter(!is.na(sap_volume) & sap_volume > 0) %>% group_by(year) %>% n_groups() # number of years

# get number of data points for sap brix ---------------------------------------
sap_data %>% filter(!is.na(sap_brix) & sap_brix > 0) %>% count() # number of data points
seasonal_data %>% filter(!is.na(sap_brix) & sap_brix > 0) %>% count() # number of data points
seasonal_data %>% filter(!is.na(sap_brix) & sap_brix > 0 & spp == "ACSA") %>% group_by(site, tree) %>% n_groups() # number of sugar maples
seasonal_data %>% filter(!is.na(sap_brix) & sap_brix > 0 & spp == "ACRU") %>% group_by(site, tree) %>% n_groups() # number of red maples
seasonal_data %>% filter(!is.na(sap_brix) & !is.na(sap_brix)) %>% group_by(site, tree, tap) %>% n_groups() # number of taps
seasonal_data %>% filter(!is.na(sap_brix) & !is.na(sap_brix)) %>% group_by(site, tree) %>% n_groups() # number of trees
seasonal_data %>% filter(!is.na(sap_brix) & !is.na(sap_brix)) %>% group_by(site) %>% n_groups() # number of sites
seasonal_data %>% filter(!is.na(sap_brix) & !is.na(sap_brix)) %>% group_by(year) %>% n_groups() # number of years

# total sap yield and mean sap sugar content for the single Norway maple ------- 
AW_data %>% filter(spp == "ACPL") %>% 
  summarise(yield = sum(sap_volume, na.rm = TRUE) / 1e3,
            brix = mean(sap_brix, na.rm = TRUE))

# remove irrelevant data -------------------------------------------------------
data4.5 <- seasonal_data %>%
  select(site, tree, tap, year, spp, log_yield, sap_volume, sap_brix, dbh) %>%
  mutate(tree = factor(tree))

# differences in sap yield between species  ------------------------------------
# fit a lognormal distibution accounting for difference in tree size
# this is identical to section 4.1, but I might consider not using dbh to be 
# able to include more data, as only relatively few trees have dbh measurements 
mod4.5.1 <- brms::brm(brms::bf(sap_volume ~
                                 (1 | year) +  # interannual differences in sap yield
                                 dbh + 
                                 (1 | tree) +  # tree-specific effects
                                 (1 | spp) +   # species-specific effects 
                                 (1 | site)),  # site-specific effects
                      data = data4.5 %>% filter(!is.na(sap_volume)),
                      family = lognormal(), 
                      prior = c(set_prior("normal(3.7, 10)", class = "Intercept"), # Corresponds to roughly 40L of sap or 1L of syrup with 40:1 conversion
                                set_prior("exponential(1)", class = "sigma"),
                                set_prior("normal(0, 2)", class = "b"),
                                set_prior("normal(0, 2)", class = "sd")), # the interannual difference falls within -20L to +20L with 95% chance
                      cores = 4, chains = 4,
                      control = list(adapt_delta = 0.98),
                      iter = 6000,
                      seed = 1353,
                      backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod4.5.1)

# additional posterior distribution checks -------------------------------------
pp_check(mod4.5.1, ndraws = 100)
pp_check(mod4.5.1, type = "error_hist",  ndraws = 10)
pp_check(mod4.5.1, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed

# get model summary and coefficients -------------------------------------------
summary(mod4.5.1)
ranef(mod4.5.1)$spp [, , "Intercept"]

# effect of the number of taps on sugar content --------------------------------
# fit a truncated normal distibution, as brix cannot be negative
mod4.5.2 <- brms::brm(brms::bf(sap_brix | trunc(lb = 0) ~
                                 (1 | year) + 
                                 dbh + # tried a non-linear effect, but it was 
                                 # basically a straight line. For parisomony's 
                                 # sake, I stick to a linear effect
                                 (1 | tree) +
                                 (1 | spp) + 
                                 (1 | site)),
                      data = data4.5 %>% filter(!is.na(sap_brix)), # exclude NAs 
                      family = gaussian(), 
                      prior = c(set_prior("normal(2, 1)", class = "Intercept"),
                                set_prior("exponential(1)", class = "sigma"),
                                set_prior("normal(0, 2)", class = "b"),
                                set_prior("normal(0, 2)", class = "sd")),
                      cores = 4, chains = 4,
                      control = list(adapt_delta = 0.99, max_treedepth = 11),
                      iter = 6000,
                      seed = 1353,
                      backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod4.5.2)

# additional posterior distribution checks -------------------------------------
pp_check(mod4.5.2, ndraws = 100)
pp_check(mod4.5.2, type = "error_hist",  ndraws = 10)
pp_check(mod4.5.2, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed 

# get model summary and coefficients -------------------------------------------
summary(mod4.5.2)
ranef(mod4.5.2)$spp [, , "Intercept"]

# conclusions ------------------------------------------------------------------
# Sugar maples provide more and sweeter sap than red maples. 
#===============================================================================