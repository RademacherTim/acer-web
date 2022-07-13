#===============================================================================
# script to explore relationship between sap volume and succrose concentration 
# and number of taps
#-------------------------------------------------------------------------------

# load dependencies ------------------------------------------------------------
library("RColorBrewer")
library("vioplot")
library("brms")

# load the wrangled data -------------------------------------------------------
source("0_wrangle_data.R")

# plot sap brix distributions for trees with one, two, and three taps ----------
vioplot(sap_brix ~ n_taps, data = seasonal_data, ylim = c(0, 6), las = 1, 
        xlab = expression(paste("Sap succrose content (",degree,"Brix )", sep = "")), 
        ylab = "Number of taps", 
        axes = FALSE, 
        col = "#91a9b466", # colour of area
        border = "white", # colour of outer border
        plotCentre = "points",
        colMed = "#91a9b4",
        horizontal = TRUE)

# plot sap yield distributions for trees with one, two, and three taps ---------
vioplot(sap_volume ~ n_taps, data = seasonal_data, ylim = c(0, 150), las = 1, 
        xlab = expression(paste("Sap yield (L ",cycle^-1,")", sep = "")), 
        ylab = "Number of taps", 
        axes = FALSE, 
        col = "#91a9b466", # colour of area
        border = "white", # colour of outer border
        plotCentre = "points",
        colMed = "#91a9b4",
        horizontal = TRUE)

# get basic stats for manuscript text ------------------------------------------
seasonal_data %>% filter(!is.na(n_taps)) %>% group_by(site, tree, tap) %>% n_groups() # number of taps
seasonal_data %>% filter(!is.na(n_taps)) %>% group_by(site, tree) %>% n_groups() # number of trees
seasonal_data %>% filter(!is.na(n_taps)) %>% group_by(site) %>% n_groups() # number of sites
seasonal_data %>% filter(!is.na(n_taps)) %>% group_by(year) %>% n_groups() # number of years

# fit a truncated log-normal distibution to determine the effect of tap number
mod3.2.1a <- brms::brm(brms::bf(sap_volume | trunc(lb = 0) ~
                                  year +       # interannual differences in sap yield
                                  mo(n_taps) + # monotonic effect of ordinal predictor of number of taps
                                  (1 | tree) + # tree-specific effects
                                  (1 | spp) +  # species-specific effects 
                                  (1 | site)), # site-specific effects
                  data = seasonal_data,
                  family = lognormal(link = "identity"), 
                  prior = c(set_prior("normal(3.7, 10)", class = "Intercept"), # Corresponds to roughly 40L of sap or 1L of syrup with 40:1 conversion
                            set_prior("normal(0, 3)", class = "b"), # the interannual difference falls within -20L to +20L with 95% chance
                            set_prior("normal(0, 2)", class = "b", coef = "mon_taps"),
                            set_prior("dirichlet(c(1, 1))", class = "simo", coef = "mon_taps1")),
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
#brms::prior_summary(mod3.2.1a)
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
mod3.2.1b <- brms::brm(brms::bf(sap_volume | trunc(lb = 0) ~ 
                                  year + 
                                  dbh * mo(n_taps) + 
                                  (1 | tree) + # tree-specific effects
                                  (1 | spp) + 
                                  (1 | site)),
                  data = seasonal_data,
                  family = lognormal(link = "identity"), 
                  prior = c(set_prior("normal(3.7, 6)", class = "Intercept"),
                            set_prior("normal(0, 3)", class = "b"),
                            set_prior("normal(0, 2)", class = "b", coef = "mon_taps"),
                            set_prior("dirichlet(1)", class = "simo", coef = "mon_taps:dbh1"),
                            set_prior("dirichlet(1)", class = "simo", coef = "mon_taps1")),
                  cores = 4, chains = 4,
                  control = list(adapt_delta = 0.99, max_treedepth = 11),
                  iter = 6000, 
                  seed = 1353,
                  backend = "cmdstanr")
brms::prior_summary(mod3.2.1b)

# posterior distribution checks ------------------------------------------------
plot(mod3.2.1b)
plot(conditional_effects(mod3.2.1b), "mon_taps:dbh")

# additional posterior distribution checks -------------------------------------
pp_check(mod3.2.1b, ndraws = 100)
pp_check(mod3.2.1b, type = "error_hist",  ndraws = 10)
pp_check(mod3.2.1b, type = "scatter_avg", ndraws = 100)

# get model summary and coeficcients -------------------------------------------
summary(mod3.2.1b)
ranef(mod3.2.1b)

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
