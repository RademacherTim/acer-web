#===============================================================================
#
# Section 3.4: Tap orientation
#
# script to explore relationship between tap hole bearing, sap flow and succrose 
# concentration.
#-------------------------------------------------------------------------------

# load dependencies ------------------------------------------------------------
library("brms")

# load the wrangled data -------------------------------------------------------
source("0_wrangle_data.R")

# get number of data points for sap yield --------------------------------------
sap_data %>% filter(!is.na(tap_bearing) & sap_volume > 100) %>% count() # number of data points
seasonal_data %>% filter(!is.na(tap_bearing) & sap_volume > 100) %>% count() # number of data points
seasonal_data %>% filter(!is.na(tap_bearing) & sap_volume > 0) %>% group_by(site, tree, tap, year) %>% n_groups() # number of taps
seasonal_data %>% filter(!is.na(tap_bearing) & sap_volume > 0) %>% group_by(site, tree) %>% n_groups() # number of trees
seasonal_data %>% filter(!is.na(tap_bearing) & sap_volume > 0) %>% group_by(site) %>% n_groups() # number of sites
seasonal_data %>% filter(!is.na(tap_bearing) & sap_volume > 0) %>% group_by(year) %>% n_groups() # number of years

# get number of data points for sap brix ---------------------------------------
sap_data %>% filter(!is.na(tap_bearing) & sap_brix > 0) %>% count() # number of data points
seasonal_data %>% filter(!is.na(tap_bearing) & sap_brix > 0) %>% count() # number of data points
seasonal_data %>% filter(!is.na(tap_bearing) & !is.na(sap_brix)) %>% group_by(site, tree, tap, year) %>% n_groups() # number of taps
seasonal_data %>% filter(!is.na(tap_bearing) & !is.na(sap_brix)) %>% group_by(site, tree) %>% n_groups() # number of trees
seasonal_data %>% filter(!is.na(tap_bearing) & !is.na(sap_brix)) %>% group_by(site) %>% n_groups() # number of sites
seasonal_data %>% filter(!is.na(tap_bearing) & !is.na(sap_brix)) %>% group_by(year) %>% n_groups() # number of years

# remove irrelevant data -------------------------------------------------------
data3.4 <- seasonal_data %>% filter(!is.na(tap_bearing)) %>% 
  select(site, lat, tree, year, spp, sap_volume, sap_brix, sap_volume_e, sap_brix_e, 
         sap_volume_l, sap_brix_l, tap_bearing) %>%
  mutate(tree = factor(tree))

# effect of tap orientation (tap_bearing) on sap yield -------------------------
# fit a lognormal distibution (NOT accounting for site latitude)
mod3.4.1a <- brms::brm(brms::bf(sap_volume ~
                                 (1 | year) +  # interannual differences in sap yield
                                 s(tap_bearing) + # non-linear effect of tap orientation
                                 # choosing a non-linear effect, because east and west may have different effects
                                 (1 | tree) +  # tree-specific effects
                                 (1 | spp) +   # species-specific effects 
                                 (1 | site)),  # site-specific effects
                       data = data3.4 %>% filter(!is.na(sap_volume) & sap_volume > 0),
                       family = lognormal(), 
                       prior = c(set_prior("normal(3.7, 10)", class = "Intercept"), # Corresponds to roughly 40L of sap or 1L of syrup with 40:1 conversion
                                 set_prior("exponential(1)", class = "sigma"),
                                 set_prior("normal(0, 2)", class = "b"),
                                 set_prior("normal(0, 2)", class = "sd")), # the interannual difference falls within -20L to +20L with 95% chance
                       cores = 4, chains = 4,
                       control = list(adapt_delta = 0.95), max_treedepth = 11,
                       iter = 6000,
                       seed = 1353,
                       backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod3.4.1a)
plot(conditional_effects(mod3.4.1a))$tap_bearing + ggplot2::ylim(0, 50)

# additional posterior distribution checks -------------------------------------
pp_check(mod3.4.1a, ndraws = 100)
pp_check(mod3.4.1a, type = "error_hist",  ndraws = 10)
pp_check(mod3.4.1a, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed

# get model summary and coefficients -------------------------------------------
summary(mod3.4.1a)

# effect of tap orientation (tap_bearing) on early-season sap yield ------------
# fit a lognormal distibution (NOT accounting for site latitude)
mod3.4.1a_e <- brms::brm(brms::bf(sap_volume_e ~
                                  (1 | year) +  # interannual differences in sap yield
                                  s(tap_bearing) + # non-linear effect of tap orientation
                                  # choosing a non-linear effect, because east and west may have different effects
                                  (1 | tree) +  # tree-specific effects
                                  (1 | spp) +   # species-specific effects 
                                  (1 | site)),  # site-specific effects
                       data = data3.4 %>% filter(!is.na(sap_volume_e) & sap_volume_e > 0),
                       family = lognormal(), 
                       prior = c(set_prior("normal(3.7, 10)", class = "Intercept"), # Corresponds to roughly 40L of sap or 1L of syrup with 40:1 conversion
                                 set_prior("exponential(1)", class = "sigma"),
                                 set_prior("normal(0, 2)", class = "b"),
                                 set_prior("normal(0, 2)", class = "sd")), # the interannual difference falls within -20L to +20L with 95% chance
                       cores = 4, chains = 4,
                       control = list(adapt_delta = 0.9),
                       iter = 6000,
                       seed = 1353,
                       backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod3.4.1a_e)
plot(conditional_effects(mod3.4.1a_e))$tap_bearing + ggplot2::ylim(0, 50)

# additional posterior distribution checks -------------------------------------
pp_check(mod3.4.1a_e, ndraws = 100)
pp_check(mod3.4.1a_e, type = "error_hist",  ndraws = 10)
pp_check(mod3.4.1a_e, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed

# get model summary and coefficients -------------------------------------------
summary(mod3.4.1a_e)

# effect of tap orientation (tap_bearing) on late-season sap yield -------------
# fit a lognormal distibution (NOT accounting for site latitude)
mod3.4.1a_l <- brms::brm(brms::bf(sap_volume_l ~
                                   (1 | year) +  # interannual differences in sap yield
                                   s(tap_bearing) + # non-linear effect of tap orientation
                                   # choosing a non-linear effect, because east and west may have different effects
                                   (1 | tree) +  # tree-specific effects
                                   (1 | spp) +   # species-specific effects 
                                   (1 | site)),  # site-specific effects
                        data = data3.4 %>% filter(!is.na(sap_volume_l) & sap_volume_l > 0),
                        family = lognormal(), 
                        prior = c(set_prior("normal(3.7, 10)", class = "Intercept"), # Corresponds to roughly 40L of sap or 1L of syrup with 40:1 conversion
                                  set_prior("exponential(1)", class = "sigma"),
                                  set_prior("normal(0, 2)", class = "b"),
                                  set_prior("normal(0, 2)", class = "sd")), # the interannual difference falls within -20L to +20L with 95% chance
                        cores = 4, chains = 4,
                        control = list(adapt_delta = 0.9),
                        iter = 6000,
                        seed = 1353,
                        backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod3.4.1a_l)
plot(conditional_effects(mod3.4.1a_l))$tap_bearing + ggplot2::ylim(0, 50)

# additional posterior distribution checks -------------------------------------
pp_check(mod3.4.1a_l, ndraws = 100)
pp_check(mod3.4.1a_l, type = "error_hist",  ndraws = 10)
pp_check(mod3.4.1a_l, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed

# get model summary and coefficients -------------------------------------------
summary(mod3.4.1a_l)

# effect of tap orientation (tap_bearing) on sap yield -------------------------
# fit a lognormal distibution (accounting for site latitude)
mod3.4.1b <- brms::brm(brms::bf(sap_volume ~
                                 (1 | year) +  # interannual differences in sap yield
                                 s(tap_bearing * lat) + # non-linear effect of tap orientation and its interaction with latitude
                                 # choosing a non-linear effect, because east and west may have different effects
                                 (1 | tree) +  # tree-specific effects
                                 (1 | spp) +   # species-specific effects 
                                 (1 | site)),  # site-specific effects
                      data = data3.4 %>% filter(!is.na(sap_volume) & sap_volume > 0),
                      family = lognormal(), 
                      prior = c(set_prior("normal(3.7, 10)", class = "Intercept"), # Corresponds to roughly 40L of sap or 1L of syrup with 40:1 conversion
                                set_prior("exponential(1)", class = "sigma"),
                                set_prior("normal(0, 2)", class = "b"),
                                set_prior("normal(0, 2)", class = "sd")), # the interannual difference falls within -20L to +20L with 95% chance
                      cores = 4, chains = 4,
                      control = list(adapt_delta = 0.99, max_treedepth = 11),
                      iter = 6000,
                      seed = 1353,
                      backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod3.4.1b)
plot(conditional_effects(mod3.4.1b))$tap_bearing + ggplot2::ylim(0, 50)

# additional posterior distribution checks -------------------------------------
pp_check(mod3.4.1b, ndraws = 100)
pp_check(mod3.4.1b, type = "error_hist",  ndraws = 10)
pp_check(mod3.4.1b, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed

# get model summary and coefficients -------------------------------------------
summary(mod3.4.1b)

# effect of tap orientation (tap_bearing) on early-season sap yield ------------
# fit a lognormal distibution (accounting for site latitude)
mod3.4.1b_e <- brms::brm(brms::bf(sap_volume_e ~
                                    (1 | year) +  # interannual differences in sap yield
                                    s(tap_bearing * lat) + # non-linear effect of tap orientation
                                    # choosing a non-linear effect, because east and west may have different effects
                                    (1 | tree) +  # tree-specific effects
                                    (1 | spp) +   # species-specific effects 
                                    (1 | site)),  # site-specific effects
                         data = data3.4 %>% filter(!is.na(sap_volume_e) & sap_volume_e > 0),
                         family = lognormal(), 
                         prior = c(set_prior("normal(3.7, 10)", class = "Intercept"), # Corresponds to roughly 40L of sap or 1L of syrup with 40:1 conversion
                                   set_prior("exponential(1)", class = "sigma"),
                                   set_prior("normal(0, 2)", class = "b"),
                                   set_prior("normal(0, 2)", class = "sd")), # the interannual difference falls within -20L to +20L with 95% chance
                         cores = 4, chains = 4,
                         control = list(adapt_delta = 0.9),
                         iter = 6000,
                         seed = 1353,
                         backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod3.4.1b_e)
plot(conditional_effects(mod3.4.1b_e))$tap_bearing + ggplot2::ylim(0, 50)

# additional posterior distribution checks -------------------------------------
pp_check(mod3.4.1b_e, ndraws = 100)
pp_check(mod3.4.1b_e, type = "error_hist",  ndraws = 10)
pp_check(mod3.4.1b_e, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed

# get model summary and coefficients -------------------------------------------
summary(mod3.4.1b_e)

# effect of tap orientation (tap_bearing) on late-season sap yield -------------
# fit a lognormal distibution (accounting for site latitude)
mod3.4.1b_l <- brms::brm(brms::bf(sap_volume_l ~
                                    (1 | year) +  # interannual differences in sap yield
                                    s(tap_bearing * lat) + # non-linear effect of tap orientation
                                    # choosing a non-linear effect, because east and west may have different effects
                                    (1 | tree) +  # tree-specific effects
                                    (1 | spp) +   # species-specific effects 
                                    (1 | site)),  # site-specific effects
                         data = data3.4 %>% filter(!is.na(sap_volume_l) & sap_volume_l > 0),
                         family = lognormal(), 
                         prior = c(set_prior("normal(3.7, 10)", class = "Intercept"), # Corresponds to roughly 40L of sap or 1L of syrup with 40:1 conversion
                                   set_prior("exponential(1)", class = "sigma"),
                                   set_prior("normal(0, 2)", class = "b"),
                                   set_prior("normal(0, 2)", class = "sd")), # the interannual difference falls within -20L to +20L with 95% chance
                         cores = 4, chains = 4,
                         control = list(adapt_delta = 0.9),
                         iter = 6000,
                         seed = 1353,
                         backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod3.4.1b_l)
plot(conditional_effects(mod3.4.1b_l))$tap_bearing + ggplot2::ylim(0, 50)

# additional posterior distribution checks -------------------------------------
pp_check(mod3.4.1b_l, ndraws = 100)
pp_check(mod3.4.1b_l, type = "error_hist",  ndraws = 10)
pp_check(mod3.4.1b_l, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed

# get model summary and coefficients -------------------------------------------
summary(mod3.4.1b_l)

# effect of tap hole orientation on sap sugar content --------------------------
# fit a truncated normal distibution, as brix cannot be negative
# not accounting for site latitude here
mod3.4.2 <- brms::brm(brms::bf(sap_brix | trunc(lb = 0) ~
                                 (1 | year) + 
                                 s(tap_bearing) + 
                                 (1 | tree) +
                                 (1 | spp) + 
                                 (1 | site)),
                      data = data3.4 %>% filter(!is.na(sap_brix)), # exclude NAs 
                      family = gaussian(), 
                      prior = c(set_prior("normal(2, 1)", class = "Intercept"),
                                set_prior("exponential(1)", class = "sigma"),
                                set_prior("normal(0, 2)", class = "b"),
                                set_prior("normal(0, 2)", class = "sd")),
                      cores = 4, chains = 4,
                      control = list(adapt_delta = 0.95, max_treedepth = 11),
                      iter = 6000,
                      seed = 1353,
                      backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod3.4.2)
plot(conditional_effects(mod3.4.2))$tap_bearing + ggplot2::ylim(0, 3.5)

# additional posterior distribution checks -------------------------------------
pp_check(mod3.4.2, ndraws = 100)
pp_check(mod3.4.2, type = "error_hist",  ndraws = 10)
pp_check(mod3.4.2, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed 

# get model summary and coeficcients -------------------------------------------
summary(mod3.4.2)
ranef(mod3.4.2)

# effect of tap hole orientation on early-season sap sugar content -------------
# fit a truncated normal distibution, as brix cannot be negative
# not accounting for site latitude here
mod3.4.2_e <- brms::brm(brms::bf(sap_brix_e | trunc(lb = 0) ~
                                 (1 | year) + 
                                 s(tap_bearing) + 
                                 (1 | tree) +
                                 (1 | spp) + 
                                 (1 | site)),
                      data = data3.4 %>% filter(!is.na(sap_brix_e)), # exclude NAs 
                      family = gaussian(), 
                      prior = c(set_prior("normal(2, 1)", class = "Intercept"),
                                set_prior("exponential(1)", class = "sigma"),
                                set_prior("normal(0, 2)", class = "b"),
                                set_prior("normal(0, 2)", class = "sd")),
                      cores = 4, chains = 4,
                      control = list(adapt_delta = 0.95, max_treedepth = 11),
                      iter = 6000,
                      seed = 1353,
                      backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod3.4.2_e)
plot(conditional_effects(mod3.4.2_e))$tap_bearing + ggplot2::ylim(0, 3.5)

# additional posterior distribution checks -------------------------------------
pp_check(mod3.4.2_e, ndraws = 100)
pp_check(mod3.4.2_e, type = "error_hist",  ndraws = 10)
pp_check(mod3.4.2_e, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed 

# get model summary and coeficcients -------------------------------------------
summary(mod3.4.2_e)

# effect of tap hole orientation on late-season sap sugar content --------------
# fit a truncated normal distibution, as brix cannot be negative
# not accounting for site latitude here
mod3.4.2_l <- brms::brm(brms::bf(sap_brix_l | trunc(lb = 0) ~
                                   (1 | year) + 
                                   s(tap_bearing) + 
                                   (1 | tree) +
                                   (1 | spp) + 
                                   (1 | site)),
                        data = data3.4 %>% filter(!is.na(sap_brix_e)), # exclude NAs 
                        family = gaussian(), 
                        prior = c(set_prior("normal(2, 1)", class = "Intercept"),
                                  set_prior("exponential(1)", class = "sigma"),
                                  set_prior("normal(0, 2)", class = "b"),
                                  set_prior("normal(0, 2)", class = "sd")),
                        cores = 4, chains = 4,
                        control = list(adapt_delta = 0.9),
                        iter = 6000,
                        seed = 1353,
                        backend = "cmdstanr")

# posterior distribution checks ------------------------------------------------
plot(mod3.4.2_l)
plot(conditional_effects(mod3.4.2_l))$tap_bearing + ggplot2::ylim(0, 3.5)

# additional posterior distribution checks -------------------------------------
pp_check(mod3.4.2_l, ndraws = 100)
pp_check(mod3.4.2_l, type = "error_hist",  ndraws = 10)
pp_check(mod3.4.2_l, type = "scatter_avg", ndraws = 100)
# Error in the posterior distribution looks normally-distributed 

# get model summary and coeficcients -------------------------------------------
summary(mod3.4.2_l)

# get all years for which we have measurements ---------------------------------
all_years <- sap_data %>% mutate (year = as.character(year)) %>% 
  select(year) %>% unique() %>% arrange(year) %>% unlist() %>% as.numeric()

# get distinctive colours ------------------------------------------------------
colours <- brewer.pal (8, "Set3")

# set opacity for plot symbols -------------------------------------------------
opa <- 1.0

# function to select different symbols for different sites ---------------------
site.symbol <- function(s){
  n <- case_when(
    s == "1" ~ 0,
    s == "5" ~ 1,
    s == "HF" ~ 2,
    s == "DOF" ~ 3,
    s == "SMM" ~ 4,
    s == "QC" ~ 5,
    s == "DR" ~ 6,
    s == "INDU" ~ 8,
  )
  return(n)
}

# plot total harvested sap volume by cardinal direction ------------------------
par(mar = c(5, 5, 1, 1))
plot(x = sap_data %>% 
       filter (sap_volume >= 100 & site == "5") %>% 
       group_by(tree, tap, year) %>% 
       summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3,
                 bearing = mean(tap.bearing, na.rm = TRUE), .groups = "keep") %>% 
       ungroup() %>% select(bearing) %>%
       unlist(),
     y = sap_data %>% 
       filter (sap_volume >= 100 & site == "5") %>% 
       group_by(tree, tap, year) %>% 
       summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3, 
                 bearing = mean(tap.bearing, na.rm = TRUE), 
                 .groups = "keep") %>% ungroup() %>%
       select(total_volume) %>% 
       unlist(),
     pch = site.symbol("5"), 
     col = colours[which(all_years == y)],
     xlab = expression(paste("Bearing (",degree,")", sep = "")), 
     ylab = "Total sap volume (L)",
     xlim = c(0, 360), ylim = c(0, 250), axes = FALSE)
axis(side = 1, at = c(0, 90, 180, 270, 360), 
     labels = c("North", "East", "South", "West", "North"))
axis(side = 2, las = 1)

# add sites --------------------------------------------------------------------
for (s in unique(sap_data$site)){
  
  # determine the years for which we got data for the site ---------------------
  years <- unique(sap_data$year[which(sap_data$site == s)])
  
  # loop over years ------------------------------------------------------------
  for (y in years){
    
    # extract and transform relevant data --------------------------------------
    d <- sap_data %>% filter(site == s & year == y & sap_volume >= 100) %>% 
      group_by(tree, tap, year) %>% 
      summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3,
                bearing = mean(tap.bearing, na.rm = TRUE), 
                .groups = "keep") %>% 
      ungroup()
    
    # plot data points ---------------------------------------------------------
    points(x = d %>% select(bearing) %>% unlist(),
           y = d %>% select(total_volume) %>% unlist(),
           pch = site.symbol(s),
           bg = add.alpha(colours[which(all_years == y)], opa), 
           col = add.alpha(colours[which(all_years == y)], opa))
  }
}
# add legend -------------------------------------------------------------------
legend(x = 0, y = 250, legend = c("L'Assomption", "Montréal", "Harvard Forest",
                                  "Dartmouth", "Southernmost Maple", "Québec",
                                  "Divide Ridge", "Indiana Dunes")[1:3], 
       pch = site.symbol(unique(sap_data$site))[1:3], box.lty = 0)
legend(x = 90, y = 250, legend = all_years[1:3], lwd = 2, 
       col = add.alpha(colours[1:3], opa), box.lty = 0)
legend(x = 150, y = 250, legend = all_years[4:6], lwd = 2, 
       col = add.alpha(colours[4:6], opa), box.lty = 0)
legend(x = 210, y = 250, legend = all_years[7:8], lwd = 2, 
       col = add.alpha(colours[7:8], opa), box.lty = 0)

# plot total harvested sap volume over N-S gradient ----------------------------
par(mar = c(5, 5, 1, 1))
plot(x = sap_data %>% 
       filter (sap_volume >= 100 & site == "5") %>% 
       group_by(tree, tap, year) %>% 
       summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3,
                 bearing = mean(tap.bearing, na.rm = TRUE), .groups = "keep") %>% 
       ungroup() %>% select(bearing) %>%
       unlist(),
     y = sap_data %>% 
       filter (sap_volume >= 100 & site == "5") %>% 
       group_by(tree, tap, year) %>% 
       summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3, 
                 bearing = abs(mean(tap.bearing, na.rm = TRUE) - 180), # use the absolute value offset by 180 degrees, where 0 is south and 180 is north on a south-north axis, assuming that east and west don't make a difference
                 .groups = "keep") %>% ungroup() %>%
       select(total_volume) %>% 
       unlist(),
     pch = site.symbol("5"), 
     col = colours[which(all_years == y)],
     xlab = expression(paste("Bearing (",degree,")", sep = "")), 
     ylab = "Total sap volume (L)",
     xlim = c(0, 180), ylim = c(0, 250), axes = FALSE)
axis(side = 1, at = c(0, 180), labels = c("South", "North"))
axis(side = 2, las = 1)

# add sites --------------------------------------------------------------------
for (s in unique(sap_data$site)){
  
  # determine the years for which we got data for the site ---------------------
  years <- unique(sap_data$year[which(sap_data$site == s)])
  
  # loop over years ------------------------------------------------------------
  for (y in years){
    
    # extract and transform relevant data --------------------------------------
    d <- sap_data %>% filter(site == s & year == y & sap_volume >= 100) %>% 
      group_by(tree, tap, year) %>% 
      summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3,
                bearing = abs(mean(tap.bearing, na.rm = TRUE) - 180), 
                .groups = "keep") %>% 
      ungroup()
    
    # plot data points ---------------------------------------------------------
    points(x = d %>% select(bearing) %>% unlist(),
           y = d %>% select(total_volume) %>% unlist(),
           pch = site.symbol(s),
           bg = add.alpha(colours[which(all_years == y)], opa), 
           col = add.alpha(colours[which(all_years == y)], opa))
  }
}
# add legend -------------------------------------------------------------------
legend(x = 0, y = 250, legend = c("L'Assomption", "Montréal", "Harvard Forest",
                                  "Dartmouth", "Southernmost Maple", "Québec",
                                  "Divide Ridge", "Indiana Dunes")[1:3], 
       pch = site.symbol(unique(sap_data$site))[1:3], box.lty = 0)
legend(x = 70, y = 250, legend = all_years[1:3], lwd = 2, 
       col = add.alpha(colours[1:3], opa), box.lty = 0)
legend(x = 100, y = 250, legend = all_years[4:6], lwd = 2, 
       col = add.alpha(colours[4:6], opa), box.lty = 0)
legend(x = 130, y = 250, legend = all_years[7:8], lwd = 2, 
       col = add.alpha(colours[7:8], opa), box.lty = 0)

# plot mean sap succrose concentration over cardinal direction -----------------
par(mar = c(5, 5, 1, 1))
plot(x = sap_data %>% 
       filter (sap_volume >= 100 & site == "5") %>% 
       group_by(tree, tap, year) %>% 
       summarise(mean_brix = mean(sap_brix, na.rm = TRUE),
                 bearing = mean(tap.bearing, na.rm = TRUE), .groups = "keep") %>% 
       ungroup() %>% select(bearing) %>%
       unlist(),
     y = sap_data %>% 
       filter (sap_volume >= 100 & site == "5") %>% 
       group_by(tree, tap, year) %>% 
       summarise(mean_brix = mean(sap_brix, na.rm = TRUE), 
                 bearing = mean(tap.bearing, na.rm = TRUE),
                 .groups = "keep") %>% ungroup() %>%
       select(mean_brix) %>% 
       unlist(),
     pch = site.symbol("5"), 
     col = colours[which(all_years == y)],
     xlab = expression(paste("Bearing (",degree,")", sep = "")), 
     ylab =  expression(paste("Mean sap succrose concentration (",degree,"Brix)", sep = "")),
     xlim = c(0, 360), ylim = c(0, 6), axes = FALSE)
axis(side = 1, at = c(0, 90, 180, 270, 360), 
     labels = c("North", "East", "South", "West", "North"))
axis(side = 2, las = 1)

# add sites --------------------------------------------------------------------
for (s in unique(sap_data$site)){
  
  # determine the years for which we got data for the site ---------------------
  years <- unique(sap_data$year[which(sap_data$site == s)])
  
  # loop over years ------------------------------------------------------------
  for (y in years){
    
    # extract and transform relevant data --------------------------------------
    d <- sap_data %>% filter(site == s & year == y & sap_volume >= 100) %>% 
      group_by(tree, tap, year) %>% 
      summarise(mean_brix = mean(sap_brix, na.rm = TRUE),
                bearing = mean(tap.bearing, na.rm = TRUE), 
                .groups = "keep") %>% 
      ungroup()
    
    # plot data points ---------------------------------------------------------
    points(x = d %>% select(bearing) %>% unlist(),
           y = d %>% select(mean_brix) %>% unlist(),
           pch = site.symbol(s),
           bg = add.alpha(colours[which(all_years == y)], opa), 
           col = add.alpha(colours[which(all_years == y)], opa))
  }
}
# add legend -------------------------------------------------------------------
legend(x = 0, y = 6, legend = c("L'Assomption", "Montréal", "Harvard Forest",
                                  "Dartmouth", "Southernmost Maple", "Québec",
                                  "Divide Ridge", "Indiana Dunes")[1:3], 
       pch = site.symbol(unique(sap_data$site))[1:3], box.lty = 0)
legend(x = 90, y = 6, legend = all_years[1:3], lwd = 2, 
       col = add.alpha(colours[1:3], opa), box.lty = 0)
legend(x = 150, y = 6, legend = all_years[4:6], lwd = 2, 
       col = add.alpha(colours[4:6], opa), box.lty = 0)
legend(x = 210, y = 6, legend = all_years[7:8], lwd = 2, 
       col = add.alpha(colours[7:8], opa), box.lty = 0)

# plot mean sap succrose concentration over cardinal direction -----------------
par(mar = c(5, 5, 1, 1))
plot(x = sap_data %>% 
       filter (sap_volume >= 100 & site == "5") %>% 
       group_by(tree, tap, year) %>% 
       summarise(mean_brix = mean(sap_brix, na.rm = TRUE),
                 bearing = abs(mean(tap.bearing, na.rm = TRUE) - 180), .groups = "keep") %>% 
       ungroup() %>% select(bearing) %>%
       unlist(),
     y = sap_data %>% 
       filter (sap_volume >= 100 & site == "5") %>% 
       group_by(tree, tap, year) %>% 
       summarise(mean_brix = mean(sap_brix, na.rm = TRUE), 
                 bearing = abs(mean(tap.bearing, na.rm = TRUE) - 180), # use the absolute value offset by 180 degrees, where 0 is south and 180 is north on a south-north axis, assuming that east and west don't make a difference
                 .groups = "keep") %>% ungroup() %>%
       select(mean_brix) %>% 
       unlist(),
     pch = site.symbol("5"), 
     col = colours[which(all_years == y)],
     xlab = expression(paste("Bearing (",degree,")", sep = "")), 
     ylab =  expression(paste("Mean sap succrose concentration (",degree,"Brix)", sep = "")),
     xlim = c(0, 180), ylim = c(0, 6), axes = FALSE)
axis(side = 1, at = c(0, 180), labels = c("South","North"))
axis(side = 2, las = 1)

# add sites --------------------------------------------------------------------
for (s in unique(sap_data$site)){
  
  # determine the years for which we got data for the site ---------------------
  years <- unique(sap_data$year[which(sap_data$site == s)])
  
  # loop over years ------------------------------------------------------------
  for (y in years){
    
    # extract and transform relevant data --------------------------------------
    d <- sap_data %>% filter(site == s & year == y & sap_volume >= 100) %>% 
      group_by(tree, tap, year) %>% 
      summarise(mean_brix = mean(sap_brix, na.rm = TRUE),
                bearing = abs(mean(tap.bearing, na.rm = TRUE) - 180), 
                .groups = "keep") %>% 
      ungroup()
    
    # plot data points ---------------------------------------------------------
    points(x = d %>% select(bearing) %>% unlist(),
           y = d %>% select(mean_brix) %>% unlist(),
           pch = site.symbol(s),
           bg = add.alpha(colours[which(all_years == y)], opa), 
           col = add.alpha(colours[which(all_years == y)], opa))
  }
}
# add legend -------------------------------------------------------------------
legend(x = 0, y = 6, legend = c("L'Assomption", "Montréal", "Harvard Forest",
                                "Dartmouth", "Southernmost Maple", "Québec",
                                "Divide Ridge", "Indiana Dunes")[1:3], 
       pch = site.symbol(unique(sap_data$site))[1:3], box.lty = 0)
legend(x = 70, y = 6, legend = all_years[1:3], lwd = 2, 
       col = add.alpha(colours[1:3], opa), box.lty = 0)
legend(x = 100, y = 6, legend = all_years[4:6], lwd = 2, 
       col = add.alpha(colours[4:6], opa), box.lty = 0)
legend(x = 130, y = 6, legend = all_years[7:8], lwd = 2, 
       col = add.alpha(colours[7:8], opa), box.lty = 0)

# determine mid-season date ---------------------------------------------------
seasons <- sap_data %>% group_by(site, year) %>% 
  summarise(sDoy = min(doy),
            eDoy = max(doy),
            mDoy = median(doy), .groups = "drop")

# tap hole orientation effect on early-season brix -----------------------------
par(mar = c(5, 5, 1, 1))
plot(x = sap_data %>% 
       filter (sap_volume >= 100 & site == "5" & 
               doy < seasons$mDoy[which(seasons$year == 2022 & seasons$site == "5")]) %>% 
       group_by(tree, tap, year) %>% 
       summarise(mean_brix = mean(sap_brix, na.rm = TRUE),
                 bearing = abs(mean(tap.bearing, na.rm = TRUE) - 180), .groups = "keep") %>% 
       ungroup() %>% select(bearing) %>%
       unlist(),
     y = sap_data %>% 
       filter (sap_volume >= 100 & site == "5" & 
                 doy < seasons$mDoy[which(seasons$year == 2022 & seasons$site == "5")]) %>% 
       group_by(tree, tap, year) %>% 
       summarise(mean_brix = mean(sap_brix, na.rm = TRUE), 
                 bearing = abs(mean(tap.bearing, na.rm = TRUE) - 180), # use the absolute value offset by 180 degrees, where 0 is south and 180 is north on a south-north axis, assuming that east and west don't make a difference
                 .groups = "keep") %>% ungroup() %>%
       select(mean_brix) %>% 
       unlist(),
     pch = site.symbol("5"), 
     col = colours[which(all_years == y)],
     xlab = expression(paste("Bearing (",degree,")", sep = "")), 
     ylab =  expression(paste("Mean early-season sap succrose concentration (",degree,"Brix)", sep = "")),
     xlim = c(0, 180), ylim = c(0, 6), axes = FALSE)
axis(side = 1, at = c(0, 180), labels = c("South","North"))
axis(side = 2, las = 1)

# add sites --------------------------------------------------------------------
for (s in unique(sap_data$site)){
  
  # determine the years for which we got data for the site ---------------------
  years <- unique(sap_data$year[which(sap_data$site == s)])
  
  # loop over years ------------------------------------------------------------
  for (y in years){
    
    # extract and transform relevant data --------------------------------------
    d <- sap_data %>% filter(site == s & year == y & sap_volume >= 100 & 
                               doy < seasons$mDoy[which(seasons$year == y & seasons$site == s)]) %>% 
      group_by(tree, tap, year) %>% 
      summarise(mean_brix = mean(sap_brix, na.rm = TRUE),
                bearing = abs(mean(tap.bearing, na.rm = TRUE) - 180), 
                .groups = "keep") %>% 
      ungroup()
    
    # plot data points ---------------------------------------------------------
    points(x = d %>% select(bearing) %>% unlist(),
           y = d %>% select(mean_brix) %>% unlist(),
           pch = site.symbol(s),
           bg = add.alpha(colours[which(all_years == y)], opa), 
           col = add.alpha(colours[which(all_years == y)], opa))
  }
}
legend(x = 0, y = 6.2, legend = c("L'Assomption", "Montréal", "Harvard Forest",
                                "Dartmouth", "Southernmost Maple", "Québec",
                                "Divide Ridge", "Indiana Dunes")[1:3], 
       pch = site.symbol(unique(sap_data$site))[1:3], box.lty = 0,
       bg = "transparent")
legend(x = 70, y = 6.2, legend = all_years[1:3], lwd = 2, 
       col = add.alpha(colours[1:3], opa), box.lty = 0)
legend(x = 100, y = 6.2, legend = all_years[4:6], lwd = 2, 
       col = add.alpha(colours[4:6], opa), box.lty = 0)
legend(x = 130, y = 6.2, legend = all_years[7:8], lwd = 2, 
       col = add.alpha(colours[7:8], opa), box.lty = 0)

# tap hole orientation effect in late-season brix ------------------------------
par(mar = c(5, 5, 1, 1))
plot(x = sap_data %>% 
       filter (sap_volume >= 100 & site == "5" & 
                 doy >= seasons$mDoy[which(seasons$year == 2022 & seasons$site == "5")]) %>% 
       group_by(tree, tap, year) %>% 
       summarise(mean_brix = mean(sap_brix, na.rm = TRUE),
                 bearing = abs(mean(tap.bearing, na.rm = TRUE) - 180), .groups = "keep") %>% 
       ungroup() %>% select(bearing) %>%
       unlist(),
     y = sap_data %>% 
       filter (sap_volume >= 100 & site == "5" & 
                 doy >= seasons$mDoy[which(seasons$year == 2022 & seasons$site == "5")]) %>% 
       group_by(tree, tap, year) %>% 
       summarise(mean_brix = mean(sap_brix, na.rm = TRUE), 
                 bearing = abs(mean(tap.bearing, na.rm = TRUE) - 180), # use the absolute value offset by 180 degrees, where 0 is south and 180 is north on a south-north axis, assuming that east and west don't make a difference
                 .groups = "keep") %>% ungroup() %>%
       select(mean_brix) %>% 
       unlist(),
     pch = site.symbol("5"), 
     col = colours[which(all_years == y)],
     xlab = expression(paste("Bearing (",degree,")", sep = "")), 
     ylab =  expression(paste("Mean late-season sap succrose concentration (",degree,"Brix)", sep = "")),
     xlim = c(0, 180), ylim = c(0, 6), axes = FALSE)
axis(side = 1, at = c(0, 180), labels = c("South","North"))
axis(side = 2, las = 1)

# add sites --------------------------------------------------------------------
for (s in unique(sap_data$site)){
  
  # determine the years for which we got data for the site ---------------------
  years <- unique(sap_data$year[which(sap_data$site == s)])
  
  # loop over years ------------------------------------------------------------
  for (y in years){
    
    # extract and transform relevant data --------------------------------------
    d <- sap_data %>% filter(site == s & year == y & sap_volume >= 100 & 
                               doy >= seasons$mDoy[which(seasons$year == y & seasons$site == s)]) %>% 
      group_by(tree, tap, year) %>% 
      summarise(mean_brix = mean(sap_brix, na.rm = TRUE),
                bearing = abs(mean(tap.bearing, na.rm = TRUE) - 180), 
                .groups = "keep") %>% 
      ungroup()
    
    # plot data points ---------------------------------------------------------
    points(x = d %>% select(bearing) %>% unlist(),
           y = d %>% select(mean_brix) %>% unlist(),
           pch = site.symbol(s),
           bg = add.alpha(colours[which(all_years == y)], opa), 
           col = add.alpha(colours[which(all_years == y)], opa))
  }
}
legend(x = 0, y = 6, legend = c("L'Assomption", "Montréal", "Harvard Forest",
                                "Dartmouth", "Southernmost Maple", "Québec",
                                "Divide Ridge", "Indiana Dunes")[1:3], 
       pch = site.symbol(unique(sap_data$site))[1:3], box.lty = 0,
       bg = "transparent")
legend(x = 70, y = 6, legend = all_years[1:3], lwd = 2, 
       col = add.alpha(colours[1:3], opa), box.lty = 0)
legend(x = 100, y = 6, legend = all_years[4:6], lwd = 2, 
       col = add.alpha(colours[4:6], opa), box.lty = 0)
legend(x = 130, y = 6, legend = all_years[7:8], lwd = 2, 
       col = add.alpha(colours[7:8], opa), box.lty = 0)

# plot total harvested sap volume over N-S gradient in early-season ------------
par(mar = c(5, 5, 1, 1))
plot(x = sap_data %>% 
       filter (sap_volume >= 100 & site == "5" & 
                 doy < seasons$mDoy[which(seasons$year == 2022 & seasons$site == "5")]) %>% 
       group_by(tree, tap, year) %>% 
       summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3,
                 bearing = mean(tap.bearing, na.rm = TRUE), .groups = "keep") %>% 
       ungroup() %>% select(bearing) %>%
       unlist(),
     y = sap_data %>% 
       filter (sap_volume >= 100 & site == "5" & 
                 doy < seasons$mDoy[which(seasons$year == 2022 & seasons$site == "5")]) %>% 
       group_by(tree, tap, year) %>% 
       summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3, 
                 bearing = abs(mean(tap.bearing, na.rm = TRUE) - 180), # use the absolute value offset by 180 degrees, where 0 is south and 180 is north on a south-north axis, assuming that east and west don't make a difference
                 .groups = "keep") %>% ungroup() %>%
       select(total_volume) %>% 
       unlist(),
     pch = site.symbol("5"), 
     col = colours[which(all_years == y)],
     xlab = expression(paste("Bearing (",degree,")", sep = "")), 
     ylab = "Total early-season sap volume (L)",
     xlim = c(0, 180), ylim = c(0, 150), axes = FALSE)
axis(side = 1, at = c(0, 180), labels = c("South", "North"))
axis(side = 2, las = 1)

# add sites --------------------------------------------------------------------
for (s in unique(sap_data$site)){
  
  # determine the years for which we got data for the site ---------------------
  years <- unique(sap_data$year[which(sap_data$site == s)])
  
  # loop over years ------------------------------------------------------------
  for (y in years){
    
    # extract and transform relevant data --------------------------------------
    d <- sap_data %>% filter(site == s & year == y & sap_volume >= 100 & 
                               doy < seasons$mDoy[which(seasons$year == y & seasons$site == s)]) %>% 
      group_by(tree, tap, year) %>% 
      summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3,
                bearing = abs(mean(tap.bearing, na.rm = TRUE) - 180), 
                .groups = "keep") %>% 
      ungroup()
    
    # plot data points ---------------------------------------------------------
    points(x = d %>% select(bearing) %>% unlist(),
           y = d %>% select(total_volume) %>% unlist(),
           pch = site.symbol(s),
           bg = add.alpha(colours[which(all_years == y)], opa), 
           col = add.alpha(colours[which(all_years == y)], opa))
  }
}
# add legend -------------------------------------------------------------------
legend(x = 0, y = 150, legend = c("L'Assomption", "Montréal", "Harvard Forest",
                                  "Dartmouth", "Southernmost Maple", "Québec",
                                  "Divide Ridge", "Indiana Dunes")[1:3], 
       pch = site.symbol(unique(sap_data$site))[1:3], box.lty = 0, 
       bg = "transparent")
legend(x = 70, y = 150, legend = all_years[1:3], lwd = 2, 
       col = add.alpha(colours[1:3], opa), box.lty = 0, 
       bg = "transparent")
legend(x = 100, y = 150, legend = all_years[4:6], lwd = 2, 
       col = add.alpha(colours[4:6], opa), box.lty = 0, 
       bg = "transparent")
legend(x = 130, y = 150, legend = all_years[7:8], lwd = 2, 
       col = add.alpha(colours[7:8], opa), box.lty = 0, 
       bg = "transparent")

# plot total harvested sap volume over N-S gradient in late-season ------------
par(mar = c(5, 5, 1, 1))
plot(x = sap_data %>% 
       filter (sap_volume >= 100 & site == "5" & 
                 doy >= seasons$mDoy[which(seasons$year == 2022 & seasons$site == "5")]) %>% 
       group_by(tree, tap, year) %>% 
       summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3,
                 bearing = mean(tap.bearing, na.rm = TRUE), .groups = "keep") %>% 
       ungroup() %>% select(bearing) %>%
       unlist(),
     y = sap_data %>% 
       filter (sap_volume >= 100 & site == "5" & 
                 doy >= seasons$mDoy[which(seasons$year == 2022 & seasons$site == "5")]) %>% 
       group_by(tree, tap, year) %>% 
       summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3, 
                 bearing = abs(mean(tap.bearing, na.rm = TRUE) - 180), # use the absolute value offset by 180 degrees, where 0 is south and 180 is north on a south-north axis, assuming that east and west don't make a difference
                 .groups = "keep") %>% ungroup() %>%
       select(total_volume) %>% 
       unlist(),
     pch = site.symbol("5"), 
     col = colours[which(all_years == y)],
     xlab = expression(paste("Bearing (",degree,")", sep = "")), 
     ylab = "Total late-season sap volume (L)",
     xlim = c(0, 180), ylim = c(0, 150), axes = FALSE)
axis(side = 1, at = c(0, 180), labels = c("South", "North"))
axis(side = 2, las = 1)

# add sites --------------------------------------------------------------------
for (s in unique(sap_data$site)){
  
  # determine the years for which we got data for the site ---------------------
  years <- unique(sap_data$year[which(sap_data$site == s)])
  
  # loop over years ------------------------------------------------------------
  for (y in years){
    
    # extract and transform relevant data --------------------------------------
    d <- sap_data %>% filter(site == s & year == y & sap_volume >= 100 & 
                               doy >= seasons$mDoy[which(seasons$year == y & seasons$site == s)]) %>% 
      group_by(tree, tap, year) %>% 
      summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3,
                bearing = abs(mean(tap.bearing, na.rm = TRUE) - 180), 
                .groups = "keep") %>% 
      ungroup()
    
    # plot data points ---------------------------------------------------------
    points(x = d %>% select(bearing) %>% unlist(),
           y = d %>% select(total_volume) %>% unlist(),
           pch = site.symbol(s),
           bg = add.alpha(colours[which(all_years == y)], opa), 
           col = add.alpha(colours[which(all_years == y)], opa))
  }
}
# add legend -------------------------------------------------------------------
legend(x = 0, y = 150, legend = c("L'Assomption", "Montréal", "Harvard Forest",
                                  "Dartmouth", "Southernmost Maple", "Québec",
                                  "Divide Ridge", "Indiana Dunes")[1:3], 
       pch = site.symbol(unique(sap_data$site))[1:3], box.lty = 0, 
       bg = "transparent")
legend(x = 70, y = 150, legend = all_years[1:3], lwd = 2, 
       col = add.alpha(colours[1:3], opa), box.lty = 0, 
       bg = "transparent")
legend(x = 100, y = 150, legend = all_years[4:6], lwd = 2, 
       col = add.alpha(colours[4:6], opa), box.lty = 0, 
       bg = "transparent")
legend(x = 130, y = 150, legend = all_years[7:8], lwd = 2, 
       col = add.alpha(colours[7:8], opa), box.lty = 0, 
       bg = "transparent")

# plot mean sap yield over N-S gradient in early-season ------------------------
par(mar = c(5, 5, 1, 1))
plot(x = sap_data %>% 
       filter (sap_volume >= 100 & site == "5" & 
                 doy < seasons$mDoy[which(seasons$year == 2022 & seasons$site == "5")]) %>% 
       group_by(tree, tap, year) %>% 
       summarise(m_volume = mean(sap_volume, na.rm = TRUE) / 1e3,
                 bearing = mean(tap.bearing, na.rm = TRUE), .groups = "keep") %>% 
       ungroup() %>% select(bearing) %>%
       unlist(),
     y = sap_data %>% 
       filter (sap_volume >= 100 & site == "5" & 
                 doy < seasons$mDoy[which(seasons$year == 2022 & seasons$site == "5")]) %>% 
       group_by(tree, tap, year) %>% 
       summarise(m_volume = mean(sap_volume, na.rm = TRUE) / 1e3, 
                 bearing = abs(mean(tap.bearing, na.rm = TRUE) - 180), # use the absolute value offset by 180 degrees, where 0 is south and 180 is north on a south-north axis, assuming that east and west don't make a difference
                 .groups = "keep") %>% ungroup() %>%
       select(m_volume) %>% 
       unlist(),
     pch = site.symbol("5"), 
     col = colours[which(all_years == y)],
     xlab = expression(paste("Bearing (",degree,")", sep = "")), 
     ylab = "Mean early-season sap volume yield (L)",
     xlim = c(0, 180), ylim = c(0, 13), axes = FALSE)
axis(side = 1, at = c(0, 180), labels = c("South", "North"))
axis(side = 2, las = 1)

# add sites --------------------------------------------------------------------
for (s in unique(sap_data$site)){
  
  # determine the years for which we got data for the site ---------------------
  years <- unique(sap_data$year[which(sap_data$site == s)])
  
  # loop over years ------------------------------------------------------------
  for (y in years){
    
    # extract and transform relevant data --------------------------------------
    d <- sap_data %>% filter(site == s & year == y & sap_volume >= 100 & 
                               doy < seasons$mDoy[which(seasons$year == y & seasons$site == s)]) %>% 
      group_by(tree, tap, year) %>% 
      summarise(m_volume = mean(sap_volume, na.rm = TRUE) / 1e3,
                bearing = abs(mean(tap.bearing, na.rm = TRUE) - 180), 
                .groups = "keep") %>% 
      ungroup()
    
    # plot data points ---------------------------------------------------------
    points(x = d %>% select(bearing) %>% unlist(),
           y = d %>% select(m_volume) %>% unlist(),
           pch = site.symbol(s),
           bg = add.alpha(colours[which(all_years == y)], opa), 
           col = add.alpha(colours[which(all_years == y)], opa))
  }
}
# add legend -------------------------------------------------------------------
legend(x = 0, y = 13, legend = c("L'Assomption", "Montréal", "Harvard Forest",
                                  "Dartmouth", "Southernmost Maple", "Québec",
                                  "Divide Ridge", "Indiana Dunes")[1:3], 
       pch = site.symbol(unique(sap_data$site))[1:3], box.lty = 0, 
       bg = "transparent")
legend(x = 70, y = 13, legend = all_years[1:3], lwd = 2, 
       col = add.alpha(colours[1:3], opa), box.lty = 0, 
       bg = "transparent")
legend(x = 100, y = 13, legend = all_years[4:6], lwd = 2, 
       col = add.alpha(colours[4:6], opa), box.lty = 0, 
       bg = "transparent")
legend(x = 130, y = 13, legend = all_years[7:8], lwd = 2, 
       col = add.alpha(colours[7:8], opa), box.lty = 0, 
       bg = "transparent")

# plot mean sap yield over N-S gradient in late-season -------------------------
par(mar = c(5, 5, 1, 1))
plot(x = sap_data %>% 
       filter (sap_volume >= 100 & site == "5" & 
                 doy >= seasons$mDoy[which(seasons$year == 2022 & seasons$site == "5")]) %>% 
       group_by(tree, tap, year) %>% 
       summarise(m_volume = mean(sap_volume, na.rm = TRUE) / 1e3,
                 bearing = mean(tap.bearing, na.rm = TRUE), .groups = "keep") %>% 
       ungroup() %>% select(bearing) %>%
       unlist(),
     y = sap_data %>% 
       filter (sap_volume >= 100 & site == "5" & 
                 doy >= seasons$mDoy[which(seasons$year == 2022 & seasons$site == "5")]) %>% 
       group_by(tree, tap, year) %>% 
       summarise(m_volume = mean(sap_volume, na.rm = TRUE) / 1e3, 
                 bearing = abs(mean(tap.bearing, na.rm = TRUE) - 180), # use the absolute value offset by 180 degrees, where 0 is south and 180 is north on a south-north axis, assuming that east and west don't make a difference
                 .groups = "keep") %>% ungroup() %>%
       select(m_volume) %>% 
       unlist(),
     pch = site.symbol("5"), 
     col = colours[which(all_years == y)],
     xlab = expression(paste("Bearing (",degree,")", sep = "")), 
     ylab = "Mean late-season sap volume yield (L)",
     xlim = c(0, 180), ylim = c(0, 13), axes = FALSE)
axis(side = 1, at = c(0, 180), labels = c("South", "North"))
axis(side = 2, las = 1)

# add sites --------------------------------------------------------------------
for (s in unique(sap_data$site)){
  
  # determine the years for which we got data for the site ---------------------
  years <- unique(sap_data$year[which(sap_data$site == s)])
  
  # loop over years ------------------------------------------------------------
  for (y in years){
    
    # extract and transform relevant data --------------------------------------
    d <- sap_data %>% filter(site == s & year == y & sap_volume >= 100 & 
                               doy >- seasons$mDoy[which(seasons$year == y & seasons$site == s)]) %>% 
      group_by(tree, tap, year) %>% 
      summarise(m_volume = mean(sap_volume, na.rm = TRUE) / 1e3,
                bearing = abs(mean(tap.bearing, na.rm = TRUE) - 180), 
                .groups = "keep") %>% 
      ungroup()
    
    # plot data points ---------------------------------------------------------
    points(x = d %>% select(bearing) %>% unlist(),
           y = d %>% select(m_volume) %>% unlist(),
           pch = site.symbol(s),
           bg = add.alpha(colours[which(all_years == y)], opa), 
           col = add.alpha(colours[which(all_years == y)], opa))
  }
}
# add legend -------------------------------------------------------------------
legend(x = 0, y = 13, legend = c("L'Assomption", "Montréal", "Harvard Forest",
                                 "Dartmouth", "Southernmost Maple", "Québec",
                                 "Divide Ridge", "Indiana Dunes")[1:3], 
       pch = site.symbol(unique(sap_data$site))[1:3], box.lty = 0, 
       bg = "transparent")
legend(x = 70, y = 13, legend = all_years[1:3], lwd = 2, 
       col = add.alpha(colours[1:3], opa), box.lty = 0, 
       bg = "transparent")
legend(x = 100, y = 13, legend = all_years[4:6], lwd = 2, 
       col = add.alpha(colours[4:6], opa), box.lty = 0, 
       bg = "transparent")
legend(x = 130, y = 13, legend = all_years[7:8], lwd = 2, 
       col = add.alpha(colours[7:8], opa), box.lty = 0, 
       bg = "transparent")
#===============================================================================