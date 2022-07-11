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

# fit a model to determine the mean differences between one, two, or three taps 
mod0 <- brms::brm(brms::bf(sap_volume ~ year + factor(n_taps) + (1 | spp) + 
                             (1 | site)),
                  data = seasonal_data,
                  family = gaussian(), 
                  iter = 4000)
#brms::prior_summary(mod0)
summary(mod0)

# include an interaction between dbh and n_taps in the model to see whether size 
# affects the amount of additional sap per tap ---------------------------------
mod1 <- brms::brm(brms::bf(sap_volume ~ year + dbh * factor(n_taps) + 
                             (1 | spp) + (1 | site)),
                  data = seasonal_data,
                  family = gaussian(), 
                  iter = 4000)
#brms::prior_summary(mod0)
summary(mod1)
