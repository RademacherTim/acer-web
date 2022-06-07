#===============================================================================
# script to explore relationship between sap volume and succrose concentration 
# and number of taps
#-------------------------------------------------------------------------------

# load dependencies ------------------------------------------------------------
library("RColorBrewer")
library("vioplot")
library("brms")

# load the wrangled data -------------------------------------------------------
source("wrangle_sap_data.R")

# plot sap brix distributions for trees with one, two, and three taps ----------
vioplot(sap_brix ~ n_taps, data = sap_data, ylim = c(0, 6), las = 1, 
        xlab = expression(paste("Sap succrose content (",degree,"Brix )", sep = "")), 
        ylab = "Number of taps", 
        axes = FALSE, 
        col = "#91a9b466", # colour of area
        border = "white", # colour of outer border
        plotCentre = "points",
        colMed = "#91a9b4",
        horizontal = TRUE)

# plot sap yield distributions for trees with one, two, and three taps ---------
vioplot(sap_volume / 1e3 ~ n_taps, data = sap_data, ylim = c(0, 20), las = 1, 
        xlab = expression(paste("Sap yield (L ",cycle^-1,")", sep = "")), 
        ylab = "Number of taps", 
        axes = FALSE, 
        col = "#91a9b466", # colour of area
        border = "white", # colour of outer border
        plotCentre = "points",
        colMed = "#91a9b4",
        horizontal = TRUE)

# fit a model to determine the mean differences between one, two, or three taps 
mod0 <- brms::brm(brms::bf(mean_volume ~ n_taps),
                  data = sap_data %>% 
                    filter (sap_volume >= 100) %>% 
                    group_by(n_taps) %>% 
                    summarise(mean_volume = mean(sap_volume, na.rm = TRUE) / 1e3, 
                              mean_dbh = mean(dbh, na.rm = TRUE),
                              .groups = "keep"),
                  family = gaussian(), 
                  iter = 4000)
brms::prior_summary(mod0)
pairs(mod0)
