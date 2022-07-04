#===============================================================================
# script to explore relationship between sap yield or sugar content and tap hole 
# width
#-------------------------------------------------------------------------------

# load the wrangled data -------------------------------------------------------
source("0_wrangle_data.R")

# plot tap depth against sap yield ---------------------------------------------
par(mar = c(5, 5, 1, 1))
boxplot(sap_volume ~ round(tap_width), data = seasonal_data,
        xlab = "Tap hole depth (cm)", ylab = "Total sap yield (L)",
        ylim = c(0, 200), axes = FALSE)
axis(side = 1, at = 1, labels = "~5 cm")
axis(side = 2, las = 1)

# get number of data points that report tap depth ------------------------------
sap_data %>% filter(!is.na(tap_width))
seasonal_data %>% filter(!is.na(tap_width))

# plot tap_depth against sap brix ----------------------------------------------
par(mar = c(5, 5, 1, 1))
boxplot(sap_brix ~ round(tap_width), data = seasonal_data,
        xlab = "Tree diameter at breast height (cm)", 
        ylab = expression(paste("Mean sap succrose concentration (",degree," Brix)", sep = "")),
        ylim = c(0, 6), axes = FALSE)
axis(side = 1, at = 1, label = "~5 cm")
axis(side = 2, las = 1)

#===============================================================================