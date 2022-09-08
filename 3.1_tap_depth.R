#===============================================================================
#
# Section 3.1: Tap depth
#
# script to explore relationship between sap yield or sugar content and tap hole 
# depth
#-------------------------------------------------------------------------------

# load the wrangled data -------------------------------------------------------
source("0_wrangle_data.R")

# plot tap depth against sap yield ---------------------------------------------
par(mar = c(5, 5, 1, 1))
boxplot(sap_volume ~ tap_depth, data = seasonal_data,
     xlab = "Tap hole depth (cm)", ylab = "Total sap yield (L)",
     ylim = c(0, 200))#, axes = FALSE)
#axis(side = 1)#, labels = "~5 cm")
#axis(side = 2, las = 1)

# get number of data points that report tap depth ------------------------------
sap_data %>% filter(!is.na(tap_depth) & sap_volume > 100) %>% count()
seasonal_data %>% filter(!is.na(tap_depth) & sap_volume > 0) %>% count()

# plot tap_depth against sap brix ----------------------------------------------
par(mar = c(5, 5, 1, 1))
boxplot(sap_brix ~ round(tap_depth), data = seasonal_data,
     xlab = "Tap hole depth (cm)", 
     ylab = expression(paste("Mean sap succrose concentration (",degree," Brix)", sep = "")),
     ylim = c(0, 6))#, axes = FALSE)
#axis(side = 1, at = 1, label = "~5 cm")
#axis(side = 2, las = 1)

# get number of data points that report tap depth ------------------------------
sap_data %>% filter(!is.na(tap_depth) & sap_brix > 0) %>% count()
seasonal_data %>% filter(!is.na(tap_depth) & sap_brix > 0) %>% count()

# get range of tap depth, bark thickness, and effective tap deppth -------------
# N.B.: both were only measured in l'Assomption
range(sap_data$tap_depth, na.rm = T)
range(sap_data$bark_thickness, na.rm = T)
range(sap_data$tap_depth - (sap_data$bark_thickness / 10), na.rm = T)
#===============================================================================