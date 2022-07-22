#===============================================================================
#
# Section 3.2: Tap width
#
# script to explore relationship between sap yield or sugar content and tap hole 
# width
#-------------------------------------------------------------------------------

# load the wrangled data -------------------------------------------------------
source("0_wrangle_data.R")

# plot tap depth against sap yield ---------------------------------------------
par(mar = c(5, 5, 1, 1))
boxplot(sap_volume ~ tap_width, data = seasonal_data,
        xlab = "Tap hole width (mm)", ylab = "Total sap yield (L)",
        ylim = c(0, 200), axes = FALSE)
axis(side = 1, at = 1, labels = "~7.9 mm")
axis(side = 2, las = 1)

# get number of data points that report tap width ------------------------------
sap_data %>% filter(!is.na(tap_width) & sap_volume > 100) %>% count() # number of data points
seasonal_data %>% filter(!is.na(tap_width) & sap_volume > 0) %>% count() # number of data points
seasonal_data %>% filter(!is.na(tap_width) & sap_volume > 0) %>% group_by(site, tree, tap) %>% n_groups() # number of taps
seasonal_data %>% filter(!is.na(tap_width) & sap_volume > 0) %>% group_by(site, tree) %>% n_groups() # number of trees
seasonal_data %>% filter(!is.na(tap_width) & sap_volume > 0 & n_taps == 1) %>% group_by(site, tree) %>% n_groups() # number of trees with one tap
seasonal_data %>% filter(!is.na(tap_width) & sap_volume > 0 & n_taps == 2) %>% group_by(site, tree) %>% n_groups() # number of trees with one tap
seasonal_data %>% filter(!is.na(tap_width) & sap_volume > 0 & n_taps == 3) %>% group_by(site, tree) %>% n_groups() # number of trees with one tap
seasonal_data %>% filter(!is.na(tap_width) & sap_volume > 0) %>% group_by(site) %>% n_groups() # number of sites
seasonal_data %>% filter(!is.na(tap_width) & sap_volume > 0) %>% group_by(year) %>% n_groups() # number of years

# plot tap_depth against sap brix ----------------------------------------------
par(mar = c(5, 5, 1, 1))
boxplot(sap_brix ~ round(tap_width), data = seasonal_data,
        xlab = "Tap hole width (mm)", 
        ylab = expression(paste("Mean sap succrose concentration (",degree," Brix)", sep = "")),
        ylim = c(0, 6), axes = FALSE)
axis(side = 1, at = 1, label = "~7.9 mm")
axis(side = 2, las = 1)

# get number of data points that report tap width ------------------------------
sap_data %>% filter(!is.na(tap_width) & sap_brix > 0) %>% count() # number of data points
seasonal_data %>% filter(!is.na(tap_width) & sap_brix > 0) %>% count() # number of data points
seasonal_data %>% filter(!is.na(tap_width) & sap_brix > 0) %>% group_by(site, tree, tap) %>% n_groups() # number of taps
seasonal_data %>% filter(!is.na(tap_width) & sap_brix > 0) %>% group_by(site, tree) %>% n_groups() # number of trees
seasonal_data %>% filter(!is.na(tap_width) & sap_brix > 0 & n_taps == 1) %>% group_by(site, tree) %>% n_groups() # number of trees with one tap
seasonal_data %>% filter(!is.na(tap_width) & sap_brix > 0 & n_taps == 2) %>% group_by(site, tree) %>% n_groups() # number of trees with one tap
seasonal_data %>% filter(!is.na(tap_width) & sap_brix > 0 & n_taps == 3) %>% group_by(site, tree) %>% n_groups() # number of trees with one tap
seasonal_data %>% filter(!is.na(tap_width) & sap_brix > 0) %>% group_by(site) %>% n_groups() # number of sites
seasonal_data %>% filter(!is.na(tap_width) & sap_brix > 0) %>% group_by(year) %>% n_groups() # number of years

#===============================================================================