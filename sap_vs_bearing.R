#===============================================================================
# script to explore relationship between tap hole bearing, sap flow and succrose 
# concentration.
#-------------------------------------------------------------------------------

# load dependencies ------------------------------------------------------------
library("RColorBrewer")

# load the wrangled data -------------------------------------------------------
source("wrangle_sap_data.R")

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