#===============================================================================
# script to explore relationship between sap yield or sugar content and tap hole 
# depth
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

# plot size against volume -----------------------------------------------------
par(mar = c(5, 5, 1, 1))
plot(x = sap_data %>% 
       filter (sap_volume >= 100 & site == "5") %>% 
       group_by(tree, tap, year) %>% 
       summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3,
                 mean_dbh = mean(dbh, na.rm = TRUE), .groups = "keep") %>% 
       ungroup() %>% select(mean_dbh) %>%
       unlist(),
     y = sap_data %>% 
       filter (sap_volume >= 100 & site == "5") %>% 
       group_by(tree, tap, year) %>% 
       summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3, 
                 mean_dbh = mean(dbh, na.rm = TRUE),
                 .groups = "keep") %>% ungroup() %>%
       select(total_volume) %>% 
       unlist(),
     pch = site.symbol("5"), 
     col = colours[which(all_years == y)],
     xlab = "Tree diameter at breast height (cm)", ylab = "Total sap volume (L)",
     xlim = c(0, 100), ylim = c(0, 200), axes = FALSE)
axis(side = 1)
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
                mean_dbh = mean(dbh, na.rm = TRUE), .groups = "keep") %>% 
      ungroup()
    
    # plot data points ---------------------------------------------------------
    points(x = d %>% select(mean_dbh) %>% unlist(),
           y = d %>% select(total_volume) %>% unlist(),
           pch = site.symbol(s),
           bg = add.alpha(colours[which(all_years == y)], opa), 
           col = add.alpha(colours[which(all_years == y)], opa))
  }
}
# add legend -------------------------------------------------------------------
legend(x = 0, y = 200, legend = c("L'Assomption", "Montréal", "Harvard Forest",
                                  "Dartmouth", "Southernmost Maple", "Québec",
                                  "Divide Ridge", "Indiana Dunes")[1:3], 
       pch = site.symbol(unique(sap_data$site))[1:3], box.lty = 0)
legend(x = 0, y = 150, legend = all_years[c(1,5,6,8)], lwd = 2, 
       col = add.alpha(colours[c(1,5,6,8)], opa), box.lty = 0)

# fit linear model to sap_volume over dbh relationship -------------------------
l_mod0 <- lm(total_volume ~ mean_dbh, data = sap_data %>% 
               filter (sap_volume >= 100) %>% 
               group_by(tree, tap, year) %>% 
               summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3, 
                         mean_dbh = mean(dbh, na.rm = TRUE),
                         .groups = "keep"))
abline(l_mod0, col = "#666666", lty = 2, lwd = 2)
summary(l_mod0)
mod1 <- brms::brm(brms::bf(total_volume ~ s(mean_dbh)),
                  data = sap_data %>% 
                    filter (sap_volume >= 100) %>% 
                    group_by(tree, tap, year) %>% 
                    summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3, 
                              mean_dbh = mean(dbh, na.rm = TRUE),
                              .groups = "keep"),
                  family = gaussian(), 
                  iter = 4000,
                  prior = brms::prior(normal(1000, 500, 500), coef = s(mean_dbh)))
brms::prior_summary(mod1)
pairs(mod1)
# per cm you gain about a liter over the season 

# plot size against sap brix content -------------------------------------------
par(mar = c(5, 5, 1, 1))
plot(x = sap_data %>% 
       filter (sap_volume >= 100 & site == "5") %>% 
       group_by(tree, tap, year) %>% 
       summarise(mean_brix = mean(sap_brix, na.rm = TRUE),
                 mean_dbh = mean(dbh, na.rm = TRUE), .groups = "keep") %>% 
       ungroup() %>% select(mean_dbh) %>%
       unlist(),
     y = sap_data %>% 
       filter (sap_volume >= 100 & site == "5") %>% 
       group_by(tree, tap, year) %>% 
       summarise(mean_brix = mean(sap_brix, na.rm = TRUE), 
                 mean_dbh = mean(dbh, na.rm = TRUE),
                 .groups = "keep") %>% ungroup() %>%
       select(mean_brix) %>% 
       unlist(),
     pch = site.symbol("5"), 
     col = colours[which(all_years == y)],
     xlab = "Tree diameter at breast height (cm)", 
     ylab = expression(paste("Mean sap succrose concentration (",degree," Brix)", sep = "")),
     xlim = c(0, 100), ylim = c(0, 6), axes = FALSE)
axis(side = 1)
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
                mean_dbh = mean(dbh, na.rm = TRUE),
                .groups = "keep") %>% 
      ungroup()
    
    # plot data points ---------------------------------------------------------
    points(x = d %>% select(mean_dbh) %>% unlist(),
           y = d %>% select(mean_brix) %>% unlist(),
           pch = site.symbol(s),
           bg = add.alpha(colours[which(all_years == y)], opa + 0.1), 
           col = add.alpha(colours[which(all_years == y)], opa))
  }
}
# add legend -------------------------------------------------------------------
legend(x = 0, y = 6, legend = c("L'Assomption", "Montréal", "Harvard Forest",
                                "Dartmouth", "Southernmost Maple", "Québec",
                                "Divide Ridge", "Indiana Dunes")[1:3], 
       pch = site.symbol(unique(sap_data$site))[1:3], box.lty = 0)
legend(x = 0, y = 4.6, legend = all_years[c(1,5,6,8)], lwd = 2, 
       col = add.alpha(colours[c(1,5,6,8)], opa + 0.1), box.lty = 0)

# plot sap brix content over volume --------------------------------------------
opa <- 0.6
par(mar = c(5, 5, 1, 1))
plot(x = sap_data %>% 
       filter (sap_volume >= 100 & site == "5") %>% 
       group_by(tree, tap, year) %>% 
       summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3,
                 mean_brix = mean(sap_brix, na.rm = TRUE), .groups = "keep") %>% 
       ungroup() %>% select(total_volume) %>%
       unlist(),
     y = sap_data %>% 
       filter (sap_volume >= 100 & site == "5") %>% 
       group_by(tree, tap, year) %>% 
       summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3, 
                 mean_brix = mean(sap_brix, na.rm = TRUE), .groups = "keep") %>%
       ungroup() %>% select(mean_brix) %>% 
       unlist(),
     pch = site.symbol("5"), col = colours[which(years == 2022)],
     xlab = "Total sap harvest (L)", 
     ylab = expression(paste("Mean sap succrose concentration (",degree," Brix)", sep = "")),
     xlim = c(0, 200), ylim = c(0, 6), axes = FALSE)
axis(side = 1)
axis(side = 2, las = 1)
for (s in unique(sap_data$site)){
  
  # determine the years for which we got data for the site ---------------------
  years <- unique(sap_data$year[which(sap_data$site == s)])
  
  # loop over years ------------------------------------------------------------
  for (y in years){
    
    # extract and transform relevant data --------------------------------------
    d <- sap_data %>% filter(site == s & year == y & sap_volume >= 100) %>% 
      group_by(tree, tap, year) %>% 
      summarise(mean_brix = mean(sap_brix, na.rm = TRUE), 
                total_volume = sum(sap_volume, na.rm = TRUE) / 1e3, 
                .groups = "keep") %>% 
      ungroup()
    
    # plot data points ---------------------------------------------------------
    points(x = d %>% select(total_volume) %>% unlist(),
           y = d %>% select(mean_brix) %>% unlist(),
           pch = site.symbol(s),
           bg = add.alpha(colours[which(all_years == y)], opa + 0.1), 
           col = add.alpha(colours[which(all_years == y)], opa))
  }
}
# add legend -------------------------------------------------------------------
legend(x = 8, y = 6.2, legend = c("L'Assomption", "Montréal", "Harvard Forest",
                                  "Dartmouth"), 
       pch = site.symbol(unique(sap_data$site))[1:4], box.lty = 0, 
       bg = "transparent", cex = 0.9)
legend(x = 53, y = 6.2, legend = c("Southernmost Maple", "Québec",
                                   "Divide Ridge", "Indiana Dunes"), 
       pch = site.symbol(unique(sap_data$site))[5:8], box.lty = 0, 
       bg = "transparent", cex = 0.9)
legend(x = 110, y = 6.2, legend = all_years[1:3], lwd = 2, 
       col = add.alpha(colours[1:3], opa + 0.1), box.lty = 0, bg = "transparent")
legend(x = 140, y = 6.2, legend = all_years[4:6], lwd = 2, 
       col = add.alpha(colours, opa + 0.1)[4:6], box.lty = 0, bg = "transparent")
legend(x = 170, y = 6.2, legend = all_years[7:8], lwd = 2, 
       col = add.alpha(colours, opa + 0.1)[7:8], box.lty = 0, bg = "transparent")

# TR - Add data from Michaël and maybe Yvon Grenier to the model
#===============================================================================