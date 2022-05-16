#===============================================================================
# script to explore relationship between sap volume and succrose concentration 
# and species (i.e., Acer rubrum, Acer saccharum, and Acer platanoides).
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

# plot total harvested sap volume by species -----------------------------------
par(mar = c(5, 5, 1, 1))
boxplot(x = sap_data %>% 
       filter (sap_volume >= 100) %>% 
       group_by(site, spp, tree, tap) %>% 
       summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3,
                 mean_brix = mean(sap_brix, na.rm = TRUE), .groups = "keep") %>% 
       filter(spp == "ACSA") %>% ungroup() %>% select(total_volume) %>%
       unlist(),
     col = "#91b9a4",
     xlab = "", ylab = "Total sap volume (L)", main = "", 
     xlim = c(0, 4), ylim = c(0.1, 1100), axes = FALSE, log = "y")
boxplot(x = sap_data %>% 
          filter (sap_volume >= 100) %>% 
          group_by(site, spp, tree, tap) %>% 
          summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3,
                    mean_brix = mean(sap_brix, na.rm = TRUE), .groups = "keep") %>% 
          filter(spp == "ACRU") %>% ungroup() %>% select(total_volume) %>%
          unlist(), at = 2,
        add = TRUE, axes = FALSE)
boxplot(x = sap_data %>% 
          filter (sap_volume >= 100) %>% 
          group_by(site, spp, tree, tap) %>% 
          summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3,
                    mean_brix = mean(sap_brix, na.rm = TRUE), .groups = "keep") %>% 
          filter(spp == "ACPL") %>% ungroup() %>% select(total_volume) %>%
          unlist(), at = 3,
        add = TRUE, axes = FALSE)
axis(side = 1, at = 1:3, 
     labels = c("Acer saccharum", "Acer rubrum", "Acer platanoides"))
axis(side = 2, las = 1, at = c(1, 10, 100, 1000))

# plot mean sap succrose concetration by species -------------------------------
par(mar = c(5, 5, 1, 1))
boxplot(x = sap_data %>% 
          filter (sap_volume >= 100) %>% 
          group_by(site, spp, tree, tap) %>% 
          summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3,
                    mean_brix = mean(sap_brix, na.rm = TRUE), .groups = "keep") %>% 
          filter(spp == "ACSA") %>% ungroup() %>% select(mean_brix) %>%
          unlist(),
        col = "#91b9a4",
        xlab = "",
        ylab = expression(paste("Mean sap succrose concentration (",degree,"Brix)", sep = "")),
        main = "",
        xlim = c(0, 4), ylim = c(0, 5), axes = FALSE)
boxplot(x = sap_data %>% 
          filter (sap_volume >= 100) %>% 
          group_by(site, spp, tree, tap) %>% 
          summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3,
                    mean_brix = mean(sap_brix, na.rm = TRUE), .groups = "keep") %>% 
          filter(spp == "ACRU") %>% ungroup() %>% select(mean_brix) %>%
          unlist(), at = 2,
        add = TRUE, axes = FALSE)
boxplot(x = sap_data %>% 
          filter (sap_volume >= 100) %>% 
          group_by(site, spp, tree, tap) %>% 
          summarise(total_volume = sum(sap_volume, na.rm = TRUE) / 1e3,
                    mean_brix = mean(sap_brix, na.rm = TRUE), .groups = "keep") %>% 
          filter(spp == "ACPL") %>% ungroup() %>% select(mean_brix) %>%
          unlist(), at = 3,
        add = TRUE, axes = FALSE)
axis(side = 1, at = 1:3, 
     labels = c("Acer saccharum", "Acer rubrum", "Acer platanoides"))
axis(side = 2, las = 1, at = 0:5)

#===============================================================================