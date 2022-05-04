#===============================================================================
# script to download, plot, and explore sugar season data
#-------------------------------------------------------------------------------

# authenthicate for spreadsheet and load the data from the acer-web sheet ------
gs4_auth(cache = ".secrets", email = "rademacher.tim@gmail.com")

# get data from online sheet ---------------------------------------------------
sap_data  <- read_sheet (ss = sheet_url, sheet = "01_sap_data",  
                         na = "NA",
                         col_types = "ciDcldddddddlc")
# met_data  <- read_sheet (ss = sheet_url, sheet = "02_met_data",  
#                          na = "NA",
#                          col_types = "cDlccdddcc")
# tree_data <- read_sheet (ss = sheet_url, sheet = "03_tree_data", 
#                          na = "NA",
#                          col_types = "cicddddddd")
# site_data <- read_sheet (ss = sheet_url, sheet = "04_site_data", 
#                          na = "NA",
#                          col_types = "cdddDci")

# filter data for the site under consideration ---------------------------------
sap_data <- sap_data %>% filter(site == "1 - L'Assomption")
# met_data <- met_data %>% filter(site == input$site)
# tree_data <- tree_data %>% filter(site == input$site)
# site_data <- site_data %>% filter(site == input$site)

# create datetime column -------------------------------------------------------
sap_data <- sap_data %>% 
  mutate(datetime = as_datetime (paste(date, time), 
                                 format = "%Y-%m-%d %H:%M", tz = "EST"))

# calculate mean sap succrose concentration (°Brix) ----------------------------
sap_data <- sap_data %>% 
  mutate(sap_brix = rowMeans(select(., sap_brix_1, sap_brix_2, sap_brix_3), 
                             na.rm = TRUE),
         bucket_brix = rowMeans(select(., bucket_brix_1, bucket_brix_2, bucket_brix_3), 
                                na.rm = TRUE))

# add day of year column to sap_data -------------------------------------------
sap_data <- sap_data %>% mutate(doy = yday(date))

# plot succrose concentration (°Brix) over time --------------------------------
par(mar = c(3, 5, 1, 1))
plot (x = sap_data %>% filter (sap_volume >= 100) %>% group_by(date) %>% 
        group_keys(),
      y = sap_data %>% filter (sap_volume >= 100) %>% group_by(date) %>% 
        summarise(mean_brix = mean(sap_brix, na.rm = TRUE)) %>% 
        select(mean_brix) %>% unlist(), 
      typ = "p", pch = 19, col = "#CC724066", 
      cex = sap_data %>% filter(!is.nan(sap_brix) & sap_volume >= 100) %>% 
        group_by(date) %>% tally() %>% select (n) %>% mutate(n = n / 2) %>% 
        sqrt() %>% unlist(),
      xlim = c(as_date("2022-02-28"), as_date("2022-05-01")), ylim = c(0, 16),
      axes = FALSE,
      xlab = "", 
      ylab = expression(paste("Sap succrose concentration (",degree,"Brix)", sep = "")))
axis(side = 1, at = as_date(c("2022-03-01","2022-04-01","2022-05-01")), 
     labels = c("March","April","May"))
axis(side = 2, las = 1, at = seq(0, 15, by = 5))
segments()
points (x = sap_data %>% filter (sap_volume >= 100) %>% group_by(date) %>% 
          group_keys() %>% unlist(),
        y = sap_data %>% filter (sap_volume >= 100) %>% group_by(date) %>% 
          summarise(mean_brix = mean(bucket_brix, na.rm = TRUE)) %>% 
          select(mean_brix) %>% unlist(), 
        pch = 19, col = "#B8AB9E66",
        cex = sap_data %>% 
          filter(!is.nan(bucket_brix) & sap_volume >= 100) %>% group_by(date) %>% 
          tally() %>% select (n)  %>% mutate(n = n / 2) %>% sqrt() %>% unlist())
abline(lm(sap_brix ~ date, data = sap_data %>% filter(sap_volume >= 100)), 
       col = "#CC7240", lty = 2, lwd = 2)
abline(lm(bucket_brix ~ date, data = sap_data %>% filter(sap_volume >= 100)), 
       col = "#B8AB9E66", lty = 2, lwd = 2)

# remove outliers on 2022-03-12 due to most sap being frozen and 2022-03-14, as 
# there was only very little sap (i.e., 50 or 100 ml with one tree at 300 ml) --
sap_data <- sap_data %>% filter(date != as_date("2022-03-12") & 
                                date != as_date("2022-03-14"))

# re-plot succrose concentration (°Brix) over time without  ---------------
par(mar = c(3, 5, 1, 1))
plot (x = sap_data %>% 
        filter (sap_volume >= 100) %>% 
        group_by(doy) %>% group_keys() %>% unlist(),
      y = sap_data %>% 
        filter (sap_volume >= 100) %>% 
        group_by(doy) %>% 
        summarise(mean_brix = mean(sap_brix, na.rm = TRUE)) %>% 
        select(mean_brix) %>% unlist(), 
      typ = "p", pch = 19, col = "#CC724066", 
      cex = sap_data %>% 
        filter(date != as_date(c("2022-03-12","2022-03-14")) & 
                 !is.nan(sap_brix) & 
                 sap_volume >= 100) %>% 
        group_by(doy) %>% tally() %>% select (n) %>% mutate(n = n/2) %>%
        sqrt() %>% unlist(),
      xlim = c(yday(as_date("2022-02-28")), yday(as_date("2022-05-01"))), 
      ylim = c(0, 6),
      axes = FALSE,
      xlab = "", 
      ylab = expression(paste("Sap succrose concentration (",degree,"Brix)", sep = "")))

# add axis --------------------------------------------------------------------- 
axis(side = 1, at = yday(as_date(c("2022-03-01","2022-04-01","2022-05-01"))), 
     labels = c("March","April","May"))
axis(side = 2, las = 1, at = seq(0, 6, by = 2))
# abline(lm(sap_brix ~ doy, 
#           data = sap_data %>% 
#             filter(date != as_date(c("2022-03-12","2022-03-14")) & 
#                      sap_volume >= 100)), 
#        col = "#CC7240", lty = 2, lwd = 2)
# abline(lm(bucket_brix ~ doy, 
#           data = sap_data %>% 
#             filter(date != as_date(c("2022-03-12","2022-03-14")) & 
#                      sap_volume >= 100)), 
#        col = "#B8AB9E66", lty = 2, lwd = 2)

# fit linear Bayesian model with tree-specific intercept and day of year (doy) 
# TR - Verify that the model converged and 
# TR - add bucket as a dummy variable to include the bucket_brix measurements in 
#      the model
# slope to sap succrose --------------------------------------------------------
mod1 <- brms::brm(formula = sap_brix ~ 1 + (1 | tree) + doy,
                  family = "gaussian",
                  data = sap_data %>% sap_volume >= 100)

# look at tree-specific intercepts ---------------------------------------------
a_tree <- brms::ranef(mod1)$tree[, , "Intercept"] %>% 
  round(digits = 2)

# get model coefficients and add them to the plot ------------------------------
a <- summary(mod1)$fixed[1, 1]
b <- coef(mod1)$tree[, , "doy"][1, 1]
b_lci <- coef(mod1)$tree[, , "doy"][1, 3]
b_uci <- coef(mod1)$tree[, , "doy"][1, 4]
polygon(x = c(55:125,125:55), 
        y = c(b_lci*55:125+a,b_uci*125:55+a),
        col = "#94452E33", lty = 0)
abline(a = a, b = b, col = "#94452E", lwd = 2, lty = 2)

# add individual data poins (tree averages) ------------------------------------
points (x = sap_data %>% 
          filter (sap_volume >= 100) %>% 
          group_by(doy) %>% 
          group_keys() %>% 
          unlist(),
        y = sap_data %>% 
          filter (sap_volume >= 100) %>% 
          group_by(doy) %>% 
          summarise(mean_brix = mean(bucket_brix, na.rm = TRUE)) %>% 
          select(mean_brix) %>% 
          unlist(), 
        pch = 19, col = "#B8AB9E66",
        cex = sap_data %>% 
          filter(!is.nan(bucket_brix) & sap_volume >= 100) %>% 
          group_by(doy) %>% 
          tally() %>% select (n) %>% mutate(n = n/2) %>% sqrt() %>% unlist())

# TR - Add data from Josh and maybe Michaël to the model
# TR - Add site and species to the model, so I can add Seb's tree and additional 
#      sites
# TR - Add uncertainty to tree averages in the model
#===============================================================================