# plot succrose concentration (°Brix) over time at l'Assomption ----------------
par(mar = c(3, 5, 1, 1))
plot (x = sap_data %>% filter (sap_volume >= 100 & site == "1") %>% 
        group_by(date) %>% group_keys(),
      y = sap_data %>% filter (sap_volume >= 100 & site == "1") %>% 
        group_by(date) %>% summarise(mean_brix = mean(sap_brix, na.rm = TRUE)) %>% 
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

# plot histograms of sap volume and brix at l'Assomption -----------------------
par(mar = c(5, 5, 1, 1))
hist(sap_data %>% filter(site == "1") %>% select(sap_volume) %>% unlist(),
     xlab = "Sap volume (ml)", main = "", col = "#CC724066")
hist(sap_data %>% filter(site == "1") %>% select(sap_brix) %>% unlist(), 
     breaks = seq(0, 25, by = 0.2), xlim = c(0, 8), col = "#CC724066",
     xlab = expression(paste("Sap succrose concentration (",degree,"Brix)", sep = "")),
     main = "", lty = 1)
abline(v = median(sap_data$sap_brix, na.rm = TRUE), col = "#94452E", lwd = 2)

# re-plot succrose concentration (°Brix) over time without  --------------------
par(mar = c(3, 5, 1, 1))
plot (x = sap_data %>% 
        filter (sap_volume >= 100 & site == 1) %>% 
        group_by(doy) %>% group_keys() %>% unlist(),
      y = sap_data %>% 
        filter (sap_volume >= 100 & site == 1) %>% 
        group_by(doy) %>% 
        summarise(mean_brix = mean(sap_brix, na.rm = TRUE)) %>% 
        select(mean_brix) %>% unlist(), 
      typ = "p", pch = 19, col = "#CC724033", 
      cex = sap_data %>% 
        filter(date != as_date(c("2022-03-12","2022-03-14")) & 
                 !is.nan(sap_brix) & 
                 sap_volume >= 100 & site == 1) %>% 
        group_by(doy) %>% tally() %>% select (n) %>% mutate(n = n/2) %>%
        sqrt() %>% unlist(),
      xlim = c(yday(as_date("2022-01-28")), yday(as_date("2022-05-01"))), 
      ylim = c(0, 6),
      axes = FALSE,
      xlab = "", 
      ylab = expression(paste("Sap succrose concentration (",degree,"Brix)", sep = "")))

# add axis --------------------------------------------------------------------- 
axis(side = 1, at = yday(as_date(c("2022-02-01","2022-03-01","2022-04-01","2022-05-01"))), 
     labels = c("February","March","April","May"))
axis(side = 2, las = 1, at = seq(0, 6, by = 2))
points (x = sap_data %>% filter (sap_volume >= 100 & site == 1) %>% 
          group_by(doy) %>% group_keys() %>% unlist(),
        y = sap_data %>% filter (sap_volume >= 100 & site == 1) %>% 
          group_by(doy) %>% summarise(mean_brix = mean(bucket_brix, na.rm = TRUE)) %>% 
          select(mean_brix) %>% unlist(), 
        pch = 19, col = "#B8AB9E66",
        cex = sap_data %>% 
          filter(!is.nan(bucket_brix) & sap_volume >= 100) %>% group_by(date) %>% 
          tally() %>% select (n)  %>% mutate(n = n / 2) %>% sqrt() %>% unlist())
abline(lm(sap_brix ~ doy,
          data = sap_data %>%
            filter(date != as_date(c("2022-03-12","2022-03-14")) &
                     sap_volume >= 100 & site == 1)),
       col = "#CC7240", lty = 2, lwd = 2)
abline(lm(bucket_brix ~ doy,
          data = sap_data %>%
            filter(date != as_date(c("2022-03-12","2022-03-14")) &
                     sap_volume >= 100 & site == 1)),
       col = "#B8AB9E66", lty = 2, lwd = 2)

# fit linear Bayesian model with tree-specific intercept and day of year (doy) 
# slope to sap succrose concentration ------------------------------------------
# TR - Verify that the model converged 
# TR - add bucket as a dummy variable to include the bucket_brix measurements in 
#      the model
mod0 <- brms::brm(formula = sap_brix ~ 1 + (1 | tree) + doy, 
                  family = "gaussian",
                  data = sap_data %>% filter (sap_volume >= 100 & site == "1"))

# look at tree-specific intercepts ---------------------------------------------
a_tree <- brms::ranef(mod0)$tree[, , "Intercept"] %>% 
  round(digits = 2)

# get model coefficients and add them to the plot ------------------------------
a <- summary(mod0)$fixed[1, 1]
b <- coef(mod0)$tree[, , "doy"][1, 1]
b_lci <- coef(mod0)$tree[, , "doy"][1, 3]
b_uci <- coef(mod0)$tree[, , "doy"][1, 4]
polygon(x = c(55:125,125:55), 
        y = c(b_lci*55:125+a,b_uci*125:55+a),
        col = "#94452E33", lty = 0)
abline(a = a, b = b, col = "#94452E", lwd = 2, lty = 2)

# plot succrose contentration over time at Harvard Forest for various years ----
colours <- c ("#8dd3c799","#ffffb399","#bebada99","#fb807299","#80b1d399",
              "#fdb46299")
plot(x = sap_data %>% 
       filter(sap_volume >= 100 & site == "HF" & year == 2012) %>% 
       group_by(doy) %>%
       group_keys() %>% unlist(),
     y = sap_data %>% 
       filter(sap_volume >= 100 & site == "HF" & year == 2012) %>% 
       group_by(doy) %>%
       summarise(mean_brix = mean(sap_brix, na.rm = TRUE)) %>% 
       select(mean_brix) %>% unlist(),
     xlab = "", 
     ylab = expression(paste("Sap succrose concentration (",degree,"Brix)", sep = "")), 
     axes = FALSE, xlim = c(yday(as_date("2022-01-28")), yday(as_date("2022-05-01"))), 
     ylim = c(0, 6), pch = 19, 
     # cex = sap_data %>% 
     #   filter(!is.nan(sap_brix) & sap_volume >= 100 & site == "HF" & year == 2012) %>% 
     #   group_by(doy) %>% tally() %>% select (n) %>% mutate(n = n/2) %>%
     #   sqrt() %>% unlist(), 
     col = "white")
axis(side = 1, at = yday(as_date(c("2022-02-01","2022-03-01","2022-04-01","2022-05-01"))), 
     labels = c("February","March","April","May"))
axis(side = 2, las = 1, at = seq(0, 6, by = 2))
for (y in 2012:2018) {
  points(x = sap_data %>% 
           filter(sap_volume >= 100 & site == "HF" & year == y) %>% 
           group_by(doy) %>%
           group_keys() %>% unlist(),
         y = sap_data %>% 
           filter(sap_volume >= 100 & site == "HF" & year == y) %>% 
           group_by(doy) %>%
           summarise(mean_brix = mean(sap_brix, na.rm = TRUE)) %>% 
           select(mean_brix) %>% unlist(),
         pch = 19, 
         # cex = sap_data %>% 
         #   filter(!is.nan(sap_brix) & sap_volume >= 100 & site == "HF" & year == y) %>% 
         #   group_by(doy) %>% tally() %>% select (n) %>% mutate(n = n/2) %>%
         #   sqrt() %>% unlist(), 
         col = colours [y-2011])
  abline(lm(sap_brix ~ doy, 
            data = sap_data %>% 
              filter(sap_volume >= 100 & site == "HF" & year == y)),
         col = colours[y-2011], lwd = 2, lty = 2)
}
legend(x = yday(as_date("2018-02-01")), y = 6, box.lty = 0, legend = 2012:2018, col = colours, pch = 19)