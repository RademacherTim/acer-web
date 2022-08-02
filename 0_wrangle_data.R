#===============================================================================
# script to download and wrangle sugar season data sap data
#-------------------------------------------------------------------------------

# tasks ------------------------------------------------------------------------
# TR - Add data from Yvon Grenier, if I can get my hands on it.
# TR - Get some additional meta-data for trees in Vallée-Jonction
# TR - Get some additional meta-data for trees in Montreal

# load dependencies ------------------------------------------------------------
if (!existsFunction("%>%")) library ("tidyverse")
if (!existsFunction("yday")) library ("lubridate")
if (!existsFunction("read_sheet")) library ("googlesheets4")
if (!existsFunction("read_excel")) library ("readxl")
if (!existsFunction("add.alpha")) library ("prettyGraphs")

# authenthicate for spreadsheet and load the data from the acer-web sheet ------
gs4_auth(cache = ".secrets", email = "rademacher.tim@gmail.com")

# set url to google spreadsheet ------------------------------------------------
sheet_url <- "https://docs.google.com/spreadsheets/d/1Iup_x-uyfN-vk9oK7bQVfSP7XrihXAtJRVygnFNazig/edit#gid=1317380413"

# get acer-wab data from online sheet ------------------------------------------
AW_data_s <- read_sheet (ss = sheet_url, sheet = "01_sap_data",  
                         na = "NA",
                         col_types = "cccDcldddddddlc")
AW_data_w <- read_sheet (ss = sheet_url, sheet = "03_wound_data",  
                         na = "NA",
                         col_types = "icccDdddcdcccdccc")
AW_data_t <- read_sheet (ss = sheet_url, sheet = "05_tree_data",  
                         na = "NA",
                         col_types = "iccciccddddddd")
AW_site_data <- read_sheet (ss = sheet_url, sheet = "06_site_data",  
                            na = "NA",
                            col_types = "ccdddDcDic")

# add bark_thickness to tree_data from wound_data ------------------------------
AW_data_t <- left_join(AW_data_t, AW_data_w, 
                       by = c("year", "site", "tree", "tap")) %>%
  select(-tap_closure_1, -tap_closure_2, -wound, -wound_distance, -wound_angle, 
         -wound_orientation, -c_wound, -c_wound_distance, -c_wound_angle, 
         -c_wound_orientation, -comments, -date)

# add tapping date and tap removal date ----------------------------------------
AW_data_s <- left_join(AW_data_s, AW_site_data, by = c("site")) %>% 
  select(-site_name, -n_trees)

# create datetime column, convert time column, and add year as factor ----------
AW_data_s <- AW_data_s %>% 
  mutate(datetime = as_datetime (paste(date, time), 
                                 format = "%Y-%m-%d %H:%M", tz = "EST"),
         time = parse_time(time, "%H:%M"),
         year = factor (lubridate::year(date)),
         tree = factor(tree),
         site = factor(site),
         tap = factor(tap))
AW_data_t <- AW_data_t %>% 
  mutate(year = factor(year),
         tree = factor(tree),
         site = factor(site),
         tap = factor(tap),
         dbh = cbh / pi,
         tap_height = h_tap_ground)

# calculate mean sap succrose concentration (°Brix) ----------------------------
AW_data_s <- AW_data_s %>% 
  mutate(sap_brix = rowMeans(select(., sap_brix_1, sap_brix_2, sap_brix_3), 
                             na.rm = TRUE),
         bucket_brix = rowMeans(select(., bucket_brix_1, bucket_brix_2, bucket_brix_3), 
                                na.rm = TRUE))

# add day of year column to AW data --------------------------------------------
AW_data_s <- AW_data_s %>% mutate(doy = yday(date))

# combine the two data sets (tree-level data and sap flow data) ----------------
AW_data <- left_join(AW_data_s, 
                     AW_data_t, by = c("tree", "tap", "year", "site")) %>%
  select(-cbh, -sap_brix_1, -sap_brix_2, -sap_brix_3, -bucket_brix_1, 
         -bucket_brix_2, -bucket_brix_3, -ice, -comment, -comments, -c_tap, 
         -h_tap_ground, -h_tap_root_collar, -tap_time, -species, -running)

# re-arrange AW data for ease of comparison ------------------------------------
AW_data <- AW_data %>% 
  dplyr::relocate(site, tree, tap, date, time, datetime, year, doy, lat, lon, 
                  alti, spp, sap_volume, sap_brix, bucket_brix, n_taps, 
                  tap_bearing, tap_height, tap_depth, tap_width, dbh)

# remove outliers on 2022-03-12 due to most sap being frozen and 2022-03-14, as 
# there was only very little sap (i.e., 50 or 100 ml with one tree at 300 ml) --
AW_data <- AW_data %>% 
  filter(!(site== "1" & date %in% as_date(c("2022-03-12","2022-03-14"))))

# remove outliers due to rain water getting into the bucket --------------------
AW_data <- AW_data %>% 
  filter(!(site == "1" & date == as_date("2022-03-07") & tree == 27),
         !(site == "1" & date == as_date("2022-03-18") & tree %in% c(16, 25, 27)),
         !(site == "1" & date == as_date("2022-04-18") & tree %in% c(15, 16, 27)),
         !(site == "1" & date == as_date("2022-04-19") & tree %in% c(1:3, 5:8, 14, 25, 32, 33)),
         !(site == "1" & date == as_date("2022-04-30") & tree %in% c(15)))

# load Harvard Forest data -----------------------------------------------------
#HF_data_t1 <- read_csv("./data/HF/hf285-01-maple-tap.csv", col_types = cols())
#HF_data_s1 <- read_csv("./data/HF/hf285-02-maple-sap.csv", col_types = cols())
# use above link once the data has been updated on the Archive
HF_data_t <- read_csv("./data/HF/HFmaple.tapping.2012_2022.csv", 
                      col_types = cols()) %>% 
  mutate(date = lubridate::as_date(date, format = "%m/%d/%Y"))
HF_data_s <- read_csv("./data/HF/HFmaple.sap.2012_2022.csv", 
                      col_types = cols()) %>% 
  mutate(date = lubridate::as_date(date, format = "%m/%d/%Y"))

# change "HFR", which stands for Harvard Forest red maple to "AR", which stands 
# for Acer rurbrum, in the tree id for consistency of the two HF data sets -----
HF_data_t <- HF_data_t %>% mutate(tree = ifelse(substr(tree,1,3) == "HFR", 
                                                paste0("AR",substr(tree,4,nchar(tree))),
                                                tree))

# add rows to tibble with data for 2015, which was missing ---------------------
# According to sap flow data trees AR1, AR2, AR3, AR4, AR6, AR7, AR9, AR10, 
# HF35, and HF40 only have one tap ---------------------------------------------
HF_data_t <- HF_data_t %>% add_row(date = as_date("2015-03-09"), # tapping date in 2015 according to Josh
                      tree = c(rep(c("HF1", "HF4", "HF5", "HF6", "HF7", "HF9", 
                                     "HF10", "HF12", "HF13", "HF16", "HF21", 
                                     "HF22", "HF23", "HF33",  "HF38",  "HF41", 
                                     "HF43", "AR5", "AR8"), each = 2), # have two taps according to sap flow file
                               c("HF35","HF40","AR1", "AR2", "AR3", "AR4","AR6", 
                                 "AR7","AR9", "AR10")), # have one tap according to sap flow file
                      tap = c(rep(c("A", "B"), 19), rep("A", 10)),
                      species = c(rep("ACSA", 17*2), rep("ACRU", 2*2), 
                                  rep("ACSA", 2), rep("ACRU", 8)),
                      dbh = NA,
                      tap.bearing = NA,
                      tap.height = NA)

# add year column to the tree-specific data ------------------------------------
HF_data_t <- HF_data_t %>% mutate(year = factor(lubridate::year(date)))

# add a column with number of taps ---------------------------------------------
HF_data_t <- HF_data_t %>% group_by(tree, year) %>% mutate(n_taps = case_when(
  "C" %in% tap ~ 3,
  "B" %in% tap ~ 2,
  "A" %in% tap ~ 1,
)) %>% ungroup()

# plot dbh with simple linear interpolation ------------------------------------
PLOT <- FALSE
for (t in unique(HF_data_t$tree)) {
  con <- HF_data_t$tree == t
  if(PLOT){
    plot(x = HF_data_t$date[con], 
         y = HF_data_t$dbh[con],
         xlab = "date", ylab = "dbh (cm)", 
         xlim = c(as_date("2012-01-01"), as_date("2023-01-01")), ylim = c(20,100),
         axes = FALSE, pch = 19, col = "#91b9a4", main = t)
    axis(side = 1, 
         at = c(as_date("2012-01-01"), as_date("2013-01-01"), as_date("2014-01-01"), 
                as_date("2015-01-01"), as_date("2016-01-01"), as_date("2017-01-01"), 
                as_date("2018-01-01"), as_date("2019-01-01"), as_date("2020-01-01"), 
                as_date("2021-01-01"), as_date("2022-01-01"), as_date("2023-01-01")),
         labels = 2012:2023)
    axis(side = 2, las = 1)
  }
  if (length(HF_data_t$date[con]) > 1) {
    fit <- lm(dbh ~ date, data = HF_data_t[con, ])
    abline(fit, lty = 2, col = "#91b9a4")
  }
  # print coefficients ---------------------------------------------------------
  #print (fit$coefficients)
  
  # get years that this tree was sampled ---------------------------------------
  yrs <- unique(lubridate::year(HF_data_s$date[HF_data_s$tree == t]))
  
  # remove years for which we have actual dbh-meansurements --------------------
  dupli_yrs <- HF_data_t %>% 
    filter(tree == t & !is.na(dbh)) %>% 
    select(year) %>% unlist() %>% unique()
  dupli_yrs <- as.numeric(levels(dupli_yrs))[dupli_yrs] # extract integer values for duplicate years
  dupli_yrs <- dupli_yrs[dupli_yrs %in% yrs] # remove years for which there is no sapflow data
  yrs <- c(yrs, dupli_yrs) # concatenate years and duplicates
  yrs <- yrs[!(yrs %in% yrs[duplicated(yrs)])]
  
  # create dates for which we interpolate the dbh ------------------------------ 
  dates <- tibble (date = as_date(paste(yrs,"-03-01"))) 
  # choose the first of march here, but dbh should not vary in winter anyway
  
  # predict dbh on these dates -------------------------------------------------
  for (y in yrs) {
    HF_data_t$dbh[con & HF_data_t$year == y] <- 
      predict(fit, dates[lubridate::year(dates$date) == y, ])  
    # plot the points to check that this works
    points(x = HF_data_t$date[con & HF_data_t$year == y],
           y = HF_data_t$dbh[con & HF_data_t$year == y], 
           pch = 1 , lwd = 1 , col = "#91b9a4")
  }
  
}

# add datetime column ----------------------------------------------------------
HF_data_s <- HF_data_s %>%  
  mutate(datetime = as_datetime (paste(date, time), 
                                 format = "%Y-%m-%d %H:%M", 
                                 tz = "EST"))

# add day of year (doy) and sap_volume columns ---------------------------------
# density of water is 997.77 kg m-3 and succrose increases the density of the 
# solution. At 2% succrose content we can use a conversion factor of 1.0 L / kg.
HF_data_s <- HF_data_s %>% 
  mutate(doy = lubridate::yday(date),
         year = factor(lubridate::year(date)),
         sap_volume = sap.wt * 1000,
         site = "HF",
         sap_brix = sugar) %>%
  select(-sap.wt,-sugar)

# combine the two data sets ----------------------------------------------------
HF_data <- left_join(HF_data_s, HF_data_t, by = c("tree", "tap", "year")) %>% 
  select(-species.y) %>% # 
  rename(date = "date.x",
         tap_date = "date.y",
         spp = "species.x",
         tap_height = "tap.height",
         tap_bearing = "tap.bearing") %>% 
  add_column(tap_depth = 5.08, # 2 inches
             tap_width = 0.79375, # 5/16" drill bit
             lat = 42.53321,
             lon = -72.19090,
             alti = 338)

# determine tap_removal as last date of data collection ------------------------
# it was the day of last sap collection according to Josh
temp <- HF_data %>% group_by(tree, tap, year) %>% 
  summarise(tap_removal = max(date), .groups = "drop")

# add tap removal to tibble ----------------------------------------------------
HF_data <- left_join(HF_data, temp, by = c("tree", "tap", "year"))
rm(temp)

# re-arrange HF data for ease of comparison ------------------------------------
HF_data <- HF_data %>% 
  dplyr::relocate(site, tree, tap, date, time, datetime, year, doy, lat, lon, 
                 alti, spp, sap_volume, sap_brix, spp, n_taps, tap_bearing, 
                 tap_height, tap_depth, tap_width, tap_date, tap_removal, dbh)

# compile different data sets---------------------------------------------------
sap_data <- full_join(AW_data, HF_data, 
                      by = c("site", "tree", "tap", "date", "time", "datetime", 
                             "year", "doy", "lat", "lon", "alti", "spp", 
                             "sap_volume", "sap_brix", "n_taps", "tap_bearing", 
                             "tap_height", "tap_depth", "tap_width", "tap_date", 
                             "tap_removal", "dbh")) 

# read AcerNet data ------------------------------------------------------------
# N.B.: This data does not include tree sizes or any metadata. It is only sap 
# flow data --------------------------------------------------------------------
AN_data <- read_csv("./data/AcerNet/ACERnet_sap_2012_2017_ID.csv", 
                    col_types = cols()) %>% 
  mutate(date = as_date(Date, format = "%m/%d/%Y"),
         doy = lubridate::yday(date),
         sap_volume = Sap.Wt * 1000, # see note on conversion in HF data
         sap_brix = Sugar,
         site = Site.ID,
         tap = Tap,
         tree = Tree,
         spp = Species,
         year = factor(Year)) %>%
  select(-Sugar, -Sap.Wt, -Date, -Site, -Species, -Tree, -Tap, -Year, -Site.ID, 
         -Tree.ID, -Tree.Record.ID, -Tap.Record.ID)

# filter out Harvard Forest data, which was obtained independently -------------
AN_data <- AN_data %>% filter(site != "HF")

# add datetime, time, and dbh columns ------------------------------------------
AN_data <- AN_data %>% add_column(datetime = NA, time = NA, dbh = NA)

# add column with number of taps -----------------------------------------------
AN_data <- AN_data %>% group_by(tree, year) %>% mutate(n_taps = case_when(
  "C" %in% tap ~ 3,
  "B" %in% tap ~ 2,
  "A" %in% tap ~ 1,
)) %>% ungroup()

# add column with additional information from publication (Rapp et al., 2019) --
AN_data <- AN_data %>% 
  add_column(tap_date = NA,    # N.B.: TR - I still hope to get these from Josh
             tap_removal = NA, # N.B.: TR - I still hope to get these from Josh
             tap_depth = 5.08, # 2" 
             tap_width = 0.79375, # 5/16" drill bit and spile
             tap_bearing = NA,
             tap_height = NA)    

# add latitude and longitude of sites and estimated altitude -------------------
AN_data <- AN_data %>% 
  mutate(lat = case_when(
      site == "DOF"  ~ 43.734,  # Dartmouth Organic Farm
      site == "QC"   ~ 48.431,  # Quebec
      site == "DR"   ~ 37.011,  # Divide Ridge
      site == "INDU" ~ 41.625,  # Indiana Dunes National Lakeshore
      site == "SMM"  ~ 38.231), # Southernmost Maple
    lon = case_when(
      site == "DOF"  ~ -72.249,  # Dartmouth Organic Farm
      site == "QC"   ~ -70.688,  # Quebec
      site == "DR"   ~ -82.676,  # Divide Ridge
      site == "INDU" ~ -87.081,  # Indiana Dunes National Lakeshore
      site == "SMM"  ~ -79.658), # Southernmost Maple
    alti = case_when( # from elevation finder
      site == "DOF"  ~ 271.0,  # Dartmouth Organic Farm
      site == "QC"   ~ 243.0,  # Quebec
      site == "DR"   ~ 634.5,  # Divide Ridge
      site == "INDU" ~ 198.0,  # Indiana Dunes National Lakeshore
      site == "SMM"  ~ 837.5)) # Southernmost Maple

# re-arrange HF data for ease of comparison ------------------------------------
AN_data <- AN_data %>% 
  dplyr::relocate(site, tree, tap, date, time, datetime, year, doy, lat, lon, 
                  alti, spp, sap_volume, sap_brix, n_taps, tap_bearing, 
                  tap_height, tap_depth, tap_width)

# combine Acer Net data with other data ----------------------------------------
sap_data <- full_join(sap_data, AN_data, 
                      by = c("site", "tree", "tap", "date", "time", "datetime",
                             "year", "doy", "lat", "lon", "alti", "spp", 
                             "sap_volume", "sap_brix", "n_taps", "tap_bearing", 
                             "tap_height", "tap_depth", "tap_width","tap_date", 
                             "tap_removal","dbh"))

# read raw data from Mont Valin rain gauges for two trees ----------------------
MV_data <- read_csv2(file = "./data/MontValin/monts_valin_data_cleaned.csv",
                    col_types = c("cccdddD")) %>% 
  select(-rain_inc, -event_counts)

# add datetime to enable interval association ----------------------------------
MV_data <- MV_data %>%
  mutate(datetime = as_datetime (paste(date, time), 
                                 format = "%Y-%m-%d %H:%M", 
                                 tz = "EST"))

# create column with days since 2022-01-01 5:00, which I should use as daily 
# interval (e.g., 5:00 to 4:59) to separate individual thaw event-related flow 
# histogram shows that the least likely time for sapflow is at 05:00 am so I 
# used that as separator for "daily" flow 
MV_data <- MV_data %>% 
  mutate(day_intervals = floor(datetime - 
                               as_datetime("2022-01-01 05:00:00", 
                                           format = "%Y-%m-%d %H:%M:%S",
                                           tz = "EST")))

# calculate "daily" (between 5am and 4:59am) flow ------------------------------
MV_data <- MV_data %>% group_by(site, tree, tap, day_intervals) %>% 
  summarise(sap_volume = sum(sap_volume_inc, na.rm = TRUE),
            .groups = "drop") %>% 
  mutate(doy = as.integer(day_intervals)) %>% select(-day_intervals)

# add year and the dbh for each tree -------------------------------------------
MV_data <- MV_data %>% mutate(date = lubridate::as_date("2022-01-01") + doy,
                              time = parse_time("05:00"),
                              datetime = as_datetime(paste(date, time), 
                                                     format = "%Y-%m-%d %H:%M", 
                                                     tz = "EST"),
                              year = factor(lubridate::year(date)),
                              doy = lubridate::yday(date), 
                              dbh = case_when(tree == 1 ~ 22,
                                              tree == 2 ~ 16))

# add required columns based on information from Sara --------------------------
MV_data <- MV_data %>% add_column (lat = 48.63011129292363, 
                                   lon = -70.8875769500555, 
                                   alti = 220, 
                                   tap_date = as_date("2022-03-17"), 
                                   tap_removal = as_date("2022-06-01"), 
                                   sap_brix = NA, 
                                   n_taps = 1, 
                                   spp = "ACSA", 
                                   tap_bearing = NA, 
                                   tap_depth = 5, # between 5 and 6cm according to Sara
                                   tap_height = NA, # they were tapped at breast height (supposedly 1.3m)
                                   tap_width = 0.79375) # 5/16" spout driilled with 19/64 drill bit

# re-arrange order to match the sap_data tibble --------------------------------
MV_data <- MV_data %>% 
  dplyr::relocate(site, tree, tap, date, time, datetime, year, doy, lat, lon, 
                  alti, spp, sap_volume, sap_brix, n_taps, tap_bearing, 
                  tap_height, tap_depth, tap_width, tap_date, tap_removal, dbh)

# combine Monts Valin data with other data sets --------------------------------
sap_data <- full_join(sap_data, MV_data, 
                      by = c("site", "tree", "tap", "date", "time", "datetime", 
                             "year", "doy", "lat", "lon", "alti", "spp", 
                             "sap_volume", "sap_brix", "n_taps", "tap_bearing", 
                             "tap_height", "tap_depth", "tap_width", "tap_date", 
                             "tap_removal", "dbh"))

# read Élise raw data for Outaouais --------------------------------------------
OU_data_t <- read_delim(file = "./data/Outaouais/Erables.txt", 
                        delim = "\t", 
                        col_types = cols())
OU_data_s2020 <- read_delim(file = "./data/Outaouais/CH2020_data.txt", 
                            delim = "\t", 
                            col_types = cols()) %>% 
  add_column(Comments = NA)
OU_data_s2021 <- read_delim(file = "./data/Outaouais/CH2021_data.txt", 
                            delim = "\t", 
                            col_types = cols())

# combine data sets for 2020 and 2021 and add year column ----------------------
OU_data_s <- rbind(OU_data_s2020, OU_data_s2021) %>% 
  mutate(year = factor(lubridate::year(Date)))

# add dbh column ---------------------------------------------------------------
OU_data <- left_join(OU_data_s, OU_data_t, by = "ID")

# rename columns for consistency -----------------------------------------------
OU_data <- OU_data %>% rename(sap_brix = Sugar,
                              sap_volume = Volume,
                              date = Date,
                              tree = ID,
                              dbh = `DBH `) %>% 
  filter(is.na(Comments)) %>% 
  select(-Comments)

# add additional columns with site info ----------------------------------------
OU_data <- OU_data %>% add_column(
  site = "OU",
  tap = "A",
  time = parse_time("19:00"), # these are aggregated from 19:00 of the previous 
                              # day to 19:00h of the current day
  lat = 45.954444,
  lon = -74.863611,
  alti = 233, # according to Christian Messier
  n_taps = 1, 
  spp = "ACSA",
  tap_depth = 5.0, # or 2"
  tap_height = 140, # Approximately at breast height
  tap_width =  0.79375 # or 5/16"
)

# add columns for datetime, year, tap_date, tap_removal, doy, tap_bearing ------
OU_data <- OU_data %>% 
  mutate(tree = factor(tree),
         date = as_date(date),
         year = factor(lubridate::year(date)),
         tap_date = as_date(ifelse(year == 2020,
                           NA, # Élise still waiting to hear from intern
                           "2021-03-06")),
         tap_removal = as_date(ifelse(year == 2020,
                              NA, # Élise still waiting to hear from intern
                              "2021-04-27")),
         datetime = as_datetime(paste(date, time), 
                                format = "%Y-%m-%d %H:%M", 
                                tz = "EST"),
         doy = lubridate::yday(date),
         tap_bearing = ifelse(year == 2020, 90, # East
                              180)) # South

# re-arrange order to match the sap_data tibble --------------------------------
OU_data <- OU_data %>% 
  dplyr::relocate(site, tree, tap, date, time, datetime, year, doy, lat, lon, 
                  alti, spp, sap_volume, sap_brix, n_taps, tap_bearing, 
                  tap_height, tap_depth, tap_width, tap_date, tap_removal, dbh)

# combine Monts Valin data with other data sets --------------------------------
sap_data <- full_join(sap_data, OU_data, 
                      by = c("site", "tree", "tap", "date", "time", "datetime", 
                             "year", "doy", "lat", "lon", "alti", "spp", 
                             "sap_volume", "sap_brix", "n_taps","tap_bearing", 
                             "tap_height", "tap_depth", "tap_width", "tap_date", 
                             "tap_removal", "dbh"))


# add days since tapping column to data ----------------------------------------
sap_data <- sap_data %>% mutate(
  days_since_tapping = as.integer(difftime(date, tap_date, units = "days"))
)

# make sure all trees have unique tree IDs -------------------------------------
sap_data <- sap_data %>% mutate(tree = paste(site, tree, sep = "_"))

# create a seasonal summary for each tap ---------------------------------------
seasonal_data <- sap_data %>% 
  group_by(site, tree, tap, year) %>%
  summarise(lat = mean(lat, na.rm = TRUE),
            spp = unique(spp),
            sap_volume = sum(sap_volume, na.rm = TRUE) / 1e3, # in litres
            sap_brix = mean(sap_brix, na.rm = TRUE),
            tap_depth = mean(tap_depth, na.rm = TRUE),
            tap_width = mean(tap_width, na.rm = TRUE),
            n_taps = as.integer(mean(n_taps, na.rm = TRUE)),
            tap_bearing = mean(tap_bearing, na.rm = TRUE),
            tap_height = mean(tap_height, na.rm = TRUE),
            dbh = mean(dbh, na.rm = TRUE),
            .groups = "drop")

# remove the data for taps that did have no sap flow at all --------------------
seasonal_data <- seasonal_data %>% filter(sap_volume > 0)

# create log_yield variable to avoid fitting a log-normal-----------------------
seasonal_data$log_yield <- log(seasonal_data$sap_volume)

# remove single data point from Norway maple -----------------------------------
seasonal_data <- seasonal_data %>% filter(spp != "ACPL")

# group by sites and year to get sao run dates for each location ---------------
mid_season <- sap_data %>% 
  group_by(site, year) %>% 
  filter(sap_volume > 0) %>% 
  select(site, year, doy) %>% 
  distinct() %>% 
  summarise(median_doy = floor(median(doy)), .groups = "drop")

# add median doy for sugaring season to sap_data -------------------------------
sap_data <- left_join(sap_data, mid_season, by = c("site", "year"))

# aggregate early-season data --------------------------------------------------
early_data <- sap_data %>% 
  filter(doy <= median_doy) %>% # only days before or on the median sap run day
  group_by(site, tree, tap, year) %>%
  summarise(sap_volume_e = sum(sap_volume, na.rm = TRUE) / 1e3, # in litres
            sap_brix_e = mean(sap_brix, na.rm = TRUE),
            .groups = "drop")
  
# aggregate late-season data ---------------------------------------------------
late_data <- sap_data %>% 
  filter(doy > median_doy) %>% # only days after the median sap run day
  group_by(site, tree, tap, year) %>%
  summarise(sap_volume_l = sum(sap_volume, na.rm = TRUE) / 1e3, # in litres
            sap_brix_l = mean(sap_brix, na.rm = TRUE),
            .groups = "drop")
  
# add early- and late-season data to seasonal_data -----------------------------
seasonal_data <- seasonal_data %>% 
  left_join(early_data, by = c("site", "tree", "tap", "year")) %>%
  left_join(late_data, by = c("site", "tree", "tap", "year"))

# plot some general charcteristics to make sure all works fine -----------------
PLOT <- FALSE
if(PLOT){
  # histogram of sap volume ----------------------------------------------------
  par(mar = c(5, 5, 1, 1))
  hist(sap_data %>% select(sap_volume) %>% unlist(),
       xlab = "Sap volume (ml)", main = "", col = "#CC724066")
  
  # histogram of sap brix ------------------------------------------------------
  hist(sap_data %>% select(sap_brix) %>% unlist(), 
       breaks = seq(0, 2000, by = 0.2), xlim = c(0, 8), col = "#CC724066",
       xlab = expression(paste("Sap succrose concentration (",degree,"Brix)", sep = "")),
       main = "", lty = 1)
  abline(v = median(sap_data$sap_brix, na.rm = TRUE), col = "#94452E", lwd = 2)
  
  # plot early- versus late-season sap yield -----------------------------------
  plot(x = seasonal_data$sap_volume_e,
       y = seasonal_data$sap_volume_l, pch = 19, col = "#91b9a466",
       xlab = "Early-season sap yield (L)", 
       ylab = "Late-season sap yield (L)", 
       axes = FALSE)
  axis(side = 1)
  axis(side = 2, las = 1)
  abline(b = 1, a = 0, col = "#999999", lty = 2, lwd = 2)
  # TR - Is whether they fall above or below the line determined by the site latitude?
  
  # plot early- versus late-season sap brix ------------------------------------
  plot(x = seasonal_data$sap_brix_e,
       y = seasonal_data$sap_brix_l, pch = 19, col = "#91b9a466",
       xlab = expression(paste("Early-season sugar content (",degree,"Brix)", sep = "")), 
       ylab = expression(paste("Late-season sugar content (",degree,"Brix)", sep = "")), 
       axes = FALSE, xlim = c(0, 8), ylim = c(0, 8))
  axis(side = 1)
  axis(side = 2, las = 1)
  abline(b = 1, a = 0, col = "#999999", lty = 2, lwd = 2)
}

# get some basic stats for intro -----------------------------------------------
sap_data %>% filter(sap_volume > 0 & !is.na (sap_volume)) %>% count() # number of daily sap volume measurements
sap_data %>% filter(!is.na (sap_brix)) %>% count() # number of daily sugar content measurements
sap_data %>% group_by(site, tree, tap, year) %>% n_groups() # number of taps, as taps differ by year 
sap_data %>% group_by(site, tree, year) %>% n_groups() # number of tree years
sap_data %>% group_by(site, tree) %>% n_groups() # number of different tree years
sap_data %>% group_by(site) %>% n_groups() # number of sites
sap_data %>% group_by(year) %>% n_groups() # number of years
sap_data %>% filter(spp == "ACSA") %>% group_by(site, tree) %>% n_groups() # number of sugar maples
sap_data %>% filter(spp == "ACRU") %>% group_by(site, tree) %>% n_groups() # number of red maples
sap_data %>% filter(spp == "ACPL") %>% group_by(site, tree) %>% n_groups() # number of Norway maples

#===============================================================================