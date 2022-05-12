#===============================================================================
# script to download and wrangle sugar season data sap data
#-------------------------------------------------------------------------------

# load dependencies ------------------------------------------------------------
if (!existsFunction("%>%")) library ("tidyverse")
if (!existsFunction("yday")) library ("lubridate")
if (!existsFunction("read_sheet")) library ("googlesheets4")
if (!existsFunction("add.alpha")) library ("prettyGraphs")

# authenthicate for spreadsheet and load the data from the acer-web sheet ------
gs4_auth(cache = ".secrets", email = "rademacher.tim@gmail.com")

# set url to google spreadsheet ------------------------------------------------
sheet_url <- "https://docs.google.com/spreadsheets/d/1Iup_x-uyfN-vk9oK7bQVfSP7XrihXAtJRVygnFNazig/edit#gid=1317380413"

# get acer-wab data from online sheet ------------------------------------------
AW_data_s <- read_sheet (ss = sheet_url, sheet = "01_sap_data",  
                         na = "NA",
                         col_types = "cccDcldddddddlc")
AW_data_t <- read_sheet (ss = sheet_url, sheet = "05_tree_data",  
                         na = "NA",
                         col_types = "iccciccddddddddd")
# filter data for the site under consideration ---------------------------------
#AW_data <- AW_data %>% filter(site == "1")

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
         dbh = cbh / pi)

# calculate mean sap succrose concentration (°Brix) ----------------------------
AW_data_s <- AW_data_s %>% 
  mutate(sap_brix = rowMeans(select(., sap_brix_1, sap_brix_2, sap_brix_3), 
                             na.rm = TRUE),
         bucket_brix = rowMeans(select(., bucket_brix_1, bucket_brix_2, bucket_brix_3), 
                                na.rm = TRUE))

# add day of year column to AW data --------------------------------------------
AW_data_s <- AW_data_s %>% mutate(doy = yday(date))

# combine the two data sets (tree-level data and sap flow data) ----------------
AW_data <- left_join(AW_data_s, AW_data_t, by = c("tree", "tap", "year", "site"))

# load Harvard Forest data -----------------------------------------------------
HF_data_t <- read_csv("./data/HF/hf285-01-maple-tap.csv", col_types = cols())
HF_data_s <- read_csv("./data/HF/hf285-02-maple-sap.csv", col_types = cols())

# change "HFR", which stands for Harvard Forest red maple to "AR", which stands 
# for Acer rurbrum, in the tree id for consistency of the two HF data sets -----
HF_data_t <- HF_data_t %>% mutate(tree = ifelse(substr(tree,1,3) == "HFR", 
                                                paste0("AR",substr(tree,4,nchar(tree))),
                                                tree))

# add year column to the tree-specific data ------------------------------------
HF_data_t <- HF_data_t %>% mutate(year = factor(lubridate::year(date)))
  
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
HF_data <- left_join(HF_data_s, HF_data_t, by = c("tree","tap","year")) %>% 
  select(-date.y, -species.y) %>% # 
  rename(date = "date.x",
         spp = "species.x")

# compile different data sets---------------------------------------------------
sap_data <- full_join(AW_data, HF_data,
                      by = c("site", "datetime", "date", "year", "doy", "time",  
                             "tree", "tap", "spp", "sap_volume", "sap_brix",
                             "dbh")) 

# read AcerNet data ------------------------------------------------------------
# N.B.: This data does not include tree sizes or any metadata. It is only sap 
# flow data --------------------------------------------------------------------
AN_data <- read_csv("./data/AcerNet/ACERnet_sap_2012_2017_ID.csv", 
                    col_types = cols()) %>% 
  mutate(date = as_date(Date, format = "%m/%d/%Y"),
         doy = lubridate::yday(date),
         sap_volume = Sap.Wt * 1000,
         sap_brix = Sugar,
         site = Site.ID,
         tap = Tap,
         tree = Tree,
         spp = Species,
         year = factor(Year)) %>%
  select(-Sugar, -Sap.Wt, -Date, -Site, -Species, -Tree, -Tap, -Year, -Site.ID, 
         -Tree.ID, -Tree.Record.ID, -Tap.Record.ID)

# filter out Harvard Forest data which was obtained independently --------------
AN_data <- AN_data %>% filter(site != "HF")

# add datetime, time, and dbh columns ------------------------------------------
AN_data <- AN_data %>% add_column(datetime = NA, time = NA, dbh = NA)

# compile different data sets---------------------------------------------------
sap_data <- full_join(sap_data, AN_data,
                      by = c("site", "datetime", "date", "year", "doy", "time",  
                             "tree", "tap", "spp", "sap_volume", "sap_brix",
                             "dbh")) 

# remove outliers on 2022-03-12 due to most sap being frozen and 2022-03-14, as 
# there was only very little sap (i.e., 50 or 100 ml with one tree at 300 ml) --
sap_data <- sap_data %>% 
  filter(!(site == "1" & date == as_date(c("2022-03-12","2022-03-14"))))

# remove outliers due to rain water getting into the bucket --------------------
sap_data <- sap_data %>% 
  filter(!(site == "1" & date == as_date("2022-03-07") & tree == 27),
         !(site == "1" & date == as_date("2022-03-18") & tree %in% c(16, 25, 27)),
         !(site == "1" & date == as_date("2022-04-18") & tree %in% c(15, 16, 27)),
         !(site == "1" & date == as_date("2022-04-19") & tree %in% c(1:3, 5:8, 14, 25, 32, 33)),
         !(site == "1" & date == as_date("2022-04-30") & tree %in% c(15)))

#===============================================================================