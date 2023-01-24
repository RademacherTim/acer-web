#===============================================================================
# script to make map of sites and red and sugar maple distributions
#-------------------------------------------------------------------------------

# load dependencies ------------------------------------------------------------
#if (!existsFunction("leaflet")) library("RColorBrewer") # for colour schemes
if (!existsFunction("leaflet")) library("leaflet") # for mapping
#library("webshot") # save saving leaflet map
#if (!existsFunction("html_legend")) library("htmlwidgets")
if (!existsFunction("spTransform")) library("sp")
if (!existsFunction("readOGR")) library("rgdal")

# load the wrangled data -------------------------------------------------------
source("0_wrangle_data.R") 

# create dataframe of site coordinates -----------------------------------------
site_data <- sap_data %>% 
  dplyr::group_by(site) %>% 
  dplyr::summarise(
    lat = mean(lat), 
    lon = mean(lon),
    alti = mean(alti),
    n_trees = dplyr::n_distinct(tree),
    n_taps = dplyr::n_distinct(tree, tap),
    n_years = dplyr::n_distinct(year),
    n_obs = dplyr::n_distinct(tree, tap, year, date),
    .groups = "drop")

# choose sugar or red maple leaf icon:
# leafIcons <- icons(
#   iconUrl = case_when(site_data$spp == "ACSA" ~ "http://leafletjs.com/examples/custom-icons/leaf-orange.png",
#                       site_data$spp == "ACRU" ~ "http://leafletjs.com/examples/custom-icons/leaf-red.png",
#                       TRUE ~ "http://leafletjs.com/examples/custom-icons/leaf-green.png",
#                    
#                    
#   ),
#   iconWidth = 38, iconHeight = 95,
#   iconAnchorX = 22, iconAnchorY = 94)

# create html legend -----------------------------------------------------------
# html_legend <- "<img src='http://leafletjs.com/examples/custom-icons/leaf-green.png'>green<br/>
# <img src='http://leafletjs.com/examples/custom-icons/leaf-red.png'>red"

# custom legend function -------------------------------------------------------
addLegendCustom <- function(map, title, colors, labels, sizes, opacity = 0.5){
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, 
                           "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, 
                           "px;margin-top: 4px;line-height: ", sizes, "px;'>", 
                           labels, "</div>")
  
  return(addLegend(map, title = title, colors = colorAdditions, 
                   labels = labelAdditions, opacity = opacity))
}

# transform files of distributions ---------------------------------------------
#disACRU <- sf::st_read("../MapleGrowth/data/distribution/little1991/ACRU/litt316av.shp",
#                       stringsAsFactors = FALSE, quiet = TRUE)

# set the coordinate system to Albers equal area projection with US Forest 
# Service parameters from https://www.fs.fed.us/nrs/atlas/littlefia/albers_prj.txt
#-------------------------------------------------------------------------------
#USFS_CRS <- "+proj=longlat +datum=WGS84 +no_defs"
#sf::st_crs (disACRU) <- USFS_CRS

# make map of the sites --------------------------------------------------------
m <- leaflet(data = site_data) %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addCircleMarkers(lng = ~lon, 
             lat = ~lat, 
             radius = ~dplyr::case_when(
               n_obs < 320 ~ 6,
               n_obs < 640 ~ 9,
               n_obs < 1280 ~ 12,
               n_obs < 2560 ~ 15,
               n_obs < 10000 ~ 18),
              stroke = FALSE,
              fillOpacity = 0.6,
              fillColor = "#CC7240") %>%
  addLegendCustom(#"bottomright", 
            colors = "#CC7240", 
            labels = c("< 320","< 640","< 1280","< 2560", ">2560"),
            sizes = seq(6, 18, by = 3),
            title = "Number of observations",
            opacity = 0.6) #%>%
  #addPolylines(data = disACRU) 
m

# site stats -------------------------------------------------------------------
#sap_data %>% select(site) %>% unique()
# spp at each site -------------------------------------------------------------
sap_data %>% filter(site == "1") %>% select(spp) %>% unique()
sap_data %>% filter(site == "3") %>% select(spp) %>% unique()
sap_data %>% filter(site == "HF") %>% select(spp) %>% unique()
sap_data %>% filter(site == "DOF") %>% select(spp) %>% unique()
sap_data %>% filter(site == "SMM") %>% select(spp) %>% unique()
sap_data %>% filter(site == "QC") %>% select(spp) %>% unique()
sap_data %>% filter(site == "DR") %>% select(spp) %>% unique()
sap_data %>% filter(site == "INDU") %>% select(spp) %>% unique()
sap_data %>% filter(site == "MV") %>% select(spp) %>% unique()
sap_data %>% filter(site == "OU") %>% select(spp) %>% unique()
#sap_data %>% filter(site == "5") %>% select(spp) %>% unique()

# number of tree (n_trees) for each site ---------------------------------------
sap_data %>% filter(site == "1") %>% select(tree) %>% unique() %>% count()
sap_data %>% filter(site == "3") %>% select(tree) %>% unique() %>% count()
sap_data %>% filter(site == "HF") %>% select(tree) %>% unique() %>% count()
sap_data %>% filter(site == "DOF") %>% select(tree) %>% unique() %>% count()
sap_data %>% filter(site == "SMM") %>% select(tree) %>% unique() %>% count()
sap_data %>% filter(site == "QC") %>% select(tree) %>% unique() %>% count()
sap_data %>% filter(site == "DR") %>% select(tree) %>% unique() %>% count()
sap_data %>% filter(site == "INDU") %>% select(tree) %>% unique() %>% count()
sap_data %>% filter(site == "MV") %>% select(tree) %>% unique() %>% count()
sap_data %>% filter(site == "OU") %>% select(tree) %>% unique() %>% count()
#sap_data %>% filter(site == "5") %>% select(tree) %>% unique() %>% count()

# number of taps (n_taps) for each site ----------------------------------------
sap_data %>% filter(site == "1") %>% select(tap) %>% unique() %>% count()
sap_data %>% filter(site == "3") %>% select(tap) %>% unique() %>% count()
sap_data %>% filter(site == "HF") %>% select(tap) %>% unique() %>% count()
sap_data %>% filter(site == "DOF") %>% select(tap) %>% unique() %>% count()
sap_data %>% filter(site == "SMM") %>% select(tap) %>% unique() %>% count()
sap_data %>% filter(site == "QC") %>% select(tap) %>% unique() %>% count()
sap_data %>% filter(site == "DR") %>% select(tap) %>% unique() %>% count()
sap_data %>% filter(site == "INDU") %>% select(tap) %>% unique() %>% count()
sap_data %>% filter(site == "MV") %>% select(tap) %>% unique() %>% count()
sap_data %>% filter(site == "OU") %>% select(tap) %>% unique() %>% count()
#sap_data %>% filter(site == "5") %>% select(tap) %>% unique() %>% count()

# years of data collection for each site ---------------------------------------
sap_data %>% filter(site == "1") %>% select(year) %>% unique()
sap_data %>% filter(site == "3") %>% select(year) %>% unique()
sap_data %>% filter(site == "HF") %>% select(year) %>% unique()
sap_data %>% filter(site == "DOF") %>% select(year) %>% unique()
sap_data %>% filter(site == "SMM") %>% select(year) %>% unique()
sap_data %>% filter(site == "QC") %>% select(year) %>% unique()
sap_data %>% filter(site == "DR") %>% select(year) %>% unique()
sap_data %>% filter(site == "INDU") %>% select(year) %>% unique()
sap_data %>% filter(site == "MV") %>% select(year) %>% unique()
sap_data %>% filter(site == "OU") %>% select(year) %>% unique()
#sap_data %>% filter(site == "5") %>% select(year) %>% unique()

# number of observations (n) for observations 
sap_data %>% filter(site == "1"    & sap_volume > 0 & !is.na (sap_volume)) %>% count()
sap_data %>% filter(site == "3"    & sap_volume > 0 & !is.na (sap_volume)) %>% count()
sap_data %>% filter(site == "HF"   & sap_volume > 0 & !is.na (sap_volume)) %>% count()
sap_data %>% filter(site == "DOF"  & sap_volume > 0 & !is.na (sap_volume)) %>% count()
sap_data %>% filter(site == "SMM"  & sap_volume > 0 & !is.na (sap_volume)) %>% count()
sap_data %>% filter(site == "QC"   & sap_volume > 0 & !is.na (sap_volume)) %>% count()
sap_data %>% filter(site == "DR"   & sap_volume > 0 & !is.na (sap_volume)) %>% count()
sap_data %>% filter(site == "INDU" & sap_volume > 0 & !is.na (sap_volume)) %>% count()
sap_data %>% filter(site == "MV"   & sap_volume > 0 & !is.na (sap_volume)) %>% count()
sap_data %>% filter(site == "OU"   & sap_volume > 0 & !is.na (sap_volume)) %>% count()
#sap_data %>% filter(site == "5" & sap_volume > 0 & !is.na (sap_volume)) %>% count()
#===============================================================================