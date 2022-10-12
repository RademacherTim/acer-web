#===============================================================================
# script to make map of sites and red and sugar maple distributions
#-------------------------------------------------------------------------------

# load dependencies ------------------------------------------------------------
library("RColorBrewer") # for colour schemes
library("leaflet") # for mapping
library("webshot") # save saving leaflet map
library("htmlwidgets")

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

# custom legend function 
addLegendCustom <- function(map, title, colors, labels, sizes, opacity = 0.5){
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
  
  return(addLegend(map, title = title, colors = colorAdditions, labels = labelAdditions, opacity = opacity))
}

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
  #addGeotiff(file = filenames[1])
m

# save the map as image --------------------------------------------------------
#saveWidget(m, "temp.html", selfcontained = FALSE)
#webshot(m, file = "./fig/map_of_sites.png")
#===============================================================================