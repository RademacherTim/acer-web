#===============================================================================
#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#

# load dependencies ------------------------------------------------------------
library("shiny")
library("shinyTime") # to input times
library("googlesheets4") # to interact with data spreadsheet
library ("RColorBrewer") # to get colour palettes for plotting
library ("lubridate") # to transform dates and times 

sheet_url <- "https://docs.google.com/spreadsheets/d/1Iup_x-uyfN-vk9oK7bQVfSP7XrihXAtJRVygnFNazig/edit#gid=66834274"

# define server logic required to draw plots -----------------------------------
shinyServer(function(input, output) {

    load_data <- reactive (
        # authenthicate for spreadsheet and load the data from the acer-web sheet 
        gs4_auth(cache = ".secrets", email = "rademacher.tim@gmail.com")
        
        # get data from online sheet --------------------------------------------
        sap_data  <- read_sheet (ss = sheet_url, sheet = "01_sap_data",  
                                 na = "NA",
                                 col_types = "ciDcldddddddlc")
        met_data  <- read_sheet (ss = sheet_url, sheet = "02_met_data",  
                                 na = "NA",
                                 col_types = "cDlccdddcc")
        tree_data <- read_sheet (ss = sheet_url, sheet = "03_tree_data", 
                                 na = "NA",
                                 col_types = "cicddddddd")
        site_data <- read_sheet (ss = sheet_url, sheet = "04_site_data", 
                                 na = "NA",
                                 col_types = "cdddDci")
        
        # filter data for the site under consideration -------------------------
        sap_data <- sap_data %>% filter(site == input$site)
        met_data <- met_data %>% filter(site == input$site)
        tree_data <- tree_data %>% filter(site == input$site)
        site_data <- site_data %>% filter(site == input$site)
        
        # create datetime column ---------------------------------------------------
        sap_data <- sap_data %>% 
            mutate(datetime = as_datetime (paste(date, time), 
                                           format = "%Y-%m-%d %H:%M", tz = "EST"))
        
        # calculate mean sap BRIX --------------------------------------------------
        sap_data <- sap_data %>% 
            mutate(sap_brix = rowMeans(select(., sap_brix_1, sap_brix_2, sap_brix_3), 
                                       na.rm = TRUE))
    )
    
    # create function to sap volume data ---------------------------------------
    output$plotVolume <- renderPlot({
        
        # get the number of trees ----------------------------------------------
        n_trees <- max (sap_data$tree, na.rm = TRUE)
        
        # create colour plalette for the trees ---------------------------------
        col_palette <- brewer.pal(n = min(n_trees, 12), name = "Set3")
        
        # draw scatter plot for data over time ---------------------------------
        par(mar = c(5, 5, 1, 1))
        plot(x = sap_data$datetime, 
             y = sap_data$sap_volume,
             xlab = "Date",
             ylab = "Sap volume (ml)",
             typ = "p", pch = 21, las = 1, #axes = FALSE,
             col = "gray", lwd = 2)
        #axis(side = 1, 
        #     at = seq(from = min(sap_data$datetime, na.rm = TRUE), 
        #              to = max(sap_data$datetime, na.rm = TRUE),
        #              by = 60*60*24),
        #     labels = c ())
        #axis(side = 2, 
        #     at = seq(0, max(sap_data$sap_volume, na.rm = TRUE), by = 500),
        #     las = 1)
        
        # connect dots for each tree with different colours --------------------
        res <- sapply(1:n_trees, function(x) {
            lines (x = sap_data$datetime [sap_data$tree == x],
                   y = sap_data$sap_volume [sap_data$tree == x], 
                   col = col_palette [ifelse(x <= 12, x, x - 12)],
                   lwd = 2)
            })

    })
    
    # create function to plot sap sugar content --------------------------------
    # maybe change this for a histogram eventually
    output$plotBRIX <- renderPlot({
        
        # plot sap BRIX --------------------------------------------------------
        par(mar = c(5, 5, 1, 1))
        plot(x = sap_data$datetime, 
             y = sap_data$sap_brix,
             xlab = "Date",
             ylab = "Sap BRIX (°)",
             typ = "p", pch = 21, las = 1,
             col = "gray", lwd = 2)
        
        # connect dots for each tree with different colours --------------------
        res <- sapply(1:n_trees, function(x) {
            lines (x = sap_data$datetime [sap_data$tree == x],
                   y = sap_data$sap_brix [sap_data$tree == x], 
                   col = col_palette [ifelse(x <= 12, x, x - 12)],
                   lwd = 2)
        })
        
    })

})
#===============================================================================
