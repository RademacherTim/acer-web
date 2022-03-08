#===============================================================================
#
# This is the user-interface definition for the acer-web shiny web application. 
# You can run the application by clicking 'Run App' above or by going to :
# "insert web link here"
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Acer web"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "site",
                label = "Site",
                choices = c("1 - L'Assomption","2 - Bolton","3 - Beauce","4 - Montréal (Mercier)","5 - Montréal Sud-Ouest"),
                selected = NULL,
                multiple = FALSE,
                selectize = TRUE,
                width = NULL,
                size = NULL
            ),
            dateInput(
                inputId = "date",
                label = "Date de mesure",
                value = NULL,
                min = "2022-01-01",
                max = "2022-12-31",
                format = "yyyy-mm-dd",
                startview = "month",
                weekstart = 1, # monday is first day of week
                language = "fr",
                width = NULL,
                autoclose = TRUE,
                datesdisabled = NULL,
                daysofweekdisabled = NULL
            ),
            shinyTime::timeInput(inputId = "start_time", 
                      label = "Temps de départ", 
                      value = NULL, 
                      seconds = FALSE,
                      minute.steps = NULL),
            shinyTime::timeInput(inputId = "end_time", 
                      label = "Temps de fin", 
                      value = NULL, 
                      seconds = FALSE,
                      minute.steps = NULL),
            numericInput(inputId = "n_trees",
                         label = "Nombre d'arbre:",
                         min = 1,
                         max = 100,
                         value = 10),
            numericInput(inputId = "t_air_C",
                         label = "Température d'aire approximative (°C):",
                         min = -40,
                         max = 30,
                         value = 0),
            checkboxInput(inputId = "precip",
                          label = "Pleuvait-il pendant les mesures ?",
                          value = FALSE, width = NULL),
            numericInput(inputId = "cloudiness",
                         label = "Couverture de nuage (%):",
                         min = 0,
                         max = 100,
                         value = 50, 
                         width = "45%"),
            numericInput(inputId = "snow_cover",
                         label = "Couverture de neige (%):",
                         min = 0,
                         max = 100,
                         value = 50, 
                         width = "45%")
        ),

        # Show a plot of the past data from the site with a line for each tree
        mainPanel(
            plotOutput("distPlot")
        )
    )
))
#===============================================================================