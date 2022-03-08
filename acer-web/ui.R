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

    tabsetPanel(
        br (), # insert some space
        
        # Home tab
        tabPanel("Acceuil", fluid = TRUE,
                 includeMarkdown("acceuil.md") 
        ),
        # Data entry tab
        tabPanel("Saisie de données", fluid = TRUE,
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
                    
                    hr (), # horizontal line
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
                    strong ("Temps de mesures approximatif"),
                    splitLayout(cellWidths = c (160,160),
                        shinyTime::timeInput(inputId = "start_time", 
                                  label = "", 
                                  value = NULL, 
                                  seconds = FALSE,
                                  minute.steps = NULL),
                        shinyTime::timeInput(inputId = "end_time", 
                                  label = "", 
                                  value = NULL, 
                                  seconds = FALSE,
                                  minute.steps = NULL)
                    ),
                    radioButtons(inputId = "flow",
                                 label = "Est-ce que la sève a coulée depuis la dernière mesure ?",
                                 choices = c ("Non", "Oui"),
                                 selected = "Non", 
                                 inline = TRUE),
                    
                    hr (), # horizontal line
                    strong ("Météo pendant les mesures"),
                    br (), 
                    
                    numericInput(inputId = "t_air_C",
                                 label = "Température d'aire (°C):",
                                 min = -40,
                                 max = 30,
                                 value = 0,
                                 width = "50%"),
                    radioButtons(inputId = "precip",
                                 label = "Avait-il de la précipitation pendant les mesures ?",
                                 choices = c ("Non", "Pluie", "Neige"),
                                 selected = "Non", 
                                 inline = TRUE),
                    splitLayout(
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
                                     width = "45%")),
                    
                    # horizontal line
                    hr (),
                    
                    # submit button
                    actionButton (inputId = "confirmSiteData",
                                  label = "Confirmer les observations",
                                  inline = TRUE)
                    ),

                # Show a plot of the past data from the site with a line for each tree
                mainPanel(
                    plotOutput("distPlot")
                )
            ))
    )
))
#===============================================================================