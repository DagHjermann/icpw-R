#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications #
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# for testing:
# setwd("app_check_stationdata")

library(shiny)
library(dplyr)
library(ggplot2)

dat <- readRDS("900_all_icpw_samples_long.RDS")  # data saved in script 900
stations <- unique(dat$station_code) %>% sort()
# countrycode <- unique(substr(stations, 1, 2))
# countrycode <- c("All", countrycode)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Check station data"),  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("param", "Parameter", 
                  c("NO3-N", "TOTN", "TOTC")),
      selectInput("station", "Station code", 
                  stations),
      sliderInput("year_start", "Start year:",
                  min = 1990, max = 2020, value = 1990),
      sliderInput("year_end", "End year:",
                  min = 1990, max = 2020, value = 2020)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("scatterplot")
    )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  get_dat_plot <- reactive({
    dat %>%
      filter(parameter %in% input$param &
               station_code %in% input$station & 
               year >= input$year_start & year <= input$year_end)
  })
  
  # browser()
  output$scatterplot <- renderPlot({
    # test <- dat_plot()
    # browser()
    # plot(1,1)
    dat_plot <- get_dat_plot()
    ggplot(dat_plot, aes(sample_date, value)) +
      geom_point() +
      labs(title = paste0(dat_plot$station_id[1], ": ", input$station, " (", dat_plot$station_name[1], ") "))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

