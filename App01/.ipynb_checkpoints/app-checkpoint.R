#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source("../002_Functions.R")

# From '901_test' and '160parm_run_markdown' (s)
vars <- paste(
    "no3_decline", 
    "catchment_area, TOC",     # change here
    "slope_dep_vs_time, NO3, TOTN_dep, latitude, longitude, altitude",
    "pre, tmp, Slope_pre, Slope_tmp, urban, cultivated, coniferous, decid_mixed, total_shrub_herbaceous",
    "wetland, lake_water, bare_sparse",
    sep = ",") 

# From '901_test' and '160parm_Time_series_results_James.Rmd'
dat <- get_data_no3trend(data_folder = "../Data") 

vars <- names(dat)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("var1", "Variable 1", choices = vars),
            selectInput("var2", "Variable 2", choices = vars)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("scatter_plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$scatter_plot <- renderPlot({
        dat$Var1 <- dat[[input$var1]]
        dat$Var2 <- dat[[input$var2]]
        gg <- ggplot(dat, aes(Var1, Var2)) +
            geom_point() +
            labs(x = input$var1, y = input$var2)
        if (class(dat$Var1) %in% c("character", "factor"))
          gg <- gg +
            theme(axis.text.x = element_text(angle = -45, hjust = 0))
        gg
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
