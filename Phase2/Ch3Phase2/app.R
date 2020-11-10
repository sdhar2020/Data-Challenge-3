#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(reticulate)
library(shinyWidgets)
source_python('helper.py')

dat <- read.csv('RUL_FD001_Out.csv')
#buffer <- 3 

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Squadron Health Projections"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("days",
                        "Days in Future:",
                        min = 1,
                        max = 150,
                        value = 30),
            sliderInput("buffer",
                        "Buffer on Estimates:",
                        min = 0,
                        max = 20,
                        value = 3),
            sliderInput("sorties",
                        "Sorties per Day:",
                        min = 0,
                        max = 5,
                        value = 1),
            prettyToggle(
                inputId = "percentOrRaw",
                label_on = "Graph Percent Forecast",
                label_off = "Graph Raw Forecast",
                inline = TRUE
            )
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            h4("Number of Jets Mission Ready:"),
           textOutput("summary"),
           br(),
           h4("Bad Motors:"),
           textOutput("badMotor"),
           #plotOutput("histPlot"),
           br(),
           plotOutput("cumline")
           #textOutput("debug")
           #tableOutput("cumTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    temp <- reactive({dat %>%
                        mutate(futureHealth = Predicted - (input$sorties*input$days))})
    ready <- reactive({temp() %>%
                            filter(futureHealth >= input$buffer)})
    not_ready <- reactive({temp() %>%
            filter(futureHealth < input$buffer)})
    
    nready <- reactive({nrow(ready())})
    
    not_nready <- reactive({nrow(not_ready())})
    
    healthyOutlook <- reactive({calc_cum_percentiles(arrange(ready(), futureHealth)$futureHealth, input$percentOrRaw)})
    
    #output$debug <- renderText({ymax()})
    output$summary <- renderText({
        nready()
    })
    output$badMotor <- renderText({not_ready()$X + 1})
    
    output$histPlot <- renderPlot({
        hist(ready()$futureHealth,
             xlab = "RUL",
             main = "Projected Motor Lifespans")
    })
    
    output$cumTable <- renderTable(healthyOutlook())
    
    #TBD Placeholder for setting ylim based on input$percentOrRaw conditional
    ymax <- reactive({ifelse(input$percentOrRaw, 1, 100)})
    yText <- reactive({ifelse(input$percentOrRaw, "Percent Healthy", "Raw Number Healthy")})
    output$cumline <- renderPlot({
        ggplot(data = healthyOutlook(), face = "bold") +
            geom_step(mapping = aes(Days_in_Future, Healthy)) +
            ggtitle("Additional Squadron Health Projections") +
            xlim(0 ,100) +
            ylim(0, ymax()) + 
            ylab(yText()) + 
            theme(plot.title = element_text(size = 20, face = "bold"))
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
