#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(kableExtra)
library(gridExtra)
library(magrittr)
library(knitr)
library(gridExtra)

# Define UI for application that draws a histogram
x <- read_csv("sberry.csv")
y <- read_csv("berries.csv")
ui <- fluidPage(

    # Application title
  title = "Strawberry",
  sidebarLayout(
    tabsetPanel(
      conditionalPanel(
        'input.dataset === "x"'),
      
      conditionalPanel(
        'input.dataset === "y"',
      )
    ),
        # Show a plot of the generated distribution
        mainPanel(
           tabsetPanel(
             id = 'dataset',
             tabPanel("Strawberry",
           fluidRow(
             column(4,
                    selectInput("Year",
                                "Year:",
                                c("All",
                                  unique(as.character(x$Year))))
             ),
             column(4,
                    selectInput("State",
                                "State:",
                                c("All",
                                  unique(as.character(x$State))))
             ),
             column(4,
                    selectInput("type",
                                "type:",
                                c("All",
                                  unique(as.character(x$type))))
             ),
             column(4,
                    selectInput("Measures",
                                "Measures:",
                                c("All",
                                  unique(as.character(x$Measures))))
             ),
             column(4,
                    selectInput("Materials",
                                "Materials:",
                                c("All",
                                  unique(as.character(x$Materials))))
             ),
             column(4,
                    selectInput("Chemical",
                                "Chemical:",
                                c("All",
                                  unique(as.character(x$Chemical))))
             ),
             column(4,
                    selectInput("Value",
                                "Value:",
                                c("All",
                                  unique(as.character(x$Value))))
             )
           ),
           
           DT::dataTableOutput("table1")),

   tabPanel("berries",
            fluidRow(
              column(4,
                     selectInput("Year",
                                 "Year:",
                                 c("All",
                                   unique(as.character(y$Year))))
              ),
              column(4,
                     selectInput("Commodity",
                                 "Commodity:",
                                 c("All",
                                   unique(as.character(y$Commodity))))
              ),
              column(4,
                     selectInput("State",
                                 "State:",
                                 c("All",
                                   unique(as.character(y$State))))
              )
            ),
            DT::dataTableOutput("table2")))
           )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$table1 <- DT::renderDataTable(DT::datatable({
      data <- x
      if (input$Year != "All") {
        data <- data[data$Year == input$Year,]
      }
      if (input$State != "All") {
        data <- data[data$State == input$State,]
      }
      if (input$type != "All") {
        data <- data[data$type == input$type,]
      }
      if (input$Measures != "All") {
        data <- data[data$Measures == input$Measures,]
      }
      if (input$Materials != "All") {
        data <- data[data$Materials == input$Materials,]
      }
      if (input$Chemical!= "All") {
        data <- data[data$Chemical == input$Chemical,]
      }
      if (input$Value != "All") {
        data <- data[data$Value == input$Value,]
      }
      
      data
    }))
    
    output$table2 <- DT::renderDataTable(DT::datatable({
      data2 <- y
      if (input$Year != "All") {
        data2 <- data2[data2$Year == input$Year,]
      }
      if (input$Commodity != "All") {
        data2 <- data2[data2$Commodity == input$Commodity,]
      }
      if (input$State!= "All") {
        data2 <- data2[data2$State == input$State,]
      }
      data2
    }))
}

# Run the application 
shinyApp(ui = ui, server = server)
