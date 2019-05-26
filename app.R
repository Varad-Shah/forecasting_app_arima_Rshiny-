library(shiny)
library(ggplot2)
library(readxl)
library(DT)

final<-read_xlsx(".\\final.xlsx")
dtforhist<-read_xlsx(".\\dtforhist.xlsx")


# Define UI for application 
ui <- fluidPage(
  
  # Application title
  titlePanel("Forecast"),
  
  #create input to select values from columns
  fluidRow(
    column(4,
           selectInput(inputId =  "Part_No",
                       label =  "Part No:",
                       c("All",
                         unique(as.character(final$`Part No.`))))
           
    ),
    column(4,
           selectInput(inputId =  "Plant_Code",
                       label = "Plant Code:",
                       c("All",
                         unique(as.character(final$`Plant Code`))))
    ),
    column(4,
           selectInput(inputId = "Description",
                       label = "Part Description:",
                       c("All",
                         unique(as.character(final$Description))))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table"),
  
  # Define the sidebar with one input
  sidebarLayout(      
            sidebarPanel(
              selectInput(inputId =  "partnumber",label =  "Part No - Plant Code - Buyer", 
                          choices=colnames(dtforhist)[-1])
            ),
      # Create a spot for the barplot
      mainPanel(
        plotOutput("partno")  
      )
    )
)

# Define server logic to create a datatable
server <- function(input, output) {
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- final
    if (input$Part_No != "All") {
      data <- data[data$`Part No.` == input$Part_No,]
    }
    if (input$Plant_Code != "All") {
      data <- data[data$`Plant Code` == input$Plant_Code,]
    }
    if (input$Description != "All") {
      data <- data[data$Description == input$Description,]
    }
    data
  }))
  # Render a barplot
  output$partno <- renderPlot({
    dthist = dtforhist[ ,c("..1",input$partnumber)]
    
    barplot(dthist[[2]],
            main=input$partnumber,
            names.arg = dtforhist$..1,font.axis=4,las=2)
    }
  )
  }


# Run the application 
shinyApp(ui = ui, server = server)

