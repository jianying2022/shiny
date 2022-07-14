library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Regression model"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      
      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator ----
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      
      # Input: Select quotes ----
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head"),
      radioButtons("regression", "Regression",
                   choices = c(regression = "regression",
                               noregression = "nonregression"),
                   selected = "nonregression")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("Plot"),
      tableOutput("contents")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dataInput <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    return(df)
  })
  
    
  output$Plot <- renderPlot({
    
    
    if(input$regression == "nonregression") {
      
      return(ggplot(data = dataInput(), aes(dataInput()$x,dataInput()$y)) + 
               geom_point())
      
      }
    
    else {
      
      return(ggplot(data = dataInput(), aes(dataInput()$x,dataInput()$y)) + 
             geom_point() + 
             geom_smooth(method = "lm", se=FALSE) +
             stat_regline_equation(label.y = 15, aes(label = ..eq.label..)) +
             stat_regline_equation(label.y = 14, aes(label = ..rr.label..)))
    }
  
  })
  
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    
    if(input$disp == "head") {
      return(head(dataInput()))
    }
    else {
      return(dataInput())
    }
    
  })
}
  
# Run the application 
shinyApp(ui = ui, server = server)