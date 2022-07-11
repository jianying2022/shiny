#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
Regrex1 <- read.csv("regrex1.csv")
ui <- fluidPage(
  titlePanel("Regression Model (Dataset: Regrex1)"),
    
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  tabPanel("Scatterplot", plotOutput("scatterplot")), # Plot
                  tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
                  tabPanel("Data", DT::dataTableOutput('tbl')) # Data as datatable
                  
      )
    )
  )



# SERVER
server <- function(input, output) {
  
  # Regression output
  #model = lm(formula = y ~ x,   data = dataset)
  #summary(model)
  output$summary <- renderPrint({
    fit <- lm(Regrex1$y ~ Regrex1$x)
    summary(fit)
  })
  
  # Data output
  output$tbl = DT::renderDataTable({
    DT::datatable(Regrex1, options = list(lengthChange = FALSE))
  })
  
  
  # Scatterplot output
  #ggplot() +
   # geom_point(aes(x = dataset$x, y = dataset$y),
    #           colour = 'red') +
    #geom_line(aes(x = dataset$x, y = predict(model, newdata = dataset)),
    #          colour = 'blue') +
   # ggtitle('y vs x') +
    # xlab('x') +
    # ylab('y') 
  
  
  output$scatterplot <- renderPlot({
    plot(Regrex1$x, Regrex1$y,  main="Scatterplot",
         xlab="Regrex1$x", ylab="Regrex1$y", pch=19)
    abline(lm(Regrex1$y ~ Regrex1$x), col="red")
  }, height=400)
  
}

shinyApp(ui = ui, server = server)
