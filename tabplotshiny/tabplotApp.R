library(shiny)
library(tabplot)
data(diamonds, package="ggplot2")

dataset <- diamonds
vars <- names(dataset)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Diamonds tableplot"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput("sortCol", label="Sort on:", choices=vars),
      checkboxInput("decreasing", label="Descending", value=FALSE),
      numericInput("from", label="from:", value=0, min=0, max=100),
      numericInput("to", label="to:", value=100, min=0, max=100),
      checkboxGroupInput("select", label="Select columns:", choices=vars, selected=vars),
      numericInput("nBins", label="# bin", value=100, min=2, max=500, step=1)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput("plot", height="800px")
      
    )
  )
)


#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
server<-function(input, output) {
  
  dataset <- reactive(function(){
    diamonds
  })
  
  output$plot <- reactivePlot(function() {
    dat <- dataset()
    sortCol <- input$sortCol
    decreasing <- input$decreasing
    select <- input$select
    from <- input$from
    to <- input$to
    nBins <- max(2,as.numeric(input$nBins), na.rm=TRUE)
    tableplot(dat, from=from, to=to, sortCol = sortCol, select_string = select, decreasing = decreasing, nBins=nBins)
  })
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)