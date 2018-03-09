library(shiny)
library(ggplot2)
prop.sale<-readRDS("data/propsale.rds")



my.col<-c("PRICE","darkgreen")

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
    #----------------------------------------------------------------------------------
      selectInput('prop.col', 'Property Characteristic', c('None', names(prop.sale))),
    
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      
      
      sliderInput(inputId = "min.val",
                  label = "min value of histogram:",
                  min = 150000,
                  max = 500000,
                  value =100000),
     
    
      sliderInput(inputId = "max.val",
                label = "max value of histogram:",
                min = 150000,
                max = 500000,
                value =160000)
    ),
    #----------------------------------------------------------------------------
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    # Main panel for displaying outputs >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  
  prop.sale.sub<-reactive({subset(prop.sale,PRICE>input$min.val & PRICE<input$max.val)})
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    ggplot(prop.sale.sub(),aes_string(my.col[1])) + 
      geom_histogram(bins=input$bins,fill=my.col[2])+xlim(c(input$min.val,input$max.val))
    })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)