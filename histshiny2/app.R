library(shiny)
library(ggplot2)
prop.sale.raw<-readRDS("data/propsale.rds")


prop.sale<-prop.sale.raw[,sapply(prop.sale.raw, is.numeric)]
#===============================================================
min.vec<-unname(sapply(prop.sale,function(x){min(x,na.rm=TRUE)}))
max.vec<-unname(sapply(prop.sale,function(x){max(x,na.rm=TRUE)}))
my.names<-names(prop.sale)
my.colors<-rainbow(length(my.names))

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  titlePanel("Property Value Histograms"),
  
  # Sidebar layout with input and output definitions ----
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
    #----------------------------------------------------------------------------------
      selectInput('prop.col', 'Property Characteristic', c("PropCol"=4.0)),
    
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      
      
      sliderInput(inputId = "min.val",
                  label = "min value of histogram:",
                  min = 0,
                  max = 1.0,
                  value =0.0),
     
    
      sliderInput(inputId = "max.val",
                label = "max value of histogram:",
                min = 0.0,
                max = 1.0,
                value =1.0)
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
  #figure out what the max and min values will be given the slider window
  min.val.col<-reactive({max.vec[as.integer(input$prop.col)]*input$min.val})
  max.val.col<-reactive({max.vec[as.integer(input$prop.col)]*input$max.val})
  #subset the data frame to include only the range of the column we are interested in displaying
  prop.sale.sub<-reactive({ 
    prop.sale[prop.sale[,as.integer(input$prop.col)]>=min.val.col() &  prop.sale[,as.integer(input$prop.col)]<=max.val.col(), ] 
              })
  
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    ggplot(prop.sale.sub(),aes_string(my.names[as.integer(input$prop.col)])) + 
      geom_histogram(bins=input$bins,fill=my.colors[as.integer(input$prop.col)])+
      xlim(c(min.val.col(),max.val.col()))+
      ggtitle(max.vec[as.integer(input$prop.col)])
    })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)