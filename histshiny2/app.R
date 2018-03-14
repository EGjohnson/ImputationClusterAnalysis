library(shiny)
library(ggplot2)
library(itertools2)
library(grid)
library(ggthemes)
prop.sale.raw<-readRDS("data/propsale.rds")
prop.sale<-prop.sale.raw[,sapply(prop.sale.raw, is.numeric)]
summary.prop<-summary(subset(prop.sale,select=-c(REID,sale.years)))
#===============================================================
min.vec<-unname(sapply(prop.sale,function(x){min(x,na.rm=TRUE)}))
max.vec<-unname(sapply(prop.sale,function(x){max(x,na.rm=TRUE)}))
my.diff<-max.vec-min.vec
my.names<-names(prop.sale)
my.colors<-rainbow(length(my.names))
my.index<-1:length(my.names)
#================================================================
theme_Publication <- function(base_size=25, base_family="helvetica") {

  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",size = rel(1.2), hjust = 0.0),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(0.2, "cm"),
            legend.margin = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,10,10,10),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
  
}
#=========================================================================================


# Define UI for app that draws a histogram ----)
ui <- fluidPage(
  
  # App title >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  titlePanel("Property Value Histograms"),
  
  # Sidebar layout with input and output definitions ----
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
    #----------------------------------------------------------------------------------
      selectInput('prop.col', 'Property Characteristic', c("Year Built" =2.0, "Building Size"=3.0,"Price"=4.0)),
    
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
                value =0.50)
    ),
    #----------------------------------------------------------------------------
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    # Main panel for displaying outputs >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot"),
      
      # Output: Header + summary of distribution ----
      h4("Summary"),
      verbatimTextOutput("summary")
      
    )
  )
)
# Define server logic required to draw a histogram ----
server <- function(input, output) {
  #figure out what the max and min values will be given the slider window
  min.val.col<-reactive({ min.vec[as.integer(input$prop.col)]+input$min.val*my.diff[as.integer(input$prop.col)]  })
  max.val.col<-reactive({ min.vec[as.integer(input$prop.col)]+input$max.val*my.diff[as.integer(input$prop.col)] })
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
  #*********************************************************************************
  output$distPlot <- renderPlot({
    ggplot(prop.sale.sub(),aes_string(my.names[as.integer(input$prop.col)])) + 
      geom_histogram(bins=input$bins,fill=my.colors[as.integer(input$prop.col)])+
      xlim(c(min.val.col(),max.val.col()))+
      ggtitle(max.vec[as.integer(input$prop.col)])+
      theme_Publication()
    })
  #**********************************************************************************
  
  # Generate a summary of the dataset ----
  output$summary <- renderPrint({
    #dataset <- datasetInput()
    summary.prop
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)