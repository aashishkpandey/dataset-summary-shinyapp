####################################################
#      Summary App    #
####################################################

library("shiny")
#library("foreign")

shinyUI(pageWithSidebar(
  # Header:
  headerPanel("Summary App"),
  # Input in sidepanel:
  sidebarPanel(

    h5(p("Data Input")),
    fileInput("file", "Upload input data (csv file with header))"),
    
    h5(p("Data Selection (Optional)")),
    htmlOutput("varselect"),
    br()
  ),
  # Main:
  mainPanel( 
    
    tabsetPanel(type = "tabs",
                #
                
                tabPanel("Overview",
                         br()
                         ),
                
                tabPanel("Summary Stats", verbatimTextOutput("summary")),
                tabPanel("Scatter Plot",uiOutput("scatterplots")),
                tabPanel("Correlation", verbatimTextOutput("correlation"),plotOutput("heatmap"))
                

                )
      ) 
    ) 
  )
