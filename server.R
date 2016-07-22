#################################################
#      Summary App    #
#################################################
if (!require("shiny")) {
  install.packages("shiny", dependencies = TRUE)
  library(gplots)
}


if (!require("pastecs")) {
  install.packages("pastecs", dependencies = TRUE)
  library(gplots)
}

if (!require("RColorBrewer")) {
  install.packages("RColorBrewer", dependencies = TRUE)
  library(RColorBrewer)
}

if (!require("Hmisc")) {
  install.packages("Hmisc", dependencies = TRUE)
  library(gplots)
}

if (!require("ggplot2")) {
  install.packages("ggplot2", dependencies = TRUE)
  library(gplots)
}

if (!require("reshape2")) {
  install.packages("reshape2", dependencies = TRUE)
  library(gplots)
}

if (!require("gplots")) {
  install.packages("gplots", dependencies = TRUE)
  library(gplots)
}


shinyServer(function(input, output,session) {
  
Dataset <- reactive({
  if (is.null(input$file)) { return(NULL) }
  else{
    Dataset <- as.data.frame(read.csv(input$file$datapath ,header=TRUE, sep = ","))
    return(Dataset)
  }
})

# Select variables:
output$varselect <- renderUI({
  if (identical(Dataset(), '') || identical(Dataset(),data.frame())) return(NULL)
  # Variable selection:
  
  checkboxGroupInput("Attr", "Choose Attributes (At least 2 attributes must be selected)",
                     colnames(Dataset()), colnames(Dataset())[1])
  
})

mydata = reactive({
  mydata = Dataset()[,input$Attr]  
  return(mydata)
})


out = reactive({
data = mydata()
Dimensions = dim(data)
Head = head(data)
Tail = tail(data)
Class = NULL
for (i in 1:ncol(data)){
  c1 = class(data[,i])
  Class = c(Class, c1)
}

nu = which(Class %in% c("numeric","interger"))
fa = which(Class %in% c("factor","character"))
nu.data = data[,nu] 
fa.data = data[,fa] 
Summary = list(Numeric.data = round(stat.desc(nu.data) ,4), factor.data = describe(fa.data))

a = seq(from = 0, to=200,by = 4)
j = length(which(a < ncol(nu.data)))
out = list(Dimensions = Dimensions,Summary =Summary ,Tail=Tail,fa.data,nu.data,a,j)
return(out)
})

output$summary = renderPrint({
  if (is.null(input$file)) {return(NULL)}
  else {
    out()[1:2]
      }
})


output$scatterplots <- renderUI({
  if (is.null(input$file)) {return(NULL)}
  else {
    
    plot_output_list <- lapply(1:out()[[7]], function(i) {
      plotname <- paste("plot", i, sep="")
      plotOutput(plotname, height = 700, width = 700)
    })
    # Convert the list to a tagList - this is necessary for the list of items
    # to display properly.
    do.call(tagList, plot_output_list)
  }
})

# Call renderPlot for each one. Plots are only actually generated when they
# are visible on the web page.
max_plots = 50

for (i in 1:max_plots) {
  # Need local so that each item gets its own number. Without it, the value
  # of i in the renderPlot() will be the same across all instances, because
  # of when the expression is evaluated.
  local({
    
    my_i <- i 
    plotname <- paste("plot", my_i, sep="")
    
    output[[plotname]] <- renderPlot({
      out1 = out()
      a = out1[[6]]
      j = my_i
      if (ncol(out1[[5]]) == a[j] + 1){
        a1 = a[j]+1
        a2 = a[j]-1
        dai = out1[[5]][,a1:a2]
        plot(dai)
        }
      
      else if ( ncol(out1[[5]]) < a[j + 1]){
        a1 = a[j]+1
        a2 = ncol(out1[[5]])
        dai = out1[[5]][,a1:a2]
        plot(dai)
      }
      
      else if(ncol(out1[[5]]) > a[j + 1]){
        a1 = a[j]+1
        a2 = a[j + 1]
        dai = out1[[5]][,a1:a2]
        plot(dai)
      }
      
      mtext(paste("Scater plot " ,my_i), side = 3, line = 2, cex=2)
        })
  })
}

output$heatmap = renderPlot({ 
  data <- airquality[,1:4]

  qplot(x=Var1, y=Var2, data=melt(cor(out()[[5]], use = "pairwise.complete.obs")), fill=value, geom="tile") +
    scale_fill_gradient2(limits=c(-1, 1))
  
})

output$correlation = renderPrint({
  cor(out()[[5]], use = "pairwise.complete.obs")
  })

})

