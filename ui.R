library(shiny)
library(markdown)
library(corrplot)


shinyUI(fluidPage(
    # Application title
  titlePanel(list("Correlation Matrix with", tags$i("corrplot"), "adapted by Matthieu Rouland (23-11-2020)")),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Dataset", 
                  c("upload my own", "mtcars")),
      conditionalPanel("input.dataset === 'upload my own'",
                       fileInput("datafile", "")
                       # textInput("chooseSize", "Graph size ", value = "1000")
                       ),
      tags$hr(),
      
      selectInput("corMethod", "Correlation Method",
                  eval(formals(cor)$method), "spearman"),
      selectInput("corUse", "NA Action",
                  c("pairwise.complete.obs", "everything", "all.obs", "complete.obs", "na.or.complete" )),
      tags$hr(),
      
      #Only works if we are not showing confidence interval
      conditionalPanel("!input.showConf",
                       selectInput("plotMethod", "Plot Method",
                                   list("mixed", all = eval(formals(corrplot)$method)), "square"),
                       conditionalPanel("input.plotMethod === 'mixed'",
                                        wellPanel(
                                          selectInput("plotLower", "Lower Method", eval(formals(corrplot)$method)),
                                          selectInput("plotUpper", "Upper Method", eval(formals(corrplot)$method)))
                                        )
                       ),
      conditionalPanel("input.showConf || input.plotMethod !== 'mixed'",
                       selectInput("plotType", "Plot Type",
                                   eval(formals(corrplot)$type), "upper")),
      
      selectInput("plotOrder", "Reorder Correlation",
                  eval(formals(corrplot)$order)),
      conditionalPanel("input.plotOrder === 'hclust'",
                       wellPanel(
                         selectInput("plotHclustMethod", "Method",
                                     eval(formals(corrplot)$hclust.method)),
                         numericInput("plotHclustAddrect", "Number of Rectangles", 3, 0, NA))),
      
      tags$hr(),
      checkboxInput("sigTest", "Significance Test", value = TRUE),
      conditionalPanel("input.sigTest",
                       # numericInput("sigLevel", "Significane Level",
                       #              0.05, 0, 1, 0.01),
                       selectInput("sigAction", "Insignificant Action",
                                   eval(formals(corrplot)$insig), "label_sig")),
      checkboxInput("showConf", "Show Confidence Interval"),
      conditionalPanel("input.showConf",
                       selectInput("confPlot", "Ploting Method",
                                   eval(formals(corrplot)$plotCI)[-1]),
                       numericInput("confLevel", "Confidence Level",
                                  0.95, 0, 1, 0.01)),
      
      sliderInput("sliderSize", "Slider pour faire jolie mais marche po'", 100, 10000, 1000),
      # downloadButton("report", "Download PDF version")
      
      
    ),

    # Show a plot of the generated correlation
    mainPanel(
      tabsetPanel(
        tabPanel("Correlation", 
                 column(3, 
                        radioButtons("variablesStyle", "Variable Selection Style", c("Checkbox", "Selectize"), inline = T),
                        helpText("Choose the variables to display. Drag and drop to reorder."), 
                        conditionalPanel("input.variablesStyle === 'Checkbox'",
                                         sortableCheckboxGroupInput("variablesCheckbox", "", c("Loading..."))),
                        conditionalPanel("input.variablesStyle === 'Selectize'",
                                         sortableSelectizeInput("variables", "", c("Loading..."), multiple = T, options = list(plugins = list("remove_button"))))),
                 #TODO: size adjuster or size selection --> need a uiOUTPUT that reflect to the presented size
                 column(9, 
                        plotOutput("corrPlot", height = 1500, width = 1500), 
                        uiOutput("sizedcorrplot"),

                        uiOutput("warning"))
                 ),
        tabPanel("Uploaded Data",
                 dataTableOutput("dataTable")),
        
        tabPanel("n= for each correlation",
                 dataTableOutput("ndataTable")),
        
        tabPanel("About/Help",
                 includeMarkdown("README.md"))
        )
    )
  )
))
