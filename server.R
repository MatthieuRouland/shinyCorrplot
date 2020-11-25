library(shiny)
library(corrplot)
library(ggplot2)
library(readxl)
library(Hmisc)
source("corTest.R", local = TRUE)

shinyServer(function(input, output, session) {
  
  ## Data Input and Pre-processing ###################
  
  dataset <- reactive({
    datasource <- input$dataset
    if(datasource == "upload my own") {
      inFile <- input$datafile
      if(is.null(inFile)) {
        NULL
      } else {
        #TODO: Better way to unescape e.g. \\t
       read_excel(inFile$datapath)
      }
    } else {
      eval(parse(text = datasource))
    }
  })
  
  
  numericColumns <- reactive({
    df <- dataset()
    colnames(df)[sapply(df, is.numeric)]
  })
  
  correlation <- reactive({
    data <- dataset()
    variables <- input$variables
    if(is.null(data) || !length(intersect(variables, colnames(data)))) {
      NULL
    } else {
      cor(dataset()[,input$variables], use = input$corUse, method = input$corMethod)
    }
  })
  
  correlationADJUSTED <- reactive({
    data <- dataset()
    variables <- input$variables
    if(is.null(data) || !length(intersect(variables, colnames(data)))) {
      NULL
    } else {
      ###++++ de stringence avec la correction de  Holm == corrected for multiple inference using Holm's
      cohort_select_corv2 <- rcorr.adjust(dataset(), type = input$corMethod, use = input$corUse)
      cohort_select_corv2_P <- cohort_select_corv2$R$P
      cohort_select_corv2_n <- cohort_select_corv2$R$n
      cohort_select_corv2_r <- cohort_select_corv2$R$r 
      
      #in case of No NA, NaN, or infinite in our matrix => replace with null values
      cohort_select_corv2_r[is.na(cohort_select_corv2_r)] <- 0
      cohort_select_corv2_r[is.nan(cohort_select_corv2_r)] <- 0
      cohort_select_corv2_r[is.infinite(cohort_select_corv2_r)] <- 0
      
      #in case of >1 or <-1 => correct to 1 or -1
      cohort_select_corv2_r[cohort_select_corv2_r > 1 ] <- 1
      cohort_select_corv2_r[cohort_select_corv2_r < sign(-1) ] <- -1
      
      #in case of No NA, NaN, or infinite in our matrix => replace with null p-values
      cohort_select_corv2_P[is.na(cohort_select_corv2_P)] <- 0.99
      cohort_select_corv2_P[is.nan(cohort_select_corv2_P)] <- 0.99
      
      return(new("cor_adjusted",
                 cohort_select_corv2_P =cohort_select_corv2_P,
                 cohort_select_corv2_n = cohort_select_corv2_n,
                 cohort_select_corv2_r = cohort_select_corv2_r))
      # cor(dataset()[,input$variables], use = input$corUse, method = input$corMethod)
    }
  })
  sigConfMat <- reactive({
    val <- correlation()
    if(!is.null(val))
      corTest(val, input$confLevel)
  })
  
  ## Data and Correlation Validation and UI Updates ##########
  
  #Update hclust rect max
  observe({
    val <- correlation()
    if(!is.null(val))
      updateNumericInput(session, "plotHclustAddrect", max = nrow(val))
  })
  
  #Update variable selection
  observe({
    updateCheckboxGroupInput(session, "variablesCheckbox", choices = numericColumns(), selected = numericColumns())
    
    updateSelectInput(session, "variables", choices = numericColumns(), selected = numericColumns())
  })
  
  #Link Variable Selection
  observe({
    if(input$variablesStyle == "Checkbox") {
      updateCheckboxGroupInput(session, "variablesCheckbox", selected = isolate(input$vairables))
    }
  })
  observe({
    updateSelectInput(session, "variables", selected = input$variablesCheckbox)
  })

    
  output$warning <- renderUI({
    val <- correlation()
    if(is.null(val)) {
      tags$i("Waiting for data input...")
    } else {
      isNA <- is.na(val)
      if(sum(isNA)) {
      tags$div(
        tags$h4("Warning: The following pairs in calculated correlation have been converted to zero because they produced NAs!"),
        # helpText("Consider using an approriate NA Action to exclude missing data"),
        renderTable(expand.grid(attr(val, "dimnames"))[isNA,]))
      }
    }
  })
  
  ## Correlation Plot ####################################
  # corrPlotReactive <- reactive({
  output$corrPlot <- renderPlot({ 
    #TODO: add color input to select differnts colorampalette
    col2 <- colorRampPalette(c( "#053061"    , "#2166AC" ,  "#4393C3"    , "#92C5DE",   "#D1E5F0", 
                                "#FFFFFF",  "#FDDBC7","#F4A582" , "#D6604D" ,"#B2182B",  "#67001F"))
    
    val <- correlation()
    if(is.null(val)) return(NULL)

    val[is.na(val)] <- 0
    args <- list(val,
                 order = if(input$plotOrder == "manual") "original" else input$plotOrder, 
                 hclust.method = input$plotHclustMethod, 
                 addrect = input$plotHclustAddrect,
                 
                 # cl.cex= input$choosenheight,
                 col = col2(100),
                 tl.col = "black", 
                 outline = "black",
                 pch.col = "black",
                 
                 p.mat = sigConfMat()[[1]],
                 # sig.level = if(input$sigTest) input$sigLevel else NULL,
                 sig.level = if(input$sigTest) c(.001, .01, .05) else NULL,
                 
                 insig = if(input$sigTest) input$sigAction else NULL,
                
                 pch.cex = 1, 
                 tl.cex = 1,
                 
                 lowCI.mat = sigConfMat()[[2]],
                 uppCI.mat = sigConfMat()[[3]],
                 plotCI = if(input$showConf) input$confPlot else "n")
    
    if(input$showConf) {
      do.call(corrplot, c(list(type = input$plotType), args))
    } else if(input$plotMethod == "mixed") {
      do.call(corrplot.mixed, c(list(lower = input$plotLower,
                                     upper = input$plotUpper),
                                args))
    } else {
      do.call(corrplot, c(list(method = input$plotMethod, type = input$plotType), args))
    }
   })
  # })
  
  output$sizedcorrplot <- renderUI({
    plotOutput("corrPlot", height = as.numeric(input$sliderSize))
  }
  )
  ### Report PDF Graph
  # output$report <- downloadHandler(
  #   filename = function () {("Myplot.png")},
  #   content = function(file) {
  #     png(file)
  #     print(
  #       uiOutput("corrPlot")
  #       )
  #     dev.off()
  #     }
  # )
  ## Data Table ####################
  
  output$dataTable <- renderDataTable(dataset())
  # output$ndataTable <- renderDataTable(ndatacor())
  
  
  ## Corrplot ADJUSTED with holm's post test
  output$corrPlotADJUSTED <- renderPlot({ 
    #TODO: add color input to select differnts colorampalette
    col2 <- colorRampPalette(c( "#053061"    , "#2166AC" ,  "#4393C3"    , "#92C5DE",   "#D1E5F0", 
                                "#FFFFFF",  "#FDDBC7","#F4A582" , "#D6604D" ,"#B2182B",  "#67001F"))
    
    val <- correlationADJUSTED()
    if(is.null(val)) return(NULL)
    
    val[is.na(val)] <- 0
    args <- list(val$cor_adjusted$cohort_select_corv2_r,
                 order = if(input$plotOrder == "manual") "original" else input$plotOrder, 
                 hclust.method = input$plotHclustMethod, 
                 addrect = input$plotHclustAddrect,
                 
                 col = col2(100),
                 tl.col = "black", 
                 outline = "black",
                 pch.col = "black",
                 
                 p.mat = sigConfMat()[[1]],
                 sig.level = if(input$sigTest) c(.001, .01, .05) else NULL,
                 
                 insig = if(input$sigTest) input$sigAction else NULL,
                 
                 pch.cex = 1, 
                 tl.cex = 1,
                 
                 lowCI.mat = sigConfMat()[[2]],
                 uppCI.mat = sigConfMat()[[3]],
                 plotCI = if(input$showConf) input$confPlot else "n")
    
    if(input$showConf) {
      do.call(corrplot, c(list(type = input$plotType), args))
    } else if(input$plotMethod == "mixed") {
      do.call(corrplot.mixed, c(list(lower = input$plotLower,
                                     upper = input$plotUpper),
                                args))
    } else {
      do.call(corrplot, c(list(method = input$plotMethod, type = input$plotType), args))
    }
  })
  # })
  
})
