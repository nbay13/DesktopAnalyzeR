#' This is an example Shiny application
#' It is the same as example_02 in the RStudio Shiny Tutorial
library(shiny)
options(shiny.maxRequestSize=1000*1024^2)
server = function(input, output, session) {
  # IMPORTANT!
  # this is needed to terminate the R process when the
  # shiny app session ends. Otherwise, you end up with a zombie process
  session$onSessionEnded(function() {
    stopApp()
  })

  annotation <- eventReactive(input$previewMetadata, {
    if(input$checkContam == TRUE) flags <- c("terminate", "relabel")
    else flags <- c("terminate", "relabel", "remove")
    temp <- readRDS("anno.rds")
    filt_temp <- ucla.btc.moR::filter.annotation(temp, ids = as.numeric(input$checkProj), sample_types = as.character(input$checkSampleType), 
      paired = as.logical(input$checkPaired), unique = !as.logical(input$checkUnique), replicate = as.logical(input$checkReplicate),
      cross.project = as.logical(input$checkCross), swap_pt = as.logical(input$checkSwap), 
      dx = as.character(input$checkDx), grade = as.numeric(input$checkGrade), 
      idh = as.character(input$checkIDH), h3 = as.character(input$checkH3),
      flags = flags
    )[,c("Short.ID", "Sample.Type", "Line", "Dx", "WHO.Grade", "IDH.status", "H3.status", "Recurrence.Status", "Age.Surgery", "Ethnicity", "Sex")]
  })
  output$anno <- DT::renderDataTable({DT::datatable(annotation(), extensions = "Buttons", options = list(dom = "Blfrtip",buttons = c("tsv", "csv", "excel"), text = "Export"), rownames = F)}, server = F)
  
  txt <- reactive({
    paste(input$checkDatasets, collapse = "\n")
  })

  output$datasets <- renderText({txt()})

  metadataPrint <- eventReactive(input$loadMetadata, {
    if(input$previewMetadata) "continue to Features"
    else ""
  })
  metadataError <- eventReactive(input$loadMetadata, {
    if(!input$previewMetadata) "No preview to load"
    else ""
  })
  output$mPrint <- renderText({metadataPrint()})
  output$mError <- renderText({metadataError()})
  datasetPrint <- eventReactive(input$loadDatasets, {
    if(length(input$checkDatasets) > 0) "continue to Samples"
    else ""
  })
  datasetError <- eventReactive(input$loadDatasets, {
    if(length(input$checkDatasets) == 0) "No datasets selected"
    else ""
  })
  output$dPrint <- renderText({datasetPrint()})
  output$dError <- renderText({datasetError()})

  ll <- reactive({
    if(input$loadDatasets){
      temp <- as.list(input$checkDatasets)
      names(temp) <- input$checkDatasets
      temp
    }
  })

  output$checkbox <- renderUI({
    Map(function(x, y) checkboxGroupInput(y, "", x), ll(), names(ll()))
  })
  observeEvent(input$loadFeatures,{
    if(input$loadDatasets & input$loadMetadata){
      output$tableButton <- renderUI({
        actionButton("makeTable", "Make Table", icon("table"), style = "color: #fff; background-color: #03ac13; border-color: #028A0F")
      })
    }
  })
  
}

#output$anno = renderTable({
#      head(annotation())
#    })