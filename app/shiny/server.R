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

  rv <- reactiveValues(tbl = NULL, clear = F)

  descriptions <- read.table("descriptions.txt", sep = "\t", header = T, row.names = 1)
  annotation <- eventReactive(input$loadMetadata, {
    if(input$checkContam == TRUE) flags <- c("terminate", "relabel")
    else flags <- c("terminate", "relabel", "remove")
    temp <- readRDS("anno.rds")

    filt_temp <- ucla.btc.moR::filter.annotation(temp, ids = input$checkProj, sample_types = input$checkSampleType, 
      paired = as.logical(input$checkPaired), unique = !as.logical(input$checkUnique), replicate = as.logical(input$checkReplicate),
      cross.project = as.logical(input$checkCross), swap_pt = as.logical(input$checkSwap), 
      dx = input$checkDx, grade = input$checkGrade, 
      idh = input$checkIDH, h3 = input$checkH3, 
      flags = flags
    )[,c("Short.ID", "Proj.ID", "Sample.Type", "Line", "Dx", "WHO.Grade", "IDH.status", "H3.status", "Recurrence.Status", "Age.Surgery", "Ethnicity", "Sex")]
  })
  output$anno <- DT::renderDataTable({DT::datatable(annotation(), extensions = "Buttons", options = list(dom = "Blfrtip",buttons = c("csv", "excel"), text = "Export"), rownames = F)}, server = F)
  

  # https://stackoverflow.com/questions/28996337/r-shiny-output-dynamic-number-of-tables
  tableize <- function(chartdatasplit){  ###can add additional arguments like dimension - add to where this is called also and how tabledata indexes
    tables <- list()
    for(x in names(chartdatasplit)){ ##go through all individually stored variable data frames in chartdatasplit list
      tabledata <- chartdatasplit[[x]]  ###function that returns a dataframe to use in table
      tables[[as.character(x)]] <- xtable::print.xtable(xtable::xtable(tabledata, caption=x),type="html", include.rownames = TRUE, html.table.attributes='class="data table table-bordered table-condensed"', caption.placement="top")
    }
    return(paste(tables))    
  }

  df <- eventReactive(input$loadFeatures, {
    features <- unlist(strsplit(input$typeFeatures, "\n"))
    df <- ucla.btc.moR::merge.datasets(annotation(), data_list(), features)
  })
  output$previewFinal <- DT::renderDataTable({DT::datatable(df(), extensions = "Buttons", options = list(dom = "Blfrtip",buttons = c("csv", "excel"), text = "Export"), rownames = T)}, server = F)

  txt <- reactive({
    prnt_msg <- ""
    for(entry in input$checkDatasets){
      temp <- paste(descriptions[entry,"Full.Name"], " (", entry, ") - ", descriptions[entry,"Description"], sep = "")
      if(nchar(prnt_msg) > 0) prnt_msg <- paste(prnt_msg, temp, sep = "\n")
      else prnt_msg <- temp
    }
    prnt_msg
  })

  output$datasets <- renderText({txt()})

  data_list <- eventReactive(input$loadDatasets, {
    data_list <- list()
    for(data in input$checkDatasets){
      temp <- readRDS(paste(data, ".rds", sep = ""))
      data_list[[data]] <- temp
    }
    return(data_list)
  })

  observeEvent(input$loadDatasets, {
    table_list <- list()
    data_list <- list()
    for(data in input$checkDatasets){
      temp <- readRDS(paste(data, ".rds", sep = ""))
      table_list[[data]] <- temp[1:min(5,nrow(temp)),1:min(5,ncol(temp))]
    }
    out <- tableize(table_list)
    output$previewData <- renderUI({
      return(div(HTML(out),class="shiny-html-output"))
    })
  })

  metadataPrint <- eventReactive(input$loadMetadata, {
    if(input$loadMetadata) "continue to Features"
    else ""
  })
  metadataError <- eventReactive(input$loadMetadata, {
    if(!input$loadMetadata) "No preview to load"
    else ""
  })
  output$mPrint <- renderText({metadataPrint()})
  output$mError <- renderText({metadataError()})
  datasetPrint <- eventReactive(input$loadDatasets, {
    if(length(input$checkDatasets) > 0) "continue to Samples"
    else ""
  })
  datasetError <- eventReactive(input$loadDatasets, {
    if(length(input$checkDatasets) == 0){
      "No datasets selected"
    }
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

  output$featBox <- renderUI({
    Map(function(x, y) checkboxGroupInput(y, "", x), ll(), names(ll()))
  })
  observeEvent(input$loadFeatures,{
    if(input$loadDatasets & input$loadMetadata){
      output$tableButton <- renderUI({
        #actionButton("makeTable", "Export Table", icon("table"), style = "color: #fff; background-color: #03ac13; border-color: #028A0F")
        shinySaveButton("makeTable", "Export Table", "Save file as ...", filetype=list(xlsx="xlsx", txt = "txt", csv = "csv"), icon = icon("table"), style = "color: #fff; background-color: #03ac13; border-color: #028A0F")
      })
    }
  })
  observe({
    volumes <- c("Users"="C:/Users/")
    shinyFileSave(input, "makeTable", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$makeTable)
    if (nrow(fileinfo) > 0) {
      if(tools::file_ext(fileinfo$datapath) == "xlsx") xlsx::write.xlsx2(df(), as.character(fileinfo$datapath))
      if(tools::file_ext(fileinfo$datapath) == "txt") write.table(df(), as.character(fileinfo$datapath), sep = "\t", row.names = T, col.names = NA, quote = F)
      if(tools::file_ext(fileinfo$datapath) == "csv") write.table(df(), as.character(fileinfo$datapath), sep = ",", row.names = T, col.names = NA, quote = F)
    }
  })

  observeEvent(input$checkLipidList, {
    rv$clear <- FALSE
  }, priority = 1000)

  lipidAnno <- eventReactive(input$loadLipids, {
    if(!is.null(input$checkLipidList) & !rv$clear){
      file <- input$checkLipidList
      ext <- tools::file_ext(file$datapath)   
      validate(need(ext %in% c("txt", "tsv"), "Please upload a .txt or .tsv"))
      lipids <- read.table(file$datapath, header = input$lipidHeader, sep = "\t")[,1]
      shinyjs::reset('checkLipidList')
      rv$clear <- TRUE
      rv$tbl <- biorunR::annotate.lipid.species(lipids)
    } else {
      lipids <- unlist(strsplit(input$typeLipids, "\n"))
      rv$tbl <- biorunR::annotate.lipid.species(lipids)
    }
    return(rv$tbl)
  })

  observeEvent(input$loadLipids,{
    if(input$loadLipids){
      output$lipidTableButton <- renderUI({
        #actionButton("makeTable", "Export Table", icon("table"), style = "color: #fff; background-color: #03ac13; border-color: #028A0F")
        shinySaveButton("makeLipidTable", "Export Table", "Save file as ...", filetype=list(xlsx="xlsx", txt = "txt", csv = "csv"), icon = icon("table"), style = "color: #fff; background-color: #03ac13; border-color: #028A0F")
      })
    }
  })
  observe({
    volumes <- c("Users"="C:/Users/")
    shinyFileSave(input, "makeLipidTable", roots=volumes, session=session)
    fileinfo <- parseSavePath(volumes, input$makeLipidTable)
    if (nrow(fileinfo) > 0) {
      if(tools::file_ext(fileinfo$datapath) == "xlsx") xlsx::write.xlsx2(lipidAnno(), as.character(fileinfo$datapath))
      if(tools::file_ext(fileinfo$datapath) == "txt") write.table(lipidAnno(), as.character(fileinfo$datapath), sep = "\t", row.names = T, col.names = NA, quote = F)
      if(tools::file_ext(fileinfo$datapath) == "csv") write.table(lipidAnno(), as.character(fileinfo$datapath), sep = ",", row.names = T, col.names = NA, quote = F)
    }
  })
  output$lipid_anno <- DT::renderDataTable({DT::datatable(lipidAnno(), rownames = F)}, server = F)
}

#output$anno = renderTable({
#      head(annotation())
#    })