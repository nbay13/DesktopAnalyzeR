#' This is an example shiny application
#' It is the same as example_02 in the RStudio Shiny Tutorial

library(shiny)
library(shinyFiles)
tweaks <- 
  list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 150px;
                                   -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 3;    /* Firefox */ 
                                   column-count: 3; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 "),
                              HTML("
                                 .multicolmid { 
                                   height: 100px;
                                   -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 3;    /* Firefox */ 
                                   column-count: 3; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 "),
                            HTML("
                                 .multicolsmall { 
                                   height: 75px;
                                   -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 3;    /* Firefox */ 
                                   column-count: 3; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 "),
                            HTML("
                                 .multicolsmaller { 
                                   height: 50px;
                                   -webkit-column-count: 4; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 4;    /* Firefox */ 
                                   column-count: 4; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
                                 ))

  controls <-
  list(h4("UCLA Datasets"), 
       tags$div(align = 'left', 
                class = 'multicol', 
                checkboxGroupInput(inputId  = 'checkDatasets',
                                   label = NULL,
                                   choices = list("Exp Counts" = "counts",
                                             "Exp CPM" = "cpm",
                                             "Exp TPM" = "tpm", 
                                             "ssGSEA" = "ssgsea",
                                             "Neftel States" = "state", 
                                             "GBM Subtypes" = "subtype",
                                             "LGG Subtypes" = "subtype2",
                                             "CSx UCLA" = "ciber",
                                             "CSx Neftel" = "ciber2",
                                             "Align QC" = "align",
                                             "Model QC" = "model",
                                             "Contam QC" = "contam"),
                                   selected = NULL,
                                   inline   = FALSE)))

  controls2 <-
  list(h4("Project ID"), 
       tags$div(align = 'left', 
                class = 'multicolsmall', 
                checkboxGroupInput(inputId  = 'checkProj',
                                   label = NULL,
                                   choices = list(3,
                                             11,
                                             12,
                                             13, 
                                             16,
                                             19),
                                   selected = c(3),
                                   inline   = FALSE)))

  controls3 <-
  list(h4("Glioma Diagnosis"), 
       tags$div(align = 'left', 
                class = 'multicolsmall', 
                checkboxGroupInput(inputId  = 'checkDx',
                                   label = NULL,
                                   choices = list("Glioblastoma" = "GBM",
                                             "Astrocytoma" = "AST",
                                             "Oligodendroglioma" = "ODG",
                                             "Diffuse midline glioma" = "DMG", 
                                             "Other" = "Other"),
                                   selected = c("GBM"),
                                   inline   = FALSE)))

  controls3_idh <-
  list(h5("IDH Status"), 
       tags$div(align = 'left', 
                class = 'multicolsmaller', 
                checkboxGroupInput(inputId  = 'checkIDH',
                                   label = NULL,
                                   choices = list("WT" = "0",
                                             "mtIDH1" = "1",
                                             "mtIDH2" = "2"),
                                   selected = c(0),
                                   inline   = FALSE)))

  controls3_h3 <-
  list(h5("H3.3 Status"), 
       tags$div(align = 'left', 
                class = 'multicolsmaller', 
                checkboxGroupInput(inputId  = 'checkH3',
                                   label = NULL,
                                   choices = list("WT" = "0",
                                             "mtH3K27M" = "K27",
                                             "mtH3G34" = "G34"),
                                   selected = c(0),
                                   inline   = FALSE)))

  controls4 <-
  list(h4("WHO Grade"), 
       tags$div(align = 'left', 
                class = 'multicolsmaller', 
                checkboxGroupInput(inputId  = 'checkGrade',
                                   label = NULL,
                                   choices = list("Grade I" = 1,
                                             "Grade II" = 2,
                                             "Grade III" = 3,
                                             "Grade IV" = 4),
                                   selected = c(4),
                                   inline   = FALSE)))

  controls5 <-
  list(h4("Sample Type"), 
       tags$div(align = 'left', 
                class = 'multicolmid', 
                checkboxGroupInput(inputId  = 'checkSampleType',
                                   label = NULL,
                                   choices = list("Purified Patient" = "PT",
                                             "Bulk Patient" = "BULK",
                                             "DPDOX" = "XG",
                                             "Gliomasphere" = "GS",
                                             "Direct SubQ Xeno" = "SQX",
                                             "SDX" = "SDX",
                                             "XDS" = "XDS",
                                             "Purified CD45+" = "CD45"),
                                   selected = c("PT", "XG", "GS"),
                                   inline   = FALSE)))

  controls6 <-
  list(h4("Use all features from these datasets"),
    tags$head(tags$style("
      #featBox{
        margin-bottom: 5px; /*set the margin, so boxes don't overlap*/
      }
      #featBox input[type='checkbox']{ /* style for checkboxes */
        margin-bottom: 0px;
      }
  ")),
    tableOutput('featBox'),
    h4("Type a list of features to include (one per line)"),
    textAreaInput(inputId = "typeFeatures",label = "", height = '150px')
  ) 
       
# ui = fluidPage(
#     tabsetPanel(
#       tabPanel("Map", fluid = TRUE,
#                sidebarLayout(
#                  sidebarPanel(selectInput("Country", "Select Country", choices = "", selected = "")),
#                  mainPanel(
#                    htmlOutput("Attacks")
#                  )
#                )
#       ),
#       tabPanel("plot", fluid = TRUE,
#                sidebarLayout(
#                  sidebarPanel(sliderInput("year", "Year:", min = 1968, max = 2009, value = 2009, sep='')),
#                  mainPanel(fluidRow(
#                    column(7,  plotly::plotlyOutput("")),
#                    column(5, plotly::plotlyOutput(""))   
#                  )
#                  )
#                )
#       )
#     )
#   )

ui = navbarPage(title = "UCLA Brain SPORE | Multi-omics Resource",
  tabPanel("Data Curator", 
    tabsetPanel(
      tabPanel("Datasets", tweaks,
        sidebarLayout(
          sidebarPanel(
            h2("Dataset Selection"),
            controls,
            h4("Upload Dataset"),
            fileInput("checkDataFile", label = NULL, multiple = TRUE),
            fluidRow(
              column(4,
                actionButton("loadDatasets", "Load Datasets", icon("paper-plane"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              ),
              column(4,
                textOutput("dPrint"),
              ),
              column(4,
                textOutput("dError"),
              ),
            ),
          ),
          mainPanel(
            tags$div(#sprintf("Global Variable Value: %s", GLOBAL_VAR)),
            h3("Datasets Preview"),
            tags$head(tags$style("#dError{color: red;
                                           font-size: 15px;
                                           font-style: bold;
                                         }"

              )),
            verbatimTextOutput("datasets"),
            uiOutput("previewData")
            )
          )
        )
      ),
      tabPanel("Samples", tweaks, 
        sidebarLayout(
          sidebarPanel(
            h2("Sample Selection"),
            br(),
            controls2,
            checkboxInput("checkCross", label = "Include cross-project samples?", value = TRUE),
            br(),
            controls4,
            controls3,
            controls3_idh, controls3_h3,
            controls5,
            h4("Other Filters"),
            checkboxInput("checkPaired", label = "Only matched samples?", value = FALSE),
            checkboxInput("checkReplicate", label = "Allow replicates?", value = FALSE),
            checkboxInput("checkUnique", label = "Allow multi-passage xenografts in the same line?", value = FALSE),
            br(),
            h4("Danger Zone"),
            checkboxInput("checkContam", label = "Include patient samples w/ normal contamination?", value = FALSE),
            checkboxInput("checkSwap", label = "Use Bulk Patient if Purified Patient is missing?", value = FALSE),
            br(),
            h4("Type a list of samples to include (one per line)"),
            textAreaInput(inputId = "typeSamples",label = "", height = '100px'),
            h4("Upload Sample List as tab-seperated txt file"),
            fileInput("checkSampleList", label = NULL, multiple = TRUE),
            fluidRow(
              column(5,
                actionButton("loadMetadata", "Load Metadata Table", icon("paper-plane"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
              ),
              column(4,
                textOutput("mPrint"),
              ),
              column(3,
                textOutput("mError"),
              ),
            ),
          ),
          mainPanel(
            tags$div(#sprintf("Global Variable Value: %s", GLOBAL_VAR)),
              h3("Annotation Table Preview"),
              br(),
              DT::dataTableOutput("anno"),
              tags$head(tags$style("#mError{color: red;
                                           font-size: 15px;
                                           font-style: bold;
                                         }"

              ))
            )
          )
        )    
      ),
      tabPanel("Features", tweaks,
        sidebarLayout(
          sidebarPanel(
            h2("Feature Selection"),
            controls6,
            h4("Upload Feature List as tab-seperated txt file"),
            fileInput("checkFeatureList", label = NULL, multiple = TRUE),
            fluidRow(
              column(4, 
                actionButton("loadFeatures", "Load Features", icon("paper-plane"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
              ),
              column(4,
                uiOutput('tableButton')
              ),
            ),
            #actionButton("makeTable", "Make Table", icon("table"), style = "color: #fff; background-color: #03ac13; border-color: #2e6da4")
          ),
          mainPanel(
            tags$div(
            h3("Features Preview"),
            DT::dataTableOutput("previewFinal")
            )
          )
        )
      )
    )
  ),
  tabPanel("Lipid Annotator",
    sidebarLayout(
      sidebarPanel(
        h2("Lipid Species Annotation"),
        br(),
        h4("Type a list of lipid species to annotate (one per line)"),
        textAreaInput(inputId = "typeLipids",label = "", height = '100px'),
        h4("Upload Lipid Species List as tab-seperated txt file"),
        fileInput("checkLipidList", label = NULL, multiple = FALSE),
        div(style = "margin-top: -15px"),
        checkboxInput("lipidHeader", "uploaded file contains column header?", value = TRUE),
        column(4, 
          actionButton("loadLipids", "Load Lipids", icon("paper-plane"), style="color: #fff; background-color: #337ab7; border-color: #2e6da4")
        ),
        column(4, 
          uiOutput('lipidTableButton')
        ),
        br(),br()
      ),
      mainPanel(
        h3("Lipid Annotation Preview"),
        br(),
        DT::dataTableOutput("lipid_anno"),
      )
    )
  )
)


