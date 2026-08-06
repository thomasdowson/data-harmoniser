library(shiny)
library(bslib)
library(DT)

source("R/01_import.R")

ui <- page_sidebar(
  title = "Data Harmoniser",
  
  sidebar = sidebar(
    fileInput(
      "csv_files",
      "Upload CSV files",
      multiple = TRUE,
      accept = ".csv"
    ),
    
    hr(),
    
    h5("Workflow"),
    
    tags$ol(
      tags$li("Upload"),
      tags$li("Inspect"),
      tags$li("Clean"),
      tags$li("Join"),
      tags$li("Export")
    )
  ),
  
  card(
    card_header("Uploaded Files"),
    DTOutput("uploaded_files")
  )
)

server <- function(input, output, session) {
  
  state <- reactiveValues(
    datasets = NULL
  )
  
  observeEvent(input$csv_files, {
    
    state$datasets <- import_csvs(input$csv_files)
    
  })
  
  output$uploaded_files <- renderDT({
    
    req(input$csv_files)
    
    datatable(
      data.frame(
        File = input$csv_files$name
      ),
      rownames = FALSE,
      options = list(
        dom = "t"
      )
    )
  })
  
}

shinyApp(ui, server)