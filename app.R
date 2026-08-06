library(shiny)
library(bslib)
library(DT)

source("R/01_import.R")
source("R/02_inspect.R")

ui <- page_sidebar(
  
  title = "Data Harmoniser",
  
  sidebar = sidebar(
    
    fileInput(
      inputId = "csv_files",
      label = "Upload CSV files",
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
  ),
  
  card(
    card_header("Dataset Summary"),
    DTOutput("dataset_summary")
  ),
  
  card(
    card_header("Dataset Preview"),
    DTOutput("dataset_preview")
  )
  
)

server <- function(input, output, session) {
  
  # Application state
  state <- reactiveValues(
    datasets = NULL
  )
  
  # Import uploaded CSV files
  observeEvent(input$csv_files, {
    
    req(input$csv_files)
    
    state$datasets <- import_csvs(input$csv_files)
    
  })
  
  # Uploaded files table
  output$uploaded_files <- renderDT({
    
    req(input$csv_files)
    
    datatable(
      data.frame(
        File = input$csv_files$name,
        stringsAsFactors = FALSE
      ),
      rownames = FALSE,
      selection = "single",
      options = list(
        dom = "t"
      )
    )
    
  })
  #
  output$dataset_summary <- renderDT({
    
    req(input$uploaded_files_rows_selected)
    req(state$datasets)
    
    selected_file <-
      input$csv_files$name[input$uploaded_files_rows_selected]
    
    datatable(
      inspect_dataset(
        state$datasets[[selected_file]]
      ),
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE
      )
    )
    
  })
  # Preview selected dataset
  output$dataset_preview <- renderDT({
    
    req(input$uploaded_files_rows_selected)
    req(state$datasets)
    
    selected_file <-
      input$csv_files$name[input$uploaded_files_rows_selected]
    
    datatable(
      head(state$datasets[[selected_file]], 10),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
    
  })
  
}

shinyApp(ui, server)