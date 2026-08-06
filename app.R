library(shiny)
library(bslib)
library(DT)

source("R/00_pipeline.R")
source("R/01_import.R")
source("R/02_inspect.R")
source("R/03_standardise_names.R")
source("R/04_clean_values.R")
source("R/05_detect_keys.R")
source("R/06_join_data.R")

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
    ),
    
    hr(),
    
    h5("Join"),
    
    selectInput(
      "left_dataset",
      "Left dataset",
      choices = NULL
    ),
    
    selectInput(
      "right_dataset",
      "Right dataset",
      choices = NULL
    ),
    
    selectInput(
      "join_key",
      "Join key",
      choices = NULL
    ),
    
    selectInput(
      "join_type",
      "Join type",
      choices = c(
        "left",
        "inner",
        "right",
        "full"
      )
    ),
    
    actionButton(
      "join_button",
      "Join datasets"
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
    card_header("Joined Dataset"),
    DTOutput("joined_dataset")
  ),
  
  card(
    card_header("Dataset Preview"),
    DTOutput("dataset_preview")
  )
)

server <- function(input, output, session) {
  
  state <- reactiveValues(
    datasets = NULL,
    key_candidates = NULL,
    joined_data = NULL
  )
  
  observeEvent(input$csv_files, {
    
    req(input$csv_files)
    
    state$datasets <- prepare_datasets(input$csv_files)
    state$key_candidates <- detect_keys(state$datasets)
    state$joined_data <- NULL
    
    updateSelectInput(
      session,
      "left_dataset",
      choices = names(state$datasets)
    )
    
    updateSelectInput(
      session,
      "right_dataset",
      choices = names(state$datasets)
    )
  })
  
  observe({
    
    req(state$key_candidates)
    
    updateSelectInput(
      session,
      "join_key",
      choices = unique(state$key_candidates$column)
    )
  })
  
  observeEvent(input$join_button, {
    
    req(state$datasets)
    req(input$left_dataset)
    req(input$right_dataset)
    req(input$join_key)
    req(input$join_type)
    
    state$joined_data <- join_data(
      left_data = state$datasets[[input$left_dataset]],
      right_data = state$datasets[[input$right_dataset]],
      by = input$join_key,
      join = input$join_type
    )
  })
  
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
  
  output$joined_dataset <- renderDT({
    
    req(state$joined_data)
    
    datatable(
      state$joined_data,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      )
    )
  })
  
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