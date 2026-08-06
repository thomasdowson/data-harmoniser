library(shiny)
library(bslib)
library(DT)
library(lubridate)

source("R/00_pipeline.R")
source("R/01_import.R")
source("R/02_inspect.R")
source("R/03_standardise_names.R")
source("R/04_clean_values.R")
source("R/05_detect_keys.R")
source("R/06_validate_joins.R")
source("R/07_join_data.R")

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
      inputId = "left_dataset",
      label = "Left dataset",
      choices = NULL
    ),
    
    selectInput(
      inputId = "right_dataset",
      label = "Right dataset",
      choices = NULL
    ),
    
    selectInput(
      inputId = "join_key",
      label = "Join key",
      choices = NULL
    ),
    
    selectInput(
      inputId = "join_type",
      label = "Join type",
      choices = c(
        "left",
        "inner",
        "right",
        "full"
      )
    ),
    
    actionButton(
      inputId = "join_button",
      label = "Join datasets"
    )
  ),
  
  card(
    card_header("Available Datasets"),
    DTOutput("available_datasets")
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
    card_header("Join Validation"),
    DTOutput("join_validation")
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
    validation = NULL,
    joined_data = NULL
  )
  
  # Import and prepare uploaded CSV files
  observeEvent(input$csv_files, {
    
    req(input$csv_files)
    
    state$datasets <- prepare_datasets(input$csv_files)
    state$key_candidates <- detect_keys(state$datasets)
    state$validation <- NULL
    state$joined_data <- NULL
    
    dataset_names <- names(state$datasets)
    
    updateSelectInput(
      session,
      "left_dataset",
      choices = dataset_names,
      selected = dataset_names[1]
    )
    
    updateSelectInput(
      session,
      "right_dataset",
      choices = dataset_names,
      selected = dataset_names[min(2, length(dataset_names))]
    )
  })
  
  # Update join keys for the selected pair of datasets
  observe({
    
    req(state$key_candidates)
    req(input$left_dataset)
    req(input$right_dataset)
    
    pair_candidates <- state$key_candidates[
      (
        state$key_candidates$dataset_1 == input$left_dataset &
          state$key_candidates$dataset_2 == input$right_dataset
      ) |
        (
          state$key_candidates$dataset_1 == input$right_dataset &
            state$key_candidates$dataset_2 == input$left_dataset
        ),
      ,
      drop = FALSE
    ]
    
    updateSelectInput(
      session,
      "join_key",
      choices = unique(pair_candidates$column)
    )
  })
  
  # Validate, join and save the joined dataset for future chained joins
  observeEvent(input$join_button, {
    
    req(state$datasets)
    req(input$left_dataset)
    req(input$right_dataset)
    req(input$join_key)
    req(input$join_type)
    
    left_data <- state$datasets[[input$left_dataset]]
    right_data <- state$datasets[[input$right_dataset]]
    
    state$validation <- validate_join(
      left_data = left_data,
      right_data = right_data,
      by = input$join_key
    )
    
    state$joined_data <- join_data(
      left_data = left_data,
      right_data = right_data,
      by = input$join_key,
      join = input$join_type
    )
    
    new_name <- paste(
      tools::file_path_sans_ext(input$left_dataset),
      tools::file_path_sans_ext(input$right_dataset),
      sep = "_"
    )
    
    # Avoid silently overwriting an earlier chained result
    if (new_name %in% names(state$datasets)) {
      suffix <- 2
      
      while (
        paste0(new_name, "_", suffix) %in% names(state$datasets)
      ) {
        suffix <- suffix + 1
      }
      
      new_name <- paste0(new_name, "_", suffix)
    }
    
    state$datasets[[new_name]] <- state$joined_data
    
    # Recalculate candidates because a new dataset now exists
    state$key_candidates <- detect_keys(state$datasets)
    
    dataset_names <- names(state$datasets)
    
    updateSelectInput(
      session,
      "left_dataset",
      choices = dataset_names,
      selected = new_name
    )
    
    updateSelectInput(
      session,
      "right_dataset",
      choices = dataset_names
    )
  })
  
  output$available_datasets <- renderDT({
    
    req(state$datasets)
    
    datatable(
      data.frame(
        Dataset = names(state$datasets),
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
    
    req(input$available_datasets_rows_selected)
    req(state$datasets)
    
    selected_dataset <- names(state$datasets)[
      input$available_datasets_rows_selected
    ]
    
    datatable(
      inspect_dataset(
        state$datasets[[selected_dataset]]
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
  
  output$join_validation <- renderDT({
    
    req(state$validation)
    
    datatable(
      state$validation,
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE
      )
    )
  })
  
  output$dataset_preview <- renderDT({
    
    req(input$available_datasets_rows_selected)
    req(state$datasets)
    
    selected_dataset <- names(state$datasets)[
      input$available_datasets_rows_selected
    ]
    
    datatable(
      head(state$datasets[[selected_dataset]], 10),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
}

shinyApp(ui, server)