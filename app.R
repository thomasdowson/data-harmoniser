library(shiny)
library(bslib)
library(DT)

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
  )
)

server <- function(input, output, session) {
  
  # Central application state
  state <- reactiveValues(
    datasets = NULL
  )
  
  output$uploaded_files <- renderDT({
    
    req(input$csv_files)
    
    datatable(
      data.frame(
        File = input$csv_files$name,
        stringsAsFactors = FALSE
      ),
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = 10
      )
    )
  })
}

shinyApp(ui, server)