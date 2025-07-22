#' File Selection Module UI
#'
#' Creates UI for demonstrating shinyFiles file selection capabilities
#'
#' @param id Module namespace ID
#'
#' @return Shiny UI elements
#'
#' @export
#' 
mod_file_select_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("File Selection Demonstration"),
    bslib::card_body(
      bslib::layout_columns(
        col_widths = c(6, 6),
        
        # File selection controls
        div(
          h4("File Operations"),
          shinyFiles::shinyFilesButton(
            ns("file_select"), 
            "Select Files", 
            "Choose files to load",
            multiple = TRUE,
            icon = shiny::icon("file")
          ),
          br(), br(),
          shinyFiles::shinyFilesButton(
            ns("csv_select"), 
            "Select CSV Files", 
            "Choose CSV files only",
            multiple = FALSE,
            icon = shiny::icon("file-csv")
          ),
          br(), br(),
          actionButton(
            ns("save_gapminder"), 
            "Save Gapminder Data",
            icon = shiny::icon("save")
          ),
          br(), br(),
          shinyFiles::shinySaveButton(
            ns("save_file"), 
            "Save As...", 
            "Save processed data",
            filetype = list(CSV = "csv", Excel = "xlsx", JSON = "json"),
            icon = shiny::icon("download")
          )
        ),
        
        # Results display
        div(
          h4("Selected Files"),
          verbatimTextOutput(ns("selected_files")),
          br(),
          h4("File Contents Preview"),
          DT::DTOutput(ns("file_preview"))
        )
      )
    )
  )
  }
#' File Selection Module Server
#'
#' Server logic for file selection demonstrations
#'
#' @param id Module namespace ID
#' @param volumes Named list of available volumes for file operations
#'
#' @return Reactive values containing selected files and data
#'
#' @export
mod_file_select_server <- function(id, volumes) {
moduleServer(id, function(input, output, session) {
    
    # shinyFiles observers
    
    shinyFiles::shinyFileChoose(
      input = input,
      id = "file_select", 
      roots = volumes, 
      session = session)
  
    shinyFiles::shinyFileChoose(
      input = input, 
      id = "csv_select", 
      roots = volumes, 
      session = session, 
      filetypes = c("", "csv", "txt")
      )
    
    shinyFiles::shinyFileSave(
      input = input, 
      id = "save_file", 
      roots = volumes, 
      session = session)
    
    # Reactive values to store data
    values <- reactiveValues(
      selected_data = NULL,
      file_paths = NULL
    )
    
    # Handle file selection
    observe({
      if (!is.null(input$file_select) && !is.integer(input$file_select)) {
        files <- shinyFiles::parseFilePaths(volumes, input$file_select)
        values$file_paths <- files
        
        # Try to load first file if it's a supported format
        if (nrow(files) > 0) {
          first_file <- as.character(files$datapath[1])
          ext <- tools::file_ext(first_file)
          
          tryCatch({
            if (ext %in% c("csv", "txt")) {
              values$selected_data <- readr::read_csv(
                first_file, 
                show_col_types = FALSE)
            }
          }, error = function(e) {
            showNotification(paste("Error loading file:", e$message), 
              type = "error")
          })
        }
      }
    }) |> 
      bindEvent(input$file_select)
    
    # Handle CSV file selection
    observe({
      if (!is.null(input$csv_select) && !is.integer(input$csv_select)) {
        file <- shinyFiles::parseFilePaths(volumes, input$csv_select)
        if (nrow(file) > 0) {
          tryCatch({
            values$selected_data <- readr::read_csv(as.character(file$datapath), 
                                                   show_col_types = FALSE)
            showNotification("CSV file loaded successfully!", type = "success")
          }, error = function(e) {
            showNotification(paste("Error loading CSV:", e$message), 
              type = "error")
          })
        }
      }
    }) |> 
      bindEvent(input$csv_select)
    
    # Save gapminder data to temporary location
    observe({
      temp_dir <- tempdir()
      gapminder_path <- file.path(temp_dir, "gapminder_demo.csv")
      readr::write_csv(gapminder::gapminder, gapminder_path)
      values$selected_data <- gapminder::gapminder
      showNotification(
        paste("Gapminder data saved to:", gapminder_path), 
        type = "success",
        duration = 5
      )
    }) |> 
      bindEvent(input$save_gapminder)
    
    # Handle file saving
    observe({
      if (!is.null(input$save_file) && !is.integer(input$save_file) && 
          !is.null(values$selected_data)) {
        file_info <- parseSavePath(volumes, input$save_file)
        
        if (nrow(file_info) > 0) {
          file_path <- as.character(file_info$datapath)
          ext <- tools::file_ext(file_path)
          tryCatch({
            switch(ext,
              "csv" = readr::write_csv(values$selected_data, file_path),
              "xlsx" = writexl::write_xlsx(values$selected_data, file_path),
              "json" = jsonlite::write_json(values$selected_data, file_path, 
                                          pretty = TRUE)
            )
            showNotification("File saved successfully!", type = "success")
          }, error = function(e) {
            showNotification(paste("Error saving file:", e$message), type = "error")
          })
        }
      }
    }) |> 
      bindEvent(input$save_file)
    
    # Display selected files
    output$selected_files <- renderText({
      if (!is.null(values$file_paths) && nrow(values$file_paths) > 0) {
        paste("Selected files:", paste(values$file_paths$name, collapse = ", "))
      } else {
        "No files selected"
      }
    })
    
    # Preview file contents
    output$file_preview <- DT::renderDT({
      if (!is.null(values$selected_data)) {
        DT::datatable(
          values$selected_data,
          options = list(
            scrollX = TRUE,
            pageLength = 5,
            dom = 'tp'
          )
        )
      }
    })
    return(values)
  })
}
# Copy in UI
# mod_file_select_ui("id")
# Copy in server
# mod_file_select_server("id")

