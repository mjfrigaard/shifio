#' Directory Browser Module UI
#'
#' Creates UI for demonstrating shinyFiles directory browsing capabilities
#'
#' @param id Module namespace ID
#'
#' @return Shiny UI elements
#'
#' @export
#' 
mod_dir_browser_ui <- function(id) {
ns <- NS(id)
  bslib::card(
    bslib::card_header("Directory Browser Demonstration"),
    bslib::card_body(
      bslib::layout_columns(
        col_widths = c(4, 8),
        # dir selection controls
        div(
          h4("Directory Operations"),
          shinyFiles::shinyDirButton(
            ns("dir_select"), 
            "Select Directory", 
            "Choose a directory to explore",
            icon = shiny::icon("folder-open")
          ),
          br(), br(),
          shinyFiles::shinyDirButton(
            ns("output_dir"), 
            "Choose Output Directory", 
            "Select directory for saving files",
            icon = shiny::icon("folder")
          ),
          br(), br(),
          actionButton(
            ns("create_sample_files"), 
            "Create Sample Files",
            icon = shiny::icon("file-plus")
          ),
          br(), br(),
          verbatimTextOutput(ns("selected_dir"))
        ),
        # dir contents
        div(
          h4("Directory Contents"),
          DT::DTOutput(
            outputId = ns("dir_contents")
            ),
          br(),
          h4("Directory Statistics"),
          tableOutput(
            outputId = ns("dir_stats")
            )
        )
      )
    )
  )
}

#' Directory Browser Module Server
#'
#' Server logic for directory browsing demonstrations
#'
#' @param id Module namespace ID
#' @param volumes Named list of available volumes for directory operations
#'
#' @return Reactive values containing directory information
#'
#' @export
#' 
mod_dir_browser_server <- function(id, volumes) {
  moduleServer(id, function(input, output, session) {
    
    # initialize shinyFiles observers
    shinyFiles::shinyDirChoose(
      input = input,
      id = "dir_select", 
      roots = volumes, 
      session = session)
  
    shinyFiles::shinyDirChoose(
      input = input, 
      id = "output_dir", 
      roots = volumes, 
      session = session
      )
    
    # reactive values
    values <- reactiveValues(
      selected_dir_path = NULL,
      output_dir_path = NULL,
      dir_contents = NULL
    )
    
    # handle directory selection
    observe({
      if (!is.null(input$dir_select) && !is.integer(input$dir_select)) {
        dir_path <- parseDirPath(volumes, input$dir_select)
        values$selected_dir_path <- dir_path
        
        # get directory contents
        if (length(dir_path) > 0 && dir.exists(dir_path)) {
          files <- list.files(dir_path, full.names = TRUE, 
                             include.dirs = TRUE, all.files = FALSE)
          
          if (length(files) > 0) {
            file_info <- file.info(files)
            file_info$name <- basename(files)
            file_info$path <- files
            file_info$type <- ifelse(file_info$isdir, "Directory", "File")
            file_info$size_mb <- round(file_info$size / 1024 / 1024, 2)
            
            values$dir_contents <- file_info[, c("name", "type", "size_mb", 
                                               "mtime", "mode")]
          } else {
            values$dir_contents <- data.frame(
              name = "Empty directory",
              type = NA,
              size_mb = NA,
              mtime = NA,
              mode = NA
            )
          }
        }
      }
    }) |> 
      bindEvent(input$dir_select)
    
    # output directory selection
    observe({
      if (!is.null(input$output_dir) && !is.integer(input$output_dir)) {
        values$output_dir_path <- parseDirPath(volumes, input$output_dir)
      }
    }) |> 
      bindEvent(input$output_dir)
    
    # sample files
    observe({
      if (!is.null(values$output_dir_path) && length(values$output_dir_path) > 0) {
        output_dir <- values$output_dir_path
        
        tryCatch({
          # sample CSV files with gapminder subsets
          countries <- unique(gapminder::gapminder$country)[1:5]
          
          for (country in countries) {
            country_data <- gapminder::gapminder[gapminder::gapminder$country == country, ]
            filename <- paste0(gsub(" ", "_", country), "_data.csv")
            filepath <- file.path(output_dir, filename)
            readr::write_csv(country_data, filepath)
          }
          
          # summary file
          summary_data <- gapminder::gapminder |>
            aggregate(cbind(lifeExp, pop, gdpPercap) ~ continent + year, 
                     data = _, FUN = mean)
          readr::write_csv(summary_data, file.path(output_dir, "summary_by_continent.csv"))
          
          showNotification("Sample files created successfully!", type = "success")
          
          # refresh directory contents if we're viewing the output directory
          if (values$selected_dir_path == output_dir) {
            # trigger refresh by simulating directory selection
            updateActionButton(session, "refresh_dir")
          }
          
        }, error = function(e) {
          showNotification(paste("Error creating files:", e$message), type = "error")
        })
        
      } else {
        showNotification("Please select an output directory first.", type = "warning")
      }
    }) |> 
      bindEvent(input$create_sample_files)
    
    # selected directory
    output$selected_dir <- renderText({
      if (!is.null(values$selected_dir_path) && length(values$selected_dir_path) > 0) {
        paste("Selected directory:", values$selected_dir_path)
      } else {
        "No directory selected"
      }
    })
    
    # directory contents
    output$dir_contents <- DT::renderDT({
      if (!is.null(values$dir_contents)) {
        DT::datatable(
          values$dir_contents,
          options = list(
            scrollX = TRUE,
            pageLength = 10,
            dom = 'tp'
          ),
          colnames = c("Name", "Type", "Size (MB)", "Modified", "Permissions")
        )
      }
    })
    
    # directory statistics
    output$dir_stats <- renderTable({
      if (!is.null(values$dir_contents) && nrow(values$dir_contents) > 0) {
        stats <- data.frame(
          Metric = c("Total Items", "Files", "Directories", "Total Size (MB)"),
          Value = c(
            nrow(values$dir_contents),
            sum(values$dir_contents$type == "File", na.rm = TRUE),
            sum(values$dir_contents$type == "Directory", na.rm = TRUE),
            round(sum(values$dir_contents$size_mb, na.rm = TRUE), 2)
          )
        )
        stats
      }
    })
    
    return(values)
  })
}
# Copy in UI
# mod_dir_browse_ui("id")
# Copy in server
# mod_dir_browse_server("id")

