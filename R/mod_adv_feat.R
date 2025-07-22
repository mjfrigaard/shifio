#' Advanced Features Module UI
#'
#' Creates UI for demonstrating advanced shinyFiles features
#'
#' @param id Module namespace ID
#'
#' @return Shiny UI elements
#'
#' @export
#' 
mod_adv_feat_ui <- function(id) {
  ns <- NS(id)
  
  bslib::card(
    bslib::card_header("Advanced Features Demonstration"),
    bslib::card_body(
      bslib::layout_columns(
        col_widths = c(6, 6),
        
        # Custom file type filters
        bslib::card(
          bslib::card_header("File Type Filtering"),
          bslib::card_body(
            p("Demonstrate custom file type filters and restrictions"),
            shinyFiles::shinyFilesButton(
              ns("image_select"), 
              "Select Images", 
              "Choose image files only",
              multiple = TRUE,
              icon = shiny::icon("image")
            ),
            br(), br(),
            shinyFiles::shinyFilesButton(
              ns("data_select"), 
              "Select Data Files", 
              "Choose data files (CSV, Excel, JSON)",
              multiple = TRUE,
              icon = shiny::icon("database")
            ),
            br(), br(),
            verbatimTextOutput(ns("filtered_files"))
          )
        ),
        
        # Custom root and restrictions
        bslib::card(
          bslib::card_header("Custom Restrictions"),
          bslib::card_body(
            p("Demonstrate custom root directories and access restrictions"),
            shinyFiles::shinyDirButton(
              ns("restricted_dir"), 
              "Browse Restricted", 
              "Limited to specific directories",
              icon = shiny::icon("lock")
            ),
            br(), br(),
            verbatimTextOutput(ns("restricted_path"))
          )
        )
      ),
      
      br(),
      
      # File operations with gapminder
      bslib::card(
        bslib::card_header("Gapminder Data Processing"),
        bslib::card_body(
          bslib::layout_columns(
            col_widths = c(4, 8),
            
            div(
              selectInput(
                ns("continent_filter"), 
                "Filter by Continent:",
                choices = c("All", unique(gapminder::gapminder$continent)),
                selected = "All"
              ),
              numericInput(
                ns("year_filter"), 
                "Filter by Year:",
                value = 2007,
                min = min(gapminder::gapminder$year),
                max = max(gapminder::gapminder$year),
                step = 5
              ),
              br(),
              shinyFiles::shinySaveButton(
                ns("save_filtered"), 
                "Save Filtered Data", 
                "Save the filtered dataset",
                filetype = list(CSV = "csv", Excel = "xlsx"),
                icon = shiny::icon("filter")
              )
            ),
            
            div(
              plotOutput(ns("gapminder_plot")),
              br(),
              DT::DTOutput(ns("filtered_data"))
            )
          )
        )
      )
    )
  )
}

#' Advanced Features Module Server
#'
#' Server logic for advanced shinyFiles features
#'
#' @param id Module namespace ID
#' @param volumes Named list of available volumes
#'
#' @return Reactive values containing advanced feature data
#'
#' @export
#' 
mod_adv_feat_server <- function(id, volumes) {
  moduleServer(id, function(input, output, session) {
    
    # create restricted volumes (only temp directory)
    restricted_volumes <- list("Temp" = tempdir())
    
    # initialize file choosers with custom filters
    shinyFiles::shinyFileChoose(
      input, "image_select", 
      roots = volumes, 
      session = session,
      filetypes = c("", "jpg", "jpeg", "png", "gif", "bmp", "tiff")
    )
    
    shinyFiles::shinyFileChoose(
      input, "data_select", 
      roots = volumes, 
      session = session,
      filetypes = c("", "csv", "xlsx", "xls", "json", "txt", "rds")
    )
    
    shinyFiles::shinyDirChoose(
      input, "restricted_dir", 
      roots = restricted_volumes, 
      session = session
    )
    
    shinyFiles::shinyFileSave(
      input, "save_filtered", 
      roots = volumes, 
      session = session
    )
    
    # reactive values
    values <- reactiveValues(
      image_files = NULL,
      data_files = NULL,
      filtered_gapminder = NULL
    )
    
    # Handle image file selection
    observe({
      if (!is.null(input$image_select) && !is.integer(input$image_select)) {
        values$image_files <- shinyFiles::parseFilePaths(volumes, input$image_select)
      }
    }) |> 
      bindEvent(input$image_select)
    
    # Handle data file selection
    observe({
      if (!is.null(input$data_select) && !is.integer(input$data_select)) {
        values$data_files <- shinyFiles::parseFilePaths(volumes, input$data_select)
      }
    }) |> 
      bindEvent(input$data_select)
    
    # filter gapminder data
    filtered_data <- reactive({
      data <- gapminder::gapminder
      
      if (input$continent_filter != "All") {
        data <- data[data$continent == input$continent_filter, ]
      }
      
      data <- data[data$year == input$year_filter, ]
      
      values$filtered_gapminder <- data
      return(data)
    })
    
    # handle saving filtered data
    observe({
      if (!is.null(input$save_filtered) && !is.integer(input$save_filtered) && 
          !is.null(values$filtered_gapminder)) {
        file_info <- parseSavePath(volumes, input$save_filtered)
        
        if (nrow(file_info) > 0) {
          file_path <- as.character(file_info$datapath)
          ext <- tools::file_ext(file_path)
          
          tryCatch({
            switch(ext,
              "csv" = readr::write_csv(values$filtered_gapminder, file_path),
              "xlsx" = writexl::write_xlsx(values$filtered_gapminder, file_path)
            )
            showNotification("Filtered data saved successfully!", type = "success")
          }, error = function(e) {
            showNotification(paste("Error saving file:", e$message), type = "error")
          })
        }
      }
    }) |> 
      bindEvent(input$save_filtered)
    
    # Display filtered files
    output$filtered_files <- renderText({
      image_text <- if (!is.null(values$image_files) && nrow(values$image_files) > 0) {
        paste("Images:", paste(values$image_files$name, collapse = ", "))
      } else {
        "Images: None selected"
      }
      
      data_text <- if (!is.null(values$data_files) && nrow(values$data_files) > 0) {
        paste("Data files:", paste(values$data_files$name, collapse = ", "))
      } else {
        "Data files: None selected"
      }
      
      paste(image_text, data_text, sep = "\n\n")
    })
    
    # Display restricted directory path
    output$restricted_path <- renderText({
      if (!is.null(input$restricted_dir) && !is.integer(input$restricted_dir)) {
        dir_path <- parseDirPath(restricted_volumes, input$restricted_dir)
        if (length(dir_path) > 0) {
          paste("Restricted directory:", dir_path)
        } else {
          "No restricted directory selected"
        }
      } else {
        "No restricted directory selected"
      }
    })
    
    # Create gapminder plot
    output$gapminder_plot <- renderPlot({
      data <- filtered_data()
      
      if (nrow(data) > 0) {
        ggplot2::ggplot(data, ggplot2::aes(x = gdpPercap, y = lifeExp)) +
          ggplot2::geom_point(ggplot2::aes(size = pop, color = continent), alpha = 0.7) +
          ggplot2::scale_x_log10() +
          ggplot2::labs(
            title = paste("Life Expectancy vs GDP Per Capita -", input$year_filter),
            x = "GDP Per Capita (log scale)",
            y = "Life Expectancy",
            size = "Population",
            color = "Continent"
          ) +
          ggplot2::theme_minimal()
      }
    })
    
    # Display filtered data table
    output$filtered_data <- DT::renderDT({
      data <- filtered_data()
      
      if (nrow(data) > 0) {
        DT::datatable(
          data,
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
# mod_name_ui("id")
# Copy in server
# mod_name_server("id")

