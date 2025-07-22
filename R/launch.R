#' Launch the shinyFiles Demo Application
#'
#' Standalone function to launch the shinyFiles demonstration application.
#' This function combines the UI and server components and provides additional
#' configuration options.
#'
#' @param port The port number for the Shiny application (default: NULL for 
#' auto-select)
#' @param host The host IP address (default: "127.0.0.1" for localhost)
#' @param launch.browser Whether to launch the application in a browser 
#' (default: TRUE)
#' @param volumes Named list of custom volumes for file operations. If NULL,
#'   default volumes based on the operating system will be used.
#' @param theme Optional bslib theme to override the default theme
#' @param run Display mode for the app when running in RStudio:
#'   * `"p"` = launch in viewer pane 
#'   * `"b"` = launch in external browser  
#'   * `"w"` = launch in window (default)
#' @param ... Additional arguments passed to shinyApp()
#'
#' @return A Shiny app object
#' 
#' @import shiny
#' @import bslib
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Basic launch
#'   launch()
#'   
#'   # Launch with custom port
#'   launch(port = 8080)
#'   
#'   # Launch in RStudio viewer pane
#'   launch(run = "p")
#'   
#'   # Launch in external browser
#'   launch(run = "b")
#'   
#'   # Launch with custom volumes
#'   custom_volumes <- c("Data" = "/path/to/data", "Projects" = "/path/to/projects")
#'   launch(volumes = custom_volumes)
#'   
#'   # Launch with custom theme
#'   launch(theme = bslib::bs_theme(bootswatch = "darkly"))
#' }
launch <- function(port = NULL, 
                   host = "127.0.0.1", 
                   launch.browser = TRUE,
                   volumes = NULL,
                   theme = NULL,
                   run = "w",
                   ...) {
  
  # validate inputs
  if (!is.null(port) && (!is.numeric(port) || port < 1 || port > 65535)) {
    stop("Port must be a number between 1 and 65535")
  }
  
  if (!is.character(host) || length(host) != 1) {
    stop("Host must be a single character string")
  }
  
  if (!is.logical(launch.browser)) {
    stop("launch.browser must be TRUE or FALSE")
  }
  
  if (!run %in% c("p", "b", "w")) {
    stop("run must be one of 'p' (pane), 'b' (browser), or 'w' (window)")
  }
  
  # Set display type for RStudio
  display_type(run = run)
  
  # set up volumes
  if (is.null(volumes)) {
    volumes <- get_default_volumes()
    message("Using default volumes: ", paste(names(volumes), collapse = ", "))
  } else {
    # validate custom volumes
    if (!is.list(volumes) || is.null(names(volumes))) {
      stop("Custom volumes must be a named list")
    }
    
    # check that all volume paths exist
    invalid_volumes <- !sapply(volumes, function(path) {
      file.exists(path) && file.info(path)$isdir
    })
    
    if (any(invalid_volumes)) {
      warning("Some volume paths do not exist or are not directories: ",
              paste(names(volumes)[invalid_volumes], collapse = ", "))
    }
    
    message("Using custom volumes: ", paste(names(volumes), collapse = ", "))
  }
  
  # UI with custom theme if provided
  ui <- if (!is.null(theme)) {
    # mod the UI to use custom theme
    ui_content <- app_ui()
    # replace the theme in the page_navbar
    ui_content$children[[1]]$attribs$theme <- theme
    ui_content
  } else {
    app_ui()
  }
  
  # server with volumes
  server <- app_server(volumes = volumes)
  
  # Shiny app
  app <- shinyApp(
    ui = ui,
    server = server,
    ...
  )
  
  # options
  launch_options <- list(
    launch.browser = launch.browser,
    host = host
  )
  
  if (!is.null(port)) {
    launch_options$port <- port
  }
  
  # startup message
  cat("\n")
  cat("=== shinyFiles Package Demonstration ===\n")
  cat("Starting application...\n")
  cat("Host:", host, "\n")
  if (!is.null(port)) cat("Port:", port, "\n")
  cat("Volumes:", paste(names(volumes), collapse = ", "), "\n")
  cat("Browser launch:", launch.browser, "\n")
  cat("Display mode:", switch(run, 
                            "p" = "RStudio Viewer Pane",
                            "b" = "External Browser", 
                            "w" = "RStudio Window"), "\n")
  cat("========================================\n")
  cat("\n")
  
  # run the application
  do.call(runApp, c(list(app), launch_options))
}