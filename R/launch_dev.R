#' Quick Launch with Development Settings
#'
#' Convenience function for development that launches the app with common
#' development settings and additional debugging features.
#'
#' @param port Port number (default: 3838)
#' @param host Host address (default: "0.0.0.0" to allow external connections)
#' @param debug Enable debugging features (default: TRUE)
#' @param run Display mode for the app when running in RStudio:
#'   * `"p"` = launch in viewer pane 
#'   * `"b"` = launch in external browser  
#'   * `"w"` = launch in window (default)
#' @param ... Additional arguments passed to launch()
#'
#' @return A Shiny app object
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   # Quick development launch
#'   launch_dev()
#'   
#'   # With custom port
#'   launch_dev(port = 4000)
#'   
#'   # Launch in viewer pane for development
#'   launch_dev(run = "p")
#'   
#'   # Launch in external browser for testing
#'   launch_dev(run = "b")
#' }
launch_dev <- function(port = 3838, 
                       host = "0.0.0.0", 
                       debug = TRUE, 
                       run = "w",
                       ...) {
  
  # Validate run parameter
  if (!run %in% c("p", "b", "w")) {
    stop("run must be one of 'p' (pane), 'b' (browser), or 'w' (window)")
  }
  
  if (debug) {
    # Enable Shiny debugging options
    options(shiny.reactlog = TRUE)
    options(shiny.trace = TRUE)
    options(shiny.error = browser)
    
    message("Debug mode enabled:")
    message("- Reactive log: options(shiny.reactlog = TRUE)")
    message("- Function tracing: options(shiny.trace = TRUE)")  
    message("- Error debugging: options(shiny.error = browser)")
    message("- Access reactive log at: http://localhost:", port, "?_reactlog_")
  }
  
  # Create development volumes with additional useful directories
  dev_volumes <- get_default_volumes()
  
  # Add project-specific directories if they exist
  project_dirs <- c(
    "R" = "R",
    "data" = "data", 
    "inst" = "inst",
    "tests" = "tests"
  )
  
  existing_project_dirs <- project_dirs[sapply(project_dirs, dir.exists)]
  if (length(existing_project_dirs) > 0) {
    # Convert to full paths
    existing_project_dirs <- setNames(
      file.path(getwd(), existing_project_dirs),
      paste("Project", names(existing_project_dirs))
    )
    dev_volumes <- c(dev_volumes, existing_project_dirs)
  }
  
  # Launch with development settings
  launch(
    port = port,
    host = host,
    volumes = dev_volumes,
    theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
    run = run,  # Pass the run parameter to launch()
    ...
  )
}