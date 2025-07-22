#' Shiny app display mode helper
#'
#' @param run where to launch app: 
#'  * `"p"` = launch in viewer pane 
#'  * `"b"` = launch in external browser  
#'  * `"w"` = launch in window (default)
#'
#' @return notification of `shinyViewerType` option
#' 
#' @export
#'
display_type <- function(run = "w") {
  
  # Validate run parameter
  if (!run %in% c("p", "b", "w")) {
    stop("run must be one of 'p' (pane), 'b' (browser), or 'w' (window)")
  }
  
  if (Sys.getenv("RSTUDIO") == "1") {
    # Set the appropriate viewer type based on run parameter
    switch(run,
      p = options(shiny.launch.browser = .rs.invokeShinyPaneViewer),
      b = options(shiny.launch.browser = .rs.invokeShinyWindowExternal),
      w = options(shiny.launch.browser = .rs.invokeShinyWindowViewer),
      stop("Invalid run parameter")
    )
    
    environment <- "RStudio"
    
    # Get viewer type information
    viewer_option <- getOption('shiny.launch.browser')
    if (is.function(viewer_option)) {
      shinyViewerType <- switch(run,
        "p" = "Viewer Pane",
        "b" = "External Browser", 
        "w" = "RStudio Window",
        "Unknown"
      )
    } else {
      shinyViewerType <- "Browser (default)"
    }
    
    # Use cli if available, otherwise use message
    if (requireNamespace("cli", quietly = TRUE)) {
      cli::cli_alert_info("App running in {environment}")
      cli::cli_alert_info("shinyViewerType set to {shinyViewerType}")
    } else {
      message("App running in ", environment)
      message("shinyViewerType set to ", shinyViewerType)
    }
  } else {
    environment <- "Non-RStudio"
    if (requireNamespace("cli", quietly = TRUE)) {
      cli::cli_alert_info("App not running in RStudio")
      cli::cli_alert_info("Using system default browser")
    } else {
      message("App not running in RStudio")
      message("Using system default browser")
    }
  } 
}