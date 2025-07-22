#' Application Server Function
#'
#' Creates the server logic for the shinyFiles demonstration application
#'
#' @param input Shiny input object
#' @param output Shiny output object  
#' @param session Shiny session object
#' @param volumes Named list of available volumes for file operations. If NULL,
#'   default volumes will be created based on the operating system.
#'
#' @return Server function for use with shinyApp
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   server <- app_server()
#'   # Use with app_ui() in shinyApp()
#' }
app_server <- function(input = NULL, output = NULL, session = NULL, volumes = NULL) {
  
  # Create server function
  function(input, output, session) {
    
    # Define available volumes for file operations if not provided
    if (is.null(volumes)) {
      volumes <- get_default_volumes()
    }
    
    # Initialize modules with cross-module communication capabilities
    file_data <- mod_file_select_server("file_demo", volumes)
    dir_data <- mod_dir_browser_server("dir_demo", volumes)
    advanced_data <- mod_adv_feat_server("advanced_demo", volumes)
    
    # Cross-module data sharing (optional enhancement)
    # This allows modules to share data and coordinate actions
    
    # Share file selections between modules
    observe({
      if (!is.null(file_data$selected_data)) {
        # Could trigger updates in other modules
        # Example: Update advanced module with new data
      }
    })
    
    # Share directory selections
    observe({
      if (!is.null(dir_data$selected_dir_path)) {
        # Could update file selection module's default directory
      }
    })
    
    # # Global error handling
    # observe({
    #   # Monitor for any global errors or notifications that need coordination
    #   # This is where you might implement application-wide error logging
    # })
    
    # Application state management
    app_state <- reactiveValues(
      current_tab = NULL,
      global_data = NULL,
      user_preferences = list()
    )
    
    # Track active tab (if needed for conditional logic)
    observe({
      app_state$current_tab <- input$navbar
    }) |> 
      bindEvent(input$navbar)
    
    # Return app state for testing or external access
    return(list(
      file_data = file_data,
      dir_data = dir_data,
      advanced_data = advanced_data,
      app_state = app_state,
      volumes = volumes
    ))
  }
}