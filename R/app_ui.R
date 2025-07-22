#' Application UI Function
#'
#' Creates the complete user interface for the shinyFiles demonstration
#' application
#'
#' @return A Shiny UI object using bslib components
#'
#' @export
#'
#' @examples
#' if (interactive()) {
#'   ui <- app_ui()
#'   # Use with app_server() in shinyApp()
#' }
app_ui <- function() {
  bslib::page_navbar(
    title = "shifio: shinyFiles Package Demonstration",
    theme = bslib::bs_theme(version = 5,
      bootswatch = "flatly"),
    
    # File Selection Tab
    bslib::nav_panel(
      title = "File Selection",
      icon = shiny::icon("file"),
      mod_file_select_ui("file_demo")
    ),
    
    # Directory Browser Tab
    bslib::nav_panel(
      title = "Directory Browser",
      icon = shiny::icon("folder"),
      mod_dir_browser_ui("dir_demo")
    ),
    
    # Advanced Features Tab
    bslib::nav_panel(
      title = "Advanced Features",
      icon = shiny::icon("cogs"),
      mod_adv_feat_ui("advanced_demo")
    ),
    
    # Documentation Tab
    bslib::nav_panel(
      title = "Documentation",
      icon = shiny::icon("book"),
      bslib::card(
        bslib::card_header("shinyFiles Package Features"),
        bslib::card_body(
          h3("Overview"),
          p("This application demonstrates some of the major features of the shinyFiles package:"),
          tags$ul(
            tags$li(strong("File Selection:"), "Choose single or multiple files with custom filters"),
            tags$li(strong("Directory Browsing:"), "Navigate and explore directory structures"),
            tags$li(strong("File Saving:"), "Save data in various formats with custom names"),
            tags$li(strong("Custom Filters:"), "Restrict file selection by type or extension"),
            tags$li(strong("Volume Management:"), "Work with different file system roots"),
            tags$li(strong("Restricted Access:"), "Limit browsing to specific directories")
          ),
          
          h3("Data Source"),
          p("This demonstration uses the", 
            tags$a("gapminder", href = "https://cran.r-project.org/web/packages/gapminder/"),
            "package, which provides life expectancy, population, and GDP data for countries over time."),
          
          h3("Usage Examples"),
          p("Try these workflows to explore all features:"),
          tags$ol(
            tags$li(strong("File Selection:"), "Use 'Save Gapminder Data' to create sample files, then select them using the file choosers"),
            tags$li(strong("Directory Operations:"), "Select directories to explore their contents and create sample datasets"),
            tags$li(strong("Data Processing:"), "Filter gapminder data by continent and year, then save the results"),
            tags$li(strong("Custom Filters:"), "Test file type restrictions with the specialized file choosers")
          )
        )
      )
    ),
    
    # Footer
    bslib::nav_spacer(),
    bslib::nav_item(
      tags$a(
        "shinyFiles GitHub", 
        href = "https://github.com/thomasp85/shinyFiles",
        target = "_blank",
        class = "nav-link"
      )
    )
  )
}
