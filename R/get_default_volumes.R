#' Get Default Volumes Based on Operating System
#'
#' Creates a list of default file system volumes appropriate for the current OS
#'
#' @return Named list of volume paths
#'
#' @keywords internal
#' 
get_default_volumes <- function() {
  # Base volumes available on all systems
  volumes <- c(
    "Home" = path.expand("~"),
    "Temp" = tempdir(),
    "Working Directory" = getwd()
  )
  
  # Add system-specific volumes
  if (Sys.info()[['sysname']] == "Windows") {
    # Add Windows drives
    tryCatch({
      windows_volumes <- shinyFiles::getVolumes()()
      volumes <- c(volumes, windows_volumes)
    }, error = function(e) {
      # If getVolumes() fails, just use base volumes
      warning("Could not detect Windows volumes: ", e$message)
    })
  } else {
    # Add Unix-like system root
    volumes <- c(volumes, "Root" = "/")
    
    # Add common Unix directories if they exist
    common_dirs <- c(
      "Documents" = file.path(path.expand("~"), "Documents"),
      "Desktop" = file.path(path.expand("~"), "Desktop"),
      "Downloads" = file.path(path.expand("~"), "Downloads")
    )
    
    # Only add directories that actually exist
    existing_dirs <- common_dirs[sapply(common_dirs, dir.exists)]
    volumes <- c(volumes, existing_dirs)
  }
  
  return(volumes)
}