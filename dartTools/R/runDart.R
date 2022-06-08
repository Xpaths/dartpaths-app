#' Run the UI for DART
#' @param port integer, port to run app on
#' @param ... further arguments that can be passed to \code{\link[shiny]{runApp}}
#' @return no return value
#' @importFrom shiny runApp
#' @importFrom devtools dev_package_deps install_github
#' @importFrom stats update
#' @export
runDart <- function(port = 3838, ...) {
  
  ldots <- list(...)
  
  if (!is.null(ldots$appDir))
    runApp(...) else
    runApp(appDir = system.file("app", package = "dartTools"), port = port, ...)
  
}
