# Helper function to create modal dialog with cross to close the window
# 
# Author: mvarewyck
###############################################################################


#' Modal dialog with cross to close the window
#' Extension of the shiny::modalDialog()
#' @param ... ui main content of the dialog
#' @param addCross boolean, whether to add cross button at right top to close the window
#' the id of button is 'modal-cross' 
#' @param title character, title for the dialog
#' @param footer ui, content for footer
#' @param size character, should be one of \code{c("m", "s", "l")}
#' @param easyClose boolean, whether the dialog can be closed by escape button
#' @param fade boolean, when closing the window using fade effect
#' @return modal dialog is being displayed 
#' 
#' @author mvarewyck
#' @importFrom htmltools div
#' @importFrom shiny tags actionButton modalButton
#' @export
modalDialogWithCross <- function(..., addCross = TRUE, title = NULL, 
        footer = modalButton("Dismiss"),
        size = c("m", "s", "l"), easyClose = FALSE, fade = TRUE) {
    
    size <- match.arg(size)
    
    cls <- if (fade) "modal fade" else "modal"
    div(id = "shiny-modal", class = cls, tabindex = "-1",
            `data-backdrop` = if (!easyClose) "static",
            `data-keyboard` = if (!easyClose) "false",
            
            div(
                    class = "modal-dialog",
                    class = switch(size, s = "modal-sm", m = NULL, l = "modal-lg"),
                    div(class = "modal-content",
                            
                            div(class = "modal-header",
                                    if (!is.null(title)) tags$h4(class = "modal-title", title),
                                    if (addCross) actionButton(inputId = "modal-cross", label = "\U2715")
                            ),
                            
                            div(class = "modal-body", ...),
                            
                            if (!is.null(footer)) div(class = "modal-footer", footer)
                    ),
                    tags$script("$('#shiny-modal').modal().focus();")
            )
    )
}


