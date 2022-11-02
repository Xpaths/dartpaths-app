#' Create contact buttons
#' @param text text (character vector of length one)
#' @importFrom fontawesome fa
#' @importFrom htmltools HTML
#' @export
createContactButton <- function(text){
	tags$div(class = "flex-wrapper contact-div", tags$div(class = "flex-item",  
						text),
		tags$div(class = "flex-item",
				tags$div(class = "btn btn-default action-button shiny-bound-input flex-item contact-link", 
						tags$div(class = "flex-wrapper",
								tags$div(class = "flex-item", tags$img(src = "images/Vivaltes_logo_withouttext.svg", height = "20px")),
								HTML(paste0('<div class = "flex-item"> Vivaltes ', fa('chevron-right'), '</div>')),
								onclick = "window.open('https://www.vivaltes.com/dartpaths', '_blank')" 
						))
		)
)}