## router ##
## ------ ##


server <- function(input, output, session) {
  router$server(input, output, session)
  
# enable search button once a selection is made for a substance
  

  observeEvent(input$substance_name, {
	
		mySearch <- input$substance_name 
		shinyjs::toggleState("substance_search", mySearch !="")
		
	})	

observeEvent(input$substance_cas1, {
			
			mySearch <- input$substance_cas1 
			shinyjs::toggleState("substance_search", mySearch !="")
			
		})	

observeEvent(input$substance_ec1, {
			
			mySearch <- input$substance_ec1 
			shinyjs::toggleState("substance_search", mySearch !="")
			
		})	


observeEvent(input$substance_smiles, {
			
			mySearch <- input$substance_smiles 
			shinyjs::toggleState("substance_search", mySearch !="")
			
		})	

  
# enable search button once the selection is made in search for UVCB  
	
	  observeEvent(input$category_selection_members, {
			    			  
			  shinyjs::toggleState("category_search_members", input$category_selection_members != "")
					  
		  }) 
  
  
  observeEvent(input$category_selection_constituents, {
			  
			  shinyjs::toggleState("category_search_constituents", input$category_selection_constituents != "")
			  
		  }) 
  
  # enable pathway search button once the selection is made
   
  observeEvent(input$gene_pathway_2, {
			  
			  shinyjs::toggleState("pathway_explore", input$gene_pathway_2 != "")
			  
		  }) 
  
  
  # change page when the button is clicked
	
	
	observeEvent(input$category_search_members, {
				change_page("category-search")
			})
	
	observeEvent(input$category_search_constituents, {
				change_page("category-search")
			})


  
  observeEvent(input$substance_search, {
        change_page("substance-search", session = shiny::getDefaultReactiveDomain(),
				mode = "push")
      })
  
  observeEvent(input$pathway_explore, {
			  
			# first remove back to substance button  
		removeUI(
			# pass in id of element to remove
		selector = "#back_button_pathway"	
			  )
			  
        change_page("pathway-search")
      })


## Substance results page: change page when the row is clicked 


observeEvent(input$substance_searchResult_rows_selected, { 
			
			change_page("substance-information")
		}, priority = 50)	



observeEvent(input$substance_searchResult_rows_selected, {
			
			info <- input$substance_searchResult_rows_selected
			#   if (is.null(info$value) || info$col != 0) return()
			
			results$selected_substance <- results$substance_set_single()
			#cat("priorityField :", attr(results$selected_substance,"priorityField"),"\n")
			#cat("priorityPattern :", attr(results$selected_substance,"priorityPattern"),"\n")
		
		})	



## Category results page: change page when the row is clicked 

observeEvent(input$category_searchResult_rows_selected, { 
			
			info <- input$category_searchResult_rows_selected
			# if (is.null(info$value) || info$col != 0) return()
			
			results$selected_substance <- results$category_filter_single()
			#cat("priorityField :", attr(results$selected_substance,"priorityField"),"\n")
			#cat("priorityPattern :", attr(results$selected_substance,"priorityPattern"),"\n")	
			
			change_page("substance-information")	
		})


## capture input value of substance search ##
## --------------------------------------- ##


rv <- reactiveVal(NULL)

observeEvent(input$substance_search, {
			
			switch(input$substance_type,
					Name = {
						mySearch <- input$substance_name 
						rv(mySearch)
					},
					CAS = {
						mySearch <- paste0(input$substance_cas1, "-", input$substance_cas2, "-",
								input$substance_cas3) 
						rv(mySearch)
					},
					EC = {
						mySearch <-  paste0(input$substance_ec1, "-", input$substance_ec2, "-",
								input$substance_ec3)
						rv(mySearch)
					},
					SMILES = {
						mySearch <- input$substance_smiles
						rv(mySearch)
					})
		})

output$substanceInputValue <- renderText({rv()})



## capture input value of category search ##
## -------------------------------------- ##


rv <- reactiveVal(NULL)

observeEvent(list(input$category_search_constituents, input$category_search_members), {
			
			switch(input$category_search_type,
					category_constituents = {
						mySearch <- input$category_selection_constituents 
						rv(mySearch)
					},
					category_members = {
						mySearch <- input$category_selection_members
						rv(mySearch)
					})
		})

output$categoryInputValue <- renderText({rv()})




## insert back buttons ##
## ------------------- ##

## insert back button for substance search
	
	observeEvent(input$substance_searchResult_rows_selected, {
				
				info = input$substance_searchResult_rows_selected
				#if (is.null(info$value) || info$col != 0) return()
				
				removeUI(
						## pass in id of element to remove
						selector = "#back_button_subst"
				)
				
				removeUI(
						## pass in id of element to remove
						selector = "#back_button_cat"
				)
				
				insertUI(
						## pass in id of element where you want to insert the ui
						selector = '#backpage',
						ui = tags$div(
								actionButton(inputId = "back_button_subst", label = tags$span(HTML("&larr;", " Back to search results")))
						)
				)
			})
	
	
	observeEvent(input$back_button_subst, { 
				
				change_page("substance-search")				
			})
	

		
## insert back button for category search
	
	observeEvent(input$category_searchResult_rows_selected, {
				
				info = input$category_searchResult_rows_selected
				#if (is.null(info$value) || info$col != 0) return()
				
				removeUI(
						# pass in id of element to remove
						selector = "#back_button_subst"
				)
				
				removeUI(
						# pass in id of element to remove
						selector = "#back_button_cat"
				)
				
				insertUI(
						# pass in id of element where you want to insert the ui
						selector = '#backpage',
						ui = tags$div(
								actionButton(inputId = "back_button_cat", label = tags$span(HTML("&larr;", " Back to search results")))
						)
				)
			})
	
	observeEvent(input$back_button_cat, { 
				change_page("category-search")			
			})
	
	
## insert back to substance results button on pathway page
	
	observeEvent(input$pathway_link, {
				
			removeUI(
				# pass in id of element to remove
				selector = "#back_button_pathway"	
			)
			
			insertUI(
				# pass in id of element where you want to insert the ui
				selector = "#backtoinfo",
				ui = tags$div(
						actionButton(inputId = "back_button_pathway", label = tags$span(HTML("&larr;", " Back to substance results")))
						)
					)
				
			})
	
	observeEvent(input$back_button_pathway, { 
				change_page("substance-information")				
			})

# trigger pathway ranking when the user clicks on recalculate button
	
	observeEvent(input$recalculatePathwaysButton, {
				click("calculate_pathway")
			})
	

# reactive value for the back button of the pathway results 
	
	rvBackButton <- reactiveValues(clicked = NULL)
	
	observe({
				rvBackButton$clicked <- input$back_button_pathway
			})
	# reset button value to null when a new substance is selected
	observeEvent(input$substance_searchResult_rows_selected, {
				rvBackButton$clicked <- NULL
			})	
	
	observeEvent(input$category_searchResult_rows_selected, {
				rvBackButton$clicked <- NULL
			})	
	
# trigger pathway ranking after page change to substance information but not coming from the pathway results page	
	
	observe({
				infoPage = get_page(session = shiny::getDefaultReactiveDomain())
				if (is.null(rvBackButton$clicked) && infoPage == "substance-information") {
					click("calculate_pathway")
				}
			})
	

## reset Overview as selected tab ##
## ------------------------------ ##

observeEvent(input$substance_searchResult_rows_selected,
    updateTabsetPanel(session = session, inputId = "tabs", selected = "tab1")
)

observeEvent(input$category_searchResult_rows_selected,
		updateTabsetPanel(session = session, inputId = "tabs", selected = "tab1")
)

# update tabset panel of pathway details

observeEvent(input$substance_searchResult_rows_selected,
		updateTabsetPanel(session = session, inputId = "pathway-details-tabs", selected = "tab1")
)

observeEvent(input$category_searchResult_rows_selected,
		updateTabsetPanel(session = session, inputId = "pathway-details-tabs", selected = "tab1")
)


 
## Filter and recalculate pathway ranking ##
## -------------------------------------- ##
  
## show pathway ranking filter modal for substances 
  
  observeEvent(input$recalculate_btn_subst, {
        showModal(modalDialogWithCross(id = "modalFilter",
                title = "Select which phenotypes and in vitro assays you would like to take into account when predicting pathways",
                addCross = TRUE,
                easyClose = TRUE,
                size = "l",
                footer = tags$div(id = "footerFilter", class = "custom-modal-footer",
                    tags$div(
                        modalButton("Cancel"),
                        actionButton(inputId = "recalculatePathwaysButton", label = "Recalculate pathways")
                    )
                ),
                tabsetPanel(tabPanel(title = "Mammalian phenotypes",
                        tags$div(class = "filter-table", dataTableOutput("mammalianPhenotypes")),
						tags$div(class = "table-btn-wrapper", 
								tags$div(class = "filter-btn", actionButton("DeselectAllMam", label = "Deselect All")),
								tags$div(class = "filter-btn", actionButton("SelectAllMam", label = "Select All"))),
						tags$div(style = "margin-bottom: 40px;")),
                    tabPanel(title = "Non-mammalian phenotypes",
                        tags$div(class = "filter-table", dataTableOutput("nonMammalianPhenotypes")),
						tags$div(class = "table-btn-wrapper", 
								tags$div(class = "filter-btn", actionButton("DeselectAllNonMam", label = "Deselect All")),
								tags$div(class = "filter-btn", actionButton("SelectAllNonMam", label = "Select All"))),
						tags$div(style = "margin-bottom: 40px;")),
                    tabPanel(title = "In vitro assays",
                        tags$div(class = "filter-table", dataTableOutput("allInVitro")),
						tags$div(class = "table-btn-wrapper", 
								tags$div(class = "filter-btn", actionButton("DeselectAllInVitro", label = "Deselect All")),
								tags$div(class = "filter-btn", actionButton("SelectAllInVitro", label = "Select All"))),
						tags$div(style = "margin-bottom: 40px;"))
                
                )))
      })
  
 
  ## recalculate pathway ranking when recalculate button is clicked 
  
  observeEvent(input$recalculatePathwaysButton, removeModal())



## Close modal dialog with cross at right top, created by modalDialogWithCross()
  
observeEvent(input$`modal-cross`, {
        removeModal()
      })
 

## Advanced debugging ##
## ------------------ ##
  
  observe({
        
        if (is.null(input$debug_console))
          return(NULL)
        
        if (input$debug_console > 0) {
          
          options(browserNLdisabled = TRUE)
          saved_console <- ".RDuetConsole"
          if (file.exists(saved_console)) {load(saved_console)}
          isolate(browser())
          save(file = saved_console, list = ls(environment()))
          
        }
        
      })
  
  
  output$debug_print <- renderPrint({
        
      })
  
  
  
  output$debug <- renderUI({
        
        if (!doDebug)
          return(NULL)
        
        tagList(
            actionLink(inputId = "debug_console", label = "Connect with console",
                icon = icon("exchange")),
            verbatimTextOutput("debug_print")
        )
        
      })
  
  
## for eventReactive in substance query (cf. serverSubstance.R) ##
## ------------------------------------------------------------ ##
  
  results <- reactiveValues(
  
  )
  
  
## Source files ##
## ------------ ##
  
  source(file = file.path("serverFiles", "serverInfo.R"), local = TRUE)
  source(file = file.path("serverFiles", "serverSubstance.R"), local = TRUE)
  source(file = file.path("serverFiles", "serverCategory.R"), local = TRUE)
  source(file = file.path("serverFiles", "serverGene.R"), local = TRUE)
}
