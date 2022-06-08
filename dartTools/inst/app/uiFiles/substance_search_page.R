substance_search_page <- div(
		
		htmlTemplate("templates/substance-search.html",
										
				substance_search = conditionalPanel(condition="!$('html').hasClass('shiny-busy')", 
													id = "substanceContainer",
								tags$div(class = "page-title", 
										tags$h2("Search results for ",
										textOutput(outputId = "substanceInputValue")
										)),
								uiOutput("substance_helptext"),
								tags$div(dataTableOutput("substance_searchResult"))
								
						),
			
				loading_substance_page = conditionalPanel(condition="(input.substance_searchResult_rows_selected && ($('html').hasClass('shiny-busy')))",
															id = "loadingContainer",
							
						tags$div(class = "content subst-info-content",
						tags$div(class = "col-sm-3", id = "subst-name-loading"),
						tabsetPanel(id = "tabs-loading",
								# B1. Tox Summary
								tabPanel(value = "tab1", title = "Overview", 
										
										tags$div(class = "info-tip-wrapper", 
												tags$div(class = "info-tip", 
														tags$span(class = "tooltiptext", "This table shows the ranking of human pathways according to observed phenotypes that are", 
																"associated with the pathways and in vitro evidence on pathway modulation.", 
																"Go the pathway ranking tab to see the individual scores for mammalian and non-mammalian phenotypes and in vitro hits.", 
																"The values in the table below are the weighted averages of those individual scores."
														
														))),
										
										fluidRow(
												column(3, class = "sidebar-overview-loading",	
														#structure
														tags$div(class = "overview-line"),
														tags$div(class = "info-sidebar-title", "Standardized structure"),
														tags$div(class = "overview-table", 
																tagList(tags$div(class = "no-data-div-loading"))		
														),
														tags$div(class = "overview-line"),
														#CLP
														tags$div(class = "info-sidebar-title", "CLP"),
														tags$div(class = "overview-table", 
																tagList(tags$div(class = "no-data-div-loading"))		
														),
														tags$div(class = "overview-line"),
														#Regulatory studies
														tags$div(class = "info-sidebar-title", style = "margin-bottom:15px;",
																"Regulatory studies"),
														tags$div(class = "overview-table", 
																tagList(tags$div(class = "no-data-div-loading"))
														),	
														tags$div(class = "overview-line"),
														#Mammalian phenotypes
														tags$div(id = "mam-overview", class = "info-sidebar-title", style = "margin-bottom:15px;", 
																"Mammalian phenotypes"),
														tags$div(class = "overview-table", 
																tagList(tags$div(class = "no-data-div-loading"))
														),
														tags$div(class = "overview-line"),
														#Non-mammalian phenotypes
														tags$div(id = "nonmam-overview", class = "info-sidebar-title", style = "margin-bottom:15px;", 
																"Non-mammalian phenotypes"),
														tags$div(class = "overview-table", 
																tagList(tags$div(class = "no-data-div-loading"))
														),
														tags$div(class = "overview-line"),
														#In vitro tests
														tags$div(id = "invitro-overview", class = "info-sidebar-title", style = "margin-bottom:15px;", 
																"In vitro hits"),
														tags$div(class = "overview-table", style = "margin-bottom:15px;",
																tagList(tags$div(class = "no-data-div-loading"))
														),	
												),
												column(5, id = "pathway-overview-loading",
														tags$h2(class = "table-title", "Predicted affected pathways in humans"),
														tags$div(class = "dropdown-filter-wrapper",
																tags$div(class = "select-evidence", selectInput("selectColLoadingSubst", "", c(
																						"Based on mammalian evidence" = "scoreMammalian", 
																						"Based on non-mammalian phenotype evidence" = "scoreNAM", 
																						"Based on in vitro evidence" = "scoreInVitro",
																						"Based on all evidence" = "scoreAll",
																						"Pathways with all types of evidence" = "scoreAllWithoutZero" 
																				), 
																				selected = "scoreAll", multiple = FALSE)),
																tags$div(class = "filter-wrapper",
																		tags$button(
																				id = "recalculate_btn_subst_loading", 
																				"Filter and recalculate",
																				class = "btn btn-default action-button shiny-bound-input",
																				type = "button",
																				tags$img(src="images/filtericon.svg")
																		
																		))),
														tags$div(class = "loading-wrapper-pathway",
																(tags$div("Loading. Please wait...", class="load-div-msg-pathway")),	
																tags$div(class="load-div-1-pathway"),
																tags$div(class="load-div-2-pathway"),
																tags$div(class="load-div-3-pathway"),
																tags$div(class="load-div-4-pathway"),
																tags$div(class="load-div-5-pathway")
														)
												
												),
												
												column(4, id = "pathway-details-loading",
														tags$div(tags$h2(class = "table-title", "Details and evidence for selected pathway")),
														tabsetPanel(id = "pathway-details-tabs-loading",
																tabPanel(title = "Overview",
																		tags$div(class = "details-overview-container-loading",																		
																		)
																),
																tabPanel(title = "Phenotypes"
																),
																tabPanel(title = "In vitro"
																),
																tabPanel(title = "Downstream pathways"
																)
														)
												)
										)
								),
								
								# pathway ranking
								tabPanel(value = "tab2", title = "Pathway ranking"
								),
								tabPanel(value = "tab3", title = "Regulatory studies"
								),
								tabPanel(value = "tab4", title = "In vitro"
								),
								tabPanel(value = "tab5", title = "Phenotypes"
								),
								tabPanel(value = "tab6", title = "Similar substances")
								)
								)
						
						) # end of conditional panel
 

		))