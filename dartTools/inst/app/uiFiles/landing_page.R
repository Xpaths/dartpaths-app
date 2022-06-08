landing_page <- div(
		
		htmlTemplate(
				"templates/landing.html",
				
				## A. Toxicity Query
				
				panel =	wellPanel(class = "search-box", id = "search-box",
						div(class = "title-box-container", div(class = "title-icon"), div(class = "title-box", tags$h2("Search for a chemical"))),	
						#helpText("You can search for substances using various identifiers.", tags$br(), 
						#		"All substances that partially match your CAS, EC or Name query will be shown.", tags$br(),
						#		"For SMILES queries, the structure is standardized and substances with similar constituents are shown."),
						
						tabsetPanel(id = "substanceTabs", type = "tabs",
								tabPanel("Search for a substance",
						
							fluidRow(
								
								column(12,
							
								awesomeRadio(
										inputId = "substance_type",
										label = NULL,
										choices = c("Name", "CAS", "EC", "SMILES"),
										selected = NULL,
										inline = TRUE,
										status = "primary",
										checkbox = FALSE,
										width = NULL),
								helpText("Enter a substance query")
								),

								
								# CAS: https://en.wikipedia.org/wiki/CAS_Registry_Number 
								column(8, 	
										conditionalPanel("input.substance_type == 'CAS'", id = "substance_cas", class = "multiple-form-container",
												div(class = "form-item-with-dash",
														div(class = "form-item",
																# 2 to 7 digits
																textInput(inputId = "substance_cas1", label = NULL,
																		placeholder = "e.g. 100")), 
														div(class = "dash",
																"-")
												),
												# 2 digits
												div(class = "form-item-with-dash",
														div(class = "form-item",		
																textInput(inputId = "substance_cas2", label = NULL,
																		placeholder = "41")),
														div(class = "dash",
																"-")
												),
												# 1 digit
												div(class = "form-item-with-dash",
														div(class = "form-item",		
																textInput(inputId = "substance_cas3", label = NULL,
																		placeholder = "4")))
										
										),
										
								# EC: https://en.wikipedia.org/wiki/European_Community_number
										conditionalPanel("input.substance_type == 'EC'", id = "substance_ec", class = "multiple-form-container",
												div(class = "form-item-with-dash",
														div(class = "form-item",
																# 3 digits 
																textInput(inputId = "substance_ec1", label = NULL,
																		placeholder = "e.g. 200")),
														div(class = "dash",
																"-")
												),
												# 3 digits
												div(class = "form-item-with-dash",
														div(class = "form-item",	
																textInput(inputId = "substance_ec2", label = NULL,
																		placeholder = "278")), 
														div(class = "dash",
																"-")
												),
												# 1 digit
												div(class = "form-item-with-dash",
														div(class = "form-item",
																textInput(inputId = "substance_ec3", label = NULL,
																		placeholder = "5")))
										),
										
										# the new database does no longer distinguish between 
										# IUPAC (https://en.wikipedia.org/wiki/Preferred_IUPAC_name) and other names
										conditionalPanel("input.substance_type == 'Name'",
												# text
												textInput(inputId = "substance_name", label = NULL, placeholder = "e.g. diethylstilbestrol")
												
							
										),
										
										
										# SMILES
										conditionalPanel("input.substance_type == 'SMILES'", class = "multiple-form-container",
												# text
												
												textInput(inputId = "substance_smiles", label = NULL, placeholder = "e.g. OCC(Br)(CO)N(=O)=O")   ,
												
												selectInput(inputId = "substance_smiles_similarity",
														label = NULL,
														choices = c("exact match" = 1, "similarity >= 90%" = 0.9,
                                "similarity >= 70%" = 0.7, "similarity >= 50%" = 0.5,
                                "similarity >= 30%" = 0.3)
														)
													
								
								)),
								column(4, class = "button-search-container",
										
							
									tags$button(
									id = "substance_search", 
									"Search",
									class = "btn btn-default action-button shiny-bound-input",
									type = "button",
									tags$img(src="images/searchIcon.svg")
		
									)
								)	
								
								)
						),
				
				
				## B. UVCB categories
				
				
				tabPanel("Search for UVCB",

							
						fluidRow(
								
								column(12, 
								
										
										awesomeRadio(
												inputId = "category_search_type",
												label = NULL,
												choices = list(`Category UVCBs` = "category_members", `Category constituents` = "category_constituents"),
												selected = NULL,
												inline = TRUE,
												status = "primary",
												checkbox = FALSE,
												width = NULL),
										
								),
								


								conditionalPanel("input.category_search_type == 'category_constituents'",
                  column(12,helpText("Select a category from the dropdown to search for its constituents")),
                  column(8,selectInput(inputId = "category_selection_constituents", label = NULL,
                          choices = c())),	
				  column(4, class = "button-search-container",	
						  
						  
						  tags$button(
								  id = "category_search_constituents", 
								  "Search",
								  class = "btn btn-default action-button shiny-bound-input",
								  type = "button",
								  tags$img(src="images/searchIcon.svg")
						  
						  )
				  )
				  ),
								
								conditionalPanel("input.category_search_type == 'category_members'",
                  column(12,helpText("Select a category from the dropdown to search for its substances")),
                  column(8,selectInput(
                          inputId = "category_selection_members", 
										      label = NULL, 
										      choices = c())),	
								
				column(4, class = "button-search-container",			
								tags$button(
										id = "category_search_members", 
										"Search",
										class = "btn btn-default action-button shiny-bound-input",
										type = "button",
										tags$img(src="images/searchIcon.svg")
								
								)	

								
						)
											
				)
				)))),
			
				
				## C. Pathway
				
				panel2 = wellPanel(id = "explore-box", class = "search-box",
						div(class = "title-box-container", div(class = "title-icon"), div(class = "title-box", tags$h2("Explore a biological pathway"))),
						helpText("Select a biological pathway from the dropdown"),

						
						fluidRow(id = "explore-row",
								column(8, uiOutput("gene_pathwayOut_2")),
								#	column(4, uiOutput("gene_reactomeButton")) ## reactome Button moved to pathway-results page
								column(4, uiOutput("pathway_searchResult"))
						)
				
				)
					
		
				)
)