about_page <- div(
    
    htmlTemplate("templates/about.html",
        
        panel_info = tabPanel("Info", 
            
            tags$div(class = "container", 
                
                ## Introduction
                
                actionLink(inputId = "info_showIntro", class = "info-title", label = tags$h2("Welcome to DARTpaths")),
                
                conditionalPanel("input.info_showIntro % 2 == 0",
                    
                    tags$p(style = "text-align:center", tags$img(src = "images/dartpaths_overview.svg", id = "dart-intro-img", width="700")),
                    
                    # Compounds
                    tags$p("This application collects and combines online information from different data sources to allow DART hazard prediction of chemical substances.",
                        "The app allows to search substances or substance (UVCB) categories and get an overview of available data such as activity in ", tags$i("in vitro"), " assays, observed phenotypes and effect levels from regulatory studies.",
                        "This data is used to rank the biological pathways that may be affected by the substance.", 
                        "The rank order reflects associations between pathways and observed phenotypes as well as observed pathway modulation in ", tags$i("in vitro"), " assays.",
                        "Also the activity pattern of a substance can be compared with that of similar substances (based on molecular fingerprint similarity or membership of a UVCB category)."
                    ),
                    
                    # Orthologous pathways
                    tags$p("DARTpaths also allows decision making about which test system(s) are most suitable when known DART pathways are disrupted.", 
                        "The app allows to search pathways and maps evolutionary conservation of those pathways across mammalian test systems such as mouse, rat and rabbit and 3R-compliant non-mammalian species such as slime mold (D. discoideum), nematode (C. elegans) and zebrafish.", 
                        "A fine-grained orthology mapping has been added to increase the reliability of the orthology predictions."),
                
				createContactButton("Go to Vivaltes for wet-lab testing to validate DARTpaths predictions or to ask other questions")),
                
                
                ## Contributors 
                
                actionLink(inputId = "info_showContributors", class = "info-title", label = tags$h2("Contributors")),
                
                conditionalPanel("input.info_showContributors % 2 == 0",
                    
                    tags$div(class = "info-logo-wrapper", 
                        tags$a(href = "https://www.vivaltes.com", target="_blank",
                            tags$img(src = "images/Vivaltes_logo.svg", height = "120px")),
                        tags$a(href = "https://www.openanalytics.eu", target="_blank", 
                            tags$img(src = "images/oa.png", height = "100px")),
                        tags$a(href = "https://www.biw.kuleuven.be/m2s/cmpg/research/CSB", target="_blank",
                            tags$img(src = "images/KU_Leuven_logo.svg", height = "100px")),
                        tags$a(href = "https://www.hu.nl/", target="_blank",
                            tags$img(src = "images/hu-logo-international.svg", height = "100px")),
                        tags$a(href = "https://www.cleverfranke.com/", target="_blank",
                            tags$img(src = "images/cleverfranke.png", height = "100px"))
                    )
                ),
                
                
                ## Sponsors
                
                actionLink(inputId = "info_showSponsors", class = "info-title", label = tags$h2("Sponsors")),
                
                conditionalPanel("input.info_showSponsors % 2 == 0",
                    
                    tags$div(class = "info-logo-wrapper",
                        tags$a(href = "https://nc3rs.org.uk/", target="_blank", 
                            tags$img(src = "images/nc3rs.jpg", height = "100px")),
                        tags$a(href = "https://www.shell.com/", target="_blank", 
                            tags$img(src = "images/shell.png", height = "100px")),
                        tags$a(href = "https://www.syngenta.com/", target="_blank", 
                            tags$img(src = "images/syngenta.png", height = "100px"))
                    )
                
                ),
                
                
                ## External data sources
                
                actionLink(inputId = "info_showSources", class = "info-title", label = tags$h2("External Data Sources")),
                
                conditionalPanel("input.info_showSources % 2 == 0", 
                    
                    "This tool makes use of and/or links to the following databases",
                    
                    tags$ul(
                        
                        tags$li(tags$a("Dictybase", target="_blank",
                                href = "http://dictybase.org/")),
                        tags$li(tags$a("European Chemicals Agency", target="_blank",
                                href = "http://echa.europa.eu/disclaimer_en.asp#registration")),
                        tags$li(tags$a("Ensembl", target="_blank",
                                href = "http://www.ensembl.org")),
                        tags$li(tags$a("EPA CompTox", target="_blank",
                                href = "https://comptox.epa.gov")),
                        tags$li(tags$a("FlyBase", target="_blank",
                                href = "https://flybase.org")),
                        tags$li(tags$a("International mouse phenotyping consortium", target="_blank",
                                href = "http://www.mousephenotype.org/")),
                        tags$li(tags$a("MGI", target="_blank",
                                href = "http://www.informatics.jax.org/")),
                        tags$li(tags$a("OBO", target="_blank",
                                href = "http://obofoundry.org/")),
                        tags$li(tags$a("OECD QSAR Toolbox", target="_blank",
                                href = "https://qsartoolbox.org/")),                        
                        tags$li(tags$a("Reactome", target="_blank",
                                href = "https://reactome.org/")),
                        tags$li(tags$a("WormBase", target="_blank",
                                href = "https://wormbase.org")),
                        tags$li(tags$a("The Zebrafish information network", target="_blank",
                                href = "http://zfin.org/"))
                    )
                
                ),
                
                ## Session Info
                
                tags$div(style = "margin-bottom:50px", 
                    actionLink(inputId = "info_showSession", class = "info-title", label = tags$h2("Software Used")),
					
					
                    conditionalPanel("input.info_showSession % 2 == 1", 
							
							
                        tags$div(uiOutput("info_session")))
                )
				
            )
        
        )
    
    ))