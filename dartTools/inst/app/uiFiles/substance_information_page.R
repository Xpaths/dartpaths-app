substance_information_page <- div(
    
    htmlTemplate("templates/substance-information.html",
        ## B. Toxicity tabs
        
        substance_name = tags$div(class = "col-sm-3", id = "subst-name", dataTableOutput("substance_name")),
        
        substance_information =  tabsetPanel(id = "tabs",
            # B1. Tox Summary
            tabPanel(value = "tab1", title = "Overview", 
                
                tags$div(id = "overview-tip-wrapper", class = "info-tip-wrapper", 
                    tags$div(class = "info-tip", 
                        tags$span(class = "tooltiptext",
                            paste("The central table shows the ranking of human pathways according to observed phenotypes that are", 
                                "associated with the pathways and in vitro evidence on pathway modulation."),
                            "Click on pathways in this table to get more details (shown on the right) on the selected pathway and the available evidence for the role of this pathway.",
                            "The central table only shows pathways at certain levels of the Reactome hierarchy. The scores for the underlying pathways are shown on the right in the 'Downstream pathways' tab.",
                            #"Go the pathway ranking tab to get more details on the scores and the contribution of mammalian and non-mammalian phenotypes and in vitro hits to this score.",
                            paste("Above the central table is a drop-down menu to select the type of included evidence",
                                "(default: 'Based on all evidence') and a button 'Filter and recalculate'",
                                "which allows to select the the individual data records used for the ranked pathways."),
                            paste("The values in the central table show a Jaccard index (sorted in decreasing order) if the option 'Based on in vitro evidence' is selected or if 'Based on all evidence' is selected and there is only in vitro data for the selected substance.",
                                "In all other cases, an FDR-adjusted p-value (sorted in increasing order) is shown.")
                        ))),
                
                fluidRow(
                    column(3, id = "sidebar-overview",	
                        #structure
                        tags$div(class = "overview-line"),
                        tags$div(class = "info-sidebar-title", "Standardized structure"),
                        tags$div(class = "overview-table", 
                            dataTableOutput("substance_summary_structure")
                        ),
                        tags$div(class = "overview-line"),
                        #CLP
                        tags$div(class = "info-sidebar-title", "CLP"),
                        tags$div(class = "overview-table clp-div", 
                            dataTableOutput("substance_summary1_CLP")
                        ),
                        tags$div(class = "overview-line"),
                        #Regulatory conclusions
                        tags$div(class = "info-sidebar-title", style = "margin-bottom:15px;",
                            "Regulatory studies"),
                        tags$div(class = "overview-table",
                            uiOutput("substance_summary1_invivo_count")
                        ),	
                        tags$div(class = "overview-table invivo-count-div", 
                            dataTableOutput("substance_summary1_invivo")
                        ),	
                        tags$div(class = "overview-line"),
                        #Mammalian phenotypes
                        tags$div(id = "mam-overview", class = "info-sidebar-title", style = "margin-bottom:15px;", 
                            "Mammalian phenotypes"),
                        tags$div(class = "overview-table", 
                            uiOutput("substance_summary1_phenotypes_mammalian")
                        ),
                        tags$div(class = "overview-line"),
                        #Non-mammalian phenotypes
                        tags$div(id = "nonmam-overview", class = "info-sidebar-title", style = "margin-bottom:15px;", 
                            "Non-mammalian phenotypes"),
                        tags$div(class = "overview-table", 
                            uiOutput("substance_summary1_phenotypes_nonmammalian")
                        ),
                        tags$div(class = "overview-line"),
                        #In vitro tests
                        tags$div(id = "invitro-overview", class = "info-sidebar-title", style = "margin-bottom:15px;", 
                            "In vitro hits"),
                        tags$div(class = "overview-table", style = "margin-bottom:15px;",
                            uiOutput("substance_summary1_invitro_count")
                        ),	
                        tags$div(class = "overview-table", 
                            dataTableOutput("substance_summary1_invitro_names")
                        )
                    ),
                    column(5, id = "pathway-overview",
                        tags$h2(class = "table-title", "Predicted affected pathways in humans"),
                        tags$div(class = "dropdown-filter-wrapper",
                            tags$div(class = "select-evidence", selectInput("selectCol", "", c(
                                        "Based on mammalian evidence" = "scoreMammalian", 
                                        "Based on non-mammalian phenotype evidence" = "scoreNAM", 
                                        "Based on in vitro evidence" = "scoreInVitro",
                                        "Based on all evidence" = "scoreAll",
                                        "Pathways with all types of evidence" = "scoreAllWithoutZero" 
                                    ), 
                                    selected = "scoreAll", multiple = FALSE)),
                            tags$div(class = "filter-wrapper",
                                tags$button(
                                    id = "recalculate_btn_subst", 
                                    "Filter and recalculate",
                                    class = "btn btn-default action-button shiny-bound-input",
                                    type = "button",
                                    tags$img(src="images/filtericon.svg")
                                
                                ))),
                        actionButton('calculate_pathway', label = NULL),
                        dataTableOutput("substance_pathways_summary") %>% withSpinner(type = 5, color="#488df4", hide.ui = TRUE)
                    
                    ),
                    
                    column(4, id = "pathway-details",
                        tags$div(tags$h2(class = "table-title", "Details and evidence for selected pathway")),
                        tabsetPanel(id = "pathway-details-tabs",
                            tabPanel(value = "tab1", title = "Overview",
                                tags$div(class = "details-overview-container",
                                    tags$div(uiOutput("pathwayNameSelectedOverview")),
                                    tags$div(uiOutput("reactomelinkOverview")),
                                    tags$div(id = "score-wrapper",
                                        tags$div(uiOutput("summaryScoreOverview")),
                                        tags$div(uiOutput("nGenesPathwayOverview"))
                                    ),
                                    tags$div(uiOutput("plotOverview")),
                                    tags$div(uiOutput("MammalianPhenotypesOverview")),
                                    tags$div(uiOutput("NAMPhenotypesOverview")),
                                    tags$div(uiOutput("InvitroDetailsOverview"))
                                )
                            ),
                            tabPanel(value = "tab2", title = "Phenotypes",
                                tags$div(uiOutput("phenotypes_details"))),
                            tabPanel(value = "tab3", title = "In vitro",
                                tags$div(uiOutput("invitro_details"))
                            ),
                            tabPanel(value = "tab4", title = "Downstream pathways",
                                tags$div(uiOutput("ranking_indiv_pathways"))
                            )
                        
                        
                        )
                    )
                )
            ),
            
            # pathway ranking
            tabPanel(value = "tab2", title = "Pathway ranking",
                tags$div(class = "info-tip-wrapper", 
                    tags$div(class = "info-tip", 
                        tags$span(class = "tooltiptext", "This table shows the score (Score all) according to which pathways are ranked for the selected substance. This score is the weighted average of the individual scores for mammalian and non-mammalian phenotype-based pathway ranking and ranking based on vitro hits.", 
                        ))),
                tags$div(class = "information-table", style = "margin-bottom:50px", 
                    dataTableOutput("substance_pathways") 
                )
            ),
            
            tabPanel(value = "tab3", title = "Regulatory studies", 
                
                tags$div(class = "info-tip-wrapper", 
                    tags$div(class = "info-tip", 
                        tags$span(class = "tooltiptext", "This table shows the effect levels (endpoint conclusions) for the selected substance from studies following OECD test guidelines for chemicals."
                        ))),
                
                tags$div(class = "information-table", style = "margin-bottom:50px", 
                    dataTableOutput("substance_mammalian")
                )
            ),
            tabPanel(value = "tab4", title = "In vitro", 
                
                tags$div(class = "info-tip-wrapper", 
                    tags$div(class = "info-tip", 
                        tags$span(class = "tooltiptext", "This table shows available in vitro assay data for the selected substance. If the measured activity is considered a hit, the corresponding AC50 value is listed."
                        ))),
                
                tags$div(style = "margin-top:20px; margin-bottom:20px",
                    helpText("")
                ),
                tags$div(class = "information-table", style = "margin-bottom:50px", 
                    dataTableOutput("substance_invitro") 
                )
            ),
            tabPanel(value = "tab5", title = "Phenotypes", 
                tags$div(class = "info-tip-wrapper", 
                    tags$div(class = "info-tip", 
                        tags$span(class = "tooltiptext", "This table shows the observed phenotypes for mammalian and non-mammalian species exposed to the selected substance."
                        ))),										
                tags$div(style = "margin-top:20px; margin-bottom:20px",
                    helpText("")
                ),
                tags$div(class = "information-table", style = "margin-bottom:50px", 
                    dataTableOutput("substance_phenotypes")
                )
            ),
            tabPanel(value = "tab6", title = "Similar substances",
                
                tags$div(style = "margin-top:20px; margin-bottom:20px"
                ),
                tags$div(id = "similar-tip-wrapper", class = "info-tip-wrapper", 
                    tags$div(class = "info-tip",
                        tags$span(class = "tooltiptext",
                            "This table shows an overview of the available data for the selected substance and similar substances.",
                            paste("Similar substances are searched by calculating similarity scores between the constituents",
                                "of the selected substance and the constituents of each substance in the database.", 
                                "Only the maximum similarity for each substance is kept.", 
                                "The listed similarity score is the Tanimoto similarity (a.k.a. Jaccard index) between the Morgan fingerprints ",
                                "(radius 3, bitvector size 1024) of the two molecular structures. Only Tanimoto similarities of 0.3 or higher are listed.", 
                                "In addition, the table also includes substances belonging to the same (UVCB) category.", 
                                "In such a case, no similarity value is shown."),
                            "The column 'Regulatory studies' shows the number of studies with reported effect level. Studies with a LOEL, LOAEL, LOAEC or LOEC value are shown in red/magenta. Studies with a NOAEL, NOEL, NOAEC, NOEC, NOAEDD or NOEDD are shown in green."))),
                
                tags$div(	  
                    tags$div(id = "current-substance-div", style = "margin-bottom:10px", 
                        uiOutput("current_substance") 
                    ),
                    tags$div(style = "margin-bottom:50px", 
                        uiOutput("substance_summary2")
                    )) 
            )
        )
    
    ))