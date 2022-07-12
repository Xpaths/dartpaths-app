informal_names_explanation <- paste(
		"Tables and figures show informal names for the following species:",
    "R. norvegicus (rat) , M. musculus (mouse), O. cuniculus (rabbit), D. rerio (zebrafish),",
    "D. melanogaster (fruit fly), C. elegans (nematode), D. discoideum (slime mold).")

pathway_search_page <- div(
    
    htmlTemplate("templates/pathway-search.html",
        
        pathway_results_summary = tableOutput("gene_summaryTable"),
        
        
        pathway_reactomeButton = div(
            div(id = "pathway-dropdown-div", uiOutput("gene_pathwayOut")),
            div(id = "reactome-btn-div", uiOutput("gene_reactomeButton"))
        ),
        
        gene_plot = plotlyOutput("gene_barPlot"),
        
        pathway_search = tabsetPanel(id = "pathway-search-tabs",
				tabPanel("Orthology relation", #"Orthologous Pathways", 

            tags$div(class = "info-tip-wrapper", 
                tags$div(class = "info-tip", 
                    tags$span(class = "tooltiptext",
                        paste("The 'Orthology relation' table shows orthologs for human genes in the selected pathway.",
                            "If orthologs are found, the color and text indicates if the human gene has one ortholog (1:1),",
                            "multiple orthologs (1:N) or if multiple human genes are orthologs of multiple genes in the other species (N:N).",
                            "A zero (0) indicates that no orthologs were found.",
                            "A red outline indicates a relation that was discovered using an extended orthology search (dartpaths-phylogeny pipeline).",
                            "All other orthology relations were obtained from Ensembl Compara."),
                        "",
                        informal_names_explanation
                    ))),
            
            tags$div(class = "substance-table", uiOutput("gene_data")),
            tags$script("$(document).on('click', '#gene_data button', function () {
                    Shiny.onInputChange('gene_dataId',this.id);
                    Shiny.onInputChange('gene_dataClick', Math.random())
                    });"),
            # Wider modal dialogs
            tags$head(tags$style(".modal-dialog{width:1500px}")),
        
        ),
        		tabPanel("Associated phenotypes", 
            
                tags$div(class = "info-tip-wrapper", 
                    tags$div(class = "info-tip", 
                        tags$span(class = "tooltiptext", "The 'Associated phenotypes' table lists phenotypes that are associated with modulation of the selected pathway.",
                            "A hypergeometric test was used to assess the significance of phenotypes co-occurring with modulation of genes in the pathway.",
                            "A false discovery rate (FDR) correction was applied to the resulting p-value. Only adjusted p-values below 0.01 (FDR of 1%) were considered significant."
                ))),
createContactButton(HTML('<div class = "overview-helptext"> Go to Vivaltes for wet-lab testing to validate phenotype predictions</div>')),
            tags$div(class = "substance-table", dataTableOutput("pathway_phenotypes"))
			
        )
)
        
    
    
    ))

