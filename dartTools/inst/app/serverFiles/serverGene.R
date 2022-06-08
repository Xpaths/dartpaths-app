# Project: DARTpaths_git
# 
# Author: mvarewyck
###############################################################################


## ------------ ##
## 1. Gene Data ##
## ------------ ##

# Select pathway on pathway page
output$gene_pathwayOut <- renderUI({
      
      choices <- database$getHumanPathways(
          levels = getOption("dartpaths_app_pathway_orthology_levels"),
          sizePathwayMin = getOption("dartpaths_app_pathway_orthology_min_genes"),
          sizePathwayMax  = getOption("dartpaths_app_pathway_orthology_max_genes"),
          asVector = TRUE, renameEvent = TRUE,
          lowestLevelOnly = getOption("dartpaths_app_pathway_orthology_lowest_level")
      )
      
      selectizeInput(inputId = "gene_pathway",
          label = NULL,
          choices = choices,
          width = '100%',
          options = list(maxOptions = length(choices) + 1))
    })

# Select pathway on landing page
output$gene_pathwayOut_2 <- renderUI({
      
      choices <- database$getHumanPathways(
          levels = getOption("dartpaths_app_pathway_orthology_levels"),
          sizePathwayMin = getOption("dartpaths_app_pathway_orthology_min_genes"),
          sizePathwayMax  = getOption("dartpaths_app_pathway_orthology_max_genes"),
          asVector = TRUE, renameEvent = TRUE,
          lowestLevelOnly = getOption("dartpaths_app_pathway_orthology_lowest_level")
      )
      selectizeInput(inputId = "gene_pathway_2", label = NULL,
          choices = c("e.g. Abacavir transport and metabolism" = "", choices),
          width = '100%',
          options = list(maxOptions = length(choices) + 1)) 
    })


#update the input value of the selectize on the pathway result page based on the input value in the selectize of the landing page
observe({
      x <- input$gene_pathway_2
      if (is.null(x)){
        x <- character(0)
      }
      updateSelectizeInput(session, "gene_pathway",
          selected = x)
      
    })


#update the input value of the selectize on the pathway result page based on the selected pathway on info page

observe({
      
      selectedRowIndex <-  input$substance_pathways_summary_rows_selected
      
      if(length(selectedRowIndex)){
        reactomeID <- substancePathwaysSorted()[selectedRowIndex, reactome_pathway_stable_identifier]

        updateSelectizeInput(session, "gene_pathway", label = NULL,
            selected = reactomeID)
        
      }
      
      
    })

#switch to pathway result page when the pathway in clicked on the right side of the info page
observeEvent(input$pathway_link, {
      
      change_page("pathway-search")
      
    })




# Load gene data
results$gene_data <- reactive({
      validate(need(input$gene_pathway, "Please select a pathway"))
      database$getPathwayOrthology(pathwayid = input$gene_pathway, renameEvent = TRUE,
          levels = getOption("dartpaths_app_pathway_orthology_levels"),
          sizePathwayMin = getOption("dartpaths_app_pathway_orthology_min_genes"),
          sizePathwayMax  = getOption("dartpaths_app_pathway_orthology_max_genes"),
          lowestLevelOnly = getOption("dartpaths_app_pathway_orthology_lowest_level")
      )
    })

# Reactome link for pathway data

#moved to pathway-results page
output$gene_reactomeButton <- renderUI({
      tags$button("View pathway on reactome.org",		
          id = "gene_showReactome", 
          class = "btn btn-default action-button shiny-bound-input",
          style = "width:100%",
          onclick = paste0("window.open('", attr(results$gene_data(), "reactomeLink"),
              "', '_blank')")
      )
      
    })

# button to trigger pathway search results

output$pathway_searchResult <- renderUI({
      tags$button("Explore",
          id = "pathway_explore", 
          class = "btn btn-default action-button shiny-bound-input",
          style = "width:100%",
          tags$img(src="images/arrowRight.svg")
      )
    })


# Show gene data
output$gene_data <- renderUI({
      
      tagList(
          tags$div(helpText("Clicking on the orthology relation results in a pop-up with 
                      detailed information on the orthologous gene(s) in the model organism.")),
          
          renderDataTable(
              interactiveGeneTable(geneData = results$gene_data(),
                  columnNames = NULL,
                  colors = geneColors,
                  opts = list(
                      fixedHeader = TRUE,
                      dom = '<"top">rt<"bottom"ilp><"clear">')
              )
          )
      )
    })

# Create links to orthology data for all buttons in table
observeEvent(input$gene_dataClick, {
      
      # Id is gene_species_row
      myId <- strsplit(input$gene_dataId, split = "_")[[1]]
      mySpecies <- myId[2]
      # map common names to full scientific names
      speciesTable <- database$getData("species")
      mySpeciesfull <- speciesTable[speciescommon == mySpecies, speciesfull]
      
      row <- as.numeric(myId[3])
      myGene <- results$gene_data()[row, ]
      orthology <- database$getData("orthology")
      matchData <- orthology[speciesfull == mySpeciesfull & geneid == myGene$geneid]
      matchData[, protein_or_transcriptid:= NULL]
      
      matchData <- matchData[, homologytype := factor(homologytype, levels = c("0", "ortholog_one2one", "ortholog_one2many", "ortholog_many2many"), labels = c("0", "1:1", "1:N", "N:N"))]
      matchData <- merge(matchData, database$tables$humangenes[,.(geneid, gene)], by = "geneid")	
      setcolorder(matchData, c("geneid", "gene"))
      matchData <- matchData[, !"homolog_protein_or_transcriptid"]
      
      
      showModal(modalDialogWithCross(
              title = "Orthology Data",
              fluidPage(id = "orthology_table",
                  renderDataTable(
                      datatable(
                          unique(matchData), 
                          colnames = c("Gene id", "Gene", "Homology Type", "Species", "Homolog gene id", "Homolog gene", "Percent identical", "Confidence"),
                          options = list(
                              dom = 't',
                              paging = FALSE,
                              pageLength = "All"
                          )
                      )
                  )
              ),
              addCross = TRUE,
              easyClose = TRUE,
              size = "l",
              footer = tags$div(class = "custom-modal-footer", modalButton("Close"))))
    })			





## ---------- ##
## 2. Summary ##
## ---------- ##


# Summary table
results$gene_summaryTable <- reactive({
      
      summarizeGeneData(geneData = results$gene_data())
      
      
    })

output$gene_summaryTable <- renderTable({
      
      results$gene_summaryTable()
      
    })

# Summary plot
output$gene_barPlot <- renderPlotly({
      
      plotGeneData(tableGenes = results$gene_summaryTable(),
          colors = geneColors)
      
    })

output$gene_barPlot_overview <- renderPlotly({
      
      plotGeneData(tableGenes = results$gene_summaryTable(),
          colors = geneColors)
      
    })

## ------------------------ ##
## 3. Associated phenotypes ##
## ------------------------ ##


results$pathway_phenotypes <- reactive({
      
      if(is.null(input$gene_pathway)) return(NULL)
      
      database$getData("pathwayphenotypes")[reactome_pathway_stable_identifier == input$gene_pathway]
      
    })

output$pathway_phenotypes <- renderDataTable({
      
	  validate(
			  need(!is.null(results$pathway_phenotypes()) && nrow(results$pathway_phenotypes() != 0), 'No data available'))
      
      # monkey patch to ensure consistent species names on pathway page 
			results$pathway_phenotypes()[species == "celegans", species:="nematode"]
			results$pathway_phenotypes()[species == "slimemold", species:="slime mold"]
      results$pathway_phenotypes()[species == "dmelanogaster", species:="fruit fly"]
      results$pathway_phenotypes()[species == "drerio", species:="zebrafish"]
      
      # hide p-values (only show adjusted p-values)
      results$pathway_phenotypes()[,pvalue:=NULL]
      
      uiTable(results$pathway_phenotypes(), interactive = TRUE, scrollX = FALSE, filter = "top",
          options = list(
              dom = '<"top">t<"bottom"lp><"clear">'))
    })

