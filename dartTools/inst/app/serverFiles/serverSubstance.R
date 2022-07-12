# Project: DARTpaths_git
# 
# Author: msteijaert
###############################################################################

nDigitsJaccard <- 3
nDigitsPValues <- 4

pvalueThresholdSummary <- 0.05 # adjusted p-value threshold for shown pathways and colored circles
pvalueThresholdIndividual <- pvalueThresholdSummary # adjusted p-value threshold for individual pathways

## ------------------- ##
## A. Substance Query  ##
## ------------------- ##


# Substance query
results$substance_query <- eventReactive(input$substance_search, {
      
      if (is.null(input$substance_type))
        return(NULL)
      
      
      mySearch <- switch(input$substance_type, 
          CAS = paste0(input$substance_cas1, "-", input$substance_cas2, "-",
              input$substance_cas3),
          EC = paste0(input$substance_ec1, "-", input$substance_ec2, "-",
              input$substance_ec3),
          Name = input$substance_name,
          SMILES = input$substance_smiles
      )
      
      if(nchar(mySearch)==0) return(simpleError("No match found"))
      
      
      # Remove double "--"
      if (input$substance_type %in% c("CAS", "EC"))
        mySearch <- gsub(pattern = "--", replacement = "-", x = mySearch)
      
      type <- if(input$substance_type %in% c("Name")){
            tolower(input$substance_type)
          } else input$substance_type
      
      if (input$substance_type == "SMILES") {
        constituentId <- database$addConstituents(mySearch)
        if(is.na(constituentId)) return(simpleError("No match found"))
        
        constituentSimilarity <- database$getSimilarConstituents(constituentId)
        substanceSimilarity <- database$tables$substanceconstituents[constituentSimilarity, , on = "constituentid"]
        substanceSimilarity <- substanceSimilarity[similarity >= input$substance_smiles_similarity]
        result <- substanceSimilarity[,
            .(similarity = min(similarity)),
            by = substanceid][!is.na(substanceid)][order(-similarity),unique(substanceid)] 
        if (length(result)==0){
          return(simpleError("No match found"))
        }
      } else {
        
        result <- tryCatch(
            matchSubstanceQuery(database, mySearch = mySearch, type = type, partialMatch = type=="name"),
            error = function(error) error
        )
      }
      attr(result,"priorityField") <- type
      attr(result,"priorityPattern") <- mySearch
      
      return(result)
    })

output$substance_helptext <- renderUI({
      
      isError <- is(results$substance_query(), "error")
      
      validate(need(!isError,
              if(isError) return(NULL)))
      
      tags$div(class = "substance-helptext", helpText("Click on a row to proceed."))
    })



# Substance table

output$substance_searchResult <- renderDataTable({
      
      isError <- is(results$substance_query(), "error")
      
      validate(need(!isError,
              if(isError) results$substance_query()$message))
      
      res <- results$substance_query()
      if(is.null(res) || length(res)<1) return(NULL)
      
      # Invalidate when changing search type
      input$substance_type
      
      myData <- database$tabulateSubstanceInfo(
          substanceids = res,
          web = TRUE, limitOutput = TRUE,
          priorityField = attr(res,"priorityField"),
          priorityPattern = attr(res,"priorityPattern"))
      if(is.null(myData)) cat("myData is NULL")
      
      myData[is.na(standardized_structure) & substancetype %in% "uvcb", standardized_structure := "(UVCB)"]
      myData[is.na(standardized_structure) & substancetype %in% "multi", standardized_structure := "(multi-constituent substance)"]
      myData[is.na(standardized_structure), standardized_structure := "(no structure available)"]
      
      if(!"CAS" %in% names(myData)) myData[, CAS := NA_character_]
      if(!"EC" %in% names(myData)) myData[, EC := NA_character_]
      
      datatable(class = "substance-table",
          cbind(myData[,.(name,substancetype,EC,CAS,standardized_structure)], 'All substance identifiers' = 'View all names, identifiers and links to external sources', 'toggle' = ''),
          extensions = "Select",
          rownames = FALSE,
          colnames = c("Name", "Substance type", "EC", "CAS", "Standardized structure", "All substance identifiers", ""),
          escape = FALSE, 
          selection = "none",		
          options = list(
              select = list(style = 'single', items = 'row'),	
              ordering = FALSE,
              dom = '<"top"><"filter"f>t<"bottom"ilp><"clear">',
              columnDefs = list(
                  list(visible = FALSE, targets = 1),
                  list(className = 'td-parent td-parent-details details-control show-control', targets = 5),
                  list(className = 'td-parent td-parent-icon toggle-icon toggle-show', targets = 6),
                  list(className = 'td-parent td-parent-name', targets = 0),
                  list(className = 'td-parent td-parent-ec', targets = 2),
                  list(className = 'td-parent td-parent-cas', targets = 3),
                  list(className = 'td-parent td-parent-structure', targets = 4)	
              ),
              rowCallback = JS("
                      function(row, data){
                      if (typeof data[0] !== 'undefined') {
                      var regexp = /br(?=>)/g;
                      var brList = [...data[0].matchAll(regexp)];
                      if (brList.length == 0){
                      $('td:eq(4)', row).html('View links to external resources');
                      
                      } else if (brList.length == 1){
                      $('td:eq(4)', row).html('+ 1 name and links to external resources');
                      }
                      else {
                      $('td:eq(4)', row).html('+ ' + brList.length + ' names and links to external resources');
                      }
                      } 
                      }
                      ")
          ),
          callback = JS("
                  table.row(1).nodes().to$().css({cursor: 'pointer'});
                  var format = function(data) {
                  if (typeof data[0] !== 'undefined') {
                  var regexp = /br(?=>)/g;
                  var brList = [...data[0].matchAll(regexp)];
                  nBr = brList.length;
                  return '<table class=\"nested-table\">' +
                  '<tr class=\"child-row\">' + 
                  '<td class=\"td-child td-child-name\">' + data[0] + '</td>' +
                  '<td class=\"td-child td-child-ec\">' + data[2] + '</td>' +
                  '<td class=\"td-child td-child-cas\">' + data[3] + '</td>' +
                  '<td class=\"td-child td-child-structure\"></td>' + 
                  '<td class=\"td-child td-child-view-more\"></td>' + 
                  '<td class=\"td-child td-child-icon\"></td></tr></table>';
                  }
                  };
                  
                  table.on('click', 'td.toggle-icon', function(e) {
                  var td = $(this); 
                  var tdmore = td.prev();
                  e.preventDefault();
                  e.stopPropagation();
                  var row = table.row(td.closest('tr'));
                  if (row.child.isShown()) {
                  row.child.hide();
                  td.removeClass(\"toggle-hide\");
                  td.addClass(\"toggle-show\");
                  tdmore.removeClass(\"hide-control\");
                  tdmore.addClass(\"show-control\");
                  if (nBr == 0) {
                  tdmore.html('View links to external resources');
                  }
                  else if (nBr == 1) {
                  tdmore.html('+ 1 name and links to external resources');
                  } else {
                  tdmore.html('+ ' + nBr + ' names and links to external resources');
                  }
                  } else {
                  row.child(format(row.data())).show();
                  td.removeClass(\"toggle-show\");
                  td.addClass(\"toggle-hide\");
                  if (nBr == 0) {
                  tdmore.html('Hide links to external resources'); 
                  }
                  else if (nBr == 1){
                  tdmore.html('- 1 name and links to external resources');
                  } else {
                  tdmore.html('- ' + nBr + ' names and links to external resources');
                  }
                  }
                  });"
          )
      ) 
    }, server = FALSE
)


## ---------------- ##
## B. Toxicity tabs ##
## ---------------- ##

## get the id of the substance

results$substance_set_single <- reactive({
      
      # Selected query compound from table
      substanceSelection <- results$substance_query()[input$substance_searchResult_rows_selected]
      if(is.null(substanceSelection) || length(substanceSelection)==0) return(NULL)
      cat("selected substance:", substanceSelection,"\n")
      
      
      attr(substanceSelection, "priorityField") <- attr(results$substance_query(), "priorityField")
      attr(substanceSelection, "priorityPattern") <- attr(results$substance_query(), "priorityPattern")
      return(substanceSelection)
      
    })


## get info about the substance (substanceid, CAS, structure, etc.), res = substanceid

results$substance_info_single <- reactive({
      
      if (!is.null(results$selected_substance))
        if (length(results$selected_substance) > 0) {
          res <- results$selected_substance
          
          
          if(is.null(res) || length(res)<1) return(NULL) #stop("no results")
          
          myData <- database$tabulateSubstanceInfo(substanceids = res,
              web = TRUE, limitOutput = TRUE,
              priorityField = attr(results$selected_substance,"priorityField"),
              priorityPattern = attr(results$selected_substance,"priorityPattern")
          )
          
          myData         
        }
      
    })

## To Data for filtered Compounds


results$substance_tox_single <- reactive({
      
      req(results$substance_info_single())
      
      toxdata <- database$getSubstanceActivity(results$selected_substance, summarize = TRUE, web = TRUE, skipDartNA = FALSE)
      
      if(is.null(toxdata) || nrow(toxdata$activitySummary) == 0) return(NULL)
      
      sapply(toxdata,
          function(x) if(!is.null(x) && nrow(x)) merge(x, results$substance_info_single() , by = "substanceid", all.x = TRUE),
          simplify = FALSE)
      
    })


results$substance_tox_cutoff <- reactive({
      
      # Selected query compound from table
      substanceSelection <- results$selected_substance
      if(is.null(substanceSelection) || length(substanceSelection)==0) return(NULL)
      
      # combine similar substances (fingerprint similarity) and substances from the same (UVCB) category
      similarityTable <- database$getSimilarSubstances(substanceSelection)
      if(!is.null(similarityTable)){
        similarityTable <- similarityTable[similarity>0.3, .(substanceid, similarity)]
        similarityTable[, similarity_type := "Fingerprint similarity"]
      }
      sameCategorySubstances <- setdiff(database$getSameCategorySubstances(substanceSelection), substanceSelection)
      
      if(length(sameCategorySubstances)){
        sameCategoryTable <- data.table(
            substanceid = sameCategorySubstances,
            similarity = NA_real_,
            similarity_type = "Same category"
        )
        similarityTable <- if(is.null(similarityTable)){
              sameCategoryTable
            } else rbind(similarityTable,sameCategoryTable)
      }
      
      # only return results if similar substances are found
      if(is.null(similarityTable) || length(setdiff(similarityTable[,substanceid],substanceSelection))==0) return(NULL)
      
      substancedata <- database$tabulateSubstanceInfo(substanceids = similarityTable[,substanceid],
          web = TRUE, limitOutput = TRUE,
          priorityField = attr(results$selected_substance,"priorityField"),
          priorityPattern = attr(results$selected_substance,"priorityPattern")
      )
      substancedata <- merge(substancedata, similarityTable, by = "substanceid", all.x = TRUE)
      
      toxdata <- database$getSubstanceActivity(similarityTable[,substanceid],
          summarize = TRUE, web = TRUE, skipDartNA = FALSE)
      
      if(is.null(toxdata) || nrow(toxdata$activitySummary) == 0) return(NULL)
      
      res <- sapply(toxdata,
          function(x) if(!is.null(x) && nrow(x)){
              thisTable <- merge(x, substancedata , by = "substanceid", all.x = TRUE)
              thisTable <- thisTable[order(similarity, na.last = FALSE, decreasing = TRUE)]
              thisTable
            },
          simplify = FALSE)
      res
    })


# II. Overview tab

# A. Left sidebar


# A.0 Name of the substance across all tabs

toxdataActivitySummary <- reactive({
      
      req(results$substance_tox_single())
      # validate(need(results$substance_tox_single(), "No data available"))
      
      toxdataActivitySummary <- results$substance_tox_single()$activitySummary
      
      toxdataActivitySummary[, alternativeSubstanceIdentifiers := 'View more names']
      
      toxdataActivitySummary
      
    })

output$substance_name <- renderDataTable(
    
    uiTable(toxdataActivitySummary(),
        columnMap = c(
            name = '',
            alternativeSubstanceIdentifiers = ''
        ), 
        interactive = TRUE, scrollX = FALSE, filter = "none",
        escape = 1,
        options = list(
            dom = 't',
            ordering = FALSE,
            columnDefs = list(
                list(orderable = FALSE, className = 'td-parent details-control', targets = 1),
                list(orderable = FALSE, targets = 0)
            ),	
            rowCallback = JS(
                "function(row, data, iDisplayIndex, iDisplayIndexFull){",
                "if (typeof data[0] !== 'undefined') {",
                "var regexp = /br(?=>)/g;",
                "var brList = [...data[0].matchAll(regexp)];",
                "if (brList.length == 0){",
                "$('td:eq(1)', row).html('');",
                "$('td:eq(1)', row).removeClass(\"details-control\");",
                "} else if (brList.length == 1){",
                "$('td:eq(1)', row).html('+ 1');",
                "var regexp1 = /(<a href[\\s\\S]*?>[\\s\\S]*?)|(\\b(http|https):\\/\\/.*[^ alt]\\b)/g;",
                "var regexp2 = /<\\/a>/g;",
                "var regexp3 = /<\\/br>/g;",																			
                "var full_text = data[0].replaceAll(regexp1, \"\");",
                "var full_text = full_text.replaceAll(regexp2, \"\");",
                "var full_text = full_text.replaceAll(regexp3, '\\n');",
                "$('td:eq(1)', row).attr('data-title', full_text);",
                "$('td:eq(1)', row).css('cursor', 'pointer');",
                "}",
                "else {",
                "$('td:eq(1)', row).html('+ ' + brList.length);",
                "var regexp1 = /(<a href[\\s\\S]*?>[\\s\\S]*?)|(\\b(http|https):\\/\\/.*[^ alt]\\b)/g;",
                "var regexp2 = /<\\/a>/g;",
                "var regexp3 = /<\\/br>/g;",
                "var full_text = data[0].replaceAll(regexp1, \"\");",
                "var full_text = full_text.replaceAll(regexp2, \"\");",
                "var full_text = full_text.replaceAll(regexp3, '\\n');",
                "$('td:eq(1)', row).attr('data-title', full_text);",
                "}",
                "} else {",
                "$('td:eq(1)', row).html('');",
                "$('td:eq(1)', row).removeClass(\"details-control\");",
                "}",
                "}")
        )
    ))


# A.1. Summary table containing the structure

output$substance_summary_structure <- 
    
    renderDataTable({
          
          
          toxdataActivitySummary()[is.na(standardized_structure) & substancetype %in% "uvcb", standardized_structure := "(UVCB)"]
          toxdataActivitySummary()[is.na(standardized_structure) & substancetype %in% "multi", standardized_structure := "(multi-constituent substance)"]
          toxdataActivitySummary()[is.na(standardized_structure), standardized_structure := "(no structure available)"]
          
          
          uiTable(toxdataActivitySummary()[,"standardized_structure"],
              interactive = TRUE, scrollX = FALSE, filter = "none",
              columnMap = c("standardized_structure" = ""),
              options = list(
                  dom = 't',
                  ordering = FALSE
              )
          ) })



# A.2. Summary table containing the CLP



output$substance_summary1_CLP <- renderDataTable({
      
      validate(
          need(!is.null(results$substance_tox_single()) && !is.na(toxdataActivitySummary()[,"DART CLP"]), 'No data available'))
      
      uiTable(toxdataActivitySummary()[,"DART CLP"],
          interactive = TRUE, scrollX = FALSE, filter = "none",
          columnMap = c("DART CLP" = " "),
          options = list(
              dom = 't',
              ordering = FALSE
          )
      )
    })


# A.3.1 Number of entries of regulatory conclusions


output$substance_summary1_invivo_count <- renderUI({
      
      toxdata <- results$substance_tox_single()
      
      if (is.null(toxdata) || is.na(toxdata$activitySummary[,"DART in vivo"])) {
        
        tags$div(HTML(paste(tags$div(class = "pathway-details-number", "-"), tags$div(class = "pathway-details-total", '/ ', "-"), 
                    tags$div(class = "pathway-details-entries", 'studies report a lowest effect level'))
            ))	
        
      } else {
        
        inVivoTargetCount <- sub("\\D*(\\d+).*", "\\1",toxdata$activitySummary[,"DART in vivo"])			
        inVivoTargetCountTotal <- nrow(toxdata$invivo) 	
        
        tags$div(HTML(paste(tags$div(class = "pathway-details-number", inVivoTargetCount), tags$div(class = "pathway-details-total", '/ ', inVivoTargetCountTotal), 
                    tags$div(class = "pathway-details-entries", 'Entries'))
            ))
      }
      
    })


# A.3.2 Summary table in Regulatory conclusions


renderDataTable({
      
      uiTable(toxdataActivitySummary()[,"DART in vivo"],
          interactive = TRUE, scrollX = FALSE, filter = "none",
          columnMap = c("DART in vivo" = ""),
          options = list(
              dom = 't',
              ordering = FALSE		
          )
      )
      
    })



# A.4. Mammalian phenotypes

generateMammalianPhenotypes <- function() {
  
  renderUI({
        
        rankingObject <- results$substance_pathwayranking_object()
        
        if (is.null(rankingObject) | is.null(rankingObject$mammalianPhenotypes)) {
          tags$div(HTML(paste(tags$div(class = "pathway-details-number", "-")#, 
                  #  tags$div(class = "pathway-details-total", '/ ', "-"), 
                  #  tags$div(class = "pathway-details-entries", 'Entries')
                  )))
        } else {
          
          nUniqueMammalianPhenotypes <- rankingObject$mammalianPhenotypes[,.N] 
          
          if (nUniqueMammalianPhenotypes <= 1) {
            tags$div(HTML(paste(tags$div(class = "pathway-details-number", nUniqueMammalianPhenotypes), 
#                      tags$div(class = "pathway-details-total", '/ ', nMammalianPhenotypes), 
#                      tags$div(class = "pathway-details-entries", 'Entries'))))	
                        tags$div(class = "pathway-details-entries", 'unique phenotype'))))	
          } else {
            tags$div(HTML(paste(tags$div(class = "pathway-details-number", nUniqueMammalianPhenotypes), 
                        tags$div(class = "pathway-details-entries", 'unique phenotypes'))))	
          }
        }
      })
}

output$substance_summary1_phenotypes_mammalian <- generateMammalianPhenotypes()

# A.5. Non-mammalian phenotypes

generateNonMammalianPhenotypes <- function() {
  
  renderUI({
        
        rankingObject <- results$substance_pathwayranking_object()
        
        
        if (is.null(rankingObject) | is.null(rankingObject$nonMammalianPhenotypes)) {
          tags$div(HTML(paste(tags$div(class = "pathway-details-number", "-")#, 
                  #       tags$div(class = "pathway-details-total", '/ ', "-"), 
                  #       tags$div(class = "pathway-details-entries", 'Entries')
                  )))
        } else {
          
          nUniqueNonmammalianPhenotypes <- rankingObject$nonMammalianPhenotypes[,.N] 
          uniquePhenotypesBySpecies <- rankingObject$nonMammalianPhenotypes[,.N, by = species] 
          speciesNames <- uniquePhenotypesBySpecies[, species]
          uniquePhenotypeCounts <- uniquePhenotypesBySpecies[, N]
          
          if (nUniqueNonmammalianPhenotypes <= 1) {
            tagList(
                tags$div(HTML(paste(tags$div(class = "pathway-details-number", nUniqueNonmammalianPhenotypes), 
                            #      tags$div(class = "pathway-details-total", '/ ', nNonmammalianPhenotypes), 
                            tags$div(class = "pathway-details-entries", 'unique phenotype')
                        ))),
                tags$div(style = "margin-bottom:10px;"),
                tags$div(HTML(paste0('<div><div class="gene-num">', uniquePhenotypeCounts,'</div>', 
                            '<div class = "gene-name">', speciesNames, '</div></div>', collapse = "")))
            )
          } else {
            tagList(
                tags$div(HTML(paste(tags$div(class = "pathway-details-number", nUniqueNonmammalianPhenotypes), 
                            #      tags$div(class = "pathway-details-total", '/ ', nNonmammalianPhenotypes), 
                            tags$div(class = "pathway-details-entries", 'unique phenotypes')
                        ))),
                tags$div(style = "margin-bottom:10px;"),
                tags$div(HTML(paste0('<div><div class="gene-num">', uniquePhenotypeCounts,'</div>', 
                            '<div class = "gene-name">', speciesNames, '</div></div>', collapse = "")))) 
            
          }
        }
      })	
}

output$substance_summary1_phenotypes_nonmammalian <- generateNonMammalianPhenotypes()

# A.6.1 number of entries of in vitro data 

output$substance_summary1_invitro_count <-
    
    renderUI({
          
          toxdata <- results$substance_tox_single() 
          
          if (is.null(results$substance_tox_single() ) || is.na(toxdataActivitySummary()[["invitroassaycount"]])) {
            
            tags$div(HTML(paste(tags$div(class = "pathway-details-number", "-"), 
                        tags$div(class = "pathway-details-total", '/ ', "-"), 
                        tags$div(class = "pathway-details-entries", 'assays')
                    )))
            
          } else {
            
            hitCount <- toxdataActivitySummary()[,"invitrohitassaycount"]
            totalCount <- toxdataActivitySummary()[,"invitroassaycount"]
            #hitGeneCount <- toxdataActivitySummary()[,"invitrohitgenecount"]
            #totalGeneCount <- toxdataActivitySummary()[,"invitrogenecount"]
            
            tags$div(HTML(paste(tags$div(class = "pathway-details-number", hitCount), 
                        tags$div(class = "pathway-details-total", '/ ', totalCount), 
                        #tags$div(class = "pathway-details-total", '(', hitGeneCount ,' genes) / ', totalCount),
                        tags$div(class = "pathway-details-entries", 'assays'))))	
            #tags$div(class = "pathway-details-entries", 'assays (',totalGeneCount,' genes)'))))	
            
          }
        })


# A.6.2 Summary table containing the in vitro data

output$substance_summary1_invitro_names <-
    
    renderDataTable({
          if (is.null(results$substance_tox_single()) || is.na(toxdataActivitySummary()[["invitrotargetoccurrences"]])) {
            return(NULL)
            message <- NULL
            
          } else { 
            
            invitroTargetNum <- toxdataActivitySummary()[["invitrotargetoccurrences"]]
            invitroTargetNum <- unlist(strsplit(invitroTargetNum, "<br>"))
            invitroTargetNames <- toxdataActivitySummary()[["invitrotargetnames"]]
            invitroTargetNames <- unlist(strsplit(invitroTargetNames, "<br>"))
            invitroTargetNames <- strsplit(invitroTargetNames, "<br>")
            
            invitroNumByGene = paste(invitroTargetNum, invitroTargetNames, collapse = " ")
            
            toxdataActivitySummary()[, invitroNumByGene := paste0('<div class = "num-name-wrapper"><div class="gene-num">', invitroTargetNum,'</div>', 
                    '<div class = "gene-name">', invitroTargetNames, '</div></div>', collapse = "")]
            
            uiTable(toxdataActivitySummary()[, 'invitroNumByGene'],
                interactive = TRUE, scrollX = FALSE, filter = "none",
                columnMap = c("invitroNumByGene" = ""),
                options = list(
                    dom = 't',
                    ordering = FALSE
                
                ))
          }
          
        })


# B. Calculation of pathway ranking

rvinvitro <- reactiveVal(NULL)
rvmam <- reactiveVal(NULL)
rvnonmam <- reactiveVal(NULL)


results$substance_pathwayranking_object <- reactive({
      
      substanceids <- results$selected_substance
      if (is.null(substanceids))
        return(NULL)
      
      pathwayRankingObject <- PathwayRanking$new(database = database,
          substanceid = substanceids,
          pathwayLevels = getOption("dartpaths_app_pathway_ranking_levels"),
          sizePathwayMin = getOption("dartpaths_app_pathway_ranking_min_genes"),
          sizePathwayMax = getOption("dartpaths_app_pathway_ranking_max_genes"),
          phenotypeRankingLowestLevel = getOption("dartpaths_app_pathway_ranking_lowest_level")
      )

      return(pathwayRankingObject) 
    })

nullOrIndex <- function(selection){
  if(is.null(selection)){
    NULL
  } else {
    which(selection)
  }
  
}

## reset selection of rows in pathway filter when a new pathway ranking object is created
# (after selecting a new substance/category)
observeEvent(results$substance_pathwayranking_object(),
    {
      pathwayRankingObject <- results$substance_pathwayranking_object()
      rvinvitro(nullOrIndex(pathwayRankingObject$defaultSelection$inVitro))
      rvmam(nullOrIndex(pathwayRankingObject$defaultSelection$mammalianPhenotypes))
      rvnonmam(nullOrIndex(pathwayRankingObject$defaultSelection$nonMammalianPhenotypes))
    })


indicesToLogicalVector <- function(defaultVec, indices){
  if (length(defaultVec) == 0){
    defaultVec
  } else {
    sapply(seq_along(defaultVec), `%in%`, indices)
  }
}


results$substance_pathways <- eventReactive(input$calculate_pathway,  {
      				
      pathwayRankingObject <- results$substance_pathwayranking_object()
      if (is.null(pathwayRankingObject))
        return(NULL)
      
      if(nrow(pathwayRankingObject$mammalianPhenotypes) + 
          nrow(pathwayRankingObject$nonMammalianPhenotypes) + 
          nrow(pathwayRankingObject$allInVitro) == 0 ) return(NULL)
      
      inVitroSelection <- indicesToLogicalVector(
          pathwayRankingObject$defaultSelection$inVitro,
          rvinvitro())
      mammalianPhenoSelection <- indicesToLogicalVector(
          pathwayRankingObject$defaultSelection$mammalianPhenotypes,
          rvmam())
      nonMammalianPhenoSelection <- indicesToLogicalVector(
          pathwayRankingObject$defaultSelection$nonMammalianPhenotypes,
          rvnonmam())
      
      pathways <- pathwayRankingObject$rankPathways(
          inVitroSelection = inVitroSelection,
          mammalianPhenoSelection = mammalianPhenoSelection,
          nonMammalianPhenoSelection = nonMammalianPhenoSelection)
      
    })




## pathway overview


substancePathways <- reactive({
      
      substancePathways <- results$substance_pathways()$summary
      
      
      substancePathways
    })

substancePathwaysWithCircle  <- reactive({

      if (is.null(substancePathways()))
        return()
      
      substancePws <- substancePathways()
      
      substancePws[, circleMammalian := paste0('<div class="',
              ifelse(min_p_adjusted_mammalian <= pvalueThresholdSummary, "mammalian-details", "no-details mammalian-no-details"),
              '"></div>')]
      substancePws[, circleNAM := paste0('<div class="',
              ifelse(min_p_adjusted_NAM <= pvalueThresholdSummary,  "nam-details", "no-details nam-no-details"),
              '"></div>')]
      substancePws[, circleInVitro := paste0('<div class="',
              ifelse(scoreInVitro>0,  "invitro-details", "no-details invitro-no-details"),
              '"></div>')]
      
    })

substancePathwaysSorted <- reactive({
      
      if (is.null(substancePathwaysWithCircle()))
        return()
      
      substancePws <- copy(substancePathwaysWithCircle())
      
      substancePathwaysSelection <- switch(input$selectCol,
          "scoreAll" = substancePws[(
                    !is.na(min_p_adjusted_all) & min_p_adjusted_all <= pvalueThresholdSummary)][
              order(min_p_adjusted_all, -scoreInVitro, decreasing = FALSE)][
              , summaryScore := min_p_adjusted_all],
          "scoreMammalian" = substancePws[
              !is.na(min_p_adjusted_mammalian) & min_p_adjusted_mammalian <= pvalueThresholdSummary, ][
              order(min_p_adjusted_mammalian, decreasing = FALSE)][
              , summaryScore := min_p_adjusted_mammalian],
          "scoreNAM" = substancePws[
              !is.na(min_p_adjusted_NAM) & min_p_adjusted_NAM <= pvalueThresholdSummary, ][
              order(min_p_adjusted_NAM, decreasing = FALSE)][
              , summaryScore := min_p_adjusted_NAM],
          "scoreInVitro" = substancePws[scoreInVitro>0, ][
              order(scoreInVitro, decreasing = TRUE)][
              , summaryScore := scoreInVitro],
          "scoreAllWithoutZero" = substancePws[
              !is.na(min_p_adjusted_mammalian) & min_p_adjusted_mammalian <= pvalueThresholdSummary &
                  !is.na(min_p_adjusted_NAM) & min_p_adjusted_NAM <= pvalueThresholdSummary & scoreInVitro>0, ][
              order(min_p_adjusted_all, -scoreInVitro,  decreasing = FALSE)][
              , summaryScore := min_p_adjusted_all]
      )

      # add attributed used for displaying name for the score
      setattr(substancePathwaysSelection, "summaryScoreType",
          if (input$selectCol == "scoreInVitro") "jaccard" else "adjustedP")
      
      # If only in vitro data is available, scoreAll should give a ranking based on in vitro scores
      # (identical to scoreInVitro)
      if (input$selectCol == "scoreAll" && nrow(substancePathwaysSelection) == 0){
          substancePathwaysSelection <- substancePws[scoreInVitro>0, ][order(scoreInVitro, decreasing = TRUE)]
          substancePathwaysSelection[, summaryScore := scoreInVitro]
          setattr(substancePathwaysSelection, "summaryScoreType", "jaccard")
      }
      
      return(substancePathwaysSelection)
      
    })

output$substance_pathways_summary <- renderDataTable({
      
      
      substancePathwaysSelection <- substancePathwaysSorted()
      
      validate(
          need(!is.null(substancePathwaysSelection), 'No data available'))
      
      columnDefs <- list(
          list(visible = TRUE, targets = c(0, 4, 5, 6, 7)),
          list(visible = FALSE, targets = c(1, 2, 3))
      )
      
      roundJaccard <- function(vec) round(vec, nDigitsJaccard)
      roundPValues <- function(vec) round(vec, nDigitsPValues)
      roundSummary <- switch(
          attributes(substancePathwaysSelection)$summaryScoreType,
          "jaccard" = roundJaccard,
          "adjustedP" = roundPValues
      )
      
      
      uiTable(substancePathwaysSelection,
          columnMap = c(
              summaryScore = "",
              min_p_adjusted_mammalian = "",
              min_p_adjusted_NAM = "",
              scoreInVitro = "",
              circleMammalian = "",
              circleNAM = "",
              circleInVitro = "",
              event_name = ""
          ),
          columnConversions = c(
              summaryScore = roundSummary,
              min_p_adjusted_mammalian = roundPValues,
              min_p_adjusted_NAM = roundPValues,
              scoreInVitro = roundJaccard
          ),
          interactive = FALSE, 
          scrollX = FALSE,
          options = list(
              dom = '<"top">t<"bottom"lp><"clear">',
              #order = orderSelection,
              pageLength = 20,
              searching = FALSE,
              columnDefs = columnDefs),
          callback = JS("table.on( 'click', 'tr', function() {
                  tr = $(this)
                  if (tr.hasClass('selected')) {
                  table.$('tr.odd').css('opacity', '0.5');					
                  table.$('tr.even').css('opacity', '0.5');
                  table.$('tr.odd.selected').css('opacity', '1');					
                  table.$('tr.even.selected').css('opacity', '1');
                  table.on('draw', function() {
                  table.$('tr.odd').css('opacity', '0.5');					
                  table.$('tr.even').css('opacity', '0.5');
                  table.$('tr.odd.selected').css('opacity', '1');					
                  table.$('tr.even.selected').css('opacity', '1');
                  });
                  } else {
                  table.$('tr.odd').css('opacity', '1');					
                  table.$('tr.even').css('opacity', '1');
                  table.on('draw', function(){
                  table.$('tr.odd').css('opacity', '1');					
                  table.$('tr.even').css('opacity', '1');
                  });
                  }});
                  ")
      )
      
    }
)



outputOptions(output, "substance_pathways_summary", priority = 2) 


## C. Pathway details 

# C.1. Name of pathway



output$pathwayNameSelectedOverview  <-
    
    renderUI({
          
          req(substancePathwaysSorted())			
          
          if (is.null(substancePathwaysSorted()) || nrow(substancePathwaysSorted()) ==0 ) 
          { return(NULL)
            message <- NULL
          } else {
            validate(
                need(input$substance_pathways_summary_rows_selected, "Select a pathway on the left to learn more")
            )
            
            selectedRowIndex <-  input$substance_pathways_summary_rows_selected
            
            if(length(selectedRowIndex)){
              info <- substancePathwaysSorted()[selectedRowIndex, ]
              actionButton(inputId = "pathway_link", label = HTML(paste0('<div>', info$event_name, '</div><span class = "pathway-link-text">', 
                          icon("arrow-right", lib = "font-awesome"), ' Explore this pathway </span>')), value = info$event_name)	
              
              
              
            }		
          }
        })




# C.2. reactome link

output$reactomelinkOverview <-
    
    
    renderUI({
          
          req(substancePathwaysSorted())	
          
          if (is.null(substancePathwaysSorted()) || nrow(substancePathwaysSorted()) ==0 ) return(NULL)
          message <- NULL
          
          selectedRowIndex <- input$substance_pathways_summary_rows_selected
          
          if(length(selectedRowIndex)){
            
            info <- substancePathwaysSorted()[selectedRowIndex, ]
            
            geneData <- database$getPathwayOrthology(pathwayid = info$reactome_pathway_stable_identifier, renameEvent = TRUE)			
            
            tagList(
                tags$button("View pathway on reactome.org",		
                    id = "gene_showReactome_overview", 
                    class = "btn btn-default action-button shiny-bound-input",
                    style = "width:100%",
                    onclick = paste0("window.open('", attr(geneData, "reactomeLink"),
                        "', '_blank')")),
                tags$div(style = "padding-bottom:20px; border-bottom:1px solid rgba(0, 0, 0, 0.2);")
            )
          }		
        })



# C.3. Summary Score

output$summaryScoreOverview <-
    
    renderUI({
          
          req(substancePathwaysSorted())
          
          if (is.null(substancePathwaysSorted()) || nrow(substancePathwaysSorted()) ==0 ) return(NULL)
          message <- NULL
          
          selectedRowIndex <- input$substance_pathways_summary_rows_selected
          
          
          if(length(selectedRowIndex)){
            info <- substancePathwaysSorted()[selectedRowIndex, ]
            
            scoreAll <- round(info$summaryScore, digits=nDigitsPValues)
            summaryScoreName <- switch(attributes(info)$summaryScoreType,
              "jaccard" = "Jaccard index",
              "adjustedP" = "Adjusted p-value"
            )
            
            tagList(
                tags$h2(class = "info-right-sidebar-title", summaryScoreName),
                tags$div(scoreAll
                )	
            )	
          }	
          else {
            return(NULL)
            message <- NULL
          }
        })


# C.4. No. of genes in pathway

output$nGenesPathwayOverview <-
    
    renderUI({
          
          req(substancePathwaysSorted())	  
          
          if (is.null(substancePathwaysSorted()) || nrow(substancePathwaysSorted()) ==0 ) return(NULL)
          message <- NULL
          
          selectedRowIndex <- input$substance_pathways_summary_rows_selected	
          
          if(length(selectedRowIndex)){
            info <- substancePathwaysSorted()[selectedRowIndex, ]
            
            tagList(
                tags$h2(class = "info-right-sidebar-title", "No. of genes in pathway"),
                tags$div(info$nGenesPathway
                )	
            )	
          }			
        })




# C.5. plot

output$plotOverview  <- 
    
    renderUI({
          
          req(substancePathwaysSorted())	  
          
          if (is.null(substancePathwaysSorted()) || nrow(substancePathwaysSorted()) ==0 ) return(NULL)
          message <- NULL
          
          selectedRowIndex <- input$substance_pathways_summary_rows_selected
          
          if(length(selectedRowIndex)){
            info <- substancePathwaysSorted()[selectedRowIndex, ]
            
            geneData <- database$getPathwayOrthology(pathwayid = info$reactome_pathway_stable_identifier, renameEvent = TRUE)			
            summarizedGeneData <- summarizeGeneData(geneData = geneData)
            
            tagList(
                tags$h2(class = "info-right-sidebar-title", "Human genes conserved per species"),
                tags$div(class = "bar-plot-container",
                    tags$div(id = "gene_barplot_overview",
                        renderPlotly({	plotGeneData(tableGenes = summarizedGeneData, colors = geneColors, width = 380)})
                    )
                )
            )
          }
        })



# C.6. Mammalian phenotypes pointing to this pathway

generateMamPhenotypesOverview <- function() {
  
  renderUI({
        
        substancePathways <- substancePathwaysSorted()
        if (is.null(substancePathways) || nrow(substancePathways) ==0 ) return(NULL)
        message <- NULL
        
        selectedRowIndex <- input$substance_pathways_summary_rows_selected
        
        if(length(selectedRowIndex)){
          info <- substancePathways[selectedRowIndex, ]
          
          numMammalian <- info$contributingPhenotypesMammalian
          
          #totalMammalian <- info$totalPhenotypesMammalian # this is the number of phenotypes contributing to at least one pathway
          totalMammalian <- results$substance_pathways()$selectedMammalianPhenotypes[,.N] # total number of (selected) phenotypes
          
          tagList(tags$div(style = "border-bottom:1px solid rgba(0, 0, 0, 0.2);"),
              tags$h2(class = "info-right-sidebar-title", "Mammalian phenotypes pointing to this pathway"),
              tags$div(HTML(paste(tags$div(class = "pathway-details-number", numMammalian),
                          tags$div(class = "pathway-details-total", '/ ', totalMammalian), 
                          tags$div(class = "pathway-details-entries", 'selected phenotypes'))
                  ))	
          )	
        } 
        
      })
}

output$MammalianPhenotypesOverview <- generateMamPhenotypesOverview()





# C.7. Non-mammalian phenotypes pointing to this pathway

generateNonMamPhenotypesOverview <- function() {
  
  renderUI({
        
        substancePathways <- substancePathwaysSorted()
        
        if (is.null(substancePathways) || nrow(substancePathways) ==0 ) return(NULL)
        message <- NULL
        
        selectedRowIndex <- input$substance_pathways_summary_rows_selected
        
        if(length(selectedRowIndex)){
          info <- substancePathwaysSorted()[selectedRowIndex, ]
          
          numNAM <- info$contributingPhenotypesNonMammalian
          
          #totalNAM <- info$totalPhenotypesNonMammalian # this is the number of phenotypes contributing to at least one pathway
          totalNAM <- results$substance_pathways()$selectedNonMammalianPhenotypes[,.N] # total number of (selected) phenotypes
          
          
          tagList(
              tags$h2(class = "info-right-sidebar-title", "Non-mammalian phenotypes pointing to this pathway"),
              tags$div(HTML(paste(tags$div(class = "pathway-details-number", numNAM), 
                          tags$div(class = "pathway-details-total", '/ ', totalNAM), 
                          tags$div(class = "pathway-details-entries", 'selected phenotypes')))
              )	
          )	
        }		
        
      })
}

output$NAMPhenotypesOverview <- generateNonMamPhenotypesOverview()

# C.8. In vitro tests pointing to this pathway

generateInVitroDetailsOverview <- function() {
  
  renderUI({
        
        substancePathways <- substancePathwaysSorted()
        
        if (is.null(substancePathways) || nrow(substancePathways) ==0 ) return(NULL)
        message <- NULL
        
        selectedRowIndex <- input$substance_pathways_summary_rows_selected
        
        if(length(selectedRowIndex)){
          info <- substancePathwaysSorted()[selectedRowIndex, ]
          
          invitroPwHits <- info$invitroPwHits
          
          nGenesPathway <- info$nGenesPathway
          
          invitroSubstanceHits <- info$invitroSubstanceHits
          # TODO: invitroSubstanceHits is not the same as what we show in the left column. 
          
          tagList(
              tags$h2(class = "info-right-sidebar-title", "Genes in this pathways with at least one in vitro hit"),
              tags$div(HTML(paste(tags$div(class = "pathway-details-number", invitroPwHits), 
                          tags$div(class = "pathway-details-total", '/ ', invitroSubstanceHits), 
                          tags$div(class = "pathway-details-entries", 'unique gene(s) with at least one hit in selected assays'))
                  )),
              tags$div(style = "margin-bottom:20px; padding-bottom:20px; border-bottom:1px solid rgba(0, 0, 0, 0.2);"),
              #    tags$h2(class = "overview-title", "Alternative test recommendation"),
              
              createContactButton(HTML('<div class = "overview-helptext"> Go to Vivaltes for wet-lab testing to validate pathway predictions </div>'))
          )
          
        }		
        
      })
}

output$InvitroDetailsOverview <- generateInVitroDetailsOverview()

# C.9. Pathway details: phenotypes tab


output$phenotypes_details <-
    
    renderUI({
          
          substancePathways <- substancePathwaysSorted()
          
          if (is.null(substancePathways) || nrow(substancePathways) ==0 ) {
            return(NULL)
            message <- NULL
          } else { validate(
                need(input$substance_pathways_summary_rows_selected, "Select a pathway on the left to learn more")
            )	
            
            selectedRowIndex <- input$substance_pathways_summary_rows_selected
            
            results_substance_pathways <- results$substance_pathways()
            
            if(length(selectedRowIndex) && "contributingPhenotypes" %in% names(results_substance_pathways)){
              
              selectedPathwayFromRanking <- substancePathwaysSorted()[selectedRowIndex,reactome_pathway_stable_identifier]
              phenotypeInfo <- results_substance_pathways$contributingPhenotypes[reactome_pathway_stable_identifier %in% selectedPathwayFromRanking]
              setnames(phenotypeInfo,c("go_id", "go_id_name"),c("phenotype", "phenotypename"))
              
              tagList(tags$h2(class = "info-tab-title", "Mammalian test phenotypes"),
                  tags$div(class = "pheno-mam-table", renderDataTable(
                          
                          uiTable(phenotypeInfo[mammalian == TRUE,],
                              interactive = TRUE, scrollX = FALSE, filter = "none",
                              columnMap = c("phenotypename" = ""),
                              options = list(
                                  dom = 't',	
                                  paging = FALSE,
                                  pageLength = all,
                                  ordering = FALSE
                              )
                          )
                      )
                  
                  ),
                  tags$h2(class = "info-tab-title", "Non-mammalian test phenotypes"),
                  tags$div(class = "pheno-non-mam-table", renderDataTable(uiTable(phenotypeInfo[mammalian == FALSE,],
                              interactive = TRUE, scrollX = FALSE, filter = "none",
                              columnMap = c("phenotypename" = ""),
                              options = list(
                                  dom = 't',
                                  ordering = FALSE,
                                  paging = FALSE,
                                  pageLength = all
                              ))))
              )}
            
            else { tags$div("No data available")}
            
          }
        })



# C.10. Pathway details: in vitro tab

generateInVitroDetails <- function() {
  
  renderUI({
        
        substancePathways <- substancePathwaysSorted()
        
        if (is.null(substancePathways) || nrow(substancePathways) ==0 ) {
          return(NULL)
          message <- NULL
        } else { validate(
              need(input$substance_pathways_summary_rows_selected, "Select a pathway on the left to learn more")
          )	
          
          selectedRowIndex <- input$substance_pathways_summary_rows_selected
          results_substance_pathways <- results$substance_pathways()	
          
          if(length(selectedRowIndex) && "contributingInVitroGenes" %in% names(results_substance_pathways)){
            
            #hitCount <- toxdataActivitySummary()[,"invitrohitassaycount"]
            #totalCount <- toxdataActivitySummary()[,"invitroassaycount"]
            #hitGeneCount <- toxdataActivitySummary()[,"invitrohitgenecount"]
            #totalGeneCount <- toxdataActivitySummary()[,"invitrogenecount"]
            scoreInVitro <- substancePathwaysSorted()[selectedRowIndex, "scoreInVitro"]
            invitroPwHits <- substancePathwaysSorted()[selectedRowIndex,invitroPwHits]
            invitroPwMeasured <- substancePathwaysSorted()[selectedRowIndex,invitroPwMeasured]
            invitroSubstanceHits <- substancePathwaysSorted()[selectedRowIndex,invitroSubstanceHits]
            #nGenesPathway <- substancePathwaysSorted()[selectedRowIndex,nGenesPathway]
            
            selectedPathwayFromRanking <- substancePathwaysSorted()[selectedRowIndex,reactome_pathway_stable_identifier]
            info <- results_substance_pathways$contributingInVitroGenes[reactome_pathway_stable_identifier %in% selectedPathwayFromRanking]
            tagList(
                tags$h2(class = "info-tab-title", "In vitro hits for this pathway with AC50 (M)"),
                tags$div(renderDataTable(uiTable(info,
                            interactive = TRUE, scrollX = FALSE, filter = "none",
                            columnMap = c("gene" = "", "ac50" = ""),
                            options = list(
                                dom = 't',
                                paging = FALSE,
                                pageLength = all,
                                ordering = FALSE
                            )
                        ))),
                tags$h2(class = "info-tab-title", "In vitro score"),
                tags$h2(class = "info-right-sidebar-title", "Unique hits in selected data for this substance"),
                tags$div(invitroSubstanceHits),
                tags$h2(class = "info-right-sidebar-title", "Unique genes measured for the selected pathway"),
                tags$div(invitroPwMeasured),
                tags$h2(class = "info-right-sidebar-title", "Overlap (hits for this substance that are part of selected pathway)"),
                tags$div(invitroPwHits),
                tags$h2(class = "info-right-sidebar-title", "In vitro score (Jaccard index)"),
                tags$div(invitroPwHits, '/ (', invitroSubstanceHits  ,' + ', invitroPwMeasured ,' - ',invitroPwHits,') = ', round(scoreInVitro,nDigitsJaccard))
            )
          }
        }
      })
}	

output$invitro_details <- generateInVitroDetails()



# C.11. Pathway details: individual (not yet aggregated) pathways


output$ranking_indiv_pathways <-
    
    renderUI({
          
          substancePathways <- substancePathwaysSorted()
          
          if (is.null(substancePathways) || nrow(substancePathways) ==0 ) {
            return(NULL)
            message <- NULL
          } else { validate(
                need(input$substance_pathways_summary_rows_selected, "Select a pathway on the left to learn more")
            )	
            
            selectedRowIndex <- input$substance_pathways_summary_rows_selected
            
            results_substance_pathways <- results$substance_pathways()
            
            if(length(selectedRowIndex) && "contributingPhenotypes" %in% names(results_substance_pathways)){
              
              selectedPathwayFromRanking <- substancePathwaysSorted()[selectedRowIndex,reactome_pathway_stable_identifier]
              indivPathways <- results_substance_pathways$individualPathwayScores[
                  level_parent %in% selectedPathwayFromRanking &
                      ( p_adjusted_all <= pvalueThresholdIndividual | 
                        p_adjusted_mammalian <= pvalueThresholdIndividual |
                        p_adjusted_NAM <= pvalueThresholdIndividual) ][
                  order(p_adjusted_all, p_adjusted_mammalian, p_adjusted_NAM, decreasing = FALSE)]
              
              indivPathways[, pathway:= createLink(paste0("https://reactome.org/PathwayBrowser/#/",reactome_pathway_stable_identifier), event_name)]
              indivPathways[, p_adjusted_mammalian := round(p_adjusted_mammalian,nDigitsPValues)]
              indivPathways[, p_adjusted_NAM := round(p_adjusted_NAM,nDigitsPValues)]
              indivPathways[, p_adjusted_all := round(p_adjusted_all,nDigitsPValues)]
              
              tagList(tags$h2(class = "info-tab-title", "Contribution of downstream pathways to score of selected pathway"),
                  tags$div(class = "indiv-pathways-table", renderDataTable(
                          
                          uiTable(indivPathways[,.(level, pathway, p_adjusted_mammalian, p_adjusted_NAM, p_adjusted_all)],
                              interactive = TRUE, scrollX = FALSE, filter = "none",
                              columnMap = c(
                                  level = "Level",
                                  pathway = "Pathway",
                                  p_adjusted_mammalian = "Mammalian<br/>(adjusted p-value)",
                                  p_adjusted_NAM = "Non-mammalian<br/>(adjusted p-value)",
                                  p_adjusted_all = "All phenotypes<br/>(adjusted p-value)"),
                              options = list(
                                  dom = 't',	
                                  paging = FALSE,
                                  pageLength = all,
                                  ordering = FALSE,
								  scrollX = TRUE
                              )
                          )
                      )
                  
                  )
              )}
            
            else { tags$div("No data available")}
            
          }
        })





## D. Filter for pathway ranking

## D.1. In vitro tab in Modal

generateAllInVitro <- function(){
  
  
  renderDataTable({
        
        tableToRender <- copy(results$substance_pathwayranking_object()$allInVitro)
        #   if (nrow(tableToRender)==0) return(NULL)
        
        validate(
            need(nrow(tableToRender) != 0, 'No data available'))
        
        tableToRender[ , ac50 := ifelse(hitcall, formatC(ac50*1e-6, format = "e", digits = 2), "(no hit)")]
        tableToRender <- cbind(tableToRender, 'checkbox'= NA)
        
        selectionExists <- !is.null(rvinvitro())
        
        if (selectionExists){
          
          callbackText <- JS(paste0("table.rows([", paste(rvinvitro()-1, collapse=","), "]).select();"))				
        } else {
          
          # no row selected by default	
          callbackText <- JS("table.rows([]).select();")
          # all rows selected by default		
          #callbackText <- JS("table.rows().select();")
        }	
        
        uiTable(tableToRender, interactive = TRUE, scrollX = FALSE, 
          #  extensions = c('Select', 'Buttons'),
            filter = "none", 
            columnSelection = c("checkbox", "assay", "ac50", "species", "gene"),
            columnConversions = c(),
            columnMap = c("checkbox" = "", "assay" = "", "hitcall" = "", "ac50" = "", "species" = "", "biologicalprocess" = "", "gene" = ""),
            #selection = 'none',
			selection = list(mode = 'multiple', selected = rvinvitro(), target = "row"),			
            #pre-selection
            callback = callbackText,
            options = list(	
                #select = list(style = 'multiple', items = 'row'),
                dom = 'Bt',
              #  buttons = c('selectAll', 'selectNone'),
                ordering = FALSE,
                paging = FALSE,
                pageLength = all,
                rowId = 0,
                columnDefs = list(
                    list(title = "", className = "select-checkbox",
                        orderDataType = "select-checkbox", 
                        targets = 0
                    )
                )
            ))
        
        
      }, server = FALSE)
  
}



asChar <- function(x){
  if(is.null(x)){
    "NULL"
  } else {
    as.character(x)
  }
  
}

observeEvent(input$allInVitro_rows_selected,
    rvinvitro(input$allInVitro_rows_selected),
    ignoreNULL = FALSE)

observeEvent(input$mammalianPhenotypes_rows_selected,
    rvmam(input$mammalianPhenotypes_rows_selected),
    ignoreNULL = FALSE)

observeEvent(input$nonMammalianPhenotypes_rows_selected,
    rvnonmam(input$nonMammalianPhenotypes_rows_selected),
    ignoreNULL = FALSE)

output$allInVitro <- generateAllInVitro()


outputOptions(output, "allInVitro", suspendWhenHidden = FALSE)

## D.2. Mammalian tab in Modal

generatePhenotypes <- function(){
  renderDataTable({
        
        
        tableToRender <- results$substance_pathwayranking_object()$mammalianPhenotypes
        
        #       if (nrow(tableToRender)==0) return(NULL)
        
        validate(
            need(nrow(tableToRender) != 0, 'No data available'))
        
        tableToRender <- cbind(tableToRender, 'Read full text'='Read full text')
        
        tableToRender <- cbind(tableToRender, 'checkbox'= NA)
        
        selectionExists <- !is.null(rvmam())
        
        cb1 <- if (selectionExists){
              
              JS(paste0("var format = function(data) {
                          if (data[3] !== null) {
                          return '<table class=\"invivo-nested-table\">' +
                          '<tr class=\"invivo-child-row\">' +
                          '<td class=\"td-before-conclusions\"></td>' +
                          '<td class=\"td-child-conclusions\">' + data[3] + '</td>' + 
                          '<td class=\"td-after-conclusions\"></td>' +
                          '</tr></table>';
                          }
                          };
                          
                          table.on('click', 'td.toggle-icon', function(e) {
                          var td = $(this); 
                          var row = table.row(td.closest('tr'));
                          e.preventDefault();
                          e.stopPropagation();
                          if (row.child.isShown()) {
                          row.child.hide();
                          td.removeClass(\"toggle-hide\");
                          td.addClass(\"toggle-show\");
                          td.html('Read full text');
                          } else {
                          row.child(format(row.data())).show();
                          td.removeClass(\"toggle-show\");
                          td.addClass(\"toggle-hide\");
                          td.html('Hide full text');
                          }
                          });
                          table.rows([", paste(rvmam()-1, collapse=","), "]).select();"
                  ))
            } else {
              JS("var format = function(data) {
                      if (data[3] !== null) {
                      return '<table class=\"invivo-nested-table\">' +
                      '<tr class=\"invivo-child-row\">' +
                      '<td class=\"td-before-conclusions\"></td>' +
                      '<td class=\"td-child-conclusions\">' + data[3] + '</td>' + 
                      '<td class=\"td-after-conclusions\"></td>' +
                      '</tr></table>';
                      }
                      };
                      
                      table.on('click', 'td.toggle-icon', function(e) {
                      var td = $(this); 
                      var row = table.row(td.closest('tr'));
                      e.preventDefault();
                      e.stopPropagation();
                      if (row.child.isShown()) {
                      row.child.hide();
                      td.removeClass(\"toggle-hide\");
                      td.addClass(\"toggle-show\");
                      td.html('Read full text');
                      } else {
                      row.child(format(row.data())).show();
                      td.removeClass(\"toggle-show\");
                      td.addClass(\"toggle-hide\");
                      td.html('Hide full text');
                      }
                      });
                      table.rows([]).select();"
              )
              
            }
        
        uiTable(tableToRender, interactive = TRUE, scrollX = FALSE, 
          #  extensions = c('Select', 'Buttons'),	
            filter = "none", 
            columnSelection = c("checkbox", "species", "phenotypename", "details", "Read full text"),
            columnMap = c("checkbox" = "", "species" = "", "phenotypename" = "", "details" = "", "Read full text" = ""),
          #  selection = "none",
			selection = list(mode = 'multiple', selected = rvmam(), target = "row"),
            options = list(	
              #  select = list(style = 'multiple', items = 'row'),		
                dom = 'Bt',
               # buttons = c('selectAll', 'selectNone'),
                ordering = FALSE,
                paging = FALSE,
                pageLength = all,
                rowId = 0,
                columnDefs = list(
                    list(title = "", className = "select-checkbox",
                        orderDataType = "select-checkbox", 
                        targets = 0
                    ),
                    list(className = 'toggle-icon toggle-show', targets = 4),
                    list(className = 'td-parent-conclusions', 
                        targets = 3
                    )
                ),
                rowCallback = JS("
                        function(row, data){
                        var full_text = data[3];
                        var max_chars = 35
                        if (data[3] == null) {
                        $('td:eq(4)', row).html('');
                        $('td:eq(4)', row).removeClass(\"toggle-icon toggle-show\");
                        } else {
                        if(full_text.length > max_chars) {
                        $('td:eq(3)', row).html(full_text.substring(0, 35) + '...');
                        }
                        }
                        }
                        ")
            ),
            callback = cb1)
      }, server = FALSE)
}

output$mammalianPhenotypes <- generatePhenotypes()

outputOptions(output, "mammalianPhenotypes", suspendWhenHidden = FALSE)


# dataTableProxy to select/ deselect all rows in modal tables

tableProxyMam <- dataTableProxy('mammalianPhenotypes')

observeEvent(input$SelectAllMam, {
			
			selected <- 1:nrow(results$substance_pathwayranking_object()$mammalianPhenotypes)
			
			selectRows(proxy = tableProxyMam,
					selected = selected)
		})


observeEvent(input$DeselectAllMam, {
			
			selectRows(proxy = tableProxyMam, list())
		})


tableProxyNonMam <- dataTableProxy('nonMammalianPhenotypes')

observeEvent(input$SelectAllNonMam, {
			
			selected <- 1:nrow(results$substance_pathwayranking_object()$nonMammalianPhenotypes)
			
			selectRows(proxy = tableProxyNonMam,
					selected = selected)
		})


observeEvent(input$DeselectAllNonMam, {
			
			selectRows(proxy = tableProxyNonMam, list())
		})


tableProxyInVitro <- dataTableProxy('allInVitro')

observeEvent(input$SelectAllInVitro, {
			
			selected <- 1:nrow(results$substance_pathwayranking_object()$allInVitro)
			
			selectRows(proxy = tableProxyInVitro,
					selected = selected)
		})


observeEvent(input$DeselectAllInVitro, {
			
			selectRows(proxy = tableProxyInVitro, list())
		})



## D.3. Non-Mammalian tab in Modal



generateNonMamPhenotypes <- function(){
  renderDataTable({
        
        tableToRender <- results$substance_pathwayranking_object()$nonMammalianPhenotypes
        
        
        validate(
            need(nrow(tableToRender) != 0, 'No data available'))
        
        tableToRender <- cbind(tableToRender, 'Read full text'='Read full text')
        
        tableToRender <- cbind(tableToRender, 'checkbox'= NA)
        
        selectionExists <- !is.null(rvnonmam())
        
        cb1 <- if (selectionExists){
              
              JS(paste0("var format = function(data) {
                          if (data[3] !== null) {
                          return '<table class=\"invivo-nested-table\">' +
                          '<tr class=\"invivo-child-row\">' +
                          '<td class=\"td-before-conclusions\"></td>' +
                          '<td class=\"td-child-conclusions\">' + data[3] + '</td>' + 
                          '<td class=\"td-after-conclusions\"></td>' +
                          '</tr></table>';
                          }
                          };
                          
                          table.on('click', 'td.toggle-icon', function(e) {
                          var td = $(this); 
                          var row = table.row(td.closest('tr'));
                          e.preventDefault();
                          e.stopPropagation();
                          if (row.child.isShown()) {
                          row.child.hide();
                          td.removeClass(\"toggle-hide\");
                          td.addClass(\"toggle-show\");
                          td.html('Read full text');
                          } else {
                          row.child(format(row.data())).show();
                          td.removeClass(\"toggle-show\");
                          td.addClass(\"toggle-hide\");
                          td.html('Hide full text');
                          }
                          });
                          table.rows([", paste(rvnonmam()-1, collapse=","), "]).select();"
                  ))
            } else {
              JS("var format = function(data) {
                      if (data[3] !== null) {
                      return '<table class=\"invivo-nested-table\">' +
                      '<tr class=\"invivo-child-row\">' +
                      '<td class=\"td-before-conclusions\"></td>' +
                      '<td class=\"td-child-conclusions\">' + data[3] + '</td>' + 
                      '<td class=\"td-after-conclusions\"></td>' +
                      '</tr></table>';
                      }
                      };
                      
                      table.on('click', 'td.toggle-icon', function(e) {
                      var td = $(this); 
                      var row = table.row(td.closest('tr'));
                      e.preventDefault();
                      e.stopPropagation();
                      if (row.child.isShown()) {
                      row.child.hide();
                      td.removeClass(\"toggle-hide\");
                      td.addClass(\"toggle-show\");
                      td.html('Read full text');
                      } else {
                      row.child(format(row.data())).show();
                      td.removeClass(\"toggle-show\");
                      td.addClass(\"toggle-hide\");
                      td.html('Hide full text');
                      }
                      });
                      table.rows([]).select();"
              )
              
            }
        
        uiTable(tableToRender, interactive = TRUE, scrollX = FALSE, 
         #   extensions = c('Select', 'Buttons'),	
            filter = "none", 
            columnSelection = c("checkbox", "species", "phenotypename", "details", "Read full text"),
            columnMap = c("checkbox" = "", "species" = "", "phenotypename" = "", "details" = "", "Read full text" = ""),
            #selection = "none",
		selection = list(mode = 'multiple', selected = rvnonmam(), target = "row"),
            options = list(	
                #select = list(style = 'multiple', items = 'row'),		
                dom = 'Bt',
             #   buttons = c('selectAll', 'selectNone'),
                ordering = FALSE,
                paging = FALSE,
                pageLength = all,
                rowId = 0,
                columnDefs = list(
                    list(title = "", className = "select-checkbox",
                        orderDataType = "select-checkbox", 
                        targets = 0
                    ),
                    list(className = 'toggle-icon toggle-show', targets = 4),
                    list(className = 'td-parent-conclusions', 
                        targets = 3
                    )
                ),
                rowCallback = JS("
                        function(row, data){
                        var full_text = data[3];
                        var max_chars = 35
                        if (data[3] == null) {
                        $('td:eq(4)', row).html('');
                        $('td:eq(4)', row).removeClass(\"toggle-icon toggle-show\");
                        } else {
                        if(full_text.length > max_chars) {
                        $('td:eq(3)', row).html(full_text.substring(0, 35) + '...');
                        }
                        }
                        }
                        ")
            ),
            callback = cb1)
      }, server = FALSE)
}


output$nonMammalianPhenotypes <- generateNonMamPhenotypes()


outputOptions(output, "nonMammalianPhenotypes", suspendWhenHidden = FALSE)


## I. Pathway Ranking tab

output$substance_pathways <-  
    
    renderDataTable({
          
          validate(
              need(!is.null(substancePathways()) || nrow(substancePathways()) != 0, 'No data available')
          )
          
          substancePathways <- substancePathways()[order(min_p_adjusted_all, -scoreInVitro, decreasing = FALSE)]		
          
          substancePathways[,pathway:= createLink(paste0("https://reactome.org/PathwayBrowser/#/",reactome_pathway_stable_identifier), event_name)]
          
          roundJaccard <- function(vec) round(vec, nDigitsJaccard)
          roundPValues <- function(vec) round(vec, nDigitsPValues)
          
          uiTable(substancePathways,
              columnMap = c(
                  level = "Level",
                  pathway = "Pathway",
                  genes = "In vitro hits",
                  min_p_adjusted_mammalian = "Mammalian<br/>(adjusted p-value)",
                  min_p_adjusted_NAM = "Non-mammalian<br/>(adjusted p-value)",
                  min_p_adjusted_all = "All phenotypes<br/>(adjusted p-value)",
                  scoreInVitro = "In vitro<br/>(Jaccard index)"),
              columnConversions = c(
                  min_p_adjusted_mammalian = roundPValues,
                  min_p_adjusted_NAM = roundPValues,
                  scoreInVitro = roundJaccard,
                  min_p_adjusted_all = roundPValues
              ),
              interactive = TRUE, scrollX = FALSE,
              options = list(dom = '<"top">t<"bottom"ilp><"clear">'
              )	
          )}
    )


## III. Mammalian tab ##

RegulatoryConclusionsTable <- reactive ({
      
      RegulatoryConclusionsTable <- cbind(results$substance_tox_single()$invivo, 'Read full text'='Read full text')
      RegulatoryConclusionsTable[!is.na(conclusions), conclusions:= paste0(conclusions, ' <a href="', url, '" target="_blank">(source)</a>')] 
      RegulatoryConclusionsTable
    })

output$substance_mammalian <- renderDataTable({
      
      
      validate(
          need(
              !is.null(results$substance_tox_single()) && 
                  !is.null(results$substance_tox_single()$invivo) && 
                  nrow(results$substance_tox_single()$invivo)!=0, 'No data available')
      )	  
      
      uiTable(RegulatoryConclusionsTable(), interactive = TRUE, scrollX = FALSE, filter = "top",
          columnSelection = c("species", "phenotypeclass", "effectdescriptor", "effect", "year", "glp", "guideline", "conclusions", "Read full text", "sourcedb"),
          columnMap = c("species" = "Species", "phenotypeclass"= "Phenotype", "effectdescriptor" = "Effect descriptor", "effect" = "Effect", "year" = "Year",
              "glp" = "GLP", "guideline" = "Guideline", "conclusions" = "Unstructured text", 
              "Read full text" = "", "sourcedb" = "Source Database"),
          options = list(
              dom = '<"top">t<"bottom"ilp><"clear">',
              columnDefs = list(
                  list(orderable = FALSE, className = 'toggle-icon toggle-show', targets = 8),
                  list(orderable = FALSE, 
                      className = 'td-parent-conclusions', 
                      targets = 7
                  )
              ),
              rowCallback = JS("
                      function(row, data){
                      var full_text = data[7];
                      var max_chars = 35
                      if (data[7] == null) {
                      $('td:eq(8)', row).html('');
                      $('td:eq(8)', row).removeClass(\"toggle-icon toggle-show\");
                      } else {
                      if(full_text.length > max_chars) {
                      $('td:eq(7)', row).html(full_text.substring(0, 35) + '...');
                      }
                      }
                      }
                      ")
          
          
          ),
          callback = JS("var format = function(data) {
                  if (data[7] !== null) {
                  return '<table class=\"invivo-nested-table\">' +
                  '<tr class=\"invivo-child-row\">' +
                  '<td class=\"td-before-conclusions\"></td>' +
                  '<td class=\"td-child-conclusions\">' + data[7] + '</td>' + 
                  '<td class=\"td-after-conclusions\"></td>' +
                  '</tr></table>';
                  }
                  };
                  
                  table.on('click', 'td.toggle-icon', function() {
                  var td = $(this); 
                  var row = table.row(td.closest('tr'));
                  if (row.child.isShown()) {
                  row.child.hide();
                  td.removeClass(\"toggle-hide\");
                  td.addClass(\"toggle-show\");
                  td.html('Read full text');
                  } else {
                  row.child(format(row.data())).show();
                  td.removeClass(\"toggle-show\");
                  td.addClass(\"toggle-hide\");
                  td.html('Hide full text');
                  }
                  });"
          )
      )
    })


outputOptions(output, "substance_mammalian", suspendWhenHidden = FALSE)

## IV. in vitro tab ##

invitroTable <- reactive({
      invitro <- results$substance_tox_single()$invitro #[, .(hitcall = as.logical(max(hitcall), sourcedb = paste(sourcedb, collapse = ","))), by = .(assay, gene, ac50, species)]
      invitroTable <- invitro[order(ac50, species == "human", decreasing = TRUE)]
      invitroTable[ , ac50 := ifelse(hitcall, formatC(ac50*1e-6, format = "e", digits = 2), "(no hit)")]
      invitroTable
    })

output$substance_invitro <- 
    
    renderDataTable({
          
          validate(
              need(
                  !is.null(results$substance_tox_single()) && 
                      !is.null(results$substance_tox_single()$invitro) && 
                      nrow(results$substance_tox_single()$invitro)!=0, 'No data available')
          )	 
          
          uiTable(invitroTable(), columnConversions = c(), interactive = TRUE, scrollX = FALSE, filter = "top",
              columnSelection = c("assay", "gene", "ac50", "species", "sourcedb"),
              options = list(dom = '<"top">t<"bottom"ilp><"clear">')						 
          )
        })



outputOptions(output, "substance_invitro", suspendWhenHidden = FALSE)


## V. Phenotypes tab ##

output$substance_phenotypes <- renderDataTable({
      
      validate(
          need(
              !is.null(results$substance_tox_single()) && 
                  !is.null(results$substance_tox_single()$phenotypes) && 
                  nrow(results$substance_tox_single()$phenotypes)!=0, 'No data available')
      )	  
      
      
      tableToRender <- cbind(results$substance_tox_single()$phenotypes, 'Read full text'='Read full text')
      
      
      uiTable(tableToRender, interactive = TRUE, scrollX = FALSE, filter = "top",
          columnSelection = c("sourcedb", "phenotypename", "gene", "species", "details", "Read full text"),
          columnMap = c("sourcedb" = "Source DB", "phenotypename" = "Phenotype", 
              "gene" = "Gene", "species" = "Species", "details"= "Details", 
              "Read full text" = ""),
          options = list(dom = '<"top">t<"bottom"ilp><"clear">',
              columnDefs = list(
                  list(orderable = FALSE, className = 'toggle-icon toggle-show', targets = 5),
                  list(orderable = FALSE, 
                      className = 'td-parent-conclusions', 
                      targets = 4)
              ),
              rowCallback = JS("
                      function(row, data){
                      var full_text = data[4];
                      var max_chars = 35
                      if (data[4] == null) {
                      $('td:eq(5)', row).html('');
                      $('td:eq(5)', row).removeClass(\"toggle-icon toggle-show\");
                      } else {
                      if(full_text.length > max_chars) {
                      $('td:eq(4)', row).html(full_text.substring(0, 35) + '...');
                      }
                      }
                      }
                      ")),
          callback = JS("var format = function(data) {
                  if (data[4] !== null) {
                  return '<table class=\"invivo-nested-table\">' +
                  '<tr class=\"invivo-child-row\">' +
                  '<td class=\"td-before-conclusions\"></td>' +
                  '<td class=\"td-child-conclusions\">' + data[4] + '</td>' + 
                  '<td class=\"td-after-conclusions\"></td>' +
                  '</tr></table>';
                  }
                  };
                  
                  table.on('click', 'td.toggle-icon', function() {
                  var td = $(this); 
                  var row = table.row(td.closest('tr'));
                  if (row.child.isShown()) {
                  row.child.hide();
                  td.removeClass(\"toggle-hide\");
                  td.addClass(\"toggle-show\");
                  td.html('Read full text');
                  } else {
                  row.child(format(row.data())).show();
                  td.removeClass(\"toggle-show\");
                  td.addClass(\"toggle-hide\");
                  td.html('Hide full text');
                  }
                  });"
          )
      
      )
      
      
    })



outputOptions(output, "substance_phenotypes", suspendWhenHidden = FALSE)


## VI. Similar substances tab

output$substance_summary2 <- renderUI({
      
      toxdata <- results$substance_tox_cutoff()
      if (is.null(toxdata)) {
        tagList(tags$div(class = "no-data-table", 
                tags$div("No data available")))
      } else {
        
        tableToRender <- toxdata$activitySummary[!substanceid %in% results$selected_substance, ]
        
        tagList(
            
            tags$div(class = "information-table",
                
                
                renderDataTable(uiTable(tableToRender, interactive = TRUE, scrollX = FALSE, filter = "top",
                        columnSelection = c("name", "standardized_structure", "DART in vivo", "DART CLP", 
                            "invitrohitassaycount", "phenosummary", "similarity"),
                        columnMap = c("name" = "Name", "standardized_structure" = "Standardized structure", 
                            "DART in vivo" = "Regulatory studies", "DART CLP" = "CLP", 
                            "invitrohitassaycount" = "In vitro hits", "phenosummary" = "Phenotypes", 
                            "similarity" = "Similarity"),
                        options = list(dom = '<"top"><"filter"f>t<"bottom"ilp><"clear">',
                            columnDefs = list(
                                list(className = 'mytooltip', targets = 6)
                            
                            ),
                            initComplete = JS("function(settings, json) {
                                    $('thead > tr > th.mytooltip:nth-child(7)')[0].setAttribute( 'data-title', 
                                    'Tanimoto similarity of molecular fingerprints of constituents' );  
                                    $('th').tooltip(); 				
                                    }")
                        
                        )
                    ))
            ))
      }
      
    })

outputOptions(output, "substance_summary2", suspendWhenHidden = FALSE)


### Current substance in similar substances page

output$current_substance <- renderUI({
      
      toxdata <- results$substance_tox_single()
      if (is.null(toxdata))
        return(NULL)
      
      message <- NULL
      
      current_substance_data <- toxdata$activitySummary
      
      current_substance_data[, current_substance := paste0('current substance')]
      
      current_substance_data[, alternative_names := paste0('see 1 more name')]
      
      tagList(
          
          tags$div(class = "information-table",
              
              renderDataTable(uiTable(current_substance_data, interactive = TRUE, scrollX = FALSE, filter = "none",
                      columnSelection = c("name", "alternative_names", "standardized_structure", "DART in vivo", "DART CLP", 
                          "invitrohitassaycount", "phenosummary", "current_substance"),
                      columnMap = c("name" = "Name", "alternative_names"= "", "standardized_structure" = "Standardized structure", 
                          "DART in vivo" = "Regulatory studies", "DART CLP" = "CLP", 
                          "invitrohitassaycount" = "In vitro hits", "phenosummary" = "Phenotypes"),
                      options = list(
                          dom = '<"top">t<"bottom"><"clear">',
                          columnDefs = list(
                              list(orderable = FALSE, className = 'td-parent td-parent-name', targets = 0),
                              list(orderable = FALSE, className = 'td-parent details-control show-control', targets = 1)
                          ),
                          rowCallback = JS("
                                  function(row, data){
                                  if (typeof data[0] !== 'undefined') {
                                  var regexp = /br(?=>)/g;
                                  var brList = [...data[0].matchAll(regexp)];
                                  if (brList.length == 0){
                                  $('td:eq(1)', row).html('');
                                  $('td:eq(1)', row).removeClass(\"details-control show-control\");
                                  } else if (brList.length == 1){
                                  $('td:eq(1)', row).html('View 1 more name +');
                                  }
                                  else {
                                  $('td:eq(1)', row).html('View ' + brList.length + ' more names +');
                                  }
                                  } else {
                                  $('td:eq(1)', row).html('');
                                  $('td:eq(1)', row).removeClass(\"details-control show-control\");
                                  }
                                  }
                                  ")		
                      ),
                      callback = JS("
                              table.row(1).nodes().to$().css({cursor: 'pointer'});
                              var format = function(data) {
                              if (typeof data[0] !== 'undefined') {
                              var regexp = /br(?=>)/g;
                              var brList = [...data[0].matchAll(regexp)];
                              nBr = brList.length;
                              return '<table class=\"nested-table\">' +
                              '<tr class=\"child-row\">' + 
                              '<td class=\"td-child td-child-name\">' + data[0] + '</td>' +
                              '<td class=\"td-child td-rest\"></td></tr></table>';
                              }
                              };
                              
                              table.on('click', 'td.details-control', function() {
                              var td = $(this); 
                              var row = table.row(td.closest('tr'));
                              if (row.child.isShown()) {
                              row.child.hide();
                              td.removeClass(\"hide-control\");
                              td.addClass(\"show-control\");
                              if (nBr == 1) {
                              td.html('View 1 more name +');
                              } else {
                              td.html('View ' + nBr + ' more names +');
                              }
                              } else {
                              row.child(format(row.data())).show();
                              if (nBr == 1){
                              td.html('Hide 1 more name -');
                              } else {
                              td.html('Hide ' + nBr + ' more names -');
                              }
                              }
                              });"
                      ))
              )))
      
    })


outputOptions(output, "current_substance", suspendWhenHidden = FALSE)





