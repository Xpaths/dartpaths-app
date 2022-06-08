# Project: DARTpaths_git
# 
# Author: mvarewyck
###############################################################################


## ----------------- ##
##    Compound Query ##
## ----------------- ##

# set choices after loading database
updateSelectInput(session,
    inputId = "category_selection_constituents",
    choices = c("e.g. LOA category A" = "", database$tables$substancecategoryprofiles[,sort(unique(category))])
)
updateSelectInput(session,
    inputId = "category_selection_members",
    choices = c("e.g. BITUMENS" = "", database$tables$substancecategorysubstances[,sort(unique(category))])
)


# Category query
results$category_query <- eventReactive({list(input$category_search_members, input$category_search_constituents) ; if(input$category_search_members>0 || input$category_search_constituents > 0) input$category_search_type}, {
      
      if (is.null(input$category_search_type))
        return(NULL)
      
      if(input$category_search_type == "category_constituents"){
        mySearch <- input$category_selection_constituents
        searchTable <- database$tables$substancecategoryprofiles
      } else if (input$category_search_type == "category_members"){
        mySearch <- input$category_selection_members
        searchTable <- database$tables$substancecategorysubstances
      } else stop("undefined category_search_type")
      
      tryCatch({
            res <- searchTable[category %in% mySearch]
            substanceInfo <- database$tabulateSubstanceInfo(res$substanceid, web = TRUE, limitOutput = TRUE)
            if(input$category_search_type == "category_constituents") substanceInfo[, name := NULL]
            substanceInfo[, smiles := NULL]
            if("qsardbsubstance" %in% names(substanceInfo)) substanceInfo[, qsardbsubstance := NULL]
            res[, category := NULL]
            res <- merge(res,
                substanceInfo,
                by = "substanceid")
            return(res)
          },
          error = function(error) error
      )
    })


output$category_helptext <- renderUI({
			
			isError <- is(results$category_query(), "error")
			
			validate(need(!isError,
							if(isError) return(NULL)))
			
			tags$div(class = "substance-helptext", helpText("Click on a row to proceed."))
		})



# Summary of compound query
output$category_searchResult <- renderDataTable({
      
      validate(need(!is(results$category_query(), "error"), 
              results$category_query()$message))
      
      # Invalidate when changing search type
      input$category_search_type
      
      myData <- results$category_query()
      
      uiTable(myData, interactive = FALSE, scrollX = FALSE,
          options = list(dom = '<"top"><"filter"f>t<"bottom"ilp><"clear">',
				  ordering = FALSE)
      )   
    })



## ---------------- ##
##    Toxicity tabs ##
## ---------------- ##

## get the id of the category

results$category_filter_single <- reactive({
      
#      validate(need(!"error" %in% class(results$category_query()), 
#              results$category_query()$message))	
			
      # Selected query substance from table
      substanceSelection <- results$category_query()$substanceid[input$category_searchResult_rows_selected]
      if(is.null(substanceSelection) || length(substanceSelection)==0) return(NULL)
      
      if(input$category_search_type == "category_constituents"){
        # set priorityPattern to ensure that the name of the category constituent
        # is also listed first on the overview page
        attr(substanceSelection,"priorityField") <- "name"
        attr(substanceSelection,"priorityPattern") <- tolower(results$category_query()$name[input$category_searchResult_rows_selected])
      }
      cat("selected substance (category search):",substanceSelection,"\n")
      return(substanceSelection)
    })
