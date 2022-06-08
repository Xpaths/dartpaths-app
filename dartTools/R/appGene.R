# Project: DARTpaths_git
# 
# Author: mvarewyck
###############################################################################

# set names of columns to NULL to avoid R CMD check complain about "no visual binding"
# https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
remainder <- total <- species <- NULL

#' Helper function: Create link to webpage in datatable
#' @param webLink character, web address
#' @param label character, label shown in table
#' @return weblink refering to page \code{webLink} with label \code{description}
#' @author mvarewyck
#' @export
createLink <- function(webLink, label) {
  
  ifelse(is.na(webLink), label,
      paste('<a href=', webLink, 'target="_blank">', label, '</a>')) 
}

#' Helper function: Create actionLink to open modalDialog in datatable
#' @param species character, species for which to create link
#' @param row integer, row in datatable
#' @param label character, label shown in table
#' @param colors list, colors for the gene classes 
#' \code{c("one2one", "one2many", "many2many")} (optional)
#' @return actionLink with identifier including the \code{species} and table \code{row}
#' @author mvarewyck
createModalLink <- function(species, row, label, colors = list(
        "1:1" = "#bee126",    # green
        "1:N" = "#ffde17",   # yellow
        "N:N" = "#488df4;"   # blue
    )) {
  
  label <- as.character(label)
  
  # Red border if "manual" source
  colorBorder <- grepl("_manual", label)
  # Clean label
  label <- gsub("_manual", "", label)
  
  colorName <- label
  
  paste0("<button id='gene_", species, "_", row, 
      "' type='button' class='btn btn-default action-button shiny-bound-input gene-btn'",
      " style='background-color:", colors[[colorName]], 
      if (colorBorder) "; border-color: red; border-width:2px",
      "'>",
      label, "</button>")
  
}

#' Helper function: Create disabled button for 0 label items
#' @param label character, label shown in table
#' @return dedicated div
#' @export
createDisabledLink <- function(label) {
  label <- "0"
  paste0("<div class='disabled-gene-item'",
      " style='background-color: #f5f5f6",
      "'>",
      label, "</div>")
}


#' Create interactive table for gene data
#' @inheritParams createModalLink
#' @param geneData data.table object (as returned by \code{DartDB$getPathwayOrthology()})
#' @param columnNames character vector, column names for the interactive table
#' @param opts options to be passed to the \code{datatable} downstream
#' @return datatable, interactive version of \code{geneData}
#' @author mvarewyck
#' @importFrom DT datatable formatStyle styleEqual
#' @export
interactiveGeneTable <- function(geneData,
    colors = list(
        "1:1" = "#bee126",  # green
        "1:N" = "#ffde17",  # yellow
        "N:N" = "#488df4"   # blue
    ),
    opts = NULL,
    columnNames = NULL) {
  
  # To prevent warnings R CMD CHECK
  geneid <- NULL
  webLink <- NULL
  
  newData <- copy(geneData)
  
  # Link to web page of pathway
  newData[, c('geneid', 'webLink') := 
          list(createLink(webLink = webLink, label = geneid), NULL)]
  
  # Link to extra info orthology & compara
  speciesColumns <- setdiff(names(geneData),c("geneid","full name", "human","webLink"))
  
  for (species in speciesColumns) {
    
    newData <- newData[, (species) := factor(get(species), levels = c("0", "ortholog_one2one", "ortholog_one2many", "ortholog_many2many", "ortholog_one2one_manual", 
                "ortholog_one2many_manual", "ortholog_many2many"), 
            labels = c("0", "1:1", "1:N", "N:N", "1:1_manual", "1:N_manual", "N:N_manual"))]
    
    
    newData[, (species) := sapply(1:nrow(newData), function(row)
              if (newData[row, get(species)] %in% c("0", NA_character_))
                createDisabledLink() else
                createModalLink(species = species, row = row, label = newData[row, get(species)],
                    colors = colors)
        )]
    
  }
  
  interactiveTable(myData = newData, columnNames = columnNames, opts = opts)
  
}


#' Create summary table of gene data
#' @inheritParams interactiveGeneTable
#' @return table, number of genes of each class
#' \code{c("0", "ortholog_one2one", "ortholog_one2many", "ortholog_many2many")}
#' @importFrom data.table as.data.table setcolorder setnames dcast melt
#' @author mvarewyck
#' @export
summarizeGeneData <- function(geneData) {
  
  # To prevent warnings R CMD CHECK
  human <- NULL
  
  newData <- copy(geneData)
  
  # Remove weblink & full name
  newData[, c("full name", "webLink") := list(NULL, NULL)]
  
  # Adapt column human: some human genes are missing, 
  # but when in data there should be a one to one match 
  newData[, human := "ortholog_one2one"]
  
  # Wide to long data format
  longData <- melt(newData, id.vars = names(newData)[1], value.name = "class")
  setnames(longData, "variable", "species")
  
  # Count manual source per species
  longData[, source := grepl("_manual", class)]
  
  longData$class <- factor(gsub("_manual", "", longData$class),
      levels = c("ortholog_one2one", "ortholog_one2many", "ortholog_many2many", "0"))
  # Create table
  myTable <- table(longData[, c("species", "class")])
  
  # Count source
  sourceTable <- longData[ , paste(sum(source), "/", length(source[class != "0"])), by = species]
  setnames(sourceTable, "V1", "At least 1 manual ortholog")
  
  # as.data.table for table converts to long format
  # Long to wide data format
  myData <- dcast(as.data.table(myTable), species ~ class, value.var = "N")
  setcolorder(myData, neworder = c("species", rev(names(myData)[-1])))
  myData <- merge(myData, sourceTable, by = "species")
  
  # return species in correct order
  myData[names(newData)[-1], on = "species"]
  
}


#' Bar plot for genes
#' @param tableGenes table, as returned by \code{\link{summarizeGeneData}}
#' @inheritParams interactiveGeneTable
#' @param barWidth bar width (optional)
#' @param width plot width (optional)
#' @param height plot height (optional)
#' @return plotly, horizontal stacked barplot
#' @author mvarewyck
#' @import plotly
#' @export
plotGeneData <- function(tableGenes, 
    colors = list(
        "dartBlue" = "#488df4",
        "remainder" = "#488df4",
        "one2one" = "#bee126",    # green
        "one2many" = "#ffde17",   # yellow
        "many2many" = "#488df4"   # blue
    ),
    barWidth = NULL, width = 600, height = NULL){
  
  barWidth <- if (is.null(barWidth)) .4 else barWidth	# width of remainder; previously .3
  
  # To prevent warnings R CMD CHECK
  species <- NULL
  ortholog_one2one <- NULL
  ortholog_one2many <- NULL
  ortholog_many2many <- NULL
  
  plotData <- copy(tableGenes)
  
  plotData[, species := factor(species, levels = species)]	
  plotData[, total := (ortholog_one2one + ortholog_one2many + ortholog_many2many) ]
  plotData[, remainder := (max(total) - total) ]
  plotData <- plotData[species != "human", ]
  
  # Create barplot
  maxValue <- max(plotData[,'total'] + plotData[,'remainder'])
  props <- paste0(plotData$total, "/", maxValue)
  
  tickText <- paste0(plotData[,species], "  ", props, "  ")
  
  myPlot <- plot_ly(x = plotData[, total], y = plotData[, species], 
          type = 'bar', 
          name = "count", 
          width = width,		
          marker = list(color = "#488df4"),
          height = height,
          hoverinfo = "skip"
      ) %>%
      add_trace(x = plotData[, remainder], 
          name = "remainder", 
          width = barWidth,
          marker = list(color = "#488df4", opacity = 0.2)) %>%
      layout(barmode = 'stack',
          xaxis = list(title = "", zeroline = FALSE, fixedrange = TRUE),
          yaxis = list(title = "",
              fixedrange = TRUE,
              tickmode = "array",
              tickvals = seq_along(tickText)-1,
              ticktext = tickText),
          margin = list(l = 90),
          showlegend = FALSE,
          bargap = 0.6 # width of bars excepted remainder, previously 0.7
      )	%>% 
      config(displayModeBar = F) # remove option bar
  
  myPlot$elementId <- NULL
  
  myPlot
  
}

