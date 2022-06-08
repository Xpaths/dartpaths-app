# Project: DARTpaths_git
# 
# Author: mvarewyck
###############################################################################

# set names of columns to NULL to avoid R CMD check complain about "no visual binding"
# https://www.r-bloggers.com/2019/08/no-visible-binding-for-global-variable/
effectunit <- effectvalue <- effect <- `standardized_structure` <- `substancetype` <- NULL
`Conclusions` <- `Details on results` <- `Details` <- NULL

`.` <- list

#' Create interactive table for UI
#' @param myData data.table
#' @param columnNames character vector, column names for the interactive table
#' @param opts additional options to pass to \code{datatable} e.g. for row callbacks
#' @return datatable, interactive version of \code{myData}
#' @author mvarewyck
#' @importFrom DT datatable
#' @export
interactiveTable <- function(myData, columnNames = NULL, opts) {
  
  allOptions <- c(opts, 
      list(autoWidth = TRUE, dom = 'Bltirp', 
          lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
          buttons = I('colvis')
      ))
  
  datatable(myData, rownames = FALSE, 
      colnames = if (is.null(columnNames)) names(myData) else columnNames,
      escape = FALSE, # for rendering HTML links
      filter = list(position = "top", clear = TRUE), 
      #    extensions = 'Buttons', 
      extensions = c('FixedHeader', 'Buttons'),
      options = allOptions,
      selection = "none")
  
}

# Mapping from column names in database to column names in app
DEFAULTCOLUMNMAP = c(name = "Name", EC = "EC", CAS = "CAS",
    #substancetype = "Substance Type",
    standardized_structure = "Standardized structure",
    SMILES = "SMILES", sourcedb = "Source DB",
    #`DART in vivo` = "DART in vivo",
    `DART in vivo` = "Regulatory studies",	
    `DART CLP` = "DART CLP",
    invitrotargetnames = "In vitro targets",	
    invitrohitassaycount = "In vitro hits",
    phenotypes = "Phenotypes",
    phenotypename = "Phenotype",
    phenotypeid = "Phenotype identifier",
    pvalue = "p-Value",
    adjusted_pvalue = "Adjusted p-value",
    assay = "Assay",
    gene = "Gene",
    hitcall = "Hit call",
    ac50 = "AC50 (M)",#"AC50 (μM)",
    biologicalprocess = "Biological process",
    details = "Details",
    dart = "DART active", 
    species = "Species",
    phenotypeclass = "Phenotype class",
    effectdescriptor = "Effect Descriptor",
    effect = "Effect",
    year = "Year",
    glp = "GLP",
    #reliability = "Reliability", # not shown. TODO: make sure to keep only reliable records
    detailsonresults = "Details on results",
    conclusions = "Conclusions",
    guideline = "Guideline",
    sourcedb = "Source database",
    typicalconcentration = "Typical concentration",
    concentrationrange = "Concentration range",
    similarity = "Similarity"
)


# COLUMNCONVERSIONS is a named vector with functions to convert values in displayed table
COLUMNCONVERSIONS = c(
    ac50 = function(x) formatC(x*1e-6, format = "e", digits = 2), # molar instead of micromolar
    pvalue = function(x) formatC(x, format = "e", digits = 2), # scientific notation with limited number of digits
    adjusted_pvalue = function(x) formatC(x, format = "e", digits = 2)  # scientific notation with limited number of digits
)

#' Create (interactive) table for UI in a unified format
#' @param myData data.table
#' @param columnMap named character vector mapping original column names to displayed column names
#' @param columnSelection columns that should be displayed (if available)
#' @param columnConversions named vector with functions to convert values in displayed table
#' @param interactive indicates wether the datatable should be interactive
#' @param escape passed to \code{datatable()}
#' @param scrollX passed to \code{datatable()}
#' @param filter passed to \code{datatable()}
#' @param container passed to \code{datatable()}
#' @param options passed to \code{datatable()}
#' @param extensions passed to \code{datatable()}
#' @param selection passed to \code{datatable()}
#' @param callback passed to \code{datatable()}
#' @return datatable, interactive version of \code{myData}
#' @details Unified format: same column names and same column order, hide non-informative columns 
#' @author Marvin Steijaert
#' @importFrom DT datatable
#' @importFrom htmlwidgets JS
#' @import data.table
#' @export
uiTable <- function(myData,
    columnMap = DEFAULTCOLUMNMAP,
    columnSelection = names(columnMap),
    columnConversions = COLUMNCONVERSIONS,
    interactive = FALSE,
    escape = FALSE,
    scrollX = TRUE, 
    filter = "none",
    container = NULL,
    options = NULL,
    extensions = NULL,
    selection = "single",
    callback = JS("return table;")
) {
  
  if(is.null(myData)) return(NULL)
  
  myData = copy(myData)
  
  # apply columnConversions
  if(!is.null(columnConversions)){
    for (thisColumn in intersect(names(myData), names(columnConversions))){
      set(myData, j = thisColumn, value = columnConversions[[thisColumn]](myData[[thisColumn]]))
    }
  }
  
  
  # special rules (may need to be exposed or moved)
  # 1. combine effectvalue and effectunit to save space 
  if(all(c("effectvalue", "effectunit") %in% names(myData))){
    myData[, effectunit := gsub('"','', effectunit)] # remove unneeded quotes
    myData[!is.na(effectvalue) & !is.na(effectunit), effect:= paste(effectvalue, effectunit)]
  }
  # 2. show "UVCB" in standardized_structure column
  if(all(c("standardized_structure", "substancetype") %in% names(myData))){
    myData[is.na(standardized_structure) & substancetype %in% "uvcb", standardized_structure := "(UVCB)"]
    myData[is.na(standardized_structure) & substancetype %in% "multi", standardized_structure := "(multi-constituent substance)"]
    myData[is.na(standardized_structure), standardized_structure := "(no structure available)"]
  }
  
  # select relevant columns and change internal column names to user-friendly variants
  selectedColumns <- intersect(columnSelection, names(myData))
  myData <- unique(myData[, selectedColumns, with = FALSE])
  newColumnNames <- columnMap[selectedColumns]
  newColumnNames <- ifelse(is.na(newColumnNames),selectedColumns,newColumnNames)
  
  if(!is.null(myData) & !is.null(columnMap)) setnames(myData, selectedColumns, newColumnNames)
  
  if("Conclusions" %in% names(myData)){
    myData[!is.na(Conclusions),
        Conclusions:= sapply(Conclusions,
            function(x) paste(strwrap(x, width = 100), collapse ="\n"))]
    myData[, Conclusions := gsub("\n", " ",Conclusions)]
  }
  if("Details on results" %in% names(myData)){
    myData[!is.na(`Details on results`),
        `Details on results`:= sapply(`Details on results`,
            function(x) paste(strwrap(x, width = 100), collapse ="\n"))]
    myData[, `Details on results` := gsub("\n", " ",`Details on results`)]
  }
  if("Details" %in% names(myData)){
    myData[!is.na(Details),
        Details:= sapply(Details,
            function(x) paste(strwrap(x, width = 100), collapse ="\n"))]
    myData[, Details := gsub("\n", " ",Details)]
  }
  
  
  
  if(interactive){
    
    if (is.null(container)){
      
      datatable(myData,
          rownames = FALSE, 
          colnames = names(myData),
          escape = FALSE, # for rendering HTML links
          filter = filter, 
          extensions = if (is.null(extensions)) list() else extensions, 
          options = c(options, list(autoWidth = TRUE,  
                  dom = '<"top">t<"bottom"ilp><"clear">',
                  lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
                  buttons = I('colvis'),
                  scrollX = scrollX
              )),
          callback = callback,
          selection = selection)
      
      
    } else {
      
      datatable(myData,
          rownames = FALSE, 
          container = container, 
          colnames = names(myData),
          escape = FALSE, # for rendering HTML links
          filter = filter, 
          extensions = if (is.null(extensions)) list() else extensions, 
          options = c(options, list(autoWidth = TRUE,  
                  dom = '<"top">t<"bottom"ilp><"clear">',
                  lengthMenu = list(c(10, 50, -1), c("10", "50", "All")),
                  buttons = I('colvis'),
                  scrollX = scrollX
              )),
          callback = callback,
          selection = selection)
    }
    
  } else {
    datatable(myData,
        rownames = FALSE,
        escape = FALSE,
        filter = filter,
        selection = selection,
        extensions = if (is.null(extensions)) list() else extensions, 
        options = c(options, list(dom = '<"top">t<"bottom"ilp><"clear">',	
                autoWidth = TRUE,
                scrollX = scrollX)
        ),
        callback = callback
    )
    
  }
}

#' Create a colored bar indicating percentages
#' @param nLeft Number of left color counts
#' @param nRight Number of right color counts
#' @param colorOrder Order of colors. Available options: "magentagreen"(default), "yellowblue", "redgreen", "greenred"
#' @return string with html img element containing base64 encoded image
#' @importFrom base64enc dataURI
#' @importFrom png writePNG
#' @author Marvin Steijaert
#' @export
#' @examples \dontrun{createBarImg(50,50)}
createBarImg <- function(nLeft, nRight, colorOrder = "magentagreen"){
  
  if(length(nRight) == 0 || length(nLeft) == 0 || !is.finite(nRight) || !is.finite(nLeft)){
    # warning("createBarImg received invalid arguments")
    return("")
  }
  if(nRight == 0 && nLeft ==0){
    return("")
  }
  if(length(nRight)>1 || length(nLeft)>1){
    return(mapply(createBarImg, nRight, nLeft))
  }
  
  pRight <- round(100 * nRight/(nRight + nLeft))
  pLeft <- 100 - pRight
  ysize <- 15
  
  imgArray <- switch(colorOrder,
      magentagreen = array(c(
              matrix(c(rep(0.917647059,ysize*pLeft),rep(0.745098039,ysize*pRight)), nrow = ysize),
              matrix(c(rep(0.235294118,ysize*pLeft),rep(0.882352941,ysize*pRight)), nrow = ysize),
              matrix(c(rep(0.643137255,ysize*pLeft),rep(0.149019608,ysize*pRight)), nrow = ysize)			
          ),dim = c(ysize,100,3)),
      redgreen = array(c(
              matrix(c(rep(1,ysize*pLeft),rep(0,ysize*pRight)), nrow = ysize),
              matrix(c(rep(0,ysize*pLeft),rep(1,ysize*pRight)), nrow = ysize),
              matrix(c(rep(0,ysize*pLeft),rep(0,ysize*pRight)), nrow = ysize)
          ),dim = c(ysize,100,3)),
      yellowblue = array(c(
              matrix(c(rep(1,ysize*pLeft),rep(0,ysize*pRight)), nrow = ysize),
              matrix(c(rep(1,ysize*pLeft),rep(0,ysize*pRight)), nrow = ysize),
              matrix(c(rep(0,ysize*pLeft),rep(1,ysize*pRight)), nrow = ysize)
          ),dim = c(ysize,100,3)),
      greenred = array(c(
              matrix(c(rep(0,ysize*pLeft),rep(1,ysize*pRight)), nrow = ysize),
              matrix(c(rep(1,ysize*pLeft),rep(0,ysize*pRight)), nrow = ysize),
              matrix(c(rep(0,ysize*pLeft),rep(0,ysize*pRight)), nrow = ysize)
          ),dim = c(ysize,100,3)),
      blueyellow = array(c(
              matrix(c(rep(0,ysize*pLeft),rep(1,ysize*pRight)), nrow = ysize),
              matrix(c(rep(0,ysize*pLeft),rep(1,ysize*pRight)), nrow = ysize),
              matrix(c(rep(1,ysize*pLeft),rep(0,ysize*pRight)), nrow = ysize)
          ),dim = c(ysize,100,3))
  )
  
  # to test output: writePNG(imgArray,"~/tmp.png")
  paste0('<img src="',
      dataURI(writePNG(imgArray), "image/png"),
      '"></img>')
}

