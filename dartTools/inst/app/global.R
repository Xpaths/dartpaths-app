
## A. Loading Packages ##
## ------------------- ##

library(dartTools)
library(shiny.router)

## Load R6 database
dumpdir <- getOption("dartpaths_default_dumppath")
smilescache <- getOption("dartpaths_smilescache")
if(!exists("database")){
  database <- DartDB$new()
  database$loadDump(dumpdir)
}

library(DT)                 # for customizing tables

library(shinyjs)            # for several features: de-activate button, scrolldown

library(openxlsx)           # for reading data files

library(plotly)             # for interactive plots

library(data.table)         # for data.table objects

library(fontawesome)

library("rlist") # use list.select and list.stack to filter columns and create a tibble respectively

library("shinyWidgets") # for widgets

library("shinycssloaders") # for loading animation


## B. General Functions ##
## -------------------- ##

#`%then%` <- shiny:::`%OR%`
#onStop(function() {
#      if (file.exists(".RDuetConsole"))
#        file.remove(".RDuetConsole")
#    })

#jsScroll <- "shinyjs.scrollTo = function(params){$('html, body').animate({scrollTop: $(params['id']).offset().top}, 2000);}"




## C. General Parameters ##
## --------------------- ##


# Rmd report, ready to use on shinyServer
tmpDir <- tempdir()

# Directory with data for the app
dataDir <- system.file("extdata", package = "dartTools")

# Gene colors
geneColors <- list(
    "1:1" = "#BEE126",    # green
    "1:N" = "#FFDE17",   # yellow
    "N:N" = "#488df4"   # blue
)

# Options related to for pathway levels
options("dartpaths_app_pathway_ranking_min_genes" = 4)
options("dartpaths_app_pathway_ranking_max_genes" = 200)
options("dartpaths_app_pathway_ranking_levels" = 3:database$getHumanPathwayLevels()[,max(level)] ) # include all levels >=3 in calculation
options("dartpaths_app_pathway_ranking_lowest_level" = FALSE) # only lowest level annotations in pathway ranking

options("dartpaths_app_pathway_orthology_min_genes" = 4)
options("dartpaths_app_pathway_orthology_max_genes" = 200)
options("dartpaths_app_pathway_orthology_levels" = 3:database$getHumanPathwayLevels()[,max(level)])
options("dartpaths_app_pathway_orthology_lowest_level" = FALSE) # show all pathway genes in orthology page

if (length(setdiff(
        getOption("dartpaths_app_pathway_ranking_levels"),
        getOption("dartpaths_app_pathway_orthology_levels")))){
  warning("Not all pathways in ranking are shown on orthology page (please revise level selection).")
}

## D. Debug ##
## -------- ##

# for fast debugging
#if (!exists("doDebug"))
#  doDebug <- FALSE


## E. Switch on/off tabs that are in development ##
## --------------------------------------------- ##
devApp <- getOption("dart_dev_app", default = FALSE)


## F. Source Widgets ##
## ----------------------------- ##

sourceFiles <- list.files(path = "widgets", full.names = TRUE)
for (iFile in sourceFiles)
  source(iFile)

## G. UI Files with htmlTemplate ##
## ----------------------------- ##


source(file = file.path("uiFiles", "landing_page.R"), local = TRUE)
source(file = file.path("uiFiles", "substance_search_page.R"), local = TRUE)
source(file = file.path("uiFiles", "category_search_page.R"), local = TRUE)
source(file = file.path("uiFiles", "pathway_search_page.R"), local = TRUE)
source(file = file.path("uiFiles", "about_page.R"), local = TRUE)
source(file = file.path("uiFiles", "substance_information_page.R"), local = TRUE)


## H. router ##
## --------- ##

router <- make_router(
    route("/", landing_page),
    route("category-search", category_search_page),
    route("substance-search", substance_search_page),
    route("pathway-search", pathway_search_page),
    route("about", about_page),
    route("substance-information", substance_information_page)
)
