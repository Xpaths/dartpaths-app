# database schema
# lowercase column names (singular)
# lowercase table names (plural)
# limit the use of underscores
DBTABLECOLUMNS = list(
    substancetypes = c("substanceid", "substancetype"), #one2one
    substanceconstituents = c("substanceid", "constituentid"), #one2many
    constituents = c("constituentid", "smiles"), # one2one # inchikey and smilesorig not included (may later be added for debugging purposes)
    externalsubstanceids = c("substanceid","idtype","idvalue","url") # one2many # Note: in current version, this includes names
    , substancecategorysubstances = c("substanceid","category")
    , substancecategoryinfo = c("category","description","url") 
    , substancecategoryprofiles = c("category", "name", "substanceid", "typicalconcentration", "concentrationrange")
    ,substanceinvivo = c("substanceid", 
        "externalsubstanceidtype", "externalsubstanceid",
        "species", "phenotypeclass",  
        "effectdescriptor","effectvalue", "effectunit", "year",
        "glp", "reliability", "guideline", "url", "sourcedb", "sourceid",
        "conclusions", "detailsonresults")
    ,substancephenotypes = c(
        "substanceid", "externalsubstanceidtype", "externalsubstanceid",
        "species", "variantid", "variantname",
        "conditions", "phenotypeid", "phenotypename",
        "gene", "details",
        "url", "sourcedb", "sourceid")
    ,substanceinvitro = c("substanceid", "externalsubstanceidtype", "externalsubstanceid",
        "assay","hitcall", "ac50", "species",
        "biologicalprocess","gene", "url", "sourcedb")
    ,substanceclp = c("substanceid", "externalsubstanceidtype", "externalsubstanceid", "hazardcode") 
    ,constituentdescriptormorgan = c("constituentid", paste0("fp", 0:1023))   
    ,ensembl2reactome = c("source_database_identifier", "reactome_pathway_stable_identifier", "url",
        "event_name", "evidence_code", "species", "lowest")
    ,pathwayrelations = c("reactome_pathway_parent", "reactome_pathway_child")
    ,genepheno = c("db_object_id", "db_object_symbol", "go_id_name", "go_id",
        "taxon", "freq", "ic", "icweight")
    ,orthology = c("geneid", "protein_or_transcriptid", "homologytype", "speciesfull", "homolog_geneid", "homolog_protein_or_transcriptid", "homolog_gene", "percent_identical", "confidence")
    ,humangenes = c("gene", "genelongname", "geneid")
    ,species = c("speciesfull", "taxon", "speciescommon", "speciesshort")
    ,pathwayphenotypes = c("reactome_pathway_stable_identifier", "species", "phenotypeid", "phenotypename", "pvalue", "adjusted_pvalue", "overlapping_genes")
    ,pathwaysimulations = c("reactome_pathway_stable_identifier", "n_phenotypes", "ontology", "n_runs", "rank_percent_list")
)

#' Auxialiary function to split character strings to numeric vectors 
#' @param vec Vector with strings
#' @param split Character string used for splitting (regular expressions are not supported).
#' @return list with character values
#' @author Marvin Steijaert
#' @details can be used as a workaround until data.table::fread supports sep2
#' see also: https://github.com/Rdatatable/data.table/issues/1162
splitToNumericVectors <- function(vec, split = "|"){
  lapply(vec, function(y) as.double(strsplit(y, split = split, fixed = TRUE)[[1]]))
}

COLUMNCONVERSIONFUNCTIONS = c(
    substanceid = as.integer, # force to load substanceid as integer in all tables
    rank_percent_list = splitToNumericVectors # convert "|"-separated strings to numeric vectors
)

# Definition of column names
# cat(paste0("# ", sort(unique(unlist(database$tableColumns))),": ", collapse = "\n"))
# ac50: 
# assay: 
# biologicalprocess: 
# category: 
# concentrationrange: 
# conclusions: free text/html
# conditions: 
# confidence: confidence level
# constituentid: 
# db_object_id: 
# db_object_symbol: 
# description: 
# details: 
# detailsonresults:  free text/html
# effectdescriptor: 
# effectunit: 
# effectvalue: 
# event_name: 
# evidence_code: 
# externalsubstanceidtype: type of "external" substance identifier
# externalsubstanceid: value of "external" substance identifier (with type in externalsubstanceidtype column)
# fp0 - fp1023: chemical fingerprints
# freq: 
# gene: official gene symbol
# genelongname: long name of gene
# geneid: Ensembl stable gene id
# glp: 
# go_id: 
# go_id_name: 
# guideline: 
# hazardcode: 
# hitcall: 
# homolog_gene: 
# homolog_geneid: 
# homologytype: 
# ic: 
# icweight: 
# idtype: 
# idvalue: 
# lowest: logical, indicates if pathway is lowest level for gene
# name: substance name OR category name # TODO rename the latter to categoryname?
# n_phenotypes: number of phenotypes in query
# n_runs: number Monte Carlo simulations
# ontology: name/identifier of ontology (e.g. "MP", "WBPhenotype", "ZP")
# percent_identical: 
# phenotypeclass: 
# phenotypeid: identifier of phenotype
# phenotypename: name of phenotype
# pvalue: p-value of hypergeometric test
# adjusted_pvalue: p-value of hypergeometric test after FDR correction
# rank_percent_list: list-column with vectors with rank_percent values in Monte Carlo simulations
# reactome_pathway_stable_identifier: reactome pathway identifier 
# reactome_pathway_parent: reactome pathway identifier
# reactome_pathway_child: reactome pathway identifier
# reliability: 
# smiles: SMILES strings describing molecular structures
# source_database_identifier: 
# sourcedb: name of original database
# sourceid: identifier from original database
# species: AMBIGUOUS. Can match to one of the following species* fields
# speciescommon: common species name
# speciesfull: full scientific species name(e.g. "Homo sapiens")
# speciesshort: short scientific notation with capital and space (e.g. "H. sapiens")
# substanceid: (non-persistent) internal substance identifier
# substancetype: type of substance
# taxon: NCBI taxonomy id
# protein_or_transcriptid: Human Ensembl stable protein or transcript id 
# homolog_protein_or_transcriptid: Homolog (non-human) Ensembl stable protein or transcript id
# typicalconcentration: 
# url: url
# variantid: 
# variantname: 
# year: year


#' Class for storing DARTpaths data
#' 
#' @export
#' @import R6
#' @details For reasons of code visibility, the different components are implemented in a series of parent classes
DartDB <- R6Class("DartDB",
    inherit = PhenoPathway,
    
    public = list(
        #' @field classname Name of the class
        classname = "DartDB",
        #' @field tableColumns Named list with names of all the columns for each table
        tableColumns = DBTABLECOLUMNS,
        #' @field columnConversionFunctions Named vector with functions (e.g. as.character) that applied after loading a columns with that name in any of the tables
        columnConversionFunctions = COLUMNCONVERSIONFUNCTIONS
    ))
