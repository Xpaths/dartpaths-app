# Dartpaths ETL : QSARToolbox
# 
# Author: Marvin Steijaert
###############################################################################

## Query QSARToolbox database
# Make sure to have an instance of the QSARToolbox database running
# See also ETL/etl_scripts/qsar_toolbox/run_dockerized_qsardb.sh
qsarToolboxVersion <- "v22"
DBNAME <- switch(qsarToolboxVersion, v20 = "toolboxv20", v21 = "Toolboxv21R1", v22 = "toolboxv22")
USER <- "postgres"
if (!exists("PASSWORD")) PASSWORD <- "password123"
HOST <- "127.0.0.1"

getDbTable <- function(tablename, dbname= DBNAME, user = USER, password = PASSWORD, outputDir = NULL,
    select = NULL, where = NULL, host = HOST){
  con <- dbConnect(dbDriver("PostgreSQL"), dbname = dbname, user = user, password = password, host = host)
  this_table <- dbGetQuery(con, paste("SELECT * from", tablename, where))
  setDT(this_table)
  dbDisconnect(con)
  if(!is.null(select)){
    this_table <- this_table[, select, with = FALSE]
  }
  if(!is.null(outputDir)){
    fwrite(this_table, file.path(outputDir, paste0("qsartoolbox_", tablename,".txt")), sep ="\t")
  }
  return(invisible(this_table))
}
queryDatatable <- function(query, dbname= DBNAME, user = USER, password = PASSWORD, host = HOST){
  con <- dbConnect(dbDriver("PostgreSQL"), dbname = dbname, user = user, password = password, host = host)
  this_table <- dbGetQuery(con, query)
  setDT(this_table)
  dbDisconnect(con)
  return(this_table)
}


# QSARTOOLBOX substances query
query <- "
    select substance.id as qsardbsubstance,
    substance_type as substancetype,
    name,
    o_smiles as smiles,
    ec_number as ec
    from substance left join substance_name
    on substance.id = substance_name.substance_id
    left join name
    on name.id = name_id
    left join ec_number
    on ec_number.substance_id  = substance.id
    "
qsartoolboxSubstances <- queryDatatable(query)
if (qsarToolboxVersion %in% c("v20","v21")){
  # fix incorrecly formed EC numbers
  qsartoolboxSubstances[!is.na(ec), EC:= intToEC(ECtoInt(ec))] 
} else {
  qsartoolboxSubstances[nchar(ec)>7, ec:= NA] # incorrect EC number
  qsartoolboxSubstances[!is.na(ec), EC:= intToEC(ec)]
}

qsartoolboxSubstances[, ec:=NULL] 
qsartoolboxSubstances[,substancetype := sapply(substancetype,
        function(x) switch(x,
              "monoc" = "mono",
              "multic" = "multi",
              "unknown" = NA_character_,
              "uvcb" = "uvcb",
              "polymer" = NA_character_))]

# QSARTOOLBOX activity data query
# now only focus on mean_data, as that is used for 98% of the data
# later we may use postGIStools::get_postgis_query to directly query hstore elements from postgreSQL db in R
query = "
    select 
    substance_id as qsardbsubstance,
    meta_data -> 'Test organisms (species)' as species,
    endpoint_path.node as phenotypeclass,
    meta_data -> 'Endpoint' as effectdescriptor,
    mean_data as effectvalue,
    unit -> 'v' as effectunit,
    meta_data -> 'Year' as year,
    meta_data -> 'GLP compliance' as glp,
    meta_data -> 'Reliability' as reliability,
    meta_data -> 'Test guideline' as guideline,
    meta_data -> 'URL' as url,
    meta_data -> 'Conclusions' as conclusions,
    meta_data -> 'Details on results' as detailsonresults,
    measured_data. id as sourceid
    from measured_data left join endpoint_path on measured_data.endpoint_path_id = endpoint_path.id
    where
    node in ('Repeated Dose Toxicity','Developmental Toxicity / Teratogenicity','Toxicity to Reproduction')
    and
    meta_data -> 'Endpoint' in ('NOAEL','NOEL','LOEL','LOAEL','NOAEC','LOAEC','NOEC','LOEC','NOAEDD','NOEDD','NOEL calculated')
    and
    mean_data is not null
    "
qsartoolboxMeasuredData = queryDatatable(query)
qsartoolboxMeasuredData[, guideline := gsub("\\s*\\(.*\\)\\s*", "", guideline)]
qsartoolboxMeasuredData[, sourcedb := "qsartoolbox"]
qsartoolboxMeasuredData[, glp := ifelse(grepl("^yes",tolower(glp)), TRUE, FALSE)]

# only add substances with activity data or EC
# to reduce size of database and time for standardization
database$addIdTypes("qsardbsubstance")
qsardbSubstanceSelection <- qsartoolboxSubstances[qsardbsubstance %in% qsartoolboxMeasuredData[,qsardbsubstance] | !is.na(EC),
    unique(qsardbsubstance)]
tmp <- database$addSubstances(qsartoolboxSubstances[qsardbsubstance %in% qsardbSubstanceSelection], verbose = TRUE, returnTable = TRUE) # 12 min
database$addSubstanceActivity(qsartoolboxMeasuredData, "qsardbsubstance", type = "invivo")
