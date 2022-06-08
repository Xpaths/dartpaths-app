if (paste(R.version[c("major","minor")], collapse = ".") < "4.2"){
  `|>` = magrittr::`%>%`
}

#' Download raw .obo ontology files
#'
#' @return parameter
#' @author Monique van der Voet
#' @export
#' @examples
#' \dontrun{
#' DownloadOntologies()
#' }
#'
DownloadOntologies <- function(rawDataDir = "data-raw/extractTransform", robotCommand = "robot") {
    
    # Instructions converting owl to obo ------------------------------------------------------------------------------
    # Some data needs to be transform from owl to obo format
    # Before getting started:
    # information on convert owl to obo: http://robot.obolibrary.org/
    # requires Java Development Kit (JDK): https://www.oracle.com/technetwork/java/javase/downloads/index.html
    # requires Java 8, in command line: java -version
    
    
    # Gene Ontology ---------------------------------------------------------------------------------------------------
    download.file(
        "http://purl.obolibrary.org/obo/go.obo",
        file.path(rawDataDir, "go.obo"),
        method = "auto"
    )
    
    
    # PATO ------------------------------------------------------------------------------------------------------------
    download.file(
        "http://purl.obolibrary.org/obo/pato.obo",
        file.path(rawDataDir, "pato.obo"),
        method = "auto"
    )
    
    
    # Dicty -----------------------------------------------------------------------------------------------------------
    
    # Phenotype
    download.file(
        "http://purl.obolibrary.org/obo/ddpheno.obo",
        file.path(rawDataDir, "ddpheno.obo"),
        method = "auto"
    )
    
    # Anatomy
    # download.file(
    #   "http://purl.obolibrary.org/obo/ddanat.obo",
    #   file.path(rawDataDir, "ddanat.obo"),
    #   method = "auto"
    # )
    
    
    # Elegans ---------------------------------------------------------------------------------------------------------
    
    # Phenotype
    download.file(
        "http://purl.obolibrary.org/obo/wbphenotype.obo",
        file.path(rawDataDir, "wbphenotype.obo"),
        method = "auto"
    )
    
    # Development
    # download.file(
    #   "http://purl.obolibrary.org/obo/wbls.obo",
    #   file.path(rawDataDir, "wbls.obo"),
    #   method = "auto"
    # )
    
    # Gross Anatomy
    # download.file(
    #   "http://purl.obolibrary.org/obo/wbbt.obo",
    #   file.path(rawDataDir, "wbbt.obo"),
    #   method = "auto"
    # )
    
    
    # Zebrafish -------------------------------------------------------------------------------------------------------
    
    # Phenotype
    # latest version (2021-12-12) on "http://purl.obolibrary.org/obo/zp.obo"
    # hangs when reading with ontologyIndex::get_ontology
    # so explicitly using v2020-08-02
    download.file(
       "https://github.com/obophenotype/zebrafish-phenotype-ontology/releases/download/v2020-08-02/zp.obo",
       file.path(rawDataDir, "zp.obo"),
       method = "auto"
    )
    
    # Developmental stages
    # download.file(
    #   "http://purl.obolibrary.org/obo/zfs.obo",
    #   file.path(rawDataDir, "zfs.obo"),
    #   method = "auto"
    # )
    
    # Anatomy and development
    # download.file(
    #   "http://purl.obolibrary.org/obo/zfa.obo",
    #   file.path(rawDataDir, "zfa.obo"),
    #   method = "auto"
    # )
    
    # Experimental conditions
    # download.file(
    #   "http://purl.obolibrary.org/obo/zeco.owl",
    #   file.path(rawDataDir, "zeco.owl"),
    #   method = "auto"
    # )
    # system(
    #   "robot convert --input data-raw/extractTransform/zeco.owl --format obo --output data-raw/extractTransform/zeco.obo"
    # )
    # system(
    #     paste(robotCommand, "convert --input", file.path(rawDataDir, "zeco.owl"),
    #         "--format obo --output", file.path(rawDataDir, "zeco.obo"))
    # )
    # file.remove(file.path(rawDataDir, "zeco.owl")
    
    
    # Mouse -----------------------------------------------------------------------------------------------------------
    
    # For developmental timeline:
    
    # Developmental stages
    download.file(
        "http://purl.obolibrary.org/obo/mmusdv.obo",
        file.path(rawDataDir, "mmusdv.obo"),
        method = "auto"
    )
    
    # Gross anatomy and development, timed (abstract)
    download.file(
        "http://purl.obolibrary.org/obo/emapa.obo",
        file.path(rawDataDir, "emapa.obo"),
        method = "auto"
    )
    
    # For appSelectizeLookup:
    
    # Pathology
    download.file(
        "http://purl.obolibrary.org/obo/mpath.owl",
        file.path(rawDataDir, "mpath.owl"),
        method = "auto"
    )
    system(
        paste(robotCommand, "convert --input", file.path(rawDataDir, "mpath.owl"),
            "--format obo --output", file.path(rawDataDir, "mpath.obo"))
    )
    file.remove(file.path(rawDataDir, "mpath.owl"))
    
    # Adult gross anatomy
    # download.file("http://purl.obolibrary.org/obo/ma.owl",
    #        file.path(rawDataDir, "ma.owl"),
    #        method = "auto")
    # system(
    #     paste(robotCommand, "convert --input", file.path(rawDataDir, "ma.owl"),
    #        "--format obo --output", file.path(rawDataDir, "ma.obo"))
    # )
    # file.remove(file.path(rawDataDir, "ma.owl")
    
    
    # Mammalian -------------------------------------------------------------------------------------------------------
    
    # Mammalian Phenotype
    download.file(
        "http://purl.obolibrary.org/obo/mp.obo",
        file.path(rawDataDir, "mp.obo"),
        method = "auto"
    )
    
    # Uber-anatomy
    # download.file(
    #   "http://purl.obolibrary.org/obo/uberon.owl",
    #   file.path(rawDataDir, "uberon.owl"),
    #   method = "auto"
    # )
    # system(
    #     paste(robotCommand, "convert --input", file.path(rawDataDir, "uberon.owl"),
    #        "--format obo --output", file.path(rawDataDir, "uberon.obo"))
    # )
    # file.remove(file.path(rawDataDir, "uberon.owl")
    
    # Unified phenotype -----------------------------------------------------------------------------------------------------------
    # download.file(
    #   "https://raw.githubusercontent.com/obophenotype/upheno-dev/master/upheno.obo", # old?
    #   #"https://github.com/obophenotype/upheno/blob/master/upheno.owl",
    #   file.path(rawDataDir, "upheno.obo"),
    #   method = "auto"
    # )
    # system(
    #     paste(robotCommand, "convert --input", file.path(rawDataDir, "upheno.owl"),
    #        "--format obo --output", file.path(rawDataDir, "upheno.obo"))
    # )
    # # file.remove(file.path(rawDataDir, "upheno.owl")
    
    
    # Human -----------------------------------------------------------------------------------------------------------
    
    # Phenotype
    download.file(
        "http://purl.obolibrary.org/obo/hp.obo",
        file.path(rawDataDir, "hp.obo"),
        method = "auto"
    )
    
    # Disease
    download.file(
        "http://purl.obolibrary.org/obo/doid.obo",
        file.path(rawDataDir, "doid.obo"),
        method = "auto"
    )
    
    # Developmental anatomy, abstract
    # download.file(
    #   "http://purl.obolibrary.org/obo/ehdaa2.obo",
    #   file.path(rawDataDir, "ehdaa2.obo"),
    #   method = "auto"
    # )
    
    # Human developmental stages
    # download.file(
    #   "http://purl.obolibrary.org/obo/hsapdv.obo",
    #   file.path(rawDataDir, "hsapdv.obo"),
    #   method = "auto"
    # )
    
}


#' Process Ontologies to create a lookup table listing all ontology terms
#'
#' @return appSelectizeLookup
#' @author Monique van der Voet
#' @importFrom ontologyIndex get_OBO get_descendants propagate_relations
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' ProcessOntologies()
#' }
#'
ProcessOntologies <- function(rawDataDir = "data-raw/extractTransform", persistentDataDir = "data-persist") {
    
    # Create appSelectizeLookup: a lookup table so that a user can search for a phenotype and retrieve the id
    # Process Disease Human and Phenotype dicty, elegans, zebrafish, mammalian, human, mouse
    
    # TODO: edit appSelectize masterphenotype.csv to appSelectizeLookup
    # TODO: add uPheno, due December 2019 (postponed from summer 2019)
    
    # Disease Human ---------------------------------------------------------------------------------------------------
    # Make a table with columns "value, name, label"
    
    # Load ontology
    ontology <- ontologyIndex::get_OBO(file.path(rawDataDir, "doid.obo"), extract_tags = "everything")
    
    # Set root to extract diseases
    roots <- c("DOID:4")
    
    # Extract disease ids
    events <-
        ontologyIndex::get_descendants(ontology, roots, exclude_roots = FALSE) %>%
        as.data.frame()
    
    # Extract value and name
    output <- cbind(events,
                    name = apply(
                        events,
                        2,
                        GetProperty,
                        query = "name",
                        ontology = ontology
                    ) %>%
                        unlist)
    
    # Add column name
    colnames(output) <- c("value", "name")
    
    # Combine columns in a "label", so that user can query column for either id or disease
    output$label <-
        paste(output$name, " (", output$value, ", Human disease)", sep = "")
    
    # Output saved to partOne, to be combined with partTwo
    partOne <- output
    
    # Phenotype dicty, elegans, zebrafish, mammalian, human, mouse ----------------------------------------------------
    # Make a table with columns "value, name, label"
    
    df <- data.frame(matrix(ncol = 3, nrow = 0))
    
    # Make table with columns "id, root, name"
    df <- df %>%
        rbind(
            data.frame(
                id = "ddpheno.obo",
                root = "DDPHENO:0010000",
                name = "Dicty phenotype",
                stringsAsFactors = FALSE
            ),
            data.frame(
                id = "wbphenotype.obo",
                root = "WBPhenotype:0000886",
                name = "C. elegans phenotype",
                stringsAsFactors = FALSE
            ),
            data.frame(
                id = "zp.obo",
                root = "ZP:00000000",
                name = "Zebrafish phenotype",
                stringsAsFactors = FALSE
            ),
            data.frame(
                id = "mp.obo",
                root = "MP:0000001",
                name = "Mammalian phenotype",
                stringsAsFactors = FALSE
            ),
            data.frame(
                id = "hp.obo",
                root = "HP:0000001",
                name = "Human phenotype",
                stringsAsFactors = FALSE
            ),
            data.frame(
                id = "mpath.obo",
                root = "MPATH:0",
                name = "Mouse pathology",
                stringsAsFactors = FALSE
            )
        )
    
    # Process the df
    datalist <- list()
    for (i in seq_len(nrow(df))) {
        
        # Load ontology
        oboFile <- file.path(rawDataDir, df[i, "id"])
        message("Processing ", oboFile)
        ontology <- ontologyIndex::get_OBO(oboFile, extract_tags = "everything")
        
        # Set root to extract id
        roots <- df[i, "root"]
        
        # Extract key developmental events
        events <-
            ontologyIndex::propagate_relations(
                ontology,
                roots,
                relations = "is_a",
                use_inverse_relations = TRUE,
                exclude_roots = TRUE
            ) %>%
            as.data.frame()
        
        # Extract value and name
        output <- cbind(
            events,
            name = apply(
                events,
                2,
                GetProperty,
                query = "name",
                ontology = ontology
            ) %>%
                unlist
        )
        
        # Add column name
        colnames(output) <- c("value", "name")
        
        # Combine columns in a "label", so that user can query column for either id or disease
        output$label <-
            paste(output$name, " (", output$value, ", ", df[i, "name"], ")", sep = "")
        
        # Add each iteration to the list
        datalist[[i]] <- output
    }
    
    # Output saved to partTwo, to be combined with partOne
    partTwo <- do.call(rbind, datalist)
    
    
    # # Phenotype Unified -----------------------------------------------------------------------------------------------
    # 
    # # TODO Upheno 2.0 due December 2019: https://github.com/obophenotype/upheno-dev
    # # TODO define root to be extracted: https://www.ebi.ac.uk/ols/ontologies/upheno
    # 
    # # Load ontology
    # ontology <- ontologyIndex::get_OBO(file.path(rawDataDir, "upheno.obo"_, extract_tags = "everything") # disease
    # # Set root to extract data
    # roots <- c("")
    # # Extract ids
    # events <- ontologyIndex::propagate_relations(ontology, roots, relations = "is_a", use_inverse_relations = TRUE,
    #  exclude_roots = TRUE) %>% as.data.frame() # Key developmental events
    # # Extract value and name
    # output <- cbind(events,
    #         name = apply(events, 2, GetProperty, query="name", ontology = ontology) %>% unlist
    # )
    # # Add column name
    # colnames(output) <- c("value","name")
    # # Combine columns in a "label", so that user can query column for either id or disease
    # output$label <- paste(output$name, " (Unified phenotype)", sep = "")
    
    
    # Combine output --------------------------------------------------------------------------------------------------
    
    appSelectizeLookup <- rbind(partOne, partTwo)
    
    # Insert an empty row at the top of the df, so that toggl box is empty on load
    empty <- data.frame(matrix(ncol = 3, nrow = 1))
    colnames(empty) <- c("value", "name", "label")
    
    output <-
        rbind(empty, appSelectizeLookup)
    
    # Write to file
    write.csv(output,
              file.path(persistentDataDir, "appSelectizeLookup.csv"),
              row.names = FALSE)
}


#' Download raw chemical-based phenotype data
#'
#' @return database files to be processed
#' @author Monique van der Voet
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#' DownloadChemPheno()
#' }
DownloadChemPheno <- function(rawDataDir = "data-raw/extractTransform") {
    
    # Zebrafish -------------------------------------------------------------------------------------------------------
    download.file(
        "http://ftp.zfin.org/downloads/file/pheno_environment_fish.txt",
        file.path(rawDataDir, "pheno_environment_fish.txt")
    )
    
    download.file(
        "http://ftp.zfin.org/downloads/file/phenotype_fish.txt",
        file.path(rawDataDir, "phenotype_fish.txt")
    )
    
    download.file(
        "http://ftp.zfin.org/downloads/file/antibody_labeling_phenotype.txt",
        file.path(rawDataDir, "antibody_labeling_phenotype.txt")
    )
    
    download.file(
        "http://ftp.zfin.org/downloads/file/gene_expression_phenotype.txt",
        file.path(rawDataDir, "gene_expression_phenotype.txt")
    )
    
}


#' Download pathway data
#'
#' @param database DartDB object (created with dartTools package)
#' @param persistentDataDir directory to stored processed data
#' @return NULL
#' @author Monique van der Voet
#' @importFrom dplyr select filter left_join rename
#' @export
#' @examples
#' \dontrun{
#' DownloadPathway()
#' }
DownloadPathway <- function(database, persistentDataDir = "data-persist") {
    
  # Download Reactome --------------------------------------------------------------------------------------------------------
  
  # Lowest level pathway diagram / Subset of the pathway
  ensembl2reactome <- read.csv(
      "https://reactome.org/download/current/Ensembl2Reactome.txt",
      header = F,
      sep = "\t",
      col.names = c(
          "source_database_identifier",
          "reactome_pathway_stable_identifier",
          "URL",
          "event_name",
          "evidence_Code",
          "species"
      ) |> unique()
  # TODO: make lean, remove columns that are not necessary
  )
  
  # All levels of the pathway hierarchy
  ensembl2reactome_all <-
      read.csv(
          "https://reactome.org/download/current/Ensembl2Reactome_All_Levels.txt",
          header = F,
          sep = "\t",
          col.names = c(
              "source_database_identifier",
              "reactome_pathway_stable_identifier",
              "URL",
              "event_name",
              "evidence_Code",
              "species"
          )
      )
  
  # Complete List of Pathways
  reactomePathways <-
      read.csv(
          "https://reactome.org/download/current/ReactomePathways.txt", header = F, sep = "\t",
          col.names = c("Reactome_pathway_stable_identifier", "Event_name", "Species")
      )
  write.csv(reactomePathways,
      file.path(persistentDataDir,"reactomepathways.csv"),
      row.names = FALSE)
  
  
  # Process pathway data, where BioMart is used as ortholog datasource --------
  .ProcessData <- function(object){
    orthology <- setDF(database$getData("orthology", copy = TRUE)) |> dplyr::select(geneid, homolog_geneid, speciesfull) |> unique()
    
    # The reactome table contains predicted pathways in other species, not the known orthologs of human genes in pathways
    # Use the human genes in pathways to convert those genes to the orthologs
    human <- object |> dplyr::filter(species == "Homo sapiens") |> unique()
    
    # Function to convert orthologs, based on human pathways
    # TODO: consider filtering orthologs, based on how good the orthology is (now all orthologs are used)
    ConvertHomolog <- function(species_full, reactome_id){
      df <- human |>
          # add homolog gene
          dplyr::left_join((orthology |> dplyr::filter(speciesfull == species_full) |> dplyr::select(-speciesfull)),
              by = c("source_database_identifier" = "geneid")) |>
          # replace column
          dplyr::select(-source_database_identifier) |>
          dplyr::rename("source_database_identifier" = "homolog_geneid") |> unique()
      # edit reactome_pathway_stable_identifier
      df$reactome_pathway_stable_identifier <- gsub("R-HSA-", reactome_id, df$reactome_pathway_stable_identifier)
      # edit url
      df$URL <- gsub("R-HSA-", reactome_id, df$URL)
      # edit evidence_Code
      df$evidence_Code <- "dartpaths"
      # edit species
      df$species <- species_full
      return(df)
    }
    # convert orthologs
    dicty <- ConvertHomolog(species_full = "Dictyostelium discoideum", reactome_id = "R-DDI-")
    nematode <- ConvertHomolog(species_full = "Caenorhabditis elegans", reactome_id = "R-CEL-")
    drosophila <- ConvertHomolog(species_full = "Drosophila melanogaster", reactome_id = "R-DME-")
    zebrafish <- ConvertHomolog(species_full = "Danio rerio", reactome_id = "R-DRE-")
    #rabbit <- ConvertHomolog(species_full = "", reactome_id = "R--") # NOT IN TABLE
    mouse <- ConvertHomolog(species_full = "Mus musculus", reactome_id = "R-MMU-")
    rat <- ConvertHomolog(species_full = "Rattus norvegicus", reactome_id = "R-RNO-")
    # combine file
    result <- rbind(human, dicty, nematode, drosophila, zebrafish, mouse, rat) |>
        na.omit() |> unique()
    return(result)
  }
  ensembl2reactome <- .ProcessData(ensembl2reactome)
  ensembl2reactome_all <- .ProcessData(ensembl2reactome_all)
  
  # Write to file
  write.csv(ensembl2reactome,
      file.path(persistentDataDir,"ensembl2reactome.csv"),
      row.names = FALSE)
  write.csv(ensembl2reactome_all,
      file.path(persistentDataDir,"ensembl2reactome_all.csv"),
      row.names = FALSE)
}


#' Download raw gene-based database files, used for phenotype to pathway mapping
#'
#' @return database files to be processed
#' @author Monique van der Voet
#' @importFrom ontologyIndex get_OBO
#' @importFrom RCurl getURL
#' @export
#' @examples
#' \dontrun{
#' DownloadGenePheno()
#' }
DownloadGenePheno <- function(rawDataDir = "data-raw/extractTransform") {
    
    # Avoid timeout for large files
    options(timeout = max(600, getOption("timeout")))
    
    # Dicty -----------------------------------------------------------------------------------------------------------
    download.file(
        "http://dictybase.org/db/cgi-bin/dictyBase/download/download.pl?area=mutant_phenotypes&ID=all-mutants-ddb_g.txt",
        file.path(rawDataDir, "all-mutants-ddb_g.txt")
    )
    
    
    # Elegans ---------------------------------------------------------------------------------------------------------
    
    # Check the filenames that are on Wormbase
    filenames <-
        RCurl::getURL(
            "ftp://ftp.wormbase.org/pub/wormbase/releases/current-production-release/ONTOLOGY/",
            ftp.use.epsv = FALSE,
            dirlistonly = TRUE
        )
    filenames <- strsplit(filenames, "\n")
    filenames <- unlist(filenames)
    
    # Get phenotypeAssociation filename of current production release
    phenotypeAssociation <-
        filenames[grep("phenotype_association", filenames)]
    
    download.file(
        paste(
            "ftp://ftp.wormbase.org/pub/wormbase/releases/current-production-release/ONTOLOGY/",
            phenotypeAssociation,
            sep = ""
        ),
        file.path(rawDataDir, "phenotype_association.wb")
    )
    
    
    # Zebrafish -------------------------------------------------------------------------------------------------------
    
    # Genotype of fish
    download.file(
        "https://zfin.org/downloads/file/fish_components_fish.txt",
        file.path(rawDataDir, "fish_components_fish.txt")
    )
    
    # No longer needed, use phenotypeFish, which creates file phenotypeFish
    # download.file(
    #   "https://zfin.org/downloads/file/phenoGeneCleanData_fish.txt",
    #   file.path(rawDataDir, "zfphenoGeneCleanData_fish.txt"
    # )
    
    
    # Mouse MGI -------------------------------------------------------------------------------------------------------
    
    # All Genotypes and Mammalian Phenotype Annotations (tab-delimited)
    download.file(
        "http://www.informatics.jax.org/downloads/reports/MGI_PhenoGenoMP.rpt",
        file.path(rawDataDir, "MGI_PhenoGenoMP.rpt")
    )
    
    # List of All Mouse Phenotypic Alleles (tab-delimited), not used because these phenotypes are high-level
    # Use phenotypeMouseMGI instead
    # download.file(
    #   "http://www.informatics.jax.org/downloads/reports/MGI_PhenotypicAllele.rpt",
    #   file.path(rawDataDir, "MGI_PhenotypicAllele.rpt"
    # )
    
    
    # Mouse IMPC ------------------------------------------------------------------------------------------------------
    
    # international mouse phenotype consortium https://www.mousephenotype.org/
    # inprogress: load and process this dataset
    download.file(
        #"ftp://ftp.ebi.ac.uk/pub/databases/impc/latest/csv/IMPC_ALL_statistical_results.csv.gz", # old
        "ftp://ftp.ebi.ac.uk/pub/databases/impc/all-data-releases/latest/results/statistical-results-ALL.csv.gz",
        file.path(rawDataDir, "IMPC_ALL_statistical_results.csv.gz")
    )
    
    
    
    # Rat -------------------------------------------------------------------------------------------------------------
    
    # TODO Rat : ftp://ftp.rgd.mcw.edu/pub/, which file contains phenotype information?
    # not good: no significant LOD?
    # download.file(
    #   "ftp://ftp.rgd.mcw.edu/pub/data_release/QTLS_RAT.txt",
    #   file.path(rawDataDir, "rgdQTLS_RAT.rpt")
    # )
    
    
    # Rabbit ----------------------------------------------------------------------------------------------------------
    
    # TODO Rabbit
    # no database available, only OECD guidelines in the QSAR toolbox
    
    
    # Human -----------------------------------------------------------------------------------------------------------
    
    # hpo: A curated database of human hereditary syndromes from OMIM, Orphanet, and DECIPHER mapped to classes
    # of the human phenotype ontology.
    # ALL_SOURCES_ALL_FREQUENCIES_genes_to_phenotype.txt provides a link between genes and hpo terms.
    # All phenotype terms associated with any disease that is associated with variants in a gene are assigned to that
    # gene in this file.
    # Other files are available on our Jenkins server that filter terms according to provenance of the annotation and
    # frequency of the features in the disease.
    # "ALL_SOURCES_ALL_FREQUENCIES_genes_to_phenotype.txt" is now named "genes_to_phenotype" in the new release
    
    download.file(
        #"http://compbio.charite.de/jenkins/job/hpo.annotations/lastSuccessfulBuild/artifact/util/annotation/genes_to_phenotype.txt", # old
        "http://purl.obolibrary.org/obo/hp/hpoa/genes_to_phenotype.txt",
        file.path(rawDataDir, "genes_to_phenotype.txt")
    )
    
}


#' Function to parse ZFIN phenotype to anatomy, expression, labeling
#'
#' @return
#' @author Monique van der Voet
#' @importFrom magrittr %>%
#' @importFrom dplyr filter left_join mutate select
#' @export
#' @examples
#' \dontrun{
#'
#' }
ParseZFINphenotype <- function(rawDataDir = "data-raw/extractTransform", persistentDataDir = "data-persist") {
    
    # Load raw data ---------------------------------------------------------------------------------------------------
    ## phenotypeFish (all phenotypes: anatomy, antibody, expression)
    ## phenotypeAntibodyLabeling (antibody phenotypes)
    ## phenotypeGeneExpression (expression phenotypes)
    
    # Load ZFIN table with phenotype
    lines <-
        readLines(file.path(rawDataDir, "phenotype_fish.txt"))
    
    # Grab header
    linesHeader <-
        # select lines that start with Date
        lines[grepl("Date: ", lines)]
    
    # Grab body
    linesBody <-
        # remove lines that start with Date
        lines[!grepl("Date:", lines)]
    
    # Read file
    phenotypeFish <-
        read.csv(text = linesBody,
                 sep = "\t",
                 header = TRUE) %>%
        dplyr::select(
            "Environment.ID",
            "Fish.ID",
            "Fish.Name",
            "Affected.Structure.or.Process.1.superterm.ID",
            "Affected.Structure.or.Process.1.superterm.Name",
            "Affected.Structure.or.Process.1.subterm.ID",
            "Affected.Structure.or.Process.1.subterm.Name",
            # "Post.composed.Relationship.ID",
            # "Post.composed.Relationship.Name",
            "Phenotype.Keyword.ID",
            "Phenotype.Keyword.Name",
            "Affected.Structure.or.Process.2.superterm.ID",
            "Affected.Structure.or.Process.2.superterm.name",
            "Affected.Structure.or.Process.2.subterm.ID",
            "Affected.Structure.or.Process.2.subterm.name",
            # "Post.composed.Relationship..rel..ID",
            # "Post.composed.Relationship..rel..Name",
            "Start.Stage.ID",
            "Start.Stage.Name",
            "End.Stage.ID",
            "End.Stage.Name",
            "Phenotype.Tag",
            "Publication.ID"
        )
    
    # Load ZFIN table with antibody labeling
    lines <-
        readLines(file.path(rawDataDir, "antibody_labeling_phenotype.txt"))
    
    # Grab header
    linesHeader <-
        # select lines that start with Date
        lines[grepl("Date: ", lines)]
    
    # Grab body
    linesBody <-
        # remove lines that start with Date
        lines[!grepl("Date:", lines)]
    
    # Read file
    phenotypeAntibodyLabeling <-
        read.csv(text = linesBody,
                 sep = "\t",
                 header = TRUE) %>%
        dplyr::select(
            "Environment.ID",
            "Fish.ID",
            "Start.Stage.ID",
            "Start.Stage.Name",
            "End.Stage.ID",
            "End.Stage.Name",
            "Superstructure.ID",
            "Superstructure.Name",
            "Substructure.ID",
            "Substructure.Name",
            "Phenotype.Keyword.ID",
            "Phenotype.Keyword.Name",
            "Phenotype.Tag",
            "Publication.ID",
            "RO.Term",
            "Antibody.ID",
            "Antibody.Name",
            "Assay",
            "PubMed.ID"
        )
    
    # Load ZFIN table with gene expression
    lines <-
        readLines(file.path(rawDataDir, "gene_expression_phenotype.txt"))
    
    # Grab header
    linesHeader <-
        # select lines that start with Date
        lines[grepl("Date: ", lines)]
    
    # Grab body
    linesBody <-
        # remove lines that start with Date
        lines[!grepl("Date:", lines)]
    
    # Read file
    phenotypeGeneExpression <-
        read.csv(text = linesBody,
                 sep = "\t",
                 header = TRUE) %>%
        dplyr::select(
            "Environment.ID",
            "Fish.ID",
            "Start.Stage.ID",
            "Start.Stage.Name",
            "End.Stage.ID",
            "End.Stage.Name",
            "Superstructure.ID",
            "Superstructure.Name",
            "Substructure.ID",
            "Substructure.Name",
            "Phenotype.Keyword.ID",
            "Phenotype.Keyword.Name",
            "Phenotype.Tag",
            "Publication.ID",
            "RO.Term",
            "Gene.ID",
            "Gene.Symbol",
            "Assay",
            "PubMed.ID"
        )
    
    # Change colname to distinguish from Gene.Symbol of fish
    colnames(phenotypeGeneExpression)[colnames(phenotypeGeneExpression) == "Gene.ID"] <-
        "Gene.ID.Expression"
    colnames(phenotypeGeneExpression)[colnames(phenotypeGeneExpression) == "Gene.Symbol"] <-
        "Gene.Symbol.Expression"
    
    # Load ZFIN table with environmental conditions
    lines <-
        readLines(file.path(rawDataDir, "pheno_environment_fish.txt"))
    
    # Grab header
    linesHeader <-
        # line with date
        lines[grepl("Date: ", lines)]
    
    # Grab body
    linesBody <-
        # lines without date
        lines[!grepl("Date:", lines)]
    
    # Read file
    phenotypeEnvironmentFish <-
        read.csv(text = linesBody,
                 sep = "\t",
                 header = TRUE) %>%
        # Make lean
        dplyr::select(
            "Environment.ID",
            "ZECO.Term.Name",
            "ZECO.Term.ID..ZECO.ID.",
            "Chebi.Term.Name",
            "Chebi.Term.ID..Chebi.ID."
        )
    
    
    # Load ZFIN table with fish components
    lines <-
        readLines(file.path(rawDataDir, "fish_components_fish.txt"))
    
    # Grab header
    linesHeader <-
        # line with date
        lines[grepl("Date: ", lines)]
    
    # Grab body
    linesBody <-
        # lines without date
        lines[!grepl("Date:", lines)]
    
    # Read file
    fishGene <-
        read.csv(text = linesBody,
                 sep = "\t",
                 header = TRUE) %>%
        # Make lean
        dplyr::select("Fish.ID",
                      "Gene.ID",
                      "Gene.Symbol")
    
    # Change colname to distinguish from Gene.Symbol of expression
    # colnames(fishGene)[colnames(fishGene) == "Gene.ID"] <- "Gene.ID.Fish"
    # colnames(fishGene)[colnames(fishGene) == "Gene.Symbol"] <- "Gene.Symbol.Fish"
    
    
    # Clean data ------------------------------------------------------------------------------------------------------
    
    # Join phenotypeFish with environment condition and fish component = master phenotype
    phenotypeFishMaster <-
        phenotypeFish %>%
        # Join environment
        dplyr::left_join(phenotypeEnvironmentFish,
                         by = "Environment.ID") %>%
        # Join fish genotype
        dplyr::left_join(fishGene,
                         by = "Fish.ID") %>%
        # There are some duplicates, sometimes one fish alters multiple genes
        # e.g. ZDB-FISH-150901-1000, WT + MO2-col27a1a + MO2-col27a1b
        unique
    
    # The phenotypeFish table contains information that connects to antibody and expression tables,
    # these should be removed to retain anatomy info
    
    phenotypeAnatomy <-
        phenotypeFishMaster %>%
        # Join to identify which fields are antibody phenotypes
        dplyr::left_join(
            (
                phenotypeAntibodyLabeling %>%
                    # Select relevant columns to merge
                    dplyr::select(
                        "Environment.ID",
                        # Foreign keys for connecting phenotypeFish to phenotypeAntibodyLabeling
                        "Fish.ID",
                        "Superstructure.ID",
                        "Superstructure.Name",
                        "Substructure.ID",
                        "Substructure.Name",
                        "Phenotype.Keyword.ID",
                        "Phenotype.Keyword.Name",
                        "Phenotype.Tag",
                        "Start.Stage.ID",
                        "Start.Stage.Name",
                        "End.Stage.ID",
                        "End.Stage.Name",
                        "Publication.ID",
                        "Antibody.ID",
                        "Antibody.Name" # Primary key to keep
                    )
            ),
            by = c(
                # Connect to phenotypeAntibodyLabeling
                "Environment.ID",
                "Fish.ID",
                "Affected.Structure.or.Process.1.superterm.ID" = "Superstructure.ID",
                "Affected.Structure.or.Process.1.superterm.Name" = "Superstructure.Name",
                "Affected.Structure.or.Process.1.subterm.ID" = "Substructure.ID",
                "Affected.Structure.or.Process.1.subterm.Name" = "Substructure.Name",
                "Phenotype.Keyword.ID",
                "Phenotype.Keyword.Name",
                "Phenotype.Tag",
                "Start.Stage.ID",
                "Start.Stage.Name",
                "End.Stage.ID",
                "End.Stage.Name",
                "Publication.ID"
            )
        ) %>%
        # Join to identify which fields are expression phenotypes
        dplyr::left_join(
            (
                phenotypeGeneExpression %>%
                    # Select relevant columns to merge
                    dplyr::select(
                        "Environment.ID",
                        "Fish.ID",
                        # Foreign keys for connecting phenotypeFish to phenotypeAntibodyLabeling
                        "Superstructure.ID",
                        "Superstructure.Name",
                        "Substructure.ID",
                        "Substructure.Name",
                        "Phenotype.Keyword.ID",
                        "Phenotype.Keyword.Name",
                        "Phenotype.Tag",
                        "Start.Stage.ID",
                        "Start.Stage.Name",
                        "End.Stage.ID",
                        "End.Stage.Name",
                        "Publication.ID",
                        "Gene.Symbol.Expression" # Primary key to keep
                    )
            ),
            by = c(
                # Connect to phenotypeGeneExpression
                "Environment.ID",
                "Fish.ID",
                "Affected.Structure.or.Process.1.subterm.ID" = "Substructure.ID",
                "Affected.Structure.or.Process.1.subterm.Name" = "Substructure.Name",
                "Affected.Structure.or.Process.1.superterm.ID" = "Superstructure.ID",
                "Affected.Structure.or.Process.1.superterm.Name" = "Superstructure.Name",
                "Phenotype.Keyword.ID",
                "Phenotype.Keyword.Name",
                "Phenotype.Tag",
                "Start.Stage.ID",
                "Start.Stage.Name",
                "End.Stage.ID",
                "End.Stage.Name",
                "Publication.ID"
            )
        ) %>%
        # Remove records that connect to antibody and expression tables
        dplyr::filter(is.na(Antibody.Name), is.na(Gene.Symbol.Expression)) %>%
        dplyr::select(-"Antibody.Name", -"Gene.Symbol.Expression") %>%
        # Add empty columns to prepare for rbind
        dplyr::mutate(
            Target.ID = "",
            Target.Name = "",
            Target.Term = ""
        ) %>%
        # Make lean
        dplyr::select(
            "Environment.ID",
            "Fish.ID",
            "Fish.Name",
            "Gene.ID",
            "Gene.Symbol",
            "ZECO.Term.Name",
            "Chebi.Term.ID..Chebi.ID.",
            "Chebi.Term.Name",
            "Affected.Structure.or.Process.1.superterm.ID",
            "Affected.Structure.or.Process.1.superterm.Name",
            "Affected.Structure.or.Process.1.subterm.ID",
            "Affected.Structure.or.Process.1.subterm.Name",
            "Target.ID",
            "Target.Name",
            "Target.Term",
            "Phenotype.Keyword.ID",
            "Phenotype.Keyword.Name",
            "Affected.Structure.or.Process.2.superterm.ID",
            "Affected.Structure.or.Process.2.superterm.name",
            "Affected.Structure.or.Process.2.subterm.ID",
            "Affected.Structure.or.Process.2.subterm.name",
            "Phenotype.Tag",
            "Start.Stage.ID",
            "Start.Stage.Name",
            "End.Stage.ID",
            "End.Stage.Name",
            "Publication.ID"
        )
    
    # Create phenotypeAntibody
    phenotypeAntibody <-
        # Combine experiment w/ phenotype
        phenotypeFishMaster %>%
        dplyr::left_join(
            phenotypeAntibodyLabeling,
            by = c(
                # Connect to phenotypeAntibodyLabeling
                "Environment.ID",
                "Fish.ID",
                "Affected.Structure.or.Process.1.subterm.ID" = "Substructure.ID",
                "Affected.Structure.or.Process.1.subterm.Name" = "Substructure.Name",
                "Affected.Structure.or.Process.1.superterm.ID" = "Superstructure.ID",
                "Affected.Structure.or.Process.1.superterm.Name" = "Superstructure.Name",
                "Phenotype.Keyword.ID",
                "Phenotype.Keyword.Name",
                "Phenotype.Tag",
                "Start.Stage.ID",
                "Start.Stage.Name",
                "End.Stage.ID",
                "End.Stage.Name",
                "Publication.ID"
            )
        ) %>%
        dplyr::filter(!is.na(Antibody.Name)) %>%
        # Add column with domain of the phenotype
        dplyr::mutate(Target.Term = "antibody") %>%
        # Make lean
        dplyr::select(
            "Environment.ID",
            "Fish.ID",
            "Fish.Name",
            "Gene.ID",
            "Gene.Symbol",
            "ZECO.Term.Name",
            "Chebi.Term.ID..Chebi.ID.",
            "Chebi.Term.Name",
            "Affected.Structure.or.Process.1.superterm.ID",
            "Affected.Structure.or.Process.1.superterm.Name",
            "Affected.Structure.or.Process.1.subterm.ID",
            "Affected.Structure.or.Process.1.subterm.Name",
            "Antibody.ID",
            "Antibody.Name",
            "Target.Term",
            "Phenotype.Keyword.ID",
            "Phenotype.Keyword.Name",
            "Affected.Structure.or.Process.2.superterm.ID",
            "Affected.Structure.or.Process.2.superterm.name",
            "Affected.Structure.or.Process.2.subterm.ID",
            "Affected.Structure.or.Process.2.subterm.name",
            "Phenotype.Tag",
            "Start.Stage.ID",
            "Start.Stage.Name",
            "End.Stage.ID",
            "End.Stage.Name",
            "Publication.ID"
        )
    
    colnames(phenotypeAntibody)[colnames(phenotypeAntibody) == "Antibody.ID"] <-
        "Target.ID"
    colnames(phenotypeAntibody)[colnames(phenotypeAntibody) == "Antibody.Name"] <-
        "Target.Name"
    
    # Create phenotypeExpression
    phenotypeExpression <-
        # Combine experiment w/ phenotype
        phenotypeFishMaster %>%
        dplyr::left_join(
            phenotypeGeneExpression,
            by = c(
                # Connect to phenotypeGeneExpression
                "Environment.ID",
                "Fish.ID",
                "Affected.Structure.or.Process.1.subterm.ID" = "Substructure.ID",
                "Affected.Structure.or.Process.1.subterm.Name" = "Substructure.Name",
                "Affected.Structure.or.Process.1.superterm.ID" = "Superstructure.ID",
                "Affected.Structure.or.Process.1.superterm.Name" = "Superstructure.Name",
                "Phenotype.Keyword.ID",
                "Phenotype.Keyword.Name",
                "Phenotype.Tag",
                "Start.Stage.ID",
                "Start.Stage.Name",
                "End.Stage.ID",
                "End.Stage.Name",
                "Publication.ID"
            )
        ) %>%
        # Remove records without expression information
        dplyr::filter(!is.na(Gene.Symbol.Expression)) %>%
        # Add column with domain of the phenotype
        dplyr::mutate(Target.Term = "expression") %>%
        # Make lean
        dplyr::select(
            "Environment.ID",
            "Fish.ID",
            "Fish.Name",
            "Gene.ID",
            "Gene.Symbol",
            "ZECO.Term.Name",
            "Chebi.Term.ID..Chebi.ID.",
            "Chebi.Term.Name",
            "Affected.Structure.or.Process.1.superterm.ID",
            "Affected.Structure.or.Process.1.superterm.Name",
            "Affected.Structure.or.Process.1.subterm.ID",
            "Affected.Structure.or.Process.1.subterm.Name",
            "Gene.ID.Expression",
            "Gene.Symbol.Expression",
            "Target.Term",
            "Phenotype.Keyword.ID",
            "Phenotype.Keyword.Name",
            "Affected.Structure.or.Process.2.superterm.ID",
            "Affected.Structure.or.Process.2.superterm.name",
            "Affected.Structure.or.Process.2.subterm.ID",
            "Affected.Structure.or.Process.2.subterm.name",
            "Phenotype.Tag",
            "Start.Stage.ID",
            "Start.Stage.Name",
            "End.Stage.ID",
            "End.Stage.Name",
            "Publication.ID"
        )
    
    colnames(phenotypeExpression)[colnames(phenotypeExpression) == "Gene.ID.Expression"] <-
        "Target.ID"
    colnames(phenotypeExpression)[colnames(phenotypeExpression) == "Gene.Symbol.Expression"] <-
        "Target.Name"
    
    # Combine ZFIN input data anatomy + antibody + expression ---------------------------------------------------------
    output <-
        # Combine tables
        rbind(phenotypeAnatomy, phenotypeAntibody) %>%
        rbind(phenotypeExpression) %>%
        unique %>%
        # Convert ZFIN to ZP
        .ConvertZFINtoZP(persistentDataDir = persistentDataDir)
    
    # Write to file
    write.csv(output,
              file.path(persistentDataDir, "phenotypeFishMaster.csv"),
              row.names = FALSE)
    
    
    # Create file focused on chemical phenotypes ----------------------------------------------------------------------
    phenotypeFishChem <- output %>%
        subset(
            ZECO.Term.Name %in% c(
                "chemical treatment",
                # only save chemical treatment columns
                "chemical treatment by diet",
                "chemical treatment by environment",
                "chemical treatment by gavage",
                "chemical treatment by injection"
            )
        )
    
    # Write to file
    write.csv(phenotypeFishChem,
              file.path(persistentDataDir, "phenotypeFishChem.csv"),
              row.names = FALSE)
    
}


#' Fuction to convert ZFIN columns to single ZP term
#'
#' @return
#' @author Monique van der Voet
#' @importFrom magrittr %>%
#' @importFrom dplyr filter select mutate left_join
#' @importFrom stringr str_squish
#' @export
#' @examples
#' \dontrun{
#' .ConvertZFINtoZP(input)
#' }
.ConvertZFINtoZP <-
    function(input, persistentDataDir = "data-persist") {
        # Load file with all phenotype ids
        appSelectizeLookup <-
            read.csv(
                file.path(persistentDataDir, "appSelectizeLookup.csv"),
                sep = ",",
                header = TRUE
            )
        
        # Create lookup table with all zebrafish ids
        lookupFish <-
            appSelectizeLookup %>%
            dplyr::filter(grepl("ZP:", .$value)) %>%
            dplyr::select(value, name)
        
        lookupFish$name <- lookupFish$name %>%
            as.character()
        
        # Create phenotype names by combining data columns in 6 different formats
        df <- input %>%
            dplyr::mutate(
                concat1 = paste(
                    Affected.Structure.or.Process.1.superterm.Name,
                    Affected.Structure.or.Process.1.subterm.Name,
                    Target.Name,
                    Target.Term,
                    Phenotype.Keyword.Name,
                    Affected.Structure.or.Process.2.superterm.name,
                    Affected.Structure.or.Process.2.subterm.name,
                    ",",
                    Phenotype.Tag,
                    sep = " "
                )
            ) %>%
            dplyr::mutate(
                concat2 = paste(
                    Affected.Structure.or.Process.1.subterm.Name,
                    Affected.Structure.or.Process.1.superterm.Name,
                    Target.Name,
                    Target.Term,
                    Phenotype.Keyword.Name,
                    Affected.Structure.or.Process.2.superterm.name,
                    Affected.Structure.or.Process.2.subterm.name,
                    ",",
                    Phenotype.Tag,
                    sep = " "
                )
            ) %>%
            dplyr::mutate(
                concat3 = paste(
                    Affected.Structure.or.Process.1.superterm.Name,
                    Affected.Structure.or.Process.1.subterm.Name,
                    Target.Name,
                    Target.Term,
                    Phenotype.Keyword.Name,
                    Affected.Structure.or.Process.2.subterm.name,
                    Affected.Structure.or.Process.2.superterm.name,
                    ",",
                    Phenotype.Tag,
                    sep = " "
                )
            ) %>%
            dplyr::mutate(
                concat4 = paste(
                    Affected.Structure.or.Process.1.superterm.Name,
                    Phenotype.Keyword.Name,
                    Affected.Structure.or.Process.1.subterm.Name,
                    Target.Name,
                    Target.Term,
                    Affected.Structure.or.Process.2.superterm.name,
                    Affected.Structure.or.Process.2.subterm.name,
                    ",",
                    Phenotype.Tag,
                    sep = " "
                )
            ) %>%
            dplyr::mutate(
                concat5 = paste(
                    Affected.Structure.or.Process.1.superterm.Name,
                    Phenotype.Keyword.Name,
                    Affected.Structure.or.Process.1.subterm.Name,
                    Target.Name,
                    Target.Term,
                    Affected.Structure.or.Process.2.subterm.name,
                    Affected.Structure.or.Process.2.superterm.name,
                    ",",
                    Phenotype.Tag,
                    sep = " "
                )
            ) %>%
            dplyr::mutate(
                concat6 = paste(
                    Affected.Structure.or.Process.1.subterm.Name,
                    Affected.Structure.or.Process.1.superterm.Name,
                    Target.Name,
                    Target.Term,
                    Phenotype.Keyword.Name,
                    Affected.Structure.or.Process.2.subterm.name,
                    Affected.Structure.or.Process.2.superterm.name,
                    ",",
                    Phenotype.Tag,
                    sep = " "
                )
            ) %>%
            # Clean up phenotype strings
            dplyr::mutate(concat1 = stringr::str_squish(.$concat1)) %>%
            dplyr::mutate(concat1 = gsub(" ,", ",", .$concat1)) %>%
            dplyr::mutate(concat2 = stringr::str_squish(.$concat2)) %>%
            dplyr::mutate(concat2 = gsub(" ,", ",", .$concat2)) %>%
            dplyr::mutate(concat3 = stringr::str_squish(.$concat3)) %>%
            dplyr::mutate(concat3 = gsub(" ,", ",", .$concat3)) %>%
            dplyr::mutate(concat4 = stringr::str_squish(.$concat4)) %>%
            dplyr::mutate(concat4 = gsub(" ,", ",", .$concat4)) %>%
            dplyr::mutate(concat5 = stringr::str_squish(.$concat5)) %>%
            dplyr::mutate(concat5 = gsub(" ,", ",", .$concat5)) %>%
            dplyr::mutate(concat6 = stringr::str_squish(.$concat6)) %>%
            dplyr::mutate(concat6 = gsub(" ,", ",", .$concat6))
        
        # Create a list column containing all phenotype combinations and make unique
        df$concat <-
            as.list(as.data.frame(t(df[, c("concat1",
                                "concat2",
                                "concat3",
                                "concat4",
                                "concat5",
                                "concat6")]))) %>%
            purrr::map( ~ unique(unlist(.x)))
        
        # Unnest list and add column with identifier that matches the phenotype name
        df <- df %>%
            tidyr::unnest(concat) %>%
            dplyr::left_join(lookupFish, by = c("concat" = "name"))
        
        # Remove unneeded columns
        df <- subset(df, select = -c(concat1:concat6))
        # Remove unmatched values
        df <- df %>% dplyr::filter(!is.na(value))
        
        # Assign permanent colnames to new columns
        colnames(df)[colnames(df) == "concat"] <- "Phenotype.Name"
        colnames(df)[colnames(df) == "value"] <- "Phenotype.ID"
        return(df)
      }

#' Process gene-based phenotype data, these tables, used for phenotype to pathway mapping
#'
#' @return genePheno
#' @author Monique van der Voet
#' @importFrom biomaRt useMart
#' @importFrom biomaRt getBM
#' @importFrom ontologyIndex get_descendants get_OBO
#' @importFrom ontologyPlot get_shortened_names
#' @importFrom dplyr filter select mutate left_join
#' @importFrom magrittr %>%
#' @importFrom tidyr unnest
#' @importFrom stringr str_trim
#' @importFrom scales rescale
#' @export
#' @examples
#' \dontrun{
#' ProcessGenePheno()
#' }
ProcessGenePheno <- function(rawDataDir = "data-raw/extractTransform", persistentDataDir = "data-persist") {
    
    # Standard GO Annotation File (GAF) file formats, used for colnames
    gaf2.0 <-
        c(
            # Format specifications: http://geneontology.org/docs/go-annotation-file-gaf-format-2.0/
            "DB",
            "DB_Object_ID",
            "DB_Object_Symbol",
            "Qualifier",
            "GO_ID",
            "DB_Reference",
            "Evidence_Code",
            "With",
            "Aspect",
            "DB_Object_Name",
            "DB_Object_Synonym",
            "DB_Object_Type",
            "Taxon",
            "Date",
            "Assigned_By",
            "Annotation_Extension",
            "Gene_Product_Form_ID"
        )
    gaf2.1 <-
        c(
            # Format specifications: http://geneontology.org/docs/go-annotation-file-gaf-format-2.1/
            "DB",
            "DB_Object_ID",
            "DB_Object_Symbol",
            "Qualifier",
            "GO_ID",
            "DB_Reference",
            "Evidence_Code",
            "With",
            "Aspect",
            "DB_Object_Name",
            "DB_Object_Synonym",
            "DB_Object_Type",
            "Taxon",
            "Date",
            "Assigned_By",
            "Annotation_Extension",
            "Gene_Product_Form_ID"
        )
    
    
    # Dicty -----------------------------------------------------------------------------------------------------------
    
    # Load file with all phenotype ids
    appSelectizeLookup <-
        read.csv(
            file.path(persistentDataDir, "appSelectizeLookup.csv"),
            sep = ",",
            header = TRUE
        )
    
    # Create lookup table to translate phenotype column phenotypeDicty to id
    lookupDicty <-
        appSelectizeLookup %>%
        dplyr::filter(grepl("DDPHENO", .$value)) %>%
        dplyr::select(value, name)
    
    lookupDicty$name <- lookupDicty$name %>%
        as.character()
    
    # Create phenotype table
    phenotypeDicty <-
        # Load file with gene data
        read.csv(
            file.path(rawDataDir, "all-mutants-ddb_g.txt"),
            sep = "\t",
            header = TRUE
        ) %>%
        # Make lean, remove columns that are not essential
        dplyr::select(DDB_G_ID, Associated.gene.s., Phenotypes) %>%
        # Remove rows with double mutants, seperated by |
        dplyr::filter(!grepl("\\|", .$DDB_G_ID)) %>%
        # Split cell over columns
        dplyr::mutate(Phenotypes = strsplit(as.character(Phenotypes), " \\| ")) %>%
        # Split cell over columns
        tidyr::unnest(Phenotypes) %>%
        # add DDPHENO
        dplyr::left_join(lookupDicty, by = c("Phenotypes" = "name")) %>%
        # Add taxon id
        dplyr::mutate(Taxon = "44689") %>%
        unique
    
    # Calculate Information Content (IC) of the phenotypes, useful for interpreting specificity of phenotype
    ic <-
        table(phenotypeDicty$Phenotypes) %>%
        as.data.frame() %>%
        dplyr::mutate(IC = -log2(Freq / nrow(phenotypeDicty)))
    
    # Add IC to phenotype table
    phenotypeDicty <-
        dplyr::left_join(phenotypeDicty, ic, by = c("Phenotypes" = "Var1")) %>%
        # There are obsolete phenotypes that are NA, remove these
        dplyr::filter(!is.na(value))
    
    # Calculate IC weight
    phenotypeDicty <-
        phenotypeDicty %>%
        #dplyr::mutate(ICweight = 100 / max(IC) * IC)
        dplyr::mutate(ICweight = scales::rescale(IC, to = c(0, 1)))
    
    
    # Add colnames corresponding to GAF 2.0
    colnames(phenotypeDicty) <-
        c(
            "DB_Object_ID",
            "DB_Object_Symbol",
            "GO_ID_Name",
            "GO_ID",
            "Taxon",
            "Freq",
            "IC",
            "ICweight"
        )
    
    # Save to file
    write.csv(
        phenotypeDicty,
        file.path(persistentDataDir, "phenotypeDicty.csv"),
        row.names = FALSE
    )
    
    
    # Elegans -----------------------------------------------------------------
    
    # Create Persistent file: C. elegans phenotype with Information Content
    # Used in dartTools e.g. for mapping a collection of phenotypes to a pathway
    
    # Read file with all phenotype ids
    lines <-
        readLines(file.path(rawDataDir, "phenotype_association.wb"))
    
    # Grab header
    linesHeader <-
        lines[grepl("^!|^.*!", lines)] # select lines that start with !
    
    # Grab body
    linesBody <-
        lines[!grepl("^!|^.*!", lines)] # remove lines that start with !
    
    # Read file, this contains RNAi and variant phenotypes
    phenotypeElegans <-
        read.csv(text = linesBody,
                 sep = "\t",
                 header = FALSE)
    
    # Add column names according to GO Annotation File (GAF) files
    if (linesHeader[1] == "!gaf-version: 2.0") {
        colnames(phenotypeElegans) <- gaf2.0
    } else if (linesHeader[1] == "!gaf-version: 2.1") {
        colnames(phenotypeElegans) <- gaf2.1
    }
    
    # add WBPheno name that matches GO_ID
    
    # Load ontology
    ontology <-
        ontologyIndex::get_OBO(file.path(rawDataDir, "wbphenotype.obo"), extract_tags = "everything")
    
    # Set root to extract C. elegans phenotype
    roots <-
        c("WBPhenotype:0000886") %>%
        as.character()
    
    # Extract phenotype ids
    events <-
        ontologyIndex::get_descendants(ontology, roots, exclude_roots = TRUE) %>%
        as.data.frame()
    
    # Extract value and name
    output <- cbind(events,
                    tolower(
                        apply(events,
                              2,
                              ontologyPlot::get_shortened_names,
                              ontology = ontology)
                    ))
    
    # Add column name
    colnames(output) <- c("GO_ID", "GO_ID_Name")
    
    # Join
    phenotypeElegans <-
        dplyr::left_join(phenotypeElegans,
                         output,
                         by = c("GO_ID" = "GO_ID"))
    
    phenotypeElegans <-
        phenotypeElegans %>%
        # Remove when phenotype was not observed
        dplyr::filter(Qualifier != "NOT") %>%
        # Make lean, remove non-essential columns
        dplyr::select(DB_Object_ID, DB_Object_Symbol, GO_ID, GO_ID_Name) %>% # DB_Reference optional
        # Add Taxon id
        dplyr::mutate(Taxon = "6239") %>%
        unique
    
    # Add Information Content (IC) to the phenotype table, useful for interpreting specificity of phenotype
    ic <-
        table(phenotypeElegans$GO_ID) %>%
        as.data.frame() %>%
        dplyr::mutate(IC = -log2(Freq / nrow(phenotypeElegans)))
    
    # Join
    phenotypeElegans <-
        dplyr::left_join(phenotypeElegans, ic, by = c("GO_ID" = "Var1"))
    
    # Add weighted IC, used for ranking pathways
    phenotypeElegans <-
        phenotypeElegans %>%
        #dplyr::mutate(ICweight = 100 / max(IC) * IC)
        dplyr::mutate(ICweight = scales::rescale(IC, to = c(0, 1)))
    
    # Write to file
    write.csv(
        phenotypeElegans,
        file.path(persistentDataDir, "phenotypeElegans.csv"),
        row.names = FALSE
    )
    
    # TODO: what is evidence, RNAi (possible off-target effects) or variant (known mutation) = weighing
    # of phenotype profile score
    
    # # Visualize IC
    # ggdensity(phenotypeElegans, x = "IC", add = "mean", rug = TRUE)
    
    
    # Zebrafish -----------------------------------------------------------------
    
    # Read file
    phenotypeFish <-
        read.csv(file.path(persistentDataDir, "phenotypeFishMaster.csv")) %>%
        # Only keep abnormal phenotype
        dplyr::filter(Phenotype.Tag == "abnormal") %>%
        # Only keep phenotype with gene annotation
        dplyr::filter(Gene.ID != "") %>%
        # Only keep phenotype with Phenotype.ID (this will remove expression and labeling data)
        dplyr::filter(Phenotype.ID != "") %>%
        # Make lean, remove non-essential column
        # TODO: note this currently is over-simplified! Consider how to add extra information for user.
        dplyr::select(Fish.ID,
                      Fish.Name,
                      Gene.ID,
                      Gene.Symbol,
                      Phenotype.ID,
                      Phenotype.Name) %>%
        dplyr::mutate(Taxon = "7955")
    
    # Biomart conversion of identifiers
    
    # If you receive an error message saying "Unexpected format to the list of available marts",
    # this is often because there is a problem with the BioMart server you are trying to connect to,
    # and something other than the list of available marts is being returned -
    # often some like a "down for maintainance" page.
    # If you browse to the provided URL and find a page that starts with "<MartRegistry>"
    # this is the correct listing
    # https://www.bioconductor.org/packages/devel/bioc/vignettes/biomaRt/inst/doc/biomaRt.html
    
    # listMarts()
    # datasets <- listDatasets(ensembl)
    # attributes <- listAttributes(ensembl) # list attributes
    # filters <- listFilters(ensembl) # list filters
    
    # Retrieve drerio_gene_ensembl
    ensembl <- biomaRt::useMart(biomart = "ensembl",
                                dataset = "drerio_gene_ensembl",
                                host = "www.ensembl.org")
    
    # Batch query, zfin_id_id to ensembl_gene_id
    output <-
        biomaRt::getBM(
            attributes = c("zfin_id_id", "ensembl_gene_id"),
            # columns to retrieve
            filters = "zfin_id_id",
            # input id type
            values = (phenotypeFish$Gene.ID %>%
                          unique %>%
                          as.vector),
            # input list
            #values = ("ZDB-GENE-991019-6"), # input list
            mart = ensembl
        )
    
    phenotypeFish <-
        dplyr::left_join(phenotypeFish,
                         output,
                         by = c("Gene.ID" = "zfin_id_id")) %>%
        dplyr::select(ensembl_gene_id,
                      Gene.Symbol,
                      Phenotype.ID,
                      Phenotype.Name,
                      Taxon) %>%
        unique
    
    # add Information Content (IC) to the phenotype table, useful for interpreting specificity of phenotype
    ic <-
        table(phenotypeFish$Phenotype.ID) %>%
        as.data.frame() %>%
        dplyr::mutate(IC = -log2(Freq / nrow(phenotypeFish)))
    
    phenotypeFish <-
        dplyr::left_join(phenotypeFish,
                         ic,
                         by = c("Phenotype.ID" = "Var1"))
    
    # Add weighted IC, used for ranking pathways
    # TODO: fix
    phenotypeFish <-
        phenotypeFish %>%
        #dplyr::mutate(ICweight = 100 / max(IC) * IC)
        dplyr::mutate(ICweight = scales::rescale(IC, to = c(0, 1)))
    
    # Add colnames
    colnames(phenotypeFish) <-
        c(
            "DB_Object_ID",
            "DB_Object_Symbol",
            "GO_ID",
            "GO_ID_Name",
            "Taxon",
            "Freq",
            "IC",
            "ICweight"
        )
    
    # Write to file
    write.csv(
        phenotypeFish,
        file.path(persistentDataDir, "phenotypeFish.csv"),
        row.names = FALSE
    )
    
    
    # Mouse MGI -----------------------------------------------------------------
    
    # MGI : http://www.informatics.jax.org/downloads/
    
    # List of All Mouse Phenotypic Alleles (tab-delimited), not used because these phenotypes are high-level
    # Use phenotypeMouseMGI instead, the code below is deprecated
    # lines <- readLines(file.path(rawDataDir, "MGI_PhenotypicAllele.rpt"))
    # linesBody <- lines[!grepl("^#|^.*#", lines)] # remove lines that start with #
    # MGI_PhenotypicAllele <- read.csv(text=linesBody, sep = "\t", header = FALSE,
    #                 col.names = c(
    #                  "MGI_Allele_Accession_ID",
    #                  "Allele_Symbol",
    #                  "Allele_Name",
    #                  "Allele_Type",
    #                  "Allele_Attribute",
    #                  "PubMed_ID_for_original_reference",
    #                  "MGI_Marker_Accession_ID",
    #                  "Marker_Symbol", ###
    #                  "Marker_RefSeq_ID",
    #                  "Marker_Ensembl_ID", ###
    #                  "Highlevel_Mammalian_Phenotype_ID", ###
    #                  "Synonyms",
    #                  "Marker_Name"
    #                 )
    # # make lean, remove columns that are not essential
    # ) %>% dplyr::select(MGI_Marker_Accession_ID, Marker_Ensembl_ID, 
    # Marker_Symbol, Highlevel_Mammalian_Phenotype_ID) %>%
    #  dplyr::filter(Marker_Ensembl_ID != "") %>% # remove rows without Ensembl ID
    #  dplyr::filter(Highlevel_Mammalian_Phenotype_ID != "") %>% # remove rows without phenotypes
    #  # split cell over columns
    #  dplyr::mutate(Highlevel_Mammalian_Phenotype_ID = strsplit(as.character(
    # Highlevel_Mammalian_Phenotype_ID), ",")) %>%
    #  unnest(Highlevel_Mammalian_Phenotype_ID) %>% # split cell over columns
    #  unique
    #
    # # add Information Content (IC) to the phenotype table, useful for interpreting specificity of phenotype
    # IC <- table(MGI_PhenotypicAllele$Highlevel_Mammalian_Phenotype_ID) %>%
    #  as.data.frame() %>% dplyr::mutate(IC = -log2(Freq/ nrow(MGI_PhenotypicAllele)))
    # MGI_PhenotypicAllele <- dplyr::left_join(MGI_PhenotypicAllele, IC,
    # by = c("Highlevel_Mammalian_Phenotype_ID" = "Var1"))
    # # add weighted IC, used for ranking pathways
    # MGI_PhenotypicAllele <- MGI_PhenotypicAllele %>% dplyr::mutate(ICweight = 100 / max(IC) * IC)
    # write.csv(MGI_PhenotypicAllele, file.path(persistentDataDir, "MGI_PhenotypicAllele.csv"), row.names = FALSE)
    
    # All Genotypes and Mammalian Phenotype Annotations (tab-delimited)
    
    # Load file
    phenotypeMouseMGI <-
        read.csv(
            file.path(rawDataDir, "MGI_PhenoGenoMP.rpt"),
            sep = "\t",
            header = F,
            col.names = c(
                "Allelic_Composition",
                "Allele_Symbol",
                "Genetic_Background",
                "Mammalian_Phenotype_ID",
                "PubMed_ID",
                "gene_id"
            )
        )
    
    # Clean Allele_Symbol, to remove allele and keep symbol
    phenotypeMouseMGI <-
        dplyr::mutate(phenotypeMouseMGI,
                      DB_Object_Symbol = sub("<.*", "", phenotypeMouseMGI$Allele_Symbol)) %>%
        # Make lean, remove non-essential columns
        dplyr::select(gene_id, Mammalian_Phenotype_ID, DB_Object_Symbol) %>%
        # Remove rows with double mutants, seperated by |
        dplyr::filter(!grepl(",", .$gene_id)) %>%
        unique
    
    # Biomart conversion of identifiers
    # datasets <- listDatasets(useMart("ensembl")) # list datasets for input next line
    
    # Retrieve mmusculus_gene_ensembl
    ensembl <-
        biomaRt::useMart("ensembl", dataset = "mmusculus_gene_ensembl")
    
    # attributes <- listAttributes(ensembl) # list attributes for input next line
    
    # Batch query "mgi_id" to "ensembl_gene_id"
    output <-
        biomaRt::getBM(
            attributes = c("mgi_id", "ensembl_gene_id"),
            # columns to retrieve
            filters = "mgi_id",
            # input id type
            values = (phenotypeMouseMGI$gene_id %>%
                          unique),
            # input list
            mart = ensembl
        )
    
    # Add column names
    colnames(output) <- c("gene_id", "DB_Object_ID")
    
    # Join
    phenotypeMouseMGI <-
        dplyr::left_join(phenotypeMouseMGI, output, by = "gene_id") %>%
        dplyr::select(DB_Object_ID,
                      Mammalian_Phenotype_ID,
                      DB_Object_Symbol)
    
    # Add MP identifier that matches MP name
    
    # Load ontology
    ontology <-
        ontologyIndex::get_OBO(file.path(rawDataDir, "mp.obo"), extract_tags = "everything")
    
    # Set root to extract "mouse life cycle stage"
    roots <-
        c("MP:0000001") %>%
        as.character()
    
    # Extract ids
    events <-
        ontologyIndex::get_descendants(ontology, roots, exclude_roots = TRUE) %>%
        as.data.frame()
    
    # Extract value and name
    output <- cbind(events,
                    tolower(
                        apply(events,
                              2,
                              ontologyPlot::get_shortened_names,
                              ontology = ontology)
                    ))
    
    # Add column name
    colnames(output) <- c("GO_ID", "GO_ID_Name")
    
    # Join
    phenotypeMouseMGI <-
        dplyr::left_join(phenotypeMouseMGI,
                         output,
                         by = c("Mammalian_Phenotype_ID" = "GO_ID")) %>%
        dplyr::mutate(Taxon = "10090") %>%
        unique
    
    # # add Information Content (IC) to the phenotype table, useful for interpreting specificity of phenotype
    # TODO: Do this when combining MGI with IMPC tables
    # ic <-
    #     table(phenotypeMouseMGI$Mammalian_Phenotype_ID) %>%
    #     as.data.frame() %>%
    #     dplyr::mutate(IC = -log2(Freq / nrow(phenotypeMouseMGI)))
    # 
    # # Join
    # phenotypeMouseMGI <-
    #     dplyr::left_join(phenotypeMouseMGI,
    #                      ic,
    #                      by = c("Mammalian_Phenotype_ID" = "Var1"))
    # 
    # # Add weighted IC, used for ranking pathways
    # phenotypeMouseMGI <-
    #     phenotypeMouseMGI %>%
    #     dplyr::mutate(ICweight = 100 / max(IC) * IC)
    
    # Add colnames
    colnames(phenotypeMouseMGI) <-
        c(
            "DB_Object_ID",
            "GO_ID",
            "DB_Object_Symbol",
            "GO_ID_Name",
            "Taxon"
            # "Freq",
            # "IC",
            # "ICweight"
        )
    
    # Write to file
    write.csv(phenotypeMouseMGI,
              file.path(persistentDataDir, "phenotypeMouseMGI.csv"),
              row.names = FALSE)
    
    
    # Mouse IMPC ------------------------------------------------------------------------------------------------------
    
    # international mouse phenotype consortium https://www.mousephenotype.org/
    # inprogress: load and process this dataset
    
    # Load file
    phenotypeMouseIMPC <-
        read.csv(file.path(rawDataDir, "IMPC_ALL_statistical_results.csv.gz"),
                 sep = ",",
                 header = TRUE) %>%
        # Make lean, remove columns that are not essential
        dplyr::select(marker_accession_id, marker_symbol, mp_term_name)
    
    # Replace empty cell with NA for tidying in next step
    phenotypeMouseIMPC[phenotypeMouseIMPC == ""] <-
        NA
    
    # Remove rows that are not complete (e.g. without phenotype)
    phenotypeMouseIMPC <-
        na.omit(phenotypeMouseIMPC) %>%
        unique
    
    # Biomart conversion of identifiers
    # datasets <- listDatasets(useMart("ensembl")) # list datasets for input next line
    
    # Retrieve mmusculus_gene_ensembl
    ensembl <-
        biomaRt::useMart("ensembl", dataset = "mmusculus_gene_ensembl")
    
    # attributes <- listAttributes(ensembl) # list attributes for input next line
    
    # Batch query "mgi_id" to "ensembl_gene_id"
    output <-
        biomaRt::getBM(
            attributes = c("mgi_id", "ensembl_gene_id"),
            # columns to retrieve
            filters = "mgi_id",
            # input id type
            values = (phenotypeMouseIMPC$marker_accession_id %>%
                          unique),
            # input list
            mart = ensembl
        )
    
    # Add column name
    colnames(output) <- c("gene_id", "DB_Object_ID")
    
    # Join
    phenotypeMouseIMPC <-
        dplyr::left_join(phenotypeMouseIMPC,
                         output,
                         by = c("marker_accession_id" = "gene_id")) %>%
        dplyr::select(DB_Object_ID, marker_symbol, mp_term_name)
    
    
    # add MP identifier that matches MP name
    
    # Load ontology
    ontology <-
        ontologyIndex::get_OBO(file.path(rawDataDir, "mp.obo"), extract_tags = "everything")
    
    # Set root to extract "mouse life cycle stage"
    roots <-
        c("MP:0000001") %>%
        as.character()
    
    # Extract ids
    events <-
        ontologyIndex::get_descendants(ontology, roots, exclude_roots = TRUE) %>%
        as.data.frame()
    
    # Extract value and name
    output <- cbind(events,
                    tolower(
                        apply(events,
                              2,
                              ontologyPlot::get_shortened_names,
                              ontology = ontology)
                    ))
    
    # Add column name
    colnames(output) <- c("GO_ID", "GO_ID_Name")
    
    # Join
    phenotypeMouseIMPC <-
        dplyr::left_join(phenotypeMouseIMPC, output, by = c("mp_term_name" = "GO_ID_Name")) %>%
        dplyr::mutate(Taxon = "10090") %>%
        unique
    
    # # add Information Content (IC) to the phenotype table, useful for interpreting specificity of phenotype
    # TODO: do IC in combined MGI/IMPC table
    # ic <-
    #     table(phenotypeMouseIMPC$mp_term_name) %>%
    #     as.data.frame() %>%
    #     dplyr::mutate(IC = -log2(Freq / nrow(phenotypeMouseIMPC)))
    # 
    # # Join
    # phenotypeMouseIMPC <-
    #     dplyr::left_join(phenotypeMouseIMPC, ic, by = c("mp_term_name" = "Var1"))
    # 
    # # Add weighted IC, used for ranking pathways
    # phenotypeMouseIMPC <-
    #     phenotypeMouseIMPC %>%
    #     dplyr::mutate(ICweight = 100 / max(IC) * IC)
    
    # Add colnames
    colnames(phenotypeMouseIMPC) <-
        c(
            "DB_Object_ID",
            "DB_Object_Symbol",
            "GO_ID_Name",
            "GO_ID",
            "Taxon"
            # "Freq",
            # "IC",
            # "ICweight"
        )
    
    # Write to file
    write.csv(phenotypeMouseIMPC,
              file.path(persistentDataDir, "phenotypeMouseIMPC.csv"),
              row.names = FALSE)
    
    # Combine phenotypeMouseMGI phenotypeMouseIMPC
    phenotypeMouse <- dplyr::bind_rows(phenotypeMouseMGI, phenotypeMouseIMPC) %>% na.omit %>% unique
    
    ic <-
        table(phenotypeMouse$GO_ID) %>%
        as.data.frame() %>%
        dplyr::mutate(IC = -log2(Freq / nrow(phenotypeMouse)))
    
    # Join
    phenotypeMouse <-
        dplyr::left_join(phenotypeMouse, ic, by = c("GO_ID" = "Var1"))
    
    # Add weighted IC, used for ranking pathways
    phenotypeMouse <-
        phenotypeMouse %>%
        #dplyr::mutate(ICweight = 100 / max(IC) * IC)
        dplyr::mutate(ICweight = scales::rescale(IC, to = c(0, 1)))
    
    # Write to file
    write.csv(phenotypeMouse,
              file.path(persistentDataDir, "phenotypeMouse.csv"),
              row.names = FALSE)
    
    # Human -----------------------------------------------------------------------------------------------------------
    
    # Load file
    lines <-
        readLines(file.path(rawDataDir, "genes_to_phenotype.txt"))
    
    # Grab body
    linesBody <-
        # Remove lines that start with #
        lines[!grepl("^#|^.*#", lines)]
    
    phenotypeHuman <-
        read.csv(text = linesBody,
                 sep = "\t",
                 header = FALSE) %>%
        dplyr::mutate(Taxon = 9606)
    
    colnames(phenotypeHuman) <-
        c(
            "gene_id",
            "DB_Object_Symbol",
            "GO_ID",
            "GO_ID_Name",
            "discard1",
            "discard2",
            "discard3",
            "discard4",
            "DB",
            "Taxon"
        )
    
    # Make lean
    phenotypeHuman <-
        phenotypeHuman %>%
        dplyr::select("gene_id",
                      "DB_Object_Symbol",
                      "GO_ID_Name",
                      "GO_ID",
                      "Taxon")
    
    # Biomart conversion of identifiers
    # datasets <- listDatasets(useMart("ensembl")) # list datasets for input next line
    
    # Retrieve hsapiens_gene_ensembl
    ensembl <-
        biomaRt::useMart("ensembl", dataset = "hsapiens_gene_ensembl")
    
    # attributes <- listAttributes(ensembl) # list attributes for input next line
    
    # Batch query
    output <-
        biomaRt::getBM(
            attributes = c("entrezgene_id", "ensembl_gene_id"),
            # columns to retrieve
            filters = "entrezgene_id",
            # input id type
            values = (phenotypeHuman$gene_id %>%
                          unique),
            # input list
            mart = ensembl
        )
    
    # Add column name
    colnames(output) <- c("gene_id", "DB_Object_ID")
    
    # Join
    phenotypeHuman <-
        dplyr::left_join(phenotypeHuman, output, by = "gene_id") %>%
        dplyr::select(DB_Object_ID,
                      DB_Object_Symbol,
                      GO_ID,
                      GO_ID_Name,
                      Taxon) %>%
        unique
    
    # add Information Content (IC) to the phenotype table, useful for interpreting specificity of phenotype
    ic <-
        table(phenotypeHuman$GO_ID) %>%
        as.data.frame() %>%
        dplyr::mutate(IC = -log2(Freq / nrow(phenotypeHuman)))
    
    # Join
    phenotypeHuman <- dplyr::left_join(phenotypeHuman, ic, by = c("GO_ID" = "Var1"))
    
    # Add weighted IC, used for ranking pathways
    phenotypeHuman <-
        phenotypeHuman %>%
        #dplyr::mutate(ICweight = 100 / max(IC) * IC)
        dplyr::mutate(ICweight = scales::rescale(IC, to = c(0, 1)))
    
    # phenotypeHuman$DB_Object_ID <- as.character(phenotypeHuman$DB_Object_ID) # convert to character
    
    # TODO: Should DB_Object_ID Entrez ID be converted to other identifier?
    
    # Write to file
    write.csv(phenotypeHuman, file.path(persistentDataDir, "phenotypeHuman.csv"), row.names = FALSE)
    
    
    # MASTER TABLE ----------------------------------------------------------------------------------------------------
    # combine all gene-phenotype data into one table
    
    # Load files
    phenotypeDicty <-
        read.csv(
            file.path(persistentDataDir, "phenotypeDicty.csv"),
            sep = ",",
            header = TRUE
        )
    
    phenotypeElegans <-
        read.csv(
            file.path(persistentDataDir, "phenotypeElegans.csv"),
            sep = ",",
            header = TRUE
        )
    
    phenotypeFish <-
        read.csv(
            file.path(persistentDataDir, "phenotypeFish.csv"),
            sep = ",",
            header = TRUE
        )
    
    phenotypeMouse <-
        read.csv(
            file.path(persistentDataDir, "phenotypeMouse.csv"),
            sep = ",",
            header = TRUE
        )
    
    phenotypeHuman <-
        read.csv(file.path(persistentDataDir, "phenotypeHuman.csv"),
                 sep = ",",
                 header = TRUE)
    
    # Combine
    genePheno <-
        do.call(
            "rbind",
            list(
                phenotypeDicty,
                phenotypeElegans,
                phenotypeFish,
                phenotypeMouse,
                phenotypeHuman
            )
        )  %>% tidyr::drop_na()
    
    genePheno$GO_ID_Name <- genePheno$GO_ID_Name %>% tolower()
    
    # Write to file
    write.csv(genePheno,
              file.path(persistentDataDir, "genePheno.csv"),
              row.names = FALSE)
    
    
    # Rat -----------------------------------------------------------------
    
    # TODO Rat : ftp://ftp.rgd.mcw.edu/pub/, which file contains phenotype information?
    # url <- "ftp://ftp.rgd.mcw.edu/pub/data_release/QTLS_RAT.txt" # not good: no significant LOD?
    # download.file(url, file.path(rawDataDir, "rgdQTLS_RAT.rpt"))
    # lines <- readLines(file.path(rawDataDir, "rgdQTLS_RAT.rpt"))
    # linesBody <- lines[!grepl("^#|^.*#", lines)] # remove lines that start with #
    # rgdQTLS_RAT <- read.csv(text=linesBody, sep = "\t", header = TRUE)
    # # TODO add Information Content (IC) to the phenotype table, useful for interpreting specificity of phenotype
    # write.csv(rgdQTLS_RAT, file.path(persistentDataDir, "rgdQTLS_RAT.csv"), row.names = FALSE)
    
    
    # Rabbit ----------------------------------------------------------------------------------------------------------
    
    # TODO Rabbit
    # no database available, only OECD guidelines in the QSAR toolbox
    
}


#' Create genePhenoSummary, used in the phenotypePathway script for creating gene profiles
#'
#' @return parameter
#' @author Monique van der Voet
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom dplyr select group_by summarise
#' @export
#' @examples
#' \dontrun{
#' GenerateGenePhenoSummary()
#' }
#'
GenerateGenePhenoSummary <- function(rawDataDir = "data-raw/extractTransform", persistentDataDir = "data-persist") {
    
    # Load file Phenotypes of genes
    genePheno <-
        read.csv(file.path(persistentDataDir, "genePheno.csv"),
                 sep = ",",
                 header = TRUE)
    
    # Create reference tables
    taxon <- unique(genePheno$Taxon) %>%
        as.data.frame
    
    output <- data.frame()
    
    for (i in seq_len(nrow(taxon))) {
        # checks each GO_ID in the reference table
        taxonGenes <-
            genePheno %>%
            subset(Taxon == taxon[i, ]) %>%
            dplyr::select(DB_Object_ID, GO_ID)
        taxonGenes <-
            # Remove empty rows
            taxonGenes[!apply(taxonGenes == "", 1, all), ]
        taxonGenes <-
            taxonGenes %>%
            dplyr::group_by(DB_Object_ID) %>%
            # Lists phenotypes for each gene
            dplyr::summarise(pheno = paste(GO_ID, collapse = ", ")) %>%
            dplyr::mutate(Taxon = taxon[i, ])
        output <- rbind(output, taxonGenes)
    }
    
    # Write to file
    write.csv(output,
              file.path(persistentDataDir, "genePhenoSummary.csv"),
              row.names = FALSE)
    
}


#' Remove raw data folder and mark the extractTransform pipeline complete
#'
#' @author Monique van der Voet
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#'
#' }
RemoveTemp <- function() {
    # Remove raw data folder
    unlink("data-raw/extractTransform/", recursive = TRUE)
    output <- "download and processing is done"
    write.csv(output,
              "data-persist/finished.txt",
              row.names = FALSE)
}



#' Fuction to query ontology
#'
#' @param query what to retrieve
#' @param result value retrieved by query
#' @param ontology to query
#' @return value
#' @author Monique van der Voet
#' @importFrom magrittr %>%
#' @export
#' @examples
#' \dontrun{
#'
#' }
GetProperty <- function(query, result, ontology) {
    ontology[[query]][result] %>%
        lapply(function(value)
            # if there is no value, return NA
            if (identical(value, character(0)))
                NA_character_ # return NA
            else
                value) # return value
}


#' Fuction to mutate a subset of rows
#'
#' @param .data todo
#' @param condition todo
#' @return .data
#' @author Monique van der Voet
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @export
#' @examples
#' \dontrun{
#'
#' }
MutateCond <-
    function(.data, condition, ..., envir = parent.frame()) {
        condition <- eval(substitute(condition), .data, envir)
        .data[condition, ] <-
            .data[condition, ] %>%
            dplyr::mutate(...)
        .data
    }
