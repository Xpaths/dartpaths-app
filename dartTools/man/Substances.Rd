% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classSubstances.R
\name{Substances}
\alias{Substances}
\title{Class for storing substance data}
\description{
Class for storing substance data

Class for storing substance data
}
\section{Super class}{
\code{\link[dartTools:Data]{dartTools::Data}} -> \code{Substances}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{classname}}{Name of the class}

\item{\code{metadata}}{Named list with metadata}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-addIdTypes}{\code{Substances$addIdTypes()}}
\item \href{#method-addSubstanceCategory}{\code{Substances$addSubstanceCategory()}}
\item \href{#method-addConstituents}{\code{Substances$addConstituents()}}
\item \href{#method-addSubstances}{\code{Substances$addSubstances()}}
\item \href{#method-addSubstanceActivity}{\code{Substances$addSubstanceActivity()}}
\item \href{#method-getMonoSubstanceSmiles}{\code{Substances$getMonoSubstanceSmiles()}}
\item \href{#method-mapSmilesToSubstance}{\code{Substances$mapSmilesToSubstance()}}
\item \href{#method-getSubstanceActivity}{\code{Substances$getSubstanceActivity()}}
\item \href{#method-getSubstanceIds}{\code{Substances$getSubstanceIds()}}
\item \href{#method-getSubstanceExtIds}{\code{Substances$getSubstanceExtIds()}}
\item \href{#method-tabulateSubstanceInfo}{\code{Substances$tabulateSubstanceInfo()}}
\item \href{#method-tabulateSubstanceCategory}{\code{Substances$tabulateSubstanceCategory()}}
\item \href{#method-maxSubstanceid}{\code{Substances$maxSubstanceid()}}
\item \href{#method-updateDescriptors}{\code{Substances$updateDescriptors()}}
\item \href{#method-getDescriptors}{\code{Substances$getDescriptors()}}
\item \href{#method-getSimilarConstituents}{\code{Substances$getSimilarConstituents()}}
\item \href{#method-getSimilarSubstances}{\code{Substances$getSimilarSubstances()}}
\item \href{#method-getSameCategorySubstances}{\code{Substances$getSameCategorySubstances()}}
\item \href{#method-inspect}{\code{Substances$inspect()}}
\item \href{#method-mergeSubstances}{\code{Substances$mergeSubstances()}}
\item \href{#method-.mergeSubstancesOnce}{\code{Substances$.mergeSubstancesOnce()}}
\item \href{#method-.updateSubstanceids}{\code{Substances$.updateSubstanceids()}}
\item \href{#method-clone}{\code{Substances$clone()}}
}
}
\if{html}{
\out{<details ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="dartTools" data-topic="Data" data-id="addData">}\href{../../dartTools/html/Data.html#method-addData}{\code{dartTools::Data$addData()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="dartTools" data-topic="Data" data-id="addTable">}\href{../../dartTools/html/Data.html#method-addTable}{\code{dartTools::Data$addTable()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="dartTools" data-topic="Data" data-id="checkTableConventions">}\href{../../dartTools/html/Data.html#method-checkTableConventions}{\code{dartTools::Data$checkTableConventions()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="dartTools" data-topic="Data" data-id="dump">}\href{../../dartTools/html/Data.html#method-dump}{\code{dartTools::Data$dump()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="dartTools" data-topic="Data" data-id="getData">}\href{../../dartTools/html/Data.html#method-getData}{\code{dartTools::Data$getData()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="dartTools" data-topic="Data" data-id="initialize">}\href{../../dartTools/html/Data.html#method-initialize}{\code{dartTools::Data$initialize()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="dartTools" data-topic="Data" data-id="loadDump">}\href{../../dartTools/html/Data.html#method-loadDump}{\code{dartTools::Data$loadDump()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="dartTools" data-topic="Data" data-id="ncols">}\href{../../dartTools/html/Data.html#method-ncols}{\code{dartTools::Data$ncols()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="dartTools" data-topic="Data" data-id="nrows">}\href{../../dartTools/html/Data.html#method-nrows}{\code{dartTools::Data$nrows()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="dartTools" data-topic="Data" data-id="print">}\href{../../dartTools/html/Data.html#method-print}{\code{dartTools::Data$print()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="dartTools" data-topic="Data" data-id="printAllTables">}\href{../../dartTools/html/Data.html#method-printAllTables}{\code{dartTools::Data$printAllTables()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="dartTools" data-topic="Data" data-id="writeTxtTable">}\href{../../dartTools/html/Data.html#method-writeTxtTable}{\code{dartTools::Data$writeTxtTable()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-addIdTypes"></a>}}
\if{latex}{\out{\hypertarget{method-addIdTypes}{}}}
\subsection{Method \code{addIdTypes()}}{
method to add external substance identifiers
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$addIdTypes(names)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{names}}{character vector with new substance identifiers}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
NULL
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-addSubstanceCategory"></a>}}
\if{latex}{\out{\hypertarget{method-addSubstanceCategory}{}}}
\subsection{Method \code{addSubstanceCategory()}}{
method to add substance category data
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$addSubstanceCategory(
  categoryInfo = NULL,
  categoryMembers = NULL,
  categoryProfiles = NULL,
  verbose = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{categoryInfo}}{NULL or data.table with columns category, description, url}

\item{\code{categoryMembers}}{NULL or data.table with column category (name/identifier of category) and EC/CAS (and optionally name)}

\item{\code{categoryProfiles}}{NULL or data.table with columns category, name,typicalconcentration and concentrationrange and EC/CAS}

\item{\code{verbose}}{Toggle verbosity (TRUE/FALSE)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
NULL
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-addConstituents"></a>}}
\if{latex}{\out{\hypertarget{method-addConstituents}{}}}
\subsection{Method \code{addConstituents()}}{
method to add constituents
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$addConstituents(smiles, returnTable = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{smiles}}{Character vector with molecules in SMILES notation}

\item{\code{returnTable}}{Logical, if TRUE, a table is returned; if FALSE, a vector of constituentid values is returned}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
data.table object or integer vector
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-addSubstances"></a>}}
\if{latex}{\out{\hypertarget{method-addSubstances}{}}}
\subsection{Method \code{addSubstances()}}{
method to add substances to database
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$addSubstances(
  inputTable,
  structureColumn = "smiles",
  verbose = FALSE,
  returnTable = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{inputTable}}{data.table or data.frame with new substances, see details}

\item{\code{structureColumn}}{should be "smiles" (if a column exist with that name) or None (otherwise)}

\item{\code{verbose}}{Toggle verbosity}

\item{\code{returnTable}}{Logical, if TRUE, a table is returned; if FALSE, a vector of substanceid values is returned}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
table should contain the following columns 
\itemize{
\item{"smiles"}{Molecular structures in SMILES notation (Optional)}
\item{"substancetype"}{(Optional) type of substances (allowed values are "uvcb","mono","multi", NA). If not specified, NA is used.}
\item{"url"}{(Optional) URL of webpage with original data}
}
In addition, additional columns can be included with external substance identifiers.
By default the following identifiers are supported: "CAS","EC","name",
Additional identifiers can be registered with the \code{addIdTypes} method.
}

\subsection{Returns}{
data.table object or integer vector
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-addSubstanceActivity"></a>}}
\if{latex}{\out{\hypertarget{method-addSubstanceActivity}{}}}
\subsection{Method \code{addSubstanceActivity()}}{
add substance activity data
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$addSubstanceActivity(
  inputTable,
  substanceIdentifier,
  type = c("invivo", "clp", "invitro", "phenotypes")
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{inputTable}}{Data.table object}

\item{\code{substanceIdentifier}}{Character string indicating which column should be used as a substance identifier ("substanceid" indicates non-persistent internal identifiers)}

\item{\code{type}}{Character string indicating type of activity data. Currently allowed: "invivo","clp","invitro"}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
NULL
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getMonoSubstanceSmiles"></a>}}
\if{latex}{\out{\hypertarget{method-getMonoSubstanceSmiles}{}}}
\subsection{Method \code{getMonoSubstanceSmiles()}}{
get smiles for constituents of mono-constituent substances
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$getMonoSubstanceSmiles(substanceids = NULL, uniqueBy = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{substanceids}}{Vector with integer substance identifiers}

\item{\code{uniqueBy}}{Character string. If NULL, all matches are returned. If "substanceid" or "smiles", the first match per substanceid or smiles is returned.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
NULL
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-mapSmilesToSubstance"></a>}}
\if{latex}{\out{\hypertarget{method-mapSmilesToSubstance}{}}}
\subsection{Method \code{mapSmilesToSubstance()}}{
lookup first mono-constituent substance for each smiles
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$mapSmilesToSubstance(smilesVector)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{smilesVector}}{Vector with smiles (character strings)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
vector with substanceids
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getSubstanceActivity"></a>}}
\if{latex}{\out{\hypertarget{method-getSubstanceActivity}{}}}
\subsection{Method \code{getSubstanceActivity()}}{
create a list of tables with substance activity information
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$getSubstanceActivity(
  substanceids = NULL,
  summarize = FALSE,
  skipDartNA = FALSE,
  web = TRUE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{substanceids}}{Vector with integer substance identifiers}

\item{\code{summarize}}{If TRUE, a summary table is added to the returned list}

\item{\code{skipDartNA}}{If TRUE, only DART-active and DART-inactive records are shown}

\item{\code{web}}{If TRUE, some elements are added for usage in a web application (hyperlinks, embedded images)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Named list of data.table objects for each of the types of activity data (invitro, invivo, clp, ...) and optionally a summary
<div class="tooltip">',
<span class="tooltiptext">hazardcode</span></div>',
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getSubstanceIds"></a>}}
\if{latex}{\out{\hypertarget{method-getSubstanceIds}{}}}
\subsection{Method \code{getSubstanceIds()}}{
get all substance ids
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$getSubstanceIds(
  substancetype = self$metadata$allowedValues$substancetype
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{substancetype}}{Optional vector with types of substances. E.g., c("uvcb","mono","multi")}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
NULL
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getSubstanceExtIds"></a>}}
\if{latex}{\out{\hypertarget{method-getSubstanceExtIds}{}}}
\subsection{Method \code{getSubstanceExtIds()}}{
get all external substance ids
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$getSubstanceExtIds(
  idType,
  firstMatch = FALSE,
  returnURL = FALSE,
  requireURL = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{idType}}{Character string with name of external id. E.g. "CAS", "EC", "name"}

\item{\code{firstMatch}}{If TRUE, the first substanceid is returned if multiple external ids map to the same internal substanceid}

\item{\code{returnURL}}{If TRUE, also a column url will be returned}

\item{\code{requireURL}}{If TRUE, rows with url==NA will be left out of the returned table}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a data.table with column "substanceid" and column matching idType
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-tabulateSubstanceInfo"></a>}}
\if{latex}{\out{\hypertarget{method-tabulateSubstanceInfo}{}}}
\subsection{Method \code{tabulateSubstanceInfo()}}{
create a table with substance information
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$tabulateSubstanceInfo(
  substanceids,
  web = FALSE,
  limitOutput = TRUE,
  priorityField = NULL,
  priorityPattern = NULL
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{substanceids}}{Vector with integer substance identifiers}

\item{\code{web}}{If TRUE, some elements are added for usage in a web application (hyperlinks, embedded images)}

\item{\code{limitOutput}}{If TRUE, some less informative information is left out}

\item{\code{priorityField}}{Field ("name", "CAS" or "EC") used for sorting results}

\item{\code{priorityPattern}}{Value in priorityField for which matches will be listed before non-matching items.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
a data.table object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-tabulateSubstanceCategory"></a>}}
\if{latex}{\out{\hypertarget{method-tabulateSubstanceCategory}{}}}
\subsection{Method \code{tabulateSubstanceCategory()}}{
create list of data.tables with substance/UVCB category information
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$tabulateSubstanceCategory(categories)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{categories}}{Name of category}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
list of data.tables with substance category information
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-maxSubstanceid"></a>}}
\if{latex}{\out{\hypertarget{method-maxSubstanceid}{}}}
\subsection{Method \code{maxSubstanceid()}}{
get maximum integer substanceid value
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$maxSubstanceid()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
integer value
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-updateDescriptors"></a>}}
\if{latex}{\out{\hypertarget{method-updateDescriptors}{}}}
\subsection{Method \code{updateDescriptors()}}{
Update stored constituent descriptors (e.g. fingerprints) for all constituents in database
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$updateDescriptors(type = "morgan")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{type}}{descriptor type used for similarity search. Allowed values: "morgan" (Morgan/ECFP fingerprints)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
NULL
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getDescriptors"></a>}}
\if{latex}{\out{\hypertarget{method-getDescriptors}{}}}
\subsection{Method \code{getDescriptors()}}{
Get constituent descriptors (e.g. fingerprints)
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$getDescriptors(constituentids = NULL, type = "morgan")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{constituentids}}{vector integer ids of query constituents}

\item{\code{type}}{descriptor type used for similarity search. Allowed values: "morgan" (Morgan/ECFP fingerprints)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
data.table with descriptors
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getSimilarConstituents"></a>}}
\if{latex}{\out{\hypertarget{method-getSimilarConstituents}{}}}
\subsection{Method \code{getSimilarConstituents()}}{
Find similar constituents
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$getSimilarConstituents(constituentids, type = "morgan")}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{constituentids}}{vector integer ids of query constituents}

\item{\code{type}}{descriptor type used for similarity search (currently, only 'morgan' is supported)}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
if the query vector constituentids has length >1, each returned row contains the maximum similarity against these query constituents
}

\subsection{Returns}{
data.table with columns constituentid and similarity
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getSimilarSubstances"></a>}}
\if{latex}{\out{\hypertarget{method-getSimilarSubstances}{}}}
\subsection{Method \code{getSimilarSubstances()}}{
Find similar constituents
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$getSimilarSubstances(substanceids, type = "morgan", digits = 3)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{substanceids}}{vector integer ids of query substances}

\item{\code{type}}{descriptor type used for similarity search (currently, only 'morgan' is supported)}

\item{\code{digits}}{Number of digits kept when rounding similarity values}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
each returned row contains the maximum similarity between constituents of query substances and returned substances
}

\subsection{Returns}{
data.table with columns substanceid and similarity
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getSameCategorySubstances"></a>}}
\if{latex}{\out{\hypertarget{method-getSameCategorySubstances}{}}}
\subsection{Method \code{getSameCategorySubstances()}}{
get substances (e.g. UVCBs) that are part of the same category as the provided substances
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$getSameCategorySubstances(substanceids)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{substanceids}}{vector with substanceid values (integers)}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-inspect"></a>}}
\if{latex}{\out{\hypertarget{method-inspect}{}}}
\subsection{Method \code{inspect()}}{
print summary of database contents and warnings about inconsistencies etc.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$inspect()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-mergeSubstances"></a>}}
\if{latex}{\out{\hypertarget{method-mergeSubstances}{}}}
\subsection{Method \code{mergeSubstances()}}{
merge substances that have matching identifiers and mono-constituent substances with identical standardized smiles
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$mergeSubstances()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-.mergeSubstancesOnce"></a>}}
\if{latex}{\out{\hypertarget{method-.mergeSubstancesOnce}{}}}
\subsection{Method \code{.mergeSubstancesOnce()}}{
loops once over external identifiers and smiles to merge substances with overlapping identifiers
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$.mergeSubstancesOnce()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-.updateSubstanceids"></a>}}
\if{latex}{\out{\hypertarget{method-.updateSubstanceids}{}}}
\subsection{Method \code{.updateSubstanceids()}}{
utility function for updating substance ids (used for merging of substances). Handle with care to avoid database corruption.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$.updateSubstanceids(mapping)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{mapping}}{data.table object with columns substanceid newsubstanceid}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Substances$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
