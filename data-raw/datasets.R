# sys.source('myPreprocessingCode.R',envir=topenv())
# keepDataObjects('mydataset')

#' BioC2015OpenCyto
#' Analyzing Designed Experiments with OpenCyto
#' @docType package
#' @aliases BioC2015OpenCyto-package
#' @title BioC2015OpenCyto
#' @name BioC2015OpenCyto
#' @description Data and code demonstrating the analysis of designed experiments using OpenCyto.
#' @details Demonstrates how to use  the OpenCyto framework to gate FCM data utilizing study metadata to structure the gating to use controls
#' @import flowWorkspace openCyto
#' @seealso \link{tbdata}
NULL

#' Subset of the TB study data.
#'@name tbdata
#'@docType data
#'@title Data from an assay.
#'@format a \code{data.table} containing the following fields
#'\describe{
#'\item{column_name}{description}
#'\item{column_name_2}{description}
#'}
#'@source {
#'bibentry(bibtype = "article",
#'         title = "COMPASS identifies T-cell subsets correlated with clinical outcomes",
#'         key = "Lin:hy",
#'         author = c(person("Lynn", "Lin"),
#'           person("Greg", "Finak"),
#'           person("Kevin","Ushey"),
#'           person("Chetan","Seshadri"),
#'           person("Thomas R", "Hawn"),
#'           person("Nicole","Frahm"),
#'           person("Thomas J","Scriba"),
#'           person("Mahomed","Hassan"),
#'           person("Willem","Hanekom"),
#'           person("Pierre-Alexandre","Bart"),
#'           person("Giuseppe","Pantaleo"),
#'           person("Georgia D","Tomaras"),
#'           person("Supachai","Rerks-Ngarm"),
#'           person("Jaranit","Kaewkungwal"),
#'           person("Sorachai","Nitayaphan"),
#'           person("Punnee","Pitisuttithum"),
#'           person("Nelson L","Michael"),
#'           person("Jerome","Kim"),
#'           person("Merlin L","Robb"),
#'           person("Robert J", "O'Connell"),
#'           person("Nicos","Karasavvas"),
#'           person("Peter","Gilbert"),
#'           person("Stephen C","De Rosa"),
#'           person("M Juliana","McElrath"),
#'           person("Raphael", "Gottardo"),
#'           year = "2015",
#'           volume = "33",
#'           number = "6",
#'           pages = "610--616")
#'}
#'@seealso \link{COMPASS}
