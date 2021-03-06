% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/qb_extractor_Einrichtungsuebergreifendes_Fehlermeldesystem.R
\name{qb_extractor_Einrichtungsuebergreifendes_Fehlermeldesystem}
\alias{qb_extractor_Einrichtungsuebergreifendes_Fehlermeldesystem}
\title{Extract all information regarding the topic "Einrichtungsuebergreifendes Fehlermeldesystem".}
\usage{
qb_extractor_Einrichtungsuebergreifendes_Fehlermeldesystem(obj, Hospital_id)
}
\arguments{
\item{obj}{An XML-object comprising a complete detailed report.}

\item{Hospital_id}{An integer value that represents the primary key of a
hospital in the designated database.}
}
\value{
A list with the following elements:
    \itemize{
        \item ExternalComSystem_exists
        \item ExternalComSystem
        \item RM_Einrichtungsuebergreifendes_Fehlermeldesystem_Tagungsfrequenz
    }
}
\description{
Extract all information regarding the topic "Einrichtungsuebergreifendes Fehlermeldesystem".
}
\examples{

doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
                        package = "qbgbaReader", mustWork = TRUE)
doc <- xml2::read_xml(doc_path)

qb_extractor_Einrichtungsuebergreifendes_Fehlermeldesystem(doc, Hospital_id = 1L)


}
