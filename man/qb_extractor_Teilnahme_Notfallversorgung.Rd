% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qb_extractor_Teilnahme_Notfallversorgung.R
\name{qb_extractor_Teilnahme_Notfallversorgung}
\alias{qb_extractor_Teilnahme_Notfallversorgung}
\title{Extract all information regarding the topic "Teilnahme Notfallversorgung".}
\usage{
qb_extractor_Teilnahme_Notfallversorgung(obj, Hospital_id)
}
\arguments{
\item{obj}{An XML-object comprising a complete detailed report.}

\item{Hospital_id}{An integer value that represents the primary key of a
hospital in the designated database.}
}
\value{
A list with the following elements:
    \itemize{
        \item emergencyCareSpecialModules_exists
        \item emergencyCareSpecialModules
        \item emergencyCareLevelPrerequisitesAssurances_exists
        \item emergencyCareLevelPrerequisitesAssurances
        \item emergencyCare_exists
        \item emergencyCare
    }
}
\description{
Extract all information regarding the topic "Teilnahme Notfallversorgung".
}
\examples{

doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
                        package = "qbgbaReader", mustWork = TRUE)
doc <- xml2::read_xml(doc_path)

qb_extractor_Teilnahme_Notfallversorgung(doc, 1L)


}
