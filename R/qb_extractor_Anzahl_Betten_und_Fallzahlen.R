#' Extract all information regarding the topic "Anzahl Betten und Fallzahlen".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item HospitalKPI_exists
#'         \item HospitalKPI
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Anzahl_Betten_und_Fallzahlen(doc, 1L)
#'
#'
#' @export
#'
qb_extractor_Anzahl_Betten_und_Fallzahlen <- function(obj, Hospital_id) {

    Anzahl_Betten <- xml_text(xml_find_first(obj, '//Anzahl_Betten'))
    Vollstationaere_Fallzahl <- xml_text(xml_find_first(obj, '//Fallzahlen/Vollstationaere_Fallzahl'))
    Teilstationaere_Fallzahl <- xml_text(xml_find_first(obj, '//Fallzahlen/Teilstationaere_Fallzahl'))
    Ambulante_Fallzahl <- xml_text(xml_find_first(obj, '//Fallzahlen/Ambulante_Fallzahl'))

    HospitalKPI <- tibble("idHospitalKPI" = NA_integer_,
                          "quantityBeds" = Anzahl_Betten,
                          "quantityCasesFull" = Vollstationaere_Fallzahl,
                          "quantityCasesPartial" = Teilstationaere_Fallzahl,
                          "quantityCasesOutpatient" = Ambulante_Fallzahl,
                          "Hospital_idHospital" = Hospital_id)

    if (any(!is.na(HospitalKPI$quantityBeds)) ||
        any(!is.na(HospitalKPI$quantityCasesFull)) ||
        any(!is.na(HospitalKPI$quantityCasesPartial)) ||
        any(!is.na(HospitalKPI$quantityCasesOutpatient))) {

        HospitalKPI_exists <- TRUE

    } else {

        HospitalKPI_exists <- FALSE

        HospitalKPI <- NA_character_

    }


    return(list("HospitalKPI_exists" = HospitalKPI_exists,
                "HospitalKPI" = HospitalKPI))

}
