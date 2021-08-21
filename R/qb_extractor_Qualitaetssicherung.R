#' Extract all information regarding the topic "Qualitaetssicherung".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item AdditionalExternalQualityManagement_exists
#'         \item AdditionalExternalQualityManagement
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Qualitaetssicherung(doc, 1L)
#'
#'
#' @export
#'
qb_extractor_Qualitaetssicherung <- function(obj, Hospital_id) {

    Sonstige_Verfahren_Externe_QS <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Qualitaetssicherung/Sonstige_Verfahren_Externe_QS'
            ),
            "Sonstiges_Verfahren_Externe_QS",
            c(
                "Leistungsbereich",
                "Bezeichnung_Qualitaetsindikator",
                "Ergebnis",
                "Messzeitraum",
                "Datenerhebung",
                "Rechenregeln",
                "Referenzbereiche",
                "Quellenangabe_Dokumentation"
            )
        )

    AdditionalExternalQualityManagement <- Sonstige_Verfahren_Externe_QS %>%
        mutate(
            idAdditionalExternalQualityManagement = NA_integer_,
            Hospital_idHospital = Hospital_id
        ) %>%
        rename(
            "section" = .data$Leistungsbereich,
            "description" = .data$Bezeichnung_Qualitaetsindikator,
            "result" = .data$Ergebnis,
            "measurementPeriod" = .data$Messzeitraum,
            "dataGathering" = .data$Datenerhebung,
            "calculationRules" = .data$Rechenregeln,
            "referenceRange" = .data$Referenzbereiche,
            "documentationReference" = .data$Quellenangabe_Dokumentation
        ) %>%
        select(.data$idAdditionalExternalQualityManagement, everything()) %>%
        filter(!is.na(.data$description))


    if (nrow(AdditionalExternalQualityManagement) > 0) {

        AdditionalExternalQualityManagement_exists <- TRUE

    } else {

        AdditionalExternalQualityManagement_exists <- FALSE

        AdditionalExternalQualityManagement <- NA_character_

    }


    return(list("AdditionalExternalQualityManagement_exists" = AdditionalExternalQualityManagement_exists,
                "AdditionalExternalQualityManagement" = AdditionalExternalQualityManagement))

}
