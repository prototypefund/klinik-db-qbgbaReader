#' Extract all information regarding the topic "Apparative Ausstattung".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item MedicalDevices_exists
#'         \item MedicalDevices
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Apparative_Ausstattung(doc, 1L)
#'
#'
#' @export
#'
qb_extractor_Apparative_Ausstattung <- function(obj, Hospital_id) {

    Apparative_Ausstattung <-
        qb_extract_simple_section(
            xml_find_first(obj, '//Apparative_Ausstattung'),
            "Geraet",
            c(
                "AA_Schluessel",
                "Notfallverfuegbarkeit_24h",
                "Erlaeuterungen"
            )
        )

    MedicalDevices <- Apparative_Ausstattung %>%
        mutate(idMedicalDevices = NA_integer_,
               Hospital_idHospital = Hospital_id) %>%
        rename(
            "code" = .data$AA_Schluessel,
            "24hEmergencyPresence" = .data$Notfallverfuegbarkeit_24h,
            "comment" = .data$Erlaeuterungen
        ) %>%
        select(.data$idMedicalDevices, everything()) %>%
        filter(!is.na(.data$code))

    if (nrow(MedicalDevices) > 0) {

        MedicalDevices_exists <- TRUE

    } else {

        MedicalDevices_exists <- FALSE

        MedicalDevices <- NA_character_

    }


    return(list("MedicalDevices_exists" = MedicalDevices_exists,
                "MedicalDevices" = MedicalDevices))

}
