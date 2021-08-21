#' Extract all information regarding the topic "QS nach Landesrecht".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item Teilnahme_landesspezifische_Qualitaetssicherungsmassnahme
#'         \item ExternalQMbyState_exists
#'         \item ExternalQMbyState
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_QS_nach_Landesrecht(doc, 1L)
#'
#'
#' @export
#'
qb_extractor_QS_nach_Landesrecht <- function(obj, Hospital_id) {

    # obj <- details
    # Hospital_id <- 1L

    Teilnahme_landesspezifische_Qualitaetssicherungsmassnahme <-
        ifelse(!is.na(
            xml_find_first(
                obj,
                '//QS_nach_Landesrecht/Keine_Teilnahme_landesspezifische_Qualitaetssicherungsmassnahme'
            )
        ),
        0L, 1L)


    if (Teilnahme_landesspezifische_Qualitaetssicherungsmassnahme == 1L) {

        ExternalQMbyState <- qb_extract_simple_section(
            xml_find_first(
                obj,
                '//QS_nach_Landesrecht/Teilnahme_landesspezifische_Qualitaetssicherungsmassnahme'
            ),
            "Leistungsbereich",
            c("Bezeichnung",
              "Teilnahme_externe_Qualitaetssicherung")
        ) %>%
            rename("description" = .data$Bezeichnung,
                   "participation" = .data$Teilnahme_externe_Qualitaetssicherung) %>%
            mutate(idExternalQMbyState = NA_integer_,
                   Hospital_idHospital = Hospital_id) %>%
            select(.data$idExternalQMbyState, everything())

        if (any(!is.na(ExternalQMbyState$description)) ||
            any(!is.na(ExternalQMbyState$participation))) {

            ExternalQMbyState_exists <- TRUE

        } else {

            ExternalQMbyState_exists <- FALSE

            ExternalQMbyState <- NA_character_

        }

    } else {

        ExternalQMbyState_exists <- FALSE

        ExternalQMbyState <- NA_character_

    }

    return(list("Teilnahme_landesspezifische_Qualitaetssicherungsmassnahme" = Teilnahme_landesspezifische_Qualitaetssicherungsmassnahme,
                "ExternalQMbyState_exists" = ExternalQMbyState_exists,
                "ExternalQMbyState" = ExternalQMbyState))

}
