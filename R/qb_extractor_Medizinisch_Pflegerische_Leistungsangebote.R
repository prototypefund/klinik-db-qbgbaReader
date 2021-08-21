#' Extract all information regarding the topic "Medizinisch Pflegerische Leistungsangebote".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item NursingServices_exists
#'         \item NursingServices
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Medizinisch_Pflegerische_Leistungsangebote(doc, 1L)
#'
#'
#' @export
#'
qb_extractor_Medizinisch_Pflegerische_Leistungsangebote <- function(obj, Hospital_id) {

    # obj <- details
    # Hospital_id <- 1L

    Medizinisch_Pflegerische_Leistungsangebote <-
        qb_extract_simple_section(
            xml_find_first(obj, '//Medizinisch_Pflegerische_Leistungsangebote'),
            "MP_Leistungsangebot",
            c("MP_Schluessel", "Erlaeuterungen")
        )

    NursingServices <- Medizinisch_Pflegerische_Leistungsangebote %>%
        mutate(idNursingServices = NA_integer_,
               Hospital_idHospital = Hospital_id) %>%
        rename("code" = .data$MP_Schluessel,
               "comment" = .data$Erlaeuterungen) %>%
        select(.data$idNursingServices, everything()) %>%
        filter(!is.na(.data$code))

    if (nrow(NursingServices) > 0) {

        NursingServices_exists <- TRUE

    } else {

        NursingServices_exists <- FALSE

        NursingServices <- NA_character_

    }


    return(list("NursingServices_exists" = NursingServices_exists,
                "NursingServices" = NursingServices))

}
