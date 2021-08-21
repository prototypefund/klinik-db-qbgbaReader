#' Extract all information regarding the topic "Fortbildung".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item ContinuingEducation_exists
#'         \item ContinuingEducation
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Fortbildung(doc, 1L)
#'
#'
#' @export
#'
qb_extractor_Fortbildung <- function(obj, Hospital_id) {

    Fortbildung <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Qualitaetssicherung'
        ),
        "Fortbildung",
        c("Fortbildungspflichtige",
          "Nachweispflichtige",
          "Fortbildungsnachweis_Erbracht_Habende")
    )

    if (any(!Fortbildung %>% mutate(across(everything(), is.na)))) {

        ContinuingEducation_exists <- TRUE

        ContinuingEducation <- Fortbildung %>%
            mutate(idContinuingEducation = NA_integer_,
                   Hospital_idHospital = Hospital_id) %>%
            rename("pplWithDutyForCE" = .data$Fortbildungspflichtige,
                   "pplWithBurdenOfProof" = .data$Nachweispflichtige,
                   "pplWithVerifiedTraining" = .data$Fortbildungsnachweis_Erbracht_Habende) %>%
            select(.data$idContinuingEducation, everything())

    } else {

        ContinuingEducation_exists <- FALSE

        ContinuingEducation <- NA_character_

    }

    return(list("ContinuingEducation_exists" = ContinuingEducation_exists,
                "ContinuingEducation" = ContinuingEducation))

}
