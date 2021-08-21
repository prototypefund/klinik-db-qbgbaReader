#' Extract all information regarding the topic "Ausbildung andere Heilberufe".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item TrainingOtherHealthProfessionals_exists
#'         \item TrainingOtherHealthProfessionals
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Ausbildung_andere_Heilberufe(doc, 1L)
#'
#'
#' @export
#'
qb_extractor_Ausbildung_andere_Heilberufe <- function(obj, Hospital_id) {


    Ausbildung_andere_Heilberufe <- qb_extract_simple_section(xml_find_first(obj, '//Ausbildung_andere_Heilberufe'),
                                                              "Ausbildung_in_anderen_Heilberufen",
                                                              c("HB_Schluessel", "Erlaeuterungen"))

    TrainingOtherHealthProfessionals <- Ausbildung_andere_Heilberufe %>%
        mutate(idTrainingOtherHealthProfessionals = NA_integer_,
               Hospital_idHospital = Hospital_id) %>%
        rename("code" = .data$HB_Schluessel,
               "comment" = .data$Erlaeuterungen) %>%
        select(.data$idTrainingOtherHealthProfessionals, everything()) %>%
        filter(!is.na(.data$code))

    if (nrow(TrainingOtherHealthProfessionals) > 0) {

        TrainingOtherHealthProfessionals_exists <- TRUE

    } else {

        TrainingOtherHealthProfessionals_exists <- FALSE

        TrainingOtherHealthProfessionals <- NA_character_

    }


    return(list("TrainingOtherHealthProfessionals_exists" = TrainingOtherHealthProfessionals_exists,
                "TrainingOtherHealthProfessionals" = TrainingOtherHealthProfessionals))

}
