#' Extract all information regarding the topic "Disease Management Programs (DMP)".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item DiseaseManagementPrograms_exists
#'         \item DiseaseManagementPrograms
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_DMP(doc, Hospital_id = 1L)
#'
#'
#' @export
#'
qb_extractor_DMP <- function(obj, Hospital_id) {

    # obj <- details
    # Hospital_id <- 1L

    DMPs <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Qualitaetssicherung/DMP'
        ),
        "Teilnahme_DMP",
        c("Bezeichnung",
          "Erlaeuterungen")
        )

    DiseaseManagementPrograms <- DMPs %>%
        mutate(idDiseaseManagementPrograms = NA_integer_,
               Hospital_idHospital = Hospital_id) %>%
        rename("description" = .data$Bezeichnung,
               "comment" = .data$Erlaeuterungen) %>%
        select(.data$idDiseaseManagementPrograms, everything()) %>%
        filter(!is.na(.data$description))

    if (nrow(DiseaseManagementPrograms) > 0) {

        DiseaseManagementPrograms_exists <- TRUE

    } else {

        DiseaseManagementPrograms_exists <- FALSE

        DiseaseManagementPrograms <- NA_character_

    }


    return(list("DiseaseManagementPrograms_exists" = DiseaseManagementPrograms_exists,
                "DiseaseManagementPrograms" = DiseaseManagementPrograms))

}
