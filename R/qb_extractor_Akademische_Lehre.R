#' Extract all information regarding the topic "Akademische Lehre".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item AcademicTeaching_exists
#'         \item AcademicTeaching
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Akademische_Lehre(doc, Hospital_id = 1L)
#'
#'
#' @export
#'
qb_extractor_Akademische_Lehre <- function(obj, Hospital_id) {

    Akademische_Lehre <- qb_extract_simple_section(xml_find_first(obj, '//Akademische_Lehre'),
                                                   "Akademische_Lehre_Wissenschaftliche_Taetigkeit",
                                                   c("FL_Schluessel", "Erlaeuterungen"))

    AcademicTeaching <- Akademische_Lehre %>%
        mutate(idAcademicTeaching = NA_integer_,
               Hospital_idHospital = Hospital_id) %>%
        rename("code" = .data$FL_Schluessel,
               "comment" = .data$Erlaeuterungen) %>%
        select(.data$idAcademicTeaching, everything()) %>%
        filter(!is.na(.data$code))

    if (nrow(AcademicTeaching) > 0) {

        AcademicTeaching_exists <- TRUE

    } else {

        AcademicTeaching_exists <- FALSE

        AcademicTeaching <- NA_character_

    }


    return(list("AcademicTeaching_exists" = AcademicTeaching_exists,
                "AcademicTeaching" = AcademicTeaching))

}
