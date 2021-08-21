#' Extract all information regarding the topic "Barrierefreiheit".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item Accessibility_exists
#'         \item Accessibility
#'         \item Ansprechpartner_Menschen_mit_Beeintraechtigung
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Barrierefreiheit(doc, 1L)
#'
#'
#' @export
#'
qb_extractor_Barrierefreiheit <- function(obj, Hospital_id) {

    Barrierefreiheit <-
        qb_extract_simple_section(
            xml_find_first(obj, '//Barrierefreiheit'),
            "Barrierefreiheit_Aspekt",
            c("BF_Schluessel", "Erlaeuterungen")
        )

    Accessibility <- Barrierefreiheit %>%
        mutate(idAccessibility = NA_integer_,
               Hospital_idHospital = Hospital_id) %>%
        rename("code" = .data$BF_Schluessel,
               "comment" = .data$Erlaeuterungen) %>%
        select(.data$idAccessibility, everything()) %>%
        filter(!is.na(.data$code))

    if (nrow(Accessibility) > 0) {

        Accessibility_exists <- TRUE

    } else {

        Accessibility_exists <- FALSE

        Accessibility <- NA_character_
    }


    Ansprechpartner_Menschen_mit_Beeintraechtigung_xml <- xml_find_first(
        obj,
        '//Barrierefreiheit/Ansprechpartner_Menschen_mit_Beeintraechtigung/Kontakt_Person_lang'
    )

    if (!is.na(Ansprechpartner_Menschen_mit_Beeintraechtigung_xml)) {

        Ansprechpartner_Menschen_mit_Beeintraechtigung <-
            qb_extract_person(Ansprechpartner_Menschen_mit_Beeintraechtigung_xml,
                              role = "Ansprechpartner_Menschen_mit_Beeintraechtigung"
            )

    } else {

        Ansprechpartner_Menschen_mit_Beeintraechtigung <- tibble("idPerson" = NA_integer_,
                                                                 "role" = NA_character_,
                                                                 "title" = NA_character_,
                                                                 "firstname" = NA_character_,
                                                                 "lastname" = NA_character_,
                                                                 "responsibility" = NA_character_,
                                                                 "phone" = NA_character_,
                                                                 "fax" = NA_character_,
                                                                 "email" = NA_character_)

    }


    return(list("Accessibility_exists" = Accessibility_exists,
                "Accessibility" = Accessibility,
                "Ansprechpartner_Menschen_mit_Beeintraechtigung" = Ansprechpartner_Menschen_mit_Beeintraechtigung))


}
