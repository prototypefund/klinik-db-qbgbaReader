#' Extract all information regarding the topic "Pflegepersonalregelung".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item FulfillmentLevelPpUG_exists
#'         \item FulfillmentLevelPpUG
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Pflegepersonalregelung(doc, 1L)
#'
#'
#' @export
#'
qb_extractor_Pflegepersonalregelung <- function(obj, Hospital_id) {

    Monatsbezogener_Erfuellungsgrad_PpUG <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Pflegepersonalregelung'
        ),
        "Monatsbezogener_Erfuellungsgrad_PpUG",
        c("Pflegesensitiver_Bereich",
          "Station",
          "Schicht",
          "Monatsbezogener_Erfuellungsgrad",
          "Ausnahmetatbestaende")
    )

    Schichtbezogener_Erfuellungsgrad_PpUG <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Pflegepersonalregelung'
        ),
        "Schichtbezogener_Erfuellungsgrad_PpUG",
        c("Pflegesensitiver_Bereich",
          "Station",
          "Schicht",
          "Schichtbezogener_Erfuellungsgrad")
    )

    Monatsbezogener_Erfuellungsgrad_PpUG <- Monatsbezogener_Erfuellungsgrad_PpUG %>%
        rename("Monatsbezogen" = .data$Monatsbezogener_Erfuellungsgrad)

    Schichtbezogener_Erfuellungsgrad_PpUG <- Schichtbezogener_Erfuellungsgrad_PpUG %>%
        rename("Schichtbezogen" = .data$Schichtbezogener_Erfuellungsgrad)


    Erfuellungsgrad_PpUG <- bind_rows(Monatsbezogener_Erfuellungsgrad_PpUG, Schichtbezogener_Erfuellungsgrad_PpUG) %>%
        pivot_longer(cols = c(.data$Monatsbezogen,
                              .data$Schichtbezogen),
                     names_to = "Bezugsrahmen",
                     values_to = "Erfuellungsgrad") %>%
        filter(!is.na(.data$Erfuellungsgrad))


    if (nrow(Erfuellungsgrad_PpUG) > 0) {

        FulfillmentLevelPpUG_exists <- TRUE

        FulfillmentLevelPpUG <- Erfuellungsgrad_PpUG %>%
            mutate(idFulfillmentLevelPpUG = NA_integer_,
                   Hospital_idHospital = Hospital_id) %>%
            rename("department" = .data$Pflegesensitiver_Bereich,
                   "hospitalWard" = .data$Station,
                   "shift" = .data$Schicht,
                   "exceptionalCases" = .data$Ausnahmetatbestaende,
                   "frameOfReference" = .data$Bezugsrahmen,
                   "degreeOfFulfillment" = .data$Erfuellungsgrad) %>%
            select(.data$idFulfillmentLevelPpUG, everything())

    } else {

        FulfillmentLevelPpUG_exists <- FALSE

        FulfillmentLevelPpUG <- NA_character_

    }


    return(list("FulfillmentLevelPpUG_exists" = FulfillmentLevelPpUG_exists,
                "FulfillmentLevelPpUG" = FulfillmentLevelPpUG))

}
