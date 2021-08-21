#' Extract all information regarding the topic "Einrichtungsuebergreifendes Fehlermeldesystem".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item ExternalComSystem_exists
#'         \item ExternalComSystem
#'         \item RM_Einrichtungsuebergreifendes_Fehlermeldesystem_Tagungsfrequenz
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Einrichtungsuebergreifendes_Fehlermeldesystem(doc, Hospital_id = 1L)
#'
#'
#' @export
#'
qb_extractor_Einrichtungsuebergreifendes_Fehlermeldesystem <- function(obj, Hospital_id) {

    # obj <- details
    # Hospital_id <- 1L

    RM_Einrichtungsuebergreifendes_Fehlermeldesystem <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Umgang_mit_Risiken_in_der_Patientenversorgung/Einrichtungsuebergreifendes_Fehlermeldesystem/Fehlermeldesystem_uebergreifend'
            ),
            "System_uebergreifend",
            c("EF_Schluessel",
              "EF_Sonstiges/EF_Sonstiges_Schluessel",
              "EF_Sonstiges/Erlaeuterungen")
        ) %>%
        rename("EF_Sonstige_Schluessel" = .data$EF_Sonstiges_EF_Sonstiges_Schluessel,
               "Erlaeuterungen" = .data$EF_Sonstiges_Erlaeuterungen) %>%
        pivot_longer(-.data$Erlaeuterungen, names_to = "EF_Schluessel_Art", values_to = "EF_Schluessel") %>%
        mutate(Erlaeuterungen = case_when(
            is.na(.data$EF_Schluessel) ~ NA_character_,
            TRUE ~ .data$Erlaeuterungen
        )) %>%
        filter(!is.na(.data$EF_Schluessel)) %>%
        select(.data$EF_Schluessel, .data$EF_Schluessel_Art, .data$Erlaeuterungen)


    RM_Einrichtungsuebergreifendes_Fehlermeldesystem_Tagungsfrequenz <-
        xml_text(
            xml_find_first(
                obj,
                '//Einrichtungsuebergreifendes_Fehlermeldesystem/Tagungsgremium/Tagungsfrequenz'
            )
        )

    ExternalComSystem <- RM_Einrichtungsuebergreifendes_Fehlermeldesystem %>%
        mutate(idExternalComSystem = NA_integer_,
               frequency = RM_Einrichtungsuebergreifendes_Fehlermeldesystem_Tagungsfrequenz,
               Hospital_idHospital = Hospital_id) %>%
        rename("code" = .data$EF_Schluessel,
               "type" = .data$EF_Schluessel_Art,
               "comment" = .data$Erlaeuterungen) %>%
        select(.data$idExternalComSystem, everything()) %>%
        filter(!is.na(.data$code))

    if (nrow(ExternalComSystem) > 0) {

        ExternalComSystem_exists <- TRUE

    } else {

        ExternalComSystem_exists <- FALSE

        ExternalComSystem <- NA_character_

    }


    return(list("ExternalComSystem_exists" = ExternalComSystem_exists,
                "ExternalComSystem" = ExternalComSystem,
                "RM_Einrichtungsuebergreifendes_Fehlermeldesystem_Tagungsfrequenz" = RM_Einrichtungsuebergreifendes_Fehlermeldesystem_Tagungsfrequenz))

}
