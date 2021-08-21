#' Extract all information regarding the topic "Einrichtungsinternes Fehlermeldesystem".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item InstrumentsInternalComSystem_exists
#'         \item InstrumentsInternalComSystem
#'         \item InternalComSystem_exists
#'         \item InternalComSystem
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Einrichtungsinternes_Fehlermeldesystem(doc, 1L)
#'
#'
#' @export
#'
qb_extractor_Einrichtungsinternes_Fehlermeldesystem <- function(obj, Hospital_id) {

    RM_Einrichtungsinternes_Fehlermeldesystem_Massnahmen <-
        xml_text(xml_find_first(
            obj,
            '//Einrichtungsinternes_Fehlermeldesystem/Massnahmen'
        ))

    RM_Einrichtungsinternes_Fehlermeldesystem_Tagungsgremium_Frequenz <-
        xml_text(
            xml_find_first(
                obj,
                '//Einrichtungsinternes_Fehlermeldesystem/Tagungsgremium/Tagungsfrequenz'
            )
        )




    if (!is.na(RM_Einrichtungsinternes_Fehlermeldesystem_Massnahmen) ||
        !is.na(RM_Einrichtungsinternes_Fehlermeldesystem_Tagungsgremium_Frequenz)) {

        InstrumentsInternalComSystem_exists <- TRUE

        InstrumentsInternalComSystem <- tibble(idInstrumentsInternalComSystem = NA_integer_,
                                               "description" = RM_Einrichtungsinternes_Fehlermeldesystem_Massnahmen,
                                               "freqency" = RM_Einrichtungsinternes_Fehlermeldesystem_Tagungsgremium_Frequenz,
                                               Hospital_idHospital = Hospital_id)

    } else {

        InstrumentsInternalComSystem_exists <- FALSE

        InstrumentsInternalComSystem <- NA_character_

    }



    RM_Fehlermeldesystem_intern <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Umgang_mit_Risiken_in_der_Patientenversorgung/Einrichtungsinternes_Fehlermeldesystem/Fehlermeldesystem_intern'
            ),
            "System_intern",
            c(
                "IF_Schluessel",
                "Zusatzangaben_IF/Datum",
                "Zusatzangaben_IF/Frequenz"
            )
        ) %>%
        rename("Datum" = .data$Zusatzangaben_IF_Datum,
               "Frequenz" = .data$Zusatzangaben_IF_Frequenz)

    InternalComSystem <- RM_Fehlermeldesystem_intern %>%
        mutate(idInternalComSystem = NA_integer_,
               Hospital_idHospital = Hospital_id) %>%
        rename("code" = .data$IF_Schluessel,
               "date" = .data$Datum,
               "frequency" = .data$Frequenz) %>%
        select(.data$idInternalComSystem, everything()) %>%
        filter(!is.na(.data$code))

    if (nrow(InternalComSystem) > 0) {

        InternalComSystem_exists <- TRUE

    } else {

        InternalComSystem_exists <- FALSE

        InternalComSystem <- NA_character_

    }


    return(list("InstrumentsInternalComSystem_exists" = InstrumentsInternalComSystem_exists,
                "InstrumentsInternalComSystem" = InstrumentsInternalComSystem,
                "InternalComSystem_exists" = InternalComSystem_exists,
                "InternalComSystem" = InternalComSystem))

}

