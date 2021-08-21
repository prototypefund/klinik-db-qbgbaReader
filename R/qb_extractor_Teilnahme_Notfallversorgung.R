#' Extract all information regarding the topic "Teilnahme Notfallversorgung".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item emergencyCareSpecialModules_exists
#'         \item emergencyCareSpecialModules
#'         \item emergencyCareLevelPrerequisitesAssurances_exists
#'         \item emergencyCareLevelPrerequisitesAssurances
#'         \item emergencyCare_exists
#'         \item emergencyCare
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Teilnahme_Notfallversorgung(doc, 1L)
#'
#'
#' @export
#'
qb_extractor_Teilnahme_Notfallversorgung <- function(obj, Hospital_id) {


    # obj <- details
    # Hospital_id <- 1L

    Notfallstufe_nicht_vereinbart <- ifelse(is.na(xml_find_first(obj, "//Teilnahme_Notfallversorgung/Teilnahme_Notfallstufe/Notfallstufe_nicht_vereinbart")), 0L, 1L)

    Basisnotfallversorgung_Stufe_1 <- ifelse(is.na(xml_find_first(obj,       "//Teilnahme_Notfallversorgung/Teilnahme_Notfallstufe/Notfallstufe_zugeordnet/Basisnotfallversorgung_Stufe_1")), 0L, 1L)
    Erweiterte_Notfallversorgung_Stufe_2 <- ifelse(is.na(xml_find_first(obj, "//Teilnahme_Notfallversorgung/Teilnahme_Notfallstufe/Notfallstufe_zugeordnet/Erweiterte_Notfallversorgung_Stufe_2")), 0L, 1L)
    Umfassende_Notfallversorgung_Stufe_3 <- ifelse(is.na(xml_find_first(obj, "//Teilnahme_Notfallversorgung/Teilnahme_Notfallstufe/Notfallstufe_zugeordnet/Umfassende_Notfallversorgung_Stufe_3")), 0L, 1L)

    Notfall_searchstring <- "Basisnotfallversorgung_Stufe_1"

    Notfallstufe_Erfuellung_Voraussetzung_1 <-
        ifelse(is.na(xml_find_first(
            obj,
            paste0(
                "//Teilnahme_Notfallversorgung/Teilnahme_Notfallstufe",
                "/Notfallstufe_zugeordnet/",
                Notfall_searchstring,
                "/Umstand_Zuordnung_Notfallstufe",
                "/Erfuellung_Voraussetzung"
            )
        )), 0L, 1L)

    emergencyCareLevelPrerequisites_1 <- tibble("emergencyCareLevel" = 1L,
                                                "emergencyCareLevelPrerequisitesGiven" = Notfallstufe_Erfuellung_Voraussetzung_1)

    Module_Spezielle_Notfallversorgung_1 <-
        tibble("Module_Spezielle_Notfallversorgung" = xml_text(xml_find_all(
            obj,
            paste0(
                "//Teilnahme_Notfallversorgung/Teilnahme_Notfallstufe",
                "/Notfallstufe_zugeordnet/",
                Notfall_searchstring,
                "/Umstand_Zuordnung_Notfallstufe",
                "/Erfuellung_Voraussetzung_Modul_Spezielle_Notfallversorgung",
                "/Teilnahme_Spezielle_Notfallversorgung",
                "/Module_Spezielle_Notfallversorgung"
            )
        ))) %>%
        rename("code" = .data$Module_Spezielle_Notfallversorgung) %>%
        mutate(emergencyCareLevel = 1L)

    Erfuellung_Voraussetzung_Sicherstellung_1 <- ifelse(is.na(xml_find_first(
        obj,
        paste0(
            "//Teilnahme_Notfallversorgung/Teilnahme_Notfallstufe",
            "/Notfallstufe_zugeordnet/",
            Notfall_searchstring,
            "/Umstand_Zuordnung_Notfallstufe",
            "/Erfuellung_Voraussetzung_Sicherstellung"
        )
    )), 0L, 1L)

    emergencyCareLevelAssurance_1 <- tibble("emergencyCareLevel" = 1L,
                                            "emergencyCareLevelAssuranceGiven" = Erfuellung_Voraussetzung_Sicherstellung_1)


    Notfall_searchstring <- "Erweiterte_Notfallversorgung_Stufe_2"

    Notfallstufe_Erfuellung_Voraussetzung_2 <-
        ifelse(is.na(xml_find_first(
            obj,
            paste0(
                "//Teilnahme_Notfallversorgung/Teilnahme_Notfallstufe",
                "/Notfallstufe_zugeordnet/",
                Notfall_searchstring,
                "/Umstand_Zuordnung_Notfallstufe",
                "/Erfuellung_Voraussetzung"
            )
        )), 0L, 1L)

    emergencyCareLevelPrerequisites_2 <- tibble("emergencyCareLevel" = 2L,
                                                "emergencyCareLevelPrerequisitesGiven" = Notfallstufe_Erfuellung_Voraussetzung_2)

    Module_Spezielle_Notfallversorgung_2 <-
        tibble("Module_Spezielle_Notfallversorgung" = xml_text(xml_find_all(
            obj,
            paste0(
                "//Teilnahme_Notfallversorgung/Teilnahme_Notfallstufe",
                "/Notfallstufe_zugeordnet/",
                Notfall_searchstring,
                "/Umstand_Zuordnung_Notfallstufe",
                "/Erfuellung_Voraussetzung_Modul_Spezielle_Notfallversorgung",
                "/Teilnahme_Spezielle_Notfallversorgung",
                "/Module_Spezielle_Notfallversorgung"
            )
        ))) %>%
        rename("code" = .data$Module_Spezielle_Notfallversorgung) %>%
        mutate(emergencyCareLevel = 2L)

    Erfuellung_Voraussetzung_Sicherstellung_2 <- ifelse(is.na(xml_find_first(
        obj,
        paste0(
            "//Teilnahme_Notfallversorgung/Teilnahme_Notfallstufe",
            "/Notfallstufe_zugeordnet/",
            Notfall_searchstring,
            "/Umstand_Zuordnung_Notfallstufe",
            "/Erfuellung_Voraussetzung_Sicherstellung"
        )
    )), 0L, 1L)

    emergencyCareLevelAssurance_2 <- tibble("emergencyCareLevel" = 2L,
                                            "emergencyCareLevelAssuranceGiven" = Erfuellung_Voraussetzung_Sicherstellung_2)


    Notfall_searchstring <- "Umfassende_Notfallversorgung_Stufe_3"

    Notfallstufe_Erfuellung_Voraussetzung_3 <-
        ifelse(is.na(xml_find_first(
            obj,
            paste0(
                "//Teilnahme_Notfallversorgung/Teilnahme_Notfallstufe",
                "/Notfallstufe_zugeordnet/",
                Notfall_searchstring,
                "/Umstand_Zuordnung_Notfallstufe",
                "/Erfuellung_Voraussetzung"
            )
        )), 0L, 1L)

    emergencyCareLevelPrerequisites_3 <- tibble("emergencyCareLevel" = 3L,
                                                "emergencyCareLevelPrerequisitesGiven" = Notfallstufe_Erfuellung_Voraussetzung_3)

    Module_Spezielle_Notfallversorgung_3 <-
        tibble("Module_Spezielle_Notfallversorgung" = xml_text(xml_find_all(
            obj,
            paste0(
                "//Teilnahme_Notfallversorgung/Teilnahme_Notfallstufe",
                "/Notfallstufe_zugeordnet/",
                Notfall_searchstring,
                "/Umstand_Zuordnung_Notfallstufe",
                "/Erfuellung_Voraussetzung_Modul_Spezielle_Notfallversorgung",
                "/Teilnahme_Spezielle_Notfallversorgung",
                "/Module_Spezielle_Notfallversorgung"
            )
        ))) %>%
        rename("code" = .data$Module_Spezielle_Notfallversorgung) %>%
        mutate(emergencyCareLevel = 3L)

    Erfuellung_Voraussetzung_Sicherstellung_3 <- ifelse(is.na(xml_find_first(
        obj,
        paste0(
            "//Teilnahme_Notfallversorgung/Teilnahme_Notfallstufe",
            "/Notfallstufe_zugeordnet/",
            Notfall_searchstring,
            "/Umstand_Zuordnung_Notfallstufe",
            "/Erfuellung_Voraussetzung_Sicherstellung"
        )
    )), 0L, 1L)

    emergencyCareLevelAssurance_3 <- tibble("emergencyCareLevel" = 3L,
                                            "emergencyCareLevelAssuranceGiven" = Erfuellung_Voraussetzung_Sicherstellung_3)


    rm(Notfall_searchstring)


    emergencyCareLevelPrerequisites <- bind_rows(emergencyCareLevelPrerequisites_1,
                                                 emergencyCareLevelPrerequisites_2,
                                                 emergencyCareLevelPrerequisites_3)

    emergencyCareSpecialModules <- bind_rows(Module_Spezielle_Notfallversorgung_1,
                                                    Module_Spezielle_Notfallversorgung_2,
                                                    Module_Spezielle_Notfallversorgung_3) %>%
        select(.data$emergencyCareLevel, everything())

    emergencyCareLevelAssurance <- bind_rows(emergencyCareLevelAssurance_1,
                                             emergencyCareLevelAssurance_2,
                                             emergencyCareLevelAssurance_3)

    emergencyCareLevelPrerequisitesAssurances <- emergencyCareLevelPrerequisites %>%
        full_join(emergencyCareLevelAssurance, by = "emergencyCareLevel")


    if (Basisnotfallversorgung_Stufe_1 == 0L) {

        emergencyCareLevelPrerequisitesAssurances <-
            emergencyCareLevelPrerequisitesAssurances[emergencyCareLevelPrerequisitesAssurances$emergencyCareLevel != 1L, , drop = FALSE]

    }

    if (Erweiterte_Notfallversorgung_Stufe_2 == 0L) {

        emergencyCareLevelPrerequisitesAssurances <-
            emergencyCareLevelPrerequisitesAssurances[emergencyCareLevelPrerequisitesAssurances$emergencyCareLevel != 2L, , drop = FALSE]

    }

    if (Umfassende_Notfallversorgung_Stufe_3 == 0L) {

        emergencyCareLevelPrerequisitesAssurances <-
            emergencyCareLevelPrerequisitesAssurances[emergencyCareLevelPrerequisitesAssurances$emergencyCareLevel != 3L, , drop = FALSE]

    }


    if (nrow(emergencyCareLevelPrerequisitesAssurances) > 0) {

        emergencyCareLevelPrerequisitesAssurances_exists <- TRUE

        emergencyCareLevelPrerequisitesAssurances <- emergencyCareLevelPrerequisitesAssurances %>%
            mutate(idEmergencyCareLevelPrerequisitesAssurances = NA_integer_,
                   "Hospital_idHospital" = Hospital_id) %>%
            select(.data$idEmergencyCareLevelPrerequisitesAssurances, everything())


    } else {

        emergencyCareLevelPrerequisitesAssurances_exists <- FALSE

        emergencyCareLevelPrerequisitesAssurances <- NA_character_

    }

    if (nrow(emergencyCareSpecialModules) > 0) {

        emergencyCareSpecialModules_exists <- TRUE

        emergencyCareSpecialModules <- emergencyCareSpecialModules %>%
            mutate(idEmergencyCareSpecialModules = NA_integer_,
                   "Hospital_idHospital" = Hospital_id) %>%
            select(.data$idEmergencyCareSpecialModules, everything())

    } else {

        emergencyCareSpecialModules_exists <- FALSE

        emergencyCareSpecialModules <- NA_character_

    }



    Tatbestand_Spezialversorgung_xml <- xml_find_first(
        obj,
        "//Teilnahme_Notfallversorgung/Voraussetzungen_Spezialversorgung_erfuellt/Tatbestand_Spezialversorgung"
        )

    if (!is.na(Tatbestand_Spezialversorgung_xml)) {

        Tatbestand_Spezialversorgung <- xml_text(Tatbestand_Spezialversorgung_xml)

    } else {

        Tatbestand_Spezialversorgung <- NA_character_

    }


    Kooperation_mit_Kassenaerztlicher_Vereinigung <-
        ifelse(is.na(
            xml_find_first(
                obj,
                '//Teilnahme_Notfallversorgung/Kooperation_mit_Kassenaerztlicher_Vereinigung'
            )
        ), 0L, 1L)

    if (Kooperation_mit_Kassenaerztlicher_Vereinigung == 1L) {

        Notdienstpraxis_Existiert <-
            xml_text(
                xml_find_first(
                    obj,
                    '//Teilnahme_Notfallversorgung/Kooperation_mit_Kassenaerztlicher_Vereinigung/Notdienstpraxis_Existiert'
                )
            )

        Vertragsaerztlicher_Notdienst_Existiert <-
            xml_text(
                xml_find_first(
                obj,
                '//Teilnahme_Notfallversorgung/Kooperation_mit_Kassenaerztlicher_Vereinigung/Vertragsaerztlicher_Notdienst_Existiert'
            )
        )

    } else {

        Notdienstpraxis_Existiert <- NA_character_
        Vertragsaerztlicher_Notdienst_Existiert <- NA_character_

    }


    emergencyCare <- tibble("idemergencyCare" = NA_integer_,
                            "noEmergencyCareLevel" = Notfallstufe_nicht_vereinbart,
                            "emergencyCareLevelOne" = Basisnotfallversorgung_Stufe_1,
                            "emergencyCareLevelTwo" = Erweiterte_Notfallversorgung_Stufe_2,
                            "emergencyCareLevelThree" = Umfassende_Notfallversorgung_Stufe_3,
                            "specializedCare" = Tatbestand_Spezialversorgung,
                            "cooperationKV" = Kooperation_mit_Kassenaerztlicher_Vereinigung,
                            "emergencyMedicalOffice" = Notdienstpraxis_Existiert,
                            "emergencyServices" = Vertragsaerztlicher_Notdienst_Existiert,
                            "Hospital_idHospital" = Hospital_id)

    if (any(!is.na(emergencyCare$noEmergencyCareLevel))) {

        emergencyCare_exists <- TRUE

    } else {

        emergencyCare_exists <- FALSE

        emergencyCare <- NA_character_

    }


    return(list("emergencyCareSpecialModules_exists" = emergencyCareSpecialModules_exists,
                "emergencyCareSpecialModules" = emergencyCareSpecialModules,
                "emergencyCareLevelPrerequisitesAssurances_exists" = emergencyCareLevelPrerequisitesAssurances_exists,
                "emergencyCareLevelPrerequisitesAssurances" = emergencyCareLevelPrerequisitesAssurances,
                "emergencyCare_exists" = emergencyCare_exists,
                "emergencyCare" = emergencyCare))

}
