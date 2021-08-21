#' Extract all information regarding the topic "Arzneimitteltherapiesicherheit".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item Arzneimittelkommission
#'         \item PharmacotherapySteeringBoard_exists
#'         \item PharmacotherapySteeringBoard
#'         \item Kein_Gremium_Arbeitsgruppe
#'         \item Verantwortliche_Person_AMTS
#'         \item Verantwortliche_Person_AMTS_nicht_festgelegt
#'         \item pharmaceuticalStaff_exists
#'         \item pharmaceuticalStaff
#'         \item PharmacotherapySafetyMeasures_exists
#'         \item PharmacotherapySafetyMeasures
#'         \item InstrumentsRM_exists
#'         \item InstrumentsRM
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Arzneimitteltherapiesicherheit(doc, 1L)
#'
#'
#' @export
#'
qb_extractor_Arzneimitteltherapiesicherheit <- function(obj, Hospital_id) {


    # obj <- details
    # Hospital_id <- 1L

    ### Arzneimittelkommission

    Arzneimittelkommission <-
        ifelse(is.na(
            xml_find_first(
                obj,
                '//Arzneimitteltherapiesicherheit/Verantwortliches_Gremium_AMTS/Arzneimittelkommission'
            )
        ), 0L, 1L)


    Anderes_Gremium_Arbeitsgruppe <- ifelse(is.na(xml_find_first(
        obj,
        '//Arzneimitteltherapiesicherheit/Verantwortliches_Gremium_AMTS/Anderes_Gremium_Arbeitsgruppe'
    )), 0L, 1L)

    if (Anderes_Gremium_Arbeitsgruppe == 1L) {

        Name_verantwortliches_Gremium_Arbeitsgruppe <- xml_text(xml_find_first(
            obj,
            '//Arzneimitteltherapiesicherheit/Verantwortliches_Gremium_AMTS/Anderes_Gremium_Arbeitsgruppe/Name_verantwortliches_Gremium_Arbeitsgruppe'
        ))

        Beteiligte_Abteilungen_Funktionsbereiche <- xml_text(xml_find_first(
            obj,
            '//Arzneimitteltherapiesicherheit/Verantwortliches_Gremium_AMTS/Anderes_Gremium_Arbeitsgruppe/Beteiligte_Abteilungen_Funktionsbereiche'
        ))

    } else {

        Name_verantwortliches_Gremium_Arbeitsgruppe <- NA_character_

        Beteiligte_Abteilungen_Funktionsbereiche <- NA_character_

    }


    Kein_Gremium_Arbeitsgruppe <-
        ifelse(is.na(
            xml_find_first(
                obj,
                '//Arzneimitteltherapiesicherheit/Verantwortliches_Gremium_AMTS/Kein_Gremium_Arbeitsgruppe'
            )
        ), 0L, 1L)


    AMTS_Lenkungsgremium_xml <- xml_find_first(
        obj,
        '//Arzneimitteltherapiesicherheit/Verantwortliches_Gremium_AMTS/Arbeitsgruppe_Qualitaetsmanagement/Lenkungsgremium_Qualitaetsmanagement'
    )

    AMTS_Lenkungsgremium <- qb_extract_simple_section(
        AMTS_Lenkungsgremium_xml,
        "Lenkungsgremium",
        c(
            "Beteiligte_Abteilungen_Funktionsbereiche",
            "Tagungsfrequenz"
        )
    )

    PharmacotherapySteeringBoard <- AMTS_Lenkungsgremium %>%
        rename("involvedDepartments" = .data$Beteiligte_Abteilungen_Funktionsbereiche,
               "frequency" = .data$Tagungsfrequenz) %>%
        mutate(idPharmacotherapySteeringBoard = NA_integer_,
               Hospital_idHospital = Hospital_id) %>%
        select(.data$idPharmacotherapySteeringBoard, everything())


    if (!is.na(PharmacotherapySteeringBoard$frequency) ||
        !is.na(PharmacotherapySteeringBoard$involvedDepartments)) {

        PharmacotherapySteeringBoard_exists <- TRUE

    } else {

        PharmacotherapySteeringBoard_exists <- FALSE

        PharmacotherapySteeringBoard <- NA_character_

    }

    ### Verantwortliche_Person_AMTS

    Verantwortliche_Person_AMTS_xml <- xml_find_first(
        obj,
        "//Arzneimitteltherapiesicherheit/Verantwortliche_Person_AMTS/Verantwortliche_Person_festgelegt/Eigenstaendige_Position_AMTS/Kontakt_Person_lang"
    )

    if (is.na(Verantwortliche_Person_AMTS_xml)) {

        Verantwortliche_Person_AMTS_xml <- xml_find_first(
            obj,
            "//Arzneimitteltherapiesicherheit/Verantwortliche_Person_AMTS/Verantwortliche_Person_festgelegt/Person_Entspricht_Angaben_Qualitaetsmanagement/Verantwortliche_Person_Qualitaetsmanagement/Kontakt_Person_lang"
        )

    }

    if (!is.na(Verantwortliche_Person_AMTS_xml)) {

        Verantwortliche_Person_AMTS <-
            qb_extract_person(Verantwortliche_Person_AMTS_xml,
                              role = "Verantwortliche_Person_AMTS")

    } else {

        Verantwortliche_Person_AMTS <- tibble("idPerson" = NA_integer_,
                                              "role" = NA_character_,
                                              "title" = NA_character_,
                                              "firstname" = NA_character_,
                                              "lastname" = NA_character_,
                                              "responsibility" = NA_character_,
                                              "phone" = NA_character_,
                                              "fax" = NA_character_,
                                              "email" = NA_character_)

    }


    Verantwortliche_Person_AMTS_nicht_festgelegt <- ifelse(is.na(xml_find_first(
        obj,
        "//Arzneimitteltherapiesicherheit/Verantwortliche_Person_AMTS/Verantwortliche_Person_nicht_festgelegt"
    )), 0L, 1L)


    ### pharmaceuticalStaff

    Pharmazeutisches_Personal <-
        qb_extract_simple_section(
            xml_find_first(obj, '//Arzneimitteltherapiesicherheit'),
            "Pharmazeutisches_Personal",
            c(
                "Anzahl_Apotheker",
                "Anzahl_weiteres_pharmazeutisches_Personal",
                "Erlaeuterungen"
            )
        )

    pharmaceuticalStaff <- Pharmazeutisches_Personal %>%
        mutate(idPharmaceuticalStaff = NA_integer_,
               Hospital_idHospital = Hospital_id) %>%
        rename(
            "pharmacists" = .data$Anzahl_Apotheker,
            "additionalStaff" = .data$Anzahl_weiteres_pharmazeutisches_Personal,
            "comment" = .data$Erlaeuterungen
        ) %>%
        select(.data$idPharmaceuticalStaff, everything())

    if (any(!pharmaceuticalStaff[, -5] %>% mutate(across(everything(), is.na)))) {

        pharmaceuticalStaff_exists <- TRUE

    } else {

        pharmaceuticalStaff_exists <- FALSE

        pharmaceuticalStaff <- NA_character_

    }


    ### PharmacotherapySafetyMeasures

    Instrumente_Massnahmen_AMTS <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Arzneimitteltherapiesicherheit/Instrumente_Massnahmen_AMTS'
            ),
            "Instrument_Massnahme_AMTS",
            c("AMTS_Schluessel")
        )

    amts_massnahmen <-
        xml_find_all(
            obj,
            '//Arzneimitteltherapiesicherheit/Instrumente_Massnahmen_AMTS/Instrument_Massnahme_AMTS'
        )

    AS03_Massnahmen_SOP <-
        xml_text(xml_find_first(amts_massnahmen[xml_text(xml_find_all(amts_massnahmen, "./AMTS_Schluessel")) == "AS03"],
                                "./Zusatzangaben_AMTS/SOP/Name"))

    if (!is_empty(AS03_Massnahmen_SOP)) {

        if (is.na(AS03_Massnahmen_SOP)) {

            AS03_Massnahmen_SOP <-
                xml_text(
                    xml_find_first(
                        amts_massnahmen[xml_text(xml_find_all(amts_massnahmen, "./AMTS_Schluessel")) == "AS03"],
                        "./Zusatzangaben_AMTS/SOP_ohne_Datum/Name"
                    )
                )
            AS03_Massnahmen_Datum <- NA_character_

        } else {

            AS03_Massnahmen_Datum <-
                xml_text(xml_find_first(amts_massnahmen[xml_text(xml_find_all(amts_massnahmen, "./AMTS_Schluessel")) == "AS03"],
                                        "./Zusatzangaben_AMTS/SOP/Datum"))

        }

    } else {

        AS03_Massnahmen_SOP <- NA_character_
        AS03_Massnahmen_Datum <- NA_character_

    }

    AS03_Massnahmen <- tibble("code" = "AS03",
                              "SOP" = AS03_Massnahmen_SOP,
                              "Datum" = AS03_Massnahmen_Datum)
    rm(AS03_Massnahmen_SOP, AS03_Massnahmen_Datum)



    AS05_Massnahmen_SOP <-
        xml_text(xml_find_first(amts_massnahmen[xml_text(xml_find_all(amts_massnahmen, "./AMTS_Schluessel")) == "AS05"],
                                "./Zusatzangaben_AMTS/SOP/Name"))

    if (!is_empty(AS05_Massnahmen_SOP)) {

        if (is.na(AS05_Massnahmen_SOP)) {

            AS05_Massnahmen_SOP <-
                xml_text(
                    xml_find_first(
                        amts_massnahmen[xml_text(xml_find_all(amts_massnahmen, "./AMTS_Schluessel")) == "AS05"],
                        "./Zusatzangaben_AMTS/SOP_ohne_Datum/Name"
                    )
                )
            AS05_Massnahmen_Datum <- NA_character_

        } else {

            AS05_Massnahmen_Datum <-
                xml_text(xml_find_first(amts_massnahmen[xml_text(xml_find_all(amts_massnahmen, "./AMTS_Schluessel")) == "AS05"],
                                        "./Zusatzangaben_AMTS/SOP/Datum"))

        }

    } else {

        AS05_Massnahmen_SOP <- NA_character_
        AS05_Massnahmen_Datum <- NA_character_

    }

    AS05_Massnahmen <- tibble("code" = "AS05",
                              "SOP" = AS05_Massnahmen_SOP,
                              "Datum" = AS05_Massnahmen_Datum)
    rm(AS05_Massnahmen_SOP, AS05_Massnahmen_Datum)


    AS06_Massnahmen_SOP <-
        xml_text(xml_find_first(amts_massnahmen[xml_text(xml_find_all(amts_massnahmen, "./AMTS_Schluessel")) == "AS06"],
                                "./Zusatzangaben_AMTS/SOP/Name"))

    if (!is_empty(AS06_Massnahmen_SOP)) {

        if (is.na(AS06_Massnahmen_SOP)) {

            AS06_Massnahmen_SOP <-
                xml_text(
                    xml_find_first(
                        amts_massnahmen[xml_text(xml_find_all(amts_massnahmen, "./AMTS_Schluessel")) == "AS06"],
                        "./Zusatzangaben_AMTS/SOP_ohne_Datum/Name"
                    )
                )
            AS06_Massnahmen_Datum <- NA_character_

        } else {

            AS06_Massnahmen_Datum <-
                xml_text(xml_find_first(amts_massnahmen[xml_text(xml_find_all(amts_massnahmen, "./AMTS_Schluessel")) == "AS06"],
                                        "./Zusatzangaben_AMTS/SOP/Datum"))

        }

    } else {

        AS06_Massnahmen_SOP <- NA_character_
        AS06_Massnahmen_Datum <- NA_character_

    }

    AS06_Massnahmen <- tibble("code" = "AS06",
                              "SOP" = AS06_Massnahmen_SOP,
                              "Datum" = AS06_Massnahmen_Datum)
    rm(AS06_Massnahmen_SOP, AS06_Massnahmen_Datum, amts_massnahmen)



    AS09_Massnahmen <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Arzneimitteltherapiesicherheit/Instrumente_Massnahmen_AMTS/Instrument_Massnahme_AMTS/Zusatzangaben_AMTS/Konzepte_Zubereitung_Arzneimittel'
            ),
            "Konzept_Zubereitung_Arzneimittel",
            c("Konzept",
              "Sonstiges")
        ) %>%
        mutate(code = "AS09")

    AS10_Massnahmen <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Arzneimitteltherapiesicherheit/Instrumente_Massnahmen_AMTS/Instrument_Massnahme_AMTS/Zusatzangaben_AMTS/Elektronische_Unterstuetzungen_Versorgung_Arzneimittel'
            ),
            "Elektronische_Unterstuetzung_Versorgung_Arzneimittel",
            c("Art",
              "Sonstige_elektronische_Unterstuetzung")
        )
    AS10_Massnahmen <- AS10_Massnahmen %>%
        fill(.data$Art, .direction = "downup") %>%
        fill(.data$Sonstige_elektronische_Unterstuetzung, .direction = "downup") %>%
        distinct() %>%
        mutate(code = "AS10")

    AS12_Massnahmen <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Arzneimitteltherapiesicherheit/Instrumente_Massnahmen_AMTS/Instrument_Massnahme_AMTS/Zusatzangaben_AMTS/Massnahmen_Minimierung_Medikationsfehler'
            ),
            "Massnahme_Minimierung_Medikationsfehler",
            c("Massnahme",
              "Andere_Massnahme")
        ) %>%
        mutate(code = "AS12")

    # %>%
    #     mutate(Massnahme = case_when(
    #         !is.na(.data$Massnahme) & is.na(.data$Andere_Massnahme) ~ .data$Massnahme,
    #         is.na(.data$Massnahme) & !is.na(.data$Andere_Massnahme) ~ .data$Andere_Massnahme,
    #         TRUE ~ .data$Massnahme
    #     )) %>%
    #     select(-.data$Andere_Massnahme)

    AS13_Massnahmen <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Arzneimitteltherapiesicherheit/Instrumente_Massnahmen_AMTS/Instrument_Massnahme_AMTS/Zusatzangaben_AMTS/Massnahmen_Arzneimitteltherapie_Entlassung'
            ),
            "Massnahme_Arzneimitteltherapie_Entlassung",
            c("Massnahme",
              "Sonstiges")
        ) %>%
        mutate(code = "AS13")

    PharmacotherapySafetyMeasures <- bind_rows(AS03_Massnahmen,
                                               AS05_Massnahmen,
                                               AS06_Massnahmen) %>%
        mutate(SOP_Datum = case_when(
            !is.na(.data$SOP) | !is.na(.data$Datum) ~ paste0("SOP: ", .data$SOP, " (Datum: ", .data$Datum, ")"),
            TRUE ~ .data$SOP)) %>%
        select(-.data$SOP, -.data$Datum) %>%
        full_join(AS09_Massnahmen, by = "code") %>%
        full_join(AS10_Massnahmen, by = "code") %>%
        full_join(AS12_Massnahmen, by = "code") %>%
        full_join(AS13_Massnahmen, by = c("code", "Sonstiges", "Massnahme")) %>%
        pivot_longer(
            cols = -.data$code,
            names_to = "furtherInformation",
            values_to = "description"
        ) %>%
        filter(!is.na(.data$description))


    PharmacotherapySafetyMeasures <- Instrumente_Massnahmen_AMTS %>%
        rename("code" = .data$AMTS_Schluessel) %>%
        left_join(PharmacotherapySafetyMeasures, by = "code") %>%
        arrange(.data$code) %>%
        mutate(
            idPharmacotherapySafetyMeasures = NA_integer_,
            Hospital_idHospital = Hospital_id
        ) %>%
        select(.data$idPharmacotherapySafetyMeasures, everything()) %>%
        filter(!is.na(.data$code))

    if (nrow(PharmacotherapySafetyMeasures) > 0) {

        PharmacotherapySafetyMeasures_exists <- TRUE

    } else {

        PharmacotherapySafetyMeasures_exists <- FALSE

        PharmacotherapySafetyMeasures <- NA_character_

    }



    ### InstrumentsRM

    Instrumente_Massnahmen_Risikomanagement <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Umgang_mit_Risiken_in_der_Patientenversorgung/Instrumente_Massnahmen_Risikomanagement'
            ),
            "Instrument_Massnahme_Risikomanagement|Instrument_Massnahme",
            c(
                "RM_Schluessel",
                "Zusatzangaben_RM/SOP/Name",
                "Zusatzangaben_RM/SOP/Datum"
            )
        ) %>%
        rename("SOP" = .data$Zusatzangaben_RM_SOP_Name,
               "Datum" = .data$Zusatzangaben_RM_SOP_Datum) %>%
        mutate(Fallbesprechungen_Konferenzen = NA_character_)

    rm_massnahmen <-
        xml_find_all(
            obj,
            '//Umgang_mit_Risiken_in_der_Patientenversorgung/Instrumente_Massnahmen_Risikomanagement/Instrument_Massnahme_Risikomanagement'
        )

    if (length(rm_massnahmen) > 0) {

        Instrumente_Massnahmen_Risikomanagement[Instrumente_Massnahmen_Risikomanagement$RM_Schluessel == "RM10",
                                                "Fallbesprechungen_Konferenzen"] <-
            paste0(c(
                paste0(xml_text(
                    xml_find_all(
                        rm_massnahmen[xml_text(xml_find_all(rm_massnahmen, "./RM_Schluessel")) == "RM10"],
                        "./Zusatzangaben_RM/Fallbesprechungen_Konferenzen/Fallbesprechung_Konferenz/Art"
                    )
                ), collapse = " / "),
                paste0(xml_text(
                    xml_find_all(
                        rm_massnahmen[xml_text(xml_find_all(rm_massnahmen, "./RM_Schluessel")) == "RM10"],
                        "./Zusatzangaben_RM/Fallbesprechungen_Konferenzen/Fallbesprechung_Konferenz/Andere"
                    )
                ), collapse = " / ")
            ),
            collapse = "; ")
        rm(rm_massnahmen)

    }

    InstrumentsRM <- Instrumente_Massnahmen_Risikomanagement %>%
        mutate(idInstrumentsRM = NA_integer_,
               Hospital_idHospital = Hospital_id) %>%
        rename("code" = .data$RM_Schluessel,
               "date" = .data$Datum,
               "caseConferences" = .data$Fallbesprechungen_Konferenzen) %>%
        select(.data$idInstrumentsRM, everything()) %>%
        filter(!is.na(.data$code))

    if (nrow(InstrumentsRM) > 0) {

        InstrumentsRM_exists <- TRUE

    } else {

        InstrumentsRM_exists <- FALSE

        InstrumentsRM <- NA_character_

    }

    return(list("Arzneimittelkommission" = Arzneimittelkommission,
                "Anderes_Gremium_Arbeitsgruppe" = Anderes_Gremium_Arbeitsgruppe,
                "Name_verantwortliches_Gremium_Arbeitsgruppe" = Name_verantwortliches_Gremium_Arbeitsgruppe,
                "Beteiligte_Abteilungen_Funktionsbereiche" = Beteiligte_Abteilungen_Funktionsbereiche,
                "PharmacotherapySteeringBoard_exists" = PharmacotherapySteeringBoard_exists,
                "PharmacotherapySteeringBoard" = PharmacotherapySteeringBoard,
                "noMedicinesWorkingGroup" = Kein_Gremium_Arbeitsgruppe,
                "Verantwortliche_Person_AMTS" = Verantwortliche_Person_AMTS,
                "Verantwortliche_Person_AMTS_nicht_festgelegt" = Verantwortliche_Person_AMTS_nicht_festgelegt,
                "pharmaceuticalStaff_exists" = pharmaceuticalStaff_exists,
                "pharmaceuticalStaff" = pharmaceuticalStaff,
                "PharmacotherapySafetyMeasures_exists" = PharmacotherapySafetyMeasures_exists,
                "PharmacotherapySafetyMeasures" = PharmacotherapySafetyMeasures,
                "InstrumentsRM_exists" = InstrumentsRM_exists,
                "InstrumentsRM" = InstrumentsRM))

}
