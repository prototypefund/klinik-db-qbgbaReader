#' Extract all information regarding the topic "Weitere Informationen Hygiene".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#' @param year A character value with the name of the year of the report, which
#'     is needed because of changes of XML-elements' names between years.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item HygieneFurtherMeasures_exists
#'         \item HygieneFurtherMeasures
#'         \item HygieneMeasures_exists
#'         \item HygieneMeasures
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Weitere_Informationen_Hygiene(doc, 1L, "2019")
#'
#'
#' @export
#'
qb_extractor_Weitere_Informationen_Hygiene <- function(obj, Hospital_id, year = NULL) {

    # obj <- details
    # Hospital_id <- 1L
    # year <- "2018"


    if (is.null(year) ||
        length(year) != 1 ||
        !is.character(year) ||
        !year %in% c("2015", "2016", "2017", "2018", "2019")) {

        stop('Parameter "year" must be a single string (permitted values: "2015", "2016", "2017", "2018" or "2019").')

    }

    # Hygienestandard_ZVK -----------------------------------------------------

    Hygienestandard_ZVK <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Weitere_Informationen_Hygiene'
        ),
        "Hygienestandard_ZVK",
        c("./Standard_Hygiene_ZVK_liegt_vor/Standard_thematisiert/Hygienische_Haendedesinfektion",
          "./Standard_Hygiene_ZVK_liegt_vor/Standard_thematisiert/Hautdesinfektion",
          "./Standard_Hygiene_ZVK_liegt_vor/Standard_thematisiert/Beachtung_Einwirkzeit",
          "./Standard_Hygiene_ZVK_liegt_vor/Standard_thematisiert/Anwendung_weiterer_Hygienemassnahmen/Sterile_Handschuhe",
          "./Standard_Hygiene_ZVK_liegt_vor/Standard_thematisiert/Anwendung_weiterer_Hygienemassnahmen/Steriler_Kittel",
          "./Standard_Hygiene_ZVK_liegt_vor/Standard_thematisiert/Anwendung_weiterer_Hygienemassnahmen/Kopfhaube",
          "./Standard_Hygiene_ZVK_liegt_vor/Standard_thematisiert/Anwendung_weiterer_Hygienemassnahmen/Mund_Nasen_Schutz",
          "./Standard_Hygiene_ZVK_liegt_vor/Standard_thematisiert/Anwendung_weiterer_Hygienemassnahmen/Steriles_Abdecktuch",
          "./Standard_Hygiene_ZVK_liegt_vor/Standard_autorisiert",
          "./Standard_Liegedauer_ZVK_liegt_vor/Standard_autorisiert")
    )
    colnames(Hygienestandard_ZVK) <- c("Hygienische_Haendedesinfektion",
                                       "Hautdesinfektion",
                                       "Beachtung_Einwirkzeit",
                                       "Sterile_Handschuhe",
                                       "Steriler_Kittel",
                                       "Kopfhaube",
                                       "Mund_Nasen_Schutz",
                                       "Steriles_Abdecktuch",
                                       "Standard_ZVK_autorisiert",
                                       "Standard_Liegedauer_ZVK_autorisiert")
    if (any(!is.na(Hygienestandard_ZVK))) {

        Hygienestandard_ZVK <- Hygienestandard_ZVK %>%
            mutate(Bereich = "Hygienestandard_ZVK") %>%
            pivot_longer(-.data$Bereich, names_to = "Massnahme", values_to = "Wert")

    } else {

        Hygienestandard_ZVK <- tibble("Bereich" = NA_character_,
                                      "Massnahme" = NA_character_,
                                      "Wert" = NA_character_)

    }


    Kein_Einsatz_ZVK <- xml_find_first(
        obj,
        '//Weitere_Informationen_Hygiene/Kein_Einsatz_ZVK|//Weitere_Informationen_Hygiene/Hygienestandard_ZVK_trifft_nicht_zu'
    )

    if (!is.na(Kein_Einsatz_ZVK)) {

        Kein_Einsatz_ZVK <- tibble("Bereich" = "Hygienestandard_ZVK",
                                   "Massnahme" = "Kein_Einsatz_ZVK",
                                   "Wert" = "ja")

    } else {

        Kein_Einsatz_ZVK <- tibble("Bereich" = NA_character_,
                                   "Massnahme" = NA_character_,
                                   "Wert" = NA_character_)

    }

    Standard_Hygiene_ZVK_liegt_nicht_vor <- xml_find_first(
        obj,
        '//Weitere_Informationen_Hygiene/Hygienestandard_ZVK/Standard_Hygiene_ZVK_liegt_nicht_vor'
    )

    if (!is.na(Standard_Hygiene_ZVK_liegt_nicht_vor)) {

        Standard_Hygiene_ZVK_liegt_nicht_vor <- tibble("Bereich" = "Hygienestandard_ZVK",
                                                       "Massnahme" = "Standard_Hygiene_ZVK_liegt_nicht_vor",
                                                       "Wert" = "ja")

    } else {

        Standard_Hygiene_ZVK_liegt_nicht_vor <- tibble("Bereich" = NA_character_,
                                                       "Massnahme" = NA_character_,
                                                       "Wert" = NA_character_)

    }

    Standard_Liegedauer_ZVK_liegt_nicht_vor <- xml_find_first(
        obj,
        '//Weitere_Informationen_Hygiene/Hygienestandard_ZVK/Standard_Liegedauer_ZVK_liegt_nicht_vor'
    )

    if (!is.na(Standard_Liegedauer_ZVK_liegt_nicht_vor)) {

        Standard_Liegedauer_ZVK_liegt_nicht_vor <- tibble("Bereich" = "Hygienestandard_ZVK",
                                                          "Massnahme" = "Standard_Liegedauer_ZVK_liegt_nicht_vor",
                                                          "Wert" = "ja")

    } else {

        Standard_Liegedauer_ZVK_liegt_nicht_vor <- tibble("Bereich" = NA_character_,
                                                          "Massnahme" = NA_character_,
                                                          "Wert" = NA_character_)

    }


    Hygienestandard_ZVK <- bind_rows(Hygienestandard_ZVK,
                                     Kein_Einsatz_ZVK,
                                     Standard_Hygiene_ZVK_liegt_nicht_vor,
                                     Standard_Liegedauer_ZVK_liegt_nicht_vor) %>%
        filter(if_any(everything(), ~ !is.na(.)))



    # Antibiotikaprophylaxe_Antibiotikatherapie -------------------------------


    Antibiotikaprophylaxe_Antibiotikatherapie <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Weitere_Informationen_Hygiene'
        ),
        "Antibiotikaprophylaxe_Antibiotikatherapie",
        c("./Leitlinie_Antibiotikatherapie_liegt_vor/Leitlinie_Resistenzlage_angepasst",
          "./Leitlinie_Antibiotikatherapie_liegt_vor/Leitlinie_autorisiert",
          "./Standard_perioperative_Antibiotikaprophylaxe_liegt_vor/Standard_thematisiert/Indikationsstellung_Antibiotikaprophylaxe",
          "./Standard_perioperative_Antibiotikaprophylaxe_liegt_vor/Standard_thematisiert/Zu_Verwendende_Antibiotika",
          "./Standard_perioperative_Antibiotikaprophylaxe_liegt_vor/Standard_thematisiert/Dauer_Antibiotikaprophylaxe",
          "./Standard_perioperative_Antibiotikaprophylaxe_liegt_vor/Standard_autorisiert",
          "./Standard_perioperative_Antibiotikaprophylaxe_liegt_vor/Ueberpruefung_durch_Checkliste")
    )

    colnames(Antibiotikaprophylaxe_Antibiotikatherapie) <- c("Leitlinie_Resistenzlage_angepasst",
                                                             "Leitlinie_autorisiert",
                                                             "Indikationsstellung_Antibiotikaprophylaxe",
                                                             "Zu_Verwendende_Antibiotika",
                                                             "Dauer_Antibiotikaprophylaxe",
                                                             "Standard_autorisiert",
                                                             "Ueberpruefung_durch_Checkliste")
    if (any(!is.na(Antibiotikaprophylaxe_Antibiotikatherapie))) {

        Antibiotikaprophylaxe_Antibiotikatherapie <- Antibiotikaprophylaxe_Antibiotikatherapie %>%
            mutate(Bereich = "Antibiotikaprophylaxe_Antibiotikatherapie") %>%
            pivot_longer(-.data$Bereich, names_to = "Massnahme", values_to = "Wert")

    } else {

        Antibiotikaprophylaxe_Antibiotikatherapie <- tibble("Bereich" = NA_character_,
                                                            "Massnahme" = NA_character_,
                                                            "Wert" = NA_character_)

    }

    Leitlinie_Antibiotikatherapie_liegt_nicht_vor <- xml_find_first(
        obj,
        '//Weitere_Informationen_Hygiene/Antibiotikaprophylaxe_Antibiotikatherapie/Leitlinie_Antibiotikatherapie_liegt_nicht_vor'
    )

    if (!is.na(Leitlinie_Antibiotikatherapie_liegt_nicht_vor)) {

        Leitlinie_Antibiotikatherapie_liegt_nicht_vor <- tibble("Bereich" = "Antibiotikaprophylaxe_Antibiotikatherapie",
                                                                "Massnahme" = "Leitlinie_Antibiotikatherapie_liegt_nicht_vor",
                                                                "Wert" = "ja")

    } else {

        Leitlinie_Antibiotikatherapie_liegt_nicht_vor <- tibble("Bereich" = NA_character_,
                                                                "Massnahme" = NA_character_,
                                                                "Wert" = NA_character_)

    }

    Standard_perioperative_Antibiotikaprophylaxe_liegt_nicht_vor <- xml_find_first(
        obj,
        '//Weitere_Informationen_Hygiene/Antibiotikaprophylaxe_Antibiotikatherapie/Standard_perioperative_Antibiotikaprophylaxe_liegt_nicht_vor'
    )

    if (!is.na(Standard_perioperative_Antibiotikaprophylaxe_liegt_nicht_vor)) {

        Standard_perioperative_Antibiotikaprophylaxe_liegt_nicht_vor <- tibble("Bereich" = "Antibiotikaprophylaxe_Antibiotikatherapie",
                                                                               "Massnahme" = "Standard_perioperative_Antibiotikaprophylaxe_liegt_nicht_vor",
                                                                               "Wert" = "ja")

    } else {

        Standard_perioperative_Antibiotikaprophylaxe_liegt_nicht_vor <- tibble("Bereich" = NA_character_,
                                                                               "Massnahme" = NA_character_,
                                                                               "Wert" = NA_character_)

    }


    Keine_Durchfuehrung_Operationen <- xml_find_first(
        obj,
        '//Weitere_Informationen_Hygiene/Antibiotikaprophylaxe_Antibiotikatherapie/Standard_perioperative_Antibiotikaprophylaxe_trifft_nicht_zu|//Weitere_Informationen_Hygiene/Antibiotikaprophylaxe_Antibiotikatherapie/Keine_Durchfuehrung_Operationen'
    )

    if (!is.na(Keine_Durchfuehrung_Operationen)) {

        Keine_Durchfuehrung_Operationen <- tibble("Bereich" = "Antibiotikaprophylaxe_Antibiotikatherapie",
                                                  "Massnahme" = "Keine_Durchfuehrung_Operationen",
                                                  "Wert" = "ja")

    } else {

        Keine_Durchfuehrung_Operationen <- tibble("Bereich" = NA_character_,
                                                  "Massnahme" = NA_character_,
                                                  "Wert" = NA_character_)

    }

    Antibiotikaprophylaxe_Antibiotikatherapie <- bind_rows(Antibiotikaprophylaxe_Antibiotikatherapie,
                                                           Leitlinie_Antibiotikatherapie_liegt_nicht_vor,
                                                           Standard_perioperative_Antibiotikaprophylaxe_liegt_nicht_vor,
                                                           Keine_Durchfuehrung_Operationen) %>%
        filter(if_any(everything(), ~ !is.na(.)))


    # Umgang_Wunden -----------------------------------------------------------

    if (year %in% c("2015", "2016", "2017", "2018")) {

        Umgang_Wunden <- qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Weitere_Informationen_Hygiene'
            ),
            "Umgang_Wunden",
            c("./Standard_Wunderversorgung_Verbandwechsel_liegt_vor/Standard_thematisiert/Hygienische_Haendedesinfektion",
              "./Standard_Wunderversorgung_Verbandwechsel_liegt_vor/Standard_thematisiert/Verbandwechsel_aseptische_Bedingungen",
              "./Standard_Wunderversorgung_Verbandwechsel_liegt_vor/Standard_thematisiert/Antiseptische_Behandlung_infizierte_Wunden",
              "./Standard_Wunderversorgung_Verbandwechsel_liegt_vor/Standard_thematisiert/Pruefung_Notwendigkeit_Wundauflage",
              "./Standard_Wunderversorgung_Verbandwechsel_liegt_vor/Standard_thematisiert/Meldung_Wundinfektion",
              "./Standard_Wunderversorgung_Verbandwechsel_liegt_vor/Standard_autorisiert")
        )

    } else if (year == "2019") {

        Umgang_Wunden <- qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Weitere_Informationen_Hygiene'
            ),
            "Umgang_Wunden",
            c("./Standard_Wundversorgung_Verbandwechsel_liegt_vor/Standard_thematisiert/Hygienische_Haendedesinfektion",
              "./Standard_Wundversorgung_Verbandwechsel_liegt_vor/Standard_thematisiert/Verbandwechsel_aseptische_Bedingungen",
              "./Standard_Wundversorgung_Verbandwechsel_liegt_vor/Standard_thematisiert/Antiseptische_Behandlung_infizierte_Wunden",
              "./Standard_Wundversorgung_Verbandwechsel_liegt_vor/Standard_thematisiert/Pruefung_Notwendigkeit_Wundauflage",
              "./Standard_Wundversorgung_Verbandwechsel_liegt_vor/Standard_thematisiert/Meldung_Wundinfektion",
              "./Standard_Wundversorgung_Verbandwechsel_liegt_vor/Standard_autorisiert")
        )

    }

    colnames(Umgang_Wunden) <- c("Hygienische_Haendedesinfektion",
                                 "Verbandwechsel_aseptische_Bedingungen",
                                 "Antiseptische_Behandlung_infizierte_Wunden",
                                 "Pruefung_Notwendigkeit_Wundauflage",
                                 "Meldung_Wundinfektion",
                                 "Standard_autorisiert")
    if (any(!is.na(Umgang_Wunden))) {

        Umgang_Wunden <- Umgang_Wunden %>%
            mutate(Bereich = "Umgang_Wunden") %>%
            pivot_longer(-.data$Bereich, names_to = "Massnahme", values_to = "Wert")

    } else {

        Umgang_Wunden <- tibble("Bereich" = NA_character_,
                                "Massnahme" = NA_character_,
                                "Wert" = NA_character_)

    }


    Keine_Durchfuehrung_Wundversorgung <- xml_find_first(
        obj,
        '//Weitere_Informationen_Hygiene/Umgang_Wunden/Standard_Wundversorgung_Verbandwechsel_trifft_nicht_zu|//Weitere_Informationen_Hygiene/Umgang_Wunden/Keine_Durchfuehrung_Wundversorgung'
    )

    if (!is.na(Keine_Durchfuehrung_Wundversorgung)) {

        Keine_Durchfuehrung_Wundversorgung <- tibble("Bereich" = "Umgang_Wunden",
                                                     "Massnahme" = "Keine_Durchfuehrung_Wundversorgung",
                                                     "Wert" = "ja")

    } else {

        Keine_Durchfuehrung_Wundversorgung <- tibble("Bereich" = NA_character_,
                                                     "Massnahme" = NA_character_,
                                                     "Wert" = NA_character_)

    }

    Standard_Wundversorgung_Verbandwechsel_liegt_nicht_vor <- xml_find_first(
        obj,
        '//Weitere_Informationen_Hygiene/Umgang_Wunden/Standard_Wundversorgung_Verbandwechsel_liegt_nicht_vor'
    )

    if (!is.na(Standard_Wundversorgung_Verbandwechsel_liegt_nicht_vor)) {

        Standard_Wundversorgung_Verbandwechsel_liegt_nicht_vor <- tibble("Bereich" = "Umgang_Wunden",
                                                                         "Massnahme" = "Standard_Wundversorgung_Verbandwechsel_liegt_nicht_vor",
                                                                         "Wert" = "ja")

    } else {

        Standard_Wundversorgung_Verbandwechsel_liegt_nicht_vor <- tibble("Bereich" = NA_character_,
                                                                         "Massnahme" = NA_character_,
                                                                         "Wert" = NA_character_)

    }

    Umgang_Wunden <- bind_rows(Umgang_Wunden,
                               Keine_Durchfuehrung_Wundversorgung,
                               Standard_Wundversorgung_Verbandwechsel_liegt_nicht_vor) %>%
        filter(if_any(everything(), ~ !is.na(.)))




    # Haendedesinfektion ------------------------------------------------------

    if (year %in% c("2015", "2016")) {

        Haendedesinfektion <- qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Weitere_Informationen_Hygiene'
            ),
            "Haendedesinfektion",
            c("./Haendedesinfektionsmittelverbrauch_wurde_erhoben/Haendedesinfektionsmittelverbrauch_Allgemeinstationen",
              "./Haendedesinfektionsmittelverbrauch_wurde_erhoben/Haendedesinfektionsmittelverbrauch_Intensivstationen",
              "./Haendedesinfektionsmittelverbrauch_wurde_erhoben/Erfassung_Haendedesinfektionsmittelverbrauch_stationsbezogen")
        )

    } else if (year %in% c("2017", "2018", "2019")) {

        Haendedesinfektion <- qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Weitere_Informationen_Hygiene'
            ),
            "Haendedesinfektion",
            c("./Haendedesinfektionsmittelverbrauch_Allgemeinstationen_wurde_erhoben/Haendedesinfektionsmittelverbrauch_Allgemeinstationen",
              "./Haendedesinfektionsmittelverbrauch_Intensivstationen_wurde_erhoben/Haendedesinfektionsmittelverbrauch_Intensivstationen",
              "Erfassung_Haendedesinfektionsmittelverbrauch_stationsbezogen")
        )

    }

    colnames(Haendedesinfektion) <- c("Haendedesinfektionsmittelverbrauch_Allgemeinstationen",
                                      "Haendedesinfektionsmittelverbrauch_Intensivstationen",
                                      "Erfassung_Haendedesinfektionsmittelverbrauch_stationsbezogen")
    if (any(!is.na(Haendedesinfektion))) {

        Haendedesinfektion <- Haendedesinfektion %>%
            mutate(Bereich = "Haendedesinfektion") %>%
            pivot_longer(-.data$Bereich, names_to = "Massnahme", values_to = "Wert")

    } else {

        Haendedesinfektion <- tibble("Bereich" = NA_character_,
                                     "Massnahme" = NA_character_,
                                     "Wert" = NA_character_)

    }

    Haendedesinfektionsmittelverbrauch_Allgemeinstationen_wurde_nicht_erhoben <- xml_find_first(
        obj,
        '//Weitere_Informationen_Hygiene/Haendedesinfektion/Haendedesinfektionsmittelverbrauch_Allgemeinstationen_wurde_nicht_erhoben'
    )

    if (!is.na(Haendedesinfektionsmittelverbrauch_Allgemeinstationen_wurde_nicht_erhoben)) {

        Haendedesinfektionsmittelverbrauch_Allgemeinstationen_wurde_nicht_erhoben <- tibble("Bereich" = "Haendedesinfektion",
                                                                         "Massnahme" = "Haendedesinfektionsmittelverbrauch_Allgemeinstationen_wurde_nicht_erhoben",
                                                                         "Wert" = "ja")

    } else {

        Haendedesinfektionsmittelverbrauch_Allgemeinstationen_wurde_nicht_erhoben <- tibble("Bereich" = NA_character_,
                                                                         "Massnahme" = NA_character_,
                                                                         "Wert" = NA_character_)

    }


    Haendedesinfektionsmittelverbrauch_Intensivstationen_wurde_nicht_erhoben <- xml_find_first(
        obj,
        '//Weitere_Informationen_Hygiene/Haendedesinfektion/Haendedesinfektionsmittelverbrauch_Intensivstationen_wurde_nicht_erhoben'
    )

    if (!is.na(Haendedesinfektionsmittelverbrauch_Intensivstationen_wurde_nicht_erhoben)) {

        Haendedesinfektionsmittelverbrauch_Intensivstationen_wurde_nicht_erhoben <- tibble("Bereich" = "Haendedesinfektion",
                                                                                           "Massnahme" = "Haendedesinfektionsmittelverbrauch_Intensivstationen_wurde_nicht_erhoben",
                                                                                           "Wert" = "ja")

    } else {

        Haendedesinfektionsmittelverbrauch_Intensivstationen_wurde_nicht_erhoben <- tibble("Bereich" = NA_character_,
                                                                                           "Massnahme" = NA_character_,
                                                                                           "Wert" = NA_character_)

    }


    Keine_Intensivstation_vorhanden <- xml_find_first(
        obj,
        '//Weitere_Informationen_Hygiene/Haendedesinfektion/Keine_Intensivstation_vorhanden'
    )

    if (!is.na(Keine_Intensivstation_vorhanden)) {

        Keine_Intensivstation_vorhanden <- tibble("Bereich" = "Haendedesinfektion",
                                                  "Massnahme" = "Keine_Intensivstation_vorhanden",
                                                  "Wert" = "ja")

    } else {

        Keine_Intensivstation_vorhanden <- tibble("Bereich" = NA_character_,
                                                  "Massnahme" = NA_character_,
                                                  "Wert" = NA_character_)

    }

    Haendedesinfektion <- bind_rows(Haendedesinfektion,
                                    Haendedesinfektionsmittelverbrauch_Allgemeinstationen_wurde_nicht_erhoben,
                                    Haendedesinfektionsmittelverbrauch_Intensivstationen_wurde_nicht_erhoben,
                                    Keine_Intensivstation_vorhanden) %>%
        filter(if_any(everything(), ~ !is.na(.)))



    # Umgang_Patienten_MRE ----------------------------------------------------

    Umgang_Patienten_MRE <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Weitere_Informationen_Hygiene'
        ),
        "Umgang_Patienten_MRE",
        c("Standardisierte_Information_MRSA",
          "Informationsmanagement_MRSA",
          "Risikoadaptives_Aufnahmescreening",
          "Regelmaessige_Schulungen")
    )
    if (any(!is.na(Umgang_Patienten_MRE))) {

        Umgang_Patienten_MRE <- Umgang_Patienten_MRE %>%
            mutate(Bereich = "Umgang_Patienten_MRE") %>%
            pivot_longer(-.data$Bereich, names_to = "Massnahme", values_to = "Wert")

    } else {

        Umgang_Patienten_MRE <- NA_character_

    }

    Hygiene_Weitere_Massnahmen <- list(Hygienestandard_ZVK, Antibiotikaprophylaxe_Antibiotikatherapie, Umgang_Wunden,
                                       Haendedesinfektion, Umgang_Patienten_MRE)
    Hygiene_Weitere_Massnahmen <- bind_rows(Hygiene_Weitere_Massnahmen[map_lgl(Hygiene_Weitere_Massnahmen, ~ all(!is.na(.x)))])


    if (nrow(Hygiene_Weitere_Massnahmen) > 0) {

        HygieneFurtherMeasures_exists <- TRUE

        HygieneFurtherMeasures <- Hygiene_Weitere_Massnahmen %>%
            mutate(idHygieneFurtherMeasures = NA_integer_,
                   Hospital_idHospital = Hospital_id) %>%
            rename("area" = .data$Bereich,
                   "measure" = .data$Massnahme,
                   "value" = .data$Wert) %>%
            select(.data$idHygieneFurtherMeasures, everything())

    } else {

        HygieneFurtherMeasures_exists <- FALSE

        HygieneFurtherMeasures <- NA_character_

    }



    ### Hygiene_Instrument_Massnahme

    Hygiene_Instrument_Massnahme <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Weitere_Informationen_Hygiene/Hygienebezogenes_Risikomanagement'
        ),
        "Hygiene_Instrument_Massnahme",
        c("HM_Schluessel")
    )

    if (any(!is.na(Hygiene_Instrument_Massnahme$HM_Schluessel))) {

        Hygiene_Instrument_Massnahme <- Hygiene_Instrument_Massnahme %>%
            mutate(Zusatzangaben_HM = NA_character_)

        if (any(Hygiene_Instrument_Massnahme$HM_Schluessel == "HM01")) {

            Hygiene_Instrument_Massnahme[Hygiene_Instrument_Massnahme$HM_Schluessel == "HM01", "Zusatzangaben_HM"] <-
                paste0(xml_text(xml_find_all(
                    obj,
                    paste0('//Weitere_Informationen_Hygiene/Hygienebezogenes_Risikomanagement/Hygiene_Instrument_Massnahme/',
                           'HM_Schluessel[text() = "HM01"]/../Zusatzangaben_HM/URL_Bericht_zu_Infektionsraten')
                )), collapse = ", ")

        }

        if (any(Hygiene_Instrument_Massnahme$HM_Schluessel == "HM02")) {

            Hygiene_Instrument_Massnahme[Hygiene_Instrument_Massnahme$HM_Schluessel == "HM02", "Zusatzangaben_HM"] <-
                paste0(xml_text(xml_find_all(
                    obj,
                    paste0('//Weitere_Informationen_Hygiene/Hygienebezogenes_Risikomanagement/Hygiene_Instrument_Massnahme/',
                           'HM_Schluessel[text() = "HM02"]/../Zusatzangaben_HM/Teilnahme_KISS/KISS_Modul/Name')
                )), collapse = ", ")

        }

        if (any(Hygiene_Instrument_Massnahme$HM_Schluessel == "HM03")) {

            Hygiene_Instrument_Massnahme[Hygiene_Instrument_Massnahme$HM_Schluessel == "HM03", "Zusatzangaben_HM"] <-
                paste0(xml_text(xml_find_all(
                    obj,
                    paste0('//Weitere_Informationen_Hygiene/Hygienebezogenes_Risikomanagement/Hygiene_Instrument_Massnahme/',
                           'HM_Schluessel[text() = "HM03"]/../Zusatzangaben_HM/Name')
                )), collapse = ", ")

        }

        if (any(Hygiene_Instrument_Massnahme$HM_Schluessel == "HM04")) {

            Hygiene_Instrument_Massnahme[Hygiene_Instrument_Massnahme$HM_Schluessel == "HM04", "Zusatzangaben_HM"] <-
                paste0(xml_text(xml_find_all(
                    obj,
                    paste0('//Weitere_Informationen_Hygiene/Hygienebezogenes_Risikomanagement/Hygiene_Instrument_Massnahme/',
                           'HM_Schluessel[text() = "HM04"]/../Zusatzangaben_HM/Teilnahme_ASH')
                )), collapse = ", ")

        }

        if (any(Hygiene_Instrument_Massnahme$HM_Schluessel == "HM05")) {

            Hygiene_Instrument_Massnahme[Hygiene_Instrument_Massnahme$HM_Schluessel == "HM05", "Zusatzangaben_HM"] <-
                paste0(xml_text(xml_find_all(
                    obj,
                    paste0('//Weitere_Informationen_Hygiene/Hygienebezogenes_Risikomanagement/Hygiene_Instrument_Massnahme/',
                           'HM_Schluessel[text() = "HM05"]/../Frequenz')
                )), collapse = ", ")

        }

        if (any(Hygiene_Instrument_Massnahme$HM_Schluessel == "HM09")) {

            Hygiene_Instrument_Massnahme[Hygiene_Instrument_Massnahme$HM_Schluessel == "HM09", "Zusatzangaben_HM"] <-
                paste0(xml_text(xml_find_all(
                    obj,
                    paste0('//Weitere_Informationen_Hygiene/Hygienebezogenes_Risikomanagement/Hygiene_Instrument_Massnahme/',
                           'HM_Schluessel[text() = "HM09"]/../Erlaeuterungen')
                )), collapse = ", ")

        }

        Hygiene_Instrument_Massnahme <- Hygiene_Instrument_Massnahme %>%
            mutate(Zusatzangaben_HM = str_replace_all(.data$Zusatzangaben_HM, "\\n", ", "))

    }


    if (any(!is.na(Hygiene_Instrument_Massnahme$HM_Schluessel))) {

        HygieneMeasures_exists <- TRUE

        HygieneMeasures <- Hygiene_Instrument_Massnahme %>%
            mutate(idHygieneMeasures = NA_integer_,
                   Hospital_idHospital = Hospital_id) %>%
            rename("code" = .data$HM_Schluessel,
                   "description" = .data$Zusatzangaben_HM) %>%
            select(.data$idHygieneMeasures, everything())

    } else if (all(is.na(Hygiene_Instrument_Massnahme$HM_Schluessel))) {

        HygieneMeasures_exists <- FALSE

        HygieneMeasures <- NA_character_

    }



    return(list("HygieneFurtherMeasures_exists" = HygieneFurtherMeasures_exists,
                "HygieneFurtherMeasures" = HygieneFurtherMeasures,
                "HygieneMeasures_exists" = HygieneMeasures_exists,
                "HygieneMeasures" = HygieneMeasures))

}
