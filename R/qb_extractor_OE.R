#' Extract all information from all departments in one hospital.
#'
#' @param obj An XML-object comprising not more than one department within a
#'     hospital.
#'
#' @param year A character value with the name of the year of the report, which
#'     is needed because of changes of XML-elements' names between years.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item OE_Stammdaten
#'         \item Aerztliche_Leitung_OE
#'         \item Kontakt_OE_Adresse
#'         \item FA_Schluessel
#'         \item Medizinische_Leistungsangebote
#'         \item Hauptdiagnosen
#'         \item Prozeduren
#'         \item Ambulante_Behandlungsmoeglichkeiten
#'         \item Ambulanz_116b
#'         \item Ambulante_Operationen
#'         \item Personal_OE
#'         \item Aerztliche_Fachexpertisen
#'         \item Pflegerische_Fachexpertisen
#'     }
#'
#'
#' @examples
#'
#' \dontrun{
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' objOEs <- xml_find_all(doc,
#'     '//Organisationseinheiten_Fachabteilungen/Organisationseinheit_Fachabteilung')
#'
#' qb_extractor_OE(objOEs[[1]])
#'
#' all_OEs <- map(objOEs, ~ qb_extractor_OE(.x))
#' }
#'
#' @export
#'
qb_extractor_OE <- function(obj, year) {


    # obj <- details
    # objOEs <- xml_find_all(details, '//Organisationseinheiten_Fachabteilungen/Organisationseinheit_Fachabteilung')
    # obj <- objOEs[[1]]
    # year = "2016"

    if (is.null(year) ||
        length(year) != 1 ||
        !is.character(year) ||
        !year %in% c("2015", "2016", "2017", "2018", "2019")) {

        stop('Parameter "year" must be a single string (permitted values: "2015", "2016", "2017", "2018" or "2019").')

    }


    Gliederungsnummer <- xml_text(xml_find_first(obj, "./Gliederungsnummer"))
    Name <- xml_text(xml_find_first(obj, "./Name"))

    Zielvereinbarung_geschlossen_nicht_an_Empfehlung_DKG_gehalten <-
        ifelse(!is.na(
            xml_find_first(
                obj,
                './Zielvereinbarungen/Getroffene_Zielvereinbarungen|./Zielvereinbarungen/Zielvereinbarung_geschlossen_nicht_an_Empfehlung_DKG_gehalten'
            )
        ), 1L, 0L)

    Zielvereinbarungen_geschlossen_an_Empfehlung_DKG_gehalten <-
        ifelse(!is.na(
            xml_find_first(
                obj,
                './Zielvereinbarungen/Zielvereinbarung_eingehalten|./Zielvereinbarungen/Zielvereinbarungen_geschlossen_an_Empfehlung_DKG_gehalten'
            )
        ), 1L, 0L)

    Keine_Zielvereinbarungen_geschlossen <-
        ifelse(!is.na(
            xml_find_first(
                obj,
                './Zielvereinbarungen/Keine_Zielvereinbarungen_geschlossen|./Zielvereinbarungen/Keine_Zielvereinbarung_geschlossen'
            )
        ), 1L, 0L)


    if (Zielvereinbarung_geschlossen_nicht_an_Empfehlung_DKG_gehalten == 1L) {

        Zielvereinbarungen <- "Zielvereinbarung_geschlossen_nicht_an_Empfehlung_DKG_gehalten"

    } else if (Zielvereinbarungen_geschlossen_an_Empfehlung_DKG_gehalten == 1L) {

        Zielvereinbarungen <- "Zielvereinbarungen_geschlossen_an_Empfehlung_DKG_gehalten"

    } else if (Keine_Zielvereinbarungen_geschlossen == 1L) {

        Zielvereinbarungen <- "Keine_Zielvereinbarungen_geschlossen"

    }

    Zielvereinbarungen_Erlaeuterungen <- xml_text(xml_find_first(obj, './Zielvereinbarungen/Erlaeuterungen'))

    Zielvereinbarungen_Erlaeuterungen_nicht_DKG <-
        xml_text(
            xml_find_first(
                obj,
                './Zielvereinbarungen/Zielvereinbarung_geschlossen_nicht_an_Empfehlung_DKG_gehalten/Erlaeuterungen'
            )
        )

    Ambulante_D_Arzt_Zulassung <- ifelse(is.na(xml_find_first(obj, './Ambulante_D_Arzt_Zulassung')), 0L, 1L)
    Stationaere_BG_Zulassung <- ifelse(is.na(xml_find_first(obj, './Stationaere_BG_Zulassung')), 0L, 1L)
    Vollstationaere_Fallzahl <- xml_text(xml_find_first(obj, './Fallzahlen_OE/Vollstationaere_Fallzahl'))
    Teilstationaere_Fallzahl <- xml_text(xml_find_first(obj, './Fallzahlen_OE/Teilstationaere_Fallzahl'))
    Fallzahlen_OE_Erlaueterung <- xml_text(xml_find_first(obj, './Fallzahlen_OE/Erlaeuterungen'))
    Ambulante_Operationen_erbracht <- ifelse(is.na(xml_find_first(obj, './Ambulante_Operationen/Keine_Erbracht')), 1L, 0L)
    Massgebliche_tarifliche_Wochenarbeitszeit_Aerzte <- xml_text(xml_find_first(obj, "./Personelle_Ausstattung/Aerztliches_Personal/Hauptabteilung/Aerzte_ohne_Belegaerzte/Massgebliche_tarifliche_Wochenarbeitszeit"))
    Massgebliche_tarifliche_Wochenarbeitszeit_Pflege <- xml_text(xml_find_first(obj, "./Personelle_Ausstattung/Pflegekraefte/Massgebliche_tarifliche_Wochenarbeitszeit"))

    OE_Stammdaten <- tibble("Gliederungsnummer" = trimws(str_replace_all(Gliederungsnummer, "[ ]{2,}", " ")),
                            "Name" = trimws(str_replace_all(Name, "[ ]{2,}", " ")),
                            "Vollstationaere_Fallzahl" = trimws(str_replace_all(Vollstationaere_Fallzahl, "[ ]{2,}", " ")),
                            "Teilstationaere_Fallzahl" = trimws(str_replace_all(Teilstationaere_Fallzahl, "[ ]{2,}", " ")),
                            "Fallzahlen_OE_Erlaueterung" = trimws(str_replace_all(Fallzahlen_OE_Erlaueterung, "[ ]{2,}", " ")),
                            "Ambulante_D_Arzt_Zulassung" = trimws(str_replace_all(Ambulante_D_Arzt_Zulassung, "[ ]{2,}", " ")),
                            "Stationaere_BG_Zulassung" = trimws(str_replace_all(Stationaere_BG_Zulassung, "[ ]{2,}", " ")),
                            "Ambulante_Operationen_erbracht" = trimws(str_replace_all(Ambulante_Operationen_erbracht, "[ ]{2,}", " ")),
                            "Zielvereinbarungen" = Zielvereinbarungen,
                            "Zielvereinbarungen_Erlaeuterungen" = Zielvereinbarungen_Erlaeuterungen,
                            "Zielvereinbarungen_Erlaeuterungen_nicht_DKG" = Zielvereinbarungen_Erlaeuterungen_nicht_DKG,
                            "Massgebliche_tarifliche_Wochenarbeitszeit_Aerzte" = trimws(str_replace_all(Massgebliche_tarifliche_Wochenarbeitszeit_Aerzte, "[ ]{2,}", " ")),
                            "Massgebliche_tarifliche_Wochenarbeitszeit_Pflege" = trimws(str_replace_all(Massgebliche_tarifliche_Wochenarbeitszeit_Pflege, "[ ]{2,}", " ")))

    if (year %in% c("2018", "2019")) {

        if (!is.na(xml_find_first(obj, "./Aerztliche_Leitung_OE/Chefarzt/Kontakt_Person_lang"))) {

            Aerztliche_Leitung_OE <- map_dfr(xml_find_all(obj, "./Aerztliche_Leitung_OE/Chefarzt/Kontakt_Person_lang"),
                                             ~ qb_extract_person(.x, role = "Aerztliche_Leitung_OE"))

            Kontakt_OE_Adresse <- distinct(map_dfr(xml_find_all(obj, "./Aerztliche_Leitung_OE/Chefarzt"), ~ get_OE_contact(.x)))

        } else if (!is.na(xml_find_first(obj, "./Aerztliche_Leitung_OE/Leitender_Belegarzt/Kontakt_Person_lang"))) {

            Aerztliche_Leitung_OE <- map_dfr(xml_find_all(obj, "./Aerztliche_Leitung_OE/Leitender_Belegarzt/Kontakt_Person_lang"),
                                             ~ qb_extract_person(.x, role = "Aerztliche_Leitung_OE"))

            Kontakt_OE_Adresse <- distinct(map_dfr(xml_find_all(obj, "./Aerztliche_Leitung_OE/Leitender_Belegarzt"), ~ get_OE_contact(.x)))

        } else {

            Aerztliche_Leitung_OE <- tibble("idPerson" = NA_integer_,
                                            "role" = NA_character_,
                                            "title" = NA_character_,
                                            "firstname" = NA_character_,
                                            "lastname" = NA_character_,
                                            "responsibility" = NA_character_,
                                            "phone" = NA_character_,
                                            "fax" = NA_character_,
                                            "email" = NA_character_)

        }

    } else if (year %in% c("2015", "2016", "2017")) {

        if (!is.na(xml_find_first(obj, "./Chefaerzte/Kontakt_Person_lang"))) {

            Aerztliche_Leitung_OE <- map_dfr(xml_find_all(obj, "./Chefaerzte/Kontakt_Person_lang"),
                                             ~ qb_extract_person(.x, role = "Aerztliche_Leitung_OE"))

        } else {

            Aerztliche_Leitung_OE <- tibble("idPerson" = NA_integer_,
                                            "role" = NA_character_,
                                            "title" = NA_character_,
                                            "firstname" = NA_character_,
                                            "lastname" = NA_character_,
                                            "responsibility" = NA_character_,
                                            "phone" = NA_character_,
                                            "fax" = NA_character_,
                                            "email" = NA_character_)

        }

        Kontakt_OE_Adresse <- distinct(map_dfr(xml_find_all(obj, "./Zugaenge"), ~ get_OE_contact(.x)))

        if (nrow(Kontakt_OE_Adresse) == 0) {

            Kontakt_OE_Adresse <- tibble("idAddress" = NA_integer_,
                                         "use" = NA_character_,
                                         "street" = NA_character_,
                                         "housenumber" = NA_character_,
                                         "zip" = NA_character_,
                                         "city" = NA_character_,
                                         "district" = NA_character_,
                                         "state" = NA_character_,
                                         "country" = NA_character_,
                                         "URL" = NA_character_,
                                         "type" = NA_character_,
                                         "text" = NA_character_,
                                         "lat" = NA_real_,
                                         "lon" = NA_real_)

        }

    }




    FA_Schluessel <- qb_extract_simple_section(obj, "Fachabteilungsschluessel", c("FA_Schluessel",
                                                                                  "Sonstiger/FA_Sonstiger_Schluessel",
                                                                                  "Sonstiger/Bezeichnung")) %>%
        mutate(FA_Schluessel = case_when(
            is.na(.data$FA_Schluessel) & !is.na(.data$Sonstiger_FA_Sonstiger_Schluessel) ~ .data$Sonstiger_FA_Sonstiger_Schluessel,
            TRUE ~ .data$FA_Schluessel
        )) %>%
        select(-.data$Sonstiger_FA_Sonstiger_Schluessel) %>%
        rename("Bezeichnung" = "Sonstiger_Bezeichnung")


    if (!is.na(xml_find_first(obj, './Medizinische_Leistungsangebote'))) {

        Medizinische_Leistungsangebote <- qb_extract_simple_section(xml_find_first(obj, './Medizinische_Leistungsangebote'),
                                                                    "Medizinisches_Leistungsangebot",
                                                                    c("VA_VU_Schluessel",
                                                                      "Erlaeuterungen",
                                                                      "Sonstiger/VA_VU_Sonstiger_Schluessel",
                                                                      "Sonstiger/Bezeichnung")) %>%
            mutate(Erlaeuterungen = case_when(
                is.na(.data$VA_VU_Schluessel) &
                    !is.na(.data$Sonstiger_VA_VU_Sonstiger_Schluessel) &
                    !is.na(.data$Sonstiger_Bezeichnung) ~ paste0(.data$Sonstiger_Bezeichnung, ": ", .data$Erlaeuterungen),
                TRUE ~ .data$Erlaeuterungen
            )) %>%
            mutate(VA_VU_Schluessel = case_when(
                is.na(.data$VA_VU_Schluessel) & !is.na(.data$Sonstiger_VA_VU_Sonstiger_Schluessel) ~ .data$Sonstiger_VA_VU_Sonstiger_Schluessel,
                TRUE ~ .data$VA_VU_Schluessel
            )) %>%
            select(.data$VA_VU_Schluessel, .data$Erlaeuterungen)

    } else {

        Medizinische_Leistungsangebote <- tibble("VA_VU_Schluessel" = NA_character_,
                                                 "Erlaeuterungen" = NA_character_)

    }

    if (!is.na(xml_find_first(obj, './Barrierefreiheit'))) {

        Barrierefreiheit_OE <-
            qb_extract_simple_section(
                xml_find_first(obj, './Barrierefreiheit'),
                "Barrierefreiheit_Aspekt",
                c("BF_Schluessel", "Erlaeuterungen")
            )

    } else {

        Barrierefreiheit_OE <- tibble("BF_Schluessel" = NA_character_,
                                      "Erlaeuterungen" = NA_character_)

    }


    if (is.na(xml_find_first(obj, './Hauptdiagnosen/Keine_Erbracht'))) {

        Hauptdiagnosen <- qb_extract_simple_section(xml_find_first(obj, './Hauptdiagnosen'),
                                                    "Hauptdiagnose",
                                                    c("ICD_10",
                                                      "Fallzahl",
                                                      "Fallzahl_Datenschutz")) %>%
            mutate(Fallzahl = case_when(
                is.na(.data$Fallzahl) ~ "Datenschutz - keine Angabe",
                TRUE ~ .data$Fallzahl
            )) %>%
            select(-.data$Fallzahl_Datenschutz)

    } else {

        Hauptdiagnosen <- tibble("ICD_10" = "0",
                                 "Fallzahl" = "Keine Hauptdiagnosen erbracht")

    }


    if (!is.na(xml_find_first(obj, './Prozeduren/Verpflichtende_Angabe|./Prozeduren/Freiwillige_Angabe'))) {

        Prozeduren <- qb_extract_simple_section(xml_find_first(obj, './Prozeduren/Verpflichtend|./Prozeduren/Verpflichtende_Angabe|./Prozeduren/Freiwillig|./Prozeduren/Freiwillige_Angabe'),
                                                "Prozedur",
                                                c("OPS_301",
                                                  "Anzahl",
                                                  "Anzahl_Datenschutz")) %>%
            mutate(Anzahl = case_when(
                is.na(.data$Anzahl) ~ "Datenschutz - keine Angabe",
                TRUE ~ .data$Anzahl
            )) %>%
            select(-.data$Anzahl_Datenschutz)

    } else {

        Prozeduren <- tibble("OPS_301" = NA_character_,
                             "Anzahl" = NA_character_)

    }


    if (!is.na(xml_find_first(obj, './Ambulante_Behandlungsmoeglichkeiten'))) {

        Ambulante_Behandlungsmoeglichkeiten <- qb_extract_simple_section(xml_find_first(obj, './Ambulante_Behandlungsmoeglichkeiten'),
                                                                         "Ambulante_Behandlungsmoeglichkeit/Ambulanz",
                                                                         c("AM_Schluessel",
                                                                           "Bezeichnung",
                                                                           "Erlaeuterungen"))

        obj_leistungen_ambulanzen <- xml_find_all(obj, './Ambulante_Behandlungsmoeglichkeiten/Ambulante_Behandlungsmoeglichkeit/Ambulanz/Leistungen_Ambulanz')

        if (length(obj_leistungen_ambulanzen) > 0) {

            VA_VU_Schluessel_Ambulanz <- map_dfr(obj_leistungen_ambulanzen, ~ qb_extract_simple_section(.x,
                                                                                                        "Medizinisches_Leistungsangebot",
                                                                                                        c("../../Bezeichnung",
                                                                                                          "VA_VU_Schluessel_Ambulanz",
                                                                                                          "Sonstiger/VA_VU_Sonstiger_Schluessel_Ambulanz",
                                                                                                          "Sonstiger/Bezeichnung"))) %>%
                rename("Bezeichnung" = .data[[".._.._Bezeichnung"]],
                       "VA_VU_Sonstiger_Schluessel_Ambulanz" = .data$Sonstiger_VA_VU_Sonstiger_Schluessel_Ambulanz,
                       "Bezeichnung_sonstiger_Schluessel" = .data$Sonstiger_Bezeichnung) %>%
                mutate(VA_VU_Schluessel_Ambulanz = case_when(
                    is.na(.data$VA_VU_Schluessel_Ambulanz) &
                        !is.na(.data$VA_VU_Sonstiger_Schluessel_Ambulanz) ~ .data$VA_VU_Sonstiger_Schluessel_Ambulanz,
                    TRUE ~ .data$VA_VU_Schluessel_Ambulanz
                )) %>%
                select(-.data$VA_VU_Sonstiger_Schluessel_Ambulanz)

            Ambulante_Behandlungsmoeglichkeiten <- Ambulante_Behandlungsmoeglichkeiten %>%
                full_join(VA_VU_Schluessel_Ambulanz, by = "Bezeichnung")

        } else {

            Ambulante_Behandlungsmoeglichkeiten <- Ambulante_Behandlungsmoeglichkeiten %>%
                mutate("VA_VU_Schluessel_Ambulanz" = NA_character_,
                       "Bezeichnung_sonstiger_Schluessel" = NA_character_)

        }

    } else {

        Ambulante_Behandlungsmoeglichkeiten <- tibble("AM_Schluessel" = NA_character_,
                                                      "Bezeichnung" = NA_character_,
                                                      "Erlaeuterungen" = NA_character_,
                                                      "VA_VU_Schluessel_Ambulanz" = NA_character_,
                                                      "Bezeichnung_sonstiger_Schluessel" = NA_character_)

    }


    if (!is.na(xml_find_first(obj, './Ambulante_Behandlungsmoeglichkeiten'))) {

        Ambulanz_116b <- qb_extract_simple_section(xml_find_first(obj, './Ambulante_Behandlungsmoeglichkeiten'),
                                                   "Ambulante_Behandlungsmoeglichkeit/Ambulanz_116b",
                                                   c("AM_116b_Schluessel",
                                                     "Bezeichnung",
                                                     "Leistungen_Ambulanz_116b/Leistung/LK_Schluessel"))

    } else {

        Ambulanz_116b <- tibble("AM_116b_Schluessel" = NA_character_,
                                "Bezeichnung" = NA_character_,
                                "Leistungen_Ambulanz_116b_Leistung_LK_Schluessel" = NA_character_)

    }


    if (Ambulante_Operationen_erbracht == 1L) {

        Ambulante_Operationen <- qb_extract_simple_section(xml_find_first(obj, './Ambulante_Operationen'),
                                                           "Verpflichtende_Angabe/Ambulante_Operation",
                                                           c("OPS_301",
                                                             "Anzahl",
                                                             "Anzahl_Datenschutz")) %>%
            mutate(Anzahl = case_when(
                is.na(.data$Anzahl) ~ "Datenschutz - keine Angabe",
                TRUE ~ .data$Anzahl
            )) %>%
            select(-.data$Anzahl_Datenschutz)

    } else {

        Ambulante_Operationen <- tibble("OPS_301" = NA_character_,
                                        "Anzahl" = NA_character_)

    }


    ### TODO - Each occupation can have elements "Erlaeuterungen" as sibling elements
    ### to "Anzahl_VK" nodes...within each of the inpatient / outpatient nodes etc, I guess...!
    ### Example:
    ### details <- read_xml("../2019_v2/Berichte-Teile-A-B-C/260100272-00-2019-xml.xml")


    if (!is.na(xml_find_first(obj, './Personelle_Ausstattung/Aerztliches_Personal/Hauptabteilung'))) {

        Arzt_searchstring <- "Hauptabteilung"

    } else if (!is.na(xml_find_first(obj, './Personelle_Ausstattung/Aerztliches_Personal/Gemischte_Haupt_Belegabteilung'))) {

        Arzt_searchstring <- "Gemischte_Haupt_Belegabteilung"

    } else if (!is.na(xml_find_first(obj, './Personelle_Ausstattung/Aerztliches_Personal/Nicht_Bettenfuehrend_Abteilung'))) {

        Arzt_searchstring <- "Nicht_Bettenfuehrend_Abteilung"

    } else if (!is.na(xml_find_first(obj, './Personelle_Ausstattung/Aerztliches_Personal/Belegabteilung'))) {

        Arzt_searchstring <- "Belegabteilung"

    }

    OE_Stammdaten$Abteilungsart <- Arzt_searchstring


    if (Arzt_searchstring == "Belegabteilung") {

        Belegaerzte_OE <-
            qb_extract_simple_section(
                xml_find_first(
                    obj,
                    paste0('./Personelle_Ausstattung/Aerztliches_Personal/', Arzt_searchstring)
                ),
                "Belegaerzte",
                c(
                    "Anzahl",
                    "Fall_je_Anzahl",
                    "Erlaeuterungen"
                )
            )

    } else {

        Belegaerzte_OE <-
            qb_extract_simple_section(
                xml_find_first(
                    obj,
                    paste0('./Personelle_Ausstattung/Aerztliches_Personal/', Arzt_searchstring)
                ),
                "Belegaerzte",
                c(
                    "Anzahl",
                    "Fall_je_Anzahl",
                    "Erlaeuterungen"
                )
            )


        Aerzte_ohne_Belegaerzte <- qb_extract_simple_section(xml_find_first(obj, './Personelle_Ausstattung'),
                                                             paste0("Aerztliches_Personal/", Arzt_searchstring, "/Aerzte_ohne_Belegaerzte/Personalerfassung"),
                                                             c("Anzahl_VK",
                                                               "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                                                               "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                                                               "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                                                               "Versorgungsform/Stationaere_Versorgung/Anzahl_VK",
                                                               "Versorgungsform/Stationaere_Versorgung/Fall_je_Anzahl")) %>%
            mutate(Personalkategorie = "Aerzte_ohne_Belegaerzte")

        Fachaerzte <- qb_extract_simple_section(xml_find_first(obj, './Personelle_Ausstattung'),
                                                paste0("Aerztliches_Personal/", Arzt_searchstring, "/Aerzte_ohne_Belegaerzte/Fachaerzte/Personalerfassung"),
                                                c("Anzahl_VK",
                                                  "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                                                  "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                                                  "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                                                  "Versorgungsform/Stationaere_Versorgung/Anzahl_VK",
                                                  "Versorgungsform/Stationaere_Versorgung/Fall_je_Anzahl")) %>%
            mutate(Personalkategorie = "Fachaerzte")

    }



    Gesundheits_Krankenpfleger <- qb_extract_simple_section(xml_find_first(obj, './Personelle_Ausstattung'),
                                                            "Pflegekraefte/Gesundheits_Krankenpfleger/Personalerfassung",
                                                            c("Anzahl_VK",
                                                              "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                                                              "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                                                              "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                                                              "Versorgungsform/Stationaere_Versorgung/Anzahl_VK",
                                                              "Versorgungsform/Stationaere_Versorgung/Fall_je_Anzahl")) %>%
        mutate(Personalkategorie = "Gesundheits_Krankenpfleger")


    Gesundheits_Kinderkrankenpfleger <- qb_extract_simple_section(xml_find_first(obj, './Personelle_Ausstattung'),
                                                                  "Pflegekraefte/Gesundheits_Kinderkrankenpfleger/Personalerfassung",
                                                                  c("Anzahl_VK",
                                                                    "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                                                                    "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                                                                    "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                                                                    "Versorgungsform/Stationaere_Versorgung/Anzahl_VK",
                                                                    "Versorgungsform/Stationaere_Versorgung/Fall_je_Anzahl")) %>%
        mutate(Personalkategorie = "Gesundheits_Kinderkrankenpfleger")


    Altenpfleger <- qb_extract_simple_section(xml_find_first(obj, './Personelle_Ausstattung'),
                                              "Pflegekraefte/Altenpfleger/Personalerfassung",
                                              c("Anzahl_VK",
                                                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                                                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                                                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                                                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK",
                                                "Versorgungsform/Stationaere_Versorgung/Fall_je_Anzahl")) %>%
        mutate(Personalkategorie = "Altenpfleger")


    Krankenpflegehelfer <- qb_extract_simple_section(xml_find_first(obj, './Personelle_Ausstattung'),
                                                     "Pflegekraefte/Krankenpflegehelfer/Personalerfassung",
                                                     c("Anzahl_VK",
                                                       "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                                                       "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                                                       "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                                                       "Versorgungsform/Stationaere_Versorgung/Anzahl_VK",
                                                       "Versorgungsform/Stationaere_Versorgung/Fall_je_Anzahl")) %>%
        mutate(Personalkategorie = "Krankenpflegehelfer")


    Pflegehelfer <- qb_extract_simple_section(xml_find_first(obj, './Personelle_Ausstattung'),
                                              "Pflegekraefte/Pflegehelfer/Personalerfassung",
                                              c("Anzahl_VK",
                                                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                                                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                                                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                                                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK",
                                                "Versorgungsform/Stationaere_Versorgung/Fall_je_Anzahl")) %>%
        mutate(Personalkategorie = "Pflegehelfer")


    Hebammen_Entbindungspfleger <- qb_extract_simple_section(xml_find_first(obj, './Personelle_Ausstattung'),
                                                             "Pflegekraefte/Hebammen_Entbindungspfleger/Personalerfassung",
                                                             c("Anzahl_VK",
                                                               "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                                                               "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                                                               "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                                                               "Versorgungsform/Stationaere_Versorgung/Anzahl_VK",
                                                               "Versorgungsform/Stationaere_Versorgung/Fall_je_Anzahl")) %>%
        mutate(Personalkategorie = "Hebammen_Entbindungspfleger")


    Operationstechnische_Assistenz <- qb_extract_simple_section(xml_find_first(obj, './Personelle_Ausstattung'),
                                                             "Pflegekraefte/Operationstechnische_Assistenz/Personalerfassung",
                                                             c("Anzahl_VK",
                                                               "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                                                               "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                                                               "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                                                               "Versorgungsform/Stationaere_Versorgung/Anzahl_VK",
                                                               "Versorgungsform/Stationaere_Versorgung/Fall_je_Anzahl")) %>%
        mutate(Personalkategorie = "Operationstechnische_Assistenz")


    Medizinische_Fachangestellte <- qb_extract_simple_section(xml_find_first(obj, './Personelle_Ausstattung'),
                                                              "Pflegekraefte/Medizinische_Fachangestellte/Personalerfassung",
                                                              c("Anzahl_VK",
                                                                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                                                                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                                                                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                                                                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK",
                                                                "Versorgungsform/Stationaere_Versorgung/Fall_je_Anzahl")) %>%
        mutate(Personalkategorie = "Medizinische_Fachangestellte")



    Diplom_Psychologen <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                './Personelle_Ausstattung/Ausgewaehltes_Therapeutisches_Personal_Psycho/Diplom_Psychologen'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Diplom_Psychologen")


    Klinische_Neuropsychologen <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                './Personelle_Ausstattung/Ausgewaehltes_Therapeutisches_Personal_Psycho/Klinische_Neuropsychologen'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Klinische_Neuropsychologen")


    Psychologische_Psychotherapeuten <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                './Personelle_Ausstattung/Ausgewaehltes_Therapeutisches_Personal_Psycho/Psychologische_Psychotherapeuten'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Psychologische_Psychotherapeuten")


    Kinder_Jugendlichenpsychotherapeuten <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                './Personelle_Ausstattung/Ausgewaehltes_Therapeutisches_Personal_Psycho/Kinder_Jugendlichenpsychotherapeuten'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Kinder_Jugendlichenpsychotherapeuten")


    Psychotherapeuten_in_Ausbildung_waehrend_Taetigkeit <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                './Personelle_Ausstattung/Ausgewaehltes_Therapeutisches_Personal_Psycho/Psychotherapeuten_in_Ausbildung_waehrend_Taetigkeit'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Psychotherapeuten_in_Ausbildung_waehrend_Taetigkeit")


    Physiotherapeuten <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                './Personelle_Ausstattung/Ausgewaehltes_Therapeutisches_Personal_Psycho/Physiotherapeuten'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Physiotherapeuten")


    Ergotherapeuten <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                './Personelle_Ausstattung/Ausgewaehltes_Therapeutisches_Personal_Psycho/Ergotherapeuten'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Ergotherapeuten")


    Sozialpaedagogen <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                './Personelle_Ausstattung/Ausgewaehltes_Therapeutisches_Personal_Psycho/Sozialpaedagogen'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Sozialpaedagogen")




    if (Arzt_searchstring == "Belegabteilung") {

        Personal_OE <- bind_rows(Gesundheits_Krankenpfleger,
                                 Gesundheits_Kinderkrankenpfleger,
                                 Altenpfleger,
                                 Krankenpflegehelfer,
                                 Pflegehelfer,
                                 Hebammen_Entbindungspfleger,
                                 Operationstechnische_Assistenz,
                                 Medizinische_Fachangestellte,
                                 Diplom_Psychologen,
                                 Klinische_Neuropsychologen,
                                 Psychologische_Psychotherapeuten,
                                 Kinder_Jugendlichenpsychotherapeuten,
                                 Psychotherapeuten_in_Ausbildung_waehrend_Taetigkeit,
                                 Physiotherapeuten,
                                 Ergotherapeuten,
                                 Sozialpaedagogen)

    } else {

        Personal_OE <- bind_rows(Aerzte_ohne_Belegaerzte,
                                 Fachaerzte,
                                 Gesundheits_Krankenpfleger,
                                 Gesundheits_Kinderkrankenpfleger,
                                 Altenpfleger,
                                 Krankenpflegehelfer,
                                 Pflegehelfer,
                                 Hebammen_Entbindungspfleger,
                                 Operationstechnische_Assistenz,
                                 Medizinische_Fachangestellte,
                                 Diplom_Psychologen,
                                 Klinische_Neuropsychologen,
                                 Psychologische_Psychotherapeuten,
                                 Kinder_Jugendlichenpsychotherapeuten,
                                 Psychotherapeuten_in_Ausbildung_waehrend_Taetigkeit,
                                 Physiotherapeuten,
                                 Ergotherapeuten,
                                 Sozialpaedagogen)

    }

    if (any(!is.na(Personal_OE$Anzahl_VK))) {

        Personal_OE <- Personal_OE %>%
            select(.data$Personalkategorie, everything()) %>%
            filter(!is.na(.data$Anzahl_VK)) %>%
            mutate(across(everything(), ~ if_else(is.na(.x), "0", .x ), .names = "{.col}"))

    } else {

        Personal_OE <- tibble("Personalkategorie" = NA_character_,
                              "Anzahl_VK" = NA_character_,
                              "Beschaeftigungsverhaeltnis_Personal_mit_direktem_BV_Anzahl_VK" = NA_character_,
                              "Beschaeftigungsverhaeltnis_Personal_ohne_direktem_BV_Anzahl_VK" = NA_character_,
                              "Versorgungsform_Ambulante_Versorgung_Anzahl_VK" = NA_character_,
                              "Versorgungsform_Stationaere_Versorgung_Anzahl_VK" = NA_character_,
                              "Versorgungsform_Stationaere_Versorgung_Fall_je_Anzahl" = NA_character_)

    }


    if (!is.na(xml_find_first(obj, './Personelle_Ausstattung/Aerztliche_Fachexpertisen'))) {

        Aerztliche_Fachexpertisen <- qb_extract_simple_section(xml_find_first(obj, './Personelle_Ausstattung/Aerztliche_Fachexpertisen'),
                                                               "Fachexpertise",
                                                               c("AQ_ZF_Schluessel"))

    } else {

        Aerztliche_Fachexpertisen <- tibble("AQ_ZF_Schluessel" = NA_character_)

    }


    if (!is.na(xml_find_first(obj, './Personelle_Ausstattung/Pflegerische_Fachexpertisen'))) {

        Pflegerische_Fachexpertisen <- qb_extract_simple_section(xml_find_first(obj, './Personelle_Ausstattung/Pflegerische_Fachexpertisen'),
                                                                 "Fachexpertise_Pflege",
                                                                 c("PQ_ZP_Schluessel"))

    } else {

        Pflegerische_Fachexpertisen <- tibble("PQ_ZP_Schluessel" = NA_character_)

    }




    return(list("OE_Stammdaten" = OE_Stammdaten,
                "Aerztliche_Leitung_OE" = Aerztliche_Leitung_OE,
                "Kontakt_OE_Adresse" = Kontakt_OE_Adresse,
                "FA_Schluessel" = FA_Schluessel,
                "Medizinische_Leistungsangebote" = Medizinische_Leistungsangebote,
                "Barrierefreiheit_OE" = Barrierefreiheit_OE,
                "Hauptdiagnosen" = Hauptdiagnosen,
                "Prozeduren" = Prozeduren,
                "Ambulante_Behandlungsmoeglichkeiten" = Ambulante_Behandlungsmoeglichkeiten,
                "Ambulanz_116b" = Ambulanz_116b,
                "Ambulante_Operationen" = Ambulante_Operationen,
                "Personal_OE" = Personal_OE,
                "Belegaerzte_OE" = Belegaerzte_OE,
                "Aerztliche_Fachexpertisen" = Aerztliche_Fachexpertisen,
                "Pflegerische_Fachexpertisen" = Pflegerische_Fachexpertisen))

}
