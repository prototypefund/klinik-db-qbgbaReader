#' Extract all information regarding the topic "Akademische Lehre".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param file_name A single string with the name of the XML-file that is
#'     currently processed. No default.
#'
#' @param Fachabteilungen A \code{data.frame} that comprises information about
#'     all possible departments types. For the exact columns that are needed,
#'     see \link{Fachabteilungen_2019}.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item HospitalOperator
#'         \item Krankenhaus_Name
#'         \item Krankenhaus_IK
#'         \item Krankenhaus_Standortnummer
#'         \item Krankenhaus_Adressen
#'         \item Krankenhaus_Telefon
#'         \item Krankenhaus_Leitungspersonen
#'         \item Standort_Name
#'         \item Standort_IK
#'         \item Standort_Standortnummer
#'         \item Standort_Adressen
#'         \item Standort_Telefon
#'         \item Standort_Leitungspersonen
#'         \item Universitaetsklinikum
#'         \item Akademisches_Lehrkrankenhaus
#'         \item Name_Universitaet
#'         \item Psychiatrisches_Krankenhaus
#'         \item Psychiatrie_Versorgungsverpflichtung
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
#' file_name <- stringr::str_replace(basename(doc_path), "-xml\\.xml$", "")
#'
#' qb_extractor_Allgemeine_Informationen(doc,
#'                                       file_name = file_name,
#'                                       Fachabteilungen = Fachabteilungen_2019)
#' }
#'
#' @export
#'
qb_extractor_Allgemeine_Informationen <- function(obj,
                                                  file_name,
                                                  Fachabteilungen) {

    # obj <- details

    # tibble("idAddress" = NA_integer_,
    #        "use" = "Standort_Zugang",
    #        "street" = Standort_Zugang_Strasse,
    #        "housenumber" = Standort_Zugang_Hausnummer,
    #        "zip" = Standort_Zugang_PLZ,
    #        "city" = Standort_Zugang_Ort,
    #        "district" = NA_character_,
    #        "state" = NA_character_,
    #        "country" = ifelse(str_detect(Standort_Zugang_PLZ, "^[0-9]{5,5}$"), "DE", NA_character_),
    #        "URL" = Standort_Zugang_URL,
    #        "type" = NA_character_,
    #        "text" = NA_character_,
    #        "lat" = NA_real_,
    #        "lon" = NA_real_)


    # Krankenhaustraeger ------------------------------------------------------

    Krankenhaustraeger_Name <- xml_text(xml_find_first(obj, '//Krankenhaustraeger/Name'))
    Krankenhaustraeger_Art <- xml_text(xml_find_first(obj, '//Krankenhaustraeger/Krankenhaustraeger_Art'))

    HospitalOperator <- tibble("idHospitalOperator" = NA_integer_,
                               "description" = Krankenhaustraeger_Name,
                               "type" = Krankenhaustraeger_Art)


    # Krankenhaus -------------------------------------------------------------

    Krankenhaus_Name <- xml_text(xml_find_first(obj, '//Krankenhaus/Krankenhauskontaktdaten/Name|//Krankenhaus/Kontaktdaten/Name'))
    Krankenhaus_IK <- xml_text(xml_find_first(obj, '//Krankenhaus/Krankenhauskontaktdaten/IK|//Krankenhaus/Kontaktdaten/IK'))
    Krankenhaus_Standortnummer <- xml_text(xml_find_first(obj, '//Krankenhaus/Krankenhauskontaktdaten/Standortnummer|//Krankenhaus/Kontaktdaten/Standortnummer'))


    # Krankenhauskontaktdaten -------------------------------------------------

    Krankenhaus_Zugang_Strasse <- xml_text(xml_find_first(obj, '//Krankenhaus/Krankenhauskontaktdaten/Kontakt_Zugang/Strasse|//Krankenhaus/Kontaktdaten/Kontakt_Zugang/Strasse'))
    Krankenhaus_Zugang_Hausnummer <- xml_text(xml_find_first(obj, '//Krankenhaus/Krankenhauskontaktdaten/Kontakt_Zugang/Hausnummer|//Krankenhaus/Kontaktdaten/Kontakt_Zugang/Hausnummer'))
    Krankenhaus_Zugang_PLZ <- xml_text(xml_find_first(obj, '//Krankenhaus/Krankenhauskontaktdaten/Kontakt_Zugang/Postleitzahl|//Krankenhaus/Kontaktdaten/Kontakt_Zugang/Postleitzahl'))
    Krankenhaus_Zugang_Ort <- xml_text(xml_find_first(obj, '//Krankenhaus/Krankenhauskontaktdaten/Kontakt_Zugang/Ort|//Krankenhaus/Kontaktdaten/Kontakt_Zugang/Ort'))
    Krankenhaus_Zugang_URL <- xml_text(xml_find_first(obj, '//Krankenhaus/Krankenhauskontaktdaten/Kontakt_Zugang/URL_Zugang|//Krankenhaus/Kontaktdaten/Kontakt_Zugang/URL_Zugang'))

    Krankenhaus_Kontakt_Strasse <- xml_text(xml_find_first(obj, '//Krankenhaus/Krankenhauskontaktdaten/Kontakt_Adresse/Adresse/Strasse|//Krankenhaus/Kontaktdaten/Kontakt_Adresse/Adresse/Strasse'))
    Krankenhaus_Kontakt_Hausnummer <- xml_text(xml_find_first(obj, '//Krankenhaus/Krankenhauskontaktdaten/Kontakt_Adresse/Adresse/Hausnummer|//Krankenhaus/Kontaktdaten/Kontakt_Adresse/Adresse/Hausnummer'))
    Krankenhaus_Kontakt_PLZ <- xml_text(xml_find_first(obj, '//Krankenhaus/Krankenhauskontaktdaten/Kontakt_Adresse/Postleitzahl|//Krankenhaus/Kontaktdaten/Kontakt_Adresse/Postleitzahl'))
    Krankenhaus_Kontakt_Ort <- xml_text(xml_find_first(obj, '//Krankenhaus/Krankenhauskontaktdaten/Kontakt_Adresse/Ort|//Krankenhaus/Kontaktdaten/Kontakt_Adresse/Ort'))

    Krankenhaus_Telefon_Vorwahl <- xml_text(xml_find_first(obj, '//Krankenhaus/Krankenhauskontaktdaten/Telefon/Vorwahl|//Krankenhaus/Kontaktdaten/Telefon/Vorwahl'))
    Krankenhaus_Telefon_Rufnummer <- xml_text(xml_find_first(obj, '//Krankenhaus/Krankenhauskontaktdaten/Telefon/Rufnummer|//Krankenhaus/Kontaktdaten/Telefon/Rufnummer'))
    Krankenhaus_Telefon_Durchwahl <- xml_text(xml_find_first(obj, '//Krankenhaus/Krankenhauskontaktdaten/Telefon/Durchwahl|//Krankenhaus/Kontaktdaten/Telefon/Durchwahl'))


    Krankenhaus_Telefon <- paste0(ifelse(is.na(Krankenhaus_Telefon_Vorwahl), "", paste0(Krankenhaus_Telefon_Vorwahl, " - ")),
                                  ifelse(is.na(Krankenhaus_Telefon_Rufnummer), "", Krankenhaus_Telefon_Rufnummer),
                                  ifelse(is.na(Krankenhaus_Telefon_Durchwahl), "", paste0(" ", Krankenhaus_Telefon_Durchwahl)))
    Krankenhaus_Telefon <- ifelse(Krankenhaus_Telefon == "", NA_character_, Krankenhaus_Telefon)
    rm(Krankenhaus_Telefon_Vorwahl, Krankenhaus_Telefon_Rufnummer, Krankenhaus_Telefon_Durchwahl)

    Krankenhaus_Zugang <- tibble("idAddress" = NA_integer_,
                                 "use" = "Krankenhaus_Zugang",
                                 "street" = Krankenhaus_Zugang_Strasse,
                                 "housenumber" = Krankenhaus_Zugang_Hausnummer,
                                 "zip" = Krankenhaus_Zugang_PLZ,
                                 "city" = Krankenhaus_Zugang_Ort,
                                 "district" = NA_character_,
                                 "state" = NA_character_,
                                 "country" = ifelse(str_detect(Krankenhaus_Zugang_PLZ, "^[0-9]{5,5}$"), "DE", NA_character_),
                                 "URL" = Krankenhaus_Zugang_URL,
                                 "type" = NA_character_,
                                 "text" = NA_character_,
                                 "lat" = NA_real_,
                                 "lon" = NA_real_)

    Krankenhaus_Kontakt <- tibble("idAddress" = NA_integer_,
                                  "use" = "Krankenhaus_Kontakt",
                                  "street" = Krankenhaus_Kontakt_Strasse,
                                  "housenumber" = Krankenhaus_Kontakt_Hausnummer,
                                  "zip" = Krankenhaus_Kontakt_PLZ,
                                  "city" = Krankenhaus_Kontakt_Ort,
                                  "district" = NA_character_,
                                  "state" = NA_character_,
                                  "country" = ifelse(str_detect(Krankenhaus_Kontakt_PLZ, "^[0-9]{5,5}$"), "DE", NA_character_),
                                  "URL" = NA_character_,
                                  "type" = NA_character_,
                                  "text" = NA_character_,
                                  "lat" = NA_real_,
                                  "lon" = NA_real_)

    Krankenhaus_Adressen <- bind_rows(Krankenhaus_Zugang,
                                      Krankenhaus_Kontakt)


    # Krankenhaus Leitung -----------------------------------------------------

    Krankenhaus_Aerztliche_Leitung <- map_dfr(xml_find_all(obj, "//Krankenhaus/Krankenhauskontaktdaten/Aerztliche_Leitung/Kontakt_Person_lang|//Krankenhaus/Kontaktdaten/Aerztliche_Leitung/Kontakt_Person_lang"),
                                              ~ qb_extract_person(.x, role = "Krankenhaus_Aerztliche_Leitung"))

    Krankenhaus_Pflegedienstleitung <- map_dfr(xml_find_all(obj, "//Krankenhaus/Krankenhauskontaktdaten/Pflegedienstleitung/Kontakt_Person_lang|//Krankenhaus/Kontaktdaten/Pflegedienstleitung/Kontakt_Person_lang"),
                                               ~ qb_extract_person(.x, role = "Krankenhaus_Pflegedienstleitung"))

    Krankenhaus_Verwaltungsleitung <- map_dfr(xml_find_all(obj, "//Krankenhaus/Krankenhauskontaktdaten/Verwaltungsleitung/Kontakt_Person_lang|//Krankenhaus/Kontaktdaten/Verwaltungsleitung/Kontakt_Person_lang"),
                                              ~ qb_extract_person(.x, role = "Krankenhaus_Verwaltungsleitung"))


    Krankenhaus_Leitungspersonen <- bind_rows(Krankenhaus_Aerztliche_Leitung,
                                              Krankenhaus_Pflegedienstleitung,
                                              Krankenhaus_Verwaltungsleitung)




    # Standortkontaktdaten ----------------------------------------------------

    if (is.na(xml_find_first(obj, "//Einziger_Standort"))) {

        Standort_Name <- xml_text(xml_find_first(obj, '//Krankenhaus/Standortkontaktdaten/Name|//Standort_dieses_Berichts/Kontaktdaten/Name'))
        Standort_IK <- xml_text(xml_find_first(obj, '//Krankenhaus/Standortkontaktdaten/IK|//Standort_dieses_Berichts/Kontaktdaten/IK'))
        Standort_Standortnummer <- xml_text(xml_find_first(obj, '//Krankenhaus/Standortkontaktdaten/Standortnummer|//Standort_dieses_Berichts/Kontaktdaten/Standortnummer'))

        if (Krankenhaus_Standortnummer == "99" &&
            str_detect(file_name, "([0-9]{9,9})-99-([0-9]{4,4})") &&
            Standort_Standortnummer != "99") {

            Standort_Standortnummer <- "99"

        }

        Standort_Zugang_Strasse <- xml_text(xml_find_first(obj, '//Krankenhaus/Standortkontaktdaten/Kontakt_Zugang/Strasse|//Standort_dieses_Berichts/Kontaktdaten//Kontakt_Zugang/Strasse'))
        Standort_Zugang_Hausnummer <- xml_text(xml_find_first(obj, '//Krankenhaus/Standortkontaktdaten/Kontakt_Zugang/Hausnummer|//Standort_dieses_Berichts/Kontaktdaten/Kontakt_Zugang/Hausnummer'))
        Standort_Zugang_PLZ <- xml_text(xml_find_first(obj, '//Krankenhaus/Standortkontaktdaten/Kontakt_Zugang/Postleitzahl|//Standort_dieses_Berichts/Kontaktdaten/Kontakt_Zugang/Postleitzahl'))
        Standort_Zugang_Ort <- xml_text(xml_find_first(obj, '//Krankenhaus/Standortkontaktdaten/Kontakt_Zugang/Ort|//Standort_dieses_Berichts/Kontaktdaten/Kontakt_Zugang/Ort'))
        Standort_Zugang_URL <- xml_text(xml_find_first(obj, '//Krankenhaus/Standortkontaktdaten/Kontakt_Zugang/URL_Zugang|//Standort_dieses_Berichts/Kontaktdaten/Kontakt_Zugang/URL_Zugang'))

        Standort_Kontakt_Strasse <- xml_text(xml_find_first(obj, '//Krankenhaus/Standortkontaktdaten/Kontakt_Adresse/Adresse/Strasse|//Standort_dieses_Berichts/Kontaktdaten/Kontakt_Adresse/Adresse/Strasse'))
        Standort_Kontakt_Hausnummer <- xml_text(xml_find_first(obj, '//Krankenhaus/Standortkontaktdaten/Kontakt_Adresse/Adresse/Hausnummer|//Standort_dieses_Berichts/Kontaktdaten/Kontakt_Adresse/Adresse/Hausnummer'))
        Standort_Kontakt_PLZ <- xml_text(xml_find_first(obj, '//Krankenhaus/Standortkontaktdaten/Kontakt_Adresse/Postleitzahl|//Standort_dieses_Berichts/Kontaktdaten/Kontakt_Adresse/Postleitzahl'))
        Standort_Kontakt_Ort <- xml_text(xml_find_first(obj, '//Krankenhaus/Standortkontaktdaten/Kontakt_Adresse/Ort|//Standort_dieses_Berichts/Kontaktdaten/Kontakt_Adresse/Ort'))

        Standort_Telefon_Vorwahl <- xml_text(xml_find_first(obj, '//Krankenhaus/Standortkontaktdaten/Telefon/Vorwahl|//Standort_dieses_Berichts/Kontaktdaten/Telefon/Vorwahl'))
        Standort_Telefon_Rufnummer <- xml_text(xml_find_first(obj, '//Krankenhaus/Standortkontaktdaten/Telefon/Rufnummer|//Standort_dieses_Berichts/Kontaktdaten/Telefon/Rufnummer'))
        Standort_Telefon_Durchwahl <- xml_text(xml_find_first(obj, '//Krankenhaus/Standortkontaktdaten/Telefon/Durchwahl|//Standort_dieses_Berichts/Kontaktdaten/Telefon/Durchwahl'))
        Standort_Telefon <- paste0(ifelse(is.na(Standort_Telefon_Vorwahl), "", paste0(Standort_Telefon_Vorwahl, " - ")),
                                   ifelse(is.na(Standort_Telefon_Rufnummer), "", Standort_Telefon_Rufnummer),
                                   ifelse(is.na(Standort_Telefon_Durchwahl), "", paste0(" ", Standort_Telefon_Durchwahl)))
        Standort_Telefon <- ifelse(Standort_Telefon == "", NA_character_, Standort_Telefon)
        rm(Standort_Telefon_Vorwahl, Standort_Telefon_Rufnummer, Standort_Telefon_Durchwahl)

        Standort_Zugang <- tibble("idAddress" = NA_integer_,
                                  "use" = "Standort_Zugang",
                                  "street" = Standort_Zugang_Strasse,
                                  "housenumber" = Standort_Zugang_Hausnummer,
                                  "zip" = Standort_Zugang_PLZ,
                                  "city" = Standort_Zugang_Ort,
                                  "district" = NA_character_,
                                  "state" = NA_character_,
                                  "country" = ifelse(str_detect(Standort_Zugang_PLZ, "^[0-9]{5,5}$"), "DE", NA_character_),
                                  "URL" = Standort_Zugang_URL,
                                  "type" = NA_character_,
                                  "text" = NA_character_,
                                  "lat" = NA_real_,
                                  "lon" = NA_real_)

        Standort_Kontakt <- tibble("idAddress" = NA_integer_,
                                   "use" = "Standort_Kontakt",
                                   "street" = Standort_Kontakt_Strasse,
                                   "housenumber" = Standort_Kontakt_Hausnummer,
                                   "zip" = Standort_Kontakt_PLZ,
                                   "city" = Standort_Kontakt_Ort,
                                   "district" = NA_character_,
                                   "state" = NA_character_,
                                   "country" = ifelse(str_detect(Standort_Kontakt_PLZ, "^[0-9]{5,5}$"), "DE", NA_character_),
                                   "URL" = NA_character_,
                                   "type" = NA_character_,
                                   "text" = NA_character_,
                                   "lat" = NA_real_,
                                   "lon" = NA_real_)

        Standort_Adressen <- bind_rows(Standort_Zugang,
                                       Standort_Kontakt)

    } else {


        Standort_Name <- NA_character_
        Standort_IK <- NA_character_
        Standort_Standortnummer <- NA_character_

        Standort_Zugang_Strasse <- NA_character_
        Standort_Zugang_Hausnummer <- NA_character_
        Standort_Zugang_PLZ <- NA_character_
        Standort_Zugang_Ort <- NA_character_
        Standort_Zugang_URL <- NA_character_

        Standort_Kontakt_Strasse <- NA_character_
        Standort_Kontakt_Hausnummer <- NA_character_
        Standort_Kontakt_PLZ <- NA_character_
        Standort_Kontakt_Ort <- NA_character_

        Standort_Telefon <- NA_character_

        Standort_Zugang <- tibble("idAddress" = NA_integer_,
                                  "use" = "Standort_Zugang",
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

        Standort_Kontakt <- tibble("idAddress" = NA_integer_,
                                   "use" = "Standort_Kontakt",
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

        Standort_Adressen <- bind_rows(Standort_Zugang,
                                       Standort_Kontakt)

    }




    # Standort Leitung --------------------------------------------------------

    Standort_Aerztliche_Leitung <- map_dfr(xml_find_all(obj, "//Krankenhaus/Standortkontaktdaten/Aerztliche_Leitung/Kontakt_Person_lang|//Standort_dieses_Berichts/Kontaktdaten/Aerztliche_Leitung/Kontakt_Person_lang"),
                                           ~ qb_extract_person(.x, role = "Standort_Aerztliche_Leitung"))

    Standort_Pflegedienstleitung <- map_dfr(xml_find_all(obj, "//Krankenhaus/Standortkontaktdaten/Pflegedienstleitung/Kontakt_Person_lang//Standort_dieses_Berichts/Kontaktdaten/Pflegedienstleitung/Kontakt_Person_lang"),
                                            ~ qb_extract_person(.x, role = "Standort_Pflegedienstleitung"))

    Standort_Verwaltungsleitung <- map_dfr(xml_find_all(obj, "//Krankenhaus/Standortkontaktdaten/Verwaltungsleitung/Kontakt_Person_lang//Standort_dieses_Berichts/Kontaktdaten/Verwaltungsleitung/Kontakt_Person_lang"),
                                           ~ qb_extract_person(.x, role = "Standort_Verwaltungsleitung"))

    Standort_Leitungspersonen <- bind_rows(Standort_Aerztliche_Leitung,
                                           Standort_Pflegedienstleitung,
                                           Standort_Verwaltungsleitung)


    # Krankenhaus-Eigenschaften -----------------------------------------------

    Universitaetsklinikum <- ifelse(is.na(xml_text(xml_find_first(obj, '//Universitaetsklinikum'))), 0L, 1L)
    Akademisches_Lehrkrankenhaus <- ifelse(is.na(xml_text(xml_find_first(obj, '//Akademisches_Lehrkrankenhaus'))), 0L, 1L)
    Name_Universitaet <- paste0(xml_text(xml_find_all(obj, '//Akademisches_Lehrkrankenhaus/Name_Universitaet')), collapse = " / ")

    Psychiatrisches_Krankenhaus <- ifelse(any(xml_text(xml_find_all(obj, '//Fachabteilungsschluessel/FA_Schluessel')) %in%
                                                  unname(unlist(Fachabteilungen[str_detect(Fachabteilungen$Bezeichnung,
                                                                                           "([Pp]sychiatrie)|([Pp]sychother)|([Pp]sychoso)"),
                                                                                "Fachabteilungsschluessel"]))), 1L, 0L)
    Psychiatrie_Versorgungsverpflichtung <- ifelse(is.na(xml_text(xml_find_first(obj, '//Versorgungsverpflichtung_Psychiatrie'))), 0L, 1L)



    return(list("HospitalOperator" = HospitalOperator,
                "Krankenhaus_Name" = Krankenhaus_Name,
                "Krankenhaus_IK" = Krankenhaus_IK,
                "Krankenhaus_Standortnummer" = Krankenhaus_Standortnummer,
                "Krankenhaus_Adressen" = Krankenhaus_Adressen,
                "Krankenhaus_Telefon" = Krankenhaus_Telefon,
                "Krankenhaus_Leitungspersonen" = Krankenhaus_Leitungspersonen,
                "Standort_Name" = Standort_Name,
                "Standort_IK" = Standort_IK,
                "Standort_Standortnummer" = Standort_Standortnummer,
                "Standort_Adressen" = Standort_Adressen,
                "Standort_Telefon" = Standort_Telefon,
                "Standort_Leitungspersonen" = Standort_Leitungspersonen,
                "Universitaetsklinikum" = Universitaetsklinikum,
                "Akademisches_Lehrkrankenhaus" = Akademisches_Lehrkrankenhaus,
                "Name_Universitaet" = Name_Universitaet,
                "Psychiatrisches_Krankenhaus" = Psychiatrisches_Krankenhaus,
                "Psychiatrie_Versorgungsverpflichtung" = Psychiatrie_Versorgungsverpflichtung))


}
