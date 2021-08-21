#' Extract all information regarding the topic "Einleitung".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item Dataset_metadata
#'         \item URL_Internetseite_Krankenhaus
#'         \item URL_Weitere_Informationen
#'         \item Further_links
#'         \item Verantwortlicher_Erstellung_Qualitaetsbericht
#'         \item Verantwortlicher_Krankenhausleitung
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Einleitung(doc)
#'
#'
#' @export
#'
qb_extractor_Einleitung <- function(obj) {

    # obj <- details

    Datensatz_Datum <- xml_text(xml_find_first(obj, '//Einleitung/Datensatz/Datum'))
    Datensatz_Uhrzeit <- xml_text(xml_find_first(obj, '//Einleitung/Datensatz/Uhrzeit'))
    Datensatz_Sprache <- xml_text(xml_find_first(obj, '//Einleitung/Datensatz/Sprache'))
    Software_Hersteller <- xml_text(xml_find_first(obj, '//Einleitung/Software/Hersteller'))
    Software_Produktname <- xml_text(xml_find_first(obj, '//Einleitung/Software/Produktname'))
    Software_Version <- xml_text(xml_find_first(obj, '//Einleitung/Software/Version'))
    URL_Internetseite_Krankenhaus <- xml_text(xml_find_first(obj, '//Einleitung/URL_Internetseite_Krankenhaus|//Einleitung/URL_Homepage_Krankenhaus'))
    URL_Weitere_Informationen <- xml_text(xml_find_first(obj, '//Einleitung/URL_Weitere_Informationen'))
    Weiterfuehrende_Links <- qb_extract_simple_section(xml_find_first(obj, '//Einleitung/Weiterfuehrende_Links'),
                                                       "Link",
                                                       c("URL", "Beschreibung"))


    ### The objects "Further_Links" and "Dataset_metadata" do not have a variable
    ### "Hospital_idHospital" at this time, because these infos are needed before
    ### this value is known. It has to be added to these objects later on in the
    ### parent function!

    FurtherLinks <- Weiterfuehrende_Links %>%
        rename("link" = .data$URL,
               "description" = .data$Beschreibung) %>%
        mutate(idFurtherLinks = NA_integer_) %>%
        select(.data$idFurtherLinks, everything()) %>%
        filter(!is.na(.data$link))


    Dataset_metadata <- tibble("idDataset_metadata" = NA_integer_,
                               "time" = Datensatz_Uhrzeit,
                               "date" = Datensatz_Datum,
                               "language" = Datensatz_Sprache,
                               "softwareCompany" = Software_Hersteller,
                               "softwareProgram" = Software_Produktname,
                               "softwareVersion" = Software_Version)


    Verantwortlicher_Erstellung_Qualitaetsbericht <- map_dfr(xml_find_all(obj, "//Verantwortlicher_Erstellung/Kontakt_Person_lang"),
                                                             ~ qb_extract_person(.x, role = "Verantwortlicher_Erstellung_Qualitaetsbericht"))


    Verantwortlicher_Krankenhausleitung <- map_dfr(xml_find_all(obj, "//Verantwortlicher_Krankenhausleitung/Kontakt_Person_lang"),
                                                   ~ qb_extract_person(.x, role = "Verantwortlicher_Krankenhausleitung"))


    return(list("Dataset_metadata" = Dataset_metadata,
                "URL_Internetseite_Krankenhaus" = URL_Internetseite_Krankenhaus,
                "URL_Weitere_Informationen" = URL_Weitere_Informationen,
                "FurtherLinks" = FurtherLinks,
                "Verantwortlicher_Erstellung_Qualitaetsbericht" = Verantwortlicher_Erstellung_Qualitaetsbericht,
                "Verantwortlicher_Krankenhausleitung" = Verantwortlicher_Krankenhausleitung))



}
