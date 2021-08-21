#' Extract all information regarding the topic "Beschwerdemanagement".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item ComplaintManagement_exists
#'         \item ComplaintManagement
#'         \item Ansprechpartner_Beschwerdemanagement
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Beschwerdemanagement(doc, 1L)
#'
#'
#' @export
#'
qb_extractor_Beschwerdemanagement <- function(obj, Hospital_id) {

    # obj <- details
    # Hospital_id <- 1L

    Strukturiertes_Beschwerdemanagement_Eingefuehrt <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Beschwerdemanagement'
        ),
        "Strukturiertes_Beschwerdemanagement",
        c("Strukturiertes_Beschwerdemanagement_Eingefuehrt",
          "Strukturiertes_Beschwerdemanagement_eingefuehrt",
          "Erlaeuterungen")
    ) %>%
        mutate(Strukturiertes_Beschwerdemanagement_Eingefuehrt = case_when(
           !is.na(.data$Strukturiertes_Beschwerdemanagement_Eingefuehrt) &
               is.na(.data$Strukturiertes_Beschwerdemanagement_eingefuehrt) ~ .data$Strukturiertes_Beschwerdemanagement_Eingefuehrt,
           is.na(.data$Strukturiertes_Beschwerdemanagement_Eingefuehrt) &
               !is.na(.data$Strukturiertes_Beschwerdemanagement_eingefuehrt) ~ .data$Strukturiertes_Beschwerdemanagement_eingefuehrt
        )) %>%
        select(-.data$Strukturiertes_Beschwerdemanagement_eingefuehrt) %>%
        mutate(Strukturiertes_Beschwerdemanagement_Eingefuehrt = case_when(
            .data$Strukturiertes_Beschwerdemanagement_Eingefuehrt == "" ~ "ja",
            TRUE ~ .data$Strukturiertes_Beschwerdemanagement_Eingefuehrt)) %>%
        mutate(Sachverhalt = "Strukturiertes_Beschwerdemanagement_Eingefuehrt") %>%
        rename("Status" = .data$Strukturiertes_Beschwerdemanagement_Eingefuehrt) %>%
        select(.data$Sachverhalt, everything())


    Schriftliches_Konzept_Existiert <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Beschwerdemanagement'
        ),
        "Schriftliches_Konzept",
        c("Schriftliches_Konzept_Existiert",
          "Schriftliches_Konzept_existiert",
          "Erlaeuterungen")
    ) %>%
        mutate(Schriftliches_Konzept_Existiert = case_when(
            !is.na(.data$Schriftliches_Konzept_Existiert) &
                is.na(.data$Schriftliches_Konzept_existiert) ~ .data$Schriftliches_Konzept_Existiert,
            is.na(.data$Schriftliches_Konzept_Existiert) &
                !is.na(.data$Schriftliches_Konzept_existiert) ~ .data$Schriftliches_Konzept_existiert
        )) %>%
        select(-.data$Schriftliches_Konzept_existiert) %>%
        mutate(Schriftliches_Konzept_Existiert = case_when(
            .data$Schriftliches_Konzept_Existiert == "" ~ "ja",
            TRUE ~ .data$Schriftliches_Konzept_Existiert)) %>%
        mutate(Sachverhalt = "Schriftliches_Konzept_Existiert") %>%
        rename("Status" = .data$Schriftliches_Konzept_Existiert) %>%
        select(.data$Sachverhalt, everything())


    Umgang_muendliche_Beschwerden_Geregelt <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Beschwerdemanagement'
        ),
        "Umgang_muendliche_Beschwerden",
        c("Umgang_muendliche_Beschwerden_Geregelt",
          "Umgang_muendliche_Beschwerden_geregelt",
          "Erlaeuterungen")
    ) %>%
        mutate(Umgang_muendliche_Beschwerden_Geregelt = case_when(
            !is.na(.data$Umgang_muendliche_Beschwerden_Geregelt) &
                is.na(.data$Umgang_muendliche_Beschwerden_geregelt) ~ .data$Umgang_muendliche_Beschwerden_Geregelt,
            is.na(.data$Umgang_muendliche_Beschwerden_Geregelt) &
                !is.na(.data$Umgang_muendliche_Beschwerden_geregelt) ~ .data$Umgang_muendliche_Beschwerden_geregelt
        )) %>%
        select(-.data$Umgang_muendliche_Beschwerden_geregelt) %>%
        mutate(Umgang_muendliche_Beschwerden_Geregelt = case_when(
            .data$Umgang_muendliche_Beschwerden_Geregelt == "" ~ "ja",
            TRUE ~ .data$Umgang_muendliche_Beschwerden_Geregelt)) %>%
        mutate(Sachverhalt = "Umgang_muendliche_Beschwerden_Geregelt") %>%
        rename("Status" = .data$Umgang_muendliche_Beschwerden_Geregelt) %>%
        select(.data$Sachverhalt, everything())


    Umgang_schriftliche_Beschwerden_Geregelt <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Beschwerdemanagement'
        ),
        "Umgang_schriftliche_Beschwerden",
        c("Umgang_schriftliche_Beschwerden_Geregelt",
          "Umgang_schriftliche_Beschwerden_geregelt",
          "Erlaeuterungen")
    ) %>%
        mutate(Umgang_schriftliche_Beschwerden_Geregelt = case_when(
            !is.na(.data$Umgang_schriftliche_Beschwerden_Geregelt) &
                is.na(.data$Umgang_schriftliche_Beschwerden_geregelt) ~ .data$Umgang_schriftliche_Beschwerden_Geregelt,
            is.na(.data$Umgang_schriftliche_Beschwerden_Geregelt) &
                !is.na(.data$Umgang_schriftliche_Beschwerden_geregelt) ~ .data$Umgang_schriftliche_Beschwerden_geregelt
        )) %>%
        select(-.data$Umgang_schriftliche_Beschwerden_geregelt) %>%
        mutate(Umgang_schriftliche_Beschwerden_Geregelt = case_when(
            .data$Umgang_schriftliche_Beschwerden_Geregelt == "" ~ "ja",
            TRUE ~ .data$Umgang_schriftliche_Beschwerden_Geregelt)) %>%
        mutate(Sachverhalt = "Umgang_schriftliche_Beschwerden_Geregelt") %>%
        rename("Status" = .data$Umgang_schriftliche_Beschwerden_Geregelt) %>%
        select(.data$Sachverhalt, everything())


    Zeitziele_fuer_Rueckmeldung_Definiert <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Beschwerdemanagement'
        ),
        "Zeitziele_fuer_Rueckmeldung",
        c("Zeitziele_fuer_Rueckmeldung_Definiert",
          "Zeitziele_fuer_Rueckmeldung_definiert",
          "Erlaeuterungen")
    ) %>%
        mutate(Zeitziele_fuer_Rueckmeldung_Definiert = case_when(
            !is.na(.data$Zeitziele_fuer_Rueckmeldung_Definiert) &
                is.na(.data$Zeitziele_fuer_Rueckmeldung_definiert) ~ .data$Zeitziele_fuer_Rueckmeldung_Definiert,
            is.na(.data$Zeitziele_fuer_Rueckmeldung_Definiert) &
                !is.na(.data$Zeitziele_fuer_Rueckmeldung_definiert) ~ .data$Zeitziele_fuer_Rueckmeldung_definiert
        )) %>%
        select(-.data$Zeitziele_fuer_Rueckmeldung_definiert) %>%
        mutate(Zeitziele_fuer_Rueckmeldung_Definiert = case_when(
            .data$Zeitziele_fuer_Rueckmeldung_Definiert == "" ~ "ja",
            TRUE ~ .data$Zeitziele_fuer_Rueckmeldung_Definiert)) %>%
        mutate(Sachverhalt = "Zeitziele_fuer_Rueckmeldung_Definiert") %>%
        rename("Status" = .data$Zeitziele_fuer_Rueckmeldung_Definiert) %>%
        select(.data$Sachverhalt, everything())


    Anonyme_Eingabemoeglichkeiten_Existieren <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Beschwerdemanagement'
        ),
        "Anonyme_Eingabemoeglichkeiten",
        c("Anonyme_Eingabemoeglichkeiten_Existieren",
          "Anonyme_Eingabemoeglichkeiten_existieren",
          "Erlaeuterungen",
          "URL_Kontaktformular")
    ) %>%
        mutate(Anonyme_Eingabemoeglichkeiten_Existieren = case_when(
            !is.na(.data$Anonyme_Eingabemoeglichkeiten_Existieren) &
                is.na(.data$Anonyme_Eingabemoeglichkeiten_existieren) ~ .data$Anonyme_Eingabemoeglichkeiten_Existieren,
            is.na(.data$Anonyme_Eingabemoeglichkeiten_Existieren) &
                !is.na(.data$Anonyme_Eingabemoeglichkeiten_existieren) ~ .data$Anonyme_Eingabemoeglichkeiten_existieren
        )) %>%
        select(-.data$Anonyme_Eingabemoeglichkeiten_existieren) %>%
        mutate(Anonyme_Eingabemoeglichkeiten_Existieren = case_when(
            .data$Anonyme_Eingabemoeglichkeiten_Existieren == "" ~ "ja",
            TRUE ~ .data$Anonyme_Eingabemoeglichkeiten_Existieren)) %>%
        mutate(Sachverhalt = "Anonyme_Eingabemoeglichkeiten_Existieren") %>%
        rename("Status" = .data$Anonyme_Eingabemoeglichkeiten_Existieren,
               "URL" = .data$URL_Kontaktformular) %>%
        select(.data$Sachverhalt, everything())


    Patientenbefragungen_Durchgefuehrt <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Beschwerdemanagement'
        ),
        "Patientenbefragungen",
        c("Patientenbefragungen_Durchgefuehrt",
          "Patientenbefragungen_durchgefuehrt",
          "Erlaeuterungen",
          "URL")
    ) %>%
        mutate(Patientenbefragungen_Durchgefuehrt = case_when(
            !is.na(.data$Patientenbefragungen_Durchgefuehrt) &
                is.na(.data$Patientenbefragungen_durchgefuehrt) ~ .data$Patientenbefragungen_Durchgefuehrt,
            is.na(.data$Patientenbefragungen_Durchgefuehrt) &
                !is.na(.data$Patientenbefragungen_durchgefuehrt) ~ .data$Patientenbefragungen_durchgefuehrt
        )) %>%
        select(-.data$Patientenbefragungen_durchgefuehrt) %>%
        mutate(Patientenbefragungen_Durchgefuehrt = case_when(
            .data$Patientenbefragungen_Durchgefuehrt == "" ~ "ja",
            TRUE ~ .data$Patientenbefragungen_Durchgefuehrt)) %>%
        mutate(Sachverhalt = "Patientenbefragungen_Durchgefuehrt") %>%
        rename("Status" = .data$Patientenbefragungen_Durchgefuehrt) %>%
        select(.data$Sachverhalt, everything())


    Einweiserbefragungen_Durchgefuehrt <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Beschwerdemanagement'
        ),
        "Einweiserbefragungen",
        c("Einweiserbefragungen_Durchgefuehrt",
          "Einweiserbefragungen_durchgefuehrt",
          "Erlaeuterungen",
          "URL")
    ) %>%
        mutate(Einweiserbefragungen_Durchgefuehrt = case_when(
            !is.na(.data$Einweiserbefragungen_Durchgefuehrt) &
                is.na(.data$Einweiserbefragungen_durchgefuehrt) ~ .data$Einweiserbefragungen_Durchgefuehrt,
            is.na(.data$Einweiserbefragungen_Durchgefuehrt) &
                !is.na(.data$Einweiserbefragungen_durchgefuehrt) ~ .data$Einweiserbefragungen_durchgefuehrt
        )) %>%
        select(-.data$Einweiserbefragungen_durchgefuehrt) %>%
        mutate(Einweiserbefragungen_Durchgefuehrt = case_when(
            .data$Einweiserbefragungen_Durchgefuehrt == "" ~ "ja",
            TRUE ~ .data$Einweiserbefragungen_Durchgefuehrt)) %>%
        mutate(Sachverhalt = "Einweiserbefragungen_Durchgefuehrt") %>%
        rename("Status" = .data$Einweiserbefragungen_Durchgefuehrt) %>%
        select(.data$Sachverhalt, everything())


    ComplaintManagement <- bind_rows(Strukturiertes_Beschwerdemanagement_Eingefuehrt,
                                     Schriftliches_Konzept_Existiert,
                                     Umgang_muendliche_Beschwerden_Geregelt,
                                     Umgang_schriftliche_Beschwerden_Geregelt,
                                     Zeitziele_fuer_Rueckmeldung_Definiert,
                                     Anonyme_Eingabemoeglichkeiten_Existieren,
                                     Patientenbefragungen_Durchgefuehrt,
                                     Einweiserbefragungen_Durchgefuehrt) %>%
        mutate(idComplaintManagement = NA_integer_,
               Hospital_idHospital = Hospital_id) %>%
        rename("description" = .data$Sachverhalt,
               "status" = .data$Status,
               "comment" = .data$Erlaeuterungen) %>%
        select(.data$idComplaintManagement, everything())


    if (any(!ComplaintManagement[, -c(1,2,6)] %>% mutate(across(everything(), is.na)))) {

        ComplaintManagement <- ComplaintManagement %>% filter(if_any(.cols = -c(1,2,6), ~ !is.na(.)))

        ComplaintManagement_exists <- TRUE

    } else {

        ComplaintManagement_exists <- FALSE

        ComplaintManagement <- NA_character_

    }



    if (!is.na(
        xml_find_first(
            obj,
            "//Beschwerdemanagement/Ansprechpartner/Ansprechpersonen/Kontakt_Person_lang"
        )
    )) {
        Ansprechpartner_Beschwerdemanagement <-
            map_dfr(
                xml_find_all(
                    obj,
                    "//Beschwerdemanagement/Ansprechpartner/Ansprechpersonen/Kontakt_Person_lang"
                ),
                ~ qb_extract_person(.x, role = "Ansprechpartner_Beschwerdemanagement")
            )

    } else {

        Ansprechpartner_Beschwerdemanagement <- tibble("idPerson" = NA_integer_,
                                                       "role" = NA_character_,
                                                       "title" = NA_character_,
                                                       "firstname" = NA_character_,
                                                       "lastname" = NA_character_,
                                                       "responsibility" = NA_character_,
                                                       "phone" = NA_character_,
                                                       "fax" = NA_character_,
                                                       "email" = NA_character_)

    }


    if (!is.na(
        xml_find_first(
            obj,
            "//Beschwerdemanagement/Patientenfuersprecher/Fuersprechpersonen/Kontakt_Person_lang"
        )
    )) {
        Patientenfuersprecher <-
            map_dfr(
                xml_find_all(
                    obj,
                    "//Beschwerdemanagement/Patientenfuersprecher/Fuersprechpersonen/Kontakt_Person_lang"
                ),
                ~ qb_extract_person(.x, role = "Patientenfuersprecher")
            )

    } else {

        Patientenfuersprecher <- tibble("idPerson" = NA_integer_,
                                        "role" = NA_character_,
                                        "title" = NA_character_,
                                        "firstname" = NA_character_,
                                        "lastname" = NA_character_,
                                        "responsibility" = NA_character_,
                                        "phone" = NA_character_,
                                        "fax" = NA_character_,
                                        "email" = NA_character_)

    }


    return(list("ComplaintManagement_exists" = ComplaintManagement_exists,
                "ComplaintManagement" = ComplaintManagement,
                "Ansprechpartner_Beschwerdemanagement" = Ansprechpartner_Beschwerdemanagement,
                "Patientenfuersprecher" = Patientenfuersprecher))

}
