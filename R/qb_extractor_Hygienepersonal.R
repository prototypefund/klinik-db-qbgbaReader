#' Extract all information regarding the topic "Hygienepersonal".
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
#'         \item HygieneStaff_exists
#'         \item HygieneStaff
#'         \item HygieneCommission_frequency
#'         \item Ansprechpartner_Hygienekommission
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Hygienepersonal(doc, 1L, "2019")
#'
#'
#' @export
#'
qb_extractor_Hygienepersonal <- function(obj, Hospital_id, year) {

    # obj <- details
    # Hospital_id <- 1L
    # year <- "2015"


    if (is.null(year) ||
        length(year) != 1 ||
        !is.character(year) ||
        !year %in% c("2015", "2016", "2017", "2018", "2019")) {

        stop('Parameter "year" must be a single string (permitted values: "2015", "2016", "2017", "2018" or "2019").')

    }


    Krankenhaushygieniker <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Hygienepersonal'
        ),
        "Krankenhaushygieniker",
        c("Anzahl",
          "Erlaeuterungen")
    )  %>%
        mutate(Berufsgruppe = "Krankenhaushygieniker")


    if (year %in% c("2016", "2017", "2018", "2019")) {

        Hygienebeauftragte_Aerzte <- qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Hygienepersonal'
            ),
            "Hygienebeauftragte_Aerzte",
            c("./Hygienepersonalerfassung/Anzahl",
              "Erlaeuterungen")
        ) %>%
        rename("Anzahl" = .data$`._Hygienepersonalerfassung_Anzahl`) %>%
            mutate(Berufsgruppe = "Hygienebeauftragte_Aerzte")


        if (is.na(Hygienebeauftragte_Aerzte$Anzahl) &&
            !is.na(xml_find_first(
                obj,
                '//Hygienepersonal/Hygienebeauftragte_Aerzte/Hygienepersonalerfassung/Keine_Angabe_aufgrund_fehlender_landesrechtlicher_Vorgaben'
            ))) {

            Hygienebeauftragte_Aerzte$Anzahl <- "Keine_Angabe_aufgrund_fehlender_landesrechtlicher_Vorgaben"

        }


        Hygienefachkraefte <- qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Hygienepersonal'
            ),
            "Hygienefachkraefte",
            c("./Hygienepersonalerfassung/Anzahl",
              "Erlaeuterungen")
        ) %>%
            rename("Anzahl" = .data$`._Hygienepersonalerfassung_Anzahl`) %>%
            mutate(Berufsgruppe = "Hygienefachkraefte")

        if (is.na(Hygienefachkraefte$Anzahl) &&
            !is.na(xml_find_first(
                obj,
                '//Hygienepersonal/Hygienefachkraefte/Hygienepersonalerfassung/Keine_Angabe_aufgrund_fehlender_landesrechtlicher_Vorgaben'
            ))) {

            Hygienefachkraefte$Anzahl <- "Keine_Angabe_aufgrund_fehlender_landesrechtlicher_Vorgaben"

        }


        Hygienebeauftragte_Pflege <- qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Hygienepersonal'
            ),
            "Hygienebeauftragte_Pflege",
            c("./Hygienepersonalerfassung/Anzahl",
              "Erlaeuterungen")
        ) %>%
            rename("Anzahl" = .data$`._Hygienepersonalerfassung_Anzahl`) %>%
            mutate(Berufsgruppe = "Hygienebeauftragte_Pflege")


        if (is.na(Hygienebeauftragte_Pflege$Anzahl) &&
            !is.na(xml_find_first(
                obj,
                '//Hygienepersonal/Hygienebeauftragte_Pflege/Hygienepersonalerfassung/Keine_Angabe_aufgrund_fehlender_landesrechtlicher_Vorgaben'
            ))) {

            Hygienebeauftragte_Pflege$Anzahl <- "Keine_Angabe_aufgrund_fehlender_landesrechtlicher_Vorgaben"

        }

    } else if (year %in% c("2015")) {

        Hygienebeauftragte_Aerzte <- qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Hygienepersonal'
            ),
            "Hygienebeauftragte_Aerzte",
            c("Anzahl",
              "Erlaeuterungen")
        ) %>%
            mutate(Berufsgruppe = "Hygienebeauftragte_Aerzte")


        Hygienefachkraefte <- qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Hygienepersonal'
            ),
            "Hygienefachkraefte",
            c("Anzahl",
              "Erlaeuterungen")
        ) %>%
            mutate(Berufsgruppe = "Hygienefachkraefte")


        Hygienebeauftragte_Pflege <- qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Hygienepersonal'
            ),
            "Hygienebeauftragte_Pflege",
            c("Anzahl",
              "Erlaeuterungen")
        ) %>%
            mutate(Berufsgruppe = "Hygienebeauftragte_Pflege")

    }





    Hygienepersonal <- bind_rows(Krankenhaushygieniker,
                                 Hygienebeauftragte_Aerzte,
                                 Hygienefachkraefte,
                                 Hygienebeauftragte_Pflege)

    if (any(!is.na(Hygienepersonal$Anzahl))) {

        HygieneStaff_exists <- TRUE

        HygieneStaff <- Hygienepersonal %>%
            mutate(idHygieneStaff = NA_integer_,
                   Hospital_idHospital = Hospital_id) %>%
            rename("amount" = .data$Anzahl,
                   "comment" = .data$Erlaeuterungen,
                   "occupationalGroup" = .data$Berufsgruppe) %>%
            select(.data$idHygieneStaff, .data$occupationalGroup,
                   everything()) %>%
            mutate(amount = case_when(
                is.na(.data$amount) ~ "0",
                TRUE ~ .data$amount
            ))
    } else {

        HygieneStaff_exists <- FALSE

        HygieneStaff <- NA_character_

    }


    if (!is.na(xml_find_first(obj, '//Hygienepersonal/Hygienekommission_nicht_eingerichtet'))) {

        Hygienekommission_Tagungsfrequenz <- "Hygienekommission_nicht_eingerichtet"

    } else {

        Hygienekommission_Tagungsfrequenz <- xml_text(xml_find_first(obj, '//Hygienepersonal/Hygienekommission_eingerichtet/Tagungsfrequenz'))

    }


    Ansprechpartner_Hygienekommission_xml <- xml_find_first(
        obj,
        '//Umgang_mit_Risiken_in_der_Patientenversorgung/Hygienepersonal/Hygienekommission_eingerichtet/Kontakt_Person_kurz'
    )

    if (!is.na(Ansprechpartner_Hygienekommission_xml)) {

        Ansprechpartner_Hygienekommission <-
            qb_extract_person(Ansprechpartner_Hygienekommission_xml,
                              role = "Ansprechpartner_Hygienekommission"
            )

    } else {

        Ansprechpartner_Hygienekommission <- tibble("idPerson" = NA_integer_,
                                                    "role" = NA_character_,
                                                    "title" = NA_character_,
                                                    "firstname" = NA_character_,
                                                    "lastname" = NA_character_,
                                                    "responsibility" = NA_character_,
                                                    "phone" = NA_character_,
                                                    "fax" = NA_character_,
                                                    "email" = NA_character_)

    }

    return(list("HygieneStaff_exists" = HygieneStaff_exists,
                "HygieneStaff" = HygieneStaff,
                "HygieneCommission_frequency" = Hygienekommission_Tagungsfrequenz,
                "Ansprechpartner_Hygienekommission" = Ansprechpartner_Hygienekommission))


}
