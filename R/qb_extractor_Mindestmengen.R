#' Extract all information regarding the topic "Mindestmengen".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item MinimumQuantities_exists
#'         \item MinimumQuantities
#'         \item MinimumQuantitiesPrognosis_exists
#'         \item MinimumQuantitiesPrognosis
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Mindestmengen(doc, 1L)
#'
#'
#' @export
#'
qb_extractor_Mindestmengen <- function(obj, Hospital_id) {


    # obj <- details
    # Hospital_id <- 1L

    ### TODO - In years before 2019, there was an alternative empty element "Keine_Ausnahme"
    ### This information is not important, but it is still not extracted by the code...


    Mindestmengen <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Qualitaetssicherung/Mindestmengen'
        ),
        "Leistungsbereich",
        c("Bezeichnung",
          "Erbrachte_Menge",
          "Begruendung/MM_Schluessel",
          "Begruendung/MM_Ausnahme_Datum")
    )

    MinimumQuantities <- Mindestmengen %>%
        mutate(idMinimumQuantities = NA_integer_,
               Hospital_idHospital = Hospital_id) %>%
        rename("description" = .data$Bezeichnung,
               "quantity" = .data$Erbrachte_Menge,
               "code" = .data$Begruendung_MM_Schluessel,
               "dateSinceWhen" = .data$Begruendung_MM_Ausnahme_Datum) %>%
        select(.data$idMinimumQuantities, everything())


    if (any(!MinimumQuantities[, -6] %>% mutate(across(everything(), is.na)))) {

        MinimumQuantities_exists <- TRUE

    } else {

        MinimumQuantities_exists <- FALSE

        MinimumQuantities <- NA_character_

    }

    Mindestmengen_Leistungsberechtigung_Prognose <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Qualitaetssicherung/Mindestmengen/Mindestmengen_Leistungsberechtigung_Prognose'
        ),
        "Leistungsbereich",
        c("Bezeichnung",
          "Gesamtergebnis_Prognosedarlegung",
          "./Leistungsmengen_Prognoseermittlung/Leistungsmenge_Berichtsjahr",
          "./Leistungsmengen_Prognoseermittlung/Q3_4_Q1_2_Leistungsmenge",
          "Pruefung_Landesverbaende",
          "Ausnahmetatbestand",
          "Ergebnis_Pruefung_Landesbehoerden",
          "Uebergangsregelung")
    )


    if (any(!Mindestmengen_Leistungsberechtigung_Prognose %>% mutate(across(everything(), is.na)))) {

        MinimumQuantitiesPrognosis_exists <- TRUE

        MinimumQuantitiesPrognosis <- Mindestmengen_Leistungsberechtigung_Prognose %>%
            rename("Prognose_Berichtsjahr" = .data$`._Leistungsmengen_Prognoseermittlung_Leistungsmenge_Berichtsjahr`,
                   "Menge_Q3_4_Q1_2" = .data$`._Leistungsmengen_Prognoseermittlung_Q3_4_Q1_2_Leistungsmenge`) %>%
            rename("description" = .data$Bezeichnung,
                   "resultPrognosis" = .data$Gesamtergebnis_Prognosedarlegung,
                   "prognosisReportingYear" = .data$Prognose_Berichtsjahr,
                   "quantityQ34Q12" = .data$Menge_Q3_4_Q1_2,
                   "auditByRegionalAuthority" = .data$Pruefung_Landesverbaende,
                   "exceptionalCase" = .data$Ausnahmetatbestand,
                   "resultOfAudit" = .data$Ergebnis_Pruefung_Landesbehoerden,
                   "transitionalArrangement" = .data$Uebergangsregelung) %>%
            mutate(idMinimumQuantitiesPrognosis = NA_integer_,
                   Hospital_idHospital = Hospital_id) %>%
            select(.data$idMinimumQuantitiesPrognosis, everything())

    } else {

        MinimumQuantitiesPrognosis_exists <- FALSE

        MinimumQuantitiesPrognosis <- NA_character_

    }


    return(list("MinimumQuantities_exists" = MinimumQuantities_exists,
                "MinimumQuantities" = MinimumQuantities,
                "MinimumQuantitiesPrognosis_exists" = MinimumQuantitiesPrognosis_exists,
                "MinimumQuantitiesPrognosis" = MinimumQuantitiesPrognosis))

}
