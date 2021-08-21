#' Extract all information regarding the topic "Nicht Medizinische Leistungsangebote".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item NonMedicalServices_exists
#'         \item NonMedicalServices
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Nicht_Medizinische_Leistungsangebote(doc, 1L)
#'
#'
#' @export
#'
qb_extractor_Nicht_Medizinische_Leistungsangebote <- function(obj, Hospital_id) {

    # obj <- details
    # Hospital_id = 1L

    Nicht_Medizinische_Leistungsangebote <-
        qb_extract_simple_section(
            xml_find_first(obj, '//Nicht_Medizinische_Leistungsangebote'),
            "NM_Leistungsangebot",
            c("NM_Schluessel",
              "Erlaeuterungen",
              "Zusatzangaben/Mediennutzung/Kosten_pro_Tag",
              "Zusatzangaben/Besondere_Ernaehrungsgewohnheiten",
              "Zusatzangaben/Telefonnutzung/Kosten_pro_Tag",
              "Zusatzangaben/Telefonnutzung/Kosten_pro_Minute_ins_deutsche_Festnetz",
              "Zusatzangaben/Telefonnutzung/Kosten_pro_Minute_bei_eintreffenden_Anrufen",
              "Zusatzangaben/Parkplatznutzung/Kosten_pro_Stunde_Max",
              "Zusatzangaben/Parkplatznutzung/Kosten_pro_Tag_Max")
        ) %>%
        mutate(Zusatzangaben = NA_character_) %>%
        mutate(Zusatzangaben = case_when(
            is.na(.data$Zusatzangaben) &
                !is.na(.data$Zusatzangaben_Mediennutzung_Kosten_pro_Tag) ~ paste0(.data$Zusatzangaben_Mediennutzung_Kosten_pro_Tag,
                                                                                  " (Kosten_pro_Tag)"),
            is.na(.data$Zusatzangaben) &
                !is.na(.data$Zusatzangaben_Besondere_Ernaehrungsgewohnheiten) ~ paste0(.data$Zusatzangaben_Besondere_Ernaehrungsgewohnheiten,
                                                                                  " (Besondere_Ernaehrungsgewohnheiten)"),
            is.na(.data$Zusatzangaben) &
                !is.na(.data$Zusatzangaben_Telefonnutzung_Kosten_pro_Tag) ~ paste0(.data$Zusatzangaben_Telefonnutzung_Kosten_pro_Tag,
                                                                                  " (Kosten_pro_Tag) / ",
                                                                                  .data$Zusatzangaben_Telefonnutzung_Kosten_pro_Minute_ins_deutsche_Festnetz,
                                                                                  " (Kosten_pro_Minute_ins_deutsche_Festnetz) / ",
                                                                                  .data$Zusatzangaben_Telefonnutzung_Kosten_pro_Minute_bei_eintreffenden_Anrufen,
                                                                                  " (Kosten_pro_Minute_bei_eintreffenden_Anrufen)"),
            is.na(.data$Zusatzangaben) &
                !is.na(.data$Zusatzangaben_Parkplatznutzung_Kosten_pro_Stunde_Max) ~ paste0(.data$Zusatzangaben_Parkplatznutzung_Kosten_pro_Stunde_Max,
                                                                                            " (Kosten_pro_Stunde_Max) / ",
                                                                                            .data$Zusatzangaben_Parkplatznutzung_Kosten_pro_Tag_Max,
                                                                                            " (Kosten_pro_Tag_Max)")
        )) %>%
        select(.data$NM_Schluessel,
               .data$Erlaeuterungen,
               .data$Zusatzangaben)

    NonMedicalServices <- Nicht_Medizinische_Leistungsangebote %>%
        mutate(idNonMedicalServices = NA_integer_,
               Hospital_idHospital = Hospital_id) %>%
        rename("code" = .data$NM_Schluessel,
               "comment" = .data$Erlaeuterungen,
               "additionalInformation" = .data$Zusatzangaben) %>%
        select(.data$idNonMedicalServices, everything()) %>%
        filter(!is.na(.data$code))

    if (nrow(NonMedicalServices) > 0) {

        NonMedicalServices_exists <- TRUE

    } else {

        NonMedicalServices_exists <- FALSE

        NonMedicalServices <- NA_character_

    }


    return(list("NonMedicalServices_exists" = NonMedicalServices_exists,
                "NonMedicalServices" = NonMedicalServices))

}
