#' Extract all information regarding the topic "Strukturqualitaetsvereinbarung".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item MeasuresQM_exists
#'         \item MeasuresQM
#'         \item messageNonPerformanceNursingCareGiven
#'         \item clearingDialogueSQM
#'         \item clearingDialogueSQMCompleted
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Strukturqualitaetsvereinbarung(doc, 1L)
#'
#'
#' @export
#'
qb_extractor_Strukturqualitaetsvereinbarung <- function(obj, Hospital_id) {

    # obj <- details
    # Hospital_id <- 1L


    Strukturqualitaetsvereinbarung <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Qualitaetssicherung/Strukturqualitaetsvereinbarung'
        ),
        "Vereinbarung",
        c("CQ_Schluessel",
          "Erlaeuterungen")
    )


    MeasuresQM <- Strukturqualitaetsvereinbarung %>%
        rename("code" = .data$CQ_Schluessel,
               "comment" = .data$Erlaeuterungen) %>%
        mutate(idMeasuresQM = NA_integer_,
               Hospital_idHospital = Hospital_id) %>%
        select(.data$idMeasuresQM, everything()) %>%
        filter(!is.na(.data$code))


    if (nrow(MeasuresQM) > 0) {

        MeasuresQM_exists <- TRUE

    } else {

        MeasuresQM_exists <- FALSE

        MeasuresQM <- NA_character_

    }



    Mitteilung_Nichterfuellung_pflegerische_Versorung_nicht_erfolgt <-
        ifelse(is.na(
            xml_find_first(
                obj,
                '//Strukturqualitaetsvereinbarung/Angabe_Erfuellung_Personalvorgaben/Mitteilung_Nichterfuellung_pflegerische_Versorung_nicht_erfolgt'
            )
        ), 0L, 1L)

    Mitteilung_Nichterfuellung_pflegerische_Versorung_erfolgt <-
        ifelse(is.na(
            xml_find_first(
                obj,
                '//Strukturqualitaetsvereinbarung/Angabe_Erfuellung_Personalvorgaben/Mitteilung_Nichterfuellung_pflegerische_Versorung_erfolgt'
            )
        ), 0L, 1L)

    if (Mitteilung_Nichterfuellung_pflegerische_Versorung_nicht_erfolgt == 0L &&
        Mitteilung_Nichterfuellung_pflegerische_Versorung_erfolgt == 1L) {

        messageNonPerformanceNursingCareGiven <- 1L

        Keine_Teilnahme_klaerender_Dialog <-
            ifelse(is.na(
                xml_find_first(
                    obj,
                    '//Strukturqualitaetsvereinbarung/Angabe_Erfuellung_Personalvorgaben/Mitteilung_Nichterfuellung_pflegerische_Versorung_erfolgt/Keine_Teilnahme_klaerender_Dialog'
                )
            ), 0L, 1L)

        Teilnahme_klaerender_Dialog <-
            ifelse(is.na(
                xml_find_first(
                    obj,
                    '//Strukturqualitaetsvereinbarung/Angabe_Erfuellung_Personalvorgaben/Mitteilung_Nichterfuellung_pflegerische_Versorung_erfolgt/Teilnahme_klaerender_Dialog'
                )
            ), 0L, 1L)


        if (Keine_Teilnahme_klaerender_Dialog == 0L &&
            Teilnahme_klaerender_Dialog == 1L) {

            clearingDialogueSQM <- 1L


            Klaerender_Dialog_nicht_abgeschlossen <-
                ifelse(is.na(
                    xml_find_first(
                        obj,
                        '//Strukturqualitaetsvereinbarung/Angabe_Erfuellung_Personalvorgaben/Mitteilung_Nichterfuellung_pflegerische_Versorung_erfolgt/Teilnahme_klaerender_Dialog/Klaerender_Dialog_nicht_abgeschlossen'
                    )
                ), 0L, 1L)


            Klaerender_Dialog_abgeschlossen <-
                ifelse(is.na(
                    xml_find_first(
                        obj,
                        '//Strukturqualitaetsvereinbarung/Angabe_Erfuellung_Personalvorgaben/Mitteilung_Nichterfuellung_pflegerische_Versorung_erfolgt/Teilnahme_klaerender_Dialog/Klaerender_Dialog_abgeschlossen'
                    )
                ), 0L, 1L)


            if (Klaerender_Dialog_nicht_abgeschlossen == 0L &&
                Klaerender_Dialog_abgeschlossen == 1L) {

                clearingDialogueSQMCompleted <- 1L

            } else {

                clearingDialogueSQMCompleted <- 0L

            }



        } else {

            clearingDialogueSQM <- 0L

            clearingDialogueSQMCompleted <- NA_integer_

        }

    } else {

        messageNonPerformanceNursingCareGiven <- 0L

        clearingDialogueSQM <- NA_integer_

        clearingDialogueSQMCompleted <- NA_integer_

    }



    return(list("MeasuresQM_exists" = MeasuresQM_exists,
                "MeasuresQM" = MeasuresQM,
                "messageNonPerformanceNursingCareGiven" = messageNonPerformanceNursingCareGiven,
                "clearingDialogueSQM" = clearingDialogueSQM,
                "clearingDialogueSQMCompleted" = clearingDialogueSQMCompleted))

}
