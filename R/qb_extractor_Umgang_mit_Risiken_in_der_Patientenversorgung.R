#' Extract all information regarding the topic "Umgang mit Risiken in der Patientenversorgung".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item Qualitymanagement_exists
#'         \item Qualitymanagement
#'         \item Riskmanagement_exists
#'         \item Riskmanagement
#'         \item Persons_QM_RM
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Umgang_mit_Risiken_in_der_Patientenversorgung(doc, 1L)
#'
#'
#' @export
#'
qb_extractor_Umgang_mit_Risiken_in_der_Patientenversorgung <- function(obj, Hospital_id) {

    QM_Lenkungsgremium_Tagungsfrequenz <-
        xml_text(
            xml_find_first(
                obj,
                '//Umgang_mit_Risiken_in_der_Patientenversorgung/Lenkungsgremium_Qualitaetsmanagement/Lenkungsgremium/Tagungsfrequenz'
            )
        )
    QM_Lenkungsgremium_Beteiligte_Abteilungen <-
        xml_text(
            xml_find_first(
                obj,
                '//Umgang_mit_Risiken_in_der_Patientenversorgung/Lenkungsgremium_Qualitaetsmanagement/Lenkungsgremium/Beteiligte_Abteilungen_Funktionsbereiche'
            )
        )


    Qualitymanagement <- tibble(
        "idQualitymanagement" = NA_integer_,
        "frequency" = QM_Lenkungsgremium_Tagungsfrequenz,
        "involvedDepartments" = QM_Lenkungsgremium_Beteiligte_Abteilungen,
        "Hospital_idHospital" = Hospital_id
    )

    if (any(!is.na(Qualitymanagement$frequency)) ||
        any(!is.na(Qualitymanagement$involvedDepartments))) {

        Qualitymanagement_exists <- TRUE

    } else {

        Qualitymanagement_exists <- FALSE

        Qualitymanagement <- NA_character_

    }



    Risikomanagement_Lenkungsgremium_Tagungsfrequenz <-
        xml_text(
            xml_find_first(
                obj,
                '//Umgang_mit_Risiken_in_der_Patientenversorgung/Lenkungsgremium_Risikomanagement/Lenkungsgremium/Tagungsfrequenz'
            )
        )
    Risikomanagement_Lenkungsgremium_Beteiligte_Abteilungen <-
        xml_text(
            xml_find_first(
                obj,
                '//Umgang_mit_Risiken_in_der_Patientenversorgung/Lenkungsgremium_Risikomanagement/Lenkungsgremium/Beteiligte_Abteilungen_Funktionsbereiche'
            )
        )

    Riskmanagement <- tibble(
        "idRiskmanagement" = NA_integer_,
        "frequency" = Risikomanagement_Lenkungsgremium_Tagungsfrequenz,
        "involvedDepartments" = Risikomanagement_Lenkungsgremium_Beteiligte_Abteilungen,
        "Hospital_idHospital" = Hospital_id
    )

    if (any(!is.na(Riskmanagement$frequency)) ||
        any(!is.na(Riskmanagement$involvedDepartments))) {

        Riskmanagement_exists <- TRUE

    } else {

        Riskmanagement_exists <- FALSE

        Riskmanagement <- NA_character_

    }



    Verantwortliche_Person_Risikomanagement_xml <- xml_find_first(
        obj,
        '//Umgang_mit_Risiken_in_der_Patientenversorgung/Verantwortliche_Person_Risikomanagement/Kontakt_Person_lang'
    )

    if (!is.na(Verantwortliche_Person_Risikomanagement_xml)) {

        Verantwortliche_Person_Risikomanagement <-
            qb_extract_person(Verantwortliche_Person_Risikomanagement_xml,
                              role = "Verantwortliche_Person_Risikomanagement"
            )

    }

    Verantwortliche_Person_QM_xml <- xml_find_first(
        obj,
        '//Umgang_mit_Risiken_in_der_Patientenversorgung/Verantwortliche_Person_Qualitaetsmanagement/Kontakt_Person_lang'
    )

    if (!is.na(Verantwortliche_Person_QM_xml)) {

        Verantwortliche_Person_QM <-
            qb_extract_person(Verantwortliche_Person_QM_xml,
                              role = "Verantwortliche_Person_QM"
            )

    }

    if (!is.na(Verantwortliche_Person_Risikomanagement_xml) && !is.na(Verantwortliche_Person_QM_xml)) {

        Persons_QM_RM <- bind_rows(Verantwortliche_Person_Risikomanagement,
                                   Verantwortliche_Person_QM)

    } else if (!is.na(Verantwortliche_Person_Risikomanagement_xml) && is.na(Verantwortliche_Person_QM_xml)) {

        Persons_QM_RM <- Verantwortliche_Person_Risikomanagement

    } else if (is.na(Verantwortliche_Person_Risikomanagement_xml) && !is.na(Verantwortliche_Person_QM_xml)) {

        Persons_QM_RM <- Verantwortliche_Person_QM

    } else {

        Persons_QM_RM <- tibble("idPerson" = NA_integer_,
                                "role" = NA_character_,
                                "title" = NA_character_,
                                "firstname" = NA_character_,
                                "lastname" = NA_character_,
                                "responsibility" = NA_character_,
                                "phone" = NA_character_,
                                "fax" = NA_character_,
                                "email" = NA_character_)

    }


    return(list("Qualitymanagement_exists" = Qualitymanagement_exists,
                "Qualitymanagement" = Qualitymanagement,
                "Riskmanagement_exists" = Riskmanagement_exists,
                "Riskmanagement" = Riskmanagement,
                "Persons_QM_RM" = Persons_QM_RM))

}
