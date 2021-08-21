#' Extract name and contact data of a Person.
#'
#' @param obj An XML-element of type "Kontakt_Person_lang" or "Kontakt_Person_kurz".
#'
#' @param role A string value which is used for inserting a variable \code{role}
#'     listing the role a person mentioned in the XML file is associated with.
#'     Defaults to \code{NA}.
#'
#' @param names_as_in_database A boolean value that if \code{TRUE} indicates that
#'     the result should have column names renamed as they are defined in the database,
#'     which are: \code{idPerson}, \code{role}, \code{title}, \code{firstname},
#'     \code{lastname}, \code{responsibility}, \code{phone}, \code{fax}, and
#'     \code{email}. The variable \code{idPerson} is added as \code{NA_integer}
#'     that represents the primary key in the database table "Person". Default is
#'     \code{TRUE}.
#'
#' @return A tibble with the following columns (all of type "character"): "Titel",
#'     "Vorname", "Nachname", "Funktion_Arbeitsschwerpunkt", "Telefon", "Fax" and
#'      "Email". Empty values are encoding as \code{NA_character}.
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#' x <- xml2::xml_find_all(doc,
#'         '//Einleitung/Verantwortlicher_Erstellung/Kontakt_Person_lang')
#'
#' qb_extract_person(x, role = "Verantwortlicher_Erstellung")
#'
#'
#' @export
#'
qb_extract_person <- function(obj, role = NA_character_, names_as_in_database = TRUE) {

    ### tests

    if (typeof(role) != "character") {

        stop('Argument "role" must be of type "character"!')

    }


    if (((class(obj) == "xml_nodeset") && length(obj) > 1) ||
        ((class(obj) == "xml_node") && length(obj) > 2)) {

        stop('Function "', match.call()[[1]], '" is not vectorized!')

    }

    permitted_elements <- c("Kontakt_Person_kurz", "Kontakt_Person_lang")


    NAME <- trimws(xml_name(obj))

    if (!NAME %in% permitted_elements) {

        stop("This is an ", NAME, " XML-element which is not applicable.")

    }

    ### End of tests


    Titel <- xml_text(xml_find_first(obj, './Person/Titel'))
    Vorname <- xml_text(xml_find_first(obj, './Person/Vorname'))
    Nachname <- xml_text(xml_find_first(obj, './Person/Nachname'))
    Funktion_Arbeitsschwerpunkt <- xml_text(xml_find_first(obj, './Person/Funktion_Arbeitsschwerpunkt'))

    Telefon_Vorwahl <- xml_text(xml_find_first(obj, './Telefon/Vorwahl'))
    Telefon_Rufnummer <- xml_text(xml_find_first(obj, './Telefon/Rufnummer'))
    Telefon_Durchwahl <- xml_text(xml_find_first(obj, './Telefon/Durchwahl'))

    Telefon <- paste0(ifelse(is.na(Telefon_Vorwahl), "", paste0(Telefon_Vorwahl, " - ")),
                      ifelse(is.na(Telefon_Rufnummer), "", Telefon_Rufnummer),
                      ifelse(is.na(Telefon_Durchwahl), "", paste0(" ", Telefon_Durchwahl)))

    Fax_Vorwahl <- xml_text(xml_find_first(obj, './Fax/Vorwahl'))
    Fax_Rufnummer <- xml_text(xml_find_first(obj, './Fax/Rufnummer'))
    Fax_Durchwahl <- xml_text(xml_find_first(obj, './Fax/Durchwahl'))

    Fax <- paste0(ifelse(is.na(Fax_Vorwahl), "", paste0(Fax_Vorwahl, " - ")),
                  ifelse(is.na(Fax_Rufnummer), "", Fax_Rufnummer),
                  ifelse(is.na(Fax_Durchwahl), "", paste0(" ", Fax_Durchwahl)))

    Email <- xml_text(xml_find_first(obj, './Email'))

    df_tmp <- tibble("Rolle" = trimws(str_replace_all(role, "[ ]{2,}", " ")),
                     "Titel" = trimws(str_replace_all(Titel, "[ ]{2,}", " ")),
                     "Vorname" = trimws(str_replace_all(Vorname, "[ ]{2,}", " ")),
                     "Nachname" = trimws(str_replace_all(Nachname, "[ ]{2,}", " ")),
                     "Funktion_Arbeitsschwerpunkt" = trimws(str_replace_all(Funktion_Arbeitsschwerpunkt, "[ ]{2,}", " ")),
                     "Telefon" = trimws(str_replace_all(Telefon, "[ ]{2,}", " ")),
                     "Fax" = trimws(str_replace_all(Fax, "[ ]{2,}", " ")),
                     "Email" = trimws(str_replace_all(Email, "[ ]{2,}", " ")))

    if (names_as_in_database) {

        df_tmp <- df_tmp %>%
            rename("role" = .data$Rolle,
                   "title" = .data$Titel,
                   "firstname" = .data$Vorname,
                   "lastname" = .data$Nachname,
                   "responsibility" = .data$Funktion_Arbeitsschwerpunkt,
                   "phone" = .data$Telefon,
                   "fax" = .data$Fax,
                   "email" = .data$Email) %>%
            mutate(idPerson = NA_integer_) %>%
            select(.data$idPerson, everything())

    }

    return(df_tmp)

}





