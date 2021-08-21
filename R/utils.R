#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#'
#' @rdname pipe
#'
#' @keywords internal
#'
#' @importFrom magrittr %>%
#'
#' @usage lhs \%>\% rhs
#'
#' @param lhs A value or the magrittr placeholder.
#'
#' @param rhs A function call using the magrittr semantics.
#'
#' @return The result of calling `rhs(lhs)`.
#'
#' @export
#'
NULL


### Helper Functions:

# Get either a question mark or "IS NULL" for a key-value-pair:

equal_or_null_condition <- function(key, value) {

    if (is.na(value)) {

        result <- paste0("`", key, "` IS NULL")

    } else {

        result <- paste0("`", key, "` = ?")

    }

    return(result)

}


# Use grep to search for the use of an XML-elements name in package files.
# Utilize
grep_after_elements <- function(search_string, search_dir = "./R/*.R", intern = FALSE, only_first = FALSE) {

    result_file <- system(paste0("grep -l ", search_string, " ",
                                 ifelse(only_first, "  --max-count=1 ", ""),
                                 search_dir),
                          intern = intern)

    if (only_first) {

        result_string <- paste0(search_string, ": ", result_file[[1]])

    } else {

        result_string <- paste0(search_string, ": ", paste0(result_file, collapse = "; "))

    }

    return(result_string)

}
grep_after_elements_possible <- purrr::possibly(grep_after_elements, otherwise = NA_character_)


# Sanitize string values:

# string_sanitizer <- function(string) {
#
#     # string <- str_replace_all(string, "", "")
#
#
# }


replace_html_characters <- function(string) {

    # &#38; suchen und ersetzen: & \u0026
    string <- str_replace_all(string, "&#38;", "\u0026")

    return(string)

}



get_OE_contact <- function(obj) {

    Kontakt_OE_Strasse <- xml_text(xml_find_first(obj, "./Kontakt_Zugang/Strasse"))
    Kontakt_OE_Hausnummer <- xml_text(xml_find_first(obj, "./Kontakt_Zugang/Hausnummer"))
    Kontakt_OE_PLZ <- xml_text(xml_find_first(obj, "./Kontakt_Zugang/Postleitzahl"))
    Kontakt_OE_Ort <- xml_text(xml_find_first(obj, "./Kontakt_Zugang/Ort"))
    Kontakt_OE_URL_Zugang <- xml_text(xml_find_first(obj, "./Kontakt_Zugang/URL_Zugang"))

    single_Kontakt_OE_Adresse <- tibble("idAddress" = NA_integer_,
                                        "use" = "OE_Zugang",
                                        "street" = Kontakt_OE_Strasse,
                                        "housenumber" = Kontakt_OE_Hausnummer,
                                        "zip" = Kontakt_OE_PLZ,
                                        "city" = Kontakt_OE_Ort,
                                        "district" = NA_character_,
                                        "state" = NA_character_,
                                        "country" = ifelse(str_detect(Kontakt_OE_PLZ, "^[0-9]{5,5}$"), "DE", NA_character_),
                                        "URL" = Kontakt_OE_URL_Zugang,
                                        "type" = NA_character_,
                                        "text" = NA_character_,
                                        "lat" = NA_real_,
                                        "lon" = NA_real_)

    return(single_Kontakt_OE_Adresse)

}



# LS_names <- ls(pattern = "^lists_")
# for (i in seq_along(LS_names)) {
#
#     current_LS <- get(LS_names[[i]])
#
#     for (j in seq_along(current_LS)[-1]) {
#
#         write_excel_csv2(current_LS[[j]],
#                          file = paste0("./00-data/Auswahllisten_CSVs/",
#                                        str_replace(LS_names[[i]], "lists_", ""),
#                                        "_", names(current_LS[j]), ".csv"))
#
#     }
#
#     rm(current_LS)
#
# }


