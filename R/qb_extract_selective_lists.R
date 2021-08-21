#' Extract all lists from an Excel-file with the selective lists provided by the
#' GBA which are then used in a "Qualitaetsbericht" to encode specific information.
#'
#' @param XLS_path The file path to an Excel-file.
#'
#'
#' @return A named list where each element is a tibble reproducing one sheet from
#'     the original Excel-file. The first tibble contains the table of contents
#'     of the remaining sheets.
#'
#'
#' @examples
#'
#' \dontrun{
#' download.file(
#'   "https://www.g-ba.de/downloads/40-268-7467/2021-04-07_Qb-R_Anlage-3_Auswahllisten.xlsx",
#'   destfile = "test.xlsx", method = "libcurl", mode = "wb")
#'
#' qb_extract_selective_lists("test.xlsx")
#' }
#'
#' @export
#'
qb_extract_selective_lists <- function(XLS_path = NULL) {

    if (is.null(XLS_path) ||
        !is.character(XLS_path) ||
        !file.exists(XLS_path)) {

        stop('Argument "XLS_path" must be a string value specifying the path\nto an excel-file with the selective lists provided by the GBA!')

    }

    # XLS_path <- "./00-data/Auswahllisten/2012_2_Qb_2012_Ueberleitung-Auswahllisten_Korrektur_2013-08-28.xls"
    # XLS_path <- "./00-data/Auswahllisten/2019-04-03_Anlage-4_Qb2018_Auswahllisten.xlsx"
    # XLS_path <- "./00-data/Auswahllisten/2020-04-01_Anlage-4_Qb-R_Auswahllisten_BJ-2019.xlsx"
    # XLS_path <- "./00-data/Auswahllisten/2021-04-07_Qb-R_Anlage-3_Auswahllisten.xlsx"
    # XLS_path <- "./00-data/Auswahllisten/2015_8-5-5_5_2016-06-21_Auswahllisten_BJ_2015.xls"

    XLS_sheets <- excel_sheets(XLS_path)
    XLS_sheets <- XLS_sheets[!str_detect(XLS_sheets, "\\(alt\\)")]
    XLS_sheets_overview <- XLS_sheets[[1]]
    XLS_sheets <- XLS_sheets[-1]

    suppressMessages(tables_list <- read_excel(XLS_path,
                                               col_types = "text",
                                               sheet = XLS_sheets_overview))
    colnames(tables_list)[1] <- "Listennummer"

    Jahr <- str_replace(colnames(tables_list)[str_detect(colnames(tables_list), "Auswahlliste")],
                        "(.*)([Qq][Bb][ ]{1,})(20[012][0-9])(.*)", "\\3")


    for (i in seq_along(XLS_sheets)) {

        suppressMessages(df <- read_excel(XLS_path,
                                          col_types = "text",
                                          XLS_sheets[[i]]))

        # testdf <- xlsx::read.xlsx(XLS_path, sheetIndex = 2, colClasses = "character")
        # testdf <- testdf[3:nrow(testdf), 2, drop = FALSE]
        # sheet = "A-12.5.4"))
        # sheet = "B-X.11.2"))
        # sheet = "B-X.11.1"))
        # sheet = "C-2"))
        # sheet = "B-X.3 und B-X.8"))
        # sheet = "B-X.8 116b"))

        table_name_orig <- colnames(df)[1]
        table_name_orig <- str_replace_all(table_name_orig, "(\\r)?\\n", " ")
        table_name_orig <- trimws(str_replace_all(table_name_orig, "^Auswahlliste", " "))
        table_name_orig <- str_replace_all(table_name_orig, '\u201a|\u201b|\u201c|\u201d|\u201e|\u201f|\\"', '')
        table_name_orig <- str_replace_all(table_name_orig, ' \\(.*$', '')

        table_list_col_with_names <- colnames(tables_list)[str_detect(colnames(tables_list), "Auswahlliste")]
        Listennummer_current <- tables_list[str_detect(unname(unlist(tables_list[, table_list_col_with_names])), table_name_orig), "Listennummer"]

        if (nrow(Listennummer_current) != 1) {

            Listennummer_current <- tables_list[str_detect(unlist(tables_list[, table_list_col_with_names]), XLS_sheets[[i]]), "Listennummer"]

        }


        df <- df %>%
            mutate(Listennummer = as.character(Listennummer_current))

        df <- df %>%
            filter(if_any(everything(), ~ !is.na(.)))

        table_name <- colnames(df)[1]
        table_name <- str_replace_all(table_name, "(\\r)?\\n", " ")
        table_name <- str_replace_all(table_name, '\u201a|\u201b|\u201c|\u201d|\u201e|\u201f|\\"|\\(|\\)|\\[|\\]', '')
        table_name <- str_replace_all(table_name, "-| |\\.", "_")
        table_name <- str_replace_all(table_name, "\u00a7", "Paragraph")
        table_name <- str_replace_all(table_name, "\u2013", "_")
        table_name <- str_replace_all(table_name, "[_]{2,}", "_")
        table_name <- str_replace_all(table_name, "\u00e4", "ae")
        table_name <- str_replace_all(table_name, "\u00df", "ss")
        table_name <- str_replace_all(table_name, "\u00fc", "ue")
        table_name <- str_replace_all(table_name, "\u00f6", "oe")
        table_name <- str_replace_all(table_name, "\u00c4", "Ae")
        table_name <- str_replace_all(table_name, "\u00dc", "Ue")
        table_name <- str_replace_all(table_name, "\u00d6", "Oe")

        var_names_idx <- which(str_detect(unlist(df[,1]), "(Nr\\.)|(Bundesland)"))
        var_names <- unname(unlist(df[var_names_idx[[1]], ]))
        var_names[var_names == as.character(Listennummer_current)] <- "Listennummer"

        if (any(is.na(var_names))) {

            df <- df[, -which(is.na(var_names))]
            var_names <- var_names[!is.na(var_names)]

        }

        var_names <- str_replace_all(var_names, '\u201a|\u201b|\u201c|\u201d|\u201e|\u201f|\\"', '')
        var_names <- str_replace_all(var_names, "(\\r)?\\n", " ")
        var_names <- str_replace(var_names, "Nr\\.", "Nummer")

        var_names <- str_replace(var_names, ":", "")
        var_names <- str_replace(var_names, "=", "bedeutet")
        var_names <- str_replace_all(var_names, "\u00a7", "Paragraph")
        var_names <- str_replace_all(var_names, "\\/| |-", "_")
        var_names <- str_replace_all(var_names, "\u2013", "_")
        var_names <- str_replace_all(var_names, "[_]{2,}", "_")

        var_names <- str_replace_all(var_names, "\\(|\\)|,|\\.", "")

        var_names <- str_replace_all(var_names, "\u00e4", "ae")
        var_names <- str_replace_all(var_names, "\u00df", "ss")
        var_names <- str_replace_all(var_names, "\u00fc", "ue")
        var_names <- str_replace_all(var_names, "\u00f6", "oe")
        var_names <- str_replace_all(var_names, "\u00c4", "Ae")
        var_names <- str_replace_all(var_names, "\u00dc", "Ue")
        var_names <- str_replace_all(var_names, "\u00d6", "Oe")

        if (length(var_names_idx) > 1) {

            colnames(df) <- paste0("V", seq_along(df))

            if (str_detect(var_names[[2]], "Versorgungsschwerpunkt")) {

                var_area <- "Bereich"
                var_names <- c(var_names, var_area)

            } else if (str_detect(var_names[[2]], "Facharztbezeichnung") ||
                       str_detect(var_names[[2]], "Fachweiterbildung")) {

                var_area <- "Art_der_Qualifikation"
                var_names <- c(var_names, var_area)
                var_names[str_detect(var_names, "Facharztbezeichnung|Fachweiterbildung")] <- "Bezeichnung"

            } else if (str_detect(var_names[[2]], "Nummer_der_Anlage")) {

                var_area <- "Behandlungsart"
                var_names <- c(var_names, var_area)
                var_names <- str_replace(var_names, "Nummer_der_Anlage(.*)", "Nummer_der_Anlage")

                df <- df %>%
                    mutate(V2 = str_replace_all(.data$V2, "(\\r)?\\n", " ")) %>%
                    mutate(V2 = str_replace_all(.data$V2, "^Nr\\.(.*)(ambulante Behandlung)(.*)$", "\\2")) %>%
                    mutate(V2 = str_replace_all(
                        .data$V2,
                        "^Nr\\.(.*)(ambulante spezialfach\u00e4rztliche Versorgung)(.*)$",
                        "\\2"
                    ))

            }

            df <- df %>%
                mutate({{var_area}} := case_when(
                    .data$V1 == "Nr." ~ .data$V2,
                    TRUE ~ NA_character_
                )) %>%
                fill(.data[[var_area]], .direction = "down")

            if (any(is.na(df$V1))) {

                var_names <- c(var_names, "Unterbereich")
                df$Unterbereich <- NA_character_

                df <- df %>%
                    mutate(Unterbereich = case_when(
                        is.na(.data$V1) ~ .data$V2,
                        TRUE ~ .data$Unterbereich
                    )) %>%
                    group_by(.data[[var_area]]) %>%
                    fill(.data$Unterbereich, .direction = "down") %>%
                    filter(!is.na(.data$V1)) %>%
                    ungroup()

                if (var_area == "Bereich") {

                    df <- df %>%
                        mutate(Bereich = trimws(
                            str_replace_all(.data$Bereich,
                                            "Versorgungsschwerpunkte im Bereich", "")
                        )) %>%
                        mutate(Bereich = trimws(
                            str_replace_all(
                                .data$Bereich,
                                "Versorgungsschwerpunkte in sonstigen medizinischen Bereichen",
                                "Sonstige medizinische Bereiche"
                            )
                        )) %>%
                        mutate(Bereich = case_when(
                            !is.na(.data$Unterbereich) ~ paste0(.data$Bereich, " - ", .data$Unterbereich),
                            TRUE ~ .data$Bereich
                        )) %>%
                        select(-.data$Unterbereich)

                    var_names <- var_names[!var_names == "Unterbereich"]
                    var_names <- str_replace(var_names, "(Versorgungsschwerpunkt)(e.*)$", "\\1")

                }

                if (any(colnames(df) == "Unterbereich") &&
                    all(is.na(df$Unterbereich))) {

                    df$Unterbereich <- NULL
                    var_names <- var_names[!var_names == "Unterbereich"]

                }

            }

            df <- df %>%
                filter(.data$V1 != "Nr.")

        } else {

            df <- df[(var_names_idx + 1):nrow(df), ]

        }

        colnames(df) <- trimws(var_names)



        listencolumn <- which(colnames(df) == "Listennummer")

        if (any(str_detect(unlist(df[, 1]), "\\(Mehrfachauswahl m\u00f6glich\\)"), na.rm = TRUE) &&
            all(is.na(unlist(df[str_detect(unlist(df[, 1]), "\\(Mehrfachauswahl m\u00f6glich\\)"), c(-1, -listencolumn)])))) {

            df <- df[!str_detect(unlist(df[, 1]), "\\(Mehrfachauswahl m\u00f6glich\\)"), ]

        }
        rm(listencolumn)

        df <- df %>%
            mutate(across(where(is.character), ~ case_when(
                str_detect(.x, ":") ~ str_replace_all(.x, "(\\r)?\\n", " / "),
                !str_detect(.x, ":") ~ str_replace_all(.x, "(\\r)?\\n", " ")
            ))) %>%
            mutate(across(where(is.character), ~ str_replace_all(.x, ': \\/', ': '))) %>%
            mutate(across(where(is.character), ~ str_replace_all(.x, '[ ]{2,}', ' '))) %>%
            mutate(across(where(is.character), ~ str_replace_all(.x, "z. B.", "z.B."))) %>%
            mutate(across(where(is.character), ~ str_replace_all(.x, '\u201e|\u201f', '"'))) %>%
            mutate(across(where(is.character), ~ str_replace_all(.x, '[ ]{1,}\\/[ ]{0,}$', ''))) %>%
            mutate(across(where(is.character), ~ trimws(.x)))


        if (any(colnames(df) == "Bundesland") &&
            any(is.na(df$Bundesland))) {

            df <- df %>%
                fill(.data$Bundesland, .direction = "down")

        }


        if (any(colnames(df) == "Nummer") &&
            any(is.na(df$Nummer))) {

            var_category <- var_names[!str_detect(var_names, "^Nummer$")]
            var_category <- var_category[!str_detect(var_category, "Kommentar")][[1]]


            df$Bereich <- NA_character_
            df <- df %>%
                mutate(Bereich = case_when(
                    is.na(.data$Nummer) ~ .data[[var_category]],
                    TRUE ~ .data$Bereich
                )) %>%
                fill(.data$Bereich, .direction = "down") %>%
                filter(!is.na(.data$Nummer)) %>%
                select(.data$Nummer, .data$Bereich, everything())

        }

        df$Jahr <- Jahr

        df <- df %>%
            select(.data$Jahr, .data$Listennummer, everything())

        df[is.na(df)] <- ""

        assign(paste0(table_name, "_", Jahr), df)
        suppressWarnings(rm(df, table_name_orig, table_name, var_names, var_names_idx,
                            var_area, var_category, Listennummer_current,
                            table_list_col_with_names))

    }

    return_list <- map(ls(pattern = "Auswahlliste"), ~ get(.x, envir = sys.frame(sys.parent(0))))
    names(return_list) <- str_replace_all(ls(pattern = "Auswahlliste"), 'Auswahlliste_(.*?)_[ABC]_.*$', '\\1')

    return_list <- return_list[sort.list(unname(map_dbl(return_list, ~ as.numeric(unique(.x$Listennummer)))))]

    return_list <- c("Auswahlliste_Ueberblick" = list(tables_list), return_list)

    rm(XLS_path, XLS_sheets, tables_list, Jahr, XLS_sheets_overview, i)

    return(return_list)

}
