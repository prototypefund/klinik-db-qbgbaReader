#' Generate the correct table from selective lists to be inserted into the database.
#'
#' @param table_name A single string value with the name of the list.
#'
#' @param primary_key_var A single string value that specifies the name of the
#'     primary key of the database table.
#'
#' @param variable_lookup_table A named list with single character values as
#'     elements that specify which original variable names in the selective lists
#'     (which are the list elements) map to which variable names in the database
#'     (which are the names of the list elements).
#'
#' @param search_env_for_lists The environment which is used to search for the
#'     selective list objects, e.g., \code{lists_2015}. Defaults is the \code{.GlobalEnv}.
#'
#'
#' @return A \code{tibble} with as many columns as the selective list has variables,
#'     and the primary key as specified in \code{primary_key_var} and the foreign
#'     key variable \code{HospitalDataYear_idHospitalDataYear}. The variables
#'     specified in \code{variable_lookup_table} are renamed accordingly.
#'
#' @details The selective list objects as obtained via calls to
#'     \code{\link{qb_extract_selective_lists}} must be present, either in the
#'     global environment (the default value for the parameter
#'     \code{search_env_for_lists}) or in another environment,
#'     and must be named following the pattern \code{lists_2015} or
#'     \code{lists_2016} (\code{lists_20xx}).
#'
#'
#' @examples
#' \dontrun{
#' path2015 <- "./00-data/Auswahllisten/2015_8-5-5_5_2016-06-21_Auswahllisten_BJ_2015.xls"
#' lists_2015 <- qb_extract_selective_lists(path2015)
#' path2016 <- "./00-data/Auswahllisten/2017-06-14_Anlage_Auswahllisten_Anhang-2_Anl-1_2016.xls"
#' lists_2016 <- qb_extract_selective_lists(path2016)
#'
#' df <- lists_insert_single_table(table_name = "Medizinisch_pflegerische_Leistungsangebote",
#'                                 primary_key_var = "idqb_list_nursingservices",
#'                                 variable_lookup_table = list("code" = "Nummer",
#'                                    "description" = "Medizinisch_pflegerisches_Leistungsangebot",
#'                                    "comment" = "Kommentar_Erlaeuterung"))
#' }
#'
#' @export
#'
lists_insert_single_table <- function(table_name,
                                      primary_key_var,
                                      variable_lookup_table,
                                      search_env_for_lists = .GlobalEnv) {

    ### Prerequisites

    if (!is.character(table_name) || length(table_name) != 1) {

        stop('"table_name" must be a single string value!')

    }

    if (!is.character(primary_key_var) || length(primary_key_var) != 1) {

        stop('"primary_key_var" must be a single string value!')

    }

    if (!is.list(variable_lookup_table)) {

        stop('"variable_lookup_table" must be a list!')

    }

    if (is.null(names(variable_lookup_table))) {

        stop('"variable_lookup_table" must be a named list!')

    }

    if (!all(unlist(map(variable_lookup_table, is.character)))) {

        stop('All elements of "variable_lookup_table" must be string values!')

    }

    if (!all(unname(unlist(map(variable_lookup_table, length))) == 1)) {

        stop('All elements of "variable_lookup_table" must be of length one!')

    }


    list_names <- ls(pattern = "^lists_201[5-9]{1}$", envir = search_env_for_lists)

    if (length(list_names) < 1) {

        stop('There must be at least one selective list named as\n"lists_20xx" present in the specified environment\n(defaults to the global environment).')
    }

    Jahre_lookuptable <- sort(as.integer(unlist(str_extract_all(list_names, "20[12][0-9]$"))))
    names(Jahre_lookuptable) <- seq_along(Jahre_lookuptable)


    ### Start of actual function

    for (i in seq_along(list_names)) {

        current_list <- get(list_names[[i]], envir = search_env_for_lists)

        df_tmp <- current_list[[names(current_list)[str_detect(names(current_list), table_name)]]]

        variable_lookup_table_current <- variable_lookup_table[variable_lookup_table %in% names(df_tmp)]

        variable_lookup_table_string <- vector(mode = "list", length = length(variable_lookup_table_current))
        for (j in seq_along(variable_lookup_table_current)) {

            variable_lookup_table_string[[j]] <- paste0('"', names(variable_lookup_table_current)[[j]], '" = ',
                                                        variable_lookup_table_current[[j]])

        }
        rm(j, variable_lookup_table_current)
        variable_lookup_table_string <- paste0("df_tmp <- rename(df_tmp, ", paste0(variable_lookup_table_string, collapse = ", "), ")")


        eval(parse(text = variable_lookup_table_string, encoding = "UTF-8"))

        df_tmp <- df_tmp %>%
            mutate(HospitalDataYear_idHospitalDataYear = as.integer(names(Jahre_lookuptable[which(Jahre_lookuptable == unique(df_tmp$Jahr))])))

        if (i == 1) {

            df_total <- df_tmp

        } else {

            df_total <- bind_rows(df_total, df_tmp)

        }

        rm(df_tmp, current_list)

    }
    rm(i, list_names)

    df_total[is.na(df_total)] <- ""

    df_total <- df_total %>%
        mutate({{primary_key_var}} := NA_integer_)


    return(df_total)

}

