#' Set (i.e., insert or update) one or several database entries in one table.
#'
#' @param conn A RMariaDB-connection object (based on the DBI-package) to the
#'     database where the GBA data is stored.
#'
#' @param dat_table A \code{data.frame} or \code{tibble} with the data to be
#'     inserted into or updated in the database. It must contain the variable which
#'     is the primary key of the corresponding table in the database. For entries of
#'     this primary key which are \code{NA}, it is assumed that the data in question
#'     is not yet in the database and an \code{INSERT} is applied, if an entry is
#'     an integer value, it is assumed that the data is already in the database and
#'     an \code{UPDATE} is applied.
#'
#' @param table_name A single string value with the name of the corresonding table
#'     in the database. Defaults to \code{NA_character_} and therefore, it must be
#'     provided.
#'
#' @param db_name The name of the database as a single string value. This is used
#'     to confirm that one is connected to the desired database.
#'
#' @param return_nn_cols A single boolean value which indicates if any variable that
#'     is not allowed to be \code{NULL} in the database (e.g., any foreign key) that
#'     might be present in the database but not in the provided \code{dat_table}
#'     object should be returned, too. For example, this means that any foreign key
#'     present in the database but not in the \code{dat_table} object is likely not
#'     set in the database, it will be initialized as the value \code{0} (i.e.,
#'     the number "zero"). Any column that is defined as to be of type "character"
#'     will be initialized as an empty string (i.e., ""). Defaults to \code{TRUE}.
#'
#' @param return_ids_only A single boolean value which specifies if the complete
#'     \code{dat_table} object should be returned (in case of an \code{INSERT} with
#'     the new primary key values inserted for the original \code{NA_integer} values),
#'     or if only the column with the primary key should be returned as a \code{tibble},
#'     and, if \code{return_foreign_keys} is also set to \code{TRUE}, the foreign
#'     keys that might be present in the specified table. Defaults to \code{FALSE}.
#'
#' @param return_orig_cols A single boolean value which indicates if columns that
#'     were not present in the database but in the \code{dat_table} provided as input
#'     should be also returned as-is, i.e., unchanged. Defaults to \code{FALSE}.
#'
#' @param bulk_insert A single boolean value indicating if this function should
#'     just delete all existing records matching the provided foreign keys,
#'     re-insert the data into the database and should not attempt to search for any
#'     existing entry for each provided entry in \code{dat_table}. This option is
#'     only applicable if all primary key values of the provided \code{dat_table}
#'     object (the primary key name is fetched from the database) are missing /
#'     unknown (i.e., \code{NA_integer_}). If any value for the primary key is
#'     given (i.e., not \code{NA_integer_}), this option is silently switched to
#'     \code{FALSE} to ensure that an update of existing records is possible!
#'
#'     If \code{bulk_insert} is set to \code{TRUE} (and all primary key values in
#'     the \code{dat_table} are missing), this function only checks if there are
#'     any foreign keys in the specified \code{table_name} that are not present
#'     in the provided \code{dat_table}. If not, all existing entries in the database
#'     belonging to the provided foreign keys are deleted and all entries from the
#'     \code{dat_table} are (re-) inserted into the database. This also means, that
#'     the values of the primary key for the given \code{dat_table} object are \emph{not}
#'     filled but returned as \code{NA_integer_} values because one cannot assure
#'     that the correct values for this variable are returned. It also means that
#'     there were any previous primary key values in the database regarding the
#'     present entries, they changed due to this "delete and insert again"-
#'     operation.
#'
#'     \strong{Do not use this option when dealing with tables that have child tables
#'     themself!}
#'
#'     If there are any foreign keys not present in the \code{dat_table}, option
#'     \code{bulk_insert} is silently switched to \code{FALSE} again.
#'
#'     This option should only be used in tables that are not linked to another
#'     child table to avoid using incorrect foreign key values. But its use should
#'     give a significant speedup in comparison to a 1-by-1 insert, because it disables
#'     the check for existing records. Defaults to {FALSE}.
#'
#'
#' @return A \code{tibble}, either the original or an updated version of the provided
#'     \code{dat_table} object, or a \code{tibble} with one column which is the primary
#'     key of the corresponding table in the database (and additionally, a number
#'     of columns representing the possible foreign keys).
#'
#'
#' @examples
#'
#' \dontrun{
#' Hospital <- tibble("idHospital" = NA_integer_,
#'                    "xmlFileName" = "260100023-01-2019",
#'                    "description" = "Diakonissenkrankenhaus Flensburg",
#'                    "alias" = NA_character_,
#'                    "ikNumber" = "260100023",
#'                    "locationNumber" = "99",
#'                    "academicTeachingHospital" = 1L,
#'                    "psychiatricHospital" = 1L,
#'                    "urlHospital" = "http://www.diako.de",
#'                    "urlAdditionalInformation" = NA_character_,
#'                    "HospitalDataYear_idHospitalDataYear" = 5L)
#'
#' con <- dbConnect(RMariaDB::MariaDB(),
#'                  host     = "localhost",
#'                  port     = 3306,
#'                  username = keyring::key_list("mysql-localhost")[1,2],
#'                  password = keyring::key_get("mysql-localhost", "dataadmin"),
#'                  dbname   = "gbadata")
#'
#' Hospital_updated <- qb_db_set_query(con,
#'                                     Hospital,
#'                                     table_name = "Hospital")
#'
#' }
#'
#' @export
#'
qb_db_set_query <- function (conn,
                             dat_table,
                             table_name = NA_character_,
                             db_name = "gbadata",
                             return_nn_cols = TRUE,
                             return_ids_only = FALSE,
                             return_orig_cols = FALSE,
                             bulk_insert = FALSE) {

    # conn <- con
    # db_name = "gbadata"
    # return_nn_cols = TRUE
    # return_ids_only = FALSE
    # return_orig_cols = TRUE
    # bulk_insert = TRUE
    # dat_table <- Diagnoses
    # table_name = "Diagnoses"

    ### Tests

    if (dbGetInfo(conn)$dbname != db_name) {

        stop('The specified database name ("', db_name, '") is not equal to the one in the provided connection object.')

    }

    tables_available <- dbListTables(conn)

    if (is_empty(table_name)) {

        stop('Parameter "table_name" must not be empty.')

    } else if (!is.character(table_name)) {

        stop('Parameter "table_name" must be a string.')

    } else if (is.na(table_name) || length(table_name) != 1) {

        stop('Parameter "table_name" must be a single value and cannot be missing.')

    } else if (!tolower(table_name) %in% tables_available) {

        stop('Parameter "table_name" was set to "', tolower(table_name),
             '"\nwhich is not a table available in database "', db_name, '".')

    } else if (!dbExistsTable(conn, table_name)) {

        stop('The specified table ("', tolower(table_name),
             '") does not exist in database "', db_name, '".')

    }


    if (is_empty(dat_table)) {

       stop('Parameter "dat_table" must not be empty.')

    } else if (!is.data.frame(dat_table)) {

        stop('Parameter "dat_table" must be a data frame or a tibble.')


    ### End of tests

    } else {

        table_fields <- dbListFields(conn, table_name)

        primary_key <- dbGetQuery(conn, paste0("SHOW KEYS FROM `", db_name,
                                              "`.`", table_name,
                                              "` WHERE Key_name = 'PRIMARY';"))[["Column_name"]]

        if (all(!table_fields %in% colnames(dat_table))) {

            stop('None of the variables in dat_table is present\nin the specified table ("', table_name, '") in the database.')

        }

        if (!primary_key %in% colnames(dat_table)) {

            dat_table[primary_key] <- NA_integer_

            dat_table <- dat_table %>%
                select(.data[[primary_key]], everything())

        }

        if (all(!is.na(unname(unlist(dat_table[,primary_key]))))) {

            bulk_insert <- FALSE

        }


        # primary_key_local <- paste0("id", table_name)
        #
        # if (primary_key != primary_key_local) {
        #
        #     stop('The primary key ("',
        #          primary_key, '") of the\nspecified table ("',
        #          table_name, '") is not equal\nto "',
        #          primary_key_local, '".\nPlease verify the correct table.')
        #
        # }


        foreign_keys <- dbGetQuery(conn, paste0("SHOW KEYS FROM `", db_name,
                                                "`.`", table_name,
                                                "` WHERE Key_name != 'PRIMARY';"))[["Column_name"]]

        table_col_info <- dbGetQuery(conn, paste0("SHOW COLUMNS FROM `", db_name,
                                                  "`.`", table_name,
                                                  "`;"))

        nn_cols <- table_col_info %>%
            tibble() %>%
            filter(.data$Null == "NO" & !.data$Extra %in% c("auto_increment", "DEFAULT_GENERATED"))

        nn_cols_new <- nn_cols[!nn_cols$Field %in% colnames(dat_table), , drop = FALSE]

        if (nrow(nn_cols_new) > 0 ) {

            bulk_insert <- FALSE

            nn_cols_new <- nn_cols_new %>%
                mutate(Default = case_when(
                    is.na(.data$Default) & Type == "int" ~ "0",
                    is.na(.data$Default) & str_detect(.data$Type, "^varchar") ~ "",
                    TRUE ~ NA_character_
                ))

            dat_table[unname(unlist(nn_cols_new[nn_cols_new$Default == "0", "Field"]))] <- 0L
            dat_table[unname(unlist(nn_cols_new[nn_cols_new$Default == "", "Field"]))] <- ""

        }

        # dbGetQuery(con, paste0("SELECT * FROM ",
        #        "INFORMATION_SCHEMA.KEY_COLUMN_USAGE WHERE ",
        #        "REFERENCED_TABLE_NAME = 'Hospital';"))
        #
        # dbGetQuery(con, paste0("SELECT TABLE_NAME,COLUMN_NAME,CONSTRAINT_NAME, ",
        #                        "REFERENCED_TABLE_NAME,REFERENCED_COLUMN_NAME FROM ",
        #                        "INFORMATION_SCHEMA.KEY_COLUMN_USAGE WHERE ",
        #                        "REFERENCED_TABLE_SCHEMA = 'gbadata' AND ",
        #                        "REFERENCED_TABLE_NAME = 'Hospital';"))

        missing_columns <- dat_table[!names(dat_table) %in% table_fields]

        dat_table_sql <- dat_table[names(dat_table) %in% table_fields]


        if (bulk_insert) {

            FK_table <- dat_table_sql[, colnames(dat_table_sql) %in% foreign_keys, drop = FALSE]

            FK_table <- distinct(FK_table)

            if (nrow(FK_table) > 0) {

                FK_table_strings <- vector(mode = "character", length(nrow(FK_table)))

                for (i in seq_len(nrow(FK_table))) {

                    FK_table_strings[[i]] <- paste0('(',
                                                    paste0(paste0('`', colnames(FK_table[i, ]),
                                                                  '` = ?'),
                                                           collapse = " AND "),
                                                    ')')

                }

                data_del <- dbSendStatement(conn, paste0("DELETE",
                                                         " FROM `", db_name, '`.`',table_name, "`",
                                                         " WHERE ", paste0(FK_table_strings, collapse = " OR "), ";"))

                data_del <- dbBind(data_del,
                                   unname(unlist(FK_table)))
                dbClearResult(data_del)

            } else {

                dat_table_sql_no_pk <- dat_table_sql[, !names(dat_table_sql) %in% primary_key, drop = FALSE]

                ColumnList <- paste0(paste0("`", names(dat_table_sql_no_pk), "`"), collapse = ", ")

                MySQLQs <- paste0(rep("?", times = length(dat_table_sql_no_pk)), collapse = ", ")

                ValueList <- unlist(dat_table_sql_no_pk)

                data_in <- dbSendStatement(conn,
                                           paste0("INSERT INTO `",
                                                  db_name, '`.`',
                                                  table_name, "` (",
                                                  ColumnList, ") VALUES (",
                                                  MySQLQs, ");"))

                data_in_string <- paste0('data_in <- dbBind(data_in, params = list(',
                                         paste0(paste0('dat_table_sql_no_pk_complete$',
                                                       colnames(dat_table_sql_no_pk_complete)),
                                                collapse = ", "),
                                         '))')
                eval(parse(text = data_in_string, encoding = "UTF-8"))

                dbClearResult(data_in)

            }

            dat_table_sql_no_pk_complete <- dat_table_sql[, !names(dat_table_sql) %in% primary_key, drop = FALSE]

            ColumnList <- paste0(paste0("`", names(dat_table_sql_no_pk_complete), "`"), collapse = ", ")

            MySQLQs <- paste0(rep("?", times = length(dat_table_sql_no_pk_complete)), collapse = ", ")

            ValueList <- unlist(dat_table_sql_no_pk_complete)

            data_in <- dbSendStatement(conn,
                                       paste0("INSERT INTO `",
                                              db_name, '`.`',
                                              table_name, "` (",
                                              ColumnList, ") VALUES (",
                                              MySQLQs, ");"))

            data_in_string <- paste0('data_in <- dbBind(data_in, params = list(',
                                     paste0(paste0('dat_table_sql_no_pk_complete$',
                                                   colnames(dat_table_sql_no_pk_complete)),
                                            collapse = ", "),
                                     '))')
            eval(parse(text = data_in_string, encoding = "UTF-8"))

            dbClearResult(data_in)


        } else {

            for (i in seq_len(nrow(dat_table_sql))){

                primary_key_value <- unname(unlist(dat_table_sql[i, names(dat_table_sql) %in% primary_key, drop = FALSE]))

                dat_table_sql_no_pk <- dat_table_sql[i, !names(dat_table_sql) %in% primary_key, drop = FALSE]

                ColumnList <- paste0(paste0("`", names(dat_table_sql_no_pk), "`"), collapse = ", ")

                MySQLQs <- paste0(rep("?", times = length(dat_table_sql_no_pk)), collapse = ", ")

                ValueList <- unlist(dat_table_sql_no_pk)

                if (is.na(primary_key_value)) {

                    exisiting_primary_key_value <- dbGetQuery(conn,
                                                              paste0("SELECT ", primary_key,
                                                                     " FROM `", db_name, '`.`',table_name, "`",
                                                                     " WHERE ",
                                                                     paste0(map2_chr(names(dat_table_sql_no_pk),
                                                                                     dat_table_sql_no_pk,
                                                                                     equal_or_null_condition),
                                                                            collapse = " AND "), ";"),
                                                              params = unname(ValueList[!is.na(ValueList)]))

                    if (nrow(exisiting_primary_key_value) == 1) {

                        primary_key_value <- unname(unlist(exisiting_primary_key_value))

                        dat_table_sql[i, names(dat_table_sql) %in% primary_key, drop = FALSE] <- unname(unlist(exisiting_primary_key_value))

                        next()

                    } else if (nrow(exisiting_primary_key_value) > 1) {

                        stop('Duplicated entries seem to be present in the table "',
                             table_name, '" with the primary key ',
                             paste0(exisiting_primary_key_value, collapse = " and "),
                             '\n', paste0(ValueList, collapse = " "))

                    }

                }


                if (is.na(primary_key_value)) {

                    data_in <- dbSendStatement(conn,
                                               paste0("INSERT INTO `",
                                                      db_name, '`.`',
                                                      table_name, "` (",
                                                      ColumnList, ") VALUES (",
                                                      MySQLQs, ");"))

                    data_in <- dbBind(data_in, unname(ValueList))
                    dbClearResult(data_in)

                    dat_table_sql[i, names(dat_table_sql) %in% primary_key, drop = FALSE] <- as.integer(dbGetQuery(conn, "SELECT LAST_INSERT_ID();")[[1]])


                } else {


                    data_in <- dbSendStatement(conn,
                                               paste0("UPDATE `",db_name,"`.`",table_name,"` SET ",
                                                      paste0(paste0("`", names(ValueList), "` = ?"), collapse = ", "),
                                                      " WHERE ", primary_key," = ?;"))

                    data_in <- dbBind(data_in, c(unname(ValueList), primary_key_value))
                    dbClearResult(data_in)

                }

            }

        }


        df_tmp <- bind_cols(dat_table_sql, missing_columns)

        if (return_ids_only && return_nn_cols) {

            if (return_orig_cols) {

                warning('"return_orig_cols" cannot be TRUE, if "return_ids_only" and\n"return_nn_cols" are also TRUE. "return_orig_cols" is ignored.')

            }

            return(tibble(df_tmp[, c(primary_key, unname(unlist(nn_cols_new$Field))), drop = FALSE]))

        } else if (return_ids_only && !return_nn_cols) {

            return(tibble(df_tmp[, primary_key, drop = FALSE]))

        } else if (!return_ids_only && !return_nn_cols) {


            dat_table_sql_new <- df_tmp %>%
                tibble() %>%
                select(-unname(unlist(nn_cols_new$Field)))

            if (return_orig_cols) {

                dat_table_sql_new <- dat_table_sql_new %>%
                    select(-colnames(missing_columns))

            }

            return(dat_table_sql_new)

        } else {

            return(tibble(df_tmp))

        }

    }

}
