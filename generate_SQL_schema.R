library(tidyverse)
library(xml2)
library(readxl)
library(htmltidy)
library(qbgbaReader)
library(DBI)
library(RMariaDB)


# keyring::key_set(service = "mysql-localhost",
#                  username = "dataadmin")

### Using the odbc-package with the following syntax gives weird problems regarding
### the encoding of special characters...:
#
# Assumes, that the MySQL-Driver is installed:
#     https://dev.mysql.com/downloads/connector/odbc/
#
# Or, alternatively, the MariaDB connector can be used:
#     https://mariadb.com/downloads/#connectors
#
# con <- dbConnect(odbc::odbc(),
#                  Driver   = "MariaDB ODBC 3.1 Driver",
#                  Server   = "localhost",
#                  Port     = 3306,
#                  UID      = keyring::key_list("mysql-localhost")[2,2],
#                  PWD      = keyring::key_get("mysql-localhost", "gbadatauser"))
#
#
###
### Therefore, the RMariaDB-package is used for the connection instead:
###
#
con <- dbConnect(RMariaDB::MariaDB(),
                 host     = "localhost",
                 port     = 3306,
                 username = keyring::key_list("mysql-localhost")[1,2],
                 password = keyring::key_get("mysql-localhost", "dataadmin")
)

rs <- dbSendQuery(con, "DROP DATABASE IF EXISTS `gbadata`;")
dbClearResult(rs)

schema_commands <- read_lines("./data-raw/mysql/schema.sql", lazy = FALSE)
schema_commands <- schema_commands[!str_detect(schema_commands, "^-- ")]
schema_commands <- str_replace_all(paste0(schema_commands, collapse = " "), "[ ]{2,}", " ")
schema_commands <- trimws(str_split(schema_commands, ";")[[1]])
schema_commands <- schema_commands[!str_detect(schema_commands, "^$")]
schema_commands <- paste0(schema_commands, ";")

for (i in seq_along(schema_commands)) {

    rs <- dbSendQuery(con, schema_commands[[i]])
    dbClearResult(rs)

}
dbDisconnect(con)
rm(con, i, schema_commands, rs)
