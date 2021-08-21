library(tidyverse)
library(xml2)
library(readxl)
library(htmltidy)
library(qbgbaReader)
library(DBI)
# library(odbc)
library(RMariaDB)


# keyring::key_set(service = "mysql-localhost",
#                  username = "gbadatauser")

### Auswahllisten

lists_2015 <- qb_extract_selective_lists("./00-data/Auswahllisten/2015_8-5-5_5_2016-06-21_Auswahllisten_BJ_2015.xls")
lists_2016 <- qb_extract_selective_lists("./00-data/Auswahllisten/2017-06-14_Anlage_Auswahllisten_Anhang-2_Anl-1_2016.xls")
lists_2017 <- qb_extract_selective_lists("./00-data/Auswahllisten/2018-04-04_Anlage-3_Qb2017_Auswahllisten.xlsx")
lists_2018 <- qb_extract_selective_lists("./00-data/Auswahllisten/2019-04-03_Anlage-4_Qb2018_Auswahllisten.xlsx")
lists_2019 <- qb_extract_selective_lists("./00-data/Auswahllisten/2020-04-01_Anlage-4_Qb-R_Auswahllisten_BJ-2019.xlsx")


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
                 password = keyring::key_get("mysql-localhost", "dataadmin"),
                 dbname   = "gbadata"
)


dbListTables(con)
dbGetInfo(con)



# Main table with the available years - HospitalDataYear ------------------

### The only table where the primary key is not defined as AUTO INCREMENT!!!

list_names <- ls(pattern = "^lists_201[5-9]{1}$")
Jahre_lookup <- sort(as.integer(unlist(str_extract_all(list_names, "20[12][0-9]$"))))
names(Jahre_lookup) <- seq_along(Jahre_lookup)
rm(list_names)

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`HospitalDataYear` ",
                                  "(`year`) VALUES ",
                                  "(?);"))
dbBind(data_in, list(Jahre_lookup))

dbClearResult(data_in)
# dbGetQuery(con, "SELECT LAST_INSERT_ID();")



# INSERT list of departments ----------------------------------------------

# This list is the same for all years as it is only expanded not changed
# during the years...this means it has no other foreign keys or any natural
# connections at the moment.

df <- Fachabteilungen_2019 %>%
    rename("code" = Fachabteilungsschluessel,
           "specialized" = Schwerpunkt_ja_nein,
           "description" = Bezeichnung)

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`DepartmentTypes` ",
                                  "(`code`, `description`, `specialized`) ",
                                  "VALUES (?, ?, ?);"))

dbBind(data_in, params = list(
    df$code,
    df$description,
    df$specialized))
dbClearResult(data_in)
rm(df, data_in)


# qb_list_nursingservices -------------------------------------------------

# dbListFields(con, "qb_list_nursingservices")

df <- lists_insert_single_table(table_name = "Medizinisch_pflegerische_Leistungsangebote",
                                primary_key_var = "idqb_list_nursingservices",
                                variable_lookup_table = list("code" = "Nummer",
                                                             "description" = "Medizinisch_pflegerisches_Leistungsangebot",
                                                             "comment" = "Kommentar_Erlaeuterung"))

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`qb_list_nursingservices` ",
                                  "(`code`, `description`, ",
                                  "`comment`, `HospitalDataYear_idHospitalDataYear`) ",
                                  "VALUES (?, ?, ?, ?);"))

dbBind(data_in, params = list(
    df$code,
    df$description,
    df$comment,
    df$HospitalDataYear_idHospitalDataYear))
dbClearResult(data_in)
rm(df, data_in)

# dbGetQuery(con, "SELECT * FROM `gbadata`.`qb_list_nursingservices`;")



# qb_list_nonmedicalservices ----------------------------------------------

# dbListFields(con, "qb_list_nonmedicalservices")
# dbListFields(con, "qb_list_nonmedserv_category")

df <- lists_insert_single_table(table_name = "Weitere_nicht_medizinische_Leistungsangebote",
                                primary_key_var = "idqb_list_nonmedicalservices",
                                variable_lookup_table = list("code" = "Nummer",
                                                             "area" = "Bereich",
                                                             "description" = "Leistungsangebot",
                                                             "comment" = "Kommentar_Erlaeuterung",
                                                             "additionalInformation" = "Zusatzangaben_verpflichtend"))

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`qb_list_nonmedicalservices` ",
                                  "(`code`, `description`, ",
                                  "`additionalInformation`, `area`, `comment`, ",
                                  "`HospitalDataYear_idHospitalDataYear`) ",
                                  "VALUES (?, ?, ?, ?, ?, ?);"))

dbBind(data_in, params = list(
    df$code,
    df$description,
    df$additionalInformation,
    df$area,
    df$comment,
    df$HospitalDataYear_idHospitalDataYear))
dbClearResult(data_in)
rm(df, data_in)

# dbGetQuery(con, "SELECT * FROM `gbadata`.`qb_list_nonmedicalservices`;")



# qb_list_accessibility ---------------------------------------------------

# dbListFields(con, "qb_list_accessibility")

df <- lists_insert_single_table(table_name = "Aspekte_der_Barrierefreiheit",
                                primary_key_var = "idqb_list_accessibility",
                                variable_lookup_table = list("code" = "Nummer",
                                                             "area" = "Bereich",
                                                             "description" = "Aspekt_der_Barrierefreiheit",
                                                             "comment" = "Kommentar_Erlaeuterung"))

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`qb_list_accessibility` ",
                                  "(`code`, `description`, ",
                                  "`comment`, `area`, `HospitalDataYear_idHospitalDataYear`) ",
                                  "VALUES (?, ?, ?, ?, ?);"))

dbBind(data_in, params = list(
    df$code,
    df$description,
    df$comment,
    df$area,
    df$HospitalDataYear_idHospitalDataYear))
dbClearResult(data_in)
rm(df, data_in)

# dbGetQuery(con, "SELECT * FROM `gbadata`.`qb_list_accessibility`;")



# qb_list_academicteaching ------------------------------------------------

# dbListFields(con, "qb_list_academicteaching")

df <- lists_insert_single_table(table_name = "Forschung_und_akademische_Lehre",
                                primary_key_var = "idqb_list_academicteaching",
                                variable_lookup_table = list("code" = "Nummer",
                                                             "description" = "Forschung_akademische_Lehre_und_weitere_ausgewaehlte_wissenschaftliche_Taetigkeiten",
                                                             "comment" = "Kommentar_Erlaeuterung"))

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`qb_list_academicteaching` ",
                                  "(`code`, `description`, ",
                                  "`comment`, `HospitalDataYear_idHospitalDataYear`) ",
                                  "VALUES (?, ?, ?, ?);"))

dbBind(data_in, params = list(
    df$code,
    df$description,
    df$comment,
    df$HospitalDataYear_idHospitalDataYear))
dbClearResult(data_in)
rm(df, data_in)

# dbGetQuery(con, "SELECT * FROM `gbadata`.`qb_list_academicteaching`;")



# qb_list_trainingotherhealthprofessions ----------------------------------

# dbListFields(con, "qb_list_trainingotherhealthprofessions")

df <- lists_insert_single_table(table_name = "Ausbildung_in_anderen_Heilberufen",
                                primary_key_var = "idqb_list_trainingotherhealthprofessions",
                                variable_lookup_table = list("code" = "Nummer",
                                                             "description" = "Ausbildung_in_anderen_Heilberufen",
                                                             "comment" = "Kommentar_Erlaeuterung"))

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`qb_list_trainingotherhealthprofessions` ",
                                  "(`code`, `description`, ",
                                  "`comment`, `HospitalDataYear_idHospitalDataYear`) ",
                                  "VALUES (?, ?, ?, ?);"))

dbBind(data_in, params = list(
    df$code,
    df$description,
    df$comment,
    df$HospitalDataYear_idHospitalDataYear))
dbClearResult(data_in)
rm(df, data_in)

# dbGetQuery(con, "SELECT * FROM `gbadata`.`qb_list_trainingotherhealthprofessions`;")



# qb_list_therapeuticstaff ------------------------------------------------

# dbListFields(con, "qb_list_therapeuticstaff")

df <- lists_insert_single_table(table_name = "Spezielles_therapeutisches_Personal",
                                primary_key_var = "idqb_list_therapeuticstaff",
                                variable_lookup_table = list("code" = "Nummer",
                                                             "description" = "Spezielles_therapeutisches_Personal",
                                                             "comment" = "Kommentar_Erlaeuterung"))

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`qb_list_therapeuticstaff` ",
                                  "(`code`, `description`, ",
                                  "`comment`, `HospitalDataYear_idHospitalDataYear`) ",
                                  "VALUES (?, ?, ?, ?);"))

dbBind(data_in, params = list(
    df$code,
    df$description,
    df$comment,
    df$HospitalDataYear_idHospitalDataYear))
dbClearResult(data_in)
rm(df, data_in)

# dbGetQuery(con, "SELECT * FROM `gbadata`.`qb_list_therapeuticstaff`;")



# qb_list_clinicalriskmanagement ------------------------------------------

# dbListFields(con, "qb_list_clinicalriskmanagement")

df <- lists_insert_single_table(table_name = "Klinisches_Risikomanagement_Instrumente_und_Massnahmen",
                                primary_key_var = "idqb_list_clinicalriskmanagement",
                                variable_lookup_table = list("code" = "Nummer",
                                                             "description" = "Instrument_bzw_Massnahme",
                                                             "comment" = "Zusatzangaben"))

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`qb_list_clinicalriskmanagement` ",
                                  "(`code`, `description`, ",
                                  "`comment`, `HospitalDataYear_idHospitalDataYear`) ",
                                  "VALUES (?, ?, ?, ?);"))

dbBind(data_in, params = list(
    df$code,
    df$description,
    df$comment,
    df$HospitalDataYear_idHospitalDataYear))
dbClearResult(data_in)
rm(df, data_in)

# dbGetQuery(con, "SELECT * FROM `gbadata`.`qb_list_clinicalriskmanagement`;")



# qb_list_internalcomsystem -----------------------------------------------

# dbListFields(con, "qb_list_internalcomsystem")

df <- lists_insert_single_table(table_name = "Einsatz_eines_einrichtungsinternen_Fehlermeldesystems",
                                primary_key_var = "idqb_list_internalcomsystem",
                                variable_lookup_table = list("code" = "Nummer",
                                                             "description" = "Instrument_bzw_Massnahme",
                                                             "comment" = "Zusatzangaben"))

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`qb_list_internalcomsystem` ",
                                  "(`code`, `description`, ",
                                  "`comment`, `HospitalDataYear_idHospitalDataYear`) ",
                                  "VALUES (?, ?, ?, ?);"))

dbBind(data_in, params = list(
    df$code,
    df$description,
    df$comment,
    df$HospitalDataYear_idHospitalDataYear))
dbClearResult(data_in)
rm(df, data_in)

# dbGetQuery(con, "SELECT * FROM `gbadata`.`qb_list_clinicalriskmanagement`;")


# qb_list_externalcomsystem -----------------------------------------------

# dbListFields(con, "qb_list_externalcomsystem")

df <- lists_insert_single_table(table_name = "Teilnahme_an_einrichtungsuebergreifenden_Fehlermeldesystemen",
                                primary_key_var = "idqb_list_externalcomsystem",
                                variable_lookup_table = list("code" = "Nummer",
                                                             "description" = "Instrument_bzw_Massnahme"))

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`qb_list_externalcomsystem` ",
                                  "(`code`, `description`, ",
                                  "`HospitalDataYear_idHospitalDataYear`) ",
                                  "VALUES (?, ?, ?);"))

dbBind(data_in, params = list(
    df$code,
    df$description,
    df$HospitalDataYear_idHospitalDataYear))
dbClearResult(data_in)
rm(df, data_in)

# dbGetQuery(con, "SELECT * FROM `gbadata`.`qb_list_externalcomsystem`;")


# qb_list_hygieneriskmanagement -----------------------------------------------

# dbListFields(con, "qb_list_hygieneriskmanagement")

df <- lists_insert_single_table(table_name = "Hygienebezogenes_Risikomanagement",
                                primary_key_var = "idqb_list_hygieneriskmanagement",
                                variable_lookup_table = list("code" = "Nummer",
                                                             "description" = "Instrument_bzw_Massnahme",
                                                             "comment" = "Zusatzangaben"))

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`qb_list_hygieneriskmanagement` ",
                                  "(`code`, `description`, ",
                                  "`comment`, `HospitalDataYear_idHospitalDataYear`) ",
                                  "VALUES (?, ?, ?, ?);"))

dbBind(data_in, params = list(
    df$code,
    df$description,
    df$comment,
    df$HospitalDataYear_idHospitalDataYear))
dbClearResult(data_in)
rm(df, data_in)

# dbGetQuery(con, "SELECT * FROM `gbadata`.`qb_list_hygieneriskmanagement`;")



# qb_list_medicaldevices --------------------------------------------------

# dbListFields(con, "qb_list_medicaldevices")

df <- lists_insert_single_table(table_name = "Besondere_apparative_Ausstattung",
                                primary_key_var = "idqb_list_medicaldevices",
                                variable_lookup_table = list("code" = "Nummer",
                                                             "device" = "Vorhandene_Geraete_X_bedeutet_Zusatzangabe_verpflichtend",
                                                             "description" = "Umgangssprachliche_Bezeichnung",
                                                             "24hEmergencyPresence" = "Zusatzangabe_24h_Notfallverfuegbarkeit",
                                                             "24hEmergencyPresence" = "Zusatzangabe_24_Stunden_Notfallverfuegbarkeit",
                                                             "comment" = "Kommentar_Erlaeuterung"))

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`qb_list_medicaldevices` ",
                                  "(`code`, `device`, ",
                                  "`description`, `24hEmergencyPresence`, ",
                                  "`comment`, `HospitalDataYear_idHospitalDataYear`) ",
                                  "VALUES (?, ?, ?, ?, ?, ?);"))

dbBind(data_in, params = list(
    df$code,
    df$device,
    df$description,
    df$`24hEmergencyPresence`,
    df$comment,
    df$HospitalDataYear_idHospitalDataYear))
dbClearResult(data_in)
rm(df, data_in)

# dbGetQuery(con, "SELECT * FROM `gbadata`.`qb_list_medicaldevices`;")


# qb_list_medicalservices -------------------------------------------------

# dbListFields(con, "qb_list_medicalservices")

df <- lists_insert_single_table(table_name = "Medizinische_Leistungsangebote",
                                primary_key_var = "idqb_list_medicalservices",
                                variable_lookup_table = list("code" = "Nummer",
                                                             "area" = "Bereich",
                                                             "description" = "Versorgungsschwerpunkt",
                                                             "comment" = "Kommentar_Erlaeuterung"))

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`qb_list_medicalservices` ",
                                  "(`code`, `description`, `comment`, ",
                                  "`area`, `HospitalDataYear_idHospitalDataYear`) ",
                                  "VALUES (?, ?, ?, ?, ?);"))

dbBind(data_in, params = list(
    df$code,
    df$description,
    df$comment,
    df$area,
    df$HospitalDataYear_idHospitalDataYear))
dbClearResult(data_in)
rm(df, data_in)

# dbGetQuery(con, "SELECT * FROM `gbadata`.`qb_list_medicalservices`;")


# qb_list_outpatienttreatments --------------------------------------------

# dbListFields(con, "qb_list_outpatienttreatments")

df <- lists_insert_single_table(table_name = "Ambulante_Behandlungsmoeglichkeiten",
                                primary_key_var = "idqb_list_outpatienttreatments",
                                variable_lookup_table = list("code" = "Nummer",
                                                             "description" = "Art_der_Ambulanz_Mehrfachnennungen_moeglich"))

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`qb_list_outpatienttreatments` ",
                                  "(`code`, `description`, ",
                                  "`HospitalDataYear_idHospitalDataYear`) ",
                                  "VALUES (?, ?, ?);"))

dbBind(data_in, params = list(
    df$code,
    df$description,
    df$HospitalDataYear_idHospitalDataYear))
dbClearResult(data_in)
rm(df, data_in)

# dbGetQuery(con, "SELECT * FROM `gbadata`.`qb_list_outpatienttreatments`;")



# qb_list_paragraph116b ---------------------------------------------------

# dbListFields(con, "qb_list_paragraph116b")

df <- lists_insert_single_table(table_name = "Leistungen_im_Katalog_nach_Paragraph_116b_SGB_V",
                                primary_key_var = "idqb_list_paragraph116b",
                                variable_lookup_table = list("code" = "Nummer",
                                                             "codeOfAnnex" = "Nummer_der_Anlage",
                                                             "codeOfAnnex" = "Nummer_der_Anlage_zur_Richtlinie_ueber_die_ambulante_spezialfachaerztliche_Versorgung_nach_Paragraph_116b_SGB_V_in_der_Fassung_vom_22012015",
                                                             "description" = "Angebotene_Leistungen",
                                                             "comment" = "Kommentar_Erlaeuterung",
                                                             "area" = "Behandlungsart"))

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`qb_list_paragraph116b` ",
                                  "(`code`, `codeOfAnnex`, ",
                                  "`description`, `comment`, `area`, ",
                                  "`HospitalDataYear_idHospitalDataYear`) ",
                                  "VALUES (?, ?, ?, ?, ?, ?);"))

dbBind(data_in, params = list(
    df$code,
    df$codeOfAnnex,
    df$description,
    df$comment,
    df$area,
    df$HospitalDataYear_idHospitalDataYear))
dbClearResult(data_in)
rm(df, data_in)

# dbGetQuery(con, "SELECT * FROM `gbadata`.`qb_list_paragraph116b`;")



# qb_list_medicalexpertise ------------------------------------------------

# dbListFields(con, "qb_list_medicalexpertise")

df <- lists_insert_single_table(table_name = "Aerztliche_Fachexpertise_der_Abteilung",
                                primary_key_var = "idqb_list_medicalexpertise",
                                variable_lookup_table = list("code" = "Nummer",
                                                             "description" = "Bezeichnung",
                                                             "comment" = "Kommentar_Erlaeuterung_z_B_Weiterbildungsbefugnisse",
                                                             "area" = "Art_der_Qualifikation"))

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`qb_list_medicalexpertise` ",
                                  "(`code`, `description`, ",
                                  "`comment`, `area`, `HospitalDataYear_idHospitalDataYear`) ",
                                  "VALUES (?, ?, ?, ?, ?);"))

dbBind(data_in, params = list(
    df$code,
    df$description,
    df$comment,
    df$area,
    df$HospitalDataYear_idHospitalDataYear))
dbClearResult(data_in)
rm(df, data_in)

# dbGetQuery(con, "SELECT * FROM `gbadata`.`qb_list_medicalexpertise`;")


# qb_list_nursingexpertise ------------------------------------------------

# dbListFields(con, "qb_list_nursingexpertise")

df <- lists_insert_single_table(table_name = "Pflegerische_Fachexpertise_der_Abteilung",
                                primary_key_var = "idqb_list_nursingexpertise",
                                variable_lookup_table = list("code" = "Nummer",
                                                             "description" = "Bezeichnung",
                                                             "comment" = "Kommentar_Erlaeuterung",
                                                             "area" = "Art_der_Qualifikation"))

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`qb_list_nursingexpertise` ",
                                  "(`code`, `description`, ",
                                  "`comment`, `area`, `HospitalDataYear_idHospitalDataYear`) ",
                                  "VALUES (?, ?, ?, ?, ?);"))

dbBind(data_in, params = list(
    df$code,
    df$description,
    df$comment,
    df$area,
    df$HospitalDataYear_idHospitalDataYear))
dbClearResult(data_in)
rm(df, data_in)

# dbGetQuery(con, "SELECT * FROM `gbadata`.`qb_list_nursingexpertise`;")


# qb_list_externalqualitymanagementbycountry ------------------------------

# dbListFields(con, "qb_list_externalqualitymanagementbystate")

df <- lists_insert_single_table(table_name = "Externe_Qualitaetssicherung_nach_Landesrecht",
                                primary_key_var = "idqb_list_externalqualitymanagementbystate",
                                variable_lookup_table = list("description" = "Leistungsbereich",
                                                             "comment" = "Kommentar_Erlaeuterung",
                                                             "state" = "Bundesland"))

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`qb_list_externalqualitymanagementbystate` ",
                                  "(`description`, ",
                                  "`comment`, `state`, `HospitalDataYear_idHospitalDataYear`) ",
                                  "VALUES (?, ?, ?, ?);"))

dbBind(data_in, params = list(
    df$description,
    df$comment,
    df$state,
    df$HospitalDataYear_idHospitalDataYear))
dbClearResult(data_in)
rm(df, data_in)

# dbGetQuery(con, "SELECT * FROM `gbadata`.`qb_list_externalqualitymanagementbystate`;")


# qb_list_minimumquantitiesregulation -------------------------------------

# dbListFields(con, "qb_list_minimumquantitiesregulation")

df <- lists_insert_single_table(table_name = "(Umsetzung_der_Mindestmengenregelungen_Begruendungen_bei_Nichterreichen_der_Mindestmenge)|(Umsetzung_der_Mindestmengenregelungen_Ausnahmetatbestaende)|(Umsetzung_der_Mindestmengenregelungen_Ausnahmen)",
                                primary_key_var = "idqb_list_minimumquantitiesregulation",
                                variable_lookup_table = list("code" = "Nummer",
                                                             "description" = "Begruendungen",
                                                             "description" = "Ausnahmen",
                                                             "description" = "Ausnahmetatbestand",
                                                             "comment" = "Kommentar_Erlaeuterung"))

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`qb_list_minimumquantitiesregulation` ",
                                  "(`code`, `description`, ",
                                  "`comment`, `HospitalDataYear_idHospitalDataYear`) ",
                                  "VALUES (?, ?, ?, ?);"))

dbBind(data_in, params = list(
    df$code,
    df$description,
    df$comment,
    df$HospitalDataYear_idHospitalDataYear))
dbClearResult(data_in)
rm(df, data_in)

# dbGetQuery(con, "SELECT * FROM `gbadata`.`qb_list_minimumquantitiesregulation`;")



# qb_list_measuresqualitymanagement ---------------------------------------

# dbListFields(con, "qb_list_measuresqualitymanagement")

df <- lists_insert_single_table(table_name = "(Umsetzung_von_Beschluessen_zur_Qualitaetssicherung_nach_Paragraph_136_Abs_1_Satz_1_Nr_2_SGB_V)|(Umsetzung_von_Beschluessen_zur_Qualitaetssicherung_nach_Paragraph_137_Abs_1_Satz_1_Nr_2_SGB_V)",
                                primary_key_var = "idqb_list_measuresqualitymanagement",
                                variable_lookup_table = list("code" = "Nummer",
                                                             "description" = "Beschluss",
                                                             "comment" = "Kommentar_Erlaeuterung"))

data_in <- dbSendStatement(con,
                           paste0("INSERT INTO `gbadata`.`qb_list_measuresqualitymanagement` ",
                                  "(`code`, `description`, ",
                                  "`comment`, `HospitalDataYear_idHospitalDataYear`) ",
                                  "VALUES (?, ?, ?, ?);"))

dbBind(data_in, params = list(
    df$code,
    df$description,
    df$comment,
    df$HospitalDataYear_idHospitalDataYear))
dbClearResult(data_in)
rm(df, data_in)

# dbGetQuery(con, "SELECT * FROM `gbadata`.`qb_list_measuresqualitymanagement`;")


# Final commands ----------------------------------------------------------

Sys.sleep(3)
dbDisconnect(con)
rm(con, lists_2015, lists_2016, lists_2017, lists_2018, lists_2019, Jahre_lookup)

