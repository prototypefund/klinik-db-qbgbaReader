#' Insert the complete data for one hospital into the database.
#'
#' @param conn A RMariaDB-connection object (based on the DBI-package) to the
#'     database where the GBA data is stored.
#'
#' @param xml_path The path to the XML-file containing the detailed report (not the
#'     ones named "bund" or "Land"!).
#'
#' @param xml_schema_path Here, the path to the XML schema file for the year in
#'     question is needed which is used in the beginning of the parsing attempt to
#'     automatically check the integrity of the XML-file against the schema. The
#'     schema files are available from the GBA.
#'
#' @param global_hospital_id A data frame that was manually constructed, which
#'     comprises a list of all hospitals with all available reports starting from
#'     2015 to 2019. Each entry gives the XML-filename, an ID that identifies the
#'     hospital in each year, an ID that is the same over all years, the
#'     IK number, the location number, the year, the name of the hospital,
#'     the original name in the XML-file, the state where the hospital is located,
#'     latitude and longitude.
#'
#' @param years_lookuptable A named vector comprising the years from 2015 to 2019:
#'     Default is \code{c("1" = 2015, "2" = 2016, "3" = 2017, "4" = 2018, "5" = 2019)}.
#'
#' @param db_name The name of the database as a single string value. This is used
#'     to confirm that one is connected to the desired database. Defaults to
#'     "gbadata".
#'
#'
#' @return As a return value the message "Success parsing file: " together with
#'     the complete path of the processed XML-file is given. If the file deals
#'     with a "Gesamtbericht", i.e., an aggregated report over more than one
#'     location, it is skipped and the message  'Skipping aggregated report
#'     ("Gesamtbericht"): ' together with the file path is returned.
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' con <- dbConnect(RMariaDB::MariaDB(),
#'                  host     = "localhost",
#'                  port     = 3306,
#'                  username = keyring::key_list("mysql-localhost")[1,2],
#'                  password = keyring::key_get("mysql-localhost", "dataadmin"),
#'                  dbname   = "gbadata")
#'
#' GlobalHospitalID <- readxl::read_excel("./00-data/GlobalHospitalID.xlsx",
#'                                        col_types = c(rep("text", times = 9),
#'                                        "numeric",
#'                                        "numeric"))
#' GlobalHospitalID <- GlobalHospitalID %>%
#'      mutate(idHospitalDataYear = case_when(
#'             year == "2015" ~ 1L,
#'             year == "2016" ~ 2L,
#'             year == "2017" ~ 3L,
#'             year == "2018" ~ 4L,
#'             year == "2019" ~ 5L
#'             ))
#'
#' reports_detailed <- list.files("../2019_v2/Berichte-Teile-A-B-C/",
#'                                pattern = "-xml\\.xml$", full.names = TRUE)
#' XML_Schema_path <-
#'     "L:/19_GBA-QB/Servicedateien_2019/2020-10-07_Anlage-5_XML_Schema-BJ-2019.xsd"
#' details <- read_xml(reports_detailed[[1]])
#'
#' qb_extract_one_clinic(con,
#'                       xml_path = details,
#'                       xml_schema_path = XML_Schema_path,
#'                       global_hospital_id = GlobalHospitalID)
#'
#'
#' }
#'
#' @export
#'
qb_extract_one_clinic <- function(conn = NULL,
                                  xml_path = NULL,
                                  xml_schema_path = NULL,
                                  global_hospital_id = NULL,
                                  years_lookuptable = c("1" = 2015,
                                                        "2" = 2016,
                                                        "3" = 2017,
                                                        "4" = 2018,
                                                        "5" = 2019),
                                  db_name = "gbadata") {

    # reports_detailed <- list.files("../2019_v2/Berichte-Teile-A-B-C/", pattern = "-xml\\.xml$", full.names = TRUE)
    # conn = con
    # xml_path = "../2015/Berichte-Teile-A-B-C2ff/260100693-00-2015-xml.xml"
    # xml_schema_path = XML_Schema_path_2015
    # global_hospital_id = GlobalHospitalID
    # years_lookuptable = c("1" = 2015,
    #                  "2" = 2016,
    #                  "3" = 2017,
    #                  "4" = 2018,
    #                  "5" = 2019)
    # db_name = "gbadata"


    # encoding = "UTF-8"
    # if (is.null(encoding) ||
    #     length(encoding) != 1 ||
    #     !is.character(encoding)) {
    #
    #     stop('Parameter "encoding" must be a single string.')
    #
    # }



    # Read in data from the xml files -----------------------------------------

    details <- read_xml(xml_path)

    file_name <- str_replace_all(basename(xml_path), "-xml\\.xml$", "")

    if (!is.na(xml_find_first(details, '//Standortkontaktdaten_Gesamtbericht')) ||
        (!is.na(xml_find_first(details, '//Standorte_des_Krankenhauses')) &&
         str_detect(file_name, "([0-9]{9,9})-99-([0-9]{4,4})"))) {

        return(paste0('Skipping aggregated report ("Gesamtbericht"): ', xml_path))

    }

    ### TODO - Dynamic loading of the xml-schema!

    if (!xml_validate(details, read_xml(xml_schema_path))) {

        stop('XML-file:\n\n', xml_path,'\n\nis not valid according to the provided schema ("', basename(xml_schema_path), '")!')

    }


    # Einleitung --------------------------------------------------------------

    Einleitung <- qb_extractor_Einleitung(details)



    # Extract "Allgemeine Informationen" --------------------------------------

    Fachabteilungen <- dbGetQuery(conn, paste0('SELECT * FROM `', db_name, '`.`DepartmentTypes`;'))

    AllgInfos <- qb_extractor_Allgemeine_Informationen(details,
                                                       file_name = file_name,
                                                       Fachabteilungen = Fachabteilungen)


    # Krankenhaustraeger ------------------------------------------------------

    HospitalOperator <- AllgInfos[["HospitalOperator"]]

    if (all(!is.na(HospitalOperator$description))) {

        HospitalOperator <- qb_db_set_query(conn,
                                            HospitalOperator,
                                            table_name = "HospitalOperator",
                                            db_name = db_name)

    } else {

        stop('The name ("description") of the hospital operator must not be missing!')

    }




    # Generate GlobalID -------------------------------------------------------

    Krankenhaus_Adressen <- AllgInfos[["Krankenhaus_Adressen"]]

    if (sum(global_hospital_id$xmlFileName == file_name) == 0) {

        lat_lon_current <-
            suppressMessages(
                tmaptools::geocode_OSM(
                    paste0(
                        unname(unlist(Krankenhaus_Adressen[Krankenhaus_Adressen$use == "Krankenhaus_Kontakt", "street"])),
                        " ",
                        unname(unlist(Krankenhaus_Adressen[Krankenhaus_Adressen$use == "Krankenhaus_Kontakt", "housenumber"])),
                        ", ",
                        unname(unlist(Krankenhaus_Adressen[Krankenhaus_Adressen$use == "Krankenhaus_Kontakt", "zip"])),
                        " ",
                        unname(unlist(Krankenhaus_Adressen[Krankenhaus_Adressen$use == "Krankenhaus_Kontakt", "city"]))
                    ),
                    as.data.frame = TRUE,
                    return.first.only = TRUE,
                    geometry = "point",
                    keep.unfound = TRUE
                )
            )

        global_hospital_id_current <- tibble(
            "xmlFileName" = file_name,
            "ID_across_Years" = file_name,
            "ID" = str_replace(file_name, "-[0-9]{4,4}$", ""),
            "ikNumber" = AllgInfos[["Krankenhaus_IK"]],
            "locationNumber" = AllgInfos[["Krankenhaus_Standortnummer"]],
            "year" = str_extract(file_name, "[0-9]{4,4}$"),
            "description" = AllgInfos[["Krankenhaus_Name"]],
            "originalNameInXML" = AllgInfos[["Krankenhaus_Name"]],
            "state" = NA_character_,
            "latitude" = lat_lon_current$lat,
            "longitude" = lat_lon_current$lon,
            "idHospitalDataYear" = as.integer(names(years_lookuptable[years_lookuptable == str_extract(file_name, "[0-9]{4,4}$")])),
            "idGlobalHospitalIdentifier" = NA_integer_
        )

    } else if (sum(global_hospital_id$xmlFileName == file_name) == 1) {

        global_hospital_id_current <- global_hospital_id[global_hospital_id$xmlFileName == file_name, ] %>%
            mutate(idGlobalHospitalIdentifier = NA_integer_)

    } else {

        stop('A global identifier could neither be obtained nor constructed: More than one ID belongs to this file!')

    }

    Krankenhaus_Adressen[Krankenhaus_Adressen$use == "Krankenhaus_Kontakt", "lat"] <- global_hospital_id_current$latitude
    Krankenhaus_Adressen[Krankenhaus_Adressen$use == "Krankenhaus_Kontakt", "lon"] <- global_hospital_id_current$longitude


    # First entry in DB -------------------------------------------------------

    global_hospital_id_current <- qb_db_set_query(conn,
                                                  global_hospital_id_current,
                                                  table_name = "GlobalHospitalIdentifier",
                                                  db_name = db_name,
                                                  return_orig_cols = TRUE)

    Hospital <- tibble("idHospital" = NA_integer_,
                       "xmlFileName" = file_name,
                       "description" = AllgInfos[["Krankenhaus_Name"]],
                       "alias" = NA_character_,
                       "ikNumber" = AllgInfos[["Krankenhaus_IK"]],
                       "locationNumberHospital" = AllgInfos[["Krankenhaus_Standortnummer"]],
                       "locationNumberSite" = AllgInfos[["Standort_Standortnummer"]],
                       "phoneHospital" = AllgInfos[["Krankenhaus_Telefon"]],
                       "phoneLocation" = AllgInfos[["Standort_Telefon"]],
                       "urlHospital" = Einleitung[["URL_Internetseite_Krankenhaus"]],
                       "urlAdditionalInformation" = Einleitung[["URL_Weitere_Informationen"]],
                       "HospitalDataYear_idHospitalDataYear" = global_hospital_id_current$idHospitalDataYear,
                       "HospitalOperator_idHospitalOperator" = HospitalOperator$idHospitalOperator,
                       "GlobalHospitalIdentifier_idGlobalHospitalIdentifier" = global_hospital_id_current$idGlobalHospitalIdentifier)

    if (is.na(Hospital$xmlFileName) ||
        is.na(Hospital$description)) {

        stop('The name of the xml-file and the name of the hospital ("description") must be present!')

    } else {

        Hospital <- qb_db_set_query(conn,
                                    Hospital,
                                    table_name = "Hospital",
                                    db_name = db_name)

    }

    AddOnInfoHospital <- tibble("idAddOnInfoHospital" = NA_integer_,
                                "academicTeachingHospital" = AllgInfos[["Akademisches_Lehrkrankenhaus"]],
                                "universityClinic" = AllgInfos[["Universitaetsklinikum"]],
                                "universityName" = AllgInfos[["Name_Universitaet"]],
                                "psychiatricHospital" = AllgInfos[["Psychiatrisches_Krankenhaus"]],
                                "psychiatricDutyToSupply" = AllgInfos[["Psychiatrie_Versorgungsverpflichtung"]],
                                "Hospital_idHospital" = Hospital$idHospital)

    AddOnInfoHospital_primary_key <- dbGetQuery(conn, paste0("SELECT `idAddOnInfoHospital` FROM `", db_name, "`.`AddOnInfoHospital`",
                                                             " WHERE `Hospital_idHospital` = ", Hospital$idHospital))

    if (nrow(AddOnInfoHospital_primary_key) == 1) {

        AddOnInfoHospital$idAddOnInfoHospital <- unlist(unname(AddOnInfoHospital_primary_key))

    }


    AddOnInfoHospital <- qb_db_set_query(conn,
                                         AddOnInfoHospital,
                                         table_name = "AddOnInfoHospital",
                                         db_name = db_name)


    Address_1 <- bind_rows(Krankenhaus_Adressen,
                           AllgInfos[["Standort_Adressen"]]) %>%
        filter(if_any(everything(), ~ !is.na(.)))

    if (any(!is.na(Address_1$street)) ||
        any(!is.na(Address_1$city)) ||
        any(!is.na(Address_1$URL))) {

        Address_1 <- qb_db_set_query(conn,
                                     Address_1,
                                     table_name = "Address",
                                     db_name = db_name)

        Relations_1 <- tibble("Hospital_idHospital" = Hospital$idHospital,
                              "Address_idAddress" = Address_1$idAddress)
        Relations_1 <- qb_db_set_query(conn,
                                       Relations_1,
                                       table_name = "Relations",
                                       db_name = db_name)

    }


    FurtherLinks <- Einleitung[["FurtherLinks"]]
    FurtherLinks$Hospital_idHospital <- Hospital$idHospital

    if (any(!is.na(FurtherLinks$link))) {

        Further_links <- qb_db_set_query(conn,
                                         FurtherLinks,
                                         table_name = "FurtherLinks",
                                         db_name = db_name)

    }

    Dataset_metadata <- Einleitung[["Dataset_metadata"]]
    Dataset_metadata$Hospital_idHospital <- Hospital$idHospital

    if (!is.na(Dataset_metadata$date)) {

        Dataset_metadata <- qb_db_set_query(conn,
                                            Dataset_metadata,
                                            table_name = "Dataset_metadata",
                                            db_name = db_name)

    }


    Person_1 <- bind_rows(Einleitung[["Verantwortlicher_Erstellung_Qualitaetsbericht"]],
                          Einleitung[["Verantwortlicher_Krankenhausleitung"]],
                          AllgInfos[["Krankenhaus_Leitungspersonen"]],
                          AllgInfos[["Standort_Leitungspersonen"]]) %>%
        filter(if_any(everything(), ~ !is.na(.)))

    if (any(!is.na(Person_1$role)) ||
        any(!is.na(Person_1$responsibility)) ||
        any(!is.na(Person_1$lastname)) ||
        any(!is.na(Person_1$email))) {

        Person_1 <- qb_db_set_query(conn,
                                    Person_1,
                                    table_name = "Person",
                                    db_name = db_name)

        Relations_2 <- tibble("Hospital_idHospital" = Hospital$idHospital,
                              "Person_idPerson" = Person_1$idPerson)
        Relations_2 <- qb_db_set_query(conn,
                                       Relations_2,
                                       table_name = "Relations",
                                       db_name = db_name)

    }




    # Anzahl Betten & Fallzahlen ----------------------------------------------

    Anzahl_Betten_und_Fallzahlen <- qb_extractor_Anzahl_Betten_und_Fallzahlen(details,
                                                                              Hospital_id = Hospital$idHospital)

    if (Anzahl_Betten_und_Fallzahlen[["HospitalKPI_exists"]]) {

        HospitalKPI <- qb_db_set_query(conn,
                                       Anzahl_Betten_und_Fallzahlen[["HospitalKPI"]],
                                       table_name = "HospitalKPI",
                                       db_name = db_name)

    }



    # Akademische_Lehre -------------------------------------------------------

    Akademische_Lehre <- qb_extractor_Akademische_Lehre(details,
                                                        Hospital_id = Hospital$idHospital)

    if (Akademische_Lehre[["AcademicTeaching_exists"]]) {

        AcademicTeaching <- qb_db_set_query(conn,
                                            Akademische_Lehre[["AcademicTeaching"]],
                                            table_name = "AcademicTeaching",
                                            db_name = db_name)

    }


    # Ausbildung_andere_Heilberufe --------------------------------------------

    Ausbildung_andere_Heilberufe <- qb_extractor_Ausbildung_andere_Heilberufe(details,
                                                                              Hospital_id = Hospital$idHospital)

    if (Ausbildung_andere_Heilberufe[["TrainingOtherHealthProfessionals_exists"]]) {

        TrainingOtherHealthProfessionals <- qb_db_set_query(conn,
                                                            Ausbildung_andere_Heilberufe[["TrainingOtherHealthProfessionals"]],
                                                            table_name = "TrainingOtherHealthProfessionals",
                                                            db_name = db_name)

    }


    # Umgang_mit_Risiken_in_der_Patientenversorgung ---------------------------

    Umgang_mit_Risiken <- qb_extractor_Umgang_mit_Risiken_in_der_Patientenversorgung(details,
                                                                                     Hospital_id = Hospital$idHospital)

    if (Umgang_mit_Risiken[["Qualitymanagement_exists"]]) {

        Qualitymanagement <- qb_db_set_query(conn,
                                             Umgang_mit_Risiken[["Qualitymanagement"]],
                                             table_name = "Qualitymanagement",
                                             db_name = db_name)

    }

    if (Umgang_mit_Risiken[["Riskmanagement_exists"]]) {

        Riskmanagement <- qb_db_set_query(conn,
                                          Umgang_mit_Risiken[["Riskmanagement"]],
                                          table_name = "Riskmanagement",
                                          db_name = db_name)

    }


    Persons_QM_RM <- Umgang_mit_Risiken[["Persons_QM_RM"]]

    if (any(!is.na(Persons_QM_RM$role)) ||
        any(!is.na(Persons_QM_RM$responsibility)) ||
        any(!is.na(Persons_QM_RM$lastname)) ||
        any(!is.na(Persons_QM_RM$email))) {

        Persons_QM_RM <- qb_db_set_query(conn,
                                         Persons_QM_RM,
                                         table_name = "Person",
                                         db_name = db_name)
        Relations_3 <- tibble("Hospital_idHospital" = Hospital$idHospital,
                              "Person_idPerson" = Persons_QM_RM$idPerson)
        Relations_3 <- qb_db_set_query(conn,
                                       Relations_3,
                                       table_name = "Relations",
                                       db_name = db_name)

    }


    # Apparative Ausstattung --------------------------------------------------

    Apparative_Ausstattung <- qb_extractor_Apparative_Ausstattung(details,
                                                                  Hospital_id = Hospital$idHospital)

    if (Apparative_Ausstattung[["MedicalDevices_exists"]]) {

        MedicalDevices <- qb_db_set_query(conn,
                                          Apparative_Ausstattung[["MedicalDevices"]],
                                          table_name = "MedicalDevices",
                                          db_name = db_name)

    }


    # Medizinisch Pflegerische Leistungsangebote ------------------------------

    Medizinisch_Pflegerische_Leistungsangebote <- qb_extractor_Medizinisch_Pflegerische_Leistungsangebote(details,
                                                                                                          Hospital_id = Hospital$idHospital)

    if (Medizinisch_Pflegerische_Leistungsangebote[["NursingServices_exists"]]) {

        NursingServices <- qb_db_set_query(conn,
                                           Medizinisch_Pflegerische_Leistungsangebote[["NursingServices"]],
                                           table_name = "NursingServices",
                                           db_name = db_name)

    }


    # Nicht Medizinische Leistungsangebote ------------------------------------

    Nicht_Medizinische_Leistungsangebote <- qb_extractor_Nicht_Medizinische_Leistungsangebote(details,
                                                                                              Hospital_id = Hospital$idHospital)

    if (Nicht_Medizinische_Leistungsangebote[["NonMedicalServices_exists"]]) {

        NonMedicalServices <- qb_db_set_query(conn,
                                              Nicht_Medizinische_Leistungsangebote[["NonMedicalServices"]],
                                              table_name = "NonMedicalServices",
                                              db_name = db_name)

    }


    # Qualitaetssicherung -----------------------------------------------------

    Qualitaetssicherung <- qb_extractor_Qualitaetssicherung(details,
                                                            Hospital_id = Hospital$idHospital)

    if (Qualitaetssicherung[["AdditionalExternalQualityManagement_exists"]]) {

        AdditionalExternalQualityManagement <- qb_db_set_query(conn,
                                                               Qualitaetssicherung[["AdditionalExternalQualityManagement"]],
                                                               table_name = "AdditionalExternalQualityManagement",
                                                               db_name = db_name)

    }


    # Teilnahme Notfallversorgung ---------------------------------------------

    Teilnahme_Notfallversorgung <- qb_extractor_Teilnahme_Notfallversorgung(details,
                                                                            Hospital_id = Hospital$idHospital)

    if (Teilnahme_Notfallversorgung[["emergencyCareSpecialModules_exists"]]) {

        emergencyCareSpecialModules <- qb_db_set_query(conn,
                                                       Teilnahme_Notfallversorgung[["emergencyCareSpecialModules"]],
                                                       table_name = "emergencyCareSpecialModules",
                                                       db_name = db_name)

    }

    if (Teilnahme_Notfallversorgung[["emergencyCareLevelPrerequisitesAssurances_exists"]]) {

        emergencyCareLevelPrerequisitesAssurances <- qb_db_set_query(conn,
                                                                     Teilnahme_Notfallversorgung[["emergencyCareLevelPrerequisitesAssurances"]],
                                                                     table_name = "emergencyCareLevelPrerequisitesAssurances",
                                                                     db_name = db_name)

    }


    if (Teilnahme_Notfallversorgung[["emergencyCare_exists"]]) {

        emergencyCare <- qb_db_set_query(conn,
                                         Teilnahme_Notfallversorgung[["emergencyCare"]],
                                         table_name = "emergencyCare",
                                         db_name = db_name)

    }


    # Beschwerdemanagement ----------------------------------------------------

    Beschwerdemanagement_section <- qb_extractor_Beschwerdemanagement(details,
                                                                      Hospital_id = Hospital$idHospital)

    if (Beschwerdemanagement_section[["ComplaintManagement_exists"]]) {

        ComplaintManagement <- qb_db_set_query(conn,
                                               Beschwerdemanagement_section[["ComplaintManagement"]],
                                               table_name = "ComplaintManagement",
                                               db_name = db_name)

    }

    Ansprechpartner_Beschwerdemanagement <- Beschwerdemanagement_section[["Ansprechpartner_Beschwerdemanagement"]]

    if (any(!is.na(Ansprechpartner_Beschwerdemanagement$role)) ||
        any(!is.na(Ansprechpartner_Beschwerdemanagement$responsibility)) ||
        any(!is.na(Ansprechpartner_Beschwerdemanagement$lastname)) ||
        any(!is.na(Ansprechpartner_Beschwerdemanagement$email))) {

        Ansprechpartner_Beschwerdemanagement <- qb_db_set_query(conn,
                                                                Ansprechpartner_Beschwerdemanagement,
                                                                table_name = "Person",
                                                                db_name = db_name)
        Relations_4a <- tibble("Hospital_idHospital" = Hospital$idHospital,
                               "Person_idPerson" = Ansprechpartner_Beschwerdemanagement$idPerson)
        Relations_4a <- qb_db_set_query(conn,
                                        Relations_4a,
                                        table_name = "Relations",
                                        db_name = db_name)

    }


    Patientenfuersprecher <- Beschwerdemanagement_section[["Patientenfuersprecher"]]

    if (any(!is.na(Patientenfuersprecher$role)) ||
        any(!is.na(Patientenfuersprecher$responsibility)) ||
        any(!is.na(Patientenfuersprecher$lastname)) ||
        any(!is.na(Patientenfuersprecher$email))) {

        Patientenfuersprecher <- qb_db_set_query(conn,
                                                 Patientenfuersprecher,
                                                 table_name = "Person",
                                                 db_name = db_name)
        Relations_4b <- tibble("Hospital_idHospital" = Hospital$idHospital,
                               "Person_idPerson" = Patientenfuersprecher$idPerson)
        Relations_4b <- qb_db_set_query(conn,
                                        Relations_4b,
                                        table_name = "Relations",
                                        db_name = db_name)

    }


    # Barrierefreiheit --------------------------------------------------------

    Barrierefreiheit <- qb_extractor_Barrierefreiheit(details,
                                                      Hospital_id = Hospital$idHospital)

    if (Barrierefreiheit[["Accessibility_exists"]]) {

        Accessibility <-
            qb_db_set_query(conn,
                            Barrierefreiheit[["Accessibility"]],
                            table_name = "Accessibility",
                            db_name = db_name)

    }


    Ansprechpartner_Menschen_mit_Beeintraechtigung <- Barrierefreiheit[["Ansprechpartner_Menschen_mit_Beeintraechtigung"]]

    if (any(!is.na(Ansprechpartner_Menschen_mit_Beeintraechtigung$role)) ||
        any(!is.na(Ansprechpartner_Menschen_mit_Beeintraechtigung$responsibility)) ||
        any(!is.na(Ansprechpartner_Menschen_mit_Beeintraechtigung$lastname)) ||
        any(!is.na(Ansprechpartner_Menschen_mit_Beeintraechtigung$email))) {

        Ansprechpartner_Menschen_mit_Beeintraechtigung <- qb_db_set_query(conn,
                                                                          Ansprechpartner_Menschen_mit_Beeintraechtigung,
                                                                          table_name = "Person",
                                                                          db_name = db_name)
        Relations_5 <- tibble("Hospital_idHospital" = Hospital$idHospital,
                              "Person_idPerson" = Ansprechpartner_Menschen_mit_Beeintraechtigung$idPerson)
        Relations_5 <- qb_db_set_query(conn,
                                       Relations_5,
                                       table_name = "Relations",
                                       db_name = db_name)

    }



    # Arzneimitteltherapiesicherheit ------------------------------------------


    Arzneimitteltherapiesicherheit <- qb_extractor_Arzneimitteltherapiesicherheit(details,
                                                                                  Hospital_id = Hospital$idHospital)

    AddOnInfoHospital <- AddOnInfoHospital %>%
        mutate(medicinesCommission = Arzneimitteltherapiesicherheit[["Arzneimittelkommission"]],
               otherMedicinesWorkingGroup = Arzneimitteltherapiesicherheit[["Anderes_Gremium_Arbeitsgruppe"]],
               medicinesWGName = Arzneimitteltherapiesicherheit[["Name_verantwortliches_Gremium_Arbeitsgruppe"]],
               medicinesWGInvolvedDepartments = Arzneimitteltherapiesicherheit[["Beteiligte_Abteilungen_Funktionsbereiche"]],
               noMedicinesWorkingGroup = Arzneimitteltherapiesicherheit[["Kein_Gremium_Arbeitsgruppe"]],
               noPharmacoSafetyPersonInCharge = Arzneimitteltherapiesicherheit[["Verantwortliche_Person_AMTS_nicht_festgelegt"]])

    AddOnInfoHospital <- qb_db_set_query(conn,
                                         AddOnInfoHospital,
                                         table_name = "AddOnInfoHospital",
                                         db_name = db_name)


    if (Arzneimitteltherapiesicherheit[["PharmacotherapySteeringBoard_exists"]]) {

        PharmacotherapySteeringBoard <- qb_db_set_query(conn,
                                                        Arzneimitteltherapiesicherheit[["PharmacotherapySteeringBoard"]],
                                                        table_name = "PharmacotherapySteeringBoard",
                                                        db_name = db_name)

    }



    Verantwortliche_Person_AMTS <- Arzneimitteltherapiesicherheit[["Verantwortliche_Person_AMTS"]]

    if (any(!is.na(Verantwortliche_Person_AMTS$role)) ||
        any(!is.na(Verantwortliche_Person_AMTS$responsibility)) ||
        any(!is.na(Verantwortliche_Person_AMTS$lastname)) ||
        any(!is.na(Verantwortliche_Person_AMTS$email))) {

        Verantwortliche_Person_AMTS <-
            qb_db_set_query(conn,
                            Verantwortliche_Person_AMTS,
                            table_name = "Person",
                            db_name = db_name)
        Relations_6 <- tibble(
            "Hospital_idHospital" = Hospital$idHospital,
            "Person_idPerson" = Verantwortliche_Person_AMTS$idPerson
        )
        Relations_6 <-
            qb_db_set_query(conn,
                            Relations_6,
                            table_name = "Relations",
                            db_name = db_name)

    }



    if (Arzneimitteltherapiesicherheit[["pharmaceuticalStaff_exists"]]) {

        pharmaceuticalStaff <- qb_db_set_query(conn,
                                               Arzneimitteltherapiesicherheit[["pharmaceuticalStaff"]],
                                               table_name = "pharmaceuticalStaff",
                                               db_name = db_name)

    }


    if (Arzneimitteltherapiesicherheit[["PharmacotherapySafetyMeasures_exists"]]) {

        PharmacotherapySafetyMeasures <- qb_db_set_query(conn,
                                                         Arzneimitteltherapiesicherheit[["PharmacotherapySafetyMeasures"]],
                                                         table_name = "PharmacotherapySafetyMeasures",
                                                         db_name = db_name)

    }


    if (Arzneimitteltherapiesicherheit[["InstrumentsRM_exists"]]) {

        InstrumentsRM <- qb_db_set_query(conn,
                                         Arzneimitteltherapiesicherheit[["InstrumentsRM"]],
                                         table_name = "InstrumentsRM",
                                         db_name = db_name)

    }



    # Risikomanagement Einrichtungsinternes Fehlermeldesystem -----------------

    Einrichtungsinternes_Fehlermeldesystem <- qb_extractor_Einrichtungsinternes_Fehlermeldesystem(details,
                                                                                                  Hospital_id = Hospital$idHospital)

    if (Einrichtungsinternes_Fehlermeldesystem[["InstrumentsInternalComSystem_exists"]]) {


        InstrumentsInternalComSystem <- qb_db_set_query(conn,
                                                        Einrichtungsinternes_Fehlermeldesystem[["InstrumentsInternalComSystem"]],
                                                        table_name = "InstrumentsInternalComSystem",
                                                        db_name = db_name)

    }


    if (Einrichtungsinternes_Fehlermeldesystem[["InternalComSystem_exists"]]) {

        InternalComSystem <- qb_db_set_query(conn,
                                             Einrichtungsinternes_Fehlermeldesystem[["InternalComSystem"]],
                                             table_name = "InternalComSystem",
                                             db_name = db_name)

    }


    # Einrichtungsuebergreifendes Fehlermeldesystem ---------------------------

    RM_Einrichtungsuebergreifendes_Fehlermeldesystem_section <- qb_extractor_Einrichtungsuebergreifendes_Fehlermeldesystem(details,
                                                                                                                           Hospital_id = Hospital$idHospital)

    if (RM_Einrichtungsuebergreifendes_Fehlermeldesystem_section[["ExternalComSystem_exists"]]) {

        ExternalComSystem <- qb_db_set_query(conn,
                                             RM_Einrichtungsuebergreifendes_Fehlermeldesystem_section[["ExternalComSystem"]],
                                             table_name = "ExternalComSystem",
                                             db_name = db_name)

    }


    AddOnInfoHospital <- AddOnInfoHospital %>%
        mutate(interinstitutionalErrorReportingConferenceFrequency =
                   RM_Einrichtungsuebergreifendes_Fehlermeldesystem_section[["RM_Einrichtungsuebergreifendes_Fehlermeldesystem_Tagungsfrequenz"]])

    AddOnInfoHospital <- qb_db_set_query(conn,
                                         AddOnInfoHospital,
                                         table_name = "AddOnInfoHospital",
                                         db_name = db_name)




    # DMPs --------------------------------------------------------------------

    DMP_section <- qb_extractor_DMP(details,
                                    Hospital_id = Hospital$idHospital)

    if (DMP_section[["DiseaseManagementPrograms_exists"]]) {

        DiseaseManagementPrograms <- qb_db_set_query(conn,
                                                     DMP_section[["DiseaseManagementPrograms"]],
                                                     table_name = "DiseaseManagementPrograms",
                                                     db_name = db_name)

    }


    # QS_nach_Landesrecht -----------------------------------------------------

    QS_nach_Landesrecht <- qb_extractor_QS_nach_Landesrecht(details,
                                                            Hospital_id = Hospital$idHospital)


    AddOnInfoHospital <- AddOnInfoHospital %>%
        mutate(extQMbyState = QS_nach_Landesrecht[["Teilnahme_landesspezifische_Qualitaetssicherungsmassnahme"]])

    AddOnInfoHospital <- qb_db_set_query(conn,
                                         AddOnInfoHospital,
                                         table_name = "AddOnInfoHospital",
                                         db_name = db_name)


    if (QS_nach_Landesrecht[["ExternalQMbyState_exists"]]) {

        ExternalQMbyState <- qb_db_set_query(conn,
                                             QS_nach_Landesrecht[["ExternalQMbyState"]],
                                             table_name = "ExternalQMbyState",
                                             db_name = db_name,
                                             bulk_insert = TRUE)

    }


    # Mindestmengen -----------------------------------------------------------

    Mindestmengen_section <- qb_extractor_Mindestmengen(details,
                                                        Hospital_id = Hospital$idHospital)


    if (Mindestmengen_section[["MinimumQuantities_exists"]]) {

        MinimumQuantities <- qb_db_set_query(conn,
                                             Mindestmengen_section[["MinimumQuantities"]],
                                             table_name = "MinimumQuantities",
                                             db_name = db_name)

    }


    if (Mindestmengen_section[["MinimumQuantitiesPrognosis_exists"]]) {

        MinimumQuantitiesPrognosis <- qb_db_set_query(conn,
                                                      Mindestmengen_section[["MinimumQuantitiesPrognosis"]],
                                                      table_name = "MinimumQuantitiesPrognosis",
                                                      db_name = db_name)

    }



    # Strukturqualitaetsvereinbarung ------------------------------------------

    Strukturqualitaetsvereinbarung_section <- qb_extractor_Strukturqualitaetsvereinbarung(details,
                                                                                          Hospital_id = Hospital$idHospital)


    if (Strukturqualitaetsvereinbarung_section[["MeasuresQM_exists"]]) {

        MeasuresQM <- qb_db_set_query(conn,
                                      Strukturqualitaetsvereinbarung_section[["MeasuresQM"]],
                                      table_name = "MeasuresQM",
                                      db_name = db_name)

    }


    AddOnInfoHospital <- AddOnInfoHospital %>%
        mutate(messageNonPerformanceNursingCareGiven = Strukturqualitaetsvereinbarung_section[["messageNonPerformanceNursingCareGiven"]],
               clearingDialogueSQM = Strukturqualitaetsvereinbarung_section[["clearingDialogueSQM"]],
               clearingDialogueSQMCompleted = Strukturqualitaetsvereinbarung_section[["clearingDialogueSQMCompleted"]])

    AddOnInfoHospital <- qb_db_set_query(conn,
                                         AddOnInfoHospital,
                                         table_name = "AddOnInfoHospital",
                                         db_name = db_name)


    # Fortbildung -------------------------------------------------------------

    Fortbildung <- qb_extractor_Fortbildung(details,
                                            Hospital_id = Hospital$idHospital)

    if (Fortbildung[["ContinuingEducation_exists"]]) {

        ContinuingEducation <- qb_db_set_query(conn,
                                               Fortbildung[["ContinuingEducation"]],
                                               table_name = "ContinuingEducation",
                                               db_name = db_name)

    }


    # Pflegepersonalregelung --------------------------------------------------

    FulfillmentLevelPpUG_section <- qb_extractor_Pflegepersonalregelung(details,
                                                                        Hospital_id = Hospital$idHospital)


    if (FulfillmentLevelPpUG_section[["FulfillmentLevelPpUG_exists"]]) {

        FulfillmentLevelPpUG <- qb_db_set_query(conn,
                                                FulfillmentLevelPpUG_section[["FulfillmentLevelPpUG"]],
                                                table_name = "FulfillmentLevelPpUG",
                                                db_name = db_name)

    }


    # Personal_des_Krankenhauses ----------------------------------------------

    Personal_des_Krankenhauses <- qb_extractor_Personal_des_Krankenhauses(details,
                                                                          Hospital_id = Hospital$idHospital)

    AddOnInfoHospital <- AddOnInfoHospital %>%
        mutate(tariffWeeklyWorkingHoursDoctors = Personal_des_Krankenhauses[["tariffWeeklyWorkingHoursDoctors"]],
               tariffWeeklyWorkingHoursNurses = Personal_des_Krankenhauses[["tariffWeeklyWorkingHoursNurses"]])

    AddOnInfoHospital <- qb_db_set_query(conn,
                                         AddOnInfoHospital,
                                         table_name = "AddOnInfoHospital",
                                         db_name = db_name)

    if (Personal_des_Krankenhauses[["PersonnelHospital_exists"]]) {

        PersonnelHospital <- qb_db_set_query(conn,
                                             Personal_des_Krankenhauses[["PersonnelHospital"]],
                                             table_name = "PersonnelHospital",
                                             db_name = db_name)

    }

    if (Personal_des_Krankenhauses[["SpecialPersonnelHospital_exists"]]) {

        SpecialPersonnelHospital <- qb_db_set_query(conn,
                                                    Personal_des_Krankenhauses[["SpecialPersonnelHospital"]],
                                                    table_name = "SpecialPersonnelHospital",
                                                    db_name = db_name)

    }

    if (Personal_des_Krankenhauses[["AttendingDoctorsHospital_exists"]]) {

        AttendingDoctorsHospital <- qb_db_set_query(conn,
                                                    Personal_des_Krankenhauses[["AttendingDoctorsHospital"]],
                                                    table_name = "AttendingDoctorsHospital",
                                                    db_name = db_name)

    }


    # Hygienepersonal ---------------------------------------------------------

    Hygienepersonal_section <- qb_extractor_Hygienepersonal(details,
                                                            Hospital_id = Hospital$idHospital,
                                                            year = as.character(unname(years_lookuptable[names(years_lookuptable) == global_hospital_id_current$idHospitalDataYear])))

    HygieneStaff <- Hygienepersonal_section[["HygieneStaff"]]

    if (Hygienepersonal_section[["HygieneStaff_exists"]]) {

        HygieneStaff <- qb_db_set_query(conn,
                                        Hygienepersonal_section[["HygieneStaff"]],
                                        table_name = "HygieneStaff",
                                        db_name = db_name)

    }


    HygieneCommission_frequency <- Hygienepersonal_section[["HygieneCommission_frequency"]]

    if (!is.na(HygieneCommission_frequency)) {

        HygieneCommission <- tibble("idHygieneCommission" = NA_integer_,
                                    "HCisImplemented" = 1L,
                                    "frequency" = HygieneCommission_frequency,
                                    "Hospital_idHospital" = Hospital$idHospital)

    } else {

        HygieneCommission <- tibble("idHygieneCommission" = NA_integer_,
                                    "HCisImplemented" = 0L,
                                    "frequency" = NA_character_,
                                    "Hospital_idHospital" = Hospital$idHospital)

    }

    HygieneCommission <- qb_db_set_query(conn,
                                         HygieneCommission,
                                         table_name = "HygieneCommission",
                                         db_name = db_name)


    Ansprechpartner_Hygienekommission <- Hygienepersonal_section[["Ansprechpartner_Hygienekommission"]]

    if (any(!is.na(Ansprechpartner_Hygienekommission$role)) ||
        any(!is.na(Ansprechpartner_Hygienekommission$responsibility)) ||
        any(!is.na(Ansprechpartner_Hygienekommission$lastname)) ||
        any(!is.na(Ansprechpartner_Hygienekommission$email))) {

        Ansprechpartner_Hygienekommission <-
            qb_db_set_query(conn,
                            Ansprechpartner_Hygienekommission,
                            table_name = "Person",
                            db_name = db_name)
        Relations_7 <- tibble(
            "Hospital_idHospital" = Hospital$idHospital,
            "Person_idPerson" = Ansprechpartner_Hygienekommission$idPerson
        )
        Relations_7 <-
            qb_db_set_query(conn,
                            Relations_7,
                            table_name = "Relations",
                            db_name = db_name)

    }



    # Weitere_Informationen_Hygiene -------------------------------------------

    Weitere_Informationen_Hygiene <-
        qb_extractor_Weitere_Informationen_Hygiene(details,
                                                   Hospital_id = Hospital$idHospital,
                                                   year = as.character(unname(years_lookuptable[names(years_lookuptable) == global_hospital_id_current$idHospitalDataYear])))



    if (Weitere_Informationen_Hygiene[["HygieneFurtherMeasures_exists"]]) {

        HygieneFurtherMeasures <- qb_db_set_query(conn,
                                                  Weitere_Informationen_Hygiene[["HygieneFurtherMeasures"]],
                                                  table_name = "HygieneFurtherMeasures",
                                                  db_name = db_name)

    }

    if (Weitere_Informationen_Hygiene[["HygieneMeasures_exists"]]) {

        HygieneMeasures <- qb_db_set_query(conn,
                                           Weitere_Informationen_Hygiene[["HygieneMeasures"]],
                                           table_name = "HygieneMeasures",
                                           db_name = db_name)

    }


    # OEs ---------------------------------------------------------------------

    objOEs <- xml_find_all(details, '//Organisationseinheiten_Fachabteilungen/Organisationseinheit_Fachabteilung')

    all_OEs <- map(objOEs, ~ qb_extractor_OE(.x,
                                             year = as.character(unname(years_lookuptable[names(years_lookuptable) == global_hospital_id_current$idHospitalDataYear]))))

    # names(all_OEs[[1]])


    OE_Stammdaten <- map_dfr(all_OEs, `[[`, "OE_Stammdaten", .id = "OE_Counter") %>%
        filter(!is.na(.data$Name))

    if (nrow(OE_Stammdaten) > 0) {

        Departments <- OE_Stammdaten %>%
            mutate("idDepartments" = NA_integer_,
                   Ambulante_D_Arzt_Zulassung = as.integer(.data$Ambulante_D_Arzt_Zulassung),
                   Stationaere_BG_Zulassung = as.integer(.data$Stationaere_BG_Zulassung),
                   Ambulante_Operationen_erbracht = as.integer(.data$Ambulante_Operationen_erbracht)) %>%
            rename("description" = .data$Name,
                   "type" = .data$Abteilungsart,
                   "quantityCasesFull" = .data$Vollstationaere_Fallzahl,
                   "quantityCasesPartial" = .data$Teilstationaere_Fallzahl,
                   "quantityCasesComment" = .data$Fallzahlen_OE_Erlaueterung,
                   "accidentInsuranceOutpatientConsultancy" = .data$Ambulante_D_Arzt_Zulassung,
                   "accidentInsuranceInpatientLicence" = .data$Stationaere_BG_Zulassung,
                   "inpatientOperationsPerformed" = .data$Ambulante_Operationen_erbracht,
                   "targetAgreements" = .data$Zielvereinbarungen,
                   "targetAgreementsComment" = .data$Zielvereinbarungen_Erlaeuterungen,
                   "targetAgreementsNoDKGComment" = .data$Zielvereinbarungen_Erlaeuterungen_nicht_DKG,
                   "tariffWeeklyWorkingHoursDoctors" = .data$Massgebliche_tarifliche_Wochenarbeitszeit_Aerzte,
                   "tariffWeeklyWorkingHoursNurses" = .data$Massgebliche_tarifliche_Wochenarbeitszeit_Pflege) %>%
            select(.data$idDepartments, everything())

        Departments <- qb_db_set_query(conn,
                                       Departments,
                                       table_name = "Departments",
                                       return_orig_cols = TRUE,
                                       db_name = db_name)

        Relations_8 <- tibble(
            "Hospital_idHospital" = Hospital$idHospital,
            "Departments_idDepartments" = Departments$idDepartments
        )
        Relations_8 <-
            qb_db_set_query(conn,
                            Relations_8,
                            table_name = "Relations",
                            db_name = db_name)

    }


    Aerztliche_Leitung_OE <- map_dfr(all_OEs, `[[`, "Aerztliche_Leitung_OE", .id = "OE_Counter") %>%
        filter(if_any(.cols = -c(1), ~ !is.na(.)))

    if (any(!is.na(Aerztliche_Leitung_OE$role)) ||
        any(!is.na(Aerztliche_Leitung_OE$responsibility)) ||
        any(!is.na(Aerztliche_Leitung_OE$lastname)) ||
        any(!is.na(Aerztliche_Leitung_OE$email))) {

        Aerztliche_Leitung_OE <- Aerztliche_Leitung_OE %>%
            left_join(Departments[, c("OE_Counter", "idDepartments")], by = "OE_Counter")

        Aerztliche_Leitung_OE <- qb_db_set_query(conn,
                                                 Aerztliche_Leitung_OE,
                                                 table_name = "Person",
                                                 return_orig_cols = TRUE,
                                                 db_name = db_name)

        Relations_9 <- tibble(
            "Person_idPerson" = Aerztliche_Leitung_OE$idPerson,
            "Hospital_idHospital" = Hospital$idHospital,
            "Departments_idDepartments" = Aerztliche_Leitung_OE$idDepartments
        )
        Relations_9 <-
            qb_db_set_query(conn,
                            Relations_9,
                            table_name = "Relations",
                            db_name = db_name)

    }


    Kontakt_OE_Adresse <- map_dfr(all_OEs, `[[`, "Kontakt_OE_Adresse", .id = "OE_Counter") %>%
        filter(if_any(.cols = -c(1), ~ !is.na(.)))

    if (any(!is.na(Kontakt_OE_Adresse$street)) ||
        any(!is.na(Kontakt_OE_Adresse$city)) ||
        any(!is.na(Kontakt_OE_Adresse$URL))) {


        Kontakt_OE_Adresse <- Kontakt_OE_Adresse %>%
            left_join(Departments[, c("OE_Counter", "idDepartments")], by = "OE_Counter")

        Kontakt_OE_Adresse <- qb_db_set_query(conn,
                                              Kontakt_OE_Adresse,
                                              table_name = "Address",
                                              db_name = db_name)

        Relations_1 <- tibble("Hospital_idHospital" = Hospital$idHospital,
                              "Address_idAddress" = Kontakt_OE_Adresse$idAddress,
                              "Departments_idDepartments" = Kontakt_OE_Adresse$idDepartments)
        Relations_1 <- qb_db_set_query(conn,
                                       Relations_1,
                                       table_name = "Relations",
                                       db_name = db_name)

    }



    FA_Schluessel <- map_dfr(all_OEs, `[[`, "FA_Schluessel", .id = "OE_Counter") %>%
        filter(!is.na(.data$FA_Schluessel))

    if (nrow(FA_Schluessel) > 0) {

        DepartmentKeys <- FA_Schluessel %>%
            left_join(Departments[, c("OE_Counter", "idDepartments")], by = "OE_Counter") %>%
            mutate(idDepartmentKeys = NA_integer_) %>%
            rename("code" = .data$FA_Schluessel,
                   "description" = .data$Bezeichnung,
                   "Departments_idDepartments" = .data$idDepartments) %>%
            select(.data$idDepartmentKeys, everything())

        DepartmentKeys <- qb_db_set_query(conn,
                                          DepartmentKeys,
                                          table_name = "DepartmentKeys",
                                          return_orig_cols = TRUE,
                                          db_name = db_name,
                                          bulk_insert = TRUE)


    }


    Medizinische_Leistungsangebote <- map_dfr(all_OEs, `[[`, "Medizinische_Leistungsangebote", .id = "OE_Counter") %>%
        filter(!is.na(.data$VA_VU_Schluessel))

    if (nrow(Medizinische_Leistungsangebote) > 0) {

        MedicalServices <- Medizinische_Leistungsangebote %>%
            left_join(Departments[, c("OE_Counter", "idDepartments")], by = "OE_Counter") %>%
            mutate(idMedicalServices = NA_integer_) %>%
            rename("code" = .data$VA_VU_Schluessel,
                   "comment" = .data$Erlaeuterungen,
                   "Departments_idDepartments" = .data$idDepartments) %>%
            select(.data$idMedicalServices, everything())

        MedicalServices <- qb_db_set_query(conn,
                                           MedicalServices,
                                           table_name = "MedicalServices",
                                           return_orig_cols = TRUE,
                                           db_name = db_name,
                                           bulk_insert = TRUE)

    }


    Barrierefreiheit_OE <- map_dfr(all_OEs, `[[`, "Barrierefreiheit_OE", .id = "OE_Counter") %>%
        filter(!is.na(.data$BF_Schluessel))

    if (nrow(Barrierefreiheit_OE) > 0) {

        AccessibilityDepartment <- Barrierefreiheit_OE %>%
            left_join(Departments[, c("OE_Counter", "idDepartments")], by = "OE_Counter") %>%
            mutate(idAccessibilityDepartment = NA_integer_) %>%
            rename("code" = .data$BF_Schluessel,
                   "comment" = .data$Erlaeuterungen,
                   "Departments_idDepartments" = .data$idDepartments) %>%
            select(.data$idAccessibilityDepartment, everything())

        AccessibilityDepartment <- qb_db_set_query(conn,
                                                   AccessibilityDepartment,
                                                   table_name = "AccessibilityDepartment",
                                                   return_orig_cols = TRUE,
                                                   db_name = db_name,
                                                   bulk_insert = TRUE)

    }


    Hauptdiagnosen <- map_dfr(all_OEs, `[[`, "Hauptdiagnosen", .id = "OE_Counter") %>%
        filter(!is.na(.data$ICD_10))

    if (nrow(Hauptdiagnosen) > 0) {

        Diagnoses <- Hauptdiagnosen %>%
            left_join(Departments[, c("OE_Counter", "idDepartments")], by = "OE_Counter") %>%
            mutate(idDiagnoses = NA_integer_) %>%
            rename("code" = .data$ICD_10,
                   "cases" = .data$Fallzahl,
                   "Departments_idDepartments" = .data$idDepartments) %>%
            select(.data$idDiagnoses, everything()) %>%
            filter(!is.na(.data$code))

        Diagnoses <- qb_db_set_query(conn,
                                     Diagnoses,
                                     table_name = "Diagnoses",
                                     return_orig_cols = TRUE,
                                     db_name = db_name,
                                     bulk_insert = TRUE)

    }

    Prozeduren <- map_dfr(all_OEs, `[[`, "Prozeduren", .id = "OE_Counter") %>%
        filter(!is.na(.data$OPS_301))

    if (nrow(Prozeduren) > 0) {

        Procedures <- Prozeduren %>%
            left_join(Departments[, c("OE_Counter", "idDepartments")], by = "OE_Counter") %>%
            mutate(idProcedures = NA_integer_) %>%
            rename("code" = .data$OPS_301,
                   "amount" = .data$Anzahl,
                   "Departments_idDepartments" = .data$idDepartments) %>%
            select(.data$idProcedures, everything()) %>%
            filter(!is.na(.data$code))

        Procedures <- qb_db_set_query(conn,
                                      Procedures,
                                      table_name = "Procedures",
                                      return_orig_cols = TRUE,
                                      db_name = db_name,
                                      bulk_insert = TRUE)

    }

    Ambulante_Behandlungsmoeglichkeiten <- map_dfr(all_OEs, `[[`, "Ambulante_Behandlungsmoeglichkeiten", .id = "OE_Counter") %>%
        filter(!is.na(.data$AM_Schluessel))

    if (nrow(Ambulante_Behandlungsmoeglichkeiten) > 0) {

        OutpatientTreatments <- Ambulante_Behandlungsmoeglichkeiten %>%
            left_join(Departments[, c("OE_Counter", "idDepartments")], by = "OE_Counter") %>%
            mutate(idOutpatientTreatments = NA_integer_) %>%
            rename("code" = .data$AM_Schluessel,
                   "description" = .data$Bezeichnung,
                   "comment" = .data$Erlaeuterungen,
                   "codeMedicalServices" = .data$VA_VU_Schluessel_Ambulanz,
                   "nameOfotherCodes" = .data$Bezeichnung_sonstiger_Schluessel,
                   "Departments_idDepartments" = .data$idDepartments) %>%
            select(.data$idOutpatientTreatments, everything()) %>%
            filter(!is.na(.data$code))


        OutpatientTreatments <- qb_db_set_query(conn,
                                                OutpatientTreatments,
                                                table_name = "OutpatientTreatments",
                                                return_orig_cols = TRUE,
                                                db_name = db_name,
                                                bulk_insert = TRUE)

    }


    Ambulanz_116b <- map_dfr(all_OEs, `[[`, "Ambulanz_116b", .id = "OE_Counter") %>%
        filter(!is.na(.data$AM_116b_Schluessel))

    if (nrow(Ambulanz_116b) > 0) {

        Paragraph116b <- Ambulanz_116b %>%
            left_join(Departments[, c("OE_Counter", "idDepartments")], by = "OE_Counter") %>%
            mutate(idParagraph116b = NA_integer_) %>%
            rename("codeOutpatientTreatments" = .data$AM_116b_Schluessel,
                   "description" = .data$Bezeichnung,
                   "codeParagraph116b" = .data$Leistungen_Ambulanz_116b_Leistung_LK_Schluessel,
                   "Departments_idDepartments" = .data$idDepartments) %>%
            select(.data$idParagraph116b, everything()) %>%
            filter(!is.na(.data$codeOutpatientTreatments))

        Paragraph116b <- qb_db_set_query(conn,
                                         Paragraph116b,
                                         table_name = "Paragraph116b",
                                         return_orig_cols = TRUE,
                                         db_name = db_name,
                                         bulk_insert = TRUE)

    }


    Ambulante_Operationen <- map_dfr(all_OEs, `[[`, "Ambulante_Operationen", .id = "OE_Counter") %>%
        filter(!is.na(.data$OPS_301))

    if (nrow(Ambulante_Operationen) > 0) {

        OutpatientOperations <- Ambulante_Operationen %>%
            left_join(Departments[, c("OE_Counter", "idDepartments")], by = "OE_Counter") %>%
            mutate(idOutpatientOperations = NA_integer_) %>%
            rename("code" = .data$OPS_301,
                   "amount" = .data$Anzahl,
                   "Departments_idDepartments" = .data$idDepartments) %>%
            select(.data$idOutpatientOperations, everything()) %>%
            filter(!is.na(.data$code))

        OutpatientOperations <- qb_db_set_query(conn,
                                                OutpatientOperations,
                                                table_name = "OutpatientOperations",
                                                return_orig_cols = TRUE,
                                                db_name = db_name,
                                                bulk_insert = TRUE)

    }


    Personal_OE <- map_dfr(all_OEs, `[[`, "Personal_OE", .id = "OE_Counter") %>%
        filter(!is.na(.data$Personalkategorie))

    if (nrow(Personal_OE) > 0) {

        Personnel <- Personal_OE %>%
            left_join(Departments[, c("OE_Counter", "idDepartments")], by = "OE_Counter") %>%
            mutate(idPersonnel = NA_integer_) %>%
            rename("occupationalGroup" = .data$Personalkategorie,
                   "fulltimeStaff" = .data$Anzahl_VK,
                   "staffWithDirectEmployment" = .data$Beschaeftigungsverhaeltnis_Personal_mit_direktem_BV_Anzahl_VK,
                   "staffWithoutDirectEmployment" = .data$Beschaeftigungsverhaeltnis_Personal_ohne_direktem_BV_Anzahl_VK,
                   "staffOutpatientCare" = .data$Versorgungsform_Ambulante_Versorgung_Anzahl_VK,
                   "staffInpatientCare" = .data$Versorgungsform_Stationaere_Versorgung_Anzahl_VK,
                   "casesPerStaffInpatientCare" = .data$Versorgungsform_Stationaere_Versorgung_Fall_je_Anzahl,
                   "Departments_idDepartments" = .data$idDepartments) %>%
            select(.data$idPersonnel, everything())

        Personnel <- qb_db_set_query(conn,
                                     Personnel,
                                     table_name = "Personnel",
                                     return_orig_cols = TRUE,
                                     db_name = db_name,
                                     bulk_insert = TRUE)

    }


    Belegaerzte_OE <- map_dfr(all_OEs, `[[`, "Belegaerzte_OE", .id = "OE_Counter") %>%
        filter(!is.na(.data$Anzahl))

    if (nrow(Belegaerzte_OE) > 0) {

        AttendingDoctors <- Belegaerzte_OE %>%
            left_join(Departments[, c("OE_Counter", "idDepartments")], by = "OE_Counter") %>%
            mutate(idAttendingDoctors = NA_integer_,
                   Hospital_idHospital = Hospital$idHospital) %>%
            rename("quantity" = .data$Anzahl,
                   "casesPerQuantity" = .data$Fall_je_Anzahl,
                   "comment" = .data$Erlaeuterungen,
                   "Departments_idDepartments" = .data$idDepartments) %>%
            select(.data$idAttendingDoctors, everything()) %>%
            filter(!is.na(.data$quantity))

        AttendingDoctors <- qb_db_set_query(conn,
                                            AttendingDoctors,
                                            table_name = "AttendingDoctors",
                                            return_orig_cols = TRUE,
                                            db_name = db_name,
                                            bulk_insert = TRUE)

    }


    Aerztliche_Fachexpertisen <- map_dfr(all_OEs, `[[`, "Aerztliche_Fachexpertisen", .id = "OE_Counter") %>%
        filter(!is.na(.data$AQ_ZF_Schluessel))

    if (nrow(Aerztliche_Fachexpertisen) > 0) {

        MedicalExpertise <- Aerztliche_Fachexpertisen %>%
            left_join(Departments[, c("OE_Counter", "idDepartments")], by = "OE_Counter") %>%
            mutate(idMedicalExpertise = NA_integer_) %>%
            rename("code" = .data$AQ_ZF_Schluessel,
                   "Departments_idDepartments" = .data$idDepartments) %>%
            select(.data$idMedicalExpertise, everything()) %>%
            filter(!is.na(.data$code))

        MedicalExpertise <- qb_db_set_query(conn,
                                            MedicalExpertise,
                                            table_name = "MedicalExpertise",
                                            return_orig_cols = TRUE,
                                            db_name = db_name,
                                            bulk_insert = TRUE)

    }


    Pflegerische_Fachexpertisen <- map_dfr(all_OEs, `[[`, "Pflegerische_Fachexpertisen", .id = "OE_Counter") %>%
        filter(!is.na(.data$PQ_ZP_Schluessel))

    if (nrow(Pflegerische_Fachexpertisen) > 0) {

        NursingExpertise <- Pflegerische_Fachexpertisen %>%
            left_join(Departments[, c("OE_Counter", "idDepartments")], by = "OE_Counter") %>%
            mutate(idNursingExpertise = NA_integer_) %>%
            rename("code" = .data$PQ_ZP_Schluessel,
                   "Departments_idDepartments" = .data$idDepartments) %>%
            select(.data$idNursingExpertise, everything()) %>%
            filter(!is.na(.data$code))

        NursingExpertise <- qb_db_set_query(conn,
                                            NursingExpertise,
                                            table_name = "NursingExpertise",
                                            return_orig_cols = TRUE,
                                            db_name = db_name,
                                            bulk_insert = TRUE)

    }


    return(paste0("Success parsing file: ", xml_path))

}



