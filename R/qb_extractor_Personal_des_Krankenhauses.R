#' Extract all information regarding the topic "Personal des Krankenhauses".
#'
#' @param obj An XML-object comprising a complete detailed report.
#'
#' @param Hospital_id An integer value that represents the primary key of a
#'     hospital in the designated database.
#'
#'
#' @return A list with the following elements:
#'     \itemize{
#'         \item PersonnelHospital_exists
#'         \item PersonnelHospital
#'         \item AttendingDoctorsHospital_exists
#'         \item AttendingDoctorsHospital
#'         \item SpecialPersonnelHospital_exists
#'         \item SpecialPersonnelHospital
#'         \item tariffWeeklyWorkingHoursDoctors
#'         \item tariffWeeklyWorkingHoursNurses
#'     }
#'
#'
#' @examples
#'
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#' qb_extractor_Personal_des_Krankenhauses(doc, 1L)
#'
#'
#' @export
#'
qb_extractor_Personal_des_Krankenhauses <- function(obj, Hospital_id) {

    # obj <- details
    # Hospital_id <- 1L


    ### TODO - Each occupation can have elements "Erlaeuterungen" as sibling elements
    ### to "Anzahl_VK" nodes...within each of the inpatient / outpatient nodes etc, I guess...!
    ### Example:
    ### details <- read_xml("../2019_v2/Berichte-Teile-A-B-C/260100272-00-2019-xml.xml")


    Aerzte_ohne_Belegaerzte <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Aerzte/Aerzte_ohne_Belegaerzte'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Aerzte_ohne_Belegaerzte")

    Aerzte_ohne_Belegaerzte_FA <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Aerzte/Aerzte_ohne_Belegaerzte/Fachaerzte'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Aerzte_ohne_Belegaerzte_FA")


    Aerzte_ohne_Fachabteilungszuordnung <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Aerzte/Aerzte_ohne_Fachabteilungszuordnung'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Aerzte_ohne_Fachabteilungszuordnung")


    Aerzte_ohne_Fachabteilungszuordnung_FA <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Aerzte/Aerzte_ohne_Fachabteilungszuordnung/Fachaerzte'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Aerzte_ohne_Fachabteilungszuordnung_FA")


    Gesundheits_Krankenpfleger <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Pflegekraefte/Gesundheits_Krankenpfleger'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Gesundheits_Krankenpfleger")

    Gesundheits_Krankenpfleger_ohne_Fachabteilungszuordnung <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Pflegekraefte/Gesundheits_Krankenpfleger'
            ),
            "Personalerfassung_ohne_Fachabteilungszuordnung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Gesundheits_Krankenpfleger_ohne_Fachabteilungszuordnung")

    Gesundheits_Kinderkrankenpfleger <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Pflegekraefte/Gesundheits_Kinderkrankenpfleger'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Gesundheits_Kinderkrankenpfleger")

    Gesundheits_Kinderkrankenpfleger_ohne_Fachabteilungszuordnung <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Pflegekraefte/Gesundheits_Kinderkrankenpfleger'
            ),
            "Personalerfassung_ohne_Fachabteilungszuordnung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Gesundheits_Kinderkrankenpfleger_ohne_Fachabteilungszuordnung")


    Altenpfleger <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Pflegekraefte/Altenpfleger'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Altenpfleger")

    Altenpfleger_ohne_Fachabteilungszuordnung <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Pflegekraefte/Altenpfleger'
            ),
            "Personalerfassung_ohne_Fachabteilungszuordnung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Altenpfleger_ohne_Fachabteilungszuordnung")

    Krankenpflegehelfer <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Pflegekraefte/Krankenpflegehelfer'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Krankenpflegehelfer")

    Krankenpflegehelfer_ohne_Fachabteilungszuordnung <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Pflegekraefte/Krankenpflegehelfer'
            ),
            "Personalerfassung_ohne_Fachabteilungszuordnung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Krankenpflegehelfer_ohne_Fachabteilungszuordnung")

    Pflegehelfer <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Pflegekraefte/Pflegehelfer'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Pflegehelfer")

    Pflegehelfer_ohne_Fachabteilungszuordnung <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Pflegekraefte/Pflegehelfer'
            ),
            "Personalerfassung_ohne_Fachabteilungszuordnung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Pflegehelfer_ohne_Fachabteilungszuordnung")


    Hebammen_Entbindungspfleger <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Pflegekraefte/Hebammen_Entbindungspfleger'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Hebammen_Entbindungspfleger")

    Hebammen_Entbindungspfleger_ohne_Fachabteilungszuordnung <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Pflegekraefte/Hebammen_Entbindungspfleger'
            ),
            "Personalerfassung_ohne_Fachabteilungszuordnung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Hebammen_Entbindungspfleger_ohne_Fachabteilungszuordnung")


    Operationstechnische_Assistenz <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Pflegekraefte/Operationstechnische_Assistenz'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Operationstechnische_Assistenz")

    Operationstechnische_Assistenz_ohne_Fachabteilungszuordnung <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Pflegekraefte/Operationstechnische_Assistenz'
            ),
            "Personalerfassung_ohne_Fachabteilungszuordnung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Operationstechnische_Assistenz_ohne_Fachabteilungszuordnung")


    Medizinische_Fachangestellte <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Pflegekraefte/Medizinische_Fachangestellte'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Medizinische_Fachangestellte")

    Medizinische_Fachangestellte_ohne_Fachabteilungszuordnung <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Pflegekraefte/Medizinische_Fachangestellte'
            ),
            "Personalerfassung_ohne_Fachabteilungszuordnung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Medizinische_Fachangestellte_ohne_Fachabteilungszuordnung")


    Diplom_Psychologen <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Ausgewaehltes_Therapeutisches_Personal_Psycho/Diplom_Psychologen'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Diplom_Psychologen")


    Klinische_Neuropsychologen <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Ausgewaehltes_Therapeutisches_Personal_Psycho/Klinische_Neuropsychologen'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Klinische_Neuropsychologen")


    Psychologische_Psychotherapeuten <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Ausgewaehltes_Therapeutisches_Personal_Psycho/Psychologische_Psychotherapeuten'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Psychologische_Psychotherapeuten")


    Kinder_Jugendlichenpsychotherapeuten <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Ausgewaehltes_Therapeutisches_Personal_Psycho/Kinder_Jugendlichenpsychotherapeuten'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Kinder_Jugendlichenpsychotherapeuten")


    Psychotherapeuten_in_Ausbildung_waehrend_Taetigkeit <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Ausgewaehltes_Therapeutisches_Personal_Psycho/Psychotherapeuten_in_Ausbildung_waehrend_Taetigkeit'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Psychotherapeuten_in_Ausbildung_waehrend_Taetigkeit")


    Physiotherapeuten <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Ausgewaehltes_Therapeutisches_Personal_Psycho/Physiotherapeuten'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Physiotherapeuten")


    Ergotherapeuten <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Ausgewaehltes_Therapeutisches_Personal_Psycho/Ergotherapeuten'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Ergotherapeuten")


    Sozialpaedagogen <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Ausgewaehltes_Therapeutisches_Personal_Psycho/Sozialpaedagogen'
            ),
            "Personalerfassung",
            c(
                "Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
                "Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
                "Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
                "Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
            )
        ) %>%
        mutate(Personalkategorie = "Sozialpaedagogen")


    PersonnelHospital <- bind_rows(Aerzte_ohne_Belegaerzte,
                                   Aerzte_ohne_Belegaerzte_FA,
                                   Aerzte_ohne_Fachabteilungszuordnung,
                                   Aerzte_ohne_Fachabteilungszuordnung_FA,
                                   Gesundheits_Krankenpfleger,
                                   Gesundheits_Krankenpfleger_ohne_Fachabteilungszuordnung,
                                   Gesundheits_Kinderkrankenpfleger,
                                   Gesundheits_Kinderkrankenpfleger_ohne_Fachabteilungszuordnung,
                                   Altenpfleger,
                                   Altenpfleger_ohne_Fachabteilungszuordnung,
                                   Krankenpflegehelfer,
                                   Krankenpflegehelfer_ohne_Fachabteilungszuordnung,
                                   Pflegehelfer,
                                   Pflegehelfer_ohne_Fachabteilungszuordnung,
                                   Hebammen_Entbindungspfleger,
                                   Hebammen_Entbindungspfleger_ohne_Fachabteilungszuordnung,
                                   Operationstechnische_Assistenz,
                                   Operationstechnische_Assistenz_ohne_Fachabteilungszuordnung,
                                   Medizinische_Fachangestellte,
                                   Medizinische_Fachangestellte_ohne_Fachabteilungszuordnung,
                                   Diplom_Psychologen,
                                   Klinische_Neuropsychologen,
                                   Psychologische_Psychotherapeuten,
                                   Kinder_Jugendlichenpsychotherapeuten,
                                   Psychotherapeuten_in_Ausbildung_waehrend_Taetigkeit,
                                   Physiotherapeuten,
                                   Ergotherapeuten,
                                   Sozialpaedagogen) %>%
        rename("occupationalGroup" = .data$Personalkategorie,
               "fulltimeStaff" = .data$Anzahl_VK,
               "staffWithDirectEmployment" = .data$Beschaeftigungsverhaeltnis_Personal_mit_direktem_BV_Anzahl_VK,
               "staffWithoutDirectEmployment" = .data$Beschaeftigungsverhaeltnis_Personal_ohne_direktem_BV_Anzahl_VK,
               "staffOutpatientCare" = .data$Versorgungsform_Ambulante_Versorgung_Anzahl_VK,
               "staffInpatientCare" = .data$Versorgungsform_Stationaere_Versorgung_Anzahl_VK) %>%
        mutate(idPersonnelHospital = NA_integer_,
               Hospital_idHospital = Hospital_id) %>%
        select(.data$idPersonnelHospital, .data$occupationalGroup, everything())


    if (any(!PersonnelHospital[, -c(2, 8)] %>% mutate(across(everything(), is.na)))) {

        PersonnelHospital <- PersonnelHospital %>% filter(if_any(.cols = -c(2,8), ~ !is.na(.)))

        PersonnelHospital_exists <- TRUE

    } else {

        PersonnelHospital_exists <- FALSE

        PersonnelHospital <- NA_character_

    }

    AttendingDoctorsHospital <-
        qb_extract_simple_section(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Aerzte'
            ),
            "Belegaerzte",
            c(
                "Anzahl",
                "Fall_je_Anzahl",
                "Erlaeuterungen"
            )
        ) %>%
        mutate(idAttendingDoctorsHospital = NA_integer_,
               Hospital_idHospital = Hospital_id) %>%
        rename("quantity" = .data$Anzahl,
               "casesPerQuantity" = .data$Fall_je_Anzahl,
               "comment" = .data$Erlaeuterungen) %>%
        select(.data$idAttendingDoctorsHospital, everything()) %>%
        filter(!is.na(.data$quantity))

    if (nrow(AttendingDoctorsHospital) > 0) {

        AttendingDoctorsHospital_exists <- TRUE

    } else {

        AttendingDoctorsHospital_exists <- FALSE

        AttendingDoctorsHospital <- NA_character_

    }


    SpecialPersonnelHospital <- qb_extract_simple_section(
        xml_find_first(
            obj,
            '//Personal_des_Krankenhauses/Spezielles_Therapeutisches_Personal'
        ),
        "Therapeutisches_Personal",
        c(
            "SP_Schluessel",
            "Personalerfassung/Anzahl_VK",
            "Personalerfassung/Beschaeftigungsverhaeltnis/Personal_mit_direktem_BV/Anzahl_VK",
            "Personalerfassung/Beschaeftigungsverhaeltnis/Personal_ohne_direktem_BV/Anzahl_VK",
            "Personalerfassung/Versorgungsform/Ambulante_Versorgung/Anzahl_VK",
            "Personalerfassung/Versorgungsform/Stationaere_Versorgung/Anzahl_VK"
        )
    ) %>%
        rename("code" = .data$SP_Schluessel,
               "fulltimeStaff" = .data$Personalerfassung_Anzahl_VK,
               "staffWithDirectEmployment" = .data$Personalerfassung_Beschaeftigungsverhaeltnis_Personal_mit_direktem_BV_Anzahl_VK,
               "staffWithoutDirectEmployment" = .data$Personalerfassung_Beschaeftigungsverhaeltnis_Personal_ohne_direktem_BV_Anzahl_VK,
               "staffOutpatientCare" = .data$Personalerfassung_Versorgungsform_Ambulante_Versorgung_Anzahl_VK,
               "staffInpatientCare" = .data$Personalerfassung_Versorgungsform_Stationaere_Versorgung_Anzahl_VK) %>%
        mutate(idSpecialPersonnelHospital = NA_integer_,
               Hospital_idHospital = Hospital_id) %>%
        select(.data$idSpecialPersonnelHospital, .data$code, everything()) %>%
        filter(!is.na(.data$code))


    if (nrow(SpecialPersonnelHospital) > 0) {

        SpecialPersonnelHospital_exists <- TRUE

    } else {

        SpecialPersonnelHospital_exists <- FALSE

        SpecialPersonnelHospital <- NA_character_

    }


    tariffWeeklyWorkingHoursDoctors <-
        xml_text(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Aerzte/Aerzte_ohne_Belegaerzte/Massgebliche_tarifliche_Wochenarbeitszeit'
            )
        )
    tariffWeeklyWorkingHoursNurses <-
        xml_text(
            xml_find_first(
                obj,
                '//Personal_des_Krankenhauses/Pflegekraefte/Massgebliche_tarifliche_Wochenarbeitszeit'
            )
        )



    return(list("PersonnelHospital_exists" = PersonnelHospital_exists,
                "PersonnelHospital" = PersonnelHospital,
                "AttendingDoctorsHospital_exists" = AttendingDoctorsHospital_exists,
                "AttendingDoctorsHospital" = AttendingDoctorsHospital,
                "SpecialPersonnelHospital_exists" = SpecialPersonnelHospital_exists,
                "SpecialPersonnelHospital" = SpecialPersonnelHospital,
                "tariffWeeklyWorkingHoursDoctors" = tariffWeeklyWorkingHoursDoctors,
                "tariffWeeklyWorkingHoursNurses" = tariffWeeklyWorkingHoursNurses))

}


