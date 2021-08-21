#' The list of possible "Fachabteilungen" in German hospitals defined in 2019.
#'
#' This dataset contains information about all possible departments in German
#' hospitals defined in the § 301 of the "Sozialgesetzbuch V" (SGB V) in 2003,
#' with the currently up-to-date version from 2019.
#'
#' @format A \code{tibble} with 148 rows and three columns:
#' \describe{
#'   \item{Fachabteilungsschluessel}{String values with the key of the department,
#'         e.g., "0100"}
#'   \item{Schwerpunkt_ja_nein}{String value of either "ja" or "nein" depending
#'         on if the department is listed as specialized or not, respectively.}
#'   \item{Bezeichnung}{String value with the name of the department, e.g.,
#'         "Innere Medizin"}
#' }
#'
#' @source See page 122 of this PDF document provided by the "Deutsche
#'     Krankenhausgesellschaft" (DKG): \cr
#'     \href{https://www.dkgev.de/fileadmin/default/Mediapool/2_Themen/2.1_Digitalisierung_Daten/2.1.3._Elektronische_Datenuebermittlung/2.1.3.1._Datenuebermittlung_zu_Abrechnungszwecken/01_GKV/01_Gesamtdokumentation/Gesamtdokumentation_SGBV_301_2019-12-06.pdf}{Datenübermittlung nach § 301 SGB V, Dokumentation: Stand Dezember 2019}
#'
#'
"Fachabteilungen_2019"
