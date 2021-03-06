
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qbgbaReader <img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->
<!-- badges: end -->

The goal of qbgbaReader is to read XML files containing the German
“Qualitaetsberichte der Krankenhaeuser” obtained from the “Gemeinsamer
Bundesausschuss” (GBA) and insert them into a SQL-database.

## Installation

You can install the released version of qbgbaReader from
[Gitlab](https://gitlab.com/klinik-db/qbgbaReader) with:

``` r
devtools::install_gitlab("klinik-db/qbgbaReader")
```

## Usage

This package gives you the means to read in the raw data, i.e., the
XML-files obtained from the GBA. These are not included in the package,
but must be requested from the GBA. One year of data takes about 2.3GB
of disk space and contains nearly 8,000 files. The following code
assumes that you have these XML-files available.

### Loading packages

``` r
library(tidyverse)
library(xml2)
library(readxl)
library(qbgbaReader)
library(DBI)
library(RMariaDB)
library(progressr)
```

### Open a connection to the database

This assumes, that you have either a MySQL or a MariaDB server setup
with an administrative user account (that has permission to create, drop
and alter a schema). Using the odbc-package with the following syntax
gave weird problems regarding the encoding of special characters, at
least for us. With the odbc package, you can choose either the
[MySQL-Driver](https://dev.mysql.com/downloads/connector/odbc/) or the
[MariaDB connector](https://mariadb.com/downloads/#connectors) as the
driver.

``` r
# Setting up the credentials using the keyring-package:

keyring::key_set(service = "mysql-localhost",
                 username = "dataadmin")


# NOT working because of encoding errors with the German special characters:
con <- dbConnect(odbc::odbc(),
                 Driver   = "MariaDB ODBC 3.1 Driver",
                 Server   = "localhost",
                 Port     = 3306,
                 UID      = keyring::key_list("mysql-localhost")[1,2],
                 PWD      = keyring::key_get("mysql-localhost", "dataadmin"))
```

Because of the encoding difficulties, the RMariaDB-package is used for
the connection instead. This again bears the little inconvenience, that
the user credentials must be set up using the standard authentification
hashing algorithm (`mysql_native_password`) and **not** use the newer
`caching_sha2_password` algorithm, which is the default for MySQL Server
version 8 and later. For a discussion of this matter, see this [Github
issue](https://github.com/r-dbi/RMariaDB/issues/134).

``` r
# This works:
con <- dbConnect(RMariaDB::MariaDB(),
                 host     = "localhost",
                 port     = 3306,
                 username = keyring::key_list("mysql-localhost")[1,2],
                 password = keyring::key_get("mysql-localhost", "dataadmin"),
                 dbname   = "gbadata"
)
```

Then, we further need the list of XML-files with the full path names,
the XML schema file for the corresponding year (these can be obtained
from the [GBA
website](https://www.g-ba.de/themen/qualitaetssicherung/datenerhebung-zur-qualitaetssicherung/datenerhebung-qualitaetsbericht/servicedateien/)
together with the Excel files comprising the selective lists for the
respective years. For your convenience, schema and selective list files
are included in the source of this package (not in the already built and
installed package!!), look into directory `./data-raw`.

``` r
lists_2019 <-
  qb_extract_selective_lists(
    "./data-raw/SelectiveLists/2020-04-01_Anlage-4_Qb-R_Auswahllisten_BJ-2019.xlsx"
  )
XML_Schema_path_2019 <- "./data-raw/XML-SchemaFiles/2020-10-07_Anlage-5_XML_Schema-BJ-2019.xsd"

reports_detailed <- list.files("../2019_v2/Berichte-Teile-A-B-C/", 
                               pattern = "-xml\\.xml$", 
                               full.names = TRUE)
```

Next, we need the data set with the global identifier for each of the
hospitals. In this file, every hospital is assigned to one ID number
that is the same over several years, so that you can track the data for
one hospital over the years. The problem is acutally, that the ID listed
in the XML files (i.e., the “IK-number”) is not correct sometimes and
also might change over time. Therefore, the raw data cannot identify
each hospital correctly through time. Again, this file is included in
the directory `./data-raw` (i.e., in the source files), not in the built
and installed one!

``` r
GlobalHospitalID <- readxl::read_excel("./data-raw/GlobalHospitalID.xlsx",
                                       col_types = c(rep("text", times = 9),
                                                     "numeric",
                                                     "numeric"))

GlobalHospitalID <- GlobalHospitalID %>%
    mutate(idHospitalDataYear = case_when(
        year == "2015" ~ 1L,
        year == "2016" ~ 2L,
        year == "2017" ~ 3L,
        year == "2018" ~ 4L,
        year == "2019" ~ 5L
    ))
```

Now you can read in the complete year of data in one go, with activated
file logging, error catching and a nice progressbar:

``` r
results_2019_all <- with_progress(
    qb_extract_many_clinics(
        conn = con,
        xml_file_names = reports_detailed,
        xml_schema_path = XML_Schema_path_2019,
        global_hospital_id = GlobalHospitalID,
        years_lookuptable = c(
            "1" = 2015,
            "2" = 2016,
            "3" = 2017,
            "4" = 2018,
            "5" = 2019
        ),
        db_name = "gbadata",
        logging = TRUE
    )
)

# Get possible errors:
results_2019_all_df <- map_dfr(results_2019_all, `[`, .id = "number")
results_2019_all_df[results_2019_all_df$Type == "error", ]
save(results_2019_all_df, file = "results_2019_all_df.RData")

table(results_2019_all_df$Type)
```

Good Luck!

## Funding

<a href='https://klinik-db.de'><img src='man/figures/BMBF_eng.png' align="left" height="139" /></a>

Sponsored through the Prototype Fund by the German Federal Ministry of
Education and Research from March 2021 to August 2021.
