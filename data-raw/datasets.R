doc_path_bund <- system.file("extdata", "260100023-01-2019-bund.xml",
                             package = "qbgbaReader", mustWork = TRUE)
bund <- xml2::read_xml(doc_path_bund)

usethis::use_data(bund, overwrite = TRUE)


doc_path_land <- system.file("extdata", "260100023-01-2019-Land.xml",
                             package = "qbgbaReader", mustWork = TRUE)
land <- xml2::read_xml(doc_path_land)

usethis::use_data(land, overwrite = TRUE)


doc_path_details <- system.file("extdata", "260100023-01-2019-xml.xml",
                             package = "qbgbaReader", mustWork = TRUE)
details <- xml2::read_xml(doc_path_details)

usethis::use_data(details, overwrite = TRUE)
