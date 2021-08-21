#' Extract repeated XML-children from a section together with its values.
#'
#' @param obj An XML-nodeset that contains several similar children which in turn
#'     contain several variables that one seek to extract. More than this level
#'     of nested structures is not supported.
#'
#' @param entrypoint A single string value that specifies the name of the
#'     children elements.
#'
#' @param subelements A string vector with the names of the variables that should
#'     be extracted which are then returned as columns in the resulting \code{tibble}.
#'     Here, one can also specify nested elements, i.e., elements that are on a
#'     deeper level than the other subelements. In this way, more complicated
#'     structures with different nesting levels can be extracted into a plain
#'     table structure.
#'
#' @param message_missing Boolean value to indicate if a message should be printed
#'     in case a missing obj is found and a dummy tibble with all specified variables
#'     from \code{subelements} as \code{NA_character_}. Defaults to \code{FALSE}, i.e.,
#'     no message is printed.
#'
#'
#' @return A \code{tibble} with as many character columns as the length of the
#'     parameter \code{subelements}.
#'
#'
#' @examples
#'
#' \dontrun{
#' doc_path <- system.file("extdata", "260100023-01-2019-xml.xml",
#'                         package = "qbgbaReader", mustWork = TRUE)
#' doc <- xml2::read_xml(doc_path)
#'
#'
#' x <- xml2::xml_find_all(doc, '//Einleitung/Weiterfuehrende_Links')
#' qb_extract_simple_section(x, "Link", c("URL", "Beschreibung"))
#'
#' x <- xml2::xml_find_all(doc, '//Akademische_Lehre')
#' qb_extract_simple_section(x, "Akademische_Lehre_Wissenschaftliche_Taetigkeit",
#'                           c("FL_Schluessel", "Erlaeuterungen"))
#'
#' x <- xml2::xml_find_all(doc, '//Ausbildung_in_anderen_Heilberufen')
#' qb_extract_simple_section(x, "Ausbildung_in_anderen_Heilberufen",
#'                           c("HB_Schluessel", "Erlaeuterungen"))
#'
#' x <- xml2::xml_find_all(doc, '//Barrierefreiheit')
#' qb_extract_simple_section(x, "Barrierefreiheit_Aspekt",
#'                           c("BF_Schluessel", "Erlaeuterungen"))
#'
#' x <- xml2::xml_find_first(doc, '//Medizinische_Leistungsangebote')
#' qb_extract_simple_section(x, "Medizinisches_Leistungsangebot",
#'                           c("VA_VU_Schluessel", "Erlaeuterungen",
#'                           "Sonstiger/VA_VU_Sonstiger_Schluessel",
#'                           "Sonstiger/Bezeichnung"))
#' }
#'
#' @export
#'
qb_extract_simple_section <- function(obj,
                                      entrypoint = NA_character_,
                                      subelements = NA_character_,
                                      message_missing = FALSE) {

    # obj <- xml_find_first(obj, './Ambulante_Behandlungsmoeglichkeiten')
    # entrypoint <- "Ambulante_Behandlungsmoeglichkeit/Ambulanz_116b"
    # subelements <- c("AM_116b_Schluessel",
    #                  "Bezeichnung",
    #                  "Leistungen_Ambulanz_116b/Leistung/LK_Schluessel")

    ### tests

    if (class(obj) == "xml_missing") {

        cl <- match.call()
        input_obj <- as.character(cl[match("obj", names(cl))])

        if (message_missing) {

            message('\nInput to argument "obj" is of class "xml_missing":\n\n', input_obj, '\n\nA dummy tibble with NAs is returned.')

        }


        pseudo_results <- vector(mode = "character", length = length(subelements))

        for (i in seq_along(pseudo_results)) {

            pseudo_results[[i]] <- paste0('"', str_replace_all(subelements[[i]], '(\\/)|\\.', '_'), '" = NA_character_')

        }

        pseudo_result_section <- paste0('df_tmp <- tibble(', paste0(pseudo_results, collapse = ',\n'), ")\n")
        eval(parse(text = pseudo_result_section, encoding = "UTF-8"))

        return(df_tmp)

    }


    if (!class(obj) %in% c("xml_node", "xml_nodeset")) {

        stop('"obj" must be either an object of class "xml_node" or "xml_nodeset"!')

    }

    if (!is.character(entrypoint)) {

        stop('Argument "entrypoint" must be of type character!')

    }

    if (length(entrypoint) != 1) {

        stop('Argument "entrypoint" must be of length one!')

    }

    if (!is.character(subelements)) {

        stop('Argument "subelements" must be of type character!')

    }

    if (length(subelements) < 1) {

        stop('Argument "subelements" must be at least of length one!')

    }

    ### End of tests


    docs <- xml_find_all(obj, paste0('./', entrypoint))

    # permitted_elements <- c("', entrypoint, '")
    #
    #
    # NAME <- trimws(xml_name(obj))
    #
    # if (!NAME %in% permitted_elements) {
    #
    #     stop("This is an ", NAME, " XML-element which is not applicable.")
    #
    # }

    start <- paste0('single_function <- function(obj) {

        ### tests

        if (((class(obj) == "xml_nodeset") && length(obj) > 1) ||
            ((class(obj) == "xml_node") && length(obj) > 2)) {

            stop("Function \\"", match.call()[[1]], "\\" is not vectorized!")

        }

        ### End of tests

        ')

    extractors <- results <- vector(mode = "character", length = length(subelements))
    for (i in seq_along(extractors)) {

        extractors[[i]] <- paste0(str_replace_all(subelements[[i]], '\\/', '_'),
                                  ' <- xml_text(xml_find_first(obj, "./', subelements[[i]], '"))')
        results[[i]] <- paste0('"', str_replace_all(subelements[[i]], '\\/', '_'),
                               '"', ' = trimws(str_replace_all(',
                               str_replace_all(subelements[[i]], '\\/', '_'),
                               ', "[ ]{2,}", " "))')

    }

    extractor_section <- paste0(paste0(extractors, collapse = "\n"), "\n")

    result_section <- paste0("df_tmp <- tibble(", paste0(results, collapse = ",\n"), ")\n")

    end <- paste0('        return(df_tmp)

    }')

    single_function_text <- paste0(start, extractor_section, result_section, end, collapse = "\n")

    single_function <- eval(parse(text = single_function_text, encoding = "UTF-8"))

    df_tmp <- map_dfr(.x = docs, ~ single_function(.x))

    if (nrow(df_tmp) == 0) {

        pseudo_results <- vector(mode = "character", length = length(subelements))

        for (i in seq_along(pseudo_results)) {

            pseudo_results[[i]] <- paste0('"', str_replace_all(subelements[[i]], '(\\/)|\\.', '_'), '" = NA_character_')

        }

        pseudo_result_section <- paste0('df_tmp <- tibble(', paste0(pseudo_results, collapse = ',\n'), ")\n")
        eval(parse(text = pseudo_result_section, encoding = "UTF-8"))


    }

    return(df_tmp)

}
