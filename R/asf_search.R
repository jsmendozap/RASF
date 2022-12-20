#' @title asf_search
#'
#' @description Retrieve a collection with available ASF products.
#'
#' @param data A sf object which represent de area where search the selected
#' ASF product.
#'
#' @param results Maximum number of results returned from search
#'
#' @param platform A string specifying the ASF product for search. Available
#' products are:
#'
#' - ALOS
#' - A3
#' - AIRSAR
#' - AS
#' - ERS
#' - ERS-1
#' - E1
#' - ERS-2
#' - E2
#' - JERS-1
#' - J1
#' - RADARSAT-1
#' - R1
#' - SEASAT
#' - SS
#' - S1
#' - Sentinel
#' - Sentinel-1
#' - Sentinel-1A
#' - SA
#' - Sentinel-1B
#' - SB
#' - SIR-C
#' - SMAP
#' - SP
#' - UAVSAR
#' - UA
#'
#' @param instrument Default NULL. For some platforms, such as ALOS, there are
#' multiple instruments to choose from.
#'
#' @param start Start Date of search
#'
#' @return A data frame with available products for the selected area.
#'
#' @example
#'
#'
#' @export
#' @importFrom dplyr "%>%"

asf_search <- function(data, results = 10, platform, instrument = NULL, start = NULL){

  bbox <- data %>%
    sf::st_transform(crs = 4326) %>%
    sf::st_bbox(data) %>%
    paste(collapse = ",")

  request <- stringr::str_glue('https://api.daac.asf.alaska.edu/services/search/param?',
                      'bbox={bbox}&platform={platform}&',
                      'maxResults={results}&output=csv', .trim = T)

  request <- ifelse(test = is.null(instrument),
                    yes = request,
                    no = request + stringr::str_glue('&instrument={instrument}'))

  request <- ifelse(test = is.null(start),
                    yes = request,
                    no = request + stringr::str_glue('&start={start}&end=today'))

  result <- httr::GET(url = request) %>%
    httr::content() %>%
    dplyr::rename(ymin = 'Far Start Lat', xmin = 'Far Start Lon',
                  ymax = 'Far End Lat', xmax = 'Far End Lon') %>%
    dplyr::mutate(geom = stringr::str_glue("POLYGON(({xmin} {ymin}, {xmax} {ymin}, {xmax} {ymax}, {xmin} {ymax}, {xmin} {ymin}))")) %>%
    sf::st_as_sf(wkt = "geom", crs = 4326) %>%
    mutate(id = row_number()) %>%
    dplyr::select(id, dplyr::contains('URL'), 'Acquisition Date', 'Processing Level', 'Size (MB)') %>%
    janitor::clean_names()

  return(result)
}
