#' @title asf_search
#'
#' @description Retrieve a collection with available ASF products.
#'
#' @param data A sf object which represent de area where search the selected
#' ASF product.
#'
#' @param results An integer, specifying the maximum number of results returned
#'  from search.
#'
#' @param platform A string specifying the ASF product for search. Available
#' products are:
#'
#' ALOS, JERS-1, RADARSAT-1, Sentinel, Sentinel-1, Sentinel-1A, Sentinel-1B, SMAP.
#'
#' @param instrument Default NULL. For some platforms, such as ALOS, there are
#' multiple instruments to choose from.
#'
#' @param start Start Date of search. Default NULL.
#'
#' @return A data frame with available products for the selected area.
#'
#' @export
#'
#' @examples
#'
#' require(sf)
#' require(readr)
#'
#' polygon <- cbind(c(-75.63, -75.49, -75.49, -75.63, -75.63),
#'                 c(6.36, 6.36, 6.17, 6.17, 6.36)) %>%
#'  {st_polygon(list(.))} %>%
#'  st_sfc(crs = 4326)
#'
#' asf_search(data = polygon, results = 5,
#'            platform = 'ALOS', instrument = 'PALSAR')
#'
#' @seealso https://docs.asf.alaska.edu/api/keywords/
#'
#' @importFrom dplyr "%>%"
#' @importFrom rlang .data

asf_search <- function(data, results = 25, platform, instrument = NULL, start = NULL){

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
    dplyr::rename(y1 = 'Far End Lat', x1 = 'Far End Lon',
                  y2 = 'Near End Lat', x2 = 'Near End Lon',
                  y3 = 'Near Start Lat', x3 = 'Near Start Lon',
                  y4 = 'Far Start Lat', x4 = 'Far Start Lon') %>%
    dplyr::mutate(geom = stringr::str_glue("POLYGON (({x1} {y1}, {x2} {y2}, {x3} {y3}, {x4} {y4}, {x1} {y1}))")) %>%
    sf::st_as_sf(wkt = "geom", crs = 4326) %>%
    dplyr::mutate(id = dplyr::row_number()) %>%
    dplyr::select(.data$id, dplyr::contains('URL'), 'Acquisition Date', 'Processing Level', 'Size (MB)') %>%
    janitor::clean_names()

  return(result)
}
