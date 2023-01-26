#' @title asf_download
#'
#' @description Download selected products from Alaska Satellite Facility platform.
#'
#' @param results A data frame retrieve from asf_search function.
#'
#' @param id Row number from selected product in data frame retrieved by
#'  asf_search function for download.
#'
#' @param user Username for Alaska Satellite Facility platform.
#'
#' @param password Key for Alaska Satellite Facility platform.
#'
#' @return None. Function downloads ASF product selected to path especified.
#'
#' @seealso https://search.asf.alaska.edu/
#'
#' @export
#'
#' @examples
#'
#'\dontrun{
#' library(sf)
#'
#' polygon <- cbind(c(-75.63, -75.49, -75.49, -75.63, -75.63),
#'                 c(6.36, 6.36, 6.17, 6.17, 6.36)) %>%
#'  {st_polygon(list(.))} %>%
#'  st_sfc(crs = 4326)
#'
#' results <- asf_search(data = polygon, results = 20,
#'  platform = 'ALOS', instrument = 'PALSAR')
#'
#' asf_download(results = results, id = 12,
#'  user = 'your_ASF_user', password = 'your_ASF_key')
#'}
#'
#' @importFrom dplyr "%>%"
#' @importFrom rlang .data

asf_download <- function(results, id, user, password, path = NULL){

  url <- results %>%
    dplyr::filter({{ id }} == id) %>%
    dplyr::pull(url)

  filepath <- ifelse(is.null(path),
                     file.path(getwd(), basename(url)),
                     file.path(path, basename(url)))

  httr::GET(url, httr::write_disk(path = filepath, overwrite = T),
            httr::progress(),
            httr::authenticate(user = user,
                               password = password))

}
