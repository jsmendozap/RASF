#' @title asf_download
#'
#' @description Downloads selected products from Alaska Satellite Facility
#'
#' @param results A data frame retrieve from asf_search function.
#'
#' @param id Row id for download product from data frame retrieved by
#'  asf_search function.
#'
#' @param username user for Alaska Satellite Facility platform.
#'
#' @param password key for Alaska Satellite Facility platform.
#'
#' @return A data frame with available products for the selected area.
#'
#' @example
#'
#'
#' @export
#' @importFrom dplyr "%>%"

asf_download <- function(results, id, user, password){

  results %>%
    dplyr::filter({{ id }} == id) %>%
    dplyr::pull(url) %>%
    httr::GET(httr::write_disk(path = basename(.), overwrite = T),
              httr::progress(),
              httr::authenticate(user = user,
                                 password = password))
}
