#' @title plot_results
#'
#' @description Plot results from asf_search.
#'
#' @param data A sf object which represent the area of interest.
#'
#' @param results A sf object obtained form asf_search.
#'
#' @return None. Function produces a plot
#'
#' @export
#'
#' @examples
#'
#' require(sf)
#'
#' polygon <- cbind(c(-75.63, -75.49, -75.49, -75.63, -75.63),
#'                 c(6.36, 6.36, 6.17, 6.17, 6.36)) %>%
#'  {st_polygon(list(.))} %>%
#'  st_sfc(crs = 4326)
#'
#' results <- asf_search(data = polygon, results = 20,
#'  platform = 'ALOS', instrument = 'PALSAR')
#'
#' plot_results(data = polygon, results = results)
#'
#' @importFrom dplyr "%>%"
#' @importFrom rlang .data

plot_results <- function(data, results){

  results <- results %>%
    dplyr::mutate(label = paste0("Id: ", .data$id, "<br>",
                                 "Processing level: ", .data$processing_level, "<br>",
                                 "Acquisition date: ", .data$acquisition_date, "<br>",
                                 "Size (MB): ", .data$size_mb))

  map <- leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addPolygons(data = data, group = 'data')

  for(i in 1:nrow(results)){
    map <- map %>%
      leaflet::addPolygons(data = results[i,], group = as.character(i),
                           popup = ~ label)
  }

  map %>%
    leaflet::addLayersControl(
      overlayGroups = c(1:nrow(results)),
      options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
    leaflet::hideGroup(as.character(c(2:nrow(results))))

}
