#' @title plot_results
#'
#' @description Plot results from asf_search
#'
#' @param data A sf object which represent de area where search the selected
#' ASF product.
#'
#' @param results sf object obtained form asf_search
#'
#' @example
#'
#'
#' @export
#' @importFrom dplyr "%>%"

plot_results <- function(data, results){

  results <- results %>%
    dplyr::mutate(label = paste0("Id: ", id, "<br>",
                                 "Processing level: ", processing_level, "<br>",
                                 "Acquisition date: ", acquisition_date, "<br>",
                                 "Size (MB): ", size_mb))

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
