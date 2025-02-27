base_map <- function() {
  leaflet(options = leafletOptions(
    preferCanvas = TRUE,
    zoomControl = FALSE,
    attributionControl = FALSE
  )) %>%
    addProviderTiles(
      providers$CartoDB.DarkMatter,
      options = providerTileOptions(
        updateWhenZooming = FALSE,
        updateWhenIdle = FALSE
      )
    ) %>%
    setView(lng = -74.2973, lat = 4.5709, zoom = 5) %>%
    addScaleBar(position = "bottomleft")
}

update_map <- function(map_id, data) {
  leafletProxy(map_id, data = data) %>%
    clearMarkers() %>%
    clearMarkerClusters() %>%
    addMarkers(
      lng = ~LONGITUD,
      lat = ~LATITUD,
      clusterOptions = markerClusterOptions(
        showCoverageOnHover = FALSE,
        spiderfyOnMaxZoom = TRUE,
        zoomToBoundsOnClick = TRUE,
        disableClusteringAtZoom = 14,
        spiderLegPolylineOptions = list(weight = 1.5, color = "#00BFC4", opacity = 0.5)
      ),
      icon = makeIcon(
        iconUrl = "https://raw.githubusercontent.com/pointhi/leaflet-color-markers/master/img/marker-icon-blue.png",
        iconWidth = 25, iconHeight = 41
      )
    )
}