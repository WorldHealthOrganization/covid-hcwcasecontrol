plot_sites <- function(location.df, zoom){
  leaflet() %>%
    setView(lng = location.df$long[1], lat =  location.df$lat[1], zoom = zoom) %>%
    addTiles() %>%
    addMarkers(lng = location.df$long, lat = location.df$lat)
}