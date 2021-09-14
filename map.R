library(tidyverse)
library(sf)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(webshot)
if (!file.exists("bin/phantomjs")) webshot::install_phantomjs()
dir.create("assets", showWarnings = FALSE)

# https://www.nps.gov/im/imd-gis.htm
# https://irma.nps.gov/DataStore/Reference/Profile/2224545?lnv=True (download and place in 'assets')


mapbox_light_template <-
  "https://api.mapbox.com/styles/v1/mapbox/light-v9/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoibHphY2htYW5uIiwiYSI6ImNpcW1oODczZTAwcjBnc2pmaGRhYjVudHIifQ.LeGAHvHXv36-vorTmuNtSg"


parks_raw <- st_read("assets/nps_boundary/nps_boundary.shp") # all NPS units

url <- "https://irmaservices.nps.gov/arcgis/rest/services/NPSData/NPS_MonitoringNetworks/MapServer/0/query?where=1%3D1&outFields=*&returnGeometry=true&f=geojson"
# url <- "https://irmaservices.nps.gov/arcgis/rest/services/NPSData/NPS_MonitoringNetworks/MapServer/0/query?where=1%3D1&outFields=*&returnGeometry=true&f=geojson"
networks_raw <- st_read(url) # all 32 I&M Networks

networks <- networks_raw %>%
  filter(NetworkCode %in% c("SOPN", "SCPN", "NCPN", "SODN", "CHDN", "ROMN"))
parks <- st_transform(parks_raw, st_crs(networks_raw)) %>%
  st_intersection(networks)

which_parks <- c("Little Bighorn Battlefield", "Organ Pipe Cactus", "Capitol Reef")
park_ex <- parks %>%
  filter(PARKNAME %in% which_parks) %>%
  group_by(UNIT_CODE) %>%
  slice(1) %>%
  ungroup()
park_ex_centers <- st_centroid(park_ex) # %>%
# mutate(NetworkLabel = trimws(sub("Network", "", NetworkName)),
#        NetworkLabel = str_wrap(NetworkLabel, 20),
#        NetworkLabel = sub("\n", "<br/>", NetworkLabel))
park_non_ex <- parks %>%
  filter(!PARKNAME %in% which_parks) %>%
  group_by(UNIT_CODE) %>%
  slice(1) %>%
  ungroup()
park_non_ex_centers <- st_centroid(park_non_ex)

network_centers <- st_centroid(networks) %>%
  mutate(
    NetworkLabel = trimws(sub("Network", "", NetworkName)),
    NetworkLabel = str_wrap(NetworkLabel, 10),
    NetworkLabel = gsub("\n", "<br/>", toupper(NetworkLabel))
  )
network_centers$NetworkLabel
css_network <- list(
  "color" = "black",
  "text-shadow" = "2px 2px #ffffff",
  "font-size" = "30px",
  "text-align" = "center",
  "padding" = "2px"
)
(m1 <- leaflet(networks,
  options = leafletOptions(zoomControl = FALSE, minZoom = 6, maxZoom = 6)
) %>%
  addTiles(urlTemplate = mapbox_light_template) %>%
  addPolygons(
    stroke = TRUE, color = "slategrey", weight = 3, opacity = 1,
    fillColor = "slategrey", fillOpacity = 0.25 / 2
  ) %>%
  addCircleMarkers(
    data = park_ex_centers,
    weight = 2,
    col = "black",
    fillColor = "black",
    radius = 8,
    fillOpacity = 0.9,
    stroke = T
  ) %>%
  addCircleMarkers(
    data = park_non_ex_centers,
    weight = 2,
    col = "black",
    fillColor = "white",
    radius = 6,
    fillOpacity = 0.9,
    stroke = T
  ) %>%
  addLabelOnlyMarkers(
    data = park_ex_centers,
    label = ~PARKNAME,
    labelOptions = lapply(c("top", "top", "bottom"), function(x) {
      labelOptions(textOnly = F, opacity = 0.85, noHide = TRUE, direction = x, textsize = "24px")
    })
    # labelOptions = labelOptions(
    #   textOnly = F, opacity=0.85, noHide=TRUE, direction = "top", textsize = "24px")
  ) %>%
  addLabelOnlyMarkers(
    data = network_centers,
    # label = ~HTML(NetworkLabel),
    label = lapply(network_centers$NetworkLabel, htmltools::HTML),
    labelOptions = mapply(function(hjust, vjust) {
      list(labelOptions(
        textOnly = T, opacity = 1, noHide = TRUE, direction = "top",
        offset = c(hjust, vjust),
        style = css_network
      ))
    }, hjust = c(-40, 0, 0, 0, -110, 0), vjust = c(40, 0, 60, 30, -110, 70))
    # labelOptions = labelOptions(
    #   textOnly = T, opacity=1, noHide=TRUE, direction = "top",
    #   offset = c(0, 60),
    #   style = css_network)
  ) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(maxWidth = 200)) %>%
  addMiniMap(
    tiles = mapbox_light_template,
    aimingRectOptions = list(color = "slategrey"),
    position = "topright",
    width = 300, height = 300,
    zoomLevelOffset = -3,
    toggleDisplay = FALSE
  ))
network_centers$NetworkLabel
# m1
html_file <- file.path(getwd(), "assets", "overview.html")
saveWidget(m1, html_file, selfcontained = FALSE)
webshot(html_file,
  file = file.path(dirname(html_file), "network-overview.jpg"),
  vwidth = 1063, vheight = 1063 * 1.25
)
