library(leaflet)
library(sf)
library(rnaturalearth)

# Sri Lanka polygon
sri_lanka <- ne_countries(country = "Sri Lanka", returnclass = "sf")

# Centroid (convert to plain coordinates properly)
centroid <- st_centroid(sri_lanka)
coords <- st_coordinates(centroid)

leaflet() |>
  addProviderTiles("CartoDB.Positron") |>
  addPolygons(
    data = sri_lanka,
    fillColor = "red",
    fillOpacity = 0.6,
    color = "black",
    weight = 2
  ) |>
  addLabelOnlyMarkers(
    lng = coords[1],
    lat = coords[2],
    label = "Sri Lanka",
    labelOptions = labelOptions(
      noHide = TRUE,
      direction = "center",
      textOnly = TRUE,
      style = list(
        "color" = "black",
        "font-size" = "16px",
        "font-weight" = "bold"
      )
    )
  )