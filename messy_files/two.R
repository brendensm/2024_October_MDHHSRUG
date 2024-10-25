library(rvest)

addy_list <- data.frame()

for (page in 70:74) {
  link <- paste0("https://harmreductionmi.org/new-page-", page)

  page1 <- read_html(link)

  address <- page1 |>
    html_nodes("p") |>
    html_text()
  if (page == 74) {
    address <- address[-c(1, length(address))]
  } else if (page == 71 | page == 72 | page == 73) {
    address <- address[-c(length(address))]
  }
  addy_list <- rbind(addy_list, data.frame(address))
}


a <- addy_list |> dplyr::filter(address != "")

a2 <- a |> geocode(address = address, method = "osm")


library(sf)
library(tigris)

directory <- sf::st_read("narcan_directory.kml")


mi_counties <- counties(state = "MI", cb = FALSE, class = "sf")

mi_counties <- st_transform(mi_counties, st_crs(directory))

points_with_counties <- st_join(directory, mi_counties, join = st_within)

library(ggplot2)


points_with_counties |> ggplot() +
  geom_sf(data = mi_counties) +
  geom_sf()
