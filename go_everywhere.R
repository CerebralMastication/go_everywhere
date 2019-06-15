library(tidyverse)
library(rjson)

library(ggmap)

## Scrape in the locations ----

## source: https://raw.githubusercontent.com/ptweir/ptweir.github.io/460899eb62ec7ee879f256fe2d5efe1a2306b25a/t3.html
## manually cut out the json bits.

json_file <- "locations.json"

# that thing has missing quotes... not sure why, but let's fix em
json_string <- readChar(json_file, file.info(json_file)$size)

json_string %>%
  str_replace_all(
    c(
      "center:" = '"center":',
      "zoom:" = '"zoom":',
      "pitch:" = '"pitch":',
      "speed:" = '"speed":'
    )
  ) ->
  replaced_json

fileConn <- file("replaced_json.json")
writeLines(c(replaced_json), fileConn)
close(fileConn)

# this pulls the json into a big list
location_list <- fromJSON(file = "replaced_json.json")

# extract only the name and the lat long pairs ----
get_vals <- function(input_item) {
  input_item$title  %>%
    str_replace_all(c("," = "")) %>%
    str_replace(" ", "") ->
    place_name
  
  tibble(
    place = place_name,
    lon = input_item$camera$center[[1]],
    lat = input_item$camera$center[[2]]
  ) ->
    output
  return(output)
}


map(location_list, get_vals) %>%
  bind_rows ->
  locations_df


register_google(key = Sys.getenv("GOOG_MAPS_API"))

# getting the map
mapgilbert <-
  get_map(
    location = c(
      lon = mean(locations_df$lon),
      lat = mean(locations_df$lat)
    ),
    zoom = 4,
    maptype = "satellite",
    scale = 2
  )

# plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(
    data = locations_df,
    aes(
      x = lon,
      y = lat,
      fill = "red",
      alpha = 0.8
    ),
    size = 5,
    shape = 21
  ) +
  guides(fill = FALSE, alpha = FALSE, size = FALSE)
