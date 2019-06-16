library(tidyverse)
library(rjson)

library(ggmap)
library(gmapsdistance)
library(TSP)

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
    zoom = 2,
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

### routing work  ----

# got great ideas from here https://datawookie.netlify.com/blog/2018/05/travelling-salesman-with-ggmap/

# function to get driving distance
drive_dist <- function(origin_lat,
                       origin_lon,
                       dest_lat,
                       dest_lon) {
  x <- paste0(origin_lat, "+", origin_lon)
  y <- paste0(dest_lat, "+", dest_lon)
  gmapsdistance(origin = x,
                destination = y,
                mode = "driving")
}

locations_df %>% 
  expand(place, place) %>%
  inner_join(locations_df, by = "place") %>%
  inner_join(locations_df, by = c("place1" = "place")) %>%
  rename(
    lon = lon.x,
    lat = lat.x,
    lon1 = lon.y,
    lat1 = lat.y
  ) ->
  dist_pairs

## gmapsdistance uses it's own API setting
set.api.key(Sys.getenv("GOOG_MAPS_API"))

drive_dist_df <- function(df) {
  drive_dist(df$lat,
             df$lon,
             df$lat1,
             df$lon1) ->
    drive_matrix
  return(as.data.frame(drive_matrix))
}

out_list <- list()
for (i in 1:nrow(dist_pairs)) {
  out <- drive_dist_df(dist_pairs[i, ])
  out <- cbind(dist_pairs[i, ], out)
  out_list[[i]] <- out
  write_csv(out, "distance_pairs.csv", append = TRUE)
  print(paste("Completed iteration", i))
}

