library(tidyverse)
library(rjson)
## Scrape in the locations ----

## source: https://raw.githubusercontent.com/ptweir/ptweir.github.io/460899eb62ec7ee879f256fe2d5efe1a2306b25a/t3.html
## manually cut out the json bits. 

json_file <- "locations.json"

# that thing has missing quotes... not sure why, but let's fix em
json_string <- readChar(json_file, file.info(json_file)$size)

json_string %>%
  str_replace_all(c("center:" = '"center":',
                    "zoom:" = '"zoom":',
                    "pitch:" = '"pitch":',
                    "speed:" = '"speed":')) ->
  replaced_json

fileConn<-file("replaced_json.json")
writeLines(c(replaced_json), fileConn)
close(fileConn)

# this pulls the json into a big list
location_list <- fromJSON(file = "replaced_json.json")

# extract only the name and the lat long pairs ----



