#!/usr/bin/env Rscript
library(eidith)
h <- here::here

# Download EIDITH database locally to "db" subdirectory
# (will download into the working directory but delete later)
if(file.exists(h("db", "eidith.sqlite")) &&
   file.exists(h("db", "last_dl_date.txt")) &&
   readLines(h("db", "last_dl_date.txt"), warn = FALSE)[1] == as.character(Sys.Date())) {

  cat("Database is current")
} else {
  ed_db_delete(path = normalizePath(file.path("db", "eidith.sqlite"))) # May throw a warning if no database is present
  ed_db_download(verbose = FALSE,
                 country = c("Vietnam", "Mongolia"),
                 p2_tables = c("Event", "Animal", "Specimen", "Test", "TestDataInterpreted"))
  cat(as.character(Sys.Date()), file = h("db", "last_dl_date.txt"))
}


# Download IUCN terrestrial mammal shapefiles to "data" subdirectory
if(file.exists(h("data", "TERRESTRIAL_MAMMALS", "TERRESTRIAL_MAMMALS.shp"))) {

  cat("IUCN terrestrial mammal shapefiles already downloaded")
} else {

  local.file <- h("data", "TERRESTRIAL_MAMMALS.zip")
  download.file("https://predict-ma-reports.s3.amazonaws.com/TERRESTRIAL_MAMMALS.zip", local.file)
  unzip(local.file, exdir = h("data", "TERRESTRIAL_MAMMALS"))
}

# Download .tif layers to "data/map-layers" subdirectory
for(file in c("gpw-v4-population-density_2015.tif", "summed_mammal_livestock.tif")) {

  local.file <- h("data", "map-layers", file)

  if(file.exists(local.file)) {

    cat(file, "already downloaded")
  } else {

    download.file(paste0("https://predict-ma-reports.s3.amazonaws.com/", file), local.file)
  }
}
