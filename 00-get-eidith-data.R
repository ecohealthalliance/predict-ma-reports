#!/usr/bin/env Rscript
library(eidith)
h <- here::here

# Set EIDITH path options
# (will download into the working directory but delete later)
if(file.exists(h("db", "eidith.sqlite")) &&
   file.exists(h("db", "last_dl_date.txt")) &&
   readLines(h("db", "last_dl_date.txt"), warn = FALSE)[1] == as.character(Sys.Date())) {
  cat("Database is current")
} else {
  ed_db_download(verbose = TRUE)
  cat(as.character(Sys.Date()), file = h("db", "last_dl_date.txt"))
}
