#!/usr/bin/env Rscript
library(purrr)
h <- here::here

countries <- eidith::eha_countries()[-c(10, 11)]
countries <- c(countries, "Malaysia")
reports <- list()

for(country in countries) {

  reports[[country]] <- safely(rmarkdown::render, quiet = FALSE)(
    "report-template.Rmd",
    output_file = paste0(country, "-ma-report.html"),
    output_dir = h("outputs"),
    params = list(country = country)
  )
}

errors <- map_lgl(reports, ~!is.null(.$error))

if (any(errors)) {

  walk(reports[errors], ~print(.$error))
  stop(paste("Error building", paste(countries[errors], collapse = ", ")))
}
