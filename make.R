#!/usr/bin/env Rscript
h <- here::here
library(purrr)
library(future)

countries <- c(
  "Bangladesh",
  "China",
  "Egypt",
  "India",
  "Indonesia",
  "Ivory Coast",
  "Jordan",
  "Liberia",
  "Republic of Congo",
  "South Sudan",
  "Thailand"
)

reports <- map(countries, funcction(country) {
  safely(rmarkdown::render)("report-template.Rmd",
                            output_file = paste0(country, "-ma-report.html"),
                            output_dir = h("outputs"),
                            params = list(country = country))
})

errors <- map_lgl(reports, ~!is.null(.$error))

if (any(errors)) stop(paste("Error building", paste(counties[errors], collapse = ", ")))
