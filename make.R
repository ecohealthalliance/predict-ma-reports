#!/usr/bin/env Rscript
library(purrr)
library(dplyr)
h <- here::here

countries <- recode(eidith::eha_countries(), "Malaysia, Peninsular" =  "Malaysia", "Malaysia, Sabah" = "Malaysia") %>% unique()
animal_reports <- list()
human_reports <- list()

for(country in countries) {
  human_reports[[country]] <- safely(rmarkdown::render, quiet = FALSE)(
    "report-human-template.Rmd",
    output_file = paste0(country, "-ma-report-human.html"),
    output_dir = h("outputs"),
    params = list(country = country)
  )
  animal_reports[[country]] <- safely(rmarkdown::render, quiet = FALSE)(
    "report-template.Rmd",
    output_file = paste0(country, "-ma-report.html"),
    output_dir = h("outputs"),
    params = list(country = country)
  )
}

animal_errors <- map_lgl(animal_reports, ~!is.null(.$error))
if (any(animal_errors)) {
  walk(animal_reports[animal_errors], ~print(.$error))
  stop(paste("Error building", paste(countries[animal_errors], collapse = ", ")))
}

human_errors <- map_lgl(human_reports, ~!is.null(.$error))
if (any(human_errors)) {
  walk(human_reports[human_errors], ~print(.$error))
  stop(paste("Error building", paste(countries[human_errors], collapse = ", ")))
}
