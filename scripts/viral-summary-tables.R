library(openxlsx)
library(eidith)
library(tidyverse)
h <- here::here

d2 <- read_rds(h("data", "animal.rds")) %>%
  as_tibble()

animal_virus_summary <- d2 %>%
  filter(!is.na(viral_species)) %>%
  group_by(country, viral_species) %>%
  summarize(n = n_distinct(specimen_id)) %>%
  ungroup() %>%
  filter(n>=30) %>%
  filter(country %in% eha_countries())


for(report_country in unique(animal_virus_summary$country)){
  animal_virus_summary2 <- animal_virus_summary %>% filter(country == !!report_country)
  wb <- createWorkbook()
  for(virus in unique(animal_virus_summary2$viral_species)){

    sheetname <- substr(virus, 1, 30)
    addWorksheet(wb, sheetName = sheetname)

  # which specimen had detects
  viral_detects <- d2 %>%
    filter(country == !!report_country) %>%
    filter(viral_species == !!virus) %>%
    distinct(specimen_id, scientific_name, test_requested_protocol)

  d2_lgl <- d2 %>%
    filter(country == !!report_country,
           scientific_name %in% unique(viral_detects$scientific_name), # relevant species
           test_requested_protocol %in% unique(viral_detects$test_requested_protocol)) %>%  # relevant lab tests
    drop_na(scientific_name) %>%
    distinct(specimen_id, scientific_name, concurrent_sampling_site, age_class, sex, specimen_type) %>%
    mutate(!!virus := specimen_id %in% unique(viral_detects$specimen_id)) %>%
    ed2_expand_wide(scientific_name, clean_names = F) %>%
    ed2_expand_wide(concurrent_sampling_site, clean_names = F) %>%
    ed2_expand_wide(age_class, clean_names = F) %>%
    ed2_expand_wide(sex, clean_names = F) %>%
    ed2_expand_wide(specimen_type, clean_names = F) %>%
    select(-scientific_name, -concurrent_sampling_site, -age_class, -sex, -specimen_type, -specimen_id)

var_counts <- d2_lgl %>%
    summarize_all(~sum(.)) %>%
    gather(value = "Total samples")

  var_counts_pos <- d2_lgl %>%
    filter(!!as.name(virus)==TRUE) %>%
    gather() %>%
    group_by(key) %>%
    summarize(`Positive samples` = sum(value)) %>%
    ungroup() %>%
    left_join(var_counts)  %>%
    filter(key != virus) %>%
    mutate(key =  str_remove_all(key, "scientific_name_|concurrent_sampling_site_")) %>%
    mutate(key = str_replace_all(key, "_", " ")) %>%
    filter(!str_detect(key, "unknown|N/A")) %>%
    mutate("Percent positive" = paste0(round(100*`Positive samples`/`Total samples`), "%")) %>%
    rename(" " = key)

  virus_rate <- d2_lgl %>%
    pull(!!virus) %>%
    sum(.)

  title <- paste0(str_replace_all(virus, "_", " "), " viral detections (", virus_rate, " total detections of ", nrow(d2_lgl), " samples)")

  writeData(wb, sheet = sheetname, x = title, startRow = 1, startCol = 1)

  writeDataTable(wb, sheet = sheetname, x = var_counts_pos, startRow = 2,
                 startCol = 1, tableStyle = "TableStyleMedium2", withFilter = FALSE)

  saveWorkbook( wb, file = h("outputs", paste0(report_country, "_viral_summary.xlsx")),  overwrite = TRUE )
  }
}



