library(eidith)
library(tidyverse)
h <- here::here

e2 <- ed2_events() %>%
  mutate(country = ifelse(country %in% c("Malaysia, Peninsular", "Malaysia, Sabah"), "Malaysia", country)) %>%
  arrange(desc(integer_id)) %>%
  distinct(event_name, .keep_all = TRUE)
a2 <- ed2_animals() %>%
  select(-c(integer_id, gains4_event_id, gains4_sample_unit_id))
s2 <- ed2_specimens() %>%
  select(-c(integer_id, gains4_sample_unit_id))
t2 <- ed2_tests() %>%
  select(-c(integer_id))

t2 <- t2 %>%
  mutate(
    sequence = case_when(
      str_detect(sequence, "\\|") & !(str_detect(virus, "\\|")) ~
        str_replace(sequence, "\\|", ";"),
      str_detect(sequence, "\\|") & is.na(virus) ~
        str_replace(sequence, "\\|", ";"),
      TRUE ~ sequence
    )
  )

t2 <- separate_rows(t2, virus, sequence, sep = "\\|")

t2 <- t2 %>%
  mutate(
    virus = case_when(
      virus == "" ~ NA_character_,
      TRUE ~ virus
    ),
    viral_species = stringi::stri_replace_first_regex(virus, "(new\\s)?strain\\sof\\s", ""),
    viral_species = ifelse(str_detect(viral_species, "(OC43)"), "Betacoronavirus 1 (OC43)", viral_species),
    viral_species = ifelse(str_detect(viral_species, "MJ/67C"), "Hipposideros bat alphacoronavirus MJ/67C", viral_species),
    viral_species = ifelse(str_detect(viral_species, "(PPR)"), "Peste des petits ruminants (PPR)", viral_species),
    viral_species = ifelse(viral_species == "Human Coronavirus 229E (Human strain)", "Coronavirus 229E (Human strain)", viral_species),
    viral_species = str_replace(viral_species, "Bat Coronavirus", "Bat coronavirus"),
    viral_species = str_replace(viral_species, " Bat ", " bat "),
    virus_detected = ifelse(confirmation_result == "Positive", 1, 0)
  ) %>%
  left_join(., read_csv(h("data", "viral_species_modifications.csv")), by = "viral_species") %>%
  mutate(viral_species = ifelse(!is.na(viral_species_mod), viral_species_mod, viral_species)) %>%
  select(-viral_species_mod)

d2 <- left_join(e2, a2, by = c("event_name", "project")) %>%
  left_join(s2, by = c("animal_id" = "animal_human_id", "project")) %>%
  left_join(t2, by = c("specimen_id", "gains4_specimen_id", "project"))

d2 %>%
  filter(
    !is.na(viral_species),
    !is.na(date_govt_approved_release)
  ) %>%
  distinct(viral_species, country) %>%
  arrange(viral_species, country) %>%
  write_csv(., h("data", "viral_species_country_lookup.csv"))


# Generate lookup table showing viral testing summary data by viral species, host species, and site

# Modify host species names

d2 <- d2 %>%
  mutate(
    species_scientific_name = case_when(
      str_detect(species_scientific_name, " cf. ") ~
        str_replace(species_scientific_name, " cf. ", " "),
      TRUE ~ species_scientific_name
    )
  )

# Generate a table showing the number of animals detected with a given virus for each species at
# each site

viral_detections_by_species_and_site <- d2 %>%
  filter(
    !is.na(test_requested),
    !is.na(viral_species)
  ) %>%
  group_by(country, viral_species, species_scientific_name, animal_id,
           site_latitude, site_longitude) %>%
  summarize(virus_detected = ifelse(sum(virus_detected) > 0, 1, 0)) %>%
  group_by(country, viral_species, species_scientific_name,
           site_latitude, site_longitude) %>%
  summarize(n_animals_w_detections = sum(virus_detected)) %>%
  ungroup() %>%
  arrange(country, viral_species, species_scientific_name, site_latitude, site_longitude)

# Generate a table showing the viral test types that could result in detection of a given
# virus

virus_test_pairs <- d2 %>%
  distinct(viral_species, test_requested) %>%
  filter(!is.na(viral_species) & !is.na(test_requested)) %>%
  ungroup() %>%
  arrange(viral_species, test_requested)

# Generate a table showing the number of animals for which a given virus could have been
# detected for each species at each site

viral_testing_by_species_and_site <- d2 %>%
  distinct(country, species_scientific_name, animal_id,
           site_latitude, site_longitude, test_requested) %>%
  left_join(virus_test_pairs, ., by = "test_requested") %>%
  # roll up viral testing summary to animal level
  distinct(viral_species, country, species_scientific_name, animal_id,
           site_latitude, site_longitude) %>%
  # roll up viral testing summary to virus by country by species by site level
  group_by(viral_species, country, species_scientific_name, site_latitude, site_longitude) %>%
  summarize(n_animals_tested = n()) %>%
  ungroup() %>%
  arrange(country, viral_species, species_scientific_name, site_latitude, site_longitude)

left_join(viral_testing_by_species_and_site, viral_detections_by_species_and_site,
          by = c("viral_species", "country", "species_scientific_name",
                 "site_latitude", "site_longitude")) %>%
  mutate(n_animals_w_detections = ifelse(is.na(n_animals_w_detections), 0, n_animals_w_detections)) %>%
  mutate_at(.vars = c("site_latitude", "site_longitude"), ~as.numeric(.)) %>%
  write_csv(., h("data", "viral_species_testing_by_host_and_site.csv"))
