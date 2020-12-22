library(eidith)
library(tidyverse)
h <- here::here

d2 <- read_rds(h("data", "animal.rds"))

d2 %>%
  filter(
    !is.na(viral_species),
    !is.na(date_govt_approved_release)
  ) %>%
  distinct(viral_species, country) %>%
  arrange(viral_species, country) %>%
  write_csv(., h("data", "viral_species_country_lookup.csv"))

# Generate lookup table showing viral testing summary data by viral species, host species, and site

# Generate a table showing the number of animals detected with a given virus for each species at
# each site

viral_detections_by_species_and_site <- d2 %>%
  filter(
    !is.na(test_requested),
    !is.na(viral_species)
  ) %>%
  group_by(country, viral_species, scientific_name, gains4_sample_unit_id,
           site_latitude, site_longitude) %>%
  summarize(virus_detected = ifelse(sum(virus_detected) > 0, 1, 0)) %>%
  group_by(country, viral_species, scientific_name,
           site_latitude, site_longitude) %>%
  summarize(n_animals_w_detections = sum(virus_detected)) %>%
  ungroup() %>%
  arrange(country, viral_species, scientific_name, site_latitude, site_longitude)

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
  distinct(country, site_name, scientific_name, animal_id,
           site_latitude, site_longitude, test_requested) %>%
  left_join(virus_test_pairs, ., by = "test_requested") %>%
  # roll up viral testing summary to animal level
  distinct(viral_species, country, site_name, scientific_name, animal_id,
           site_latitude, site_longitude) %>%
  # roll up viral testing summary to virus by country by species by site level
  group_by(viral_species, country, site_name, scientific_name, site_latitude, site_longitude) %>%
  summarize(n_animals_tested = n()) %>%
  ungroup() %>%
  arrange(country, viral_species, scientific_name, site_latitude, site_longitude)

left_join(viral_testing_by_species_and_site, viral_detections_by_species_and_site,
          by = c("viral_species", "country", "scientific_name",
                 "site_latitude", "site_longitude")) %>%
  mutate(n_animals_w_detections = ifelse(is.na(n_animals_w_detections), 0, n_animals_w_detections)) %>%
  mutate_at(.vars = c("site_latitude", "site_longitude"), ~as.numeric(.)) %>%
  write_csv(., h("data", "viral_species_testing_by_host_and_site.csv"))


# Generate a table showing all P2 mammalian hosts of viruses

# "species" to exclude from P2 associations

species.to.exclude <- c("Chiroptera", "Rhinopomatidae", "Rodentia")

p2.associations <- d2 %>%
  distinct(country, viral_species, class, scientific_name) %>%
  filter(
    !is.na(viral_species),
    class == "Mammalia",
    !str_detect(scientific_name, " sp."),
    !(scientific_name %in% species.to.exclude)
  ) %>%
  select(-class) %>%
  write_csv(., h("data", "P2_virus_mammal_host_associations.csv"))
