library(sf)
h <- here::here

# Establish countries of interest

eha.countries <- eidith::eha_countries()[-c(10, 11)]

# Import all terrestrial mammal shapefiles from IUCN

mammals <- st_read(h("data", "TERRESTRIAL_MAMMALS"))

mammals <- mammals %>%
  left_join(., read_csv(h("data", "iucn_to_hp3_taxonomic_harmonization.csv")),
            by = c("binomial" = "IUCN_name")) %>%
  mutate(binomial = ifelse(!is.na(hp3_zoo_prediction_name), hp3_zoo_prediction_name, binomial))

# Import all country border information, subsetting down to countries of interest

borders <- st_read(h("data", "TM_WORLD_BORDERS-0.3")) %>%
  mutate(
    country = case_when(
      NAME == "Cote d'Ivoire" ~ "Ivory Coast",
      NAME == "Congo" ~ "Republic of Congo",
      NAME == "Sudan" ~ "South Sudan",
      TRUE ~ as.character(NAME)
    )
  ) %>%
  filter(country %in% eha.countries)

# Do a spatial join across the country borders and mammals

joined <- st_join(borders, mammals)

# Generate a lookup table

lookup_table <- joined %>%
  st_drop_geometry() %>%
  distinct(country, binomial) %>%
  mutate(binomial = as.character(binomial)) %>%
  arrange(country, binomial)

taxa.to.eliminate <- c(
  "Alces alces", "Axis axis", "Bos gaurus", "Bos javanicus", "Bos mutus"
)

lookup_table <- filter(lookup_table, !(binomial %in% taxa.to.eliminate))

# Verify all taxa in the lookup table will match with the HP3 missing zoonoses data

mz <- read_csv(h("data", "hp3_zoo_predictions.csv")) %>%
  mutate(species = str_replace(species, "_", " ")) %>%
  mutate_if(is.numeric, round, 2) %>%
  filter(species != "Pteropus pelewensis ssp. yapensis") %>%
  left_join(., read_csv(h("data", "hp3_taxonomy_harmonization.csv")), by = "species") %>%
  mutate(species =
           ifelse(!is.na(species_alt),
                  paste0(species, "|", species_alt),
                  species)
  ) %>%
  separate_rows(species, sep = "\\|") %>%
  select(-species_alt) %>%
  arrange(desc(pred_mean))

sort(unique(lookup_table$binomial))[!(sort(unique(lookup_table$binomial)) %in% unique(mz$species))]

# Write lookup table to a CSV file

write_csv(lookup_table, h("data", "mammal_country_lookup.csv"))
