library(sf)
library(tidyverse)
h <- here::here

# Establish countries of interest

#eha.countries <- c(eidith::eha_countries()[-c(10, 11)], "Malaysia")
countries <- c(eidith::eha_countries()[-c(10, 11)], "Malaysia")

# Import all terrestrial mammal shapefiles from IUCN

mammals <- st_read(h("data", "TERRESTRIAL_MAMMALS")) %>%
  # subset to extant ranges
  filter(presence == 1) %>%
  mutate(
    binomial = case_when(
      binomial == "Rattus arfakienis" ~ "Rattus arfakiensis",
      str_detect(binomial, "Oryx beisa ssp") ~ "Oryx beisa",
      TRUE ~ as.character(binomial)
    )
  )

mammals <- mammals %>%
  left_join(., read_csv(h("data", "iucn_to_hp3_taxonomic_harmonization.csv")),
            by = c("binomial" = "IUCN_name")) %>%
  mutate(binomial = ifelse(!is.na(hp3_zoo_prediction_name), hp3_zoo_prediction_name, binomial))

# Import all country border information

borders <- st_read(h("data", "TM_WORLD_BORDERS-0.3")) %>%
  # modify country names
  mutate(
    country = case_when(
      NAME == "Cote d'Ivoire" ~ "Ivory Coast",
      NAME == "Congo" ~ "Republic of Congo",
      NAME == "Sudan" ~ "South Sudan",
      NAME == "Viet Nam" ~ "Vietnam",
      TRUE ~ as.character(NAME)
    )
  )

#eha.borders <- filter(borders, country %in% eha.countries)
countries.borders <- filter(borders, country %in% countries)

# Do a spatial join across the country borders and mammals

#joined <- st_join(eha.borders, mammals)
joined <- st_join(countries.borders, mammals)

# Generate a lookup table

lookup.table <- joined %>%
  st_drop_geometry() %>%
  distinct(country, binomial) %>%
  mutate(binomial = as.character(binomial)) %>%
  arrange(country, binomial)

taxa.to.eliminate <- c(
  "Alces alces", "Axis axis", "Bos gaurus", "Bos javanicus",
  "Bos mutus", "Bunomys karokophilus", "Bunomys torajae", "Canis lupaster",
  "Cervus hanglu", "Cervus nippon", "Coccymys shawmayeri", "Cricetomys ansorgei",
  "Cricetomys gambianus", "Crocidura absconditus", "Crocidura eburnea", "Crocidura gathornei",
  "Elephas maximus", "Eudiscoderma thongareeae", "Gracilimus radix", "Halmaheramys bokimekot",
  "Hyorhinomys stuempkei", "Margaretamys christinae", "Murina balaensis", "Mus musculus",
  "Ondatra zibethicus", "Pongo tapanuliensis", "Rattus rattus", "Rhinolophus xinanzhongguoensis",
  "Sus scrofa"
)

lookup.table <- lookup.table %>%
  filter(
    !(binomial %in% taxa.to.eliminate),
    !str_detect(binomial, "Miniopterus")
  )

# Verify all taxa in the lookup table will match with the HP3 missing zoonoses data

mz <- read_csv(h("data", "hp3_zoo_predictions.csv")) %>%
  mutate(
    species = case_when(
      species == "Rattus_arfakienis" ~ "Rattus arfakiensis",
      TRUE ~ str_replace(species, "_", " ")
    )
  ) %>%
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

assertthat::assert_that(sum(lookup.table$binomial %in% unique(mz$species)) == nrow(lookup.table))
lookup.table$binomial[!lookup.table$binomial %in% unique(mz$species)]

# Write lookup table to a CSV file

write_csv(lookup.table, h("data", "mammal_country_lookup.csv"))
