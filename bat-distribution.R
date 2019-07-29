library(CoordinateCleaner)
library(countrycode)
library(DT)
library(eidith)
library(glue)
library(iNEXT)
library(kableExtra)
library(leaflet)
library(leaflet.extras)
library(raster)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(tidyverse)
library(wesanderson)

h <- here::here
walk(list.files(h("R/"), full.names = TRUE),
     source, echo = FALSE, verbose = FALSE)
# Load EIDITH data tables

e <- ed_events() #%>%
#filter(country == !!params$country) # events filtered to country
a <- ed_animals()
s <- ed_specimens()
t <- ed_tests()
v <- ed_viruses()
ts <- ed_testspecimen()

e2 <- ed2_events() %>%
  mutate(country = ifelse(country %in% c("Malaysia, Peninsular", "Malaysia, Sabah"), "Malaysia", country)) %>%
  #filter(country == !!params$country) %>% # events filtered to country
  arrange(desc(integer_id)) %>%
  distinct(event_name, .keep_all = TRUE)
a2 <- ed2_animals() %>%
  select(-c(integer_id, gains4_event_id, gains4_sample_unit_id))
s2 <- ed2_specimens() %>%
  select(-c(integer_id, gains4_sample_unit_id))
t2 <- ed2_tests() %>%
  select(-c(integer_id))
ti <- ed2_test_interpreted() %>%
  arrange(desc(integer_id)) %>%
  distinct(gains4_test_id, .keep_all = TRUE) %>%
  select(
    -c(integer_id, is_outbreak_testing, season,
       lab_name, real_time_positive_control_value_1, real_time_positive_control_value_2,
       real_time_internal_control_value, real_time_ct_value, lab_name_confirmation,
       test_status, date_govt_approved_release)
  )

# Modify animal taxonomy information

a2 <- a2 %>%
  mutate(
    class = stringr::str_to_sentence(class),
    order = stringr::str_to_sentence(order),
    family = stringr::str_to_sentence(family),
    taxa_group_mod = case_when(
      taxa_group == "cats" ~ "carnivores",
      taxa_group == "dogs" ~ "carnivores",
      taxa_group == "poultry/other fowl" ~ "birds",
      family == "Sciuridae" ~ "rodents/shrews",
      TRUE ~ taxa_group
    ),
    species_scientific_name = case_when(
      species_scientific_name == "Syncerus Caffer" ~ "Syncerus caffer",
      TRUE ~ species_scientific_name
    )
  )

# Split out P2 viral co-infection data

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

# Modify virus name (in new "viral_species" column) and detection data

v <- v %>%
  mutate(viral_species = stringi::stri_replace_first_regex(
    virus_name, "(new\\s)?strain\\sof\\s", ""))

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

# Join across all tables to get complete data

d <- left_join(e, a, by = "event_id") %>%
  left_join(s, by = "animal_id") %>%
  left_join(ts, by = "specimen_id") %>%
  left_join(t, by = "test_id") %>%
  left_join(v, by = "test_id")

d2 <- left_join(e2, a2, by = c("event_name", "project")) %>%
  left_join(s2, by = c("animal_id" = "animal_human_id", "project")) %>%
  left_join(t2, by = c("specimen_id", "gains4_specimen_id", "project")) %>%
  left_join(ti, by = c("gains4_specimen_id", "gains4_test_id", "project"))

country_eha <-  mod_country(eidith::eha_countries()) %>% unique() # for range layers
admin_eha <- ne_countries(country = country_eha, type='countries', scale = 'large')

# Which bat most sampled?
bat_select <- d2 %>%
  filter(taxa_group == "bats") %>%
  group_by(species_scientific_name) %>%
  count() %>%
  ungroup() %>%
  arrange(-n) %>%
  slice(1:10)

d3 <- d2 %>%
  mutate_at(.vars = c("site_latitude", "site_longitude"), ~as.numeric(.)) %>%
  filter(species_scientific_name %in% bat_select$species_scientific_name) %>%
  group_by(species_scientific_name, site_latitude, site_longitude) %>%
  count() %>%
  ungroup() %>%
  mutate(n_scale = scales::rescale(n, to = c(3, 7)))


# Get iucn ranges for species with viral detections
iucn <- st_read(h("data", "TERRESTRIAL_MAMMALS")) %>% # downloaded: https://www.iucnredlist.org/resources/spatial-data-download
  filter(presence == 1) %>%
  mutate(
    binomial = case_when(
      binomial == "Rattus arfakienis" ~ "Rattus arfakiensis",
      str_detect(binomial, "Oryx beisa ssp") ~ "Oryx beisa",
      TRUE ~ as.character(binomial)
    )
  )

iucn <- iucn %>%
  left_join(., read_csv(h("data", "iucn_to_hp3_taxonomic_harmonization.csv")),
            by = c("binomial" = "IUCN_name")) %>%
  mutate(binomial = ifelse(!is.na(hp3_zoo_prediction_name), hp3_zoo_prediction_name, binomial))

iucn_detects <- iucn %>%
  filter(binomial %in% bat_select$species_scientific_name)

# leaflet
lf <- leaflet() %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolygons(data = admin_eha, fill = FALSE, stroke = TRUE, weight = 1, color = "white", opacity = 0.5) %>%
  addFullscreenControl(position = "topright")

opac <- 0.3
rad <- 5


for(spec in unique(iucn_detects$binomial)){

  d4 <- d3 %>%
    filter(species_scientific_name == spec)

  iucn_sub <- iucn_detects %>% filter(binomial==spec)

  lf <- lf %>%
    addPolygons(data = iucn_sub,
                stroke = TRUE, color = "#42ecf5", weight = 2, opacity = opac,
                fill = TRUE, fillColor = "#42ecf5", fillOpacity = opac,
                label = spec,
                group = spec) %>%
    addCircleMarkers(data = d4, radius =  ~n_scale,
                     lng = ~jitter(site_longitude), lat = ~jitter(site_latitude),
                     stroke = TRUE, color = "#eb4034", weight = 1, opacity = 1,
                     fill = TRUE, fillColor = "#e342f5", fillOpacity = 1,
                     label = ~paste0("PREDICT ", spec, "n = ", n),
                     group = spec)

}

# User controlled layers
lf <- lf %>%
  addLayersControl(
    baseGroups = unique(iucn_detects$binomial),
    options = layersControlOptions(collapsed = TRUE),
    position = "bottomleft"
  )

htmlwidgets::saveWidget(lf, "PREDICT_bats.html")
