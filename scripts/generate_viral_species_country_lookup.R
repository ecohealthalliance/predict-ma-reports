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
  left_join(t2, by = c("specimen_id", "gains4_specimen_id", "project")) %>%
  filter(!is.na(date_govt_approved_release))

distinct(d2, viral_species, country) %>%
  filter(!is.na(viral_species)) %>%
  arrange(viral_species, country) %>%
  write_csv(., h("data", "viral_species_country_lookup.csv"))
