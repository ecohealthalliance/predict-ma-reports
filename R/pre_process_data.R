h <- here::here

pre_process_data <- function(){

  # Load EIDITH data tables
  e2 <- ed2_events() %>%
    mutate(country = ifelse(country %in% c("Malaysia, Peninsular", "Malaysia, Sabah"), "Malaysia", country)) %>%
    arrange(desc(integer_id)) %>%
    distinct(event_name, .keep_all = TRUE) %>%
    dplyr::select(-c(integer_id))
  a2 <- ed2_animals() %>%
    dplyr::select(-integer_id)
  s2 <- ed2_specimens() %>%
    dplyr::select(-integer_id, -starts_with("season"))
  t2 <- ed2_tests() %>%
    dplyr::select(-c(integer_id))
  ti <- ed2_test_interpreted() %>%
    arrange(desc(integer_id)) %>%
    distinct(gains4_test_id, .keep_all = TRUE) %>%
    dplyr::select(
      -c(integer_id, is_outbreak_testing,
         lab_name, real_time_positive_control_value_1, real_time_positive_control_value_2,
         real_time_internal_control_value, real_time_ct_value, lab_name_confirmation,
         test_status, date_govt_approved_release)
    )
  h2 <- ed2_human()

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
      scientific_name = case_when(
        scientific_name == "Mus musculus cf. castaneus" ~ "Mus musculus",
        scientific_name == "Syncerus Caffer" ~ "Syncerus caffer",
        str_detect(scientific_name, " cf. ") ~ str_replace(scientific_name, " cf. ", " "),
        str_count(scientific_name, " ") == 2 ~ word(scientific_name, 1, 2),
        TRUE ~ scientific_name
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

  t2 <- separate_rows(t2, virus, sequence, sep = "\\|") %>%
    separate_rows(specimen_id, sep = ",")

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
    dplyr::select(-viral_species_mod)

  return(list(e2 = e2,
              a2 = a2,
              h2 = h2,
              s2 = s2,
              t2 = t2,
              ti = ti))
}

merge_data <- function(report, eidith){

  iwalk(eidith, function(x, y){
    assign(y, x, envir = .GlobalEnv)
  })

  if(report == "animal"){
    d2 <- left_join(e2, a2, by = c("event_name", "project")) %>% # excluding gains4_event_id because of inconsistencies
      left_join(s2, by = c("gains4_sample_unit_id", "animal_id" = "animal_human_id", "project")) %>%
      left_join(t2, by = c("specimen_id", "animal_id", "project")) %>%  # excluding gains4_specimen_id because of inconsistencies
      left_join(ti, by = c("gains4_test_id", "project")) # excluding gains4_specimen_id because of inconsistencies
  d2 <- d2 %>%
    dplyr::select(-starts_with("gains4_specimen_id"), -starts_with("gains4_event_id"))
    }
  if(report == "human"){
    d2 <- left_join(e2, h2, by = c("gains4_event_id", "event_name")) %>%
      left_join(s2, by = c("gains4_sample_unit_id", "participant_id" = "animal_human_id", "project")) %>%
      left_join(t2, by = c("specimen_id",  "participant_id" = "animal_id", "project")) %>% # excluding gains4_specimen_id because of inconsistencies
      left_join(ti, by = c("gains4_test_id", "project")) # excluding gains4_specimen_id because of inconsistencies
    d2 <- d2 %>%
      dplyr::select(-starts_with("gains4_specimen_id"))
  }
    }
  return(d2)
}

# QA ----------------------------------------------------------------------

# ### in animal but not event
# setdiff(unique(a2$gains4_event_id), unique(e2$gains4_event_id))
# a2 %>% filter(gains4_event_id == 363) %>% pull(event_name)
# e2 %>% filter(event_name == "ID-Bolaang Mongondow-Barat-Tengah-2016Mar09") %>% pull(gains4_event_id)
# a2 %>% filter(gains4_event_id == 387) %>% pull(event_name)
#
# setdiff(unique(h2$gains4_event_id), unique(e2$gains4_event_id))
#
# setdiff(unique(a2$event_name), unique(e2$event_name))
#
# ### in specimen but not animal or human
# setdiff(unique(s2$gains4_sample_unit_id), unique(c(a2$gains4_sample_unit_id, h2$gains4_sample_unit_id)))
# setdiff(unique(s2$animal_human_id), unique(c(a2$animal_id, h2$participant_id)))
#
# ### in test but not specimen
# setdiff(unique(t2$gains4_specimen_id), unique(s2$gains4_specimen_id))
# setdiff(unique(t2$specimen_id), unique(s2$specimen_id))
#
# diffs = setdiff(unique(t2$gains4_specimen_id), unique(s2$gains4_specimen_id))
# t2 %>% filter(gains4_specimen_id %in% diffs) %>% pull(specimen_id)
# t2_unfiltered <- ed2_tests()
# t2_unfiltered %>%
#   filter(gains4_specimen_id %in% diffs) %>%
#   pull(specimen_id) %>% unique()
#
# ### in test interp but not test
# setdiff(unique(ti$gains4_test_id), unique(t2$gains4_test_id))
# setdiff(unique(ti$gains4_specimen_id), unique(t2$gains4_specimen_id))



