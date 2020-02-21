library(tidyverse)
library(glue)
library(stringi)
library(googlesheets4)
library(pander)
library(sf)
library(countrycode)
library(jsonlite)
library(here)
h <- here::here

# Get species-country lists from IUCN, requires API key
download_wildlife <- function(){
  token <- Sys.getenv("IUCN_REDLIST_KEY")
  countries <- fromJSON(paste0("https://apiv3.iucnredlist.org/api/v3/country/list?token=", token))$results
  wildlife <- map_df(countries$isocode, function(iso){
    result <- fromJSON(paste0("https://apiv3.iucnredlist.org/api/v3/country/getspecies/", iso, "?token=", token))$result
    if(length(result)){
      result <-  result %>% mutate(country = iso)
    }else{
      result <- NULL
    }
    return(result)
  })
  suppressWarnings(dir.create(here("data")))
  write_rds(wildlife, here("data/iucn-wildlife.rds"))
}
if (!file.exists(h("data/iucn-wildlife.rds"))) download_wildlife()

# Load spreadsheet matching HP3 and PREDICT virus names, supplemental host-virus assocaitions
harmonization <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1w3j7yUobQ42gBTtpHhtF8R1-fYIbg3iJ71F_RYOA_Kw/edit#gid=0") %>%
  select(p2_virus_name, hp3_virus_name, additional_hosts)

# Load HP3 virus host-associations and only keep those that have P2 virus names, marge additional associations
hp3.associations <- read_csv(h("data", "associations.csv")) %>%
  rename(hp3_virus_name = vVirusNameCorrected, host_name = hHostNameFinal) %>%
  select(-WildDomInReference, -DetectionMethod, -DetectionQuality, -Reference) %>%
  left_join(harmonization, by = c("hp3_virus_name")) %>%
  filter(!is.na(p2_virus_name)) %>%
  mutate(host_name = stri_replace_all_fixed(host_name, "_", " ")) %>%
  mutate(host_name = map2(additional_hosts, host_name,  ~c(.y, na.omit(unlist(stri_split_fixed(.x, ", ")))))) %>%
  select(-additional_hosts) %>%
  unnest(host_name) %>%
  mutate(project = "Previously known")

# Get P2 mammal-virus associations, summaraizz by country and standardize virus name with HP3
p2.associations <- read_csv(h("data", "P2_virus_mammal_host_associations.csv")) %>%
  rename(p2_virus_name = viral_species, host_name = scientific_name) %>%
  group_by(p2_virus_name, host_name) %>%
  summarize(country_list = glue_collapse(country, sep = ", ")) %>%
  ungroup() %>%
  mutate(project = "PREDICT 2") %>%
  left_join(harmonization, by = "p2_virus_name") %>%
  select(-additional_hosts)


# Gather alll associations, determine which P2 associations are novel
full.association.table <- hp3.associations %>%
  dplyr::select(hp3_virus_name, host_name, project) %>%
  bind_rows(., dplyr::select(p2.associations, hp3_virus_name, host_name, project, country_list)) %>%
  distinct(hp3_virus_name, host_name, project, country_list) %>%
  arrange(hp3_virus_name, host_name, desc(project)) %>%
  group_by(hp3_virus_name) %>%
  mutate(expansion = ("Previously known" %in% project & "PREDICT 2" %in% project) &
           any(!(host_name[project == "PREDICT 2"] %in% host_name[project == "Previously known"]))) %>%
  ungroup() %>%
  filter(expansion) %>%
  select(-expansion)

# Load wildlife-country associations
wildlife <- read_rds(here("data-raw/iucn-wildlife.rds")) %>%
  as_tibble() %>%
  select(scientific_name, country) %>%
  distinct() %>%
  filter(country != "DT") %>% # disputed territory
  mutate(country = countrycode(country, origin = "iso2c", destination = "country.name"))


# Create table of host species expansions
expansion <- full.association.table %>%
  group_by(hp3_virus_name) %>%
  summarize(`Previously Known Hosts` = paste(host_name[project == "Previously known"], collapse = ", "),
            `New hosts found in PREDICT 2` = paste(host_name[project == "PREDICT 2" & !(host_name %in% host_name[project == "Previously known"])], collapse = ", "),
            `New Hosts in Countries` = paste(unique(na.omit(unlist(stri_split_fixed(country_list, ", ")))), collapse = ", ")) %>%
  rename(Virus = hp3_virus_name) %>%
  mutate(Virus = stri_replace_all_fixed(Virus, "_", " "))

# Print table of species expansions
pandoc.table(expansion[3,], split.table = Inf, table.split.cells = Inf)

# Print table of country expansions
left_join(full.association.table, wildlife, by = c("host_name" = "scientific_name")) %>%
  group_by(hp3_virus_name) %>%
  mutate(expansion = project == "PREDICT 2" & !(country %in% country[project == "Previously known"])) %>%
  filter(any(expansion)) %>%
  summarize(original_countries = paste(unique(country[!expansion]), collapse = ", "),
            detected_in = paste(unique(na.omit(unlist(stri_split_fixed(country_list, ", ")))), collapse = ", "),
            expansion_countries = paste(unique(country[expansion]), collapse = ", ")) %>%
  pandoc.table(split.table = Inf, table.split.cells = Inf)
