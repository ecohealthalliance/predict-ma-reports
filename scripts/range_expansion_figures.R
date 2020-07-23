library(tidyverse)
library(glue)
library(stringi)
library(googlesheets4)
library(jsonlite)
library(here)
h <- here::here

# Load spreadsheet matching HP3 and PREDICT virus names, supplemental host-virus assocaitions
harmonization <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1w3j7yUobQ42gBTtpHhtF8R1-fYIbg3iJ71F_RYOA_Kw/edit#gid=0") %>%
  select(p2_virus_name, hp3_virus_name, additional_hosts)

# Load HP3 virus host-associations and only keep those that have P2 virus names, merge additional associations
hp3.associations <- read_csv(h("data", "associations.csv")) %>% # known associations
  rename(hp3_virus_name = vVirusNameCorrected, host_name = hHostNameFinal) %>%
  select(-WildDomInReference, -DetectionMethod, -DetectionQuality, -Reference) %>%
  left_join(harmonization, by = c("hp3_virus_name")) %>%
  filter(!is.na(p2_virus_name)) %>%
  mutate(host_name = stri_replace_all_fixed(host_name, "_", " ")) %>%
  mutate(host_name = map2(additional_hosts, host_name,  ~c(.y, na.omit(unlist(stri_split_fixed(.x, ", ")))))) %>%
  unnest(host_name) %>%
  select(-additional_hosts, -hp3_virus_name) %>%
  mutate(status = "Previously known")

# Load P2 detected viruses
p2.associations <- read_csv(h("data", "P2_virus_mammal_host_associations.csv")) %>%
  rename(p2_virus_name = viral_species, host_name = scientific_name) %>%
  left_join(harmonization, by = "p2_virus_name") %>%
  drop_na(hp3_virus_name) %>%
  select(-additional_hosts, -hp3_virus_name) %>%
  distinct()

# Join known associations (hp3.associations) with P2 detected viruses (p2.associations)
full.association.table <- left_join(p2.associations, hp3.associations) %>%
  mutate(status = replace_na(status, "PREDICT Identified")) %>%
  mutate(p2_virus_name = str_replace(p2_virus_name, " \\(", "\n\\("))

# Get global totals of number of host species
total_expansion <- full.association.table %>%
  distinct(p2_virus_name, host_name, status) %>%
  group_by(p2_virus_name, status) %>%
  count() %>%
  ungroup() %>%
  mutate(country = "Global PREDICT") %>%
  mutate(grp = "global")

# Get country totals of number of host species
country_expansion <- full.association.table %>%
  group_by(country, p2_virus_name, status) %>%
  count() %>%
  group_by(p2_virus_name) %>%
  mutate(exclude = length(unique(status))==1 && unique(status) == "Previously known") %>%
  ungroup() %>%
  filter(!exclude) %>%
  select(-exclude) %>%
  mutate(grp = "countries") %>%
  bind_rows(total_expansion)

# Look just at Murine Cov
murine <- country_expansion %>%
  filter(p2_virus_name == "Murine coronavirus") %>%
  group_by(country, p2_virus_name, grp) %>%
  mutate(nn = sum(n)) %>%
  ungroup() %>%
  mutate(country = fct_reorder(country, nn))

# Plot
ggplot(murine, aes(x = country, y = n, fill = status)) +
  geom_bar(stat = "identity") +
  facet_grid(grp ~ ., scales = "free", space = "free") +
  coord_flip() +
  labs(y = "Number of host species", x = "", title =  unique(murine$p2_virus_name)) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(limits = c(0, ceiling(max(murine$nn)/10)*10)) +
  ggthemes::theme_fivethirtyeight() +
  theme(axis.title = element_text(size = 20),
        axis.text.y = element_text(size = 18),
        axis.text.x = element_text(size = 14, face = "bold"),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = "bottom",
        strip.text = element_blank(),
        plot.title.position = "plot",
        rect = element_rect(fill = "white"))

ggsave(h("outputs/global_murine_host_expansion.png"), width = 9)
