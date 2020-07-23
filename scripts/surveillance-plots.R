library(tidyverse)
library(eidith)
library(ggthemes)
h <- here::here


# Read in joined data -----------------------------------------------------
animal_eidith <- read_rds(h("data", "animal.rds"))
human_eidith <- read_rds(h("data", "human.rds"))
site_names <- read_csv(h("site-name-lookup.csv")) %>%
  mutate(new = str_remove_all(new, " Concurrent Site [1-9]")) %>%
  group_by(country) %>%
  mutate(order = row_number())
countries <- recode(eidith::eha_countries(), "Malaysia, Peninsular" =  "Malaysia", "Malaysia, Sabah" = "Malaysia") %>% unique()

# Filter data  -----------------------------------------------------
taxa <- animal_eidith %>%
  as_tibble() %>%
  drop_na(specimen_id) %>%  # select only taxa that were sampled
  distinct(project, country, site_name, concurrent_sampling_site, id = animal_id, taxa_group, season, human_density_impact) %>%
  drop_na(id)

humans <- human_eidith %>%
  as_tibble() %>%
  drop_na(specimen_id) %>%  # select only individuals who submitted sampled
  distinct(project, country, site_name, concurrent_sampling_site, id = participant_id, season, human_density_impact) %>%
  drop_na(id) %>%
  mutate(taxa_group = ifelse(grepl("Clinic", concurrent_sampling_site), "Humans (clinic)", "Humans (community)"))

dat <- bind_rows(taxa, humans) %>%
  filter(!grepl("Independent|Independant|Not Mapped", concurrent_sampling_site)) %>%
  mutate(season = factor(season, levels = c("Dry", "Wet"))) %>%
  mutate(taxa_group = factor(taxa_group,
                             levels = c("bats", "rodents/shrews", "non-human primates", "swine", "poultry/other fowl",  "Humans (clinic)", "Humans (community)"),
                             labels = c("Bats", "Rodents/Shrews", "Primates", "Swine", "Poultry/Other fowl",  "Humans (clinic)", "Humans (community)"))) %>%
  drop_na(taxa_group) %>%
  mutate(taxa_group = as.character(taxa_group)) %>%
  left_join(., site_names, by = c("country", "concurrent_sampling_site" = "old"))

# For each country  -----------------------------------------------------
for(cntry in countries){

  # Get and process country data
  cdat <- dat %>%
    filter(country == cntry) %>%
    group_by(concurrent_sampling_site, new, order, season, taxa_group) %>%
    count() %>%
    ungroup() %>%
    mutate(concurrent_sampling_site = fct_reorder(concurrent_sampling_site, order)) %>%
    mutate(site_name = fct_reorder(new, order)) %>%
    select(-new) %>%
    group_by(concurrent_sampling_site, site_name) %>%
    mutate(n_taxa = n_distinct(taxa_group)) %>%
    group_by(concurrent_sampling_site, site_name, taxa_group) %>%
    mutate(total_by_taxa_site = sum(n)) %>%
    ungroup()

  # Dummy vars to get ordering in plots to work
  dummy_vars <- cdat %>%
    distinct(concurrent_sampling_site, taxa_group) %>%
    mutate(dummy_var = letters[1:nrow(.)])

  # Tibble for placing site labels
  site_labs <- cdat %>%
    mutate(season = "Dry", max_count = 1.1*max(total_by_taxa_site) ) %>%
    select(-n, -total_by_taxa_site, -taxa_group) %>%
    distinct()

  # Plot
  cdat %>%
    left_join(dummy_vars) %>%
    ggplot(aes(x = fct_reorder(dummy_var, -total_by_taxa_site), y = n, fill = season, label = n)) +
    geom_bar(stat = "identity") +
    geom_text(size = 4, position = position_stack(vjust = 0.5),  family =  "sans") +
    geom_text(data = site_labs, aes(x = n_taxa, y = max_count, label = site_name), hjust = 1, size = 4) +
    scale_y_continuous(expand = c(0.01,0.01), limits = c(0, unique(site_labs$max_count))) +
    scale_x_discrete(breaks = dummy_vars$dummy_var, labels = dummy_vars$taxa_group) +
    coord_flip() +
    scale_fill_manual(values = c("Wet" = "dodgerblue1", "Dry" = "goldenrod1")) +
    facet_grid(concurrent_sampling_site~., scales = "free", space = "free") +
    labs(y = "Individuals sampled", x = "", fill = "Sampling Season") +
    theme_foundation(base_size = 12, base_family =  "sans") +
    theme(strip.background = element_blank(),
          strip.text = element_blank(),
          axis.text.y = element_text(size = rel(1.5)),
          axis.ticks = element_blank(),
          axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          strip.placement = "inside")

  ggsave(h("outputs", paste0(cntry, "-sample-bar-plot.png")), width = 12)


}
