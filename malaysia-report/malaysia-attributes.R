library(ggplot2)
library(eidith)
library(glue)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(ggspatial) # https://github.com/paleolimbot/ggspatial
library(snapbox) #https://github.com/anthonynorth/snapbox, requires MAPBOX_ACCESS_TOKEN env var
library(inlegend) #https://github.com/milesmcbain/inlegend
library(patchwork)
library(viridis)
library(RColorBrewer)
library(png)
library(FNN)
library(reshape2)
library(tidyr)
library(dplyr)
library(affinity) #devtools::install_github("hypertidy/affinity")
library(ggnewscale)
library(flextable)
h <- here::here

# Setup -------------------------------------------------------------------

bbox_malaysia <- st_bbox(c(xmin =  99.39, xmax = 119.41, ymin = 0.73, ymax = 7.51), crs = 4326) # crs = WGS84

layout <- c(
  area(t = 2, l = 2, b = 7, r = 4),
  area(t = 1, l = 0, b = 4.5, r = 2)
)

admin <- ne_countries(country = "Malaysia", type='countries', scale = 'large')

get_layers <- function(filename){

  h <- here::here

  local_file <- h(glue("data/map-layers/", filename))

  if(stringr::str_ends(local_file, ".rds")){
    r <- readr::read_rds(local_file)
  }else{
    r <- raster(local_file)
  }

  r <- r %>%
    crop(., bbox_malaysia) %>%
    mask(., admin)

  if(filename %in% c("gpw-v4-population-density_2015.tif", "summed_mammal_livestock.tif", "2kcor/glbpod1f0503m/w001001.adf")){
    values(r)[values(r)==0] <- 1
  }

  return(r)
}

# Plot functions -----------------------------------------------------------

plot_malaysia_raster <- function(rast, legend_title = "", trans = "identity", scale_fill){

  plot_malaysia <- ggplot() +
    layer_spatial(rast) +
    layer_mapbox(bbox_malaysia,
                 map_style = "mapbox://styles/emmamendelsohn/ckiyr0xe00mqu19ob4r17xuom",
                 mapbox_logo = FALSE,
                 attribution = FALSE,
                 purge_cache = TRUE) + # doesn't seem to work
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    labs(fill = legend_title) +
    theme_void() +
    theme(panel.background = element_rect(fill = "gray50"), plot.margin = margin())

  min_fill <- min(values(rast), na.rm = TRUE)
  max_fill <- max(values(rast), na.rm = TRUE)

  if(scale_fill == "magma") plot_malaysia <- plot_malaysia + scale_fill_viridis(option = "magma", trans = trans, breaks = c(1, 10, 100, 1000, 10000), na.value = "transparent")
  if(scale_fill == "viridis") plot_malaysia <- plot_malaysia + scale_fill_viridis(option = "viridis", breaks = c(min_fill, max_fill), labels = c("Low", "High"),  na.value = "transparent")
  if(scale_fill == "green") plot_malaysia <- plot_malaysia + scale_fill_gradient(low = "#dff5ef", high = "#0b6630",  na.value = "transparent")

  return(plot_malaysia)
}

compress_malaysia_raster <- function(plot_malaysia, main_title){

  plot_compressed <-
    plot_malaysia + scale_x_continuous(limits = c(109.43, 119.41), expand = c(0,0)) + theme(legend.position = "none") +
    plot_malaysia + scale_x_continuous(limits = c(99.39, 104.8), expand = c(0,0)) + theme(legend.position = "right") +
    plot_layout(design = layout, guides = 'collect') +
    plot_annotation(title = main_title)

  return(plot_compressed)

}

# Human, livestock, poultry density ---------------------------------

human_dens <- get_layers("gpw-v4-population-density_2015.tif")
mammal_dens <- get_layers("summed_mammal_livestock.tif")
poultry_dens <- get_layers("2kcor/glbpod1f0503m/w001001.adf")

values(human_dens) <- ifelse(values(human_dens) < 1, 0.1, values(human_dens))
values(mammal_dens) <- ifelse(values(mammal_dens) < 1, 0.1, values(mammal_dens))
values(poultry_dens) <- ifelse(values(poultry_dens) < 1, 0.1, values(poultry_dens))

map_human_dens <- plot_malaysia_raster(rast = human_dens,
                                       legend_title = "people per cell (log scale)",
                                       trans = "log10",
                                       scale_fill = "magma") %>%
  compress_malaysia_raster(., main_title = "Human Population Density")

ggsave(h("malaysia-report/malaysia_human_density.pdf"), map_human_dens, width = 11, height = 8.5, units = "in")

map_livestock_dens <- plot_malaysia_raster(rast = mammal_dens,
                                           legend_title = "livestock per cell (log scale)",
                                           trans = "log10",
                                           scale_fill = "magma") %>%
  compress_malaysia_raster(., main_title = "Mammalian Livestock Density")

ggsave(h("malaysia-report/malaysia_mammalian_livestock_density.pdf"), map_livestock_dens, width = 11, height = 8.5, units = "in")

map_poultry_dens <- plot_malaysia_raster(rast = poultry_dens,
                                         legend_title = "poultry per cell (log scale)",
                                         trans = "log10",
                                         scale_fill = "magma") %>%
  compress_malaysia_raster(.,  main_title = "Poultry Density")

ggsave(h("malaysia-report/malaysia_poultry_livestock_density.pdf"), map_poultry_dens, width = 11, height = 8.5, units = "in")


# HP2 ---------------------------------------------------------------------

hp2 <- get_layers("hotspots2_clipped_hires.tif")

map_hp2 <- plot_malaysia_raster(rast = hp2,
                                scale_fill = "viridis")  +
  scale_y_continuous(limits = c(0.75, 7.5), expand = c(0,0))

map_hp2 <- compress_malaysia_raster(map_hp2,  main_title = "Hotspots 2.0: Risk of New Zoonotic Spillover")

ggsave(h("malaysia-report/malaysia_hp2.pdf"), map_hp2, width = 11, height = 8.5, units = "in")

# Land use  ---------------------------------

trees <- get_layers("land_use/global_trees.tif")

current_urban <- get_layers(filename = "land_use/global_urb.rds")
current_crops <- get_layers(filename = "land_use/global_crops.rds")
new_urban <- get_layers(filename = "land_use/new_urban.rds")
new_crops <- get_layers(filename = "land_use/new_crops.rds")

land_use <- purrr::map2(c(current_urban, current_crops, new_urban, new_crops),
                        c("urban", "crops","urban", "crops"),
                        function(dat, dname){
                          crs(dat) <- crs(trees)
                          values(dat) <- ifelse(values(dat), 1, NA)
                          dat <- rasterToPolygons(dat)
                          dat <- st_as_sf(dat) %>%
                            select(-layer) %>%
                            mutate(land_type = dname)
                        })

current_land_use <- purrr::reduce(land_use[c(1,2)], bind_rows)
new_land_use <- purrr::reduce(land_use[c(3,4)], bind_rows)

map_land_use <- purrr::map2(list(current_land_use, new_land_use),
                            c("Current Land-use (2005)",  "Land-use change (1970 - 2005)"),
                            function(lu, mt){
                              plot_malaysia <- ggplot() +
                                layer_spatial(trees) +
                                scale_fill_gradient(name = "Tree coverage (%)", low = "#dff5ef", high = "#0b6630",  na.value = "transparent") +
                                new_scale_fill()+
                                layer_spatial(lu, aes(fill = land_type, color = land_type)) +
                                scale_color_manual(name = "Land Type",
                                                   labels = c("crops", "urban"),
                                                   values = c("darkgoldenrod2", "blueviolet")) +
                                scale_fill_manual(name = "Land Type",
                                                  labels = c("crops", "urban"),
                                                  values = c("darkgoldenrod2", "blueviolet")) +
                                layer_mapbox(bbox_malaysia,
                                             map_style = "mapbox://styles/emmamendelsohn/ckiyr0xe00mqu19ob4r17xuom",
                                             mapbox_logo = FALSE,
                                             attribution = FALSE,
                                             purge_cache = TRUE) + # doesn't seem to work
                                scale_y_continuous(expand = c(0,0)) +
                                scale_x_continuous(expand = c(0,0)) +
                                theme_void() +
                                theme(panel.background = element_rect(fill = "gray50"), plot.margin = margin())

                              compress_malaysia_raster(plot_malaysia,  main_title = mt)
                            })

ggsave(h("malaysia-report/malaysia_current_land_use.pdf"), map_land_use[[1]], width = 11, height = 8.5, units = "in")
ggsave(h("malaysia-report/malaysia_new_land_use.pdf"), map_land_use[[2]], width = 11, height = 8.5, units = "in")

# GVP ---------------------------------------------------------------------

### code from NR to convert png to raster ###
vals <- readPNG(h("data", "predictGVPmaps", "malaysia.PNG"))[,,1:3]
# Extract one row of pixels from the color scale
color_scale <- vals[508:413,26,]
colnames(color_scale) <- c("r", "g", "b")
# Get only the part without map annotations and make a tidy frame of pixels
malaysia_cropped <- vals[70:388,4:870,]
dimnames(malaysia_cropped) <- list("y"=NULL, "x"=NULL, "col"= c("r", "g", "b"))
pixels <- melt(malaysia_cropped) %>%
  pivot_wider(names_from = col, values_from = value)
# Match each pixel to the closest value on the color scale,
nn <- get.knnx(color_scale, as.matrix(pixels[,c("r", "g", "b")]), k=1)
priority <- nn$nn.index
# Discard colors not closely matching the scale - here this gets rid of the background points
# hist(nn$nn.dist)
priority[nn$nn.dist > 0.2] <- NA
# Make into a raster
priority <- matrix(as.double(priority), ncol = ncol(malaysia_cropped), nrow = nrow(malaysia_cropped))
priority <- raster(priority)
# Georeference - select two points on the map and get their long/lat
pts_latlong <- rbind(
  c(99.715356, 6.393455),
  c(109.714570, 1.831211)
)
#Run affine::affinething(priority) to get those two points in local space
pts_local <- rbind(
  c(0.0004835719, 0.8433727),
  c(0.5132698078, 0.1999396)
)
# Transform and assign extent and projection
gvp <- assignproj(
  setExtent(priority, affinity::domath(pts_latlong, pts_local, priority)),
  proj = "+proj=longlat +datum=WGS84")

map_gvp <- plot_malaysia_raster(rast = gvp,
                                scale_fill = "viridis") +
  scale_y_continuous(limits = c(0.75, 7.5), expand = c(0,0))

map_gvp <- compress_malaysia_raster(map_gvp,  main_title = "Geographic Sampling Prioritization Based on Global Virome Project Spatial Analyses")

ggsave(h("malaysia-report/malaysia_gvp.pdf"), map_gvp, width = 11, height = 8.5, units = "in")


# Cov and Pmx sampling ----------------------------------------------------

d2 <- readr::read_rds(h("data", "animal.rds"))

virus_lookup <- d2 %>%
  distinct(test_requested, viral_species) %>%
  filter(test_requested %in% c("Coronaviruses", "Paramyxoviruses")) %>%
  drop_na()

taxa_lookup <- d2 %>%
  distinct(taxa_group, scientific_name)

virus <- readr::read_csv(h("data", "animal_viral_pairs", "Malaysia_animal_viral_pairs.csv")) %>%
  inner_join(virus_lookup) %>%
  left_join(taxa_lookup) %>%
  arrange(-n_animals_w_detections) %>%
  mutate(detected = ifelse(n_animals_w_detections > 0, "Detect", "Non-detect")) %>%
  mutate(detected = factor(detected, levels = c("Non-detect", "Detect")))

# virus_detect <- virus %>%
#   filter(detected == "Detect") %>%
#   mutate_at(.vars = c("event_latitude", "event_longitude"), .funs = ~round(.,0)) %>%
#   distinct(test_requested, viral_species, event_longitude, event_latitude)
#
# virus_detect_locs <- distinct(virus_detect, event_latitude, event_longitude) %>% mutate(site = row_number())
#
# virus_detect <- virus_detect %>%
#   left_join(virus_detect_locs) %>%
#   arrange(test_requested, site, viral_species) %>%
#   select(test_requested, site, viral_species, everything())

virus_sf <- virus %>%
  st_as_sf(coords = c("event_longitude", "event_latitude"), crs = 4326)

map_viruses <- purrr::map(c("Coronaviruses",  "Paramyxoviruses"), function(tr){

  plot_malaysia <- ggplot() +
    layer_spatial(virus_sf %>% filter(test_requested == tr), aes(color = detected, alpha = detected, size = detected)) +
    layer_mapbox(bbox_malaysia,
                 map_style = "mapbox://styles/emmamendelsohn/ckiyr0xe00mqu19ob4r17xuom",
                 mapbox_logo = FALSE,
                 attribution = FALSE,
                 purge_cache = TRUE) + # doesn't seem to work
    scale_y_continuous(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    scale_color_manual(values = c("Detect" = "#5f3cc7", "Non-detect" = "#a5cf9b"), guide = guide_legend(reverse = TRUE) ) +
    scale_alpha_manual(values = c("Detect" = 1, "Non-detect" = 0.5), guide = guide_legend(reverse = TRUE)) +
    scale_size_manual(values = c("Detect" = 4, "Non-detect" = 2), guide = guide_legend(reverse = TRUE)) +
    theme_void() +
    theme(panel.background = element_rect(fill = "gray50"),
          plot.margin = margin(),
          legend.title = element_blank())
  compress_malaysia_raster(plot_malaysia,  main_title =  paste("PREDICT-2", tr, "Detections"))

})

ggsave(h("malaysia-report/malaysia_coronaviruses.pdf"), map_viruses[[1]], width = 11, height = 8.5, units = "in")
ggsave(h("malaysia-report/malaysia_paramyxoviruses.pdf"), map_viruses[[2]], width = 11, height = 8.5, units = "in")

animal_virus_summary_table <- readr::read_csv(h("data/animal_viral_pairs/Malaysia_animal_virus_summary_table.csv"))

animal_virus_summary_table <- animal_virus_summary_table %>%
  mutate(`Virus Name` = ifelse(!is.na(interpretation), paste0(`Virus Name`, "*"), `Virus Name`))

for(tr in c("Coronaviruses",  "Paramyxoviruses")){
  ft <- animal_virus_summary_table %>%
    filter(`Viral Test Type` == tr) %>%
    select(-`Viral Test Type`)

  if(all(is.na(unique(ft$interpretation)))) footer <- ""
  if(length(na.omit(unique(ft$interpretation))) == 1) footer <- paste("*", na.omit(unique(ft$interpretation)))

  ft <- ft %>%
    select(-interpretation)

  ft %>%
    flextable() %>%
    theme_vanilla() %>%
    width(j = NULL, 1.3) %>%
    width(j = 2:4, 0.75) %>%
    align(i = NULL, j = 2:4, "center") %>%
    add_header_lines(values = paste("PREDICT-2", tr, "Detections"), top = TRUE) %>%
    add_footer_lines(values = footer) %>%
    save_as_docx(path = h(paste0("malaysia-report/malaysia_", tr, "_detects.docx")),
                 pr_section =  officer::prop_section(page_size =  officer::page_size(orient = "landscape")))
}

# Viral prioritization table ----------------------------------------------
mz.subset <- readr::read_csv(h("data", "viral_prioritization", "Malaysia_viral_prioritization.csv"))

viral_prior <- mz.subset %>%
  dplyr::select(species, specimen_count, viruses_detected) %>%
  mutate(
    specimen_count = ifelse(is.na(specimen_count), 0, specimen_count),
    viruses_detected = ifelse(is.na(viruses_detected), "--", viruses_detected)) %>%
  rename(
    `Host Species` = species,
    `No. of Specimens Collected` = specimen_count,
    `Viruses Detected In-Country?` = viruses_detected
  ) %>%
  arrange(- `No. of Specimens Collected`)

viral_prior %>%
  flextable() %>%
  theme_vanilla() %>%
  width(j = NULL, 1.5) %>%
  align(i = NULL, j = 2:3, "center") %>%
  add_header_lines(values = paste("Geographic Sampling Prioritization Based on Global Virome Project Spatial Analyses"), top = TRUE) %>%
  save_as_docx(path = h(paste0("malaysia-report/malaysia_sample_prioritization.docx")))

