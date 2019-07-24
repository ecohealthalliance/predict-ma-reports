

datatable_ <- function(x, caption = NULL) {

  datatable(x,
            options = list(
              pageLength = 1000,
              initComplete = JS(
                "function(settings, json) {",
                "$('body').css({'font-family': 'Futura'});",
                "}"
              )
            ),
            class = "hover row-border compact",
            caption = caption,
            autoHideNavigation = TRUE
  )
}


get_virus_summary <- function(x, view = FALSE) {

  tmp <- animal_virus_summary %>%
    filter(viral_species == x) %>%
    distinct(country, taxa_group_mod, species_scientific_name) %>%
    arrange(country, taxa_group_mod, species_scientific_name)

  ifelse(view == TRUE,
         return(datatable_(tmp, x)),
         return(tmp)
  )
}


get_virus_summary2 <- function(x, view = FALSE) {

  tmp <- animal_virus_summary %>%
    filter(viral_species == x) %>%
    arrange(country, taxa_group_mod, species_scientific_name) %>%
    group_by(country) %>%
    summarize(
      n_taxa_groups = length(unique(taxa_group_mod)),
      taxa_groups = paste(unique(taxa_group_mod), collapse = "\n"),
      n_species = length(unique(species_scientific_name)),
      species = paste(unique(species_scientific_name), collapse = "\n")
    ) %>%
    ungroup()

  ifelse(view == TRUE,
         return(datatable_(tmp, x)),
         return(tmp)
  )
}


get_host_viruses <- function(x, view = FALSE) {

  tmp <- animal_virus_summary %>%
    filter(species_scientific_name == x) %>%
    distinct(country, viral_species) %>%
    arrange(country, viral_species)

  ifelse(view == TRUE,
         return(datatable_(tmp, x)),
         return(tmp)
  )
}


get_country_viruses <- function(view = FALSE) {

  tmp <- animal_virus_summary %>%
    arrange(viral_species, taxa_group_mod, species_scientific_name, site_name) %>%
    filter(!is.na(viral_species)) %>%
    group_by(viral_species) %>%
    summarize(
      n_detections = sum(n_positives),
      taxa_groups = paste(unique(taxa_group_mod), collapse = "\n"),
      species = paste(unique(species_scientific_name), collapse = "\n"),
      site_names = paste(unique(site_name), collapse = "\n")
    ) %>%
    ungroup()

  colnames(tmp) <- c("Virus Name", "Number of Detections", "Taxa Groups",
                     "Species Names", "Site Names")

  ifelse(view == TRUE,
         return(datatable_(tmp)),
         return(tmp)
  )
}


get_country_site_viruses <- function(view = FALSE) {

  tmp <- animal_virus_summary %>%
    distinct(site_name, concurrent_sampling_site, viral_species) %>%
    arrange(site_name, viral_species) %>%
    filter(!is.na(site_name))

  ifelse(view == TRUE,
         return(datatable_(tmp)),
         return(tmp)
  )
}


get_country_site_viruses2 <- function(view = FALSE) {

  tmp <- animal_virus_summary %>%
    arrange(site_name, viral_species, taxa_group_mod, species_scientific_name) %>%
    filter(!is.na(site_name)) %>%
    group_by(site_name, concurrent_sampling_site, human_density_impact) %>%
    summarize(
      n_viruses = length(unique(viral_species)),
      viruses = paste(unique(viral_species), collapse = "\n"),
      n_detections = sum(n_positives),
      n_taxa_groups = length(unique(taxa_group_mod)),
      taxa_groups = paste(unique(taxa_group_mod), collapse = "\n"),
      n_species = length(unique(species_scientific_name)),
      species = paste(unique(species_scientific_name), collapse = "\n")
    ) %>%
    ungroup()

  colnames(tmp) <- c("Site Name", "Concurrent Site Status", "Human Density Impact",
                     "Unique Viruses", "Virus Names", "Number of Detections",
                     "No. Taxa Groups", "Taxa Groups", "No. Species", "Species")

  ifelse(view == TRUE,
         return(datatable_(tmp)),
         return(tmp)
  )
}


# Convert a dataframe with host species and viral observation data into an
# incidence matrix representing viral observation data
# All arguments besides the dataframe must be given as strings

# matrix.of = the grouping variable to be used to generate each individual
# incidence matrix (i.e., matrix by host species, genus, family, etc.)

# matrix.of.var = Specific value of the matrix.of variable used to generate
# the matrix

# column.var = the variable represented along the columns of the incidence
# matrix (virus species will always be the rows)

i_matrix <- function(dataframe, matrix.of, matrix.of.var, column.var) {

  filter_criteria <-
    lazyeval::interp(~y == x,
                     .values = list(y = as.name(matrix.of), x = matrix.of.var))

  # Create a matrix of data, removing the row for NA viral observations
  i.matrix <- dataframe %>%
    filter_(filter_criteria) %>%
    group_by_(column.var, "viral_species") %>%
    summarize(Count = n()) %>%
    spread_(column.var, "Count") %>%
    filter(!is.na(viral_species))

  # Generate labels from the first matrix column (to be used a rownames)
  labels <- pull(i.matrix, 1)

  # Remove the first matrix column
  i.matrix <- select(i.matrix, -1) %>%
    as.matrix()

  # Set rownames
  rownames(i.matrix) <- labels

  # Treat NAs as 0s
  i.matrix[is.na(i.matrix)] <- 0

  # Treat any value > 1 as 1
  # Note: can occur if multiple viral sequences from the same virus species
  # are recovered from a sample
  i.matrix[i.matrix > 1] <- 1

  return(i.matrix)
}


# Remove matrices from a list of matrices if they only have a single row

rm_single_row_matrices <- function(list) {

  matrices_to_keep <- sapply(list, function(x) dim(x)[1] > 1)

  list[matrices_to_keep]
}


# Create a list of incidence matrices

# matrix.of = the grouping variable to be used to generate each individual
# incidence matrix (i.e., matrix by host species, genus, family, etc.)

# column.var = the variable represented along the columns of the incidence
# matrices (virus species will always be the rows)

create_ilist <- function(dataframe, matrix.of, column.var) {

  # Create a vector of unique values of the matrix.of variable
  matrix.of.vec <- dataframe %>%
    pull(matrix.of) %>%
    unique() %>%
    sort()

  # Create the initial incidence matrix list
  i.list <-
    lapply(matrix.of.vec, function(x)
      i_matrix(dataframe, matrix.of, x, column.var))

  # Rename the list elements according to the matrix.of.vec variable
  names(i.list) <- matrix.of.vec

  paste0("Original incidence matrix list contains ",
         length(i.list), " matrices") %>%
    print()

  # Remove any matrices with only a single row
  i.list <- rm_single_row_matrices(i.list)

  paste0("Pruned incidence matrix list contains ",
         length(i.list), " matrices") %>%
    print()

  return(i.list)
}

# Download and read in layers for viral range maps
# get layers
get_layers <- function(filename, admin){

  local_file <- h(glue("map-layers/", filename))
  aws_folder <- switch(filename, "gpw-v4-population-density_2015.tif" = "country-maps",
                       "summed_mammal_livestock.tif" = "country-maps/mammal_livestock")

  if(!file.exists(local_file)){
    download.file(glue("https://s3.amazonaws.com/", aws_folder, "/", filename), local_file)
  }

  r <- raster(local_file) %>%
    crop(., admin) %>%
    mask(., admin)

  values(r)[values(r)==0] <- 1
  if(filename == "gpw-v4-population-density_2015.tif"){
    values(r) <- log10(values(r))
  }

  return(r)
}
