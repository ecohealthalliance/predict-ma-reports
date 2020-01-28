# PREDICT modeling and analytics reports

This repository contains R code to generate M&A summary reports using EIDITH data.

To use, clone this repository and set it as your working directory. (You may open the predict-ma-reports.Rproj file if you use RStudio.)

1. First run devtools::install_deps(upgrade = "always") in R to get all packages required to run this code and ensure that they are up to date. These packages are listed in the DESCRIPTION file. You should in general keep up to date with the latest version of the eidith package, which tracks changes made upstream in the EIDITH database.

2. Run `00-get-data.R` 
  - This will download EIDITH data into the (currently empty) db/ folder. Set all countries of interest in the `ed_db_download()` call. Make sure you have set your EIDITH_USERNAME and EIDITH_PASSWORD environment variables. See ?eidith::ed_auth in the eidith R package for details. 
  - This will also download supporting data (IUCN species, population and mammal density map layers) from a public AWS repository.
  
3. Run `scripts/generate_viral_species_lookups.R`. 

4. Run `scripts/generate_mammal_country_lookup.R`. 
  - Note that line 96 in this script will print any species names that need to be added to `data/iucn_to_hp3_taxonomic_harmonization.csv` to reconcile IUCN and HP3 species names.

5. Modify `report-template.Rmd` however you like.

6. Run `make.R` to generate the reports (from `report-template.Rmd`). Specify your countries.

Output reports are saved to the `outputs/` folder. 

*`.encrypted`, `dropbox_upload.R`, and `.gitlab-ci.yml` files are specific to EcoHealth Alliance's automated pipeline infrastructure and require EHA encryption keys to use.
