#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#### SCRIPT INTRODUCTION ####
#
#' @name _targets.R  
#' @description R script to launch the target pipeline
#' @author Julien BARRERE
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Options and packages ----------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Load targets
library(targets)
# Load functions
lapply(grep("R$", list.files("R"), value = TRUE), function(x) source(file.path("R", x)))
# install if needed and load packages
packages.in <- c("dplyr", "ggplot2", "RCurl", "httr", "tidyr", "data.table", "sp")
for(i in 1:length(packages.in)) if(!(packages.in[i] %in% rownames(installed.packages()))) install.packages(packages.in[i])
# Targets options
options(tidyverse.quiet = TRUE)
tar_option_set(packages = packages.in)
set.seed(2)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Targets workflow --------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list(
  # Download files
  tar_target(files, get_FrenchNFI(), format = "file"), 
  
  # Load raw data
  tar_target(FrenchNFI_tree_raw, fread(files[1])), 
  tar_target(FrenchNFI_plot_raw, fread(files[6])), 
  tar_target(FrenchNFI_species, fread("data/FrenchNFI_species.csv")), 
  
  # Format raw data 
  tar_target(FrenchNFI, format_FrenchNFI_raw(FrenchNFI_tree_raw, FrenchNFI_plot_raw)),
  
  # Format to FUNDIV template
  tar_target(FUNDIV_tree_FR, format_FrenchNFI_tree_to_FUNDIV(FrenchNFI, FrenchNFI_species)), 
  tar_target(FUNDIV_plot_FR, format_FrenchNFI_plot_to_FUNDIV(FrenchNFI, FUNDIV_tree_FR)), 
  
  # Export files
  tar_target(FUNDIV_tree_FR_file, write_on_disk(FUNDIV_tree_FR, "output/FrenchNFI_tree.csv"), format = "file"), 
  tar_target(FUNDIV_plot_FR_file, write_on_disk(FUNDIV_plot_FR, "output/FrenchNFI_plot.csv"), format = "file")
)