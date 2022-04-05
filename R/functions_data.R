#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#### SCRIPT INTRODUCTION ####
#
#' @name functions_data.R  
#' @description R script containing all functions relative to data
#               importation and formatting
#' @author Natheo Beauchamp, Julien Barrere
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#' Get file from its url and write it on disk, at a specified location. 
#' @param dir.name.in Directory where the file should be written (ex: "data/BWI")
#' @param url.in URL where to download the file.
get_and_write <- function(dir.name.in, url.in){
  
  # Write directories if they do not exist
  path.in <- strsplit(dir.name.in, "/")[[1]]
  for(i in 1:length(path.in)){
    if(i == 1) path.in_i <- path.in[i]
    else path.in_i <- paste(path.in_i, path.in[i], sep = "/")
    if(!dir.exists(path.in_i)) dir.create(path.in_i)
  }
  
  # Write file on the disk
  url.in_split <- strsplit(url.in, "/")[[1]]
  file.in <- paste(dir.name.in, url.in_split[length(url.in_split)], sep = "/")
  if(!file.exists(file.in)){
    try(GET(url.in, authenticate('guest', ""), write_disk(file.in, overwrite = TRUE)))
    # Specific case of zip file: unzip and delete zip file
    if("zip" %in% strsplit(file.in, split = "\\.")[[1]]){
      unzip(file.in, exdir = dir.name.in, overwrite = T)
      print(paste0("---Getting and unzipping ", file.in))
      unlink(file.in)
    }else{print(paste0("---Getting ", file.in))}
  } 
}


#' Function to write all FRENCH NFI data on disk
#' @return a dataframe containing the directories where each file is stored, and the url to get these files
get_FrenchNFI <- function(){
  
  dir.in = c("data/FrenchNFI")
  url.in = c("https://inventaire-forestier.ign.fr/dataifn/data/export_dataifn_2005_2020.zip")
  get_and_write(dir.in, url.in)
  return(paste(dir.in, list.files(dir.in), sep = "/"))
}


#' Format the raw FrenchNFI data
#' @author Natheo BEAUCHAMP (rearranged by Julien BARRERE)
#' @param FrenchNFI_tree_raw Raw french NFI tree table
#' @param FrenchNFI_plot_raw Raw french NFI plot table
#' @return A list with two elements: formated tree table (FrenchNFI_tree) and plot table (FrenchNFI_plot)
format_FrenchNFI_raw <- function(FrenchNFI_tree_raw, FrenchNFI_plot_raw){
  ## - Format plot level data
  FrenchNFI_plot <- FrenchNFI_plot_raw %>% 
    # FILTER BY NUMBER OF VISITS
    group_by(IDP) %>% 
    mutate(Nvisits = n(),
           lastCampagne = max(CAMPAGNE)) %>% 
    filter(Nvisits == 2) %>% 
    mutate(firstVisit = !(CAMPAGNE == lastCampagne)) %>% 
    # FILTER TO REMOVE FIRST CAMPAIGN BECAUSE NO REMEASURENT OF C13
    mutate(firstCampaign = (CAMPAGNE == 2005 & firstVisit) | (CAMPAGNE == 2010 & !firstVisit)) %>% 
    filter(!firstCampaign) %>% 
    # FILTER BY VEGETATION TYPE
    mutate(isForest = CSA %in% c(1,2,3)) %>% 
    group_by(IDP) %>% 
    mutate(isForestBothVisits = (sum(isForest)==2)) %>%
    filter(isForestBothVisits) %>%
    # FILTER BY INTERCEPTION WITH FOREST EDGE
    # Set modalities of before-2007 campaign to fit those of post-2007 
    mutate(PLISI = case_when(
      CAMPAGNE > 2007  ~ ifelse(is.na(PLISI), 1, PLISI + 1), 
      CAMPAGNE <= 2007 ~ ifelse(is.na(PLISI), 1, PLISI + 0) )) %>%
    mutate(isNotAtEdge = (PLISI==1)) %>% 
    group_by(IDP) %>% 
    mutate(isNotAtEdgeBothVisits = (sum(isNotAtEdge)==2))
  
  
  ## FILTER THE TREE DATASET 
  
  # Filter tree dataset to keep only filtered plots
  FrenchNFI_tree <- FrenchNFI_tree_raw %>% 
    filter(IDP %in% unique(FrenchNFI_plot$IDP))
  
  
  # Get IDP of plots that are visited twice
  plots_visited_twice <- FrenchNFI_tree %>% 
    group_by(IDP) %>% 
    summarize(Nvisits = length(unique((CAMPAGNE)))) %>% 
    filter(Nvisits == 2)
  plots_visited_twice <- plots_visited_twice$IDP
  
  # Filter for keeping only plots visited twice in tree and stand datasets
  FrenchNFI_tree <- FrenchNFI_tree %>% 
    filter(IDP %in% plots_visited_twice)
  FrenchNFI_plot <- FrenchNFI_plot %>% 
    filter(IDP %in% plots_visited_twice)
  
  
  
  
  # FILTER THE TREE DATASET FOR KEEPING ONLY STANDS WHERE TREES HAS BEES REMEASURED AT THE SECOND VISIT
  FrenchNFI_tree <- FrenchNFI_tree %>% 
    left_join(FrenchNFI_plot %>% select(IDP, CAMPAGNE, firstVisit),
              by = c("IDP", "CAMPAGNE")) %>% 
    mutate(isNotRemeasured = ifelse(!firstVisit & VEGET5 == 0, is.na(C13), NA))
  # IDs of remeasured stands
  FrenchNFI_plot_remeasured <- (FrenchNFI_tree %>% 
                                  group_by(IDP) %>% 
                                  summarize(SumNotRemeasured = sum(isNotRemeasured, na.rm = TRUE)) %>% 
                                  mutate(standsRemeasured = (SumNotRemeasured==0)) %>% 
                                  filter(standsRemeasured))$IDP
  # Filter datasets with only remeasured stands
  FrenchNFI_tree <- FrenchNFI_tree %>%
    filter(IDP %in% FrenchNFI_plot_remeasured)
  FrenchNFI_plot <- FrenchNFI_plot %>% 
    filter(IDP %in% FrenchNFI_plot_remeasured)
  
  
  
  # FILTER THE TREE DATASET FOR KEEPING ONLY LIVING TREES AT FIRST VISIT
  # Searching for IDP, A with VEGET != 0
  FrenchNFI_tree <- FrenchNFI_tree %>%
    mutate(isVeget0 = ifelse(firstVisit, VEGET==0, NA)) %>% 
    group_by(IDP, A) %>% 
    mutate(firstVisitIsVeget0 = sum(isVeget0, na.rm = TRUE)==1) %>% 
    filter(firstVisitIsVeget0) 
  FrenchNFI_plot_livingtrees <- unique(FrenchNFI_tree$IDP)
  FrenchNFI_plot <- FrenchNFI_plot %>% 
    filter(IDP %in% FrenchNFI_plot_livingtrees)
  
  out <- list(FrenchNFI_tree, FrenchNFI_plot)
  return(out)
}



#' Format French NFI tree data to FUNIV original format
#' @author Natheo BEAUCHAMP (rearranged by Julien BARRERE)
#' @param FrenchNFI french NFI data formatted. 
#' @param FrenchNFI_species FrenchNFI species table
format_FrenchNFI_tree_to_FUNDIV <- function(FrenchNFI, FrenchNFI_species){
  FrenchNFI[[1]] %>% 
    group_by() %>% 
    select(CAMPAGNE, IDP, firstVisit, A, ESPAR, C13, HTOT, VEGET, VEGET5, W) %>% 
    pivot_wider(names_from = firstVisit,
                names_sep = "_",
                values_from = c(CAMPAGNE, ESPAR, C13, HTOT, VEGET, VEGET5, W)) %>% 
    mutate(country = "France") %>%
    rename(plotcode = IDP) %>%
    mutate(treecode = paste(plotcode, A, sep = "_")) %>%
    dplyr::select(country, plotcode, treecode, VEGET5 = VEGET5_FALSE,
                  c13_1 = C13_TRUE, c13_2 = C13_FALSE, weight1 = W_TRUE, 
                  height1 = HTOT_TRUE, height2 = HTOT_FALSE, sp_code = ESPAR_TRUE) %>% 
    mutate(dbh1 = c13_1/(pi) * 1000,
           dbh2 = c13_2/(pi) * 1000) %>% 
    filter(!(dbh1 < 100 & dbh2 < 100)) %>%
    mutate(treestatus = case_when((VEGET5 == "0" & dbh1 >= 100) ~ 2,
                                  (VEGET5 == "0" & dbh1 < 100) ~ 1,
                                  VEGET5 %in% c("6", "7") ~ 3,
                                  VEGET5 %in% c("1", "2", "5", "A", "C", "M", "T") ~ 4,
                                  VEGET5 == "N" ~ 5), 
      ba1 = (pi*((dbh1/1000)/2)^2),
      ba2 = (pi*((dbh2/1000)/2)^2), 
      weight2 = NA_real_) %>% 
    mutate(ba_ha1 = ba1*weight1,
           ba_ha2 = ba2*weight1,
           bachange_ha_yr = weight1*(ba2 - ba1)/5) %>% 
    left_join(FrenchNFI_species %>%
                dplyr::select(code, latinName), 
              by = c("sp_code" = "code")) %>%
    dplyr::select(treecode, plotcode, species = latinName, treestatus, dbh1, dbh2, 
                  height1, height2, ba1, ba_ha1, ba2, ba_ha2, bachange_ha_yr, 
                  weight1, weight2, country)
}



#' Format French NFI plot data to FUNIV original format
#' @author Natheo BEAUCHAMP (rearranged by Julien BARRERE)
#' @param FrenchNFI french NFI data formatted. 
#' @param FUNDIV_tree_FR_raw FrenchNFI tree table formatted for FUNDIV
format_FrenchNFI_plot_to_FUNDIV <- function(FrenchNFI, FUNDIV_tree_FR_raw){
  # Select interest variables
  out <- FrenchNFI[[2]] %>% 
    group_by() %>% 
    dplyr::select(IDP, CAMPAGNE, firstVisit, XL, YL, INCID, NINCID) %>% 
    pivot_wider(names_from = firstVisit,
                names_sep = "_",
                values_from = c(CAMPAGNE, XL, YL, INCID, NINCID))  %>% 
    mutate(XL_SAME = (XL_TRUE==XL_FALSE),
           YL_SAME = (YL_TRUE==YL_FALSE)) %>% 
    dplyr::select(IDP, surveydate1 = CAMPAGNE_TRUE, surveydate2 = CAMPAGNE_FALSE,
                  XL = XL_TRUE, YL = YL_TRUE, incid = INCID_FALSE, nincid = NINCID_FALSE) %>% 
    mutate(country = "France", 
           yearsbetweensurveys = 5, 
           biome = NA_real_, 
           cluster = NA_real_) %>%
    mutate(disturbance.severity = case_when(nincid == 0 ~ 0, 
                                            TRUE ~ as.numeric(incid)), 
           disturbance.nature = case_when(nincid == 0 ~ "none", 
                                          nincid == 1 ~ "fire", 
                                          nincid == 4 ~ "storm", 
                                          nincid %in% c(2, 3, 5) ~ "other", 
                                          is.na(nincid) ~ if_else(incid == 0, "none", NA_character_))) %>%
    rename(plotcode = IDP)
  
  # Convert coord from Lambert93 French national projection system to WGS84 projection system
  out.coordinates <- out %>% dplyr::select(XL, YL)
  coordinates(out.coordinates) <- c("XL", "YL")
  proj4string(out.coordinates) <- CRS("+init=epsg:2154") 
  out.coordinates <- spTransform(out.coordinates,  CRS("+init=epsg:4326"))
  out$longitude <- coordinates(out.coordinates)[,"XL"]
  out$latitude <- coordinates(out.coordinates)[,"YL"]
  
  # Get management and basal area per hectare from tree level data
  out.management.weight <- FUNDIV_tree_FR_raw %>%
    mutate(harvest = case_when(treestatus == 3 ~ 1, TRUE ~ 0)) %>%
    group_by(plotcode) %>%
    summarise(ba_ha1 = sum(ba_ha1, na.rm = T),
              ba_ha2 = sum(ba_ha2, na.rm = T), 
              n.harvest = sum(harvest, na.rm = T)) %>%
    mutate(management = case_when(n.harvest > 0 ~ 1, TRUE ~ 0)) %>%
    dplyr::select(plotcode, ba_ha1, ba_ha2, management)
  
  
  # Finish formating
  out <- out %>%
    left_join(out.management.weight, by = "plotcode") %>%
    dplyr::select(plotcode, cluster, country, longitude, latitude, 
                  yearsbetweensurveys, surveydate1, surveydate2, biome, 
                  ba_ha1, ba_ha2, management, disturbance.severity, disturbance.nature)
  
  return(out)
}


#' Function to get the path of a file, and create directories if they don't exist
#' @param file.in character: path of the file, filename included (ex: "plot/plot.png")
create_dir_if_needed <- function(file.in){
  
  path.in <- strsplit(file.in, "/")[[1]]
  if(length(path.in) > 1){
    for(i in 1:(length(path.in)-1)){
      if(i == 1) path.in_i <- path.in[i]
      else path.in_i <- paste(path.in_i, path.in[i], sep = "/")
      if(!dir.exists(path.in_i)) dir.create(path.in_i)
    }
  }
}




#' Write a table on disk
#' @param table.in dataframe to write on the disk
#' @param file.in Name (and path) of the file on the disk
write_on_disk <- function(table.in, file.in){
  create_dir_if_needed(file.in)
  write.table(table.in, file = file.in, row.names = F)
  return(file.in)
}

