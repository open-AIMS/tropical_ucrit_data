require(tidyverse)
require(lubridate)
require(datamodelr)

#### Read in all data_tables -------------
data_files <- paste("clean_data", list.files("clean_data"), sep="/")
all_data <- lapply(data_files, read_csv)
names(all_data) <- gsub(".csv", "", gsub("clean_data/", "", data_files))

# listed data.frame version
dm_f <- dm_from_data_frames(all_data)
dm_f <- dm_set_key(dm_f, 'site_dat', "trap")
dm_f <- dm_set_key(dm_f, 'species_dat', "species_id")
dm_f <- dm_set_key(dm_f, 'fish_id_dat', "fish_id")
dm_f <- dm_set_key(dm_f, 'dev_sample_dat', "sample_id")

dm_f <- dm_add_references(dm_f,
                          fish_id_dat$trap == site_dat$trap,
                          fish_id_dat$species_id == species_dat$species_id,
                          ucrit_sett_dat$fish_id == fish_id_dat$fish_id,
                          morph_dev_dat$sample_id==dev_sample_dat$sample_id,
                           dev_sample_dat$species_id  ==species_dat$species_id,
                          morph_sett_dat$fish_id== fish_id_dat$fish_id,
                          ucrit_dev_dat$sample_id ==  dev_sample_dat$sample_id
                           )

table_segments <- list(Site_data = "site_dat",
                       Species_data = "species_dat",
                       Settlment_data = c("fish_id_dat", "ucrit_sett_dat", "morph_sett_dat"),
                       Development_data = c("dev_sample_dat", "ucrit_dev_dat", "morph_dev_dat"))

dm_seg <- dm_set_segment(dm_f, table_segments)

display <- list(
  accent1 = c("fish_id_dat", "ucrit_sett_dat", "morph_sett_dat"),
  accent2 = c("dev_sample_dat", "ucrit_dev_dat", "morph_dev_dat"),
  accent3 = c("species_dat"),
  accent4 = c("site_dat") )

dm_seg <- dm_set_display(dm_seg, display)

graph <- dm_create_graph(dm_seg, rankdir = "RL", col_attr = c("column", "type"))
dm_render_graph(graph)

# Make a table of info
meta_list <- list(
  age = "Age of the sample in days since hatching.",
  AphiaID = "Field retrieved from the [**WORMS database**](http://www.marinespecies.org/) for that taxa",
  authority =  "Field retrieved from the [**WORMS database**](http://www.marinespecies.org/) for that taxa",
  BAmm = "Body Area in mm. See methods for details.",
  batch_id = "Unique clutch identifier.",
  BDmm = "Body depth in mm. See methods for details.",
  CFAmm = "Caudal fin area in mm. See methods for details.",
  CFDmm = "Caudal fin depth in mm. See methods for details.",
  citation =  "Field retrieved from the [**WORMS database**](http://www.marinespecies.org/) for that taxa",
  class = "The taxonomic class retrieved from the [**WORMS database**](http://www.marinespecies.org/) for that taxa",
  CPDmm = "Caudle peduncle depth in mm. See methods for details",
  date = "Date the sample was used in experiment.",
  eggdur = "Approximate egg duration of the species",
  family =  "The taxonomic family retrieved from the [**WORMS database**](http://www.marinespecies.org/) for that taxa",
  filename = "Likely file name of image of sample.",
  fish_id = "Unique identifier of a particular fish sample.",
  genus =  "The taxonomic geuns retrieved from the [**WORMS database**](http://www.marinespecies.org/) for that taxa",
  hw = "Head width in mm, measured using calipers. See methods for details",
  image = "Likely image name.",
  kingdom = "The taxonomic kingdom retrieved from the [**WORMS database**](http://www.marinespecies.org/) for that taxa",
  lat = "approximate value of the latitude of the trap/site in decimal degrees.",
  location = "An abbreviation indicating the location where the data were collected, values includs BLZ (Belize), TCI (Turks and Caicos Islands), LI (Lizard Island), GI/MI (Green Island or Magnetic Island), SI (Society Islands) and JCU (James Cook University Research Aquarium).",
  lon = "approximate value of the latitude of the trap/site in decimal degrees.",
  Mamm = "Muscle area in mm. See methods for details",
  month =  "Month the sample was used in experiment.",
  notes = "notes taken during measurements",
  order = "The taxonomic order retrieved from the [**WORMS database**](http://www.marinespecies.org/) for that taxa",
  PAmm = "Propulsive Area in mm. See methods for details",
  parentNameUsageID = "Field retrieved from the [**WORMS database**](http://www.marinespecies.org/) for that taxa",
  phylum = "The taxonomic phylum retrieved from the [**WORMS database**](http://www.marinespecies.org/) for that taxa",
  rank = "The taxonomic rank retrieved from the [**WORMS database**](http://www.marinespecies.org/) for that taxa",
  region = "Either Caribbean or GBR (Great Barrier Reef).",
  sample_id = "A unique sample identifier specific to a given clutch, for a given species at a specific sample age.",
  sampling_gear = "The sampling gear used to obtain the sample.",
  scientificname = "Field retrieved from the [**WORMS database**](http://www.marinespecies.org/) for that taxa",
  SLmm = "Standard length in mm. See methods for details",
  species_id = "Unique ID for a specific taxon",
  species_note = "Notes on taxa identification", 
  status =  "Field retrieved from the [**WORMS database**](http://www.marinespecies.org/) for that taxa",
  taxa = "The taxa name used to search for current taxonomic information in the [**WORMS database**](http://www.marinespecies.org/)",
  taxonRankID =  "Field retrieved from the [**WORMS database**](http://www.marinespecies.org/) for that taxa",
  TLmm = "Total length in mm. See methods for details",
  trap = "Unique ID for the specific light trap, crest net or site from which the fish sample was obtained.",
  AMS_rego = "Australian Musesum Sydney registration number",
  stage = "Most likely developmental stage of larvae",
  publication = "Most likely publication in which data appear",
  ucrit = "Swimming speed measurement, See methods for more details.",
  unacceptreason =  "Field retrieved from the [**WORMS database**](http://www.marinespecies.org/) for that taxa",
  url =  "Field retrieved from the [**WORMS database**](http://www.marinespecies.org/) for that taxa",
  valid_AphiaID =  "Field retrieved from the [**WORMS database**](http://www.marinespecies.org/) for that taxa",
  valid_authority =  "Field retrieved from the [**WORMS database**](http://www.marinespecies.org/) for that taxa",
  valid_name = "Valid taxon name for that recorded taxa. Will be same as 'taxa' unless that is no longer accepted according to the [**WORMS database**](http://www.marinespecies.org/)",
  wg = "Sample weight in grams",
  year =  "year the sample was used in experiment."
)      

meta_dat <- do.call("rbind", meta_list) %>% 
  data.frame()
meta_dat$column <- rownames(meta_dat)
colnames(meta_dat) <- c("Description", "column")
 
data_dat <- dm_seg$columns %>% 
  select(table, column, type) %>% 
  left_join(meta_dat) %>% 
  write.csv("meta_data.csv")

  

