require(tidyverse)
require(lubridate)

#### Read in all data_tables -------------
data_files <- paste("clean_data", list.files("clean_data"), sep="/")
all_data <- lapply(data_files, read_csv)
names(all_data) <- gsub(".csv", "", gsub("clean_data/", "", data_files))

#### Join data ----------------------
# Make a fully joined settlement dataset
sett_dat <-all_data$fish_id_dat %>% 
  left_join(all_data$ucrit_sett_dat) %>% 
  left_join(all_data$morph_sett_dat) %>% 
  left_join(all_data$species_dat) %>% 
  left_join(all_data$site_dat)

# Make a fully joined development dataset - ucrit, using sample_id means for morphology measurements
dev_dat_ucrit <- all_data$dev_sample_dat %>% 
  left_join(all_data$ucrit_dev_dat) %>% 
  left_join(all_data$species_dat) %>% 
  left_join(all_data$morph_dev_dat %>% 
              dplyr:: select(sample_id, TLmm,  BDmm,  BAmm,  PAmm) %>% 
              dplyr::group_by(sample_id) %>% 
              dplyr::summarise_all(mean))

# Make a fully joined development dataset - morphology, using sample_id means for ucrit measurements
dev_dat_morph <- all_data$dev_sample_dat %>% 
  left_join(all_data$morph_dev_dat) %>% 
  left_join(all_data$species_dat) %>% 
  left_join(all_data$ucrit_dev_dat %>% 
              dplyr:: select(sample_id, ucrit) %>% 
              dplyr::group_by(sample_id) %>% 
              dplyr::summarise_all(mean))
    
#### Data summaries -----------------
## Settlement data
# variables by species + lowest taxon, settlement data
sett_dat %>%
  dplyr::filter(!is.na(valid_name)) %>% 
  dplyr::mutate(index=1) %>% 
  dplyr::group_by(family, valid_name) %>% 
  dplyr::summarise(count=sum(index)) %>% 
  dim()

# how many ucrit measurements for all valid species?
sett_dat %>% 
  dplyr::filter(!is.na(ucrit) & !is.na(valid_name) & rank=="Species") %>% 
  dplyr::mutate(index=1) %>% 
  dplyr::group_by(family, valid_name) %>% 
  dplyr::summarise(count=sum(index)) 

## development dat
# how many morphology measurements for each species and age?
dev_dat_morph %>% 
  dplyr::mutate(index=1) %>% 
  dplyr::group_by(family, valid_name, age) %>% 
  dplyr::summarise(count=sum(index)) %>% 
  View()
# how many ucrit measurements for each species and age?
dev_dat_ucrit %>% 
  dplyr::mutate(index=1) %>% 
  dplyr::group_by(family, valid_name, age) %>% 
  dplyr::summarise(count=sum(index)) %>% 
  View()  

#### Look at some linked images ------------

library(magick)
sett_image_dat <- all_data$fish_id_dat %>% 
  right_join(all_data$species_dat) %>% 
  dplyr::filter(!is.na(filename)) %>% 
  arrange(family, genus, valid_name, year) 

# show a single image
image_test <- image_read(paste("images", sett_image_dat$filename[1], sep="/"))
print(image_test)

# show a sequence of all images
imgs <- paste("images", sett_image_dat$filename[1:10], sep="/")
img_list <- lapply(imgs, image_read)
img_joined <- image_join(img_list)
image_resize(img_joined, '400x300!') %>%
  image_morph() %>%
  image_animate(optimize = TRUE)
