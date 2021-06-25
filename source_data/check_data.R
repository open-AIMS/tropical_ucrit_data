require(tidyverse)
require(lubridate)
require(readxl)

#### Read in all data and clean up -------------
image_dat <- read_csv("source_data/images.csv") %>% 
  mutate(image = tolower(image),
         fish_id = tolower(fish_id))
head(image_dat)

image_file_dat <- list.files("images") %>% 
  data.frame() 
colnames(image_file_dat) <- "filename"
image_file_dat <- image_file_dat %>%
  mutate(image=tolower(ifelse(grepl(".jpg", filename), gsub(".jpg", "", filename), gsub(".JPG", "", filename))),
         image=gsub("bottom", "v00j", image),    
         image=gsub("11mid", "11v00j", image),     
         image=gsub("11top", "11v00j", image),
         image=gsub("11bot", "11v00j", image),  
         image=gsub("12mid", "12v00j", image),     
         image=gsub("12top", "12v00j", image),
         image=gsub("12bot", "12v00j", image),
         image=gsub("01mid", "01j", image),     
         image=gsub("01top", "01j", image),
         image=gsub("01bot", "01j", image),
         image=gsub("bott1", "j", image),
         image=gsub("olivia", "j", image),
         image=gsub("olive", "", image),
         image=gsub("top1", "", image),        
         image=gsub("top2", "", image),  
         image=gsub("bot1", "", image),
         image=gsub("12mark", "12v00j", image),
         image=gsub("1mark", "1v01j", image),    
         image=gsub("jeff", "v00j", image),
         image=gsub("jar", "j", image),  
         image=gsub("j1j", "j", image),
         image=gsub("j2j", "j", image),
         image=gsub("jj", "j", image),     
         image=gsub("j2-", "j", image),
         image=gsub("j12v00jt1", "j12", image), 
         image=gsub("j32j", "j32", image),
         image=gsub("sp9", "",image),
         image=gsub("sp11v00", "",image),
         image=gsub(" a.cyanosoma", "",image),
         image=gsub("51b", "51",image),
         image=gsub("117b", "117",image),
         image=gsub("226b", "226",image),
         image=gsub("288b", "288",image)
         ) %>% 
   left_join(image_dat) %>% 
   mutate(fish_id=ifelse(is.na(fish_id), image, fish_id))


site_dat <- read_csv("source_data/sites.csv") %>% 
  arrange(region)
head(site_dat)

species_dat <- read_csv("source_data/species.csv") %>% 
  rename_all(~str_replace(., " ", "_"))  %>% 
  rename_all(~str_replace(., " ", "_")) %>% 
  mutate(species=ifelse(species=="quinequelineatus", "quinquelineatus",
                        ifelse(species=="mitratis", "mitratus", 
                               ifelse(species=="plebius", "plebeius", 
                                      ifelse(species=="unimaculatis", "unimaculatus", 
                                             ifelse(species=="mahogani", "mahogoni", 
                                                    ifelse(species=="ciliarus", "ciliaris", 
                                                           ifelse(species=="vagiensis", "vaigiensis",
                                                                  ifelse(species=="nigoris", "nigroris", species)))))))),
         genus=ifelse(genus=="Nemia", "Neamia", 
                      ifelse(genus=="Exsenius", "Ecsenius",
                             ifelse(genus=="Meicanthus", "Meiacanthus", 
                                    ifelse(genus=="Chylomycterus", "Chilomycterus", 
                                           ifelse(genus=="Dacyllus", "Dascyllus", 
                                                  ifelse(species=="cf doederlini type", "Ostorhinchus",
                                                         ifelse(species=="exostigma, kalloperus or frenatus", "Pristiapogon",
                                                                ifelse(genus=="Apogonid" | 
                                                                         genus=="Balistid" | 
                                                                         genus=="Chaetodontid" |
                                                                         genus=="Pomacentrid" |
                                                                         genus=="Pseudochromid", NA, genus))))))))) %>% 
  arrange(family, genus, species)

id_vec <- species_dat %>% 
  select(species_id, species, genus, family) %>% 
  mutate(species_clean = ifelse(grepl("sp", species, ignore.case = TRUE), NA, 
                                ifelse(grepl(" ", species), NA, species)),
         species_note = species) %>% 
  mutate(taxa = ifelse(is.na(genus), family, 
                       ifelse(is.na(species_clean), genus, paste(genus, species_clean)))) %>% 
  select(species_id, species_note, taxa)
head(id_vec)
require(worms)
worms_dat <- wormsbynames(id_vec$taxa)
clean_species_dat <- cbind(id_vec, worms_dat)
View(clean_species_dat)

# Morphology data ------------
morph_dev_dat <- read_csv("source_data/morph_development.csv")
colnames(morph_dev_dat) <- c("species_id","batch_id","sample_id","age","TLmm","BDmm","BAmm","PAmm")  

morph_sett_dat <- read_csv("source_data/morph_settlement.csv") %>% 
  mutate(fish_id = tolower(fish_id))

head(morph_dev_dat)
head(morph_sett_dat)

# settlement data ucrit ------------
test=c("cn1", "cn2", "cn3", "cn4", "reef", "ltCalab")

ucrit_sett_dat <- read_csv("source_data/ucrit_settlement.csv") %>% 
  rename_all(~str_replace(., " ", "_")) %>% 
  dplyr::mutate(fish_id = tolower(fish_id),
                date=dmy(date),
                year=year(date),
                month=month(date),
                trap=ifelse(trap=="Liv", "liv", trap),
                guess_year=ifelse(grepl("05l", fish_id), 2005,
                                  ifelse(grepl("04l", fish_id), 2004, 
                                         ifelse(grepl("03l", fish_id), 2003, NA))),
                AMS_rego=NA,
                publication=ifelse((trap %in% test), "Hogan_etal2007", "Fisher_etal2005"),
                stage=ifelse(trap!="reared" & trap!="ac" & sampling_gear!="scuba" & trap!="reef", 
                             "Settlement", 
                             ifelse(trap=="reef" | trap=="scuba" , "post", NA))) %>% 

  
  filter(trap!="reared")

head(ucrit_sett_dat)

# Add hogan data -----------
hogan_dat <- read_csv("source_data/swimming data All_raw_data.csv") %>% 
  mutate(family=Family,
         genus=ifelse(Genus=="Chylomycterus", "Chilomycterus", 
                      ifelse(Genus=="Doratonatus", "Doratonotus", Genus)), 
         species=ifelse(Species=="mahogani", "mahogoni", Species),
         fish_id=tolower(`Larvae #`),
         ucrit=`u-crit`, 
         trap=ifelse(source=="liftnet", "ltCalab", source),      
         date=dmy(Date),
         year=year(date),
         guess_year=ifelse(grepl("05l", fish_id), 2005,
                           ifelse(grepl("04l", fish_id), 2004, 
                                  ifelse(grepl("03l", fish_id), 2003, NA))),
         month=month(date),         
         sampling_gear=ifelse(grepl("cn", source), "net", source),
         notes=NOTES,
         TLmm=`TL(mm)`)

hogan_spp <- hogan_dat %>% 
  select(fish_id, family, genus, species)  %>% 
  mutate(species_clean = ifelse(grepl("sp", species, ignore.case = TRUE), NA, species),
         species_note = species) %>% 
  mutate(taxa = ifelse(is.na(genus), family, 
                       ifelse(is.na(species_clean), genus, paste(genus, species_clean)))) %>% 
  select(species_note, taxa) %>% 
  left_join(clean_species_dat)
head(hogan_spp)

# find taxa not already assigned
new_spp <- unique(hogan_spp[which(is.na(hogan_spp$valid_name)) ,]) %>% 
  select(species_id, species_note, taxa)
new_worms_dat <- wormsbynames(new_spp$taxa)
new_clean_species_dat <- cbind(new_spp, new_worms_dat) %>% 
  mutate(species_id=paste(taxa, species_note)) %>% 
  unique()

View(new_clean_species_dat)
all_clean_species_dat <- rbind(clean_species_dat, new_clean_species_dat) %>% 
  dplyr::select(species_id,species_note,taxa,AphiaID,          
                url,scientificname,authority,status,           
                unacceptreason,taxonRankID,rank,valid_AphiaID,    
                valid_name,valid_authority,parentNameUsageID,kingdom,          
                phylum ,class,order,family,           
                genus,citation)


new_dat <- cbind(hogan_dat, hogan_spp) %>% 
  subset(fish_id %in% setdiff(hogan_dat$fish_id,ucrit_sett_dat$fish_id)) %>% 
  mutate(species_id=ifelse(is.na(species_id), paste(taxa, species_note), species_id)) %>% 
   mutate(AMS_rego=NA,
         publication="hogan_etal2007",
         stage=NA) %>% 
  dplyr::select(c(colnames(ucrit_sett_dat), "TLmm")) 

all_ucrit_sett_dat <- rbind(ucrit_sett_dat, new_dat[, colnames(ucrit_sett_dat)]) %>% 
  mutate(year=ifelse(is.na(year), guess_year, year)) %>% 
  dplyr::select(-guess_year)

# Morphology data 
all_morph_sett_dat <- new_dat %>% 
  select(species_id, fish_id, TLmm) %>% 
  bind_rows(morph_sett_dat)

# Add Stobutzki data ---------------
stob_dat <- read_excel("source_data/Swimming Speeds Raw Data.xlsx", "Sheet1") %>% 
  data.frame() %>% 
  rowid_to_column("ID")  %>% 
  mutate(taxa=Species,
         species_id=paste("IlSt", taxa, sep="_"),
         fish_id=paste("Stobutkzi_", ID, sep=""),
         w.velocity=as.numeric(gsub(">", "", Water.velocity...cm.s.)),
         time=as.numeric(ifelse(grepl("CNF", Time.to.fatigue..min.), NA, Time.to.fatigue..min.)),
         ucrit=ifelse(is.na(time), w.velocity, (w.velocity-5)+(time/(time*w.velocity))), 
         trap="Stobutzki",      
         date=NA,
         year=1992,
         month=NA,         
         sampling_gear="various",
         stage=Pre...Post.Settlement,
         AMS_rego=NA,
         publication="Stobutzki1992; Stobutzki&Bellwood1994",
         notes=paste(Time.to.fatigue..min., Water.velocity...cm.s., sep="; "),
         TLmm=as.numeric(Tl.mm.),
         wg=as.numeric(Wgt...gm.)) %>%
  fill(taxa)
    
stob_spp <- stob_dat %>% 
  select(species_id, taxa)

# treat all as new species
new_worms_dat <- wormsbynames(stob_spp$taxa)
new_clean_species_dat <- cbind(stob_spp, new_worms_dat) %>% 
  mutate(species_id=taxa,
         species_note=taxa) %>% 
  unique()

View(new_clean_species_dat)
all_clean_species_dat <- rbind(all_clean_species_dat, new_clean_species_dat[, colnames(all_clean_species_dat)]) %>% 
  dplyr::select(species_id,species_note,taxa,AphiaID,          
                url,scientificname,authority,status,           
                unacceptreason,taxonRankID,rank,valid_AphiaID,    
                valid_name,valid_authority,parentNameUsageID,kingdom,          
                phylum ,class,order,family,           
                genus,citation)

new_dat <- stob_dat %>% 
  left_join(all_clean_species_dat) %>% 
  subset(fish_id %in% setdiff(stob_dat$fish_id,ucrit_sett_dat$fish_id)) %>% 
  mutate(species_id=ifelse(is.na(species_id), paste(taxa, species_note), species_id)) %>% 
  dplyr::select(c(colnames(all_ucrit_sett_dat), "TLmm", "wg")) 
  
all_ucrit_sett_dat <- rbind(all_ucrit_sett_dat, new_dat[, colnames(all_ucrit_sett_dat)])


# Morphology data
all_morph_sett_dat <- new_dat %>% 
  select(species_id, fish_id, TLmm, wg) %>% 
  bind_rows(morph_sett_dat)


# Add Leis data ---------------
leis_dat_a <- read_excel("source_data/Settlement-stage larvae.xlsx", "Moorea") %>% 
  data.frame() %>% 
  mutate(family=family,
         fish_id=fish.ID,
         species_id=species.abbreviation,
         AMS_rego=AMS.rego.no,
         ucrit.raw=Ucrit,
         ucrit=as.numeric(gsub(">", "", Ucrit)), 
         trap="Leis_moorea",      
         date=ymd(measurement.date),
         year=year(date),
         guess_year=NA,
         month=month(date),         
         sampling_gear=collection.method,
         notes=note,
         TLmm=as.numeric(size.TL..mm.),
         SLmm=as.numeric(size..BL..mm.))

leis_dat_b <- read_excel("source_data/Settlement-stage larvae.xlsx", "Lizard") %>% 
  data.frame() %>% 
  mutate(family=family,
         fish_id=fish.ID,
         species_id=species.abbreviation,
         AMS_rego=AMS.rego.no,
         ucrit.raw=Ucrit,
         ucrit=as.numeric(gsub(">", "", Ucrit)), 
         trap="Leis_lizard",      
         date=ymd(measurement.date),
         year=year(date),
         guess_year=NA,
         month=month(date),         
         sampling_gear=collection.method,
         notes=note,
         TLmm=as.numeric(size.TL..mm.),
         SLmm=as.numeric(size..BL..mm.))
cols_select <- c(colnames(all_ucrit_sett_dat), "TLmm", "SLmm", "family", "genus", "species", "ucrit.raw")
leis_dat <- rbind(leis_dat_a[, cols_select], 
                  leis_dat_b[, cols_select])
leis_dat <- leis_dat %>% 
  mutate(notes=ifelse(grepl(">", ucrit.raw) & is.na(notes), 
                      "Fish reached max speed of flume, speed is an underestimate", 
                      ifelse(grepl(">", ucrit.raw) & !is.na(notes), 
                             paste(notes, "Fish reached max speed of flume, speed is an underestimate", sep="; "), 
                             notes))) %>% 
  select(-ucrit.raw)

leis_spp <- leis_dat %>% 
  select(fish_id, family, genus, species, species_id)  %>% 
  mutate(species_clean = ifelse(grepl("sp", species, ignore.case = TRUE), NA, 
                                ifelse(grepl("NP(TA)", species, ignore.case = TRUE), NA, species)),
         species_note = species) %>% 
  mutate(taxa = ifelse(is.na(genus), family, 
                       ifelse(is.na(species_clean), genus, paste(genus, species_clean)))) %>% 
  mutate(taxa=gsub(" TC", "", taxa)) %>% 
  mutate(taxa=gsub(" NP(TA)", "", taxa)) %>% 
  mutate(taxa=gsub(" AC", "", taxa)) %>% 
  mutate(taxa=gsub("Halicoheres", "Halichoeres", taxa)) %>% 
  select(species_id, taxa)

# treat all as new species
new_worms_dat <- wormsbynames(leis_spp$taxa)
new_clean_species_dat <- cbind(leis_spp, new_worms_dat) %>% 
  mutate(species_id=taxa,
         species_note=taxa) %>% 
  unique()

View(new_clean_species_dat)
all_clean_species_dat <- rbind(all_clean_species_dat, new_clean_species_dat[, colnames(all_clean_species_dat)]) %>% 
  dplyr::select(species_id,species_note,taxa,AphiaID,          
                url,scientificname,authority,status,           
                unacceptreason,taxonRankID,rank,valid_AphiaID,    
                valid_name,valid_authority,parentNameUsageID,kingdom,          
                phylum ,class,order,family,           
                genus,citation)

new_dat <- leis_dat %>% 
  left_join(all_clean_species_dat) %>% 
  dplyr::select(c(colnames(all_ucrit_sett_dat), "TLmm", "SLmm")) 

all_ucrit_sett_dat <- rbind(all_ucrit_sett_dat, new_dat[, colnames(all_ucrit_sett_dat)])

# Morphology data
all_morph_sett_dat <- new_dat %>% 
  select(species_id, fish_id, TLmm, SLmm) %>% 
  bind_rows(morph_sett_dat)

# image data - note only photos for leis_dat_a
tt <- leis_dat_a %>% 
  mutate(image=photo) %>% 
  dplyr::select(fish_id, photo) %>% 
  na.omit() %>% 
  unique() 

image_file_dat <- image_file_dat %>% 
  bind_rows(tt)

# development data ucrit -----
ucrit_dev_dat <- read_csv("source_data/ucrit_development.csv") %>% 
  rename_all(~str_replace(., " ", "_")) %>% 
  dplyr::rename(ucrit = critical_speed) %>% 
  dplyr::mutate(stage=as.character(NA),
         publication=ifelse(species_id=="apsnem" | species_id=="peamel" | species_id== "pepamb", 
                            "Fisher_etal2000", "Fisher2005"),
         comment=NA)
colnames(ucrit_dev_dat)
colnames(morph_dev_dat)
morph_dev_dat$SLmm=as.numeric(NA)
morph_dev_dat$TLAmm=as.numeric(NA)

# Leis development data
leis_dat_dev1 <- read_csv("source_data/ontogenetic data_1.csv") %>% 
  data.frame() %>% 
  dplyr::mutate(TLAmm=NA,
                PAmm=NA,
                TLmm=as.numeric(NA),
                cohort.date=dmy(cohort.date),
                measurement.date=dmy(measurement.date))
colnames(leis_dat_dev1)
leis_dat_dev2 <- read_csv("source_data/ontogenetic data_2.csv") %>% 
  data.frame() %>% 
  dplyr::mutate(TLAmm=NA,
                PAmm=NA,
                TLmm=NA,
                cohort.date=dmy(cohort.date),
                measurement.date=dmy(measurement.date))
colnames(leis_dat_dev2)
leis_dat_dev3 <- read_csv("source_data/ontogenetic data_3.csv") %>% 
  data.frame() %>% 
  dplyr::mutate(Ucrit.raw=Ucrit..cm.s.,
                Ucrit=gsub(">", "", Ucrit..cm.s.),
                TLmm=as.numeric(size..TL.),
                TLAmm=Total.Lateral.area..mm².,
                PAmm=Propulsive.Area..mm²,
                cohort.date=dmy(cohort.date),
                measurement.date=dmy(measurement.date),
                comment2=ifelse(grepl(">", Ucrit.raw), Ucrit.raw, "")) %>% 
  unite(comment, comment2)
colnames(leis_dat_dev3)

select.vars=c("species.abbreviation", "species", "genus", "family",
              "fish.ID", "cohort.date", "measurement.date", "size..BL.", "TLmm",
              "Ucrit", "publication", "stage",  "TLAmm", "PAmm", "comment")

l1 <- leis_dat_dev1 %>% 
  dplyr::select(select.vars)
l2 <- leis_dat_dev2 %>% 
  dplyr::select(select.vars)
l3 <- leis_dat_dev3 %>% 
  dplyr::select(select.vars)
str(l1)
str(l2)
str(l3)

leis_dev_dat <- rbind(l1, l2, l3) %>% 
  data.frame() %>% 
  dplyr::mutate(batch_id=as.character(julian(cohort.date)),
                species_id=species.abbreviation,
                fish_id=fish.ID,
                sample_id=fish_id,
                SLmm=as.numeric(size..BL.),
                eggdur=NA,
                BDmm=NA,
                BAmm=NA,
                age=as.integer(measurement.date-cohort.date),
                ucrit=as.numeric(Ucrit))
colnames(leis_dev_dat)
head(leis_dev_dat)
str(leis_dev_dat)

# Now append to the other data
str(ucrit_dev_dat)
str(morph_dev_dat)

ucrit_dev_dat <- ucrit_dev_dat %>% 
  bind_rows(leis_dev_dat[, colnames(ucrit_dev_dat)])
morph_dev_dat <- morph_dev_dat %>% 
  bind_rows(leis_dev_dat[, colnames(morph_dev_dat)])

# Make a sample ID table for the dev data
dev_sample_dat <- ucrit_dev_dat %>% 
  bind_rows(morph_dev_dat) %>% 
  dplyr::select(species_id, batch_id, sample_id, age, eggdur) %>% 
  unique()

# Make a Fish ID table for the sett data
fish_id_dat <- all_ucrit_sett_dat %>% 
  full_join(morph_sett_dat) %>% 
  left_join(image_file_dat) %>% 
  dplyr::select(species_id, fish_id, date, trap, sampling_gear, year, month, filename, image, AMS_rego, publication, stage)

# find missing species
new_worms_dat <- wormsbynames(paste(leis_dev_dat$genus, leis_dev_dat$species))
new_clean_species_dat <- cbind(leis_dev_dat[, c("species_id", "genus", "species")], new_worms_dat) %>% 
   mutate(species_note=paste(genus, species),
          taxa=paste(genus, species)) %>% 
   unique()

all_clean_species_dat <-  
  bind_rows(all_clean_species_dat,
            new_clean_species_dat[, colnames(all_clean_species_dat)])

# Write out the clean data files
write_csv(site_dat, "clean_data/site_dat.csv")
write_csv(all_clean_species_dat, "clean_data/species_dat.csv")
write_csv(fish_id_dat, "clean_data/fish_id_dat.csv")
all_ucrit_sett_dat %>% 
  dplyr::select(fish_id, ucrit, notes) %>% 
  write_csv("clean_data/ucrit_sett_dat.csv")
all_morph_sett_dat %>% 
  dplyr::select(-species_id) %>% 
  write_csv("clean_data/morph_sett_dat.csv")
write_csv(dev_sample_dat, "clean_data/dev_sample_dat.csv")
ucrit_dev_dat %>% 
  dplyr::select(sample_id, ucrit, comment) %>% 
write_csv("clean_data/ucrit_dev_dat.csv")
morph_dev_dat %>% 
  dplyr::select(-species_id, -batch_id, -age) %>% 
  write_csv("clean_data/morph_dev_dat.csv")

write_csv(image_file_dat, "clean_data/image_data.csv")

# Location by year
check_locations <- all_ucrit_sett_dat %>% 
  left_join(site_dat) %>% 
  mutate(index=1,
         name = ifelse(location=="BLZ", "Calabash Caye, Belize", 
                       ifelse(location=="LI", "Lizard Island, Australia",
                              ifelse(location=="TCI", "South Caicos, Turks and Caicos Island",
                                     ifelse(location=="GI/MI", "Green Island or Magnetic Island",
                                            ifelse(location=="SI", "Society Islands",location)))))) %>% 
  dplyr::group_by(region, year, location, name) %>%
  dplyr::summarise(count=sum(index)) %>% 
  arrange(region,location, year) %>%  
  data.frame()

check_locations
write.csv(check_locations, "MS_table1.csv")

# Check clutches
check_clutches <- ucrit_dev_dat %>% 
  group_by(species_id, batch_id) %>% 
  dplyr::summarise(ucrit=mean(ucrit))
check_clutches

