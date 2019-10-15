### Prep data for Group Project 3

# Load libraries
library("tidyverse")
library("readxl")
library("vegan")

communityNO_raw <- read_excel("data/funcab_composition_2016.xlsx")
spNO = read_excel("data/fsystematics_species.xlsx")
traitNO_raw <- read_delim("data/traitdata_NO.csv", col_names = TRUE, delim = ",")

#Cleaning Norway Community
#Dictionary to make names in the community data speak with the names in the trait data
Dictionary_communityNO <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
                                       "old new
                                     Nar_stri Nar_str
                                     Tarax Tar_sp
                                     Euph_sp Eup_sp
                                     Phle_alp Phl_alp
                                     Rhin_min Rhi_min
                                     Rum_ac-la Rum_acl
                                     Trien_eur Tri_eur
                                     Rub_idae Rub_ida
                                     Saus_alp Sau_alp
                                     Ave__pub Ave_pub
                                     Car_atra Car_atr
                                     Hypo_rad Hyp_rad
                                     Bart_alp Bar_alp
                                     Car_pulic Car_pul
                                     Carex_sp Car_sp
                                     Hier_sp Hie_sp
                                     Salix_sp Sal_sp
                                     Emp_her Emp_nig
                                     Emp Emp_nig
                                     Hie_vulg Hie_vul
                                     Vio_can Vio_riv")

spNO <- spNO %>% 
  mutate(Species = gsub(" ", "_", Species))

community <- communityNO_raw %>% 
  filter(Measure == "Cover") %>%
  select(-Treatment, -'Nid herb', 'Nid gram', -'Nid rosett', -'Nid seedling', -liver, -lichen, -litter, -soil, -rock, -'#Seedlings', -TotalGraminoids, -totalForbs, -totalBryophytes...285, -vegetationHeight, -mossHeight, -comment, -'ver seedl', -canum, -totalVascular, -totalBryophytes...292, -acro, -pleuro, -totalLichen) %>% 
  gather(key = Taxon, value = Cover, -Site, -Block, -turfID, -subPlot, -year, -date, -Measure, -recorder) %>% 
  filter(!is.na(Cover)) %>% 
  mutate(Taxon = gsub(" ", "_", Taxon))%>%
  mutate(Taxon = plyr::mapvalues(Taxon, from = Dictionary_communityNO$old, to = Dictionary_communityNO$new, warn_missing = FALSE)) %>%
  left_join(spNO, by = c("Taxon" = "Species")) %>% 
  select(-Genus, -Family, -Order, -Taxon) %>% 
  rename(Taxon = Full_name) %>% 
  mutate(Site = substr(Site, 1, 3)) %>% 
  mutate(Country = "NO") %>% 
  mutate(Gradient = case_when(Site %in% c("Fau", "Alr", "Ulv") ~ as.character(1),
                              Site %in% c("Vik", "Hog", "Lav") ~ as.character(2),
                              Site %in% c("Arh", "Ram", "Gud") ~ as.character(3),
                              Site %in% c("Ovs", "Ves", "Skj") ~ as.character(4)
  )) %>% 
  rename(Year = year, BlockID = Block, PlotID = turfID) %>% 
  mutate(Cover = as.numeric(Cover),
         BlockID = as.character(BlockID)) %>% 
  select(Country, Year, Site, Gradient, BlockID, PlotID, Taxon, Cover) %>% 
  filter(!is.na(Cover), !Cover == 0)


community <- community %>% 
  select(Site, PlotID, Taxon, Cover) %>% 
  filter(Site %in% c("Gud", "Arh")) %>% 
  filter(!is.na(Taxon)) %>% 
  mutate(Site = ifelse(Site == "Gud", "Alpine", "Lowland")) %>% 
  mutate(Taxon = ifelse(Taxon == "Potentilla  crantzii", "Potentilla crantzii", Taxon))



#Cleaning Norway Trait
trait <- traitNO_raw %>% 
  mutate(Leaf_Thickness_Ave_mm = (Leaf_Thickness_1_mm + Leaf_Thickness_2_mm + Leaf_Thickness_3_mm)/3) %>% 
  mutate(Gradient = case_when(Site %in% c("Fau", "Alr", "Ulv") ~ as.character(1),
                              Site %in% c("Vik", "Hog", "Lav") ~ as.character(2),
                              Site %in% c("Arh", "Ram", "Gud") ~ as.character(3),
                              Site %in% c("Ovs", "Ves", "Skj") ~ as.character(4)
  )) %>%
  # mutate(BlockID = as.character(1),
  #        PlotID = as.character(1)) %>% 
  select(Country, Year, Site, Gradient, Taxon, Plant_Height_cm, Wet_Mass_g, Dry_Mass_g, Leaf_Thickness_Ave_mm, Leaf_Area_cm2, SLA_cm2_g, LDMC, N_percent, C_percent, CN_ratio) %>% 
  gather(key = Trait, value = Value, -Country, -Year, -Site, -Gradient, -Taxon) %>% 
  filter(!is.na(Value)) %>% 
  mutate(Taxon = recode(Taxon, "Empetrum nigrum subsp. Hermaphroditum" = "Empetrum nigrum")) %>% 
  filter(!is.na(Value)) %>% 
  mutate(Country = "NO") #Overwrite junk...

trait <- trait %>% 
  select(Site, Taxon, Trait, Value) %>% 
  filter(Site %in% c("Gud", "Arh")) %>% 
  mutate(Site = ifelse(Site == "Gud", "Alpine", "Lowland")) %>% 
  filter(!Trait %in% c("Dry_Mass_g", "Wet_Mass_g"))


FunctionalGroups <- read.table(header = TRUE, stringsAsFactors = FALSE, text = 
                                 "Taxon Taxon2 FunctionalGroup
                               Achillea millefolium herb
                               Agrostis capilaris graminoid
                               Alchemilla alpina herb
                               Alchemilla sp. herb
                               Antennaria dioica herb
                               Anthoxanthum odoratum graminoid
                               Astragalus alpinus herb
                               Avenella flexuosa graminoid
                               Bistorta vivipara herb
                               Campanula rotundifolia herb
                               Carex bigelowii graminoid
                               Carex capillaris graminoid
                               Carex nigra graminoid
                               Carex pallescens graminoid
                               Carex pilulifera graminoid
                               Carex vaginata graminoid
                               Deschampsia cespitosa graminoid
                               Empetrum nigrum shrub 
                               Euphrasia sp. herb  
                               Festuca ovina graminoid
                               Festuca rubra graminoid 
                               Geranium sylvaticum herb
                               Hieracium pilosella herb
                               Hypericum maculatum herb
                               Luzula multiflora graminoid
                               Luzula pilosella graminoid
                               Melampyrum pratense herb
                               Nardus stricta graminoid
                               Parnassia palustris herb
                               Phleum alpinum graminoid   
                               Pinguicula vulgaris herb
                               Poa pratensis graminoid
                               Potentilla crantzii herb
                               Potentilla erecta herb 
                               Prunella vulgaris herb
                               Pyrola minor herb      
                               Ranunculus acris herb
                               Rhinanthus minor herb  
                               Rumex acetosa herb   
                               Saussurea alpina herb
                               Saxifraga aizoides herb
                               Stellaria graminea herb
                               Taraxacum sp. herb  
                               Thalictrum alpinum herb
                               Tofieldia pusilla herb
                               Trifolium repens herb
                               Vaccinium myrtillus shrub
                               Vaccinium uliginosum shrub
                               Vaccinium vitis-idaea shrub
                               Veronica alpina herb
                               Veronica chamaedrys herb
                               Viola palustris herb")

FunctionalGroups <- FunctionalGroups %>% 
  mutate(Taxon = paste(Taxon, Taxon2, sep = " ")) %>% 
  select(-Taxon2)


save(community, file = "community.RData")
save(trait, file = "trait.RData")
save(FunctionalGroups, file = "FunctionalGroups.RData")
