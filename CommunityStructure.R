########################################
### Chapter 16 - Community Structure ###
### Species diversity and food webs ###
########################################

# Load libraries
library("tidyverse")
library("vegan")

# Load data set
data(BCI)

# prepare abundance data set
Abundance <- BCI %>% 
  # add site names
  mutate(site = 1:50) %>% 
  gather(key = species, value = abundance, -site) %>% 
  filter(!abundance == 0) %>% 
  # reduce the data set to 2 sites (18 and 35)
  filter(site %in% c(35, 18)) %>% 
  # arrange the species according to their abundance, within each site
  arrange(site, -abundance) %>% 
  # Tweak the data a bit to make the them more uneven
  mutate(abundance = ifelse(site == 18 & species == "Faramea.occidentalis", 18, abundance),
         abundance = ifelse(site == 18 & species == "Oenocarpus.mapora", 18, abundance),
         abundance = ifelse(site == 18 & species == "Trichilia.tuberculata", 17, abundance))


# 1) 1)	Calculate the relative abundance for each species within each site and draw a rank-abundance plot.
RelAbundance <- Abundance %>% 
  # calculate the total nr of species, relative abundnace and rank the species according to their abundance.
  group_by() %>% 
  mutate(totalN = ,
         relAbunance = ,
         rankAbundance = )

# Make a rank-abundance plot
ggplot(RelAbundance, aes(x = , y = , colour = factor()))  +
  geom_point() +
  geom_line()



# 2) 2)	Calculate species richness, diversity and evenness for both sites. Calculate diversity and evenness indexes using the Simpson and the Shannon index.
RelAbundance %>% 
  group_by() %>% 
  summarise(# species richness
            richnessS = ,
            
            # Simpson indexes D
            simpsonD = ,
            simpsonIndexofD = ,
            simpsonRecipIndex = ,
            EvennessD = ,
            
            # Shannon index H
            H = ,
            Hmax = ,
            EvennessH = ,
            
            # Check values with diversity function from the vegan package
            diversityD = diversity(abundance, index = "simpson"),
            diversityH = diversity(abundance, index = "shannon"))



# 4) Foodweb, connectance and linkage density
foodweb <- tibble(FoodWeb = c("A", "B"),
       SpeciesRichness = c(, ),
       NrLinkage = c(, ))

# Calculate connectance and linkage density
foodweb %>% 
  mutate(MaxLinkage = ,
         Connectance = ,
         LinkageDensity = )
