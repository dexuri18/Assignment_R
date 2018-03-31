#load the tidyverse library
library(tidyverse) 

#load the data
surveys <- read.csv("data/portal_data_joined.csv") 

#clean the data from NA and blank value
surveys_complete <- surveys %>%
  filter(!is.na(hindfoot_length)) %>%
  filter(!is.na(weight)) %>%
  filter(sex !="") %>%
  filter(species_id !="")

#list of species with n >= 1000
species_count <- surveys_complete %>%
  group_by(species) %>%
  tally() %>%
  filter(n >= 1000)

#filter of the common sample (>= 1000)
surveys_common <- surveys_complete %>%
  filter(species %in% species_count$species)
  
#summary of common sample's weight by species 
surveys_common %>%
  group_by(species,sex) %>%
  summarize(mean_weight = mean(weight),
            max_weight = max(weight),
            min_weight = min(weight))

species_vs_weight <- select(surveys_common, species, weight)
plot(species_vs_weight)


#summary of common sample's legth of hindfoot by species 
surveys_common %>%
  group_by(species,sex) %>%
  summarize(mean_length = mean(hindfoot_length),
            max_length = max(hindfoot_length),
            min_length = min(hindfoot_length))

species_vs_length <- select(surveys_common, species, hindfoot_length)
plot(species_vs_length)


# Linier Model Analysis between weight and hindfoot legth on the most common species

#create object for analysis : 
most_common_data <- surveys_common %>%
  filter(species == "merriami")

weight_vs_length <- select(most_common_data, weight, hindfoot_length)

#plot the data in scatter
plot(weight_vs_length)

#fitting linier model between weight and length of hindfoot
lm_weight_vs_length <- lm(hindfoot_length ~ weight, data=most_common_data)
summary(lm_weight_vs_length)
plot(lm_weight_vs_length)




  
