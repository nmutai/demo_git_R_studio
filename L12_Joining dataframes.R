###################################################################
############### Module 6, Basics of data wrangling ################
###################################################################
## Lesson 12: Joining data together:

###################################
##### JOINING DATASETS 

library(tidyverse)

island_plants <- read_csv("data/island_veg_data_raw.csv")

### Add our uppercase first letter of scientific name function from before:
cap_genus <- function(spp_name) {
  first_letter <- substr(spp_name, start=1, stop=1) #extract the first letter
  rest_of_name <- substr(spp_name, start=2, stop=nchar(spp_name)) # extract the rest
  first_let_cap <- toupper(first_letter)
  new_name <- paste(first_let_cap, rest_of_name, sep="")
  return(new_name)
}

tree_BA_data <- select(island_plants, island = Island, plot = Plot, date = `Date (mm/dd/yy)`, 
                       covertype = CoverType, species = Species, diameter_cm = Over_Midstory) %>%
  # keep only overstory trees
  filter(covertype == "overstory") %>%
  select(-covertype) %>% 
  # calculate trunk area in meters squared of each tree
  mutate(trunk_area_cm2 = pi*(diameter_cm/2)^2,
         trunk_area_m2 = trunk_area_cm2 * 0.0001) %>%
  # remove unnecessary columns
  select(-trunk_area_cm2, -diameter_cm) %>% 
  # capitalize all genera in the scientific names
  mutate(species = cap_genus(species),
         # add column that indicates if the trees are small or large
         tree_size = ifelse(trunk_area_m2 < 0.1, "small", "large")) %>% 
  group_by(species, island) %>%
  summarize(mean_BA_ha = sum(trunk_area_m2)/0.05,
            trees_per_samp = n()) %>%
  ungroup()





###################################
##### SUMMARY
# - How to use the left_join (or right) for combining dataframes

