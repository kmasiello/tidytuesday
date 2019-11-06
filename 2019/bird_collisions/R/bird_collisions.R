## Get the data --------
bird_collisions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")
mp_light <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/mp_light.csv")
save(bird_collisions, mp_light, file=here::here("data","rawData.rdata"))
load(here::here("data", "rawData.rdata"))

## Cleaning script ------
library(tidyverse)
library(here)


# Read in the raw data

raw_birds <- read_csv(here("2019", "2019-04-30", "Chicago_collision_data.csv")) %>% 
  janitor::clean_names()

raw_light <- read_csv(here("2019", "2019-04-30", "Light_levels_dryad.csv")) %>% 
  janitor::clean_names()

raw_call <- read_csv(here("2019", "2019-04-30", "bird_call.csv")) %>% 
  set_names(nm = "col_to_split")

# add column names for call_data
clean_col <- c("genus", "species", "Family", "Collisions", "Flight Call", "Habitat", "Stratum")

# separate space delim columns
clean_call <- raw_call %>% 
  separate(col_to_split, into = clean_col, sep = " ") %>% 
  janitor::clean_names() %>% 
  select(family, genus, species, everything())

# join bird metadata to impact data
clean_birds <- raw_birds %>% 
  left_join(clean_call, by = c("genus", "species")) %>% 
  mutate(flight_call = case_when(genus == "Ammodramus" & species == "nelsoni" ~ "Yes",
                                 genus == "Ammodramus" & species == "leconteii" ~ "Yes",
                                 TRUE ~ flight_call)) %>% 
  filter(!is.na(flight_call),
         !is.na(collisions)) %>% 
  select(-collisions)

# confirm match - matches the summary-level data
raw_birds %>% 
  unite("g_s", c("genus","species")) %>% 
  group_by(g_s) %>% 
  count(sort = TRUE)

clean_birds %>% write_csv(here("2019", "2019-04-30", "bird_collisions.csv"))
raw_light %>% write_csv(here("2019", "2019-04-30", "mp_light.csv"))
